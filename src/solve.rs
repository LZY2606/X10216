//! Constraint solver: instantiation, generalization (with value restriction),
//! unification with occurs-check and row polymorphism. Produces a step trace
//! and structured errors with provenance.

use crate::constrain::{CKind, Constraint};
use crate::types::{show_ty, Row, Scheme, Ty};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SolveError {
    Occurs { var: u32, ty: String },
    Mismatch { a: String, b: String },
    MissingFields { fields: Vec<String>, record: String },
    ExtraFields { fields: Vec<String>, record: String },
    Unbound { name: String },
}

impl SolveError {
    pub fn message(&self) -> String {
        match self {
            SolveError::Occurs { var, ty } =>
                format!("occurs check failed: '{}' occurs in {} (infinite type)", var, ty),
            SolveError::Mismatch { a, b } =>
                format!("cannot unify {} with {}", a, b),
            SolveError::MissingFields { fields, record } =>
                format!("record is missing field(s) {} (required by {})", fields.join(", "), record),
            SolveError::ExtraFields { fields, record } =>
                format!("record has extra field(s) {} (not present in {})", fields.join(", "), record),
            SolveError::Unbound { name } =>
                format!("unbound variable `{}`", name),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracedError {
    pub constraint_id: String,
    pub span: (usize, usize),
    pub desc: String,
    pub message: String,
    pub error: SolveError,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Step {
    pub index: usize,
    pub constraint_id: String,
    pub action: String,
    pub detail: String,
    pub span: (usize, usize),
}

pub struct Solver {
    pub subst: HashMap<u32, Ty>,
    pub env: HashMap<String, Vec<Scheme>>,
    pub next_var: u32,
    pub steps: Vec<Step>,
    pub errors: Vec<TracedError>,
    pub recover: bool,
}

fn free_vars(t: &Ty, subst: &HashMap<u32, Ty>, out: &mut BTreeSet<u32>) {
    match t {
        Ty::Int | Ty::Bool => {}
        Ty::Var(v) => {
            if let Some(r) = subst.get(v) {
                let r = r.clone();
                free_vars(&r, subst, out);
            } else {
                out.insert(*v);
            }
        }
        Ty::Fun(a, b) => {
            free_vars(a, subst, out);
            free_vars(b, subst, out);
        }
        Ty::Record(r) | Ty::Variant(r) => {
            for (_, t) in &r.fields { free_vars(t, subst, out); }
            if let Some(v) = r.tail {
                if let Some(Ty::Record(rr)) = subst.get(&v) {
                    let rr = rr.clone();
                    free_vars(&Ty::Record(rr), subst, out);
                } else {
                    out.insert(v);
                }
            }
        }
    }
}

fn occurs(v: u32, t: &Ty, subst: &HashMap<u32, Ty>) -> bool {
    let mut s = BTreeSet::new();
    free_vars(t, subst, &mut s);
    s.contains(&v)
}

impl Solver {
    pub fn new(next_var: u32) -> Self {
        Solver {
            subst: HashMap::new(),
            env: HashMap::new(),
            next_var,
            steps: Vec::new(),
            errors: Vec::new(),
            recover: true,
        }
    }

    fn fresh(&mut self) -> u32 {
        let v = self.next_var;
        self.next_var += 1;
        v
    }

    pub fn resolve(&self, t: &Ty) -> Ty {
        match t {
            Ty::Var(v) => match self.subst.get(v) {
                Some(r) => self.resolve(&r.clone()),
                None => t.clone(),
            },
            Ty::Fun(a, b) => Ty::Fun(Box::new(self.resolve(a)), Box::new(self.resolve(b))),
            Ty::Record(r) => {
                let mut row = self.resolve_row(r);
                // flatten chained row tails
                let mut fields = row.fields.clone();
                let mut tail = row.tail;
                while let Some(v) = tail {
                    match self.subst.get(&v) {
                        Some(Ty::Record(rr)) => {
                            let rr = self.resolve_row(rr);
                            for (k, val) in rr.fields { fields.entry(k).or_insert(val); }
                            tail = rr.tail;
                        }
                        _ => break,
                    }
                }
                row.fields = fields;
                row.tail = tail;
                Ty::Record(row)
            }
            Ty::Variant(r) => {
                if let Ty::Record(rr) = self.resolve(&Ty::Record(r.clone())) {
                    Ty::Variant(rr)
                } else { unreachable!() }
            }
            other => other.clone(),
        }
    }

    fn resolve_row(&self, r: &Row) -> Row {
        Row {
            fields: r.fields.iter().map(|(k, v)| (k.clone(), self.resolve(v))).collect(),
            tail: r.tail,
        }
    }

    pub fn show(&self, t: &Ty) -> String {
        let r = self.resolve(t);
        show_ty(&r, &|v| self.subst.get(&v).map(|t| self.resolve(t)))
    }

    fn bind_var(&mut self, v: u32, t: Ty) -> Result<(), SolveError> {
        if occurs(v, &t, &self.subst) {
            return Err(SolveError::Occurs { var: v, ty: self.show(&t) });
        }
        self.subst.insert(v, t);
        Ok(())
    }

    pub fn unify(&mut self, a: &Ty, b: &Ty) -> Result<(), SolveError> {
        let a = self.resolve(a);
        let b = self.resolve(b);
        match (&a, &b) {
            (Ty::Int, Ty::Int) | (Ty::Bool, Ty::Bool) => Ok(()),
            (Ty::Var(v), t) => {
                if *t == Ty::Var(*v) { return Ok(()); }
                self.bind_var(*v, t.clone())
            }
            (t, Ty::Var(v)) => self.bind_var(*v, t.clone()),
            (Ty::Fun(a1, b1), Ty::Fun(a2, b2)) => {
                self.unify(a1, a2)?;
                self.unify(b1, b2)
            }
            (Ty::Record(r1), Ty::Record(r2)) => self.unify_rows(r1, r2, false),
            (Ty::Variant(r1), Ty::Variant(r2)) => self.unify_rows(r1, r2, true),
            _ => Err(SolveError::Mismatch { a: self.show(&a), b: self.show(&b) }),
        }
    }

    fn unify_rows(&mut self, r1: &Row, r2: &Row, _variant: bool) -> Result<(), SolveError> {
        let keys: BTreeSet<String> = r1.fields.keys().chain(r2.fields.keys()).cloned().collect();
        let mut only1 = Vec::new();
        let mut only2 = Vec::new();
        for k in &keys {
            match (r1.fields.get(k), r2.fields.get(k)) {
                (Some(t1), Some(t2)) => self.unify(t1, t2)?,
                (Some(_), None) => only1.push(k.clone()),
                (None, Some(_)) => only2.push(k.clone()),
                (None, None) => unreachable!(),
            }
        }
        let mk_row = |fields: Vec<String>, src: &Row, tail: Option<u32>| Row {
            fields: fields
                .into_iter()
                .map(|k| (k.clone(), src.fields[&k].clone()))
                .collect(),
            tail,
        };
        match (r1.tail, r2.tail) {
            (Some(t1), Some(t2)) => {
                if only1.is_empty() && only2.is_empty() {
                    // merge tails
                    if t1 != t2 {
                        self.bind_var(t1, Ty::Record(Row::open(t2)))?;
                    }
                    Ok(())
                } else {
                    let fresh = self.fresh();
                    self.bind_var(t1, Ty::Record(mk_row(only2, r2, Some(fresh))))?;
                    self.bind_var(t2, Ty::Record(mk_row(only1, r1, Some(fresh))))?;
                    Ok(())
                }
            }
            (None, None) => {
                // two closed rows with different field sets: report the
                // fields that are extra on either side
                if !only1.is_empty() {
                    return Err(SolveError::ExtraFields {
                        fields: only1,
                        record: self.show(&Ty::Record(r2.clone())),
                    });
                }
                if !only2.is_empty() {
                    return Err(SolveError::ExtraFields {
                        fields: only2,
                        record: self.show(&Ty::Record(r1.clone())),
                    });
                }
                Ok(())
            }
            (Some(t1), None) => {
                // r2 is closed; fields only in r2 are missing from r1's known
                // part but r1 is open, so they flow into r1's tail.
                if !only1.is_empty() {
                    // fields only in r1: r2 closed cannot accept them
                    return Err(SolveError::ExtraFields {
                        fields: only1,
                        record: self.show(&Ty::Record(r2.clone())),
                    });
                }
                self.bind_var(t1, Ty::Record(mk_row(only2, r2, None)))?;
                Ok(())
            }
            (None, Some(t2)) => {
                if !only2.is_empty() {
                    return Err(SolveError::MissingFields {
                        fields: only2,
                        record: self.show(&Ty::Record(r1.clone())),
                    });
                }
                self.bind_var(t2, Ty::Record(mk_row(only1, r1, None)))?;
                Ok(())
            }
        }
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Ty {
        let mut map: HashMap<u32, Ty> = HashMap::new();
        for v in &scheme.vars {
            let f = self.fresh();
            map.insert(*v, Ty::Var(f));
        }
        let subst_one = |t: &Ty, map: &HashMap<u32, Ty>| -> Ty {
            fn go(t: &Ty, map: &HashMap<u32, Ty>) -> Ty {
                match t {
                    Ty::Var(v) => map.get(v).cloned().unwrap_or_else(|| Ty::Var(*v)),
                    Ty::Fun(a, b) => Ty::Fun(Box::new(go(a, map)), Box::new(go(b, map))),
                    Ty::Record(r) => Ty::Record(Row {
                        fields: r.fields.iter().map(|(k, v)| (k.clone(), go(v, map))).collect(),
                        tail: r.tail.map(|v| match map.get(&v) {
                            Some(Ty::Var(nv)) => *nv,
                            _ => v,
                        }),
                    }),
                    Ty::Variant(r) => {
                        if let Ty::Record(rr) = go(&Ty::Record(r.clone()), map) { Ty::Variant(rr) }
                        else { unreachable!() }
                    }
                    other => other.clone(),
                }
            }
            go(t, map)
        };
        subst_one(&scheme.ty, &map)
    }

    fn generalize(&self, ty: &Ty) -> Scheme {
        let mut env_free = BTreeSet::new();
        for schemes in self.env.values() {
            for s in schemes {
                // only the top binding is reachable, but be conservative
                free_vars(&s.ty, &self.subst, &mut env_free);
            }
        }
        let mut ty_free = BTreeSet::new();
        free_vars(ty, &self.subst, &mut ty_free);
        let vars: Vec<u32> = ty_free.difference(&env_free).cloned().collect();
        Scheme { vars, ty: self.resolve(ty) }
    }

    fn step(&mut self, c: &Constraint, action: &str, detail: String) {
        self.steps.push(Step {
            index: self.steps.len(),
            constraint_id: c.id.clone(),
            action: action.to_string(),
            detail,
            span: c.prov.span,
        });
    }

    fn record_error(&mut self, c: &Constraint, e: SolveError) {
        self.errors.push(TracedError {
            constraint_id: c.id.clone(),
            span: c.prov.span,
            desc: c.prov.desc.clone(),
            message: e.message(),
            error: e,
        });
    }

    /// Run the constraint list. `disabled` constraints are skipped (the
    /// "temporarily disable one constraint" feature). With `recover` the
    /// solver continues after errors to find multiple errors.
    pub fn run(&mut self, constraints: &[Constraint], disabled: &BTreeSet<String>) {
        for c in constraints {
            if disabled.contains(&c.id) {
                self.step(c, "disabled", "constraint disabled by user".into());
                continue;
            }
            let res = self.exec(c);
            if let Err(e) = res {
                self.step(c, "error", e.message());
                self.record_error(c, e);
                if !self.recover { return; }
            }
        }
    }

    fn exec(&mut self, c: &Constraint) -> Result<(), SolveError> {
        match &c.kind {
            CKind::Unify { a, b } => {
                let sa = self.show(a);
                let sb = self.show(b);
                self.unify(a, b)?;
                self.step(c, "unify", format!("{} ~ {}  ⇒  {}", sa, sb, self.show(a)));
                Ok(())
            }
            CKind::Inst { name, expect } => {
                let scheme = self.env.get(name).and_then(|v| v.last()).cloned();
                match scheme {
                    Some(s) => {
                        let inst = self.instantiate(&s);
                        self.step(
                            c,
                            "instantiate",
                            format!("{} : ∀{:?}.{}  ⇒  {}", name, s.vars, self.show(&s.ty), self.show(&inst)),
                        );
                        self.unify(&inst, expect)?;
                        self.step(c, "unify", format!("{} ~ {}", self.show(&inst), self.show(expect)));
                        Ok(())
                    }
                    None => {
                        if self.recover {
                            // bind a fresh var so later constraints can proceed
                            let f = Ty::Var(self.fresh());
                            self.env.entry(name.clone()).or_default().push(Scheme {
                                vars: vec![],
                                ty: f.clone(),
                            });
                            self.unify(&f, expect)?;
                        }
                        Err(SolveError::Unbound { name: name.clone() })
                    }
                }
            }
            CKind::Gen { name, ty, expansive } => {
                let scheme = if *expansive {
                    // value restriction: do not generalize expansive expressions
                    Scheme { vars: vec![], ty: self.resolve(ty) }
                } else {
                    self.generalize(ty)
                };
                self.step(
                    c,
                    "generalize",
                    if *expansive {
                        format!("{} : {} (value restriction: not generalized)", name, self.show(&scheme.ty))
                    } else {
                        format!("{} : ∀{:?}.{}", name, scheme.vars, self.show(&scheme.ty))
                    },
                );
                self.env.entry(name.clone()).or_default().push(scheme);
                Ok(())
            }
            CKind::BindMono { name, ty } => {
                self.step(c, "bind", format!("{} : {} (monomorphic)", name, self.show(ty)));
                self.env.entry(name.clone()).or_default().push(Scheme {
                    vars: vec![],
                    ty: ty.clone(),
                });
                Ok(())
            }
            CKind::Unbind { name } => {
                if let Some(stack) = self.env.get_mut(name) {
                    stack.pop();
                }
                self.step(c, "unbind", format!("leave scope of {}", name));
                Ok(())
            }
        }
    }
}
