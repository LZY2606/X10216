//! Rank-based HM solver: union-find unification with occurs check, value
//! restriction, and separate missing/extra row fields. Produces a replayable
//! step trace and post-hoc minimal unsat cores.

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::constraints::{Op, Reason};
use crate::types::{OrigPoint, Row, Scheme, Type, TV};

#[derive(Clone, Debug)]
pub enum TypeErrorKind {
    Mismatch { expected: Type, actual: Type, detail: String },
    InfiniteType { variable: String, expanded: Type, detail: String },
    RowMissing { label: String, present: Vec<String> },
    RowExtra { label: String, present: Vec<String> },
    Arity { expected: usize, actual: usize },
}

#[derive(Clone, Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub anchor: u64,
    pub nodes: Vec<crate::ast::NodeId>,
    pub detail: String,
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(tag = "tag", content = "data")]
pub enum StepEffect {
    Fresh { var: String },
    Linked { a: String, b: String },
    Generalized { name: String, quantified: Vec<String> },
    Instantiated { name: String, vars: Vec<String> },
    Failed { detail: String },
    Scope,
    Note,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Step {
    pub index: usize,
    pub op_index: usize,
    pub constraint_id: Option<u64>,
    pub kind: String,
    pub summary: String,
    pub effect: StepEffect,
    pub level: u32,
}

#[derive(Clone)]
struct Desc {
    term: Type,
    rank: u32,
    origins: Vec<OrigPoint>,
}

pub struct Solver {
    parent: HashMap<u32, u32>,
    desc: HashMap<u32, Desc>,
    schemes: HashMap<crate::ast::NodeId, Scheme>,
    next_fresh_solver: u32,
    level: u32,
    errors: Vec<TypeError>,
    /// constraint ids that failed during the forward pass
    pub failed_ids: HashSet<u64>,
    var_owner: HashMap<u32, crate::ast::NodeId>,
    var_role: HashMap<u32, String>,
    var_names: HashMap<u32, String>,
}

impl Solver {
    pub fn new() -> Self {
        Solver {
            parent: HashMap::new(),
            desc: HashMap::new(),
            schemes: HashMap::new(),
            next_fresh_solver: 100_000,
            level: 1,
            errors: Vec::new(),
            failed_ids: HashSet::new(),
            var_owner: HashMap::new(),
            var_role: HashMap::new(),
            var_names: HashMap::new(),
        }
    }

    fn fresh_solver_var(&mut self, rank: u32) -> TV {
        let v = self.next_fresh_solver;
        self.next_fresh_solver += 1;
        self.parent.insert(v, v);
        self.desc.insert(v, Desc { term: Type::Var(TV(v)), rank, origins: Vec::new() });
        TV(v)
    }

    fn find(&mut self, tv: TV) -> TV {
        let mut cur = tv.0;
        let mut root = cur;
        while self.parent[&root] != root {
            root = self.parent[&root];
        }
        while self.parent[&cur] != cur {
            let next = self.parent[&cur];
            self.parent.insert(cur, root);
            cur = next;
        }
        TV(root)
    }

    fn desc_of(&self, tv: TV) -> &Desc {
        self.desc.get(&tv.0).expect("descriptor present")
    }
    fn desc_mut(&mut self, tv: TV) -> &mut Desc {
        self.desc.get_mut(&tv.0).expect("descriptor present")
    }

    /// Resolve a type to its current representative form (substitutes vars).
    pub fn resolve(&mut self, t: &Type) -> Type {
        match t {
            Type::Var(v) => {
                if !self.parent.contains_key(&v.0) {
                    return Type::Var(*v);
                }
                let root = self.find(*v);
                let d = self.desc[&root.0].clone();
                if matches!(d.term, Type::Var(_)) {
                    Type::Var(root)
                } else {
                    self.resolve(&d.term)
                }
            }
            Type::Con(n, ts) => Type::Con(n.clone(), ts.iter().map(|x| self.resolve(x)).collect()),
            Type::Arrow(a, b) => {
                Type::Arrow(Box::new(self.resolve(a)), Box::new(self.resolve(b)))
            }
            Type::Record(r) | Type::Variant(r) => {
                let nr = Row {
                    fields: r.fields.iter().map(|(l, t)| (l.clone(), self.resolve(t))).collect(),
                    tail: r.tail.map(|v| self.find(v)),
                };
                if matches!(t, Type::Record(_)) {
                    Type::Record(nr)
                } else {
                    Type::Variant(nr)
                }
            }
        }
    }

    fn ensure_var(&mut self, tv: TV, owner: Option<crate::ast::NodeId>, role: Option<String>, origins: Vec<OrigPoint>) {
        if !self.parent.contains_key(&tv.0) {
            self.parent.insert(tv.0, tv.0);
            self.desc.insert(
                tv.0,
                Desc { term: Type::Var(tv), rank: self.level, origins },
            );
            if let Some(o) = owner {
                self.var_owner.insert(tv.0, o);
            }
            if let Some(r) = role {
                self.var_role.insert(tv.0, r);
            }
        }
    }

    /// Register a variable without provenance, at rank 1 (used by the
    /// standalone satisfiability checks in unsat-core extraction).
    pub fn ensure_pure(&mut self, tv: TV) {
        if !self.parent.contains_key(&tv.0) {
            self.parent.insert(tv.0, tv.0);
            self.desc.insert(
                tv.0,
                Desc { term: Type::Var(tv), rank: 1, origins: Vec::new() },
            );
            self.var_names.insert(tv.0, format!("t{}", tv.0));
        }
    }

    fn merge_origins(&mut self, a: TV, b: TV) {
        let db = self.desc[&b.0].origins.clone();
        let da = self.desc_mut(a);
        for o in db {
            if !da.origins.contains(&o) {
                da.origins.push(o);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct UnifyFailure {
    pub kind: TypeErrorKind,
    pub nodes: Vec<crate::ast::NodeId>,
    pub detail: String,
}

impl Solver {
    fn occurs(&self, v: TV, t: &Type) -> bool {
        match t {
            Type::Var(x) => v == *x,
            Type::Con(_, ts) => ts.iter().any(|x| self.occurs(v, x)),
            Type::Arrow(a, b) => self.occurs(v, a) || self.occurs(v, b),
            Type::Record(r) | Type::Variant(r) => {
                r.fields.iter().any(|(_, t)| self.occurs(v, t))
                    || r.tail.map(|x| x == v).unwrap_or(false)
            }
        }
    }

    fn origin_nodes(roots: &[TV], desc: &HashMap<u32, Desc>) -> Vec<crate::ast::NodeId> {
        let mut out = Vec::new();
        for r in roots {
            if let Some(d) = desc.get(&r.0) {
                for o in &d.origins {
                    if let Some(n) = o.node {
                        if !out.contains(&n) {
                            out.push(n);
                        }
                    }
                }
            }
        }
        out
    }

    /// Bind unbound variable `v` to term `t` (occurs check + rank merge).
    fn bind(&mut self, v: TV, t: Type) -> Result<(), UnifyFailure> {
        let resolved = self.resolve(&t);
        if let Type::Var(other) = resolved {
            if other == v {
                return Ok(());
            }
            // union the two variables
            let (rv, ro) = (v, other);
            let (lo, hi) = if self.desc[&rv.0].rank <= self.desc[&ro.0].rank {
                (rv, ro)
            } else {
                (ro, rv)
            };
            let rank = self.desc[&hi.0].rank;
            let origins_hi = self.desc[&hi.0].origins.clone();
            let dlo = self.desc_mut(lo);
            dlo.rank = rank;
            for o in origins_hi {
                if !dlo.origins.contains(&o) {
                    dlo.origins.push(o);
                }
            }
            self.parent.insert(hi.0, lo.0);
            return Ok(());
        }
        if self.occurs(v, &resolved) {
            let name = self.name_for(v);
            return Err(UnifyFailure {
                kind: TypeErrorKind::InfiniteType {
                    variable: name.clone(),
                    expanded: resolved.clone(),
                    detail: format!("infinite type: {name} occurs in {}", self.render_type(&resolved)),
                },
                nodes: Self::origin_nodes(&[v], &self.desc),
                detail: format!("occurs check failed: {name} ↩ {}", self.render_type(&resolved)),
            });
        }
        let term_rank = self.term_rank(&resolved);
        let dv = self.desc_mut(v);
        dv.term = resolved;
        dv.rank = dv.rank.min(term_rank);
        Ok(())
    }

    fn term_rank(&self, t: &Type) -> u32 {
        t.vars().iter().map(|v| self.desc.get(&v.0).map(|d| d.rank).unwrap_or(0)).min().unwrap_or(0)
    }

    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<(), UnifyFailure> {
        self.unify_strict(a, b, false)
    }

    fn unify_strict(&mut self, a: &Type, b: &Type, strict_extra: bool) -> Result<(), UnifyFailure> {
        let ra = self.resolve(a);
        let rb = self.resolve(b);
        match (&ra, &rb) {
            (Type::Var(v), _) => {
                let v = *v;
                self.bind(v, rb.clone())
            }
            (_, Type::Var(v)) => {
                let v = *v;
                self.bind(v, ra.clone())
            }
            (Type::Con(n1, ts1), Type::Con(n2, ts2)) if n1 == n2 && ts1.len() == ts2.len() => {
                for (x, y) in ts1.iter().zip(ts2.iter()) {
                    self.unify_strict(x, y, strict_extra)?;
                }
                Ok(())
            }
            (Type::Arrow(a1, b1), Type::Arrow(a2, b2)) => {
                eprintln!("ARROW a1={:?} a2={:?}", a1, a2);
                self.unify_strict(a1, a2, strict_extra)?;
                self.unify_strict(b1, b2, strict_extra)
            }
            (Type::Record(r1), Type::Record(r2)) => self.unify_rows(r1, r2, true, strict_extra),
            (Type::Variant(r1), Type::Variant(r2)) => self.unify_rows(r1, r2, false, strict_extra),
            _ => Err(UnifyFailure {
                kind: TypeErrorKind::Mismatch {
                    expected: ra.clone(),
                    actual: rb.clone(),
                    detail: format!("{} ≠ {}", self.render_type(&ra), self.render_type(&rb)),
                },
                nodes: {
                    let mut roots = ra.vars();
                    roots.extend(rb.vars());
                    Self::origin_nodes(&roots, &self.desc)
                },
                detail: format!("cannot unify {} with {}", self.render_type(&ra), self.render_type(&rb)),
            }),
        }
    }

    /// Row unification. The first row is the *expected* side, the second the
    /// *actual* side: labels expected but absent are "missing", labels present
    /// but not expected are "extra". Open tail variables absorb surplus
    /// fields, which is what gives record/variant row polymorphism.
    fn unify_rows(&mut self, expected: &Row, actual: &Row, is_record: bool, strict_extra: bool) -> Result<(), UnifyFailure> {
        let mut emap: BTreeMap<String, Type> =
            expected.fields.iter().map(|(l, t)| (l.clone(), t.clone())).collect();
        let mut amap: BTreeMap<String, Type> =
            actual.fields.iter().map(|(l, t)| (l.clone(), t.clone())).collect();

        let labels: Vec<String> = {
            let mut ls: Vec<String> = emap.keys().cloned().chain(amap.keys().cloned()).collect();
            ls.sort();
            ls.dedup();
            ls
        };

        // common labels: payload types must agree
        for l in &labels {
            if let (Some(et), Some(at)) = (emap.get(l).cloned(), amap.get(l).cloned()) {
                if let Err(mut e) = self.unify_strict(&et, &at, strict_extra) {
                    e.detail = format!("{} '{l}': {}", item_word(is_record), e.detail);
                    return Err(e);
                }
            }
        }

        let expected_only: Vec<String> =
            labels.iter().filter(|l| emap.contains_key(*l) && !amap.contains_key(*l)).cloned().collect();
        let actual_only: Vec<String> =
            labels.iter().filter(|l| amap.contains_key(*l) && !emap.contains_key(*l)).cloned().collect();

        let e_tail = expected.tail.map(|v| self.find(v));
        let a_tail = actual.tail.map(|v| self.find(v));

        let row_failure = |label: &str, missing: bool, present: &[String]| UnifyFailure {
            kind: if missing {
                TypeErrorKind::RowMissing { label: label.to_string(), present: present.to_vec() }
            } else {
                TypeErrorKind::RowExtra { label: label.to_string(), present: present.to_vec() }
            },
            nodes: Vec::new(),
            detail: format!(
                "{} {} '{label}'（当前有: {}）",
                if missing { "缺少" } else { "多余" },
                item_word(is_record),
                if present.is_empty() { "（无）".to_string() } else { present.join(", ") }
            ),
        };

        match (e_tail, a_tail) {
            (None, None) => {
                if let Some(l) = expected_only.first() {
                    let present: Vec<String> = actual.fields.iter().map(|(x, _)| x.clone()).collect();
                    return Err(row_failure(l, true, &present));
                }
                if let Some(l) = actual_only.first() {
                    let present: Vec<String> = expected.fields.iter().map(|(x, _)| x.clone()).collect();
                    return Err(row_failure(l, false, &present));
                }
            }
            (Some(ev), None) => {
                eprintln!("ROW some/none E={:?} A={:?} eonly={:?} aonly={:?}", expected.fields, actual.fields, expected_only, actual_only); Labels required by the
                // open row but absent from the closed actual are missing.
                // Surplus actual fields are absorbed into the open tail —
                // this is row polymorphism (passing a wider record to a
                // function that reads fewer fields).
                if let Some(l) = expected_only.first().cloned() {
                    let present: Vec<String> = actual.fields.iter().map(|(x, _)| x.clone()).collect();
                    return Err(row_failure(&l, true, &present));
                }
                if strict_extra {
                    if let Some(l) = actual_only.first().cloned() {
                        let present: Vec<String> = expected.fields.iter().map(|(x, _)| x.clone()).collect();
                        return Err(row_failure(&l, false, &present));
                    }
                }
                let absorbed: Vec<(String, Type)> =
                    actual_only.iter().map(|l| (l.clone(), amap.remove(l).unwrap())).collect();
                self.absorb_into_tail(ev, absorbed, is_record, true)?;
            }
            (None, Some(_av)) => {
                // actual is open: it absorbs actual-only labels. Expected-only
                // labels are genuinely missing from the actual value.
                if let Some(l) = expected_only.first().cloned() {
                    let present: Vec<String> = actual.fields.iter().map(|(x, _)| x.clone()).collect();
                    return Err(row_failure(&l, true, &present));
                }
                let absorbed: Vec<(String, Type)> =
                    actual_only.iter().map(|l| (l.clone(), amap.remove(l).unwrap())).collect();
                // nothing to merge: the open actual tail simply admits them
                let _ = absorbed;
            }
            (Some(ev), Some(av)) => {
                // both open: equate the tails and distribute each side's
                // surplus onto the (now shared) tail row.
                self.unify_strict(&Type::Var(ev), &Type::Var(av), strict_extra)?;
                let root = self.find(ev);
                let mut absorbed: Vec<(String, Type)> = expected_only
                    .iter()
                    .map(|l| (l.clone(), emap.remove(l).unwrap()))
                    .chain(actual_only.iter().map(|l| (l.clone(), amap.remove(l).unwrap())))
                    .collect();
                // fold in anything already living on the tail
                if let Type::Record(r) | Type::Variant(r) = self.resolve(&Type::Var(root)) {
                    for (l, t) in r.fields {
                        if !absorbed.iter().any(|(fl, _)| fl == &l) {
                            absorbed.push((l, t));
                        }
                    }
                }
                self.absorb_into_tail(root, absorbed, is_record, false)?;
            }
        }
        Ok(())
    }

    fn absorb_into_tail(
        &mut self,
        tail: TV,
        mut fields: Vec<(String, Type)>,
        is_record: bool,
        close: bool,
    ) -> Result<(), UnifyFailure> {
        let root = self.find(tail);
        let mut inner_tail: Option<TV> = None;
        if let Type::Record(r) | Type::Variant(r) = self.resolve(&Type::Var(root)) {
            for (l, t) in r.fields {
                if !fields.iter().any(|(fl, _)| fl == &l) {
                    fields.push((l, t));
                }
            }
            inner_tail = r.tail;
        }
        if close {
            inner_tail = None;
        }
        let row = if is_record {
            Type::Record(Row { fields, tail: inner_tail })
        } else {
            Type::Variant(Row { fields, tail: inner_tail })
        };
        self.bind(root, row)
    }

}

fn item_word(is_record: bool) -> &'static str {
    if is_record {
        "field"
    } else {
        "tag"
    }
}

fn ra_vars(r: &Row) -> Vec<TV> {
    let mut out: Vec<TV> = r.fields.iter().flat_map(|(_, t)| t.vars()).collect();
    if let Some(v) = r.tail {
        out.push(v);
    }
    out.sort();
    out.dedup();
    out
}

impl Solver {
    fn name_for(&self, tv: TV) -> String {
        self.var_names.get(&tv.0).cloned().unwrap_or_else(|| format!("v{}", tv.0))
    }

    pub fn render_type(&self, t: &Type) -> String {
        let names: HashMap<TV, String> = self.var_names.iter().map(|(k, v)| (TV(*k), v.clone())).collect();
        t.render(&names)
    }

    fn generalize(&mut self, node: crate::ast::NodeId, name: &str, root: &Type, generalized: bool, rec: bool) -> Vec<String> {
        let resolved = self.resolve(root);
        let mut bounds: Vec<TV> = Vec::new();
        if generalized {
            let mut vars = resolved.vars();
            vars.sort();
            for v in vars {
                let r = self.desc[&v.0].rank;
                if r == self.level {
                    bounds.push(v);
                }
            }
        }
        for v in &bounds {
            self.desc_mut(*v).rank = 0;
        }
        let names = bounds.iter().map(|v| self.name_for(*v)).collect();
        self.schemes.insert(
            node,
            Scheme { bounds: bounds.clone(), body: resolved.clone() },
        );
        let _ = rec;
        names
    }

    fn instantiate(&mut self, binder: crate::ast::NodeId, target: TV) -> Result<Vec<String>, String> {
        let scheme = self.schemes.get(&binder).cloned().ok_or_else(|| "no scheme".to_string())?;
        if scheme.bounds.is_empty() {
            // monomorphic: link directly, preserving sharing
            self.unify(&Type::Var(target), &scheme.body).map_err(|e| e.detail)?;
            return Ok(Vec::new());
        }
        let mut mapping: HashMap<u32, u32> = HashMap::new();
        let mut new_names = Vec::new();
        for b in &scheme.bounds {
            let nv = self.fresh_solver_var(self.level);
            mapping.insert(b.0, nv.0);
            let nm = format!("{}#{}", self.var_names.get(&b.0).map(|s| s.as_str()).unwrap_or("t"), nv.0);
            self.var_names.insert(nv.0, nm.clone());
            new_names.push(nm);
        }
        let mapped = crate::gen::remap_type_pub(&scheme.body, &mapping);
        self.unify(&Type::Var(target), &mapped).map_err(|e| e.detail)?;
        Ok(new_names)
    }

    /// Execute `ops`, recording steps.
    ///
    /// Phase 1 runs binding ops (Fresh/BeginLet/Generalize/Instantiate/Note)
    /// to establish scopes and links. Phase 2 unifies structural constraints
    /// with a fixpoint scheduler, so a projection inside a function body sees
    /// the record that only reaches the parameter through an application.
    pub fn run(
        &mut self,
        ops: &[Op],
        disabled: &HashSet<u64>,
        cursor: Option<usize>,
    ) -> RunResult {
        let mut steps: Vec<Step> = Vec::new();
        let limit = cursor.unwrap_or(ops.len()).min(ops.len());

        let mut unify_ops: Vec<(usize, Op)> = Vec::new();

        for (i, op) in ops.iter().enumerate().take(limit) {
            match op {
                Op::Unify { .. } => unify_ops.push((i, op.clone())),
                Op::Fresh { var, owner, role } => {
                    let tv = TV(*var);
                    self.ensure_var(
                        tv,
                        Some(*owner),
                        Some(role.clone()),
                        vec![OrigPoint::node(*owner, role.clone())],
                    );
                    if !self.var_names.contains_key(&tv.0) {
                        self.var_names.insert(tv.0, format!("t{}", tv.0));
                    }
                    steps.push(Step {
                        index: steps.len(),
                        op_index: i,
                        constraint_id: None,
                        kind: "fresh".into(),
                        summary: format!("{} = fresh({role})", self.name_for(tv)),
                        effect: StepEffect::Fresh { var: self.name_for(tv) },
                        level: self.level,
                    });
                }
                Op::BeginLet { node } => {
                    self.level += 1;
                    steps.push(Step {
                        index: steps.len(),
                        op_index: i,
                        constraint_id: Some(node.0),
                        kind: "scope".into(),
                        summary: format!("enter let scope (rank {})", self.level),
                        effect: StepEffect::Scope,
                        level: self.level,
                    });
                }
                Op::Generalize { node, name, root, generalized, rec } => {
                    let resolved = self.resolve(root);
                    let quantified = self.generalize(*node, name, root, *generalized, *rec);
                    self.level = self.level.saturating_sub(1);
                    steps.push(Step {
                        index: steps.len(),
                        op_index: i,
                        constraint_id: Some(node.0),
                        kind: "generalize".into(),
                        summary: format!(
                            "{name}: {}  [{}]",
                            self.render_type(&resolved),
                            if *generalized {
                                format!("generalize {}", quantified.join(" "))
                            } else {
                                "value restriction: kept monomorphic".to_string()
                            }
                        ),
                        effect: StepEffect::Generalized {
                            name: name.clone(),
                            quantified: quantified.clone(),
                        },
                        level: self.level,
                    });
                }
                Op::Instantiate { binder, use_node, name, target } => {
                    let tv = TV(*target);
                    self.ensure_var(tv, None, None, vec![OrigPoint::node(*use_node, "use")]);
                    let r = self.instantiate(*binder, tv);
                    let (summary, effect) = match r {
                        Ok(vars) => (
                            format!("{name} instantiated at use site"),
                            StepEffect::Instantiated { name: name.clone(), vars },
                        ),
                        Err(e) => (
                            format!("{name}: instantiation failed ({e})"),
                            StepEffect::Failed { detail: e },
                        ),
                    };
                    steps.push(Step {
                        index: steps.len(),
                        op_index: i,
                        constraint_id: Some(use_node.0),
                        kind: "instantiate".into(),
                        summary,
                        effect,
                        level: self.level,
                    });
                }
                Op::Note { text, owner } => {
                    steps.push(Step {
                        index: steps.len(),
                        op_index: i,
                        constraint_id: owner.map(|n| n.0),
                        kind: "note".into(),
                        summary: text.clone(),
                        effect: StepEffect::Note,
                        level: self.level,
                    });
                }
            }
        }

        self.schedule_unifications(unify_ops, disabled, &mut steps);

        RunResult { steps, errors: self.errors.clone(), failed: self.failed_ids.clone() }
    }

    /// Fixpoint unification with three ordered bands per pass:
    /// 0: var↔var links, 1: shape↔bare bindings (projections, literals),
    /// 2: shape↔shape comparisons (arrows, rows). Processing lower bands
    /// first propagates a function body's row requirements onto the
    /// parameter before the application compares the function arrow with the
    /// argument. Constraints whose bare side cannot yet link are deferred and
    /// retried; when a pass stalls, survivors are forced and report errors.
    fn schedule_unifications(
        &mut self,
        unify_ops: Vec<(usize, Op)>,
        disabled: &HashSet<u64>,
        steps: &mut Vec<Step>,
    ) {
        let mut pending: Vec<(usize, Op)> = unify_ops;
        pending.sort_by_key(|(i, _)| *i);

        loop {
            // classify and order by (band, source index)
            let mut bands: Vec<(u8, usize, Op)> = pending
                .drain(..)
                .map(|(i, op)| {
                    let band = if let Op::Unify { expected, actual, .. } = &op {
                        let re = self.resolve(expected);
                        let ra = self.resolve(actual);
                        match (matches!(re, Type::Var(_)), matches!(ra, Type::Var(_))) {
                            (true, true) => 0,
                            (false, false) => 2,
                            _ => 1,
                        }
                    } else {
                        2
                    };
                    (band, i, op)
                })
                .collect();
            bands.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

            let mut next: Vec<(usize, Op)> = Vec::new();
            let mut progressed = false;

            for (_band, i, op) in bands {
                let Op::Unify { id, expected, actual, reason, .. } = &op else {
                    continue;
                };
                if disabled.contains(id) {
                    steps.push(Step {
                        index: steps.len(),
                        op_index: i,
                        constraint_id: Some(*id),
                        kind: "disabled".into(),
                        summary: format!("(disabled) {}", reason.describe()),
                        effect: StepEffect::Note,
                        level: self.level,
                    });
                    continue;
                }
                let re = self.resolve(expected);
                let ra = self.resolve(actual);
                let bare_e = matches!(re, Type::Var(_));
                let bare_a = matches!(ra, Type::Var(_));
                let strict = matches!(reason, Reason::MatchCoverage(_));

                if bare_e ^ bare_a {
                    // band 1: bind the bare variable to the shaped side.
                    match self.unify_strict(&expected, &actual, strict) {
                        Ok(()) => {
                            progressed = true;
                            self.push_unify_step(steps, i, *id, reason, &re, &ra);
                        }
                        Err(fail) => {
                            // occurs check or row failure while binding: definite
                            self.record_failure(*id, i, reason, &re, &ra, fail, steps);
                            progressed = true;
                        }
                    }
                    continue;
                }

                // band 0 (both bare) and band 2 (both shaped): apply directly.
                match self.unify_strict(&expected, &actual, strict) {
                    Ok(()) => {
                        progressed = true;
                        self.push_unify_step(steps, i, *id, reason, &re, &ra);
                    }
                    Err(fail) => {
                        // A band-2 clash whose shapes still contain unresolved
                        // tail variables is deferred once (open rows may get
                        // more fields via a later band-1 binding).
                        if bare_e || bare_a {
                            next.push((i, op));
                        } else if open_row_pending(&re, &ra) {
                            next.push((i, op));
                        } else {
                            self.record_failure(*id, i, reason, &re, &ra, fail, steps);
                            progressed = true;
                        }
                    }
                }
            }

            pending = next;
            if pending.is_empty() {
                break;
            }
            if !progressed {
                for (i, op) in pending.drain(..) {
                    if let Op::Unify { id, expected, actual, reason, .. } = &op {
                        if disabled.contains(id) {
                            continue;
                        }
                        let re = self.resolve(expected);
                        let ra = self.resolve(actual);
                        let strict = matches!(reason, Reason::MatchCoverage(_));
                        if let Err(fail) = self.unify_strict(expected, actual, strict) {
                            self.record_failure(*id, i, reason, &re, &ra, fail, steps);
                        }
                    }
                }
                break;
            }
        }
    }

    fn push_unify_step(
        &self,
        steps: &mut Vec<Step>,
        i: usize,
        id: u64,
        reason: &Reason,
        re: &Type,
        ra: &Type,
    ) {
        steps.push(Step {
            index: steps.len(),
            op_index: i,
            constraint_id: Some(id),
            kind: "unify".into(),
            summary: format!(
                "{} ≡ {}   [{}]",
                self.render_type(re),
                self.render_type(ra),
                reason.describe()
            ),
            effect: StepEffect::Linked {
                a: self.render_type(re),
                b: self.render_type(ra),
            },
            level: self.level,
        });
    }

    fn record_failure(
        &mut self,
        id: u64,
        i: usize,
        reason: &Reason,
        re: &Type,
        ra: &Type,
        fail: UnifyFailure,
        steps: &mut Vec<Step>,
    ) {
        self.failed_ids.insert(id);
        self.errors.push(TypeError {
            kind: fail.kind,
            anchor: id,
            nodes: fail.nodes,
            detail: fail.detail.clone(),
        });
        steps.push(Step {
            index: steps.len(),
            op_index: i,
            constraint_id: Some(id),
            kind: "unify-failed".into(),
            summary: format!(
                "✗ {} ≠ {}   [{}]",
                self.render_type(re),
                self.render_type(ra),
                reason.describe()
            ),
            effect: StepEffect::Failed { detail: fail.detail },
            level: self.level,
        });
    }

    #[allow(clippy::too_many_arguments)]
    fn commit_resolved(
        &mut self,
        id: u64,
        i: usize,
        expected: &Type,
        actual: &Type,
        reason: &Reason,
        re: &Type,
        ra: &Type,
        steps: &mut Vec<Step>,
    ) {
        let strict = matches!(reason, Reason::MatchCoverage(_));
        match self.unify_strict(expected, actual, strict) {
            Ok(()) => steps.push(Step {
                index: steps.len(),
                op_index: i,
                constraint_id: Some(id),
                kind: "unify".into(),
                summary: format!(
                    "{} ≡ {}   [{}]",
                    self.render_type(re),
                    self.render_type(ra),
                    reason.describe()
                ),
                effect: StepEffect::Linked {
                    a: self.render_type(re),
                    b: self.render_type(ra),
                },
                level: self.level,
            }),
            Err(fail) => {
                self.failed_ids.insert(id);
                self.errors.push(TypeError {
                    kind: fail.kind,
                    anchor: id,
                    nodes: fail.nodes,
                    detail: fail.detail.clone(),
                });
                steps.push(Step {
                    index: steps.len(),
                    op_index: i,
                    constraint_id: Some(id),
                    kind: "unify-failed".into(),
                    summary: format!(
                        "✗ {} ≠ {}   [{}]",
                        self.render_type(re),
                        self.render_type(ra),
                        reason.describe()
                    ),
                    effect: StepEffect::Failed { detail: fail.detail },
                    level: self.level,
                });
            }
        }
    }

    /// Origins attached to a variable's current representative.
    pub fn public_desc(&self, tv: crate::types::TV) -> Option<Vec<OrigPoint>> {
        let mut root = tv.0;
        if !self.parent.contains_key(&root) {
            return None;
        }
        while self.parent[&root] != root {
            root = self.parent[&root];
        }
        Some(self.desc[&root].origins.clone())
    }

    pub fn final_type(&mut self, tv: TV) -> Type {
        self.resolve(&Type::Var(tv))
    }

    pub fn names(&self) -> HashMap<u32, String> {
        self.var_names.clone()
    }
}

impl Default for Solver {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct RunResult {
    pub steps: Vec<Step>,
    pub errors: Vec<TypeError>,
    pub failed: HashSet<u64>,
}

/// True when a row-vs-row failure involves an open tail that might absorb
/// fields via a later binding (defers the comparison one round).
fn open_row_pending(a: &Type, b: &Type) -> bool {
    fn open(t: &Type) -> bool {
        matches!(t, Type::Record(r) | Type::Variant(r) if r.tail.is_some())
    }
    open(a) || open(b)
}
