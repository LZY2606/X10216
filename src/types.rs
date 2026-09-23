use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// Provenance: set of constraint ids that produced a type node.
pub type Prov = BTreeSet<u32>;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    Var(u32, Prov),
    Int(Prov),
    Bool(Prov),
    Str(Prov),
    Fun(Prov, Box<Type>, Box<Type>),
    Record(Prov, Row),
    Variant(Prov, Row),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Row {
    Empty(Prov),
    Var(u32, Prov),
    Extend(Prov, String, Box<Type>, Box<Row>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<u32>,
    pub row_vars: Vec<u32>,
    pub ty: Type,
}

impl Type {
    pub fn prov(&self) -> &Prov {
        match self {
            Type::Var(_, p) | Type::Int(p) | Type::Bool(p) | Type::Str(p) => p,
            Type::Fun(p, _, _) | Type::Record(p, _) | Type::Variant(p, _) => p,
        }
    }
    pub fn prov_mut(&mut self) -> &mut Prov {
        match self {
            Type::Var(_, p) | Type::Int(p) | Type::Bool(p) | Type::Str(p) => p,
            Type::Fun(p, _, _) | Type::Record(p, _) | Type::Variant(p, _) => p,
        }
    }
    pub fn fresh_var(id: u32) -> Type {
        Type::Var(id, Prov::new())
    }
    /// Union `extra` into every node's provenance set.
    pub fn stamp(&mut self, extra: &Prov) {
        if extra.is_empty() {
            return;
        }
        self.prov_mut().extend(extra.iter().copied());
        match self {
            Type::Fun(_, a, b) => {
                a.stamp(extra);
                b.stamp(extra);
            }
            Type::Record(_, r) | Type::Variant(_, r) => r.stamp(extra),
            _ => {}
        }
    }
    pub fn free_vars(&self, tvs: &mut BTreeSet<u32>, rvs: &mut BTreeSet<u32>) {
        match self {
            Type::Var(v, _) => {
                tvs.insert(*v);
            }
            Type::Fun(_, a, b) => {
                a.free_vars(tvs, rvs);
                b.free_vars(tvs, rvs);
            }
            Type::Record(_, r) | Type::Variant(_, r) => r.free_vars(tvs, rvs),
            _ => {}
        }
    }
    /// Structural equality ignoring provenance.
    pub fn shape_eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Var(a, _), Type::Var(b, _)) => a == b,
            (Type::Int(_), Type::Int(_)) => true,
            (Type::Bool(_), Type::Bool(_)) => true,
            (Type::Str(_), Type::Str(_)) => true,
            (Type::Fun(_, a1, b1), Type::Fun(_, a2, b2)) => a1.shape_eq(a2) && b1.shape_eq(b2),
            (Type::Record(_, r1), Type::Record(_, r2)) => r1.shape_eq(r2),
            (Type::Variant(_, r1), Type::Variant(_, r2)) => r1.shape_eq(r2),
            _ => false,
        }
    }
}

impl Row {
    pub fn prov(&self) -> &Prov {
        match self {
            Row::Empty(p) | Row::Var(_, p) => p,
            Row::Extend(p, _, _, _) => p,
        }
    }
    pub fn prov_mut(&mut self) -> &mut Prov {
        match self {
            Row::Empty(p) | Row::Var(_, p) => p,
            Row::Extend(p, _, _, _) => p,
        }
    }
    pub fn stamp(&mut self, extra: &Prov) {
        if extra.is_empty() {
            return;
        }
        self.prov_mut().extend(extra.iter().copied());
        if let Row::Extend(_, _, t, rest) = self {
            t.stamp(extra);
            rest.stamp(extra);
        }
    }
    pub fn free_vars(&self, tvs: &mut BTreeSet<u32>, rvs: &mut BTreeSet<u32>) {
        match self {
            Row::Var(v, _) => {
                rvs.insert(*v);
            }
            Row::Extend(_, _, t, rest) => {
                t.free_vars(tvs, rvs);
                rest.free_vars(tvs, rvs);
            }
            Row::Empty(_) => {}
        }
    }
    pub fn shape_eq(&self, other: &Row) -> bool {
        match (self, other) {
            (Row::Empty(_), Row::Empty(_)) => true,
            (Row::Var(a, _), Row::Var(b, _)) => a == b,
            (Row::Extend(_, l1, t1, r1), Row::Extend(_, l2, t2, r2)) => {
                l1 == l2 && t1.shape_eq(t2) && r1.shape_eq(r2)
            }
            _ => false,
        }
    }
}

/// Substitution for type vars and row vars.
#[derive(Clone, Debug, Default)]
pub struct Subst {
    pub tmap: BTreeMap<u32, Type>,
    pub rmap: BTreeMap<u32, Row>,
}

impl Subst {
    pub fn zonk(&self, t: &Type) -> Type {
        match t {
            Type::Var(v, p) => {
                if let Some(b) = self.tmap.get(v) {
                    let mut z = self.zonk(b);
                    let mut prov = z.prov().clone();
                    prov.extend(p.iter().copied());
                    *z.prov_mut() = prov;
                    z
                } else {
                    t.clone()
                }
            }
            Type::Fun(p, a, b) => Type::Fun(p.clone(), Box::new(self.zonk(a)), Box::new(self.zonk(b))),
            Type::Record(p, r) => Type::Record(p.clone(), self.zonk_row(r)),
            Type::Variant(p, r) => Type::Variant(p.clone(), self.zonk_row(r)),
            _ => t.clone(),
        }
    }
    pub fn zonk_row(&self, r: &Row) -> Row {
        match r {
            Row::Var(v, p) => {
                if let Some(b) = self.rmap.get(v) {
                    let mut z = self.zonk_row(b);
                    let mut prov = z.prov().clone();
                    prov.extend(p.iter().copied());
                    *z.prov_mut() = prov;
                    z
                } else {
                    r.clone()
                }
            }
            Row::Extend(p, l, t, rest) => Row::Extend(
                p.clone(),
                l.clone(),
                Box::new(self.zonk(t)),
                Box::new(self.zonk_row(rest)),
            ),
            Row::Empty(_) => r.clone(),
        }
    }
}

pub fn union_prov(a: &Prov, b: &Prov) -> Prov {
    let mut p = a.clone();
    p.extend(b.iter().copied());
    p
}

// ---------- pretty printing ----------

pub fn pretty_type(t: &Type) -> String {
    let mut names: BTreeMap<u32, String> = BTreeMap::new();
    let mut rnames: BTreeMap<u32, String> = BTreeMap::new();
    let (mut tvs, mut rvs) = (BTreeSet::new(), BTreeSet::new());
    t.free_vars(&mut tvs, &mut rvs);
    for (i, v) in tvs.iter().enumerate() {
        names.insert(*v, format!("t{}", i));
    }
    for (i, v) in rvs.iter().enumerate() {
        rnames.insert(*v, format!("r{}", i));
    }
    pretty_t(t, &names, &rnames)
}

fn pretty_t(t: &Type, names: &BTreeMap<u32, String>, rnames: &BTreeMap<u32, String>) -> String {
    match t {
        Type::Var(v, _) => names
            .get(v)
            .cloned()
            .unwrap_or_else(|| format!("t{}", v)),
        Type::Int(_) => "Int".into(),
        Type::Bool(_) => "Bool".into(),
        Type::Str(_) => "Str".into(),
        Type::Fun(_, a, b) => {
            let l = match **a {
                Type::Fun(..) => format!("({})", pretty_t(a, names, rnames)),
                _ => pretty_t(a, names, rnames),
            };
            format!("{} -> {}", l, pretty_t(b, names, rnames))
        }
        Type::Record(_, r) => format!("{{{}}}", pretty_row(r, names, rnames)),
        Type::Variant(_, r) => format!("<{}>", pretty_row(r, names, rnames)),
    }
}

fn pretty_row(r: &Row, names: &BTreeMap<u32, String>, rnames: &BTreeMap<u32, String>) -> String {
    let mut fields = Vec::new();
    let mut cur = r;
    loop {
        match cur {
            Row::Empty(_) => break,
            Row::Var(v, _) => {
                let tail = rnames.get(v).cloned().unwrap_or_else(|| format!("r{}", v));
                if fields.is_empty() {
                    return format!("..{}", tail);
                }
                return format!("{} | ..{}", fields.join(", "), tail);
            }
            Row::Extend(_, l, t, rest) => {
                fields.push(format!("{}: {}", l, pretty_t(t, names, rnames)));
                cur = rest;
            }
        }
    }
    fields.join(", ")
}

pub fn pretty_scheme(s: &Scheme) -> String {
    if s.vars.is_empty() && s.row_vars.is_empty() {
        pretty_type(&s.ty)
    } else {
        let mut parts: Vec<String> = s.vars.iter().map(|v| format!("t{}", v)).collect();
        parts.extend(s.row_vars.iter().map(|v| format!("r{}", v)));
        format!("forall {}. {}", parts.join(" "), pretty_type(&s.ty))
    }
}
