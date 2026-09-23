//! Generalization (rank-based), instantiation, rank lowering.

use super::ty::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct QField { pub name: String, pub ty: QTy }

#[derive(Clone, Debug)]
pub enum QRowTail {
    /// generalized row variable
    Open(usize),
    /// monomorphic free row variable
    MonoR(RVarId),
    Closed,
}

#[derive(Clone, Debug)]
pub enum QTy {
    /// generalized type variable
    Q(usize),
    /// monomorphic free variable (kept by id; used only within one analysis)
    Mono(VarId),
    Con(Con),
    Arrow(Box<QTy>, Box<QTy>),
    Row(RowKind, Vec<QField>, QRowTail),
}

#[derive(Clone, Debug)]
pub struct Schema { pub body: QTy }

struct Gen<'a> {
    e: &'a Engine,
    level: u32,
    tmap: HashMap<VarId, usize>,
    rmap: HashMap<RVarId, usize>,
}

impl<'a> Gen<'a> {
    fn tv(&mut self, v: VarId) -> usize {
        if let Some(i) = self.tmap.get(&v) { return *i; }
        let i = self.tmap.len();
        self.tmap.insert(v, i);
        i
    }
    fn rv(&mut self, v: RVarId) -> usize {
        if let Some(i) = self.rmap.get(&v) { return *i; }
        let i = self.rmap.len();
        self.rmap.insert(v, i);
        i
    }
    fn go_t(&mut self, t: &Ty) -> QTy {
        match self.e.resolve(t) {
            Ty::Var(v) => {
                if self.e.tvar_rank(v) > self.level { QTy::Q(self.tv(v)) }
                else { QTy::Mono(v) }
            }
            Ty::Con(c) => QTy::Con(c),
            Ty::Arrow(a, b) => QTy::Arrow(Box::new(self.go_t(&a)), Box::new(self.go_t(&b))),
            Ty::Row(k, fs, tail) => {
                // normalize: flatten bound rvar prefixes
                let (fs, tail) = flatten(self.e, fs, tail);
                let qfs = fs.iter().map(|f| QField { name: f.name.clone(), ty: self.go_t(&f.ty) }).collect();
                let qtail = match tail {
                    Tail::Closed => QRowTail::Closed,
                    Tail::Open(rv) => match self.e.rvar(rv) {
                        RVar::Free { rank, .. } if rank > self.level => QRowTail::Open(self.rv(rv)),
                        RVar::Free { .. } => QRowTail::MonoR(rv),
                        RVar::Bound(_) => QRowTail::Closed,
                    },
                };
                QTy::Row(k, qfs, qtail)
            }
        }
    }
}

fn flatten(e: &Engine, mut acc: Vec<Field>, tail: Tail) -> (Vec<Field>, Tail) {
    let mut cur = tail;
    loop {
        match cur {
            Tail::Closed => return (acc, Tail::Closed),
            Tail::Open(rv) => match e.rvar(rv) {
                RVar::Free { .. } => return (acc, Tail::Open(rv)),
                RVar::Bound(entry) => {
                    for f in entry.fields { acc.push(f); }
                    cur = entry.tail;
                }
            },
        }
    }
}

/// Generalize all free variables with rank > level.
pub fn generalize(e: &Engine, t: &Ty, level: u32) -> Schema {
    let mut g = Gen { e, level, tmap: HashMap::new(), rmap: HashMap::new() };
    Schema { body: g.go_t(t) }
}

/// Lower the rank of every unbound variable reachable from `roots` that is
/// above `level` to `level` (value restriction / scope exit).
pub fn lower_ranks(e: &Engine, roots: &[Ty], level: u32) {
    let mut seen_t: Vec<VarId> = Vec::new();
    let mut seen_r: Vec<RVarId> = Vec::new();
    fn walk_t(e: &Engine, t: &Ty, st: &mut Vec<VarId>, sr: &mut Vec<RVarId>) {
        match e.resolve(t) {
            Ty::Var(v) => if !st.contains(&v) { st.push(v); },
            Ty::Con(_) => {}
            Ty::Arrow(a, b) => { walk_t(e, &a, st, sr); walk_t(e, &b, st, sr); }
            Ty::Row(_, fs, tail) => {
                for f in fs { walk_t(e, &f.ty, st, sr); }
                if let Tail::Open(rv) = tail { walk_r(e, rv, st, sr); }
            }
        }
    }
    fn walk_r(e: &Engine, rv: RVarId, st: &mut Vec<VarId>, sr: &mut Vec<RVarId>) {
        if sr.contains(&rv) { return; }
        sr.push(rv);
        if let RVar::Bound(entry) = e.rvar(rv) {
            for f in entry.fields { walk_t(e, &f.ty, st, sr); }
            if let Tail::Open(r2) = entry.tail { walk_r(e, r2, st, sr); }
        }
    }
    for r in roots { walk_t(e, r, &mut seen_t, &mut seen_r); }
    for v in seen_t {
        if let TVar::Unbound { rank } = e.tvars.borrow()[v as usize].clone() {
            if rank > level { e.set_tvar_rank(v, level); }
        }
    }
    for rv in seen_r {
        if let RVar::Free { rank, .. } = e.rvar(rv) {
            if rank > level {
                e.rvars.borrow_mut()[rv as usize] = RVar::Free {
                    rank: level,
                    entry: REntry { fields: Vec::new(), tail: Tail::Open(rv) },
                };
            }
        }
    }
}

struct Inst<'a> {
    e: &'a Engine,
    level: u32,
    prov: Option<ProvTag>,
    tmap: HashMap<usize, VarId>,
    rmap: HashMap<usize, RVarId>,
}

impl<'a> Inst<'a> {
    fn tv(&mut self, q: usize) -> VarId {
        if let Some(v) = self.tmap.get(&q) { return *v; }
        let t = self.e.fresh(self.level, self.prov);
        if let Ty::Var(v) = t { self.tmap.insert(q, v); v } else { unreachable!() }
    }
    fn rv(&mut self, q: usize, kind: RowKind) -> RVarId {
        if let Some(v) = self.rmap.get(&q) { return *v; }
        let v = self.e.fresh_row_var(self.level, kind, self.prov);
        self.rmap.insert(q, v);
        v
    }
    fn go(&mut self, t: &QTy) -> Ty {
        match t {
            QTy::Q(q) => Ty::Var(self.tv(*q)),
            QTy::Mono(v) => Ty::Var(*v),
            QTy::Con(c) => Ty::Con(*c),
            QTy::Arrow(a, b) => Ty::Arrow(Box::new(self.go(a)), Box::new(self.go(b))),
            QTy::Row(k, fs, tail) => {
                let fs = fs.iter().map(|f| Field { name: f.name.clone(), ty: self.go(&f.ty) }).collect();
                let tail = match tail {
                    QRowTail::Closed => Tail::Closed,
                    QRowTail::Open(q) => Tail::Open(self.rv(*q, *k)),
                    QRowTail::MonoR(rv) => Tail::Open(*rv),
                };
                Ty::Row(*k, fs, tail)
            }
        }
    }
}

pub fn instantiate(e: &Engine, s: &Schema, level: u32, prov: Option<ProvTag>) -> Ty {
    let mut i = Inst { e, level, prov, tmap: HashMap::new(), rmap: HashMap::new() };
    i.go(&s.body)
}
