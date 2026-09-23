//! Type representation: union-find variables, levels (ranks), row polymorphism.

use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashMap;

pub type VarId = u32;
pub type RVarId = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Con { Int, Bool, Str, Unit }

impl Con {
    pub fn name(self) -> &'static str {
        match self { Con::Int => "Int", Con::Bool => "Bool", Con::Str => "String", Con::Unit => "Unit" }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum RowKind { Record, Variant }

impl RowKind {
    pub fn brace(self) -> (&'static str, &'static str) {
        match self {
            RowKind::Record => ("{", "}"),
            RowKind::Variant => ("[", "]"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Tail {
    Open(RVarId),
    Closed,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
pub enum Ty {
    Var(VarId),
    Con(Con),
    Arrow(Box<Ty>, Box<Ty>),
    Row(RowKind, Vec<Field>, Tail),
}

// ---------- provenance ----------

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum ProvTag {
    /// variable introduced for a specific AST node
    Node(u64),
    /// variable introduced for a named binder occurrence
    Binder(u64),
    /// unknown / merged
    Unknown,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct Provs {
    pub tags: Vec<ProvTag>,
}

impl Provs {
    pub fn one(t: ProvTag) -> Self { Provs { tags: vec![t] } }
    pub fn merge(&mut self, other: &Provs) {
        for t in &other.tags {
            if !self.tags.contains(t) { self.tags.push(*t); }
        }
    }
    fn node(t: ProvTag) -> Option<u64> {
        match t { ProvTag::Node(n) => Some(n), ProvTag::Binder(n) => Some(n), ProvTag::Unknown => None }
    }
    pub fn first_node(&self) -> Option<u64> {
        for t in &self.tags { if let Some(n) = Provs::node(*t) { return Some(n); } }
        None
    }
}

// ---------- union-find ----------

#[derive(Clone, Debug)]
pub enum TVar {
    Unbound { rank: u32 },
    Link(Ty),
}

#[derive(Clone, Debug)]
pub struct REntry {
    pub fields: Vec<Field>,
    pub tail: Tail,
}

#[derive(Clone, Debug)]
pub enum RVar {
    /// still free at a rank, with accumulated row prefix fields
    Free { rank: u32, entry: REntry },
    Bound(REntry),
}

pub struct Engine {
    pub tvars: RefCell<Vec<TVar>>,
    pub rvars: RefCell<Vec<RVar>>,
    pub provs: RefCell<HashMap<u64, Provs>>,
    next_t: RefCell<u32>,
    next_r: RefCell<u32>,
}

impl Engine {
    pub fn new() -> Self {
        Engine {
            tvars: RefCell::new(Vec::new()),
            rvars: RefCell::new(Vec::new()),
            provs: RefCell::new(HashMap::new()),
            next_t: RefCell::new(0),
            next_r: RefCell::new(0),
        }
    }

    pub fn snapshot_counter(&self) -> (u32, u32) { (*self.next_t.borrow(), *self.next_r.borrow()) }
    pub fn ensure_counter(&self, nt: u32, nr: u32) {
        let mut a = self.next_t.borrow_mut();
        if *a < nt { *a = nt; }
        drop(a);
        let mut b = self.next_r.borrow_mut();
        if *b < nr { *b = nr; }
    }

    fn key_t(v: VarId) -> u64 { (v as u64) | (1u64 << 63) }
    fn key_r(v: RVarId) -> u64 { (v as u64) | (1u64 << 62) }

    pub fn prov_t(&self, v: VarId) -> Provs {
        self.provs.borrow().get(&Engine::key_t(v)).cloned().unwrap_or_default()
    }
    pub fn prov_r(&self, v: RVarId) -> Provs {
        self.provs.borrow().get(&Engine::key_r(v)).cloned().unwrap_or_default()
    }
    pub fn set_prov_t(&self, v: VarId, p: Provs) { self.provs.borrow_mut().insert(Engine::key_t(v), p); }
    pub fn set_prov_r(&self, v: RVarId, p: Provs) { self.provs.borrow_mut().insert(Engine::key_r(v), p); }
    pub fn merge_prov_t(&self, v: VarId, p: &Provs) {
        let mut mp = self.provs.borrow_mut();
        let e = mp.entry(Engine::key_t(v)).or_default();
        e.merge(p);
    }

    pub fn fresh(&self, rank: u32, prov: Option<ProvTag>) -> Ty {
        let id = *self.next_t.borrow();
        *self.next_t.borrow_mut() += 1;
        self.tvars.borrow_mut().push(TVar::Unbound { rank });
        if let Some(t) = prov { self.set_prov_t(id, Provs::one(t)); }
        Ty::Var(id)
    }

    pub fn fresh_row_var(&self, rank: u32, kind: RowKind, prov: Option<ProvTag>) -> RVarId {
        let id = *self.next_r.borrow();
        *self.next_r.borrow_mut() += 1;
        self.rvars.borrow_mut().push(RVar::Free {
            rank,
            entry: REntry { fields: Vec::new(), tail: Tail::Open(id) },
        });
        if let Some(t) = prov { self.set_prov_r(id, Provs::one(t)); }
        let _ = kind;
        id
    }

    pub fn fresh_open_row(&self, rank: u32, kind: RowKind, prov: Option<ProvTag>) -> Ty {
        let rv = self.fresh_row_var(rank, kind, prov);
        Ty::Row(kind, Vec::new(), Tail::Open(rv))
    }

    // ---------- resolution ----------

    pub fn resolve(&self, t: &Ty) -> Ty {
        match t {
            Ty::Var(v) => {
                let rep = { self.tvars.borrow().get(*v as usize).cloned() };
                match rep {
                    Some(TVar::Link(to)) => {
                        let r = self.resolve(&to);
                        self.tvars.borrow_mut()[*v as usize] = TVar::Link(r.clone());
                        r
                    }
                    _ => t.clone(),
                }
            }
            Ty::Arrow(a, b) => Ty::Arrow(Box::new(self.resolve(a)), Box::new(self.resolve(b))),
            Ty::Row(kind, fields, tail) => {
                let fields: Vec<Field> = fields.iter().map(|f| Field { name: f.name.clone(), ty: self.resolve(&f.ty) }).collect();
                match tail {
                    Tail::Open(rv) => {
                        let rep = { self.rvars.borrow().get(*rv as usize).cloned() };
                        match rep {
                            Some(RVar::Bound(entry)) => {
                                let mut entry_fields: Vec<Field> = entry.fields.iter().map(|f| Field { name: f.name.clone(), ty: self.resolve(&f.ty) }).collect();
                                let sub = self.resolve(&Ty::Row(*kind, std::mem::take(&mut entry_fields), entry.tail.clone()));
                                if let Ty::Row(_, mut more, tail) = sub {
                                    let mut all = fields;
                                    all.append(&mut more);
                                    Ty::Row(*kind, all, tail)
                                } else { unreachable!() }
                            }
                            _ => Ty::Row(*kind, fields, Tail::Open(*rv)),
                        }
                    }
                    Tail::Closed => Ty::Row(*kind, fields, Tail::Closed),
                }
            }
            c => c.clone(),
        }
    }

    pub fn tvar_rank(&self, v: VarId) -> u32 {
        match self.tvars.borrow()[v as usize] {
            TVar::Unbound { rank } => rank,
            TVar::Link(_) => panic!("rank of linked var"),
        }
    }
    pub fn set_tvar_rank(&self, v: VarId, rank: u32) {
        if let TVar::Unbound { rank: r } = &mut self.tvars.borrow_mut()[v as usize] {
            *r = rank;
        }
    }
    pub fn link_t(&self, v: VarId, t: Ty) {
        self.tvars.borrow_mut()[v as usize] = TVar::Link(t);
    }
    pub fn bind_r(&self, rv: RVarId, entry: REntry) {
        self.rvars.borrow_mut()[rv as usize] = RVar::Bound(entry);
    }
    pub fn rvar(&self, rv: RVarId) -> RVar { self.rvars.borrow()[rv as usize].clone() }

    /// All root type variables reachable from t.
    pub fn collect_vars(&self, t: &Ty, out: &mut Vec<VarId>) {
        match self.resolve(t) {
            Ty::Var(v) => if !out.contains(&v) { out.push(v) },
            Ty::Con(_) => {}
            Ty::Arrow(a, b) => { self.collect_vars(&a, out); self.collect_vars(&b, out); }
            Ty::Row(_, fs, _) => for f in fs { self.collect_vars(&f.ty, out); },
        }
    }
}
