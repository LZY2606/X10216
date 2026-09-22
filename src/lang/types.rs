//! Types, rows and a union-find store.
//!
//! Type variables and row variables share one id space but carry a [`Kind`];
//! a single store holds both kinds of bindings.

use crate::lang::span::Span;
use serde::Serialize;
use std::collections::HashMap;

pub type VarId = u32;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum Kind {
    Type,
    Row,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Ty {
    Var(VarId),
    Con(Prim),
    Arrow(Box<Ty>, Box<Ty>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum Prim {
    Int,
    Bool,
    Unit,
    Str,
}

impl Prim {
    pub fn name(self) -> &'static str {
        match self {
            Prim::Int => "Int",
            Prim::Bool => "Bool",
            Prim::Unit => "Unit",
            Prim::Str => "String",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct RowField {
    pub label: String,
    pub ty: Ty,
    /// Origin expression span of this field occurrence.
    pub at: Span,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Row {
    pub fields: Vec<RowField>,
    /// `None` means a closed row; `Some(v)` an open row variable.
    pub tail: Option<VarId>,
}

impl Ty {
    pub fn arrow(from: Ty, to: Ty) -> Ty {
        Ty::Arrow(Box::new(from), Box::new(to))
    }
}

impl Row {
    pub fn closed(mut fields: Vec<RowField>) -> Row {
        fields.sort_by(|a, b| a.label.cmp(&b.label));
        Row { fields, tail: None }
    }
    pub fn open(mut fields: Vec<RowField>, tail: VarId) -> Row {
        fields.sort_by(|a, b| a.label.cmp(&b.label));
        Row {
            fields,
            tail: Some(tail),
        }
    }
    pub fn empty_closed() -> Row {
        Row {
            fields: Vec::new(),
            tail: None,
        }
    }
    pub fn empty_open(tail: VarId) -> Row {
        Row {
            fields: Vec::new(),
            tail: Some(tail),
        }
    }
}

/// A polymorphic scheme: `forall qs. body`.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Scheme {
    pub quantifiers: Vec<VarId>,
    pub body: Ty,
}

#[derive(Clone, Debug)]
pub struct Store {
    next: VarId,
    /// Unbound type vars are absent. Bound type vars point at a type.
    ty_bind: HashMap<VarId, Ty>,
    /// Unbound row vars are absent. Bound row vars point at a row (whose tail
    /// may itself be another row variable).
    row_bind: HashMap<VarId, Row>,
    kinds: HashMap<VarId, Kind>,
}

impl Default for Store {
    fn default() -> Self {
        Store {
            next: 0,
            ty_bind: HashMap::new(),
            row_bind: HashMap::new(),
            kinds: HashMap::new(),
        }
    }
}

impl Store {
    pub fn new() -> Store {
        Store::default()
    }

    pub fn fresh(&mut self, kind: Kind) -> VarId {
        let id = self.next;
        self.next += 1;
        self.kinds.insert(id, kind);
        id
    }

    pub fn fresh_ty(&mut self) -> Ty {
        Ty::Var(self.fresh(Kind::Type))
    }

    pub fn fresh_row(&mut self) -> VarId {
        self.fresh(Kind::Row)
    }

    pub fn kind(&self, v: VarId) -> Kind {
        *self.kinds.get(&v).unwrap_or(&Kind::Type)
    }

    pub fn alloc_count(&self) -> u32 {
        self.next
    }

    /// Representative of a type variable with full path compression.
    pub fn root_ty(&mut self, v: VarId) -> VarId {
        let mut cur = v;
        let mut path = Vec::new();
        loop {
            match self.ty_bind.get(&cur).cloned() {
                Some(Ty::Var(w)) if w != cur => {
                    path.push(cur);
                    cur = w;
                }
                _ => {
                    for p in path {
                        if let Some(t) = self.ty_bind.get(&cur).cloned() {
                            self.ty_bind.insert(p, t);
                        }
                    }
                    return cur;
                }
            }
        }
    }

    /// Terminal row variable: follow rows that are merely variable aliases
    /// (`Row { fields: [], tail: Some(w) }`).
    pub fn root_row(&self, v: VarId) -> VarId {
        let mut cur = v;
        let mut guard = 0u32;
        loop {
            guard += 1;
            if guard > 1_000_000 {
                panic!("row alias cycle");
            }
            match self.row_bind.get(&cur) {
                Some(Row {
                    fields,
                    tail: Some(w),
                }) if fields.is_empty() && *w != cur => cur = *w,
                _ => return cur,
            }
        }
    }

    pub fn prune(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(v) => match self.ty_bind.get(v) {
                Some(t) => self.prune(t),
                None => Ty::Var(*v),
            },
            Ty::Con(p) => Ty::Con(*p),
            Ty::Arrow(a, b) => Ty::arrow(self.prune(a), self.prune(b)),
            Ty::Record(r) => Ty::Record(self.prune_row(r)),
            Ty::Variant(r) => Ty::Variant(self.prune_row(r)),
        }
    }

    /// Flatten a row, following bound row tails. Result fields are sorted.
    pub fn prune_row(&self, row: &Row) -> Row {
        let mut collected: Vec<RowField> = Vec::new();
        let mut tail = row.tail;
        let initial: Vec<RowField> = row.fields.clone();
        let mut queue = initial;
        let mut guard = 0u32;
        loop {
            guard += 1;
            if guard > 1_000_000 {
                panic!("row expansion cycle");
            }
            collected.append(&mut queue);
            match tail {
                None => {
                    return Row::closed(collected);
                }
                Some(v) => match self.row_bind.get(&v) {
                    Some(r) => {
                        queue = r.fields.clone();
                        tail = r.tail;
                    }
                    None => {
                        collected.sort_by(|a, b| a.label.cmp(&b.label));
                        return Row {
                            fields: collected,
                            tail: Some(v),
                        };
                    }
                },
            }
        }
    }

    pub fn bind_ty(&mut self, v: VarId, ty: Ty) {
        self.ty_bind.insert(v, ty);
    }

    pub fn alias_ty(&mut self, from: VarId, to: VarId) {
        self.ty_bind.insert(from, Ty::Var(to));
    }

    pub fn bind_row(&mut self, v: VarId, row: Row) {
        self.row_bind.insert(v, row);
    }

    /// Free (currently unbound) variables of a type.
    pub fn free_vars(&self, ty: &Ty, out: &mut Vec<VarId>) {
        match self.prune(ty) {
            Ty::Var(v) => {
                if !out.contains(&v) {
                    out.push(v);
                }
            }
            Ty::Con(_) => {}
            Ty::Arrow(a, b) => {
                self.free_vars(&a, out);
                self.free_vars(&b, out);
            }
            Ty::Record(r) | Ty::Variant(r) => {
                for f in &r.fields {
                    self.free_vars(&f.ty, out);
                }
                if let Some(t) = r.tail {
                    if !out.contains(&t) {
                        out.push(t);
                    }
                }
            }
        }
    }

    /// Deep-copy a scheme, allocating fresh variables for its quantifiers.
    pub fn instantiate(&mut self, scheme: &Scheme) -> (Ty, HashMap<VarId, VarId>) {
        let mut map = HashMap::new();
        for &q in &scheme.quantifiers {
            let kind = self.kind(q);
            map.insert(q, self.fresh(kind));
        }
        let body = clone_ty(&scheme.body, &map);
        (body, map)
    }
}

fn clone_ty(ty: &Ty, map: &HashMap<VarId, VarId>) -> Ty {
    match ty {
        Ty::Var(v) => map.get(v).copied().map(Ty::Var).unwrap_or(Ty::Var(*v)),
        Ty::Con(p) => Ty::Con(*p),
        Ty::Arrow(a, b) => Ty::arrow(clone_ty(a, map), clone_ty(b, map)),
        Ty::Record(r) => Ty::Record(clone_row(r, map)),
        Ty::Variant(r) => Ty::Variant(clone_row(r, map)),
    }
}

fn clone_row(row: &Row, map: &HashMap<VarId, VarId>) -> Row {
    Row {
        fields: row
            .fields
            .iter()
            .map(|f| RowField {
                label: f.label.clone(),
                ty: clone_ty(&f.ty, map),
                at: f.at,
            })
            .collect(),
        tail: row.tail.map(|t| *map.get(&t).unwrap_or(&t)),
    }
}
