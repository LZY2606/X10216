use crate::infer::Constraint;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

pub type Var = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Origin {
    Node(usize),
    Constraint(u64),
    Builtin,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TyKind {
    Var(Var),
    Int,
    Bool,
    Fun(Box<Ty>, Box<Ty>),
    Record(Row),
    Variant(Row),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Ty {
    pub kind: TyKind,
    pub origin: Origin,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RowKind {
    Var(Var),
    Empty,
    Extend { label: String, value: Box<Ty>, rest: Box<Row> },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Row {
    pub kind: RowKind,
    pub origin: Origin,
}

impl Ty {
    pub fn var(v: Var, origin: Origin) -> Self { Self { kind: TyKind::Var(v), origin } }
    pub fn int(origin: Origin) -> Self { Self { kind: TyKind::Int, origin } }
    pub fn bool(origin: Origin) -> Self { Self { kind: TyKind::Bool, origin } }
    pub fn fun(a: Ty, b: Ty, origin: Origin) -> Self { Self { kind: TyKind::Fun(Box::new(a), Box::new(b)), origin } }
    pub fn record(r: Row, origin: Origin) -> Self { Self { kind: TyKind::Record(r), origin } }
    pub fn variant(r: Row, origin: Origin) -> Self { Self { kind: TyKind::Variant(r), origin } }
    pub fn vars(&self, out: &mut HashSet<Var>) {
        match &self.kind {
            TyKind::Var(v) => { out.insert(*v); }
            TyKind::Int | TyKind::Bool => {}
            TyKind::Fun(a,b) => { a.vars(out); b.vars(out); }
            TyKind::Record(r) | TyKind::Variant(r) => r.vars(out),
        }
    }
    pub fn map_vars(&self, f: &mut impl FnMut(Var) -> Ty) -> Ty {
        match &self.kind {
            TyKind::Var(v) => f(*v),
            TyKind::Int | TyKind::Bool => self.clone(),
            TyKind::Fun(a,b) => Ty::fun(a.map_vars(f), b.map_vars(f), self.origin),
            TyKind::Record(r) => Ty::record(r.map_vars(f), self.origin),
            TyKind::Variant(r) => Ty::variant(r.map_vars(f), self.origin),
        }
    }
    pub fn display(&self, uf: &UnionFind) -> String {
        match &self.kind {
            TyKind::Var(v) => match uf.resolve_ty(self) { Some(t) => t.display(uf), None => format!("?{v}") },
            TyKind::Int => "Int".into(),
            TyKind::Bool => "Bool".into(),
            TyKind::Fun(a,b) => format!("({} -> {})", a.display(uf), b.display(uf)),
            TyKind::Record(r) => format!("{{{}}}", r.display(uf)),
            TyKind::Variant(r) => format!("[{}]", r.display(uf)),
        }
    }
}

impl Row {
    pub fn var(v: Var, origin: Origin) -> Self { Self { kind: RowKind::Var(v), origin } }
    pub fn empty(origin: Origin) -> Self { Self { kind: RowKind::Empty, origin } }
    pub fn extend(label: impl Into<String>, value: Ty, rest: Row, origin: Origin) -> Self {
        Self { kind: RowKind::Extend { label: label.into(), value: Box::new(value), rest: Box::new(rest) }, origin }
    }
    pub fn vars(&self, out: &mut HashSet<Var>) {
        match &self.kind {
            RowKind::Var(v) => { out.insert(*v); }
            RowKind::Empty => {}
            RowKind::Extend { value, rest, .. } => { value.vars(out); rest.vars(out); }
        }
    }
    pub fn map_vars(&self, f: &mut impl FnMut(Var) -> Ty) -> Row {
        match &self.kind {
            RowKind::Var(v) => match f(*v).kind { TyKind::Var(nv) => Row::var(nv, self.origin), TyKind::Record(_) | TyKind::Variant(_) => unreachable!("row variable cannot bind a scalar type"), _ => unreachable!("row variable cannot bind a scalar type") },
            RowKind::Empty => self.clone(),
            RowKind::Extend { label, value, rest } => Row::extend(label.clone(), value.map_vars(f), rest.map_vars(f), self.origin),
        }
    }
    pub fn display(&self, uf: &UnionFind) -> String {
        let mut parts = vec![];
        let mut row = self.clone();
        loop {
            row = match uf.resolve_row(&row) { Some(r) => r, None => break };
            match row.kind {
                RowKind::Var(v) => { parts.push(format!("..?{v}")); break; }
                RowKind::Empty => break,
                RowKind::Extend { label, value, rest } => { parts.push(format!("{label}: {}", value.display(uf))); row = rest; }
            }
        }
        parts.join(", ")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<Var>,
    pub ty: Ty,
}

impl Scheme {
    pub fn instantiate(&self, uf: &mut UnionFind, origin: Origin) -> Ty {
        let mut map = HashMap::new();
        for &v in &self.vars { map.insert(v, uf.fresh(origin)); }
        self.ty.map_vars(&mut |v| match map.get(&v) { Some(n) => Ty::var(*n, origin), None => Ty::var(v, origin) })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeErrorKind {
    Mismatch,
    InfiniteType,
    RowOccurs,
    MissingFields,
    ExtraFields,
    Arity,
    UnboundVariable,
    Disabled,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub message: String,
    pub constraint_id: u64,
    pub node: usize,
    pub spans: Vec<usize>,
    pub expected: Option<String>,
    pub actual: Option<String>,
    pub missing: Vec<String>,
    pub extra: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum StepKind { Begin, Bind, Error, Generalize, Instantiate, SkipDisabled }

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Step {
    pub index: usize,
    pub constraint_id: u64,
    pub kind: StepKind,
    pub detail: String,
    pub error: Option<TypeError>,
}

#[derive(Default)]
pub struct NodeCell { parent: Option<Var>, rank: u32 }

#[derive(Default)]
pub struct UnionFind {
    cells: Vec<NodeCell>,
    origins: Vec<Origin>,
    bindings: Vec<Option<Ty>>,
    row_bindings: Vec<Option<Row>>,
}

impl UnionFind {
    pub fn new() -> Self { Self::default() }
    pub fn fresh(&mut self, origin: Origin) -> Var {
        let id = self.cells.len();
        self.cells.push(NodeCell { parent: None, rank: 0 });
        self.origins.push(origin);
        self.bindings.push(None);
        self.row_bindings.push(None);
        id
    }
    pub fn origin(&self, v: Var) -> Origin { self.origins[v] }
    pub fn find(&mut self, v: Var) -> Var {
        if let Some(p) = self.cells[v].parent {
            let r = self.find(p);
            self.cells[v].parent = Some(r);
            r
        } else { v }
    }
    fn union_vars(&mut self, a: Var, b: Var) -> Var {
        let a = self.find(a);
        let b = self.find(b);
        if a == b { return a; }
        if self.cells[a].rank < self.cells[b].rank { self.cells[a].parent = Some(b); b }
        else if self.cells[a].rank > self.cells[b].rank { self.cells[b].parent = Some(a); a }
        else { self.cells[b].parent = Some(a); self.cells[a].rank += 1; a }
    }
    fn bind_ty(&mut self, v: Var, ty: Ty) {
        let r = self.find(v);
        self.bindings[r] = Some(ty);
    }
    fn bind_row_var(&mut self, v: Var, row: Row) {
        let r = self.find(v);
        self.row_bindings[r] = Some(row);
    }
    pub fn resolve_ty(&mut self, ty: &Ty) -> Option<Ty> {
        if let TyKind::Var(v) = ty.kind {
            let root = self.find(v);
            if let Some(bound) = self.bindings[root].clone() {
                return self.resolve_ty(&bound);
            }
            if root != v { return Some(Ty::var(root, ty.origin)); }
            return None;
        }
        Some(match &ty.kind {
            TyKind::Var(_) => unreachable!(),
            TyKind::Int | TyKind::Bool => ty.clone(),
            TyKind::Fun(a,b) => Ty::fun(self.resolve_ty(a).unwrap_or_else(|| a.clone()), self.resolve_ty(b).unwrap_or_else(|| b.clone()), ty.origin),
            TyKind::Record(r) => Ty::record(self.resolve_row(r).unwrap_or_else(|| r.clone()), ty.origin),
            TyKind::Variant(r) => Ty::variant(self.resolve_row(r).unwrap_or_else(|| r.clone()), ty.origin),
        })
    }
    pub fn resolve_row(&mut self, row: &Row) -> Option<Row> {
        if let RowKind::Var(v) = row.kind {
            let root = self.find(v);
            if let Some(bound) = self.row_bindings[root].clone() { return self.resolve_row(&bound); }
            if root != v { return Some(Row::var(root, row.origin)); }
            return None;
        }
        if let RowKind::Extend { label, value, rest } = &row.kind {
            let value = self.resolve_ty(value).unwrap_or_else(|| (**value).clone());
            let rest = self.resolve_row(rest).unwrap_or_else(|| (**rest).clone());
            return Some(Row::extend(label.clone(), value, rest, row.origin));
        }
        None
    }
    pub fn prune(&mut self, ty: &Ty) -> Ty { self.resolve_ty(ty).unwrap_or_else(|| ty.clone()) }
    pub fn prune_row(&mut self, row: &Row) -> Row { self.resolve_row(row).unwrap_or_else(|| row.clone()) }
    pub fn occurs_ty(&mut self, v: Var, ty: &Ty) -> bool {
        let mut set = HashSet::new();
        ty.vars(&mut set);
        set.into_iter().any(|x| self.find(x) == self.find(v))
    }
    pub fn occurs_row(&mut self, v: Var, row: &Row) -> bool {
        let mut set = HashSet::new();
        row.vars(&mut set);
        set.into_iter().any(|x| self.find(x) == self.find(v))
    }
}

fn origin_node(origin: Origin) -> Option<usize> {
    if let Origin::Node(id) = origin { Some(id) } else { None }
}

fn normalize_row(uf: &mut UnionFind, row: &Row) -> (BTreeMap<String, Ty>, Row) {
    let mut current = uf.prune_row(row);
    let mut fields = BTreeMap::new();
    loop {
        match current.kind {
            RowKind::Empty => return (fields, Row::empty(current.origin)),
            RowKind::Var(v) => return (fields, Row::var(v, current.origin)),
            RowKind::Extend { label, value, rest } => {
                fields.insert(label, uf.prune(&value));
                current = uf.prune_row(&rest);
            }
        }
    }
}

fn row_var(row: &Row) -> Option<Var> { if let RowKind::Var(v) = row.kind { Some(v) } else { None } }

fn ext_chain(items: impl DoubleEndedIterator<Item = (String, Ty)>, tail: Row, origin: Origin) -> Row {
    let mut row = tail;
    for (label, value) in items.rev() { row = Row::extend(label, value, row, origin); }
    row
}

pub fn unify_rows(uf: &mut UnionFind, left: &Row, right: &Row, constraint_id: u64, node: usize, variant: bool) -> Vec<TypeError> {
    let (mut lmap, ltail) = normalize_row(uf, left);
    let (mut rmap, rtail) = normalize_row(uf, right);
    let mut errors = vec![];
    for label in lmap.keys().cloned().collect::<Vec<_>>() {
        if let Some(rv) = rmap.remove(&label) {
            let lv = lmap.remove(&label).unwrap();
            errors.extend(unify(uf, &lv, &rv, constraint_id, node));
        }
    }
    let missing: Vec<String> = lmap.keys().cloned().collect();
    let extra: Vec<String> = rmap.keys().cloned().collect();
    let lv = row_var(&ltail);
    let rv = row_var(&rtail);
    let kind_name = if variant { "variant" } else { "record" };
    match (lv, rv) {
        (Some(lv), Some(rv)) => {
            let lv = uf.find(lv);
            let rv = uf.find(rv);
            if uf.occurs_row(lv, &rtail) || extra.iter().any(|_| rmap.values().any(|t| uf.occurs_ty(lv, t))) {
                errors.push(row_occurs(constraint_id, node));
            } else {
                let merged = ext_chain(rmap.into_iter(), rtail, left.origin);
                uf.bind_row_var(lv, merged);
                if lv != rv { uf.union_vars(lv, rv); }
            }
            if !missing.is_empty() || !extra.is_empty() {
                errors.push(row_field_error(constraint_id,node,&missing,&extra,kind_name,left,right,uf));
            }
        }
        (Some(lv), None) => {
            if matches!(rtail.kind, RowKind::Empty) && (!missing.is_empty() || !extra.is_empty()) {
                errors.push(row_field_error(constraint_id,node,&missing,&extra,kind_name,left,right,uf));
                if missing.is_empty() && !uf.occurs_row(lv, &rtail) { uf.bind_row_var(uf.find(lv), rtail); }
            } else if !missing.is_empty() {
                errors.push(row_field_error(constraint_id,node,&missing,&extra,kind_name,left,right,uf));
            } else if uf.occurs_row(lv, &rtail) || rmap.values().any(|t| uf.occurs_ty(lv, t)) {
                errors.push(row_occurs(constraint_id, node));
            } else {
                uf.bind_row_var(uf.find(lv), ext_chain(rmap.into_iter(), rtail, left.origin));
            }
        }
        (None, Some(rv)) => {
            if matches!(ltail.kind, RowKind::Empty) && (!missing.is_empty() || !extra.is_empty()) {
                errors.push(row_field_error(constraint_id,node,&missing,&extra,kind_name,left,right,uf));
                if extra.is_empty() && !uf.occurs_row(rv, &ltail) { uf.bind_row_var(uf.find(rv), ltail); }
            } else if !extra.is_empty() {
                errors.push(row_field_error(constraint_id,node,&missing,&extra,kind_name,left,right,uf));
            } else if uf.occurs_row(rv, &ltail) || lmap.values().any(|t| uf.occurs_ty(rv, t)) {
                errors.push(row_occurs(constraint_id, node));
            } else {
                uf.bind_row_var(uf.find(rv), ext_chain(lmap.into_iter(), ltail, right.origin));
            }
        }
        (None, None) => {
            if !missing.is_empty() || !extra.is_empty() {
                errors.push(row_field_error(constraint_id,node,&missing,&extra,kind_name,left,right,uf));
            }
        }
    }
    errors
}

fn row_occurs(id: u64, node: usize) -> TypeError {
    TypeError { kind: TypeErrorKind::RowOccurs, message: "无限行类型：字段扩展通过行变量回到自身".into(), constraint_id: id, node, spans: vec![node], expected: None, actual: None, missing: vec![], extra: vec![] }
}

fn row_field_error(id: u64, node: usize, missing: &[String], extra: &[String], what: &str, left: &Row, right: &Row, uf: &mut UnionFind) -> TypeError {
    let kind = if extra.is_empty() { TypeErrorKind::MissingFields } else if missing.is_empty() { TypeErrorKind::ExtraFields } else { TypeErrorKind::Mismatch };
    let mut parts = vec![format!("{what} 字段不兼容")];
    if !missing.is_empty() { parts.push(format!("缺少字段: {}", missing.join(", "))); }
    if !extra.is_empty() { parts.push(format!("多余字段: {}", extra.join(", "))); }
    TypeError { kind, message: parts.join("；"), constraint_id: id, node, spans: vec![node], expected: Some(left.display(uf)), actual: Some(right.display(uf)), missing: missing.to_vec(), extra: extra.to_vec() }
}

pub fn unify(uf: &mut UnionFind, a: &Ty, b: &Ty, constraint_id: u64, node: usize) -> Vec<TypeError> {
    let a = uf.prune(a);
    let b = uf.prune(b);
    match (a.kind, b.kind) {
        (TyKind::Var(x), TyKind::Var(y)) => {
            let rx = uf.find(x);
            let ry = uf.find(y);
            if rx != ry { uf.union_vars(rx, ry); }
            vec![]
        }
        (TyKind::Var(v), ty) | (ty, TyKind::Var(v)) => {
            if uf.occurs_ty(v, &ty) {
                vec![TypeError { kind: TypeErrorKind::InfiniteType, message: format!("occurs check 失败：?{v} 已出现在 {} 中", ty.display(uf)), constraint_id, node, spans: vec![node], expected: Some(Ty::var(v, Origin::Constraint(constraint_id)).display(uf)), actual: Some(ty.display(uf)), missing: vec![], extra: vec![] }]
            } else {
                uf.bind_ty(uf.find(v), ty);
                vec![]
            }
        }
        (TyKind::Int, TyKind::Int) | (TyKind::Bool, TyKind::Bool) => vec![],
        (TyKind::Fun(a1,a2), TyKind::Fun(b1,b2)) => {
            let mut e = unify(uf,&a1,&b1,constraint_id,node);
            e.extend(unify(uf,&a2,&b2,constraint_id,node));
            e
        }
        (TyKind::Record(r1), TyKind::Record(r2)) => unify_rows(uf,&r1,&r2,constraint_id,false),
        (TyKind::Variant(r1), TyKind::Variant(r2)) => unify_rows(uf,&r1,&r2,constraint_id,true),
        _ => vec![TypeError {
            kind: TypeErrorKind::Mismatch,
            message: format!("类型不兼容：{} 与 {}", a.display(uf), b.display(uf)),
            constraint_id,
            node,
            spans: vec![origin_node(a.origin).into_iter().chain(origin_node(b.origin)).collect::<Vec<_>>()].into_iter().flatten().collect(),
            expected: Some(a.display(uf)),
            actual: Some(b.display(uf)),
            missing: vec![],
            extra: vec![],
        }],
    }
}

pub fn generalize(uf: &mut UnionFind, ty: &Ty, active: &HashSet<Var>) -> Scheme {
    let ty = uf.prune(ty);
    let mut free = HashSet::new();
    ty.vars(&mut free);
    let vars: Vec<Var> = free.into_iter().filter(|v| !active.contains(&uf.find(*v))).collect();
    Scheme { vars, ty }
}
