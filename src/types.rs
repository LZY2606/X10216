use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

pub type TVar = u64;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    Var(TVar),
    Int,
    Bool,
    Fun(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
    /// 错误恢复用的“毒”类型，与任何类型统一成功，避免级联错误。
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Row {
    Empty,
    Var(TVar),
    Extend { label: String, ty: Box<Type>, rest: Box<Row> },
}

/// 构造一个按标签排序的规范行。
pub fn row_from_fields(mut fields: Vec<(String, Type)>, rest: Row) -> Row {
    fields.sort_by(|a, b| a.0.cmp(&b.0));
    fields.dedup_by(|a, b| a.0 == b.0);
    let mut r = rest;
    for (l, t) in fields.into_iter().rev() {
        r = Row::Extend { label: l, ty: Box::new(t), rest: Box::new(r) };
    }
    r
}

/// 展开行：返回 (字段列表, 尾部)。尾部只会是 Empty 或 Var。
pub fn row_collect(r: &Row) -> (Vec<(String, Type)>, Row) {
    let mut fields = Vec::new();
    let mut cur = r;
    loop {
        match cur {
            Row::Extend { label, ty, rest } => {
                fields.push((label.clone(), (**ty).clone()));
                cur = rest;
            }
            Row::Empty => return (fields, Row::Empty),
            Row::Var(v) => return (fields, Row::Var(*v)),
        }
    }
}

pub fn ftv_ty(t: &Type, out: &mut BTreeSet<TVar>) {
    match t {
        Type::Var(v) => {
            out.insert(*v);
        }
        Type::Fun(a, b) => {
            ftv_ty(a, out);
            ftv_ty(b, out);
        }
        Type::Record(r) | Type::Variant(r) => ftv_row(r, out),
        _ => {}
    }
}

pub fn ftv_row(r: &Row, out: &mut BTreeSet<TVar>) {
    match r {
        Row::Var(v) => {
            out.insert(*v);
        }
        Row::Extend { ty, rest, .. } => {
            ftv_ty(ty, out);
            ftv_row(rest, out);
        }
        Row::Empty => {}
    }
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub vars: Vec<TVar>,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeOrRow {
    Ty(Type),
    Row(Row),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Subst {
    pub map: BTreeMap<TVar, TypeOrRow>,
}

impl Subst {
    pub fn apply_ty(&self, t: &Type) -> Type {
        match t {
            Type::Var(v) => match self.map.get(v) {
                Some(TypeOrRow::Ty(t2)) => self.apply_ty(t2),
                _ => t.clone(),
            },
            Type::Fun(a, b) => Type::Fun(Box::new(self.apply_ty(a)), Box::new(self.apply_ty(b))),
            Type::Record(r) => Type::Record(self.apply_row(r)),
            Type::Variant(r) => Type::Variant(self.apply_row(r)),
            other => other.clone(),
        }
    }

    pub fn apply_row(&self, r: &Row) -> Row {
        match r {
            Row::Var(v) => match self.map.get(v) {
                Some(TypeOrRow::Row(r2)) => self.apply_row(r2),
                _ => r.clone(),
            },
            Row::Extend { label, ty, rest } => Row::Extend {
                label: label.clone(),
                ty: Box::new(self.apply_ty(ty)),
                rest: Box::new(self.apply_row(rest)),
            },
            Row::Empty => Row::Empty,
        }
    }

    pub fn bind_ty(&mut self, v: TVar, t: Type) {
        if t != Type::Var(v) {
            self.map.insert(v, TypeOrRow::Ty(t));
        }
    }

    pub fn bind_row(&mut self, v: TVar, r: Row) {
        if r != Row::Var(v) {
            self.map.insert(v, TypeOrRow::Row(r));
        }
    }

    pub fn occurs_ty(&self, v: TVar, t: &Type) -> bool {
        let mut s = BTreeSet::new();
        ftv_ty(&self.apply_ty(t), &mut s);
        s.contains(&v)
    }

    pub fn occurs_row(&self, v: TVar, r: &Row) -> bool {
        let mut s = BTreeSet::new();
        ftv_row(&self.apply_row(r), &mut s);
        s.contains(&v)
    }
}

/// 类型变量命名器：保证一次求解/分析中名字稳定。
#[derive(Clone, Debug, Default)]
pub struct Renderer {
    pub names: BTreeMap<TVar, String>,
    pub next: usize,
}

impl Renderer {
    pub fn new() -> Self {
        Renderer::default()
    }

    pub fn var_name(&mut self, v: TVar) -> String {
        if let Some(n) = self.names.get(&v) {
            return n.clone();
        }
        let i = self.next;
        self.next += 1;
        let name = if i < 26 {
            format!("'{}", (b'a' + i as u8) as char)
        } else {
            format!("'t{}", i)
        };
        self.names.insert(v, name.clone());
        name
    }

    pub fn ty(&mut self, t: &Type) -> String {
        match t {
            Type::Var(v) => self.var_name(*v),
            Type::Int => "Int".into(),
            Type::Bool => "Bool".into(),
            Type::Fun(a, b) => {
                let sa = self.ty(a);
                let sb = self.ty(b);
                format!("({} -> {})", sa, sb)
            }
            Type::Record(r) => {
                let s = self.row(r);
                format!("{{{}}}", s)
            }
            Type::Variant(r) => {
                let s = self.row(r);
                format!("<{}>", s)
            }
            Type::Error => "<错误>".into(),
        }
    }

    pub fn row(&mut self, r: &Row) -> String {
        let (fields, tail) = row_collect(r);
        let mut parts = Vec::new();
        for (l, t) in &fields {
            let s = self.ty(t);
            parts.push(format!("{}: {}", l, s));
        }
        if let Row::Var(v) = tail {
            let n = self.var_name(v);
            parts.push(format!("| {}", n));
        }
        parts.join(", ")
    }

    pub fn scheme(&mut self, s: &Scheme) -> String {
        if s.vars.is_empty() {
            return self.ty(&s.ty);
        }
        let mut names = Vec::new();
        for v in &s.vars {
            names.push(self.var_name(*v));
        }
        let body = self.ty(&s.ty);
        format!("∀{}. {}", names.join(" "), body)
    }
}
