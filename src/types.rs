use serde::Serialize;
use std::collections::BTreeSet;

/// 普通类型变量
pub type Var = u32;
/// 行变量
pub type RVar = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum Row {
    Open(RVar),
    Closed(Vec<(String, Ty)>),
}

impl Row {
    pub fn empty() -> Row {
        Row::Closed(Vec::new())
    }
    pub fn get(&self, name: &str) -> Option<&Ty> {
        match self {
            Row::Closed(fs) => fs.iter().find(|(n, _)| n == name).map(|(_, t)| t),
            Row::Open(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub enum Ty {
    TVar(Var),
    Con(String),
    App(String, Vec<Ty>),
    Record(Box<Row>),
    Variant(Box<Row>),
}

impl Ty {
    pub fn int() -> Ty {
        Ty::Con("Int".into())
    }
    pub fn bool_() -> Ty {
        Ty::Con("Bool".into())
    }
    pub fn string() -> Ty {
        Ty::Con("String".into())
    }
    pub fn unit() -> Ty {
        Ty::Con("()".into())
    }
    pub fn arrow(a: Ty, b: Ty) -> Ty {
        Ty::App("->".into(), vec![a, b])
    }
}

pub fn sort_fields(mut fs: Vec<(String, Ty)>) -> Vec<(String, Ty)> {
    fs.sort_by(|a, b| a.0.cmp(&b.0));
    fs
}

pub struct Pool {
    pub tv: u32,
    pub rv: u32,
}

impl Pool {
    pub fn new() -> Self {
        Pool { tv: 0, rv: 0 }
    }
    pub fn fresh(&mut self) -> Var {
        let v = self.tv;
        self.tv += 1;
        v
    }
    pub fn rfresh(&mut self) -> RVar {
        let v = self.rv;
        self.rv += 1;
        v
    }
}

/// 多态类型方案
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Scheme {
    pub vars: Vec<Var>,
    pub rvars: Vec<RVar>,
    pub body: Ty,
}

pub fn ty_ftv(t: &Ty, out: &mut BTreeSet<Var>) {
    match t {
        Ty::TVar(v) => {
            out.insert(*v);
        }
        Ty::Con(_) => {}
        Ty::App(_, ts) => {
            for t in ts {
                ty_ftv(t, out);
            }
        }
        Ty::Record(r) | Ty::Variant(r) => row_ftv(r, out),
    }
}

pub fn row_ftv(r: &Row, out: &mut BTreeSet<Var>) {
    if let Row::Closed(fs) = r {
        for (_, t) in fs {
            ty_ftv(t, out);
        }
    }
}

pub fn ty_rvars(t: &Ty, out: &mut BTreeSet<RVar>) {
    match t {
        Ty::TVar(_) | Ty::Con(_) => {}
        Ty::App(_, ts) => {
            for t in ts {
                ty_rvars(t, out);
            }
        }
        Ty::Record(r) | Ty::Variant(r) => match &**r {
            Row::Open(v) => {
                out.insert(*v);
            }
            Row::Closed(fs) => {
                for (_, t) in fs {
                    ty_rvars(t, out);
                }
            }
        },
    }
}

/// 用于展示的已解析类型视图（变量以并查集根编号表示）
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum TyView {
    Var(u32),
    Con(String),
    App(String, Vec<TyView>),
    Record(RowView),
    Variant(RowView),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RowView {
    pub fields: Vec<(String, TyView)>,
    pub tail: Option<u32>,
}

impl RowView {
    pub fn closed(fields: Vec<(String, TyView)>) -> RowView {
        RowView { fields, tail: None }
    }
}
