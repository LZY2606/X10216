use std::rc::Rc;

pub type VarId = u32;

#[derive(Clone, Copy, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum VarKind {
    Ty,
    Row,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Var(VarId),
    Int,
    Bool,
    Fun(Rc<Ty>, Rc<Ty>),
    Record(Rc<Row>),
    Variant(Rc<Row>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Row {
    Empty,
    Var(VarId),
    Ext(String, Rc<Ty>, Rc<Row>),
}

impl Ty {
    pub fn fun(a: Ty, b: Ty) -> Ty {
        Ty::Fun(Rc::new(a), Rc::new(b))
    }
    pub fn record(fields: Vec<(String, Ty)>) -> Ty {
        let mut row = Row::Empty;
        for (l, t) in fields.into_iter().rev() {
            row = Row::Ext(l, Rc::new(t), Rc::new(row));
        }
        Ty::Record(Rc::new(row))
    }
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub vars: Vec<(VarId, VarKind)>,
    pub ty: Ty,
}

impl Scheme {
    pub fn mono(t: Ty) -> Scheme {
        Scheme { vars: vec![], ty: t }
    }
}
