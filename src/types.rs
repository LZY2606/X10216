use serde::{Deserialize, Serialize};
use std::rc::Rc;

use crate::syntax::Span;

pub type VarId = u64;

/// 类型构造的来源：哪个表达式节点、什么角色产生了它。
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Origin {
    pub path: String,
    pub span: Span,
    pub note: String,
}

impl Origin {
    pub fn none() -> Origin {
        Origin { path: String::new(), span: Span { start: 0, end: 0 }, note: String::new() }
    }
    pub fn is_none(&self) -> bool {
        self.path.is_empty() && self.note.is_empty()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Type {
    Var(VarId),
    Int(Origin),
    Bool(Origin),
    Fun(Origin, Rc<Type>, Rc<Type>),
    Record(Origin, Row),
    Variant(Origin, Row),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Row {
    Empty(Origin),
    Var(VarId),
    Extend(Origin, String, Rc<Type>, Rc<Row>),
}

#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<VarId>,
    pub ty: Type,
}
