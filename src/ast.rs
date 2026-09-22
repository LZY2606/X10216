use crate::span::Span;
use serde::{Deserialize, Serialize};

/// AST 节点身份。跨编辑版本保持稳定（见 incremental::diff）。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NodeId(pub u32);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub k: Box<ExprK>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExprK {
    Int(i64),
    Bool(bool),
    Str(String),
    Var(String),
    Lam { param: String, body: Expr },
    App { f: Expr, a: Expr },
    Let { rec: bool, name: String, bind: Expr, body: Expr },
    Record(Vec<(String, Expr)>),
    Field { e: Expr, name: String },
    Variant { tag: String, payload: Expr },
    Match { e: Expr, arms: Vec<Arm> },
    If { c: Expr, t: Expr, e: Expr },
    Op { op: String, l: Expr, r: Expr },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Arm {
    pub id: NodeId,
    pub tag: String,
    pub var: String,
    pub body: Expr,
}

impl Expr {
    pub fn kind_str(&self) -> &'static str {
        match &*self.k {
            ExprK::Int(_) => "Int",
            ExprK::Bool(_) => "Bool",
            ExprK::Str(_) => "Str",
            ExprK::Var(_) => "Var",
            ExprK::Lam { .. } => "Lam",
            ExprK::App { .. } => "App",
            ExprK::Let { .. } => "Let",
            ExprK::Record(_) => "Record",
            ExprK::Field { .. } => "Field",
            ExprK::Variant { .. } => "Variant",
            ExprK::Match { .. } => "Match",
            ExprK::If { .. } => "If",
            ExprK::Op { .. } => "Op",
        }
    }
}
