//! Surface AST, stored in an arena so every node can be referenced by id
//! and carries a byte span plus a rename-stable structural key.

use crate::span::Span;
use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct NodeId(pub usize);

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Var(String),
    Lam { param: String, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Let {
        rec: bool,
        name: String,
        value: NodeId,
        body: NodeId,
    },
    If {
        cond: NodeId,
        then_br: NodeId,
        else_br: NodeId,
    },
    Binop {
        op: BinOp,
        lhs: NodeId,
        rhs: NodeId,
    },
    Record(Vec<(String, NodeId)>),
    Field {
        record: NodeId,
        field: String,
    },
    Variant {
        label: String,
        payload: NodeId,
    },
    Match {
        scrut: NodeId,
        arms: Vec<(Pattern, NodeId)>,
    },
    Unit,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum BinOp {
    Add, Sub, Mul, Div, Eq, Neq, Lt, Le, Gt, Ge, And, Or,
}

impl BinOp {
    pub fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+", BinOp::Sub => "-", BinOp::Mul => "*", BinOp::Div => "/",
            BinOp::Eq => "=", BinOp::Neq => "!=", BinOp::Lt => "<", BinOp::Le => "<=",
            BinOp::Gt => ">", BinOp::Ge => ">=", BinOp::And => "&&", BinOp::Or => "||",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Pattern {
    PVar(String),
    PVariant(String, Option<String>),
    PRecord(Vec<String>),
    PWild,
}

#[derive(Clone, Debug, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub expr: Expr,
    pub span: Span,
    /// Structural key: tree address + node tag. Survives renaming of
    /// binders (names intentionally excluded) and insertions elsewhere.
    pub key: String,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct Ast {
    pub nodes: Vec<Node>,
    pub root: Option<NodeId>,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0]
    }

    /// Enumerate the direct child expression ids in fixed order.
    pub fn children(&self, id: NodeId) -> Vec<NodeId> {
        match &self.node(id).expr {
            Expr::Lam { body, .. } => vec![*body],
            Expr::App { fun, arg } => vec![*fun, *arg],
            Expr::Let { value, body, .. } => vec![*value, *body],
            Expr::If { cond, then_br, else_br } => vec![*cond, *then_br, *else_br],
            Expr::Binop { lhs, rhs, .. } => vec![*lhs, *rhs],
            Expr::Record(fs) => fs.iter().map(|(_, n)| *n).collect(),
            Expr::Field { record, .. } => vec![*record],
            Expr::Variant { payload, .. } => vec![*payload],
            Expr::Match { scrut, arms } => {
                let mut v = vec![*scrut];
                v.extend(arms.iter().map(|(_, n)| *n));
                v
            }
            _ => vec![],
        }
    }

    pub fn tag(&self, id: NodeId) -> &'static str {
        match &self.node(id).expr {
            Expr::Int(_) => "int",
            Expr::Bool(_) => "bool",
            Expr::Str(_) => "str",
            Expr::Var(_) => "var",
            Expr::Lam { .. } => "lam",
            Expr::App { .. } => "app",
            Expr::Let { rec: false, .. } => "let",
            Expr::Let { rec: true, .. } => "letrec",
            Expr::If { .. } => "if",
            Expr::Binop { .. } => "binop",
            Expr::Record(_) => "record",
            Expr::Field { .. } => "field",
            Expr::Variant { .. } => "variant",
            Expr::Match { .. } => "match",
            Expr::Unit => "unit",
        }
    }
}
