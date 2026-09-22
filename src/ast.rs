use crate::lexer::Span;
use std::collections::BTreeMap;

pub type NodeId = u64;
pub type BinderId = u64;

#[derive(Clone, Debug, serde::Serialize)]
pub struct Program {
    pub statements: Vec<Stmt>,
    pub node_ids: BTreeMap<NodeId, NodeInfo>,
    pub binders: BTreeMap<BinderId, String>,
    pub scope_hashes: BTreeMap<NodeId, u64>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub node_id: NodeId,
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StmtKind {
    Let(LetBind),
    Expr(Expr),
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub node_id: NodeId,
    pub binder_id: Option<BinderId>,
    pub binder_key: Option<String>,
    pub ref_binder: Option<BinderId>,
    pub scope_hash: Option<u64>,
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ExprKind {
    Int(i64),
    Str(String),
    Bool(bool),
    Var(String),
    Lam { param: String, body: Box<Expr> },
    App { fun: Box<Expr>, arg: Box<Expr> },
    Let { bind: Box<LetBind>, body: Box<Expr> },
    If { cond: Box<Expr>, then_branch: Box<Expr>, else_branch: Box<Expr> },
    Binary { op: String, left: Box<Expr>, right: Box<Expr> },
    Record { fields: Vec<(String, Expr)>, closed: bool },
    RecordFields { record: Box<Expr>, fields: Vec<(String, Expr)>, closed: bool },
    Field { record: Box<Expr>, name: String },
    Variant { label: String, payload: Option<Box<Expr>>, closed: Option<Span> },
    Match { scrutinee: Box<Expr>, arms: Vec<MatchArm> },
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct LetBind {
    pub name: String,
    pub rec: Option<Span>,
    pub value: Expr,
    pub body: Option<Expr>,
    pub is_value: bool,
    pub span: Span,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Pattern {
    Wildcard(Span),
    Variant { label: String, payload: Option<String>, label_span: Span, span: Span },
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct NodeInfo {
    pub span: Span,
    pub kind: String,
    pub text: String,
    pub binder_id: Option<BinderId>,
    pub ref_binder: Option<BinderId>,
    pub scope_hash: Option<u64>,
}
