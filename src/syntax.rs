use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Lit {
    Int(String),
    Str(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    Name(String),
    Tag(String, Option<Box<Pattern>>),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expr {
    Lit(LitNode),
    Var(VarNode),
    Lam(LamNode),
    App(AppNode),
    Let {
        node: LetNode,
    },
    Record(RecordNode),
    Field(FieldNode),
    Extend(ExtendNode),
    Tag(TagNode),
    Match(MatchNode),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit(n) => n.span,
            Expr::Var(n) => n.span,
            Expr::Lam(n) => n.span,
            Expr::App(n) => n.span,
            Expr::Let { node } => node.span,
            Expr::Record(n) => n.span,
            Expr::Field(n) => n.span,
            Expr::Extend(n) => n.span,
            Expr::Tag(n) => n.span,
            Expr::Match(n) => n.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LitNode {
    pub value: Lit,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VarNode {
    pub name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LamNode {
    pub var_span: Span,
    pub param: String,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AppNode {
    pub fun: Expr,
    pub arg: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LetNode {
    pub rec: bool,
    pub name: String,
    pub name_span: Span,
    pub rhs: Expr,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecordNode {
    pub fields: Vec<RecordField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecordField {
    pub label: String,
    pub label_span: Span,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FieldNode {
    pub record: Expr,
    pub label: String,
    pub label_span: Span,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExtendNode {
    pub base: Expr,
    pub fields: Vec<RecordField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TagNode {
    pub label: String,
    pub label_span: Span,
    pub payload: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: PatternNode,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PatternNode {
    pub pattern: Pattern,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MatchNode {
    pub scrutinee: Expr,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Decl {
    pub rec: bool,
    pub name: String,
    pub name_span: Span,
    pub rhs: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub result: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NodeInfo {
    pub id: String,
    pub kind: String,
    pub label: String,
    pub span: Span,
}

pub fn lit_ty_name(lit: &Lit) -> &'static str {
    match lit {
        Lit::Int(_) => "Int",
        Lit::Str(_) => "String",
    }
}
