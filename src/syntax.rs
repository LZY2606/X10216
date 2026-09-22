use serde::Serialize;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
    pub id: String,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span, id: String) -> Self {
        Self { node, span, id }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var { name: String, depth: usize },
    Fun { param: String, body: Box<ExprNode> },
    App { fun: Box<ExprNode>, arg: Box<ExprNode> },
    Let { name: String, rec: bool, value: Box<ExprNode>, body: Box<ExprNode> },
    Record { fields: Vec<Field> },
    Field { record: Box<ExprNode>, label: String },
    Extend { label: String, value: Box<ExprNode>, rest: Option<Box<ExprNode>> },
    Variant { label: String, payload: Option<Box<ExprNode>> },
    Match { scrutinee: Box<ExprNode>, arms: Vec<Arm> },
    Annotation { expr: Box<ExprNode>, ty: TypeNode },
}

pub type ExprNode = Spanned<Expr>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Field {
    pub label: String,
    pub value: ExprNode,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Arm {
    pub pattern: PatternNode,
    pub body: ExprNode,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Pattern {
    Binding(String),
    Variant { label: String, binding: Option<String> },
}

pub type PatternNode = Spanned<Pattern>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Type {
    Var(String),
    Int,
    Bool,
    Fun(Box<TypeNode>, Box<TypeNode>),
    Record(Row),
    Variant(Row),
}

pub type TypeNode = Spanned<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Row {
    pub fields: Vec<RowField>,
    pub extension: Option<Spanned<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RowField {
    pub label: Spanned<String>,
    pub value: TypeNode,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({}..{})", self.message, self.span.start, self.span.end)
    }
}
