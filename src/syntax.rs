use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn join(self, other: Self) -> Self {
        Self { start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", content = "text", rename_all = "snake_case")]
pub enum TokenKind {
    Let, Rec, In, Fun, If, Then, Else, Match, With, True, False,
    Ident(String),
    Int(String),
    String(String),
    Dot, Comma, Semicolon, Equals, Arrow, FatArrow, Backslash,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Plus, Minus, Star, Slash, Lt, Gt, Le, Ge, EqEq, NotEq,
    Pipe, Colon, ColonColon, Underscore,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Lit {
    Int(i64),
    Bool(bool),
    String(String),
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", content = "label", rename_all = "snake_case")]
pub enum Pattern {
    Wildcard(Span),
    Var { name: String, span: Span },
    Variant { label: String, payload: Option<Box<Pattern>>, span: Span },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard(span) | Pattern::Var { span, .. } | Pattern::Variant { span, .. } => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Field {
    pub name: String,
    pub name_span: Span,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Expr {
    Lit { value: Lit, span: Span },
    Var { name: String, span: Span },
    Fun { param: String, param_span: Span, body: Box<Expr>, span: Span },
    App { fun: Box<Expr>, arg: Box<Expr>, span: Span },
    Let { rec: bool, name: String, name_span: Span, value: Box<Expr>, body: Box<Expr>, span: Span },
    Record { fields: Vec<Field>, span: Span },
    Field { record: Box<Expr>, name: String, name_span: Span, span: Span },
    Extend { base: Box<Expr>, fields: Vec<Field>, span: Span },
    Variant { label: String, payload: Option<Box<Expr>>, span: Span },
    Match { scrutinee: Box<Expr>, arms: Vec<MatchArm>, span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Binding {
    pub rec: bool,
    pub name: String,
    pub name_span: Span,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Program {
    pub bindings: Vec<Binding>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AstNode {
    pub id: NodeId,
    pub kind: String,
    pub label: String,
    pub span: Span,
    pub children: Vec<NodeId>,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit { span, .. } | Expr::Var { span, .. } | Expr::Fun { span, .. }
            | Expr::App { span, .. } | Expr::Let { span, .. } | Expr::Record { span, .. }
            | Expr::Field { span, .. } | Expr::Extend { span, .. } | Expr::Variant { span, .. }
            | Expr::Match { span, .. } => *span,
        }
    }
}

impl Binding {
    pub fn as_expr(&self) -> Expr {
        if self.rec {
            Expr::Let {
                rec: true,
                name: self.name.clone(),
                name_span: self.name_span,
                value: Box::new(self.value.clone()),
                body: Box::new(Expr::Lit { value: Lit::Unit, span: self.span }),
                span: self.span,
            }
        } else {
            self.value.clone()
        }
    }
}
