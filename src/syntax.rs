use std::fmt;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start: start as u32, end: end as u32 }
    }
    pub fn join(self, other: Span) -> Span {
        Span::new(self.start.min(other.start) as usize, self.end.max(other.end) as usize)
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Expr {
    Int { value: i64, span: Span },
    Bool { value: bool, span: Span },
    Str { value: Rc<str>, span: Span },
    Var { name: Rc<str>, span: Span },
    Lam { param: Rc<str>, param_span: Span, body: PExpr, span: Span },
    App { fun: PExpr, arg: PExpr, span: Span },
    Bin { op: BinOp, left: PExpr, right: PExpr, span: Span },
    If { cond: PExpr, then_branch: PExpr, else_branch: PExpr, span: Span },
    Let { recursive: bool, name: Rc<str>, name_span: Span, value: PExpr, body: PExpr, span: Span },
    Record { fields: Vec<(Rc<str>, PExpr)>, span: Span },
    Field { record: PExpr, name: Rc<str>, name_span: Span, span: Span },
    Variant { tag: Rc<str>, payload: PExpr, span: Span },
    Match { scrutinee: PExpr, arms: Vec<MatchArm>, fallback: Option<PExpr>, span: Span },
}

pub type PExpr = Rc<Expr>;

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct MatchArm {
    pub tag: Rc<str>,
    pub tag_span: Span,
    pub binding: Rc<str>,
    pub binding_span: Span,
    pub body: PExpr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub enum BinOp { Add, Sub, Mul, Eq, Lt }

impl BinOp {
    pub fn as_str(self) -> &'static str {
        match self { BinOp::Add => "+", BinOp::Sub => "-", BinOp::Mul => "*", BinOp::Eq => "==", BinOp::Lt => "<" }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int { span, .. }
            | Expr::Bool { span, .. }
            | Expr::Str { span, .. }
            | Expr::Var { span, .. }
            | Expr::Lam { span, .. }
            | Expr::App { span, .. }
            | Expr::Bin { span, .. }
            | Expr::If { span, .. }
            | Expr::Let { span, .. }
            | Expr::Record { span, .. }
            | Expr::Field { span, .. }
            | Expr::Variant { span, .. }
            | Expr::Match { span, .. } => *span,
        }
    }
    pub fn kind(&self) -> &'static str {
        match self {
            Expr::Int { .. } => "int", Expr::Bool { .. } => "bool", Expr::Str { .. } => "str",
            Expr::Var { .. } => "var", Expr::Lam { .. } => "lam", Expr::App { .. } => "app",
            Expr::Bin { .. } => "bin", Expr::If { .. } => "if", Expr::Let { .. } => "let",
            Expr::Record { .. } => "record", Expr::Field { .. } => "field", Expr::Variant { .. } => "variant",
            Expr::Match { .. } => "match",
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Int { value, .. } => write!(f, "{value}"),
            Expr::Bool { value, .. } => write!(f, "{value}"),
            Expr::Str { value, .. } => write!(f, "\"{value}\""),
            Expr::Var { name, .. } => write!(f, "{name}"),
            Expr::Lam { param, body, .. } => write!(f, "fn {param} => {body}"),
            Expr::App { fun, arg, .. } => write!(f, "{fun}({arg})"),
            Expr::Bin { op, left, right, .. } => write!(f, "({left} {op} {right})", op = op.as_str()),
            Expr::If { cond, then_branch, else_branch, .. } => write!(f, "if {cond} then {then_branch} else {else_branch}"),
            Expr::Let { recursive, name, value, body, .. } => write!(f, "let{} {name} = {value} in {body}", if *recursive { " rec" } else { "" }),
            Expr::Record { fields, .. } => {
                write!(f, "{{ ")?;
                for (i, (n, e)) in fields.iter().enumerate() { if i > 0 { write!(f, ", ")?; } write!(f, "{n} = {e}")?; }
                write!(f, " }}")
            }
            Expr::Field { record, name, .. } => write!(f, "{record}.{name}"),
            Expr::Variant { tag, payload, .. } => write!(f, "#{tag}({payload})"),
            Expr::Match { scrutinee, arms, fallback, .. } => {
                write!(f, "match {scrutinee} {{ ")?;
                for arm in arms { write!(f, "#{}(x) => {}; ", arm.tag, arm.body)?; }
                if let Some(e) = fallback { write!(f, "_ => {e}; ")?; }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{} at {}..{}", self.message, self.span.start, self.span.end) }
}
