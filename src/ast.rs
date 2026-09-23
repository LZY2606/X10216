//! 抽象语法树、词法与语法解析。所有 AST 节点都带有源码区间 [`Span`]。
use serde::Serialize;

/// 源码字节区间（半开区间 `[start, end)`，均为 UTF-8 字节偏移）。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span { start: a.start.min(b.start), end: a.end.max(b.end) }
    }
}

/// AST 节点种类，用于前端展示与结构断言。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum NodeKind {
    Program,
    Let,
    LitInt,
    LitBool,
    Var,
    Lam,
    App,
    BinOp,
    If,
    Record,
    Field,
    Proj,
    Variant,
    Match,
    Arm,
    Unit,
}

/// 节点身份：同一次解析内唯一；跨解析用 `fp`（结构指纹）做增量对齐。
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct NodeId(pub u32);

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Eq,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct FieldExpr {
    pub name: String,
    pub name_span: Span,
    pub value: Expr,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct MatchArm {
    pub label: String,
    pub label_span: Span,
    pub payload: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub payload_span: Option<Span>,
    /// 载荷类型节点（无载荷时为合成的 unit 节点）。
    pub payload_nid: NodeId,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Expr {
    Unit { span: Span, nid: NodeId },
    LitInt { span: Span, nid: NodeId, value: i64 },
    LitBool { span: Span, nid: NodeId, value: bool },
    Var { span: Span, nid: NodeId, name: String },
    Lam { span: Span, nid: NodeId, param: String, param_span: Span, param_nid: NodeId, body: Box<Expr> },
    App { span: Span, nid: NodeId, fun: Box<Expr>, arg: Box<Expr> },
    BinOp { span: Span, nid: NodeId, op: BinOpKind, lhs: Box<Expr>, rhs: Box<Expr> },
    If { span: Span, nid: NodeId, cond: Box<Expr>, then_e: Box<Expr>, else_e: Box<Expr> },
    Record { span: Span, nid: NodeId, fields: Vec<FieldExpr> },
    Proj { span: Span, nid: NodeId, record: Box<Expr>, field: String, field_span: Span },
    Variant { span: Span, nid: NodeId, label: String, label_span: Span, payload: Option<Box<Expr>> },
    Match { span: Span, nid: NodeId, scrutinee: Box<Expr>, arms: Vec<MatchArm> },
    Let {
        span: Span,
        nid: NodeId,
        rec_: bool,
        name: String,
        name_span: Span,
        value: Box<Expr>,
        body: Option<Box<Expr>>,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Unit { span, .. }
            | Expr::LitInt { span, .. }
            | Expr::LitBool { span, .. }
            | Expr::Var { span, .. }
            | Expr::Lam { span, .. }
            | Expr::App { span, .. }
            | Expr::BinOp { span, .. }
            | Expr::If { span, .. }
            | Expr::Record { span, .. }
            | Expr::Proj { span, .. }
            | Expr::Variant { span, .. }
            | Expr::Match { span, .. }
            | Expr::Let { span, .. } => *span,
        }
    }
    pub fn nid(&self) -> NodeId {
        match self {
            Expr::Unit { nid, .. }
            | Expr::LitInt { nid, .. }
            | Expr::LitBool { nid, .. }
            | Expr::Var { nid, .. }
            | Expr::Lam { nid, .. }
            | Expr::App { nid, .. }
            | Expr::BinOp { nid, .. }
            | Expr::If { nid, .. }
            | Expr::Record { nid, .. }
            | Expr::Proj { nid, .. }
            | Expr::Variant { nid, .. }
            | Expr::Match { nid, .. }
            | Expr::Let { nid, .. } => *nid,
        }
    }
    pub fn kind(&self) -> NodeKind {
        match self {
            Expr::Unit { .. } => NodeKind::Unit,
            Expr::LitInt { .. } => NodeKind::LitInt,
            Expr::LitBool { .. } => NodeKind::LitBool,
            Expr::Var { .. } => NodeKind::Var,
            Expr::Lam { .. } => NodeKind::Lam,
            Expr::App { .. } => NodeKind::App,
            Expr::BinOp { .. } => NodeKind::BinOp,
            Expr::If { .. } => NodeKind::If,
            Expr::Record { .. } => NodeKind::Record,
            Expr::Proj { .. } => NodeKind::Proj,
            Expr::Variant { .. } => NodeKind::Variant,
            Expr::Match { .. } => NodeKind::Match,
            Expr::Let { .. } => NodeKind::Let,
        }
    }
    pub fn children<'a>(&'a self, out: &mut Vec<&'a Expr>) {
        match self {
            Expr::Lam { body, .. } => out.push(body),
            Expr::App { fun, arg, .. } => { out.push(fun); out.push(arg); }
            Expr::BinOp { lhs, rhs, .. } => { out.push(lhs); out.push(rhs); }
            Expr::If { cond, then_e, else_e, .. } => { out.push(cond); out.push(then_e); out.push(else_e); }
            Expr::Record { fields, .. } => { for f in fields { out.push(&f.value); } }
            Expr::Proj { record, .. } => out.push(record),
            Expr::Variant { payload, .. } => if let Some(p) = payload { out.push(p) },
            Expr::Match { scrutinee, arms, .. } => {
                out.push(scrutinee);
                for a in arms { out.push(&a.body); }
            }
            Expr::Let { value, body, .. } => {
                out.push(value);
                if let Some(b) = body { out.push(b); }
            }
            _ => {}
        }
    }
}

/// 解析诊断（词法或语法错误）。
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

/// 一次解析的结果：节点按 id 顺序存放于 `nodes`，`root` 为顶层节点。
#[derive(Clone, Debug, Serialize)]
pub struct Program {
    pub root: Expr,
    pub nodes: Vec<NodeInfo>,
    pub errors: Vec<ParseError>,
}

#[derive(Clone, Debug, Serialize)]
pub struct NodeInfo {
    pub id: NodeId,
    pub kind: NodeKind,
    pub span: Span,
    pub label: String,
}
