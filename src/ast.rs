//! 抽象语法树与源码范围，以及在增量编辑下保持稳定的节点身份。

use serde::Serialize;

/// 源码半开区间 [start, end)，以字节偏移计。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start as u32, end: end as u32 }
    }
    pub fn union(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// 节点稳定身份：在小幅编辑/改名后保持不变。
pub type NodeId = u64;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub enum Lit {
    Int(String),
    Str(String),
    Bool(bool),
    Unit,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ExprNode {
    pub id: NodeId,
    pub span: Span,
    pub data: Expr,
}

pub type Ex = Box<ExprNode>;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(tag = "kind")]
pub enum Expr {
    Lit { lit: Lit },
    Var { name: String },
    Lam { param: String, param_ty: Option<TyExprNode>, body: Ex },
    App { fun: Ex, arg: Ex },
    Bin { op: String, lhs: Ex, rhs: Ex },
    If { cond: Ex, then_e: Ex, else_e: Ex },
    /// `{ field = e, ..base }`：字段列表按书写顺序。
    Record { base: Option<Ex>, fields: Vec<FieldDef> },
    Field { record: Ex, name: String },
    Tag { label: String, payload: Option<Ex> },
    Match { scrut: Ex, arms: Vec<Arm> },
    Let {
        rec_kw: bool,
        name: String,
        annot: Option<TyExprNode>,
        rhs: Ex,
        body: Ex,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct FieldDef {
    pub name: String,
    pub name_span: Span,
    pub value: Ex,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Arm {
    pub label: String,
    pub label_span: Span,
    pub binding: Option<String>,
    pub body: Ex,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct TyExprNode {
    pub span: Span,
    pub data: TyExpr,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "kind")]
pub enum TyExpr {
    Con { name: String },
    Var { name: String },
    Arrow { from: Box<TyExprNode>, to: Box<TyExprNode> },
    Record { fields: Vec<(String, TyExprNode)>, closed: bool },
    Variant { fields: Vec<(String, TyExprNode)>, closed: bool },
}

/// 绑定出现，用于在推断时把位置对应到 AST 节点。
#[derive(Clone, Copy, Debug)]
pub struct BindingSite {
    pub node: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}
