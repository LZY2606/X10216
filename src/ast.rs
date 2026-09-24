//! AST、源码范围与稳定节点身份。
//!
//! 节点 id 是从根出发的语法路径（如 `d0/e/f1`），只依赖树结构与子节点次序，
//! 因此标识符改名不会导致其他节点身份漂移。约束 id 由产生它的节点路径派生。

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span { lo: a.lo.min(b.lo), hi: a.hi.max(b.hi) }
    }
}

pub type NodeId = String;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr_ {
    Int(i64),
    Bool(bool),
    Str(String),
    Var(String),
    Fun { param: String, body: Box<Node> },
    App { fun: Box<Node>, arg: Box<Node> },
    If { cond: Box<Node>, then_b: Box<Node>, else_b: Box<Node> },
    Let {
        rec: bool,
        name: String,
        ann: Option<SurfaceTy>,
        value: Box<Node>,
        body: Box<Node>,
    },
    Record { fields: Vec<(String, Node)> },
    Proj { record: Box<Node>, label: String },
    Extend { label: String, value: Box<Node>, rest: Box<Node> },
    Variant { label: String, payload: Option<Box<Node>> },
    Match { scrut: Box<Node>, arms: Vec<(Pat, Node)> },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub id: NodeId,
    pub kind: Expr_,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    Wild,
    PVar(String),
    PTag(String, Option<String>),
}

impl Pat {
    pub fn span_label(&self) -> Option<&str> {
        match self {
            Pat::PTag(l, _) => Some(l),
            _ => None,
        }
    }
}

/// 表面类型标注（可选功能）。
#[derive(Clone, Debug, PartialEq)]
pub enum SurfaceTy {
    TCon(String),
    TVar(String),
    TFun(Box<SurfaceTy>, Box<SurfaceTy>),
    TRecord(Vec<(String, SurfaceTy)>, bool),
    TVariant(Vec<(String, Option<SurfaceTy>)>, bool),
}

#[derive(Clone, Debug)]
pub struct Decl {
    pub id: String,
    pub rec: bool,
    pub name: String,
    pub ann: Option<SurfaceTy>,
    pub value: Node,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub result: Option<Node>,
    pub source: String,
}

/// 解析错误，同样带源码范围，不做“裸 panic”。
#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}: {}", self.span.lo, self.span.hi, self.message)
    }
}

impl Node {
    pub fn new(id: impl Into<String>, kind: Expr_, span: Span) -> Self {
        Node { id: id.into(), kind, span }
    }

    pub fn child_id(&self, tag: &str) -> String {
        format!("{}/{}", self.id, tag)
    }

    pub fn walk<'a>(&'a self, out: &mut Vec<&'a Node>) {
        out.push(self);
        match &self.kind {
            Expr_::Int(_) | Expr_::Bool(_) | Expr_::Str(_) | Expr_::Var(_) => {}
            Expr_::Fun { body, .. } => body.walk(out),
            Expr_::App { fun, arg } => {
                fun.walk(out);
                arg.walk(out);
            }
            Expr_::If { cond, then_b, else_b } => {
                cond.walk(out);
                then_b.walk(out);
                else_b.walk(out);
            }
            Expr_::Let { value, body, .. } => {
                value.walk(out);
                body.walk(out);
            }
            Expr_::Record { fields } => {
                for (_, e) in fields {
                    e.walk(out);
                }
            }
            Expr_::Proj { record, .. } => record.walk(out),
            Expr_::Extend { value, rest, .. } => {
                value.walk(out);
                rest.walk(out);
            }
            Expr_::Variant { payload, .. } => {
                if let Some(p) = payload {
                    p.walk(out);
                }
            }
            Expr_::Match { scrut, arms } => {
                scrut.walk(out);
                for (_, e) in arms {
                    e.walk(out);
                }
            }
        }
    }
}
