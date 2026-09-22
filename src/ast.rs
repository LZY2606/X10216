use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn text<'a>(&self, src: &'a str) -> &'a str {
        &src[self.start..self.end]
    }
}

/// 稳定的节点身份:按语法树路径分配(如 "0", "0.1", "0.1.0"),
/// 与变量名无关,因此变量改名不会让全部身份漂移。
pub type NodeId = String;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExprKind {
    Int(i64),
    Bool(bool),
    Unit,
    Var(String),
    Fun {
        param: String,
        body: Box<Expr>,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Add(Box<Expr>, Box<Expr>),
    Let {
        rec_: bool,
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_: Box<Expr>,
        else_: Box<Expr>,
    },
    Record {
        fields: Vec<(String, Expr)>,
    },
    Field {
        base: Box<Expr>,
        label: String,
    },
    Variant {
        label: String,
        arg: Box<Expr>,
    },
    Match {
        scrut: Box<Expr>,
        arms: Vec<(String, String, Expr)>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SrcError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for SrcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({}..{})", self.message, self.span.start, self.span.end)
    }
}

impl std::error::Error for SrcError {}
