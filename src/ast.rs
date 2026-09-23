//! AST 定义。每个节点带一个「路径身份」nid，由父节点身份 + 结构槽位 + 位置派生，
//! 因此重命名变量（位置不变）不会导致身份漂移。

use crate::span::Span;
use serde::Serialize;
use twox_hash::XxHash64;
use std::hash::Hasher;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct NodeId(pub u64);

impl NodeId {
    pub fn child(self, role: &str, index: usize) -> NodeId {
        let mut h = XxHash64::with_seed(self.0 ^ 0x9E37_79B9_7F4A_7C15);
        h.write(role.as_bytes());
        h.write_u8(0);
        h.write_usize(index);
        NodeId(h.finish())
    }
}

pub const ROOT: NodeId = NodeId(0x5147_4200_0000_0001);

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Program {
    pub nid: NodeId,
    pub bindings: Vec<Binding>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Binding {
    pub nid: NodeId,
    pub span: Span,
    pub rec_kw: bool,
    pub name: String,
    pub name_span: Span,
    pub expr: Expr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Literal {
    Int,
    Bool,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum Expr {
    Lit {
        nid: NodeId,
        span: Span,
        lit: Literal,
        value: String,
    },
    Var {
        nid: NodeId,
        span: Span,
        name: String,
    },
    Lam {
        nid: NodeId,
        span: Span,
        params: Vec<(String, Span, NodeId)>,
        body: Box<Expr>,
    },
    App {
        nid: NodeId,
        span: Span,
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    Let {
        nid: NodeId,
        span: Span,
        rec_kw: bool,
        name: String,
        name_span: Span,
        bound: Box<Expr>,
        body: Box<Expr>,
    },
    If {
        nid: NodeId,
        span: Span,
        cond: Box<Expr>,
        then_e: Box<Expr>,
        else_e: Box<Expr>,
    },
    Record {
        nid: NodeId,
        span: Span,
        fields: Vec<(String, Span, Expr)>,
        open: bool,
    },
    Proj {
        nid: NodeId,
        span: Span,
        record: Box<Expr>,
        label: String,
        label_span: Span,
    },
    Update {
        nid: NodeId,
        span: Span,
        record: Box<Expr>,
        fields: Vec<(String, Span, Expr)>,
    },
    Inject {
        nid: NodeId,
        span: Span,
        label: String,
        label_span: Span,
        payload: Box<Expr>,
    },
    Match {
        nid: NodeId,
        span: Span,
        scrutinee: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    Paren {
        nid: NodeId,
        span: Span,
        inner: Box<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct MatchArm {
    pub nid: NodeId,
    pub span: Span,
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum Pattern {
    Var {
        nid: NodeId,
        span: Span,
        name: String,
    },
    Variant {
        nid: NodeId,
        span: Span,
        label: String,
        label_span: Span,
        payload: Option<Box<Pattern>>,
    },
}

impl Expr {
    pub fn nid(&self) -> NodeId {
        match self {
            Expr::Lit { nid, .. }
            | Expr::Var { nid, .. }
            | Expr::Lam { nid, .. }
            | Expr::App { nid, .. }
            | Expr::Let { nid, .. }
            | Expr::If { nid, .. }
            | Expr::Record { nid, .. }
            | Expr::Proj { nid, .. }

            | Expr::Update { nid, .. }
            | Expr::Inject { nid, .. }
            | Expr::Match { nid, .. }
            | Expr::Paren { nid, .. } => *nid,
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit { span, .. }
            | Expr::Var { span, .. }
            | Expr::Lam { span, .. }
            | Expr::App { span, .. }
            | Expr::Let { span, .. }
            | Expr::If { span, .. }
            | Expr::Record { span, .. }
            | Expr::Proj { span, .. }
            | Expr::Update { span, .. }
            | Expr::Inject { span, .. }
            | Expr::Match { span, .. }
            | Expr::Paren { span, .. } => *span,
        }
    }
}

impl Pattern {
    pub fn nid(&self) -> NodeId {
        match self {
            Pattern::Var { nid, .. } | Pattern::Variant { nid, .. } => *nid,
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Pattern::Var { span, .. } | Pattern::Variant { span, .. } => *span,
        }
    }
}
