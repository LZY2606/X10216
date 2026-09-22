//! AST（解析产物）与用于增量身份匹配的原始树 RawNode。

use crate::span::{Gid, Span};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl BinOp {
    pub fn name(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Pat {
    Wild,
    Tag(String, Option<String>),
}

#[derive(Clone, Debug)]
pub enum TyAnn {
    TVar(String),
    Con(String, Vec<TyAnn>),
    Arrow(Box<TyAnn>, Box<TyAnn>),
    Record(Vec<(String, TyAnn)>, Option<String>),
    Variant(Vec<(String, TyAnn)>, Option<String>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Str,
    Unit,
    Var(String),
    Lam {
        param: String,
        ann: Option<TyAnn>,
        body: Box<Expr>,
    },
    App(Box<Expr>, Box<Expr>),
    Bin(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let {
        rec: bool,
        name: String,
        ann: Option<TyAnn>,
        value: Box<Expr>,
        rest: Box<Expr>,
    },
    Record(Vec<(String, Expr)>),
    Field(Box<Expr>, String),
    RecExt {
        base: Box<Expr>,
        fields: Vec<(String, Expr)>,
    },
    Tag(String, Option<Box<Expr>>),
    Match(Box<Expr>, Vec<(Pat, Expr)>),
    Ann(Box<Expr>, TyAnn),
}

/// 所有 AST 节点共同携带的定位信息。
#[derive(Clone, Debug)]
pub struct Node {
    pub expr: Expr,
    pub gid: Gid,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LetItem {
    pub rec: bool,
    pub name: String,
    pub ann: Option<(TyAnn, Gid, Span)>,
    pub value: Node,
    pub span: Span,
    pub gid: Gid,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub items: Vec<LetItem>,
    pub body: Node,
    pub root_gid: Gid,
    pub raw: RawNode,
}

/// 与语义解耦的语法树，增量编辑时据此复用节点身份。
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RawNode {
    pub gid: Gid,
    pub label: String,
    pub text: Option<String>,
    pub span: Span,
    pub children: Vec<RawNode>,
}

impl RawNode {
    pub fn max_gid(&self) -> Gid {
        let mut m = self.gid;
        for c in &self.children {
            m = m.max(c.max_gid());
        }
        m
    }
    pub fn find(&self, gid: Gid) -> Option<&RawNode> {
        if self.gid == gid {
            return Some(self);
        }
        self.children.iter().find_map(|c| c.find(gid))
    }
}
