use crate::span::{NodeId, Span};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Expr {
    Int { val: i64, name: Option<String> },
    Bool { val: bool },
    Str { val: String },
    Var { name: String },
    Lam { param: String, body: Box<Expr> },
    App { fun: Box<Expr>, arg: Box<Expr> },
    Let { name: String, rec: bool, value: Box<Expr>, body: Option<Box<Expr>> },
    Seq { first: Box<Expr>, second: Box<Expr> },
    If { cond: Box<Expr>, then_e: Box<Expr>, else_e: Box<Expr> },
    RecordLit { fields: Vec<(String, Expr)> },
    Field { record: Box<Expr>, name: String },
    Variant { tag: String, payload: Option<Box<Expr>> },
    Match { scrutinee: Box<Expr>, arms: Vec<Arm>, wildcard: Option<Box<Expr>> },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Arm {
    pub tag: String,
    pub binding: Option<String>,
    pub body: Expr,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AstNode {
    pub id: NodeId,
    pub span: Span,
    pub expr: Expr,
    pub children: Vec<AstNode>,
}

impl AstNode {
    pub fn walk<'a>(&'a self, out: &mut Vec<&'a AstNode>) {
        out.push(self);
        for c in &self.children {
            c.walk(out);
        }
    }

    pub fn all<'a>(&'a self) -> Vec<&'a AstNode> {
        let mut v = Vec::new();
        self.walk(&mut v);
        v
    }
}
