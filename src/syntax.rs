//! AST with stable, rename-insensitive node identity.

use crate::span::{NodeId, Span};
use serde::{Deserialize, Serialize};

pub type BinderId = u32;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expr {
    Int { val: i64 },
    Str { val: String },
    Bool { val: bool },
    Var { name: String, binder: Option<BinderId> },
    Abs { param: String, param_id: BinderId, body: Box<Expr> },
    App { fun: Box<Expr>, arg: Box<Expr> },
    Let { name: String, binder: BinderId, rec: bool, rhs: Box<Expr>, body: Box<Expr> },
    If { cond: Box<Expr>, then_e: Box<Expr>, else_e: Box<Expr> },
    Record { fields: Vec<(String, NodeId, Expr)> },
    Proj { obj: Box<Expr>, field: String },
    Extend { base: Option<Box<Expr>>, fields: Vec<(String, NodeId, Expr)> },
    Inject { tag: String, payload: Option<Box<Expr>> },
    Match { scrut: Box<Expr>, arms: Vec<Arm>, default: Option<DefaultArm> },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Arm {
    pub tag: String,
    pub payload: Option<(String, BinderId)>,
    pub body: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DefaultArm {
    pub binder: Option<(String, BinderId)>,
    pub body: Expr,
}

#[derive(Clone, Debug)]
pub struct AstNode {
    pub id: NodeId,
    pub span: Span,
    pub fingerprint: u64,
    pub kind: String,
    pub label: String,
    pub children: Vec<NodeId>,
    pub expr: Expr,
    pub binders: Vec<BinderId>,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub source: String,
    pub root: NodeId,
    pub nodes: Vec<AstNode>,
    pub binder_names: Vec<(BinderId, String)>,
    pub parse_errors: Vec<crate::span::ParseError>,
}

impl Expr {
    pub fn walk<'a>(&'a self, f: &mut impl FnMut(&'a Expr)) {
        f(self);
        match self {
            Expr::Abs { body, .. } => body.walk(f),
            Expr::App { fun, arg } => { fun.walk(f); arg.walk(f); }
            Expr::Let { rhs, body, .. } => { rhs.walk(f); body.walk(f); }
            Expr::If { cond, then_e, else_e } => { cond.walk(f); then_e.walk(f); else_e.walk(f); }
            Expr::Record { fields } => fields.iter().for_each(|(_, _, e)| e.walk(f)),
            Expr::Proj { obj, .. } => obj.walk(f),
            Expr::Extend { base, fields } => {
                if let Some(b) = base { b.walk(f); }
                fields.iter().for_each(|(_, _, e)| e.walk(f));
            }
            Expr::Inject { payload, .. } => if let Some(p) = payload { p.walk(f) },
            Expr::Match { scrut, arms, default } => {
                scrut.walk(f);
                arms.iter().for_each(|a| a.body.walk(f));
                if let Some(d) = default { d.body.walk(f); }
            }
            _ => {}
        }
    }
}
