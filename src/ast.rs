//! 抽象语法树。每个表达式携带源码范围与稳定 NodePath。

use crate::common::{NodePath, Span};

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Bool(bool),
    Str,
    Var(String),
    /// fn x -> body
    Lam { param: String, body: Box<Expr> },
    App { fun: Box<Expr>, arg: Box<Expr> },
    /// let [rec] x = e1 in e2（rec 时 e1 中 x 在作用域内）
    Let {
        rec: bool,
        name: String,
        bind: Box<Expr>,
        body: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then_e: Box<Expr>,
        else_e: Box<Expr>,
    },
    /// { a = e, b = e }
    Record { fields: Vec<(String, Expr)> },
    /// e.field
    Proj { record: Box<Expr>, field: String },
    /// Tag(e)
    Variant { tag: String, payload: Box<Expr> },
    /// case e of Tag(x) -> b; _ -> d end
    Case {
        scrut: Box<Expr>,
        tag: String,
        binder: String,
        branch: Box<Expr>,
        default: Box<Expr>,
    },
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub path: NodePath,
}

impl Expr {
    pub fn short(&self) -> String {
        match &self.kind {
            ExprKind::Int(n) => format!("{}", n),
            ExprKind::Bool(b) => format!("{}", b),
            ExprKind::Str => "\"…\"".into(),
            ExprKind::Var(n) => n.clone(),
            ExprKind::Lam { param, .. } => format!("fn {}", param),
            ExprKind::App { .. } => "应用".into(),
            ExprKind::Let { name, rec, .. } => {
                if *rec { format!("let rec {}", name) } else { format!("let {}", name) }
            }
            ExprKind::If { .. } => "if".into(),
            ExprKind::Record { .. } => "记录".into(),
            ExprKind::Proj { field, .. } => format!(".{}", field),
            ExprKind::Variant { tag, .. } => format!("{}(_)", tag),
            ExprKind::Case { tag, .. } => format!("case … {}", tag),
        }
    }
}
