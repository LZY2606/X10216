// Abstract syntax and source spans for the slice language.

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span {
            start: a.start.min(b.start),
            end: a.end.max(b.end),
        }
    }
    pub fn text<'a>(&self, src: &'a str) -> &'a str {
        &src[self.start..self.end]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u32);

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    Str(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    Var(String, Span),
    Wild(Span),
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Var(_, s) | Pat::Wild(s) => *s,
        }
    }
    pub fn binder(&self) -> Option<&str> {
        match self {
            Pat::Var(n, _) => Some(n),
            Pat::Wild(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Lit(Lit, Span),
    Var(String, Span),
    Lam {
        param: Pat,
        body: Box<Expr>,
        span: Span,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
        span: Span,
    },
    Let {
        rec: bool,
        pat: Pat,
        rhs: Box<Expr>,
        body: Box<Expr>,
        span: Span,
    },
    If {
        cond: Box<Expr>,
        then_e: Box<Expr>,
        else_e: Box<Expr>,
        span: Span,
    },
    Record {
        fields: Vec<(String, Expr, Span)>,
        span: Span,
    },
    Extend {
        base: Box<Expr>,
        fields: Vec<(String, Expr, Span)>,
        span: Span,
    },
    Field {
        record: Box<Expr>,
        name: String,
        name_span: Span,
        span: Span,
    },
    Variant {
        label: String,
        payload: Option<Box<Expr>>,
        span: Span,
    },
    Match {
        scrut: Box<Expr>,
        cases: Vec<(String, Pat, Expr, Span)>,
        span: Span,
    },
    Ann {
        expr: Box<Expr>,
        ty: TyAst,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum TyAst {
    Var(String, Span),
    Con(String, Span),
    Arrow(Box<TyAst>, Box<TyAst>, Span),
    Record(Vec<(String, TyAst)>, bool, Span),
    Variant(Vec<(String, TyAst)>, bool, Span),
}

impl TyAst {
    pub fn span(&self) -> Span {
        match self {
            TyAst::Var(_, s)
            | TyAst::Con(_, s)
            | TyAst::Arrow(.., s)
            | TyAst::Record(.., s)
            | TyAst::Variant(.., s) => *s,
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit(_, s)
            | Expr::Var(_, s)
            | Expr::Lam { span: s, .. }
            | Expr::App { span: s, .. }
            | Expr::Let { span: s, .. }
            | Expr::If { span: s, .. }
            | Expr::Record { span: s, .. }
            | Expr::Extend { span: s, .. }
            | Expr::Field { span: s, .. }
            | Expr::Variant { span: s, .. }
            | Expr::Match { span: s, .. }
            | Expr::Ann { span: s, .. } => *s,
        }
    }
}

/// Structural identity path: stable across edits that do not change shape,
/// and stable under renaming of binders.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathSeg {
    Root,
    LamBody,
    AppFun,
    AppArg,
    LetRhs,
    LetBody,
    IfCond,
    IfThen,
    IfElse,
    RecordField(String),
    ExtendBase,
    ExtendField(String),
    FieldOf,
    VariantPayload,
    MatchScrut,
    MatchCase(String),
    CaseBody(String),
    AnnInner,
}

pub fn visit_path(expr: &Expr, f: &mut impl FnMut(&Expr, Vec<PathSeg>), path: &mut Vec<PathSeg>) {
    f(expr, path.clone());
    match expr {
        Expr::Lit(..) | Expr::Var(..) => {}
        Expr::Lam { body, .. } => {
            path.push(PathSeg::LamBody);
            visit_path(body, f, path);
            path.pop();
        }
        Expr::App { fun, arg, .. } => {
            path.push(PathSeg::AppFun);
            visit_path(fun, f, path);
            path.pop();
            path.push(PathSeg::AppArg);
            visit_path(arg, f, path);
            path.pop();
        }
        Expr::Let {
            rhs, body, ..
        } => {
            path.push(PathSeg::LetRhs);
            visit_path(rhs, f, path);
            path.pop();
            path.push(PathSeg::LetBody);
            visit_path(body, f, path);
            path.pop();
        }
        Expr::If {
            cond,
            then_e,
            else_e,
            ..
        } => {
            for (seg, e) in [
                (PathSeg::IfCond, &**cond),
                (PathSeg::IfThen, &**then_e),
                (PathSeg::IfElse, &**else_e),
            ] {
                path.push(seg);
                visit_path(e, f, path);
                path.pop();
            }
        }
        Expr::Record { fields, .. } => {
            for (name, e, _) in fields {
                path.push(PathSeg::RecordField(name.clone()));
                visit_path(e, f, path);
                path.pop();
            }
        }
        Expr::Extend { base, fields, .. } => {
            path.push(PathSeg::ExtendBase);
            visit_path(base, f, path);
            path.pop();
            for (name, e, _) in fields {
                path.push(PathSeg::ExtendField(name.clone()));
                visit_path(e, f, path);
                path.pop();
            }
        }
        Expr::Field { record, .. } => {
            path.push(PathSeg::FieldOf);
            visit_path(record, f, path);
            path.pop();
        }
        Expr::Variant {
            payload: Some(p), ..
        } => {
            path.push(PathSeg::VariantPayload);
            visit_path(p, f, path);
            path.pop();
        }
        Expr::Variant { payload: None, .. } => {}
        Expr::Match { scrut, cases, .. } => {
            path.push(PathSeg::MatchScrut);
            visit_path(scrut, f, path);
            path.pop();
            for (label, _, body, _) in cases {
                path.push(PathSeg::MatchCase(label.clone()));
                path.push(PathSeg::CaseBody(label.clone()));
                visit_path(body, f, path);
                path.pop();
                path.pop();
            }
        }
        Expr::Ann { expr: e, .. } => {
            path.push(PathSeg::AnnInner);
            visit_path(e, f, path);
            path.pop();
        }
    }
}
