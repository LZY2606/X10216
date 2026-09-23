use serde::Serialize;
use crate::span::{Label, Span};

/// Structural, position-independent identity of an AST node.
/// Children first, so that renaming a binding only changes the nodes
/// that actually mention that name; unaffected siblings keep their id.
pub type NodeId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum ExprTag {
    LitInt, LitBool, Var, Lam, App, Let, Rec, Record, Proj, Extend,
    Variant, Match, Ann, If, BinOp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Lit { Int, Bool }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub enum Bin { Add, Sub, Eq, Lt }

impl Bin {
    pub fn symbol(self) -> &'static str {
        match self { Bin::Add => "+", Bin::Sub => "-", Bin::Eq => "=", Bin::Lt => "<" }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Pat {
    Var(String, Span),
    Wild(Span),
    Tag(String, Option<Box<Pat>>, Span),
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Var(_, s) | Pat::Wild(s) | Pat::Tag(_, _, s) => *s,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Arm {
    pub pat: Pat,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Field {
    pub label: Label,
    pub label_span: Span,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Type {
    TCon(String, Span),
    TVar(String, Span),
    TFun(Box<Type>, Box<Type>, Span),
    TRecord(Vec<(Label, Span, Type)>, Option<String>, Span),
    TVariant(Vec<(Label, Span, Option<Type>)>, Option<String>, Span),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::TCon(_, s) | Type::TVar(_, s) => *s,
            Type::TFun(_, _, s) | Type::TRecord(_, _, s) | Type::TVariant(_, _, s) => *s,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LetBind {
    pub rec: bool,
    pub name: String,
    pub name_span: Span,
    pub ann: Option<Type>,
    pub value: Expr,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ExprData {
    Lit(i64, Lit),
    Var(String),
    Lam { param: String, param_span: Span, body: Expr },
    App { fun: Expr, arg: Expr },
    Let(Box<LetBind>),
    Record { fields: Vec<Field>, row: Option<String> },
    Proj { record: Expr, label: Label, label_span: Span },
    Extend { label: Label, label_span: Span, value: Expr, rest: Expr },
    Variant { tag: Label, tag_span: Span, payload: Option<Expr> },
    Match { scrut: Expr, arms: Vec<Arm> },
    Ann { expr: Expr, ty: Type },
    If { cond: Expr, then_: Expr, else_: Expr },
    Bin { op: Bin, lhs: Expr, rhs: Expr },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub data: ExprData,
}

impl Expr {
    pub fn tag(&self) -> ExprTag {
        match &self.data {
            ExprData::Lit(_, Lit::Int) => ExprTag::LitInt,
            ExprData::Lit(_, Lit::Bool) => ExprTag::LitBool,
            ExprData::Var(_) => ExprTag::Var,
            ExprData::Lam { .. } => ExprTag::Lam,
            ExprData::App { .. } => ExprTag::App,
            ExprData::Let(b) if b.rec => ExprTag::Rec,
            ExprData::Let(_) => ExprTag::Let,
            ExprData::Record { .. } => ExprTag::Record,
            ExprData::Proj { .. } => ExprTag::Proj,
            ExprData::Extend { .. } => ExprTag::Extend,
            ExprData::Variant { .. } => ExprTag::Variant,
            ExprData::Match { .. } => ExprTag::Match,
            ExprData::Ann { .. } => ExprTag::Ann,
            ExprData::If { .. } => ExprTag::If,
            ExprData::Bin { .. } => ExprTag::BinOp,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Program {
    pub expr: Expr,
}

// ---- FNV-1a structural hashing (span independent) ----

const FNV_OFFSET: u64 = 0xcbf29ce484222325;
const FNV_PRIME: u64 = 0x100000001b3;

fn mix(acc: &mut u64, bytes: &[u8]) {
    for &x in bytes {
        *acc ^= x as u64;
        *acc = acc.wrapping_mul(FNV_PRIME);
    }
}
fn mix_u64(acc: &mut u64, x: u64) { mix(acc, &x.to_le_bytes()); }
fn mix_str(acc: &mut u64, s: &str) { mix(acc, &[0xFF]); mix(acc, s.as_bytes()); }

fn hash_type(t: &Type, h: &mut u64) {
    match t {
        Type::TCon(n, _) => { mix(h, b"tcon"); mix_str(h, n); }
        Type::TVar(n, _) => { mix(h, b"tvar"); mix_str(h, n); }
        Type::TFun(a, c, _) => { mix(h, b"tfun"); hash_type(a, h); hash_type(c, h); }
        Type::TRecord(fs, row, _) => {
            mix(h, b"rrec");
            for (l, _, t) in fs { mix_str(h, &l.0); hash_type(t, h); }
            match row { Some(n) => { mix(h, b"open"); mix_str(h, n); } None => mix(h, b"closed") }
        }
        Type::TVariant(fs, row, _) => {
            mix(h, b"rvar");
            for (l, _, t) in fs {
                mix_str(h, &l.0);
                match t { Some(t) => { mix(h, b"some"); hash_type(t, h); } None => mix(h, b"none") }
            }
            match row { Some(n) => { mix(h, b"open"); mix_str(h, n); } None => mix(h, b"closed") }
        }
    }
}

fn hash_pat(p: &Pat, h: &mut u64) {
    match p {
        Pat::Var(n, _) => { mix(h, b"pvar"); mix_str(h, n); }
        Pat::Wild(_) => mix(h, b"pwild"),
        Pat::Tag(n, inner, _) => {
            mix(h, b"ptag"); mix_str(h, n);
            if let Some(i) = inner { hash_pat(i, h); } else { mix(h, b"none"); }
        }
    }
}

pub fn compute_id(e: &Expr) -> NodeId {
    let mut h = FNV_OFFSET;
    match &e.data {
        ExprData::Lit(n, Lit::Int) => { mix(&mut h, b"eint"); mix_u64(&mut h, *n as u64); }
        ExprData::Lit(n, Lit::Bool) => { mix(&mut h, b"ebool"); mix(&mut h, &[*n as u8]); }
        ExprData::Var(n) => { mix(&mut h, b"evar"); mix_str(&mut h, n); }
        ExprData::Lam { param, body, .. } => {
            mix(&mut h, b"elam"); mix_str(&mut h, param); mix_u64(&mut h, body.id);
        }
        ExprData::App { fun, arg } => {
            mix(&mut h, b"eapp"); mix_u64(&mut h, fun.id); mix_u64(&mut h, arg.id);
        }
        ExprData::Let(b) => {
            mix(&mut h, if b.rec { b"erec" } else { b"elet" });
            mix_str(&mut h, &b.name);
            if let Some(a) = &b.ann { hash_type(a, &mut h); } else { mix(&mut h, b"noann"); }
            mix_u64(&mut h, b.value.id);
            mix_u64(&mut h, b.body.id);
        }
        ExprData::Record { fields, row } => {
            mix(&mut h, if *closed { b"erecord" } else { b"eorecord" });
            for f in fields {
                mix_str(&mut h, &f.label.0);
                mix_u64(&mut h, f.value.id);
            }
            mix(&mut h, b"endf");
        }
        ExprData::Proj { record, label, .. } => {
            mix(&mut h, b"eproj"); mix_str(&mut h, &label.0); mix_u64(&mut h, record.id);
        }
        ExprData::Extend { label, value, rest, .. } => {
            mix(&mut h, b"eextend"); mix_str(&mut h, &label.0);
            mix_u64(&mut h, value.id); mix_u64(&mut h, rest.id);
        }
        ExprData::Variant { tag, payload, .. } => {
            mix(&mut h, b"evariant"); mix_str(&mut h, &tag.0);
            if let Some(p) = payload { mix_u64(&mut h, p.id); } else { mix(&mut h, b"none"); }
        }
        ExprData::Match { scrut, arms } => {
            mix(&mut h, b"ematch"); mix_u64(&mut h, scrut.id);
            for a in arms {
                hash_pat(&a.pat, &mut h);
                mix_u64(&mut h, a.body.id);
            }
            mix(&mut h, b"endf");
        }
        ExprData::Ann { expr, ty } => {
            mix(&mut h, b"eann"); mix_u64(&mut h, expr.id); hash_type(ty, &mut h);
        }
        ExprData::If { cond, then_, else_ } => {
            mix(&mut h, b"eif"); mix_u64(&mut h, cond.id);
            mix_u64(&mut h, then_.id); mix_u64(&mut h, else_.id);
        }
        ExprData::Bin { op, lhs, rhs } => {
            mix(&mut h, b"ebin"); mix_str(&mut h, op.symbol());
            mix_u64(&mut h, lhs.id); mix_u64(&mut h, rhs.id);
        }
    }
    h
}
