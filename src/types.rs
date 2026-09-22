use crate::syntax::{Span, TypeExpr};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

pub type VarId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Origin {
    pub node_id: usize,
    pub part: &'static str,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Ty {
    Var(VarId, Origin),
    Con(String, Origin),
    Arrow(Box<Ty>, Box<Ty>, Origin),
    Record(Row, Origin),
    Variant(Row, Origin),
}

impl Ty {
    pub fn origin(&self) -> Origin {
        match self {
            Ty::Var(_, o) | Ty::Con(_, o) | Ty::Arrow(_, _, o) | Ty::Record(_, o) | Ty::Variant(_, o) => *o,
        }
    }

    pub fn with_origin(self, origin: Origin) -> Ty {
        match self {
            Ty::Var(v, _) => Ty::Var(v, origin),
            Ty::Con(c, _) => Ty::Con(c, origin),
            Ty::Arrow(a, b, _) => Ty::Arrow(a, b, origin),
            Ty::Record(r, _) => Ty::Record(r, origin),
            Ty::Variant(r, _) => Ty::Variant(r, origin),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RowField {
    pub name: String,
    pub name_span: Span,
    pub value: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RowTail {
    Closed,
    Open(VarId),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Row {
    pub fields: Vec<RowField>,
    pub tail: RowTail,
    pub origin: Origin,
}

impl Row {
    pub fn sort(mut self) -> Self {
        self.fields.sort_by(|a, b| a.name.cmp(&b.name));
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: BTreeSet<VarId>,
    pub monomorphic: BTreeSet<VarId>,
    pub body: Ty,
    pub value_restricted: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConstraintKind {
    Equal,
    FieldPresent,
    FieldAbsent,
    RowExtend,
    VariantMatch,
    Instantiate,
    Generalize,
    Ascribe,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Constraint {
    pub id: String,
    pub kind: ConstraintKind,
    pub label: String,
    pub span: Span,
    pub node_id: usize,
    pub scope: String,
    pub gen_env: String,
    pub left: Ty,
    pub right: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TraceReason {
    Expression,
    Param,
    Result,
    BoundVariable,
    Instantiated,
    Generalized,
    Ascribed,
    RecordField,
    VariantLabel,
    MatchPayload,
    RowTail,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TracePart {
    pub node_id: usize,
    pub span: Span,
    pub reason: TraceReason,
    pub constraint_id: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorKind {
    ConstructorMismatch,
    OccursCheck,
    MissingFields,
    ExtraFields,
    FieldTypeMismatch,
    VariantMissing,
    VariantExtra,
    ArityMismatch,
    UnboundVariable,
    DuplicateField,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeError {
    pub id: usize,
    pub kind: ErrorKind,
    pub message: String,
    pub span: Span,
    pub path: String,
    pub expected: Ty,
    pub actual: Ty,
    pub core: Vec<String>,
    pub alternate_cores: Vec<Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum StepKind {
    Instantiate,
    Generalize,
    Unify,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Step {
    pub index: usize,
    pub kind: StepKind,
    pub constraint_id: String,
    pub description: String,
    pub before: String,
    pub after: String,
    pub error_id: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AstNode {
    pub id: usize,
    pub stable: String,
    pub kind: String,
    pub label: String,
    pub span: Span,
    pub parent: Option<usize>,
    pub path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinalType {
    pub name: String,
    pub span: Span,
    pub scheme: Scheme,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Analysis {
    pub source: String,
    pub nodes: Vec<AstNode>,
    pub constraints: Vec<Constraint>,
    pub steps: Vec<Step>,
    pub errors: Vec<TypeError>,
    pub finals: Vec<FinalType>,
    pub traces: BTreeMap<usize, Vec<TracePart>>,
}

pub fn type_expr_vars(ty: &TypeExpr, out: &mut BTreeSet<String>) {
    match &ty.kind {
        TypeExprKind::Var(v) => { out.insert(v.clone()); }
        TypeExprKind::Con(_) => {}
        TypeExprKind::Arrow(a, b) => {
            type_expr_vars(a, out);
            type_expr_vars(b, out);
        }
        TypeExprKind::Record(fs, tail) | TypeExprKind::Variant(fs, tail) => {
            for f in fs { type_expr_vars(&f.value, out); }
            if let Some(t) = tail { out.insert(t.clone()); }
        }
    }
}

use crate::syntax::TypeExprKind;

pub fn display_ty(ty: &Ty) -> String {
    match ty {
        Ty::Var(v, _) => format!("?v{v}"),
        Ty::Con(c, _) => c.to_lowercase(),
        Ty::Arrow(a, b, _) => format!("{} -> {}", display_arrow_arg(a), display_ty(b)),
        Ty::Record(row, _) => display_row("{", "}", row),
        Ty::Variant(row, _) => display_row("{|", "|}", row),
    }
}

fn display_arrow_arg(ty: &Ty) -> String {
    match ty {
        Ty::Arrow(..) => format!("({})", display_ty(ty)),
        _ => display_ty(ty),
    }
}

fn display_row(start: &str, end: &str, row: &Row) -> String {
    let mut s = String::from(start);
    for (i, f) in row.fields.iter().enumerate() {
        if i > 0 { s.push_str(", "); }
        s.push_str(&f.name);
        s.push_str(": ");
        s.push_str(&display_ty(&f.value));
    }
    match row.tail {
        RowTail::Closed => {}
        RowTail::Open(v) => {
            if !row.fields.is_empty() { s.push_str(", "); }
            s.push_str(&format!("?r{v}"));
        }
    }
    s.push_str(end);
    s
}

pub fn display_scheme(scheme: &Scheme) -> String {
    if scheme.vars.is_empty() {
        display_ty(&scheme.body)
    } else {
        let vars = scheme.vars.iter().map(|v| format!("?v{v}")).collect::<Vec<_>>().join(" ");
        format!("forall {vars}. {}", display_ty(&scheme.body))
    }
}
