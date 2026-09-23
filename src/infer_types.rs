use crate::syntax::Span;
use crate::types::Nid;
use serde::Serialize;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Constraint {
    pub id: Rc<str>,
    pub anchor: Rc<str>,
    pub role: Rc<str>,
    pub cache_key: Rc<str>,
    pub span: Span,
    pub label: String,
    pub left: Nid,
    pub right: Nid,
    pub left_initial: SnapshotRef,
    pub right_initial: SnapshotRef,
}

#[derive(Clone, Debug)]
pub struct SnapshotRef { pub nodes: Vec<crate::types::TypeJson>, pub root: u32 }

#[derive(Clone, Debug, Serialize)]
pub struct ConstraintJson {
    pub id: String,
    pub anchor: String,
    pub role: String,
    pub cache_key: String,
    pub label: String,
    pub span: [u32; 2],
    pub left: SnapshotRefJson,
    pub right: SnapshotRefJson,
    pub disabled: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct SnapshotRefJson { pub nodes: Vec<crate::types::TypeJson>, pub root: u32 }

#[derive(Clone, Debug, Serialize)]
pub struct StepJson {
    pub index: usize,
    pub kind: String,
    pub constraint_id: Option<String>,
    pub title: String,
    pub detail: String,
    pub span: Option<[u32; 2]>,
    pub active_anchor: Option<String>,
    pub before_left: Option<SnapshotRefJson>,
    pub before_right: Option<SnapshotRefJson>,
    pub after: Vec<crate::types::TypeJson>,
    pub after_roots: Vec<u32>,
    pub expr_types: Vec<ExprTypeJson>,
    pub errors: Vec<TypeErrorJson>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ExprTypeJson { pub anchor: String, pub typ: String, pub root: u32 }

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct TypeErrorJson {
    pub id: String,
    pub kind: String,
    pub message: String,
    pub span: [u32; 2],
    pub constraint_id: String,
    pub anchor: String,
    pub left: String,
    pub right: String,
    pub occurs_var: Option<String>,
    pub value_restriction: bool,
    pub missing_fields: Vec<String>,
    pub extra_fields: Vec<String>,
    pub field_conflicts: Vec<FieldConflictJson>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct FieldConflictJson { pub field: String, pub left: String, pub right: String }

#[derive(Clone, Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
    pub constraint_id: Rc<str>,
    pub anchor: Rc<str>,
    pub left: Nid,
    pub right: Nid,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeErrorKind {
    Mismatch,
    Occurs(Nid),
    RowMismatch(RowMismatchDetail),
    ValueRestriction,
    Arity,
    NotRecord,
    NotVariant,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RowMismatchDetail {
    pub missing: Vec<(Rc<str>, Nid)>,
    pub extra: Vec<(Rc<str>, Nid)>,
    pub conflicts: Vec<(Rc<str>, Nid, Nid)>,
}

#[derive(Clone, Debug, Serialize)]
pub struct Notice {
    pub kind: String,
    pub message: String,
    pub span: [u32; 2],
    pub anchor: String,
    pub name: String,
    pub inferred: String,
    pub generalized: String,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct ReuseStats {
    pub ast_reused: usize,
    pub ast_added: usize,
    pub ast_invalidated: usize,
    pub constraint_reused: usize,
    pub constraint_added: usize,
    pub constraint_invalidated: usize,
    pub cache_hits: usize,
    pub cache_misses: usize,
}
