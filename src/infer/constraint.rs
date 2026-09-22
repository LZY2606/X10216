use serde::Serialize;
use crate::lang::{NodeId, Span};
use super::types::CId;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
pub enum CTermKind {
    /// A solver variable
    Var,
    Con,
    Arrow,
    Record,
    Variant,
    /// open empty row
    OpenRow,
}

#[derive(Clone, Debug, Serialize)]
pub struct CTerm {
    pub kind: CTermKind,
    pub name: Option<String>,
    pub children: Vec<CTerm>,
    /// for record/variant rows, in declaration order
    pub fields: Vec<(String, CTerm)>,
    pub closed: bool,
    /// solver-allocated id (filled after solving), for cross-references
    pub tid: Option<u32>,
    pub rid: Option<u32>,
}

impl CTerm {
    pub fn var() -> Self {
        CTerm { kind: CTermKind::Var, name: None, children: vec![], fields: vec![], closed: false, tid: None, rid: None }
    }
    pub fn con(name: &str) -> Self {
        CTerm { kind: CTermKind::Con, name: Some(name.into()), children: vec![], fields: vec![], closed: false, tid: None, rid: None }
    }
    pub fn arrow(dom: CTerm, cod: CTerm) -> Self {
        CTerm { kind: CTermKind::Arrow, name: None, children: vec![dom, cod], fields: vec![], closed: false, tid: None, rid: None }
    }
    pub fn record(fields: Vec<(String, CTerm)>, closed: bool) -> Self {
        CTerm { kind: CTermKind::Record, name: None, children: vec![], fields, closed, tid: None, rid: None }
    }
    pub fn variant(fields: Vec<(String, CTerm)>, closed: bool) -> Self {
        CTerm { kind: CTermKind::Variant, name: None, children: vec![], fields, closed, tid: None, rid: None }
    }
    pub fn open_row() -> Self {
        CTerm { kind: CTermKind::OpenRow, name: None, children: vec![], fields: vec![], closed: false, tid: None, rid: None }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
pub enum EqKind {
    /// ordinary equation introduced at an AST node
    Expr,
    /// instantiation copy of a generalized scheme
    Instantiate,
    /// builtin scheme instantiation
    Builtin,
}

#[derive(Clone, Debug, Serialize)]
pub struct EqCon {
    pub id: CId,
    pub kind: EqKind,
    pub lhs: CTerm,
    pub rhs: CTerm,
    /// AST node producing this equation
    pub node: NodeId,
    /// AST node this equation is about (e.g. the use site for instantiation)
    pub site: NodeId,
    /// human readable label
    pub label: String,
    pub span: Span,
    /// structural key, stable across edits/renames
    pub key: String,
    /// scheme scope this belongs to (instance cids point into quantified schemes)
    pub scope: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
pub enum ScopeKind {
    Let,
    Rec,
    Root,
}

#[derive(Clone, Debug, Serialize)]
pub struct Scope {
    pub id: u32,
    pub kind: ScopeKind,
    pub binder_anchor: Option<String>,
    pub binder_name: String,
    pub node: Option<NodeId>,
    pub value_restricted: bool,
    /// equations belonging to this scope, in solve order
    pub cids: Vec<CId>,
    pub children: Vec<u32>,
    /// instance equations (use sites), solved within body
    pub instances: Vec<CId>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ConstraintSystem {
    pub equations: Vec<EqCon>,
    pub scopes: Vec<Scope>,
    pub root_scope: u32,
}
