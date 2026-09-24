//! Constraint graph model and JSON projections shared by inference and UI.

use serde::Serialize;
use crate::syntax::{NodeId, Span};
use crate::types::{PartOrigin, RTy, UErr};

pub type Cid = String;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum ConstraintKind {
    Eq,
    Instantiate,
    Generalize,
    Restrict,
}

#[derive(Debug, Clone, Serialize)]
pub struct Constraint {
    pub id: Cid,
    pub kind: ConstraintKind,
    pub left: String,
    pub right: String,
    pub origin: PartOrigin,
    /// Extra human-readable detail (e.g. generalized variables, scheme).
    pub detail: String,
    /// Ordered step index in the solving schedule.
    pub order: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct GraphEdge {
    pub from: String,
    pub to: String,
    pub label: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypeView {
    pub vid: String,
    pub rendered: RTy,
}

#[derive(Debug, Clone, Serialize)]
pub enum StepStatus {
    Ok,
    Failed,
    Skipped,
}

#[derive(Debug, Clone, Serialize)]
pub struct Step {
    pub index: usize,
    pub cid: Cid,
    pub kind: ConstraintKind,
    pub title: String,
    pub detail: String,
    pub status: StepStatus,
    pub error: Option<UErr>,
    /// Snapshot of *all* current variable bindings after this step
    /// (rendered types), so the UI can scrub step by step.
    pub snapshot: Vec<TypeView>,
    /// Newly instantiated/generalized variables introduced at this step.
    pub fresh: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize)]
pub struct TyError {
    pub step_index: usize,
    pub cid: Cid,
    pub message: String,
    pub kind: String,
    pub spans: Vec<Span>,
    pub error: UErr,
}

#[derive(Debug, Clone, Serialize)]
pub struct CoreInfo {
    /// Constraint ids in this minimal unsat core.
    pub members: Vec<Cid>,
    /// Constraint ids that must be removed to make the core satisfiable
    /// (same as `members` for a minimal core, kept explicit for clarity).
    pub removable: Vec<Cid>,
    pub size: usize,
    /// Why each member participates (origin description).
    pub reasons: Vec<CoreMember>,
    pub found_at_try: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct CoreMember {
    pub cid: Cid,
    pub node_id: NodeId,
    pub node_kind: String,
    pub detail: String,
}
