use crate::ast::NodeId;
use crate::span::Span;
use crate::types::SType;
use serde::Serialize;

/// Content-addressed identity of a constraint (see constraintgen for the mix).
pub type Cid = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ConstraintKind {
    Eq,
    Generalize,
    Instantiate,
    NodeType,
    Unbound,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum Action {
    Eq {
        cid: Cid,
        expected: SType,
        actual: SType,
        origin: Origin,
        /// short human description of why this equation exists
        reason: String,
    },
    Generalize {
        cid: Cid,
        let_node: NodeId,
        name: String,
        /// expression whose current type is generalized
        body_node: NodeId,
        /// variables that must stay monomorphic (outer-scope + value-restriction)
        ambient: Vec<String>,
        recursive: bool,
        origin: Origin,
    },
    Instantiate {
        cid: Cid,
        scheme_name: String,
        /// destination type variable for the fresh copy
        target: SType,
        /// Some for built-in primitives whose scheme is fixed
        builtin: Option<SType>,
        origin: Origin,
        reason: String,
    },
    Unbound {
        cid: Cid,
        name: String,
        origin: Origin,
    },
    NodeType {
        cid: Cid,
        node: NodeId,
        ty: SType,
    },
}

impl Action {
    pub fn cid(&self) -> Cid {
        match self {
            Action::Eq { cid, .. } | Action::Generalize { cid, .. }
            | Action::Instantiate { cid, .. } | Action::NodeType { cid, .. } => *cid,
        }
    }
    pub fn kind(&self) -> ConstraintKind {
        match self {
            Action::Eq { .. } => ConstraintKind::Eq,
            Action::Generalize { .. } => ConstraintKind::Generalize,
            Action::Instantiate { .. } => ConstraintKind::Instantiate,
            Action::NodeType { .. } => ConstraintKind::NodeType,
            Action::Unbound { .. } => ConstraintKind::Unbound,
        }
    }
    pub fn origin(&self) -> Option<&Origin> {
        match self {
            Action::Eq { origin, .. } | Action::Generalize { origin, .. }
            | Action::Instantiate { origin, .. } => Some(origin),
            Action::Unbound { origin, .. } => Some(origin),
            Action::NodeType { .. } => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Origin {
    pub span: Span,
    /// primary expression node responsible
    pub node: Option<NodeId>,
    /// role label, e.g. "函数参数", "函数返回", "if 条件"
    pub role: String,
    /// optional secondary node (e.g. projection field)
    pub node2: Option<NodeId>,
}

impl Origin {
    pub fn new(span: Span, node: Option<NodeId>, role: impl Into<String>) -> Self {
        Origin { span, node, role: role.into(), node2: None }
    }
}
