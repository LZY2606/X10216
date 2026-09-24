pub mod gen;

use crate::ast::*;
use crate::types::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ConstraintKind {
    Eq { left: Ty, right: Ty },
    Gen {
        binder: NodeId,
        name: String,
        rec: bool,
        is_value: bool,
        ty: Ty,
    },
    Inst {
        binder: NodeId,
        name: String,
        instantiated: Ty,
        quantified: Vec<MetaVar>,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Constraint {
    pub cid: Cid,
    pub origin: NodeId,
    pub span: Span,
    pub tag: String,
    pub kind: ConstraintKind,
    pub scope: ScopeId,
    pub genv: Vec<(String, NodeId)>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BindingInfo {
    pub node: NodeId,
    pub name: String,
    pub scope: ScopeId,
    pub rec: bool,
    pub is_value: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GenResult {
    pub constraints: Vec<Constraint>,
    pub bindings: Vec<BindingInfo>,
    pub root_ty: Ty,
    pub unbound: Vec<Unbound>,
    pub origins: Vec<(MetaVar, MetaOrigin)>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Unbound {
    pub node: NodeId,
    pub span: Span,
    pub name: String,
}
