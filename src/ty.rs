//! Types and row types with provenance.
//!
//! Every constructed type remembers the syntax node that gave rise to it, so
//! pieces of a final type can be traced back to an expression. Type variables
//! carry a stable key derived from the node and role that created them, which
//! survives re-parsing and renaming.

use crate::span::NodeId;
use serde::{Deserialize, Serialize};

pub type VarId = u32;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Ty {
    Var(VarId, VarKey),
    Int(Option<NodeId>),
    Bool(Option<NodeId>),
    Fun(Box<Ty>, Box<Ty>, Option<NodeId>),
    Record(Row, Option<NodeId>),
    Variant(Row, Option<NodeId>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum VarKey {
    /// Stable key derived from a syntax node (node key + role suffix).
    Stable(String),
    /// Fresh variable introduced by instantiation or an internal step.
    Fresh(u32),
}

impl VarKey {
    pub fn stable(node: &NodeId, suffix: &str) -> Self {
        VarKey::Stable(format!("{}:{}", node.key(), suffix))
    }
    pub fn as_label(&self) -> String {
        match self {
            VarKey::Stable(s) => s.clone(),
            VarKey::Fresh(n) => format!("?f{}", n),
        }
    }
}

/// A row: known fields mapping to types, plus an optional extension variable.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Row {
    pub fields: Vec<(String, Ty)>,
    pub ext: Option<VarId>,
    pub origin: Option<NodeId>,
}

impl Row {
    pub fn empty(origin: Option<NodeId>) -> Self {
        Row { fields: Vec::new(), ext: None, origin }
    }
    pub fn open(ext: VarId, origin: Option<NodeId>) -> Self {
        Row { fields: Vec::new(), ext: Some(ext), origin }
    }
    pub fn get(&self, label: &str) -> Option<&Ty> {
        self.fields.iter().find(|(l, _)| l == label).map(|(_, t)| t)
    }
}

impl Ty {
    pub fn origin(&self) -> Option<&NodeId> {
        match self {
            Ty::Int(o) | Ty::Bool(o) | Ty::Fun(_, _, o) | Ty::Record(_, o) | Ty::Variant(_, o) => {
                o.as_ref()
            }
            Ty::Var(..) => None,
        }
    }
    pub fn int() -> Ty {
        Ty::Int(None)
    }
    pub fn bool() -> Ty {
        Ty::Bool(None)
    }
    pub fn fun(a: Ty, b: Ty) -> Ty {
        Ty::Fun(Box::new(a), Box::new(b), None)
    }
    pub fn record(fields: Vec<(String, Ty)>, ext: Option<VarId>, o: Option<NodeId>) -> Ty {
        Ty::Record(Row { fields, ext, origin: o }, o)
    }
    pub fn variant(fields: Vec<(String, Ty)>, ext: Option<VarId>, o: Option<NodeId>) -> Ty {
        Ty::Variant(Row { fields, ext, origin: o }, o)
    }

    pub fn as_var(&self) -> Option<VarId> {
        if let Ty::Var(v, _) = self {
            Some(*v)
        } else {
            None
        }
    }
}

/// A polymorphic type scheme: quantified variable keys with their kind.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Scheme {
    pub body: Ty,
    pub quantified: Vec<VarKey>,
    pub value_restricted: bool,
}
