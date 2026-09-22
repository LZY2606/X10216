//! Serializable constraint model.  Every meta variable is referenced by a
//! node-relative stable string key, so meta identities survive edits that
//! happen elsewhere in the program.

use crate::syntax::{NodeId, Span};
use crate::types::TCon;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ActionId(pub String);

impl ActionId {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct MetaKey(pub String);

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct SchemeKey(pub String);

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum STerm {
    V(MetaKey),
    Q(u32),
    C(TCon),
    Arrow(Box<STerm>, Box<STerm>),
    Rec(SRow),
    Variant(SRow),
}

impl STerm {
    pub fn v(key: impl Into<String>) -> STerm {
        STerm::V(MetaKey(key.into()))
    }
    pub fn con(c: TCon) -> STerm {
        STerm::C(c)
    }
    pub fn arrow(a: STerm, b: STerm) -> STerm {
        STerm::Arrow(Box::new(a), Box::new(b))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SRow {
    pub fields: Vec<(String, STerm)>,
    pub end: SRowEnd,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum SRowEnd {
    Closed,
    Open(Box<STerm>),
}

impl SRow {
    pub fn closed(fields: Vec<(String, STerm)>) -> Self {
        let mut fields = fields;
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        SRow { fields, end: SRowEnd::Closed }
    }
    pub fn open(fields: Vec<(String, STerm)>, tail: STerm) -> Self {
        let mut fields = fields;
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        SRow { fields, end: SRowEnd::Open(Box::new(tail)) }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Action {
    pub id: ActionId,
    pub node: NodeId,
    pub span: Span,
    /// Per-node emission index, part of the stable identity.
    pub seq: u32,
    /// FNV hash of the lexical scope at emission time.
    pub scope_key: u64,
    /// FNV hash of the generalization/name environment at emission time.
    pub genv_key: u64,
    pub title: String,
    pub kind: ActionKind,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum ActionKind {
    /// A front-end detected error (e.g. unbound variable).
    Error { message: String },
    /// Introduce a fresh unification variable with provenance.
    Intro {
        key: MetaKey,
        var_kind: String,
        level: u32,
        label: String,
    },
    EnterScope { level: u32 },
    ExitScope,
    Unify {
        left: STerm,
        right: STerm,
        relation: String,
    },
    Prim {
        op: String,
        operands: Vec<MetaKey>,
        result: MetaKey,
        result_con: String,
    },
    Match {
        scrut: MetaKey,
        result: MetaKey,
        arms: Vec<MatchArmSpec>,
        tail: MetaKey,
    },
    Generalize {
        scheme: SchemeKey,
        result: MetaKey,
        at_level: u32,
        is_value: bool,
        role: String,
    },
    Instantiate {
        scheme: SchemeRef,
        result: MetaKey,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct MatchArmSpec {
    pub arm_node: NodeId,
    pub label: String,
    pub payload: Option<MetaKey>,
    pub body: MetaKey,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum SchemeRef {
    /// Scheme produced by a Generalize action.
    Local(SchemeKey),
    /// Builtin polymorphic scheme known to the solver.
    Builtin(String),
}

/// Full artifact of the front end.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConstraintProgram {
    pub root: NodeId,
    pub root_meta: MetaKey,
    /// Expression node -> meta key holding its type.
    pub node_types: Vec<(NodeId, MetaKey)>,
    pub actions: Vec<Action>,
}
