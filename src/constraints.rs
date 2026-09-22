//! Constraint operations generated from the AST.

use serde::{Deserialize, Serialize};

use crate::ast::NodeId;

/// Why two types were equated; drives both the human label and provenance.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum Reason {
    /// `f x`: argument unified with the parameter type.
    AppArg(NodeId),
    /// `f x`: whole application unified with the function result type.
    AppResult(NodeId),
    /// fun param introduced on the lhs of the arrow.
    FunParam(NodeId),
    /// fun body unified with the arrow result.
    FunResult(NodeId),
    /// `if b then .. else ..`: condition must be bool.
    IfCond(NodeId),
    /// the two branches must agree.
    IfBranches(NodeId),
    /// let binding's value type stored/scheme position.
    LetBind(NodeId),
    /// occurrence of a variable (instantiation link).
    VarOccur { use_node: NodeId, binder_node: Option<NodeId> },
    /// a literal's known type.
    Literal(NodeId),
    /// binary operator: int operands (or bool for logical operators).
    BinOpLhs(NodeId),
    BinOpRhs(NodeId),
    BinOpResult(NodeId),
    /// record literal field vs access type.
    RecordField(NodeId, String),
    /// field projection requires the label.
    FieldProject(NodeId, String),
    /// injected variant tag payload.
    VariantTag(NodeId, String),
    /// match arm binds the payload of a tag.
    MatchPayload { arm_index: usize, tag: String },
    /// match whole result equals each arm body.
    MatchArmResult(NodeId, usize),
    /// match scrutinee row vs arm tag.
    MatchScrutTag(NodeId, usize),
    /// match exhaustiveness coverage over all tag arms.
    MatchCoverage(NodeId),
    /// synthetic expectation (for reporting).
    Synthetic(String),
}

impl Reason {
    pub fn describe(&self) -> String {
        match self {
            Reason::AppArg(_) => "argument fits parameter".into(),
            Reason::AppResult(_) => "application result".into(),
            Reason::FunParam(_) => "function parameter".into(),
            Reason::FunResult(_) => "function body type".into(),
            Reason::IfCond(_) => "if-condition must be bool".into(),
            Reason::IfBranches(_) => "if branches must agree".into(),
            Reason::LetBind(_) => "let-bound value".into(),
            Reason::VarOccur { .. } => "occurrence instantiation".into(),
            Reason::Literal(_) => "literal type".into(),
            Reason::BinOpLhs(_) => "operator left operand".into(),
            Reason::BinOpRhs(_) => "operator right operand".into(),
            Reason::BinOpResult(_) => "operator result".into(),
            Reason::RecordField(_, l) => format!("record field '{l}'"),
            Reason::FieldProject(_, l) => format!("projecting field '{l}'"),
            Reason::VariantTag(_, l) => format!("variant tag '{l}' payload"),
            Reason::MatchPayload { tag, .. } => format!("match binds payload of '{tag}'"),
            Reason::MatchArmResult(_, i) => format!("match arm {i} result"),
            Reason::MatchScrutTag(_, i) => format!("match arm {i} tag"),
            Reason::MatchCoverage(_) => "match tag coverage".into(),
            Reason::Synthetic(s) => s.clone(),
        }
    }

    pub fn origin_node(&self) -> Option<NodeId> {
        match self {
            Reason::AppArg(n)
            | Reason::AppResult(n)
            | Reason::FunParam(n)
            | Reason::FunResult(n)
            | Reason::IfCond(n)
            | Reason::IfBranches(n)
            | Reason::LetBind(n)
            | Reason::Literal(n)
            | Reason::BinOpLhs(n)
            | Reason::BinOpRhs(n)
            | Reason::BinOpResult(n)
            | Reason::RecordField(n, _)
            | Reason::FieldProject(n, _)
            | Reason::VariantTag(n, _)
            | Reason::MatchArmResult(n, _)
            | Reason::MatchScrutTag(n, _)
            | Reason::MatchCoverage(n) => Some(*n),
            Reason::VarOccur { use_node, .. } => Some(*use_node),
            Reason::MatchPayload { .. } | Reason::Synthetic(_) => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum Op {
    /// Create a type variable with an owning expression node and a role.
    Fresh { var: u32, owner: NodeId, role: String },
    /// Equate two current type expressions.
    Unify {
        id: u64,
        expected: crate::types::Type,
        actual: crate::types::Type,
        reason: Reason,
        /// span used to point at the relationship itself.
        span_owner: Option<NodeId>,
    },
    /// Enter a let scope: bump the level so RHS variables are rank-marked.
    BeginLet { node: NodeId },
    /// Generalise a value's root type into a scheme.
    Generalize {
        node: NodeId,
        name: String,
        root: crate::types::Type,
        generalized: bool,
        rec: bool,
    },
    /// Instantiate a scheme at a use site, mapping the scheme's root type onto
    /// `target` (a freshly introduced variable).
    Instantiate {
        binder: NodeId,
        use_node: NodeId,
        name: String,
        target: u32,
    },
    /// Declares a binder purely for visualization/notes.
    Note { text: String, owner: Option<NodeId> },
}

impl Op {
    pub fn stable_id(&self) -> u64 {
        use crate::ast::hash_str;
        let mut h = 0xcbf29ce484222325u64;
        h = hash_str(h, &format!("{:?}", self));
        // include the variant tag first is covered by Debug; mix a constant too
        h.wrapping_mul(0x100000001b3)
    }
}

/// A chunk is a pre-order script: runs of flat operations interleaved with
/// references to child node chunks. Flattening yields the global solver order.
#[derive(Clone, Serialize, Deserialize, Debug)]
pub enum Segment {
    Ops(Vec<Op>),
    Child(NodeId),
}

/// A generated chunk: operations plus the "result variable" of the node, and
/// enough structural information to build the graph/diff.
#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Chunk {
    pub node: NodeId,
    pub result: u32,
    /// Variable numbers as originally allocated (chunk-local canonical order).
    pub vars: Vec<u32>,
    /// Symbolic keys aligned with `vars`: (owner node, role).
    pub var_defs: Vec<(NodeId, String)>,
    pub segments: Vec<Segment>,
    /// Child node ids in generation order (used for structural diffs).
    pub children: Vec<NodeId>,
}

/// Key under which a chunk can be reused across edits.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct CacheKey {
    pub node: NodeId,
    /// Hash over this node id and its ordered direct-children ids.
    pub shape: u64,
    /// Hash of the lexical scope (ordered binder entries).
    pub scope: u64,
    /// Hash of the generalization/level environment.
    pub gen_env: u64,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Generated {
    pub chunks: Vec<Chunk>,
    pub root_result: u32,
    /// chunk cache keys aligned with `chunks`.
    pub keys: Vec<CacheKey>,
    /// map variable -> (owner node, role) for naming and provenance.
    pub var_info: Vec<(u32, NodeId, String)>,
    /// map binder node id -> let name (for scope resolution).
    pub binders: Vec<(NodeId, String)>,
}

use crate::ast::hash_str;
use crate::types::Type;

/// Stable id for a unify constraint derived from its types and reason.
pub fn unify_id(expected: &Type, actual: &Type, reason: &Reason) -> u64 {
    let mut h = 0xcbf29ce484222325u64;
    h = hash_str(h, "unify");
    h = hash_str(h, &format!("{:?}", reason));
    h = hash_str(h, &format!("{:?}", expected));
    h = hash_str(h, &format!("{:?}", actual));
    h
}
