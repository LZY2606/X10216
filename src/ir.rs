// Constraint-generation output: symbolic types plus a list of keyed operations.
use crate::keys::NodeKey;
use crate::resolve::NodeId;

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarKey(pub String);
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CKey(pub String);
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SchemeKey(pub String);

pub type TId = usize;

#[derive(Clone, Debug)]
pub enum STy {
    Var(VarKey),
    Int,
    Bool,
    Str,
    Arrow(TId, TId),
    Record(Vec<(String, TId)>, Tail),
    Variant(Vec<(String, TId)>, Tail),
}

#[derive(Clone, Debug)]
pub enum Tail { Closed, Open(VarKey) }

pub struct Term {
    pub id: TId,
    pub ty: STy,
    pub birth: NodeId,
}

pub struct TermArena {
    pub terms: Vec<Term>,
}

impl TermArena {
    pub fn new() -> Self { TermArena { terms: Vec::new() } }
    pub fn mk(&mut self, ty: STy, birth: NodeId) -> TId {
        let id = self.terms.len();
        self.terms.push(Term { id, ty, birth });
        id
    }
    pub fn var(&mut self, v: VarKey, birth: NodeId) -> TId { self.mk(STy::Var(v), birth) }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side { Left, Right }

#[derive(Clone, Debug)]
pub enum Op {
    Unify {
        key: CKey,
        left: TId,
        right: TId,
        origin: NodeId,
        reason: String,
        // semantic direction of (left,right)
        left_role: Role,
        right_role: Role,
    },
    BindScheme {
        scheme: SchemeKey,
        binder: String, // human-readable binder label
        node: NodeId,
        gen_node: NodeKey,
        body: TId,
        quantifier_scope: NodeKey,
    },
    Inst {
        key: CKey,
        scheme: SchemeKey,
        target: TId,
        origin: NodeId,
        reason: String,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Role { Actual, Expected, Equal }

impl Op {
    pub fn ckey(&self) -> Option<&CKey> {
        match self {
            Op::Unify { key, .. } | Op::Inst { key, .. } => Some(key),
            Op::BindScheme { .. } => None,
        }
    }
    pub fn origin(&self) -> NodeId {
        match self {
            Op::Unify { origin, .. } | Op::Inst { origin, .. } => *origin,
            Op::BindScheme { node, .. } => *node,
        }
    }
}

pub struct Program {
    pub arena: TermArena,
    pub root: TId,
    pub node_ty: Vec<(NodeId, TId)>,
    pub ops: Vec<Op>,
}
