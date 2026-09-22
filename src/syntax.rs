// AST, spans and stable identifiers for the "类型切片台" language.

use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn lo(self) -> u32 {
        self.0
    }
    pub fn hi(self) -> u32 {
        self.1
    }
    pub fn join(self, other: Span) -> Span {
        Span(self.0.min(other.0), self.1.max(other.1))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct NodeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct CId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct VarId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct BindId(pub u32);

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    Str(String),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum FieldInit {
    Expr(NodeId),
    /// `{ ... | base }` style row extension base.
    Rest(NodeId),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Pattern {
    /// Variant pattern: tag name, optional binder name, binder node id.
    Tag(String, Option<(String, NodeId)>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    Lam {
        param: String,
        param_node: NodeId,
        body: NodeId,
    },
    App {
        fun: NodeId,
        arg: NodeId,
    },
    Let {
        rec: bool,
        name: String,
        name_node: NodeId,
        bound: NodeId,
        body: NodeId,
    },
    Record {
        fields: Vec<(String, NodeId, FieldInit)>,
    },
    Field {
        record: NodeId,
        label: String,
        label_span: Span,
    },
    Variant {
        tag: String,
        payload: Option<NodeId>,
    },
    Match {
        scrut: NodeId,
        arms: Vec<(Pattern, NodeId)>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub kind: &'static str,
    pub span: Span,
    pub expr: Expr,
    /// Positional path, stable across edits that do not touch this node and
    /// stable across renames of binders.
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Program {
    pub root: NodeId,
    pub nodes: Vec<Node>,
}

impl Program {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0 as usize]
    }
}
