use crate::span::Span;
use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct NodeId(pub u32);

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub key: u64,
    pub span: Span,
    pub kind: NodeKind,
    /// Stable textual label for debugging/UI: a name if this node binds one.
    pub binder_name: Option<String>,
}

impl Node {
    pub fn label(&self) -> &str {
        match &self.kind {
            NodeKind::Var(name) => name,
            _ => self.kind.kernel(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum NodeKind {
    LitInt(i64),
    LitStr(String),
    LitBool(bool),
    Var(String),
    Lam { var: NodeId, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Let { bindings: Vec<BindingId>, body: NodeId },
    Record { fields: Vec<(String, NodeId)> },
    Field { obj: NodeId, label: String },
    Extend { label: String, value: NodeId, rest: Option<NodeId> },
    Tag { label: String, payload: NodeId },
    Match { scrut: NodeId, arms: Vec<ArmId> },
}

impl NodeKind {
    pub fn kernel(&self) -> &'static str {
        match self {
            NodeKind::LitInt(_) => "int",
            NodeKind::LitStr(_) => "str",
            NodeKind::LitBool(_) => "bool",
            NodeKind::Var(_) => "var",
            NodeKind::Lam { .. } => "lam",
            NodeKind::App { .. } => "app",
            NodeKind::Let { .. } => "let",
            NodeKind::Record { .. } => "record",
            NodeKind::Field { .. } => "field",
            NodeKind::Extend { .. } => "extend",
            NodeKind::Tag { .. } => "tag",
            NodeKind::Match { .. } => "match",
        }
    }
    pub fn children(&self) -> Vec<NodeId> {
        match self {
            NodeKind::LitInt(_) | NodeKind::LitStr(_) | NodeKind::LitBool(_) | NodeKind::Var(_) => {
                vec![]
            }
            NodeKind::Lam { var, body } => vec![*var, *body],
            NodeKind::App { fun, arg } => vec![*fun, *arg],
            NodeKind::Let { bindings, body } => {
                let mut out: Vec<NodeId> = Vec::new();
                for _ in bindings {
                    // bindings resolved through the Binding tables.
                }
                out.push(*body);
                out
            }
            NodeKind::Record { fields } => fields.iter().map(|(_, n)| *n).collect(),
            NodeKind::Field { obj, .. } => vec![*obj],
            NodeKind::Extend { value, rest, .. } => {
                let mut v = vec![*value];
                v.extend(rest);
                v
            }
            NodeKind::Tag { payload, .. } => vec![*payload],
            NodeKind::Match { scrut, .. } => vec![*scrut],
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct BindingId(pub u32);

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Binding {
    pub id: BindingId,
    pub key: u64,
    pub span: Span,
    pub name: String,
    pub rec_: bool,
    pub value: NodeId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct ArmId(pub u32);

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Arm {
    pub id: ArmId,
    pub key: u64,
    pub span: Span,
    pub pat: Pat,
    pub body: NodeId,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Pat {
    PVar(String),
    PTag {
        label: String,
        var: Option<String>,
    },
}

impl Pat {
    pub fn span_vars(&self) -> Vec<&str> {
        match self {
            Pat::PVar(n) => vec![n],
            Pat::PTag { var, .. } => var.iter().map(|s| s.as_str()).collect(),
        }
    }
    pub fn kernel(&self) -> &'static str {
        match self {
            Pat::PVar(_) => "pvar",
            Pat::PTag { .. } => "ptag",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub root: NodeId,
    pub nodes: Vec<Node>,
    pub bindings: Vec<Binding>,
    pub arms: Vec<Arm>,
    /// For every Var node: stable key of the binding entry it resolves to,
    /// together with the binding name (display only).
    pub var_resolution: Vec<(NodeId, u64, String)>,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0 as usize]
    }
    pub fn binding(&self, id: BindingId) -> &Binding {
        &self.bindings[id.0 as usize]
    }
    pub fn arm(&self, id: ArmId) -> &Arm {
        &self.arms[id.0 as usize]
    }
}
