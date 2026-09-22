// Constraint graph: nodes, provenance, stable identity keys and generation.

use crate::syntax::*;
use crate::types::NodeRaw;
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct CId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NodeIdT(pub NodeRaw);

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ConstraintKey {
    /// Structural path of the owning AST node.
    pub path: String,
    /// Kind tag, e.g. "eq", "inst:id", "gen", "empty_row".
    pub tag: String,
    /// Disambiguation within one path/kind.
    pub ordinal: u32,
    /// Stack of enclosing binder paths (renaming-insensitive).
    pub scope: String,
    /// Fingerprint of the generalization environment at the key site.
    pub gen_env: String,
}

impl ConstraintKey {
    pub fn stable(&self) -> String {
        format!("{}|{}|{}|{}|{}", self.path, self.tag, self.ordinal, self.scope, self.gen_env)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EqSpec {
    pub left: NodeIdT,
    pub right: NodeIdT,
    /// Short human label e.g. "f : t_arg -> t_ret".
    pub label: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InstSpec {
    pub name: String,
    /// Type node the instantiated copy is equated with.
    pub target: NodeIdT,
    /// Let constraint in which the scheme was generalized (0 for ambient builtins).
    pub origin_let: CId,
    pub occurrence: NodeIdT,
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenSpec {
    pub name: String,
    pub value_restricted: bool,
    pub rec: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EmptyRowSpec {
    pub row: NodeIdT,
    pub sort: &'static str,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CKind {
    Block,
    Marker,
    Eq(EqSpec),
    Inst(InstSpec),
    Gen(GenSpec),
    EmptyRow(EmptyRowSpec),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstraintNode {
    pub id: CId,
    pub parent: Option<CId>,
    pub children: Vec<CId>,
    pub kind: CKind,
    pub span: Span,
    /// Expression node this constraint primarily explains.
    pub origin: Option<NodeIdT>,
    pub key: ConstraintKey,
    /// True if the user temporarily disabled this constraint.
    pub disabled: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeNodeInfo {
    pub id: NodeIdT,
    pub expr: Option<NodeIdT>,
    pub span: Option<Span>,
    pub hint: String,
    pub flex_row: bool,
    pub rigid: Option<String>,
    pub concrete: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AstNodeInfo {
    pub id: NodeIdT,
    pub path: String,
    pub span: Span,
    pub kind: String,
    pub label: String,
    pub children: Vec<NodeIdT>,
}

/// Fingerprint of the polymorphism environment visible at a use site.
/// Entries are `binder_path:binder_name`, including only generalized lets
/// and ambient builtins. Renaming a bound variable therefore changes its own
/// fingerprint but not the structural path identity.
#[derive(Default, Clone, Debug)]
pub struct GenFingerprint {
    entries: Vec<String>,
}

impl GenFingerprint {
    pub fn push(&mut self, binder_path: &str, name: &str, generalized: bool, rec: bool) {
        if generalized {
            self.entries.push(format!("{}:{}:g", binder_path, name));
        } else if rec {
            self.entries.push(format!("{}:{}:m", binder_path, name));
        }
    }
    pub fn pop(&mut self) {
        self.entries.pop();
    }
    pub fn string(&self) -> String {
        self.entries.join(">")
    }
}

pub struct ConstraintGraph {
    pub nodes: Vec<ConstraintNode>,
    pub type_nodes: Vec<TypeNodeInfo>,
    pub ast_nodes: Vec<AstNodeInfo>,
    pub root: CId,
    pub expr_type: std::collections::HashMap<NodeIdT, NodeIdT>,
    /// AST path per node id.
    pub ast_paths: std::collections::HashMap<NodeIdT, String>,
}

pub mod serde_graph {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct EqSpecDto {
        pub left: u32,
        pub right: u32,
        pub label: String,
    }
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct InstSpecDto {
        pub name: String,
        pub target: u32,
        pub origin_let: u32,
        pub occurrence: u32,
    }
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct GenSpecDto {
        pub name: String,
        pub value_restricted: bool,
        pub rec: bool,
    }
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct EmptyRowSpecDto {
        pub row: u32,
        pub sort: String,
    }
    #[derive(Clone, Debug, Serialize, Deserialize)]
    pub struct ConstraintNodeDto {
        pub id: u32,
        pub parent: Option<u32>,
        pub children: Vec<u32>,
        pub kind: String,
        pub eq: Option<EqSpecDto>,
        pub inst: Option<InstSpecDto>,
        pub gen: Option<GenSpecDto>,
        pub empty: Option<EmptyRowSpecDto>,
        pub span: Span,
        pub origin: Option<u32>,
        pub key_path: String,
        pub key_tag: String,
        pub key_ordinal: u32,
        pub key_scope: String,
        pub key_gen_env: String,
        pub disabled: bool,
    }

    pub fn to_dto(n: &ConstraintNode) -> ConstraintNodeDto {
        let mut dto = ConstraintNodeDto {
            id: n.id.0,
            parent: n.parent.map(|p| p.0),
            children: n.children.iter().map(|c| c.0).collect(),
            kind: match &n.kind {
                CKind::Block => "block",
                CKind::Marker => "marker",
                CKind::Eq(_) => "eq",
                CKind::Inst(_) => "inst",
                CKind::Gen(_) => "gen",
                CKind::EmptyRow(_) => "empty_row",
            }
            .into(),
            eq: None,
            inst: None,
            gen: None,
            empty: None,
            span: n.span,
            origin: n.origin.map(|x| x.0),
            key_path: n.key.path.clone(),
            key_tag: n.key.tag.clone(),
            key_ordinal: n.key.ordinal,
            key_scope: n.key.scope.clone(),
            key_gen_env: n.key.gen_env.clone(),
            disabled: n.disabled,
        };
        match &n.kind {
            CKind::Eq(e) => {
                dto.eq = Some(EqSpecDto {
                    left: e.left.0,
                    right: e.right.0,
                    label: e.label.clone(),
                });
            }
            CKind::Inst(i) => {
                dto.inst = Some(InstSpecDto {
                    name: i.name.clone(),
                    target: i.target.0,
                    origin_let: i.origin_let.0,
                    occurrence: i.occurrence.0,
                });
            }
            CKind::Gen(g) => {
                dto.gen = Some(GenSpecDto {
                    name: g.name.clone(),
                    value_restricted: g.value_restricted,
                    rec: g.rec,
                });
            }
            CKind::EmptyRow(r) => {
                dto.empty = Some(EmptyRowSpecDto { row: r.row.0, sort: r.sort.into() });
            }
            CKind::Block | CKind::Marker => {}
        }
        dto
    }
}
