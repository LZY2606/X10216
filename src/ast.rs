use crate::span::Span;
use serde::{Deserialize, Serialize};

pub type NodeId = u32;

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    Unit,
}

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
#[serde(tag = "kind", content = "data")]
pub enum NodeKind {
    Lit(Lit),
    /// de Bruijn depth; surface name kept only for display
    Var { depth: u32 },
    Lam,
    App,
    Let { rec: bool },
    If,
    Record,
    RecordField,
    Variant,
    Proj,
    Switch,
    Arm { has_binder: bool },
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub span: Span,
    pub children: Vec<NodeId>,
    /// Surface label/binder/var name depending on kind.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    /// Content hash independent of node id, span and bound variable names.
    #[serde(skip)]
    pub hash: u64,
    /// Occurrence ordinal among nodes sharing the same content hash (DFS order).
    #[serde(skip)]
    pub occ: u32,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Ast {
    pub nodes: Vec<Node>,
    pub root: NodeId,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id as usize]
    }

    pub fn add(&mut self, kind: NodeKind, span: Span, name: Option<String>, children: Vec<NodeId>) -> NodeId {
        let id = self.nodes.len() as NodeId;
        self.nodes.push(Node { id, kind, span, name, children, hash: 0, occ: 0 });
        id
    }

    /// Content-addressed hashes, bottom-up. Bound names and spans are ignored so
    /// renaming a variable preserves identity.
    pub fn compute_hashes(&mut self) {
        fn go(ast: &mut Ast, id: NodeId) -> u64 {
            let child_hashes: Vec<u64> = {
                let node = &ast.nodes[id as usize];
                node.children.iter().map(|&c| {
                    let h = ast.nodes[c as usize].hash;
                    if h == 0 { go(ast, c) } else { h }
                }).collect()
            };
            let mut h: u64 = 0xcbf29ce484222325;
            let node = &mut ast.nodes[id as usize];
            let tag = match &node.kind {
                NodeKind::Lit(l) => format!("lit:{:?}", l),
                NodeKind::Var { depth } => format!("var:{}", depth),
                NodeKind::Lam => "lam".to_string(),
                NodeKind::App => "app".to_string(),
                NodeKind::Let { rec } => format!("let:{}", rec),
                NodeKind::If => "if".to_string(),
                NodeKind::Record => "record".to_string(),
                NodeKind::RecordField => format!("field:{}", node.name.as_deref().unwrap_or("")),
                NodeKind::Variant => format!("variant:{}", node.name.as_deref().unwrap_or("")),
                NodeKind::Proj => format!("proj:{}", node.name.as_deref().unwrap_or("")),
                NodeKind::Switch => "switch".to_string(),
                NodeKind::Arm { has_binder } => format!("arm:{}:{}", node.name.as_deref().map(|s| s.as_str()).unwrap_or(""), has_binder),
            };
            for b in tag.bytes() {
                h ^= b as u64;
                h = h.wrapping_mul(0x100000001b3);
            }
            for ch in child_hashes {
                h ^= ch;
                h = h.wrapping_mul(0x100000001b3);
            }
            node.hash = h;
            h
        }
        let root = self.root;
        go(self, root);
        // Occurrence ordinal per hash in DFS order.
        let mut counts: std::collections::HashMap<u64, u32> = Default::default();
        let mut order = Vec::new();
        fn dfs(ast: &Ast, id: NodeId, out: &mut Vec<NodeId>) {
            out.push(id);
            for &c in &ast.node(id).children {
                dfs(ast, c, out);
            }
        }
        dfs(self, root, &mut order);
        for id in order {
            let h = self.nodes[id as usize].hash;
            let e = counts.entry(h).or_insert(0);
            self.nodes[id as usize].occ = *e;
            *e += 1;
        }
    }

    /// Stable identity across edits: content hash + occurrence ordinal.
    pub fn canonical_id(&self, id: NodeId) -> String {
        let n = self.node(id);
        format!("n{:016x}#{}", n.hash, n.occ)
    }
}
