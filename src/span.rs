use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeId(pub String);

impl NodeId {
    pub fn as_str(&self) -> &str {
        &self.0
    }
    pub fn child(&self, idx: usize) -> NodeId {
        NodeId(format!("{}.{}", self.0, idx))
    }
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Origin {
    pub expr: NodeId,
    pub cid: Option<String>,
    pub note: String,
}

impl Origin {
    pub fn expr(expr: NodeId) -> Self {
        Origin { expr, cid: None, note: String::new() }
    }
    pub fn with_cid(mut self, cid: &str) -> Self {
        self.cid = Some(cid.to_string());
        self
    }
}
