// AST, lexer, parser and structurally-stable node identity.

use serde::{Deserialize, Serialize};

pub const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
pub const FNV_PRIME: u64 = 0x1000_0000_1b3;

pub fn fnv1a(seed: u64, bytes: &[u8]) -> u64 {
    let mut h = seed;
    for &b in bytes {
        h ^= b as u64;
        h = h.wrapping_mul(FNV_PRIME);
    }
    h
}

pub fn seed_str(s: &str) -> u64 {
    fnv1a(FNV_OFFSET, s.as_bytes())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NodeId(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct ScopeId(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Cid(pub u64);

impl NodeId {
    pub fn hex(self) -> String {
        format!("n{:012x}", self.0)
    }
    pub fn child(self, edge: Edge) -> NodeId {
        let mut bytes = Vec::with_capacity(9);
        edge.encode(&mut bytes);
        NodeId(fnv1a(self.0, &bytes))
    }
}

impl Cid {
    pub fn hex(self) -> String {
        format!("c{:012x}", self.0)
    }
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.hex())
    }
}
impl std::fmt::Display for Cid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.hex())
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Edge {
    Child(u8),
    Keyed(u64),
}

impl Edge {
    fn encode(self, out: &mut Vec<u8>) {
        match self {
            Edge::Child(b) => {
                out.push(1);
                out.push(b);
            }
            Edge::Keyed(k) => {
                out.push(2);
                out.extend_from_slice(&k.to_le_bytes());
            }
        }
    }
}

pub fn keyed_label(label: &str) -> u64 {
    fnv1a(FNV_OFFSET, label.as_bytes())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Lit {
    Int(i64),
    Str(String),
    Unit,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LetBind {
    pub rec: bool,
    pub name: String,
    pub name_span: Span,
    pub rhs: Spanned<Expr>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pub tag: String,
    pub tag_span: Span,
    pub wildcard: bool,
    pub binding: Option<String>,
    pub binding_span: Option<Span>,
    pub body: Spanned<Expr>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Expr {
    Lit(Lit),
    Var(String),
    Fun {
        param: String,
        param_span: Span,
        body: Box<Spanned<Expr>>,
    },
    App {
        fun: Box<Spanned<Expr>>,
        arg: Box<Spanned<Expr>>,
    },
    Let {
        bind: LetBind,
        body: Box<Spanned<Expr>>,
    },
    Record(Vec<RecordField>),
    Field {
        object: Box<Spanned<Expr>>,
        label: String,
        label_span: Span,
    },
    Variant {
        tag: String,
        tag_span: Span,
        payload: Option<Box<Spanned<Expr>>>,
    },
    Match {
        scrut: Box<Spanned<Expr>>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RecordField {
    pub name: String,
    pub name_span: Span,
    pub value: Spanned<Expr>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Spanned<T> {
    pub node: NodeId,
    pub span: Span,
    pub inner: T,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NodeInfo {
    pub id: NodeId,
    pub span: Span,
    pub kind: String,
    pub label: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Program {
    pub root: Spanned<Expr>,
    pub nodes: Vec<NodeInfo>,
    pub source: String,
    pub parse_errors: Vec<ParseError>,
}
