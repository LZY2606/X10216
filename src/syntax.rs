//! Surface syntax: spans, AST nodes and source types.

pub type NId = u32;

#[derive(Clone, Copy, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub struct Node {
    pub id: NId,
    pub sid: String,
    pub kind: Kind,
    pub span: Span,
    pub children: Vec<NId>,
    pub label: Option<String>,
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum DefKind {
    Let { rec: bool, value_restricted: bool },
    Param,
    Tag,
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub struct Binder {
    pub id: NId,
    pub name: String,
    pub def: DefKind,
    pub scope: String,
}

#[derive(Clone, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum Kind {
    Program,
    IntLit,
    StrLit,
    BoolLit,
    Var,
    Fn,
    Param,
    App,
    Let { rec: bool },
    Binder,
    If,
    BinOp { op: String },
    Record,
    RecordField { label: String },
    Proj { label: String },
    With { label: String },
    Variant { label: String },
    Match,
    MatchArm { label: String },
    TagBinder,
    Wild,
    Ann,
    TyCon { name: String },
    TyArrow,
    TyRecord { closed: bool },
    TyRecordField { label: String },
    TyVariant { closed: bool },
    TyVariantTag { label: String },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, serde::Serialize, serde::Deserialize)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
    And,
}

impl Op {
    pub fn symbol(self) -> &'static str {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Eq => "==",
            Op::Lt => "<",
            Op::And => "&&",
        }
    }
    pub fn parse(s: &str) -> Option<Op> {
        Some(match s {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "==" => Op::Eq,
            "<" => Op::Lt,
            "&&" => Op::And,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct Program {
    pub root: NId,
    pub nodes: Vec<Node>,
    pub binders: Vec<Binder>,
    /// var-node -> binder node it resolves to
    pub refs: Vec<(NId, NId)>,
    /// top level let binders in order
    pub top_binders: Vec<NId>,
    pub parse_errors: Vec<ParseError>,
}

impl Program {
    pub fn node(&self, id: NId) -> &Node {
        &self.nodes[id as usize]
    }
    pub fn span_of(&self, id: NId) -> Span {
        self.nodes[id as usize].span
    }
    pub fn ref_target(&self, id: NId) -> Option<NId> {
        self.refs
            .iter()
            .find(|(v, _)| *v == id)
            .map(|(_, b)| *b)
    }
    pub fn binder(&self, id: NId) -> Option<&Binder> {
        self.binders.iter().find(|b| b.id == id)
    }
}
