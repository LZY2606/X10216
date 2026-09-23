//! AST with byte spans and stable, rename-resistant node identities.

use serde::Serialize;

pub type NodeId = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn lo(self) -> u32 { self.0 }
    pub fn hi(self) -> u32 { self.1 }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum Node {
    Program { decls: Vec<NodeId> },
    Let {
        rec: bool,
        bind: Bind,
        value: NodeId,
        #[serde(skip_serializing_if = "Option::is_none")]
        body: Option<NodeId>,
    },
    Lam { bind: Bind, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Var { name: String, #[serde(skip)] resolved: Option<BindId> },
    Int { value: i64 },
    Bool { value: bool },
    Str { value: String },
    Bin { op: BinOp, lhs: NodeId, rhs: NodeId },
    If { cond: NodeId, then_b: NodeId, else_b: NodeId },
    Seq { items: Vec<NodeId> },
    Record { fields: Vec<(String, NodeId)>, rest: Option<NodeId> },
    Proj { target: NodeId, field: String },
    Tag { label: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<Arm> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum BinOp { Add, Sub, Eq2, Lt }

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Arm {
    pub pat: Pat,
    pub expr: NodeId,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum Pat {
    PVar { bind: Bind },
    PWild,
    PRecord { fields: Vec<(String, Pat)>, rest: bool },
    PTag { label: String, payload: Option<Box<Pat>> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct BindId(pub u64);

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct Bind {
    pub id: BindId,
    pub span: Span,
    #[serde(skip)]
    pub name: String,
    pub recursive: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct AstNode {
    pub id: NodeId,
    pub span: Span,
    pub node: Node,
}

#[derive(Clone, Debug, Serialize)]
pub struct Ast {
    pub root: NodeId,
    pub nodes: Vec<AstNode>,
    pub source: String,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &AstNode {
        self.nodes.iter().find(|n| n.id == id).expect("missing node")
    }
}

// ---------- stable identity hashing (FNV-1a based) ----------

const OFF: u64 = 0xcbf29ce484222325;
const PRIME: u64 = 0x100000001b3;

#[derive(Default)]
pub struct Hasher { state: u64 }

impl Hasher {
    pub fn new() -> Self { Hasher { state: OFF } }
    pub fn u8(&mut self, b: u8) {
        self.state ^= b as u64;
        self.state = self.state.wrapping_mul(PRIME);
    }
    pub fn bytes(&mut self, s: &[u8]) { for b in s { self.u8(*b); } }
    pub fn tag(&mut self, t: &str) { self.u8(0); self.bytes(t.as_bytes()); }
    pub fn fin(self) -> u64 { self.state }
}

/// Content of a node used for its stable id. Variable names and binder names
/// are deliberately excluded: renaming must not shift identities.
pub enum Content<'a> {
    N(&'a Node),
    Pat(&'a Pat),
    Arm,
    Binder { rec: bool },
}

pub fn content_hash(c: Content) -> u64 {
    let mut h = Hasher::new();
    match c {
        Content::N(n) => match n {
            Node::Program { .. } => h.tag("Program"),
            Node::Let { rec, body, .. } => {
                h.tag("Let"); h.u8(if *rec { 1 } else { 0 });
                h.u8(if body.is_some() { 1 } else { 0 });
            }
            Node::Lam { .. } => h.tag("Lam"),
            Node::App { .. } => h.tag("App"),
            Node::Var { .. } => h.tag("Var"),
            Node::Int { value } => { h.tag("Int"); h.bytes(&value.to_le_bytes()); }
            Node::Bool { value } => { h.tag("Bool"); h.u8(if *value { 1 } else { 0 }); }
            Node::Str { value } => { h.tag("Str"); h.bytes(value.as_bytes()); }
            Node::Bin { op, .. } => { h.tag("Bin"); h.u8(match op {
                BinOp::Add => 0, BinOp::Sub => 1, BinOp::Eq2 => 2, BinOp::Lt => 3 });
            }
            Node::If { .. } => h.tag("If"),
            Node::Seq { items } => { h.tag("Seq"); h.bytes(&(items.len() as u64).to_le_bytes()); }
            Node::Record { fields, rest } => {
                h.tag("Record");
                for (f, _) in fields { h.u8(1); h.bytes(f.as_bytes()); }
                h.u8(if rest.is_some() { 2 } else { 3 });
            }
            Node::Proj { field, .. } => { h.tag("Proj"); h.bytes(field.as_bytes()); }
            Node::Tag { label, payload } => {
                h.tag("Tag"); h.bytes(label.as_bytes()); h.u8(if payload.is_some() { 1 } else { 0 });
            }
            Node::Match { arms, .. } => { h.tag("Match"); h.bytes(&(arms.len() as u64).to_le_bytes()); }
        },
        Content::Pat(p) => match p {
            Pat::PVar { .. } => h.tag("PVar"),
            Pat::PWild => h.tag("PWild"),
            Pat::PRecord { fields, rest } => {
                h.tag("PRecord");
                for (f, _) in fields { h.u8(1); h.bytes(f.as_bytes()); }
                h.u8(if *rest { 2 } else { 3 });
            }
            Pat::PTag { label, payload } => {
                h.tag("PTag"); h.bytes(label.as_bytes()); h.u8(if payload.is_some() { 1 } else { 0 });
            }
        },
        Content::Arm => h.tag("Arm"),
        Content::Binder { rec } => { h.tag("Bind"); h.u8(if rec { 1 } else { 0 }); }
    }
    h.fin()
}
