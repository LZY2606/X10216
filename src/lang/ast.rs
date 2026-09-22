use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize)]
pub struct NodeId(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: u32, hi: u32) -> Self {
        Span { lo, hi }
    }
    pub fn union(a: Span, b: Span) -> Span {
        Span { lo: a.lo.min(b.lo), hi: a.hi.max(b.hi) }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum ArmPat {
    Tag { tag: String, param: Option<String> },
    Wild,
}

#[derive(Clone, Debug, Serialize)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Unit,
    Var(String),
    Lam {
        param: String,
        param_span: Span,
        body: NodeId,
    },
    App {
        fun: NodeId,
        arg: NodeId,
    },
    Bin {
        op: String,
        lhs: NodeId,
        rhs: NodeId,
    },
    Let {
        rec: bool,
        name: String,
        name_span: Span,
        init: NodeId,
        body: NodeId,
    },
    Record(Vec<Field>),
    Field {
        obj: NodeId,
        name: String,
    },
    Variant {
        tag: String,
        payload: Option<NodeId>,
    },
    Match {
        scrut: NodeId,
        arms: Vec<Arm>,
    },
    Ite {
        cond: NodeId,
        then_b: NodeId,
        else_b: NodeId,
    },
}

#[derive(Clone, Debug, Serialize)]
pub struct Field {
    pub name: String,
    pub value: NodeId,
}

#[derive(Clone, Debug, Serialize)]
pub struct Arm {
    pub pat: ArmPat,
    pub body: NodeId,
}

#[derive(Clone, Debug, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub expr: Expr,
    pub span: Span,
    /// Structural, positional identity — stable under binder renaming.
    pub anchor: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseError {
    pub message: String,
    pub lo: u32,
    pub hi: u32,
}
