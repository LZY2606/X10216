//! Lexer + parser producing an AST whose node ids are assigned in post-order,
//! which keeps structural diffing (see `incremental.rs`) straightforward.

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct NodeId(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
    pub fn merge(self, other: Span) -> Span {
        Span { lo: self.lo.min(other.lo), hi: self.hi.max(other.hi) }
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub label: String,
    pub value: NodeId,
    pub label_span: Span,
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub label: String,
    pub label_span: Span,
    pub binding: Option<(String, Span)>,
    pub body: NodeId,
}

#[derive(Clone, Debug)]
pub enum NodeKind {
    Int(i64),
    Bool(bool),
    Unit,
    Var(String),
    Lam { param: String, param_span: Span, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Binop { op: String, op_span: Span, left: NodeId, right: NodeId },
    Let {
        name: String,
        name_span: Span,
        rec_kw: Option<Span>,
        value_kw: Option<Span>,
        bound: NodeId,
        body: NodeId,
    },
    If { cond: NodeId, then_br: NodeId, else_br: NodeId },
    Record(Vec<Field>),
    FieldGet { record: NodeId, label: String, label_span: Span },
    Variant { label: String, label_span: Span, payload: Option<NodeId> },
    Case { scrut: NodeId, arms: Vec<Arm> },
}

impl NodeKind {
    pub fn children(&self) -> Vec<NodeId> {
        match self {
            NodeKind::Int(_) | NodeKind::Bool(_) | NodeKind::Unit | NodeKind::Var(_) => vec![],
            NodeKind::Lam { body, .. } => vec![*body],
            NodeKind::App { fun, arg } => vec![*fun, *arg],
            NodeKind::Binop { left, right, .. } => vec![*left, *right],
            NodeKind::Let { bound, body, .. } => vec![*bound, *body],
            NodeKind::If { cond, then_br, else_br } => vec![*cond, *then_br, *else_br],
            NodeKind::Record(fs) => fs.iter().map(|f| f.value).collect(),
            NodeKind::FieldGet { record, .. } => vec![*record],
            NodeKind::Variant { payload, .. } => payload.iter().copied().collect(),
            NodeKind::Case { scrut, arms } => {
                let mut v = vec![*scrut];
                v.extend(arms.iter().map(|a| a.body));
                v
            }
        }
    }

    /// A stable discriminant so structural matching never pairs, say, a lambda
    /// with an application.
    pub fn tag(&self) -> &'static str {
        match self {
            NodeKind::Int(_) => "Int",
            NodeKind::Bool(_) => "Bool",
            NodeKind::Unit => "Unit",
            NodeKind::Var(_) => "Var",
            NodeKind::Lam { .. } => "Lam",
            NodeKind::App { .. } => "App",
            NodeKind::Binop { .. } => "Binop",
            NodeKind::Let { .. } => "Let",
            NodeKind::If { .. } => "If",
            NodeKind::Record(_) => "Record",
            NodeKind::FieldGet { .. } => "FieldGet",
            NodeKind::Variant { .. } => "Variant",
            NodeKind::Case { .. } => "Case",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub nodes: Vec<Node>,
    pub root: NodeId,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0 as usize]
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Option<Span>,
    pub message: String,
}
