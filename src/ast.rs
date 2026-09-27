#[derive(Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn len(&self) -> u32 {
        self.1.saturating_sub(self.0)
    }
}

pub type NodeId = u32;

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Gt,
    And,
    Or,
}

impl BinOp {
    pub fn symbol(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::And => "&&",
            BinOp::Or => "||",
        }
    }
}

#[derive(Clone, serde::Serialize)]
#[serde(tag = "ty")]
pub enum ATy {
    Con { name: String },
    TVar { name: String },
    Arrow { from: Box<ATy>, to: Box<ATy> },
    Rec { fields: Vec<(String, ATy)>, tail: Option<String> },
    Variant { arms: Vec<(String, Option<ATy>)>, tail: Option<String> },
}

impl ATy {
    pub fn display(&self) -> String {
        match self {
            ATy::Con { name } => name.clone(),
            ATy::TVar { name } => name.clone(),
            ATy::Arrow { from, to } => {
                let l = match from.as_ref() {
                    ATy::Arrow { .. } => format!("({})", from.display()),
                    _ => from.display(),
                };
                format!("{l} -> {}", to.display())
            }
            ATy::Rec { fields, tail } => {
                let mut s = String::from("{");
                for (i, (n, t)) in fields.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(n);
                    s.push_str(": ");
                    s.push_str(&t.display());
                }
                if let Some(r) = tail {
                    if !fields.is_empty() {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{r}:?"));
                }
                s.push('}');
                s
            }
            ATy::Variant { arms, tail } => {
                let mut s = String::from("[");
                for (i, (n, t)) in arms.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(n);
                    if let Some(t) = t {
                        s.push(' ');
                        s.push_str(&t.display());
                    }
                }
                if let Some(r) = tail {
                    if !arms.is_empty() {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("{r}:?"));
                }
                s.push(']');
                s
            }
        }
    }
}

#[derive(Clone, serde::Serialize)]
#[serde(tag = "kind")]
pub enum Expr {
    Int { val: i64 },
    Bool { val: bool },
    Str { val: String },
    Var { name: String, db: Option<u32>, builtin: bool },
    Lam { var: String, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Bin { op: BinOp, left: NodeId, right: NodeId },
    Let {
        rec: bool,
        name: String,
        annot: Option<ATy>,
        value: NodeId,
        body: NodeId,
    },
    Record { fields: Vec<(String, NodeId)>, ext: Option<NodeId> },
    Field { obj: NodeId, name: String },
    VariantT { tag: String, payload: Option<NodeId> },
    Case { scrut: NodeId, arms: Vec<CaseArm>, flex: Option<FlexArm> },
    Annot { expr: NodeId, ty: ATy },
    Seq { exprs: Vec<NodeId> },
}

#[derive(Clone, serde::Serialize)]
pub struct CaseArm {
    pub tag: String,
    pub binder: Option<String>,
    pub body: NodeId,
    pub pat_span: Span,
}

#[derive(Clone, serde::Serialize)]
pub struct FlexArm {
    pub var: String,
    pub body: NodeId,
}

#[derive(Clone, serde::Serialize)]
pub struct Node {
    pub id: NodeId,
    pub span: Span,
    pub parent: Option<NodeId>,
    pub key: u64,
    pub expr: Expr,
}

#[derive(Clone, serde::Serialize)]
pub struct Program {
    pub items: Vec<NodeId>,
}

#[derive(Clone, serde::Serialize)]
pub struct Ast {
    pub source: String,
    pub nodes: Vec<Node>,
    pub program: Program,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id as usize]
    }
}
