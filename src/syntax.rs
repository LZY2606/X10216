use serde::Serialize;
use std::collections::HashMap;

pub type NodeId = u32;
pub type ScopeId = u32;
pub type NameId = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn join(self, other: Span) -> Span {
        Span(self.0.min(other.0), self.1.max(other.1))
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "k", content = "d")]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Var(NameId),
    Lam { arg: NameId, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Bin { op: BinOp, lhs: NodeId, rhs: NodeId },
    If { cond: NodeId, then_b: NodeId, else_b: NodeId },
    Let {
        rec: bool,
        name: NameId,
        value: NodeId,
        body: NodeId,
    },
    Record(Vec<(NameId, NodeId)>),
    RecordExt {
        base: NodeId,
        fields: Vec<(NameId, NodeId)>,
    },
    Field { obj: NodeId, label: NameId },
    Variant { label: NameId, payload: NodeId },
    Match { scrut: NodeId, arms: Vec<(NodeId, NodeId)> },
    Unit,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "k", content = "d")]
pub enum Pat {
    PWild,
    PVar(NameId),
    PVariant { label: NameId, payload: Option<NodeId> },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub kind: NK,
    pub expr: Option<Expr>,
    pub pat: Option<Pat>,
    pub span: Span,
    pub scope: ScopeId,
    pub text: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum NK {
    Program,
    Expr,
    Pat,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct Ast {
    pub nodes: Vec<Option<Node>>,
    pub root: NodeId,
    pub interner: NameInterner,
    pub parse_errors: Vec<ParseError>,
    pub name_errors: Vec<NameError>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct NameError {
    pub span: Span,
    pub message: String,
    pub kind: String,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct NameInterner {
    map: HashMap<String, NameId>,
    names: Vec<String>,
}

impl NameInterner {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn intern(&mut self, s: &str) -> NameId {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        let id = self.names.len() as NameId;
        self.names.push(s.to_string());
        self.map.insert(s.to_string(), id);
        id
    }
    pub fn resolve(&self, id: NameId) -> &str {
        &self.names[id as usize]
    }
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Node {
        self.nodes[id as usize].as_ref().expect("live node")
    }

    pub fn node_opt(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(id as usize).and_then(|n| n.as_ref())
    }

    /// 与名称、字面量无关的结构判别签名，用于跨编辑复用节点身份。
    pub fn shape_tag(&self, id: NodeId) -> String {
        let n = self.node(id);
        match (&n.expr, &n.pat) {
            (Some(e), _) => match e {
                Expr::Int(_) => "e:int".into(),
                Expr::Bool(_) => "e:bool".into(),
                Expr::Str(_) => "e:str".into(),
                Expr::Var(_) => "e:var".into(),
                Expr::Lam { .. } => "e:lam".into(),
                Expr::App { .. } => "e:app".into(),
                Expr::Bin { op, .. } => format!("e:bin:{}", op.tag()),
                Expr::If { .. } => "e:if".into(),
                Expr::Let { rec, .. } => format!("e:let:{}", rec),
                Expr::Record(fs) => format!("e:rec:{}", fs.len()),
                Expr::RecordExt { fields, .. } => format!("e:recext:{}", fields.len()),
                Expr::Field { .. } => "e:field".into(),
                Expr::Variant { .. } => "e:variant".into(),
                Expr::Match { arms, .. } => format!("e:match:{}", arms.len()),
                Expr::Unit => "e:unit".into(),
            },
            (None, Some(p)) => match p {
                Pat::PWild => "p:wild".into(),
                Pat::PVar(_) => "p:var".into(),
                Pat::PVariant { payload, .. } => {
                    format!("p:variant:{}", payload.is_some())
                }
            },
            _ => "program".into(),
        }
    }
}

impl BinOp {
    pub fn tag(&self) -> &'static str {
        match self {
            BinOp::Add => "add",
            BinOp::Sub => "sub",
            BinOp::Mul => "mul",
            BinOp::Eq => "eq",
        }
    }
}

/// 子节点在 LCS 对齐时使用的结构标签（含字段标签与操作符，不含绑定名）。
pub fn child_tags(ast: &Ast, n: &Node) -> Vec<(NodeId, String)> {
    let mut out = Vec::new();
    let tag_of = |id: NodeId| ast.shape_tag(id);
    if let Some(e) = &n.expr {
        match e {
            Expr::Lam { body, .. } => out.push((*body, tag_of(*body))),
            Expr::App { fun, arg } => {
                out.push((*fun, tag_of(*fun)));
                out.push((*arg, tag_of(*arg)));
            }
            Expr::Bin { op, lhs, rhs } => {
                out.push((*lhs, format!("l:{}", op.tag())));
                out.push((*rhs, format!("r:{}", op.tag())));
            }
            Expr::If { cond, then_b, else_b } => {
                out.push((*cond, "if-c".into()));
                out.push((*then_b, "if-t".into()));
                out.push((*else_b, "if-e".into()));
            }
            Expr::Let { value, body, .. } => {
                out.push((*value, "let-v".into()));
                out.push((*body, "let-b".into()));
            }
            Expr::Record(fs) => {
                for (lab, fid) in fs {
                    out.push((*fid, format!("f:{}", ast.interner.resolve(*lab))));
                }
            }
            Expr::RecordExt { base, fields } => {
                out.push((*base, "ext-base".into()));
                for (lab, fid) in fields {
                    out.push((*fid, format!("f:{}", ast.interner.resolve(*lab))));
                }
            }
            Expr::Field { obj, label } => {
                out.push((*obj, format!("fd:{}", ast.interner.resolve(*label))));
            }
            Expr::Variant { label, payload } => {
                out.push((*payload, format!("v:{}", ast.interner.resolve(*label))));
            }
            Expr::Match { scrut, arms } => {
                out.push((*scrut, "m-scrut".into()));
                for (i, (p, b)) in arms.iter().enumerate() {
                    out.push((*p, format!("m{}-p", i)));
                    out.push((*b, format!("m{}-b", i)));
                }
            }
            _ => {}
        }
    }
    out
}
