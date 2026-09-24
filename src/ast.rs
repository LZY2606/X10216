//! Flat AST: every node gets a stable integer id assigned in source order.
//! Identity is structural, so renaming a bound variable does not shift ids of
//! any other node.

use crate::types::{RcStr, Span};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NodeId(pub u32);

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Str(RcStr),
    /// Variable occurrence; resolved during scope resolution.
    Var(RcStr),
    /// Resolved variable (binder node id), filled in after parsing.
    ResolvedVar { name: RcStr, binder: Option<NodeId> },
    Lam { param: NodeId, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Let { pat: NodeId, value: NodeId, body: NodeId, rec_kw: bool },
    Record(Vec<(RcStr, NodeId)>),
    RecordLit(Vec<(RcStr, NodeId)>),
    Field { record: NodeId, name: RcStr },
    Extend { base: Option<NodeId>, fields: Vec<(RcStr, NodeId)> },
    Variant { label: RcStr, payload: NodeId },
    Match { scrut: NodeId, arms: Vec<(NodeId, NodeId)> },
    If { cond: NodeId, then_b: NodeId, else_b: NodeId },
    Seq(Vec<NodeId>),
    Unit,
}

#[derive(Clone, Debug)]
pub enum Pat {
    PVar(RcStr),
    PWild,
    PRecord(Vec<(RcStr, NodeId)>),
    PVariant { label: RcStr, payload: NodeId },
}

#[derive(Clone, Debug)]
pub enum NodeData {
    E(Expr),
    P(Pat),
    /// Binder node (lambda parameter / let target / pattern var).
    Bind { name: RcStr },
}

#[derive(Clone, Debug)]
pub struct AstNode {
    pub data: NodeData,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
    pub root: NodeId,
}

impl Ast {
    pub fn new() -> Self {
        Ast { nodes: Vec::new(), root: NodeId(0) }
    }

    pub fn add(&mut self, data: NodeData, span: Span) -> NodeId {
        let id = NodeId(self.nodes.len() as u32);
        self.nodes.push(AstNode { data, span });
        id
    }

    pub fn bind(&mut self, name: &str, span: Span) -> NodeId {
        self.add(NodeData::Bind { name: Rc::from(name) }, span)
    }

    pub fn span_of(&self, id: NodeId) -> Span {
        self.nodes[id.0 as usize].span
    }
}

/// Walk every node reachable from a root in a stable order, invoking `f` with
/// the node id. Binders are visited before uses.
pub fn walk(ast: &Ast, root: NodeId, f: &mut impl FnMut(NodeId)) {
    f(root);
    match &ast.nodes[root.0 as usize].data {
        NodeData::P(_) => {}
        NodeData::Bind { .. } => {}
        NodeData::E(e) => match e {
            Expr::Int(_) | Expr::Bool(_) | Expr::Str(_) | Expr::Var(_)
            | Expr::ResolvedVar { .. } | Expr::Unit => {}
            Expr::Lam { param, body } => {
                walk(ast, *param, f);
                walk(ast, *body, f);
            }
            Expr::App { fun, arg } => { walk(ast, *fun, f); walk(ast, *arg, f); }
            Expr::Let { pat, value, body, .. } => {
                walk(ast, *pat, f);
                walk(ast, *value, f);
                walk(ast, *body, f);
            }
            Expr::Record(xs) | Expr::RecordLit(xs) =>
                for (_, n) in xs { walk(ast, *n, f); }
            Expr::Field { record, .. } => walk(ast, *record, f),
            Expr::Extend { base, fields } => {
                if let Some(b) = base { walk(ast, *b, f); }
                for (_, n) in fields { walk(ast, *n, f); }
            }
            Expr::Variant { payload, .. } => walk(ast, *payload, f),
            Expr::Match { scrut, arms } => {
                walk(ast, *scrut, f);
                for (p, b) in arms { walk(ast, *p, f); walk(ast, *b, f); }
            }
            Expr::If { cond, then_b, else_b } => {
                walk(ast, *cond, f); walk(ast, *then_b, f); walk(ast, *else_b, f);
            }
            Expr::Seq(xs) => for n in xs { walk(ast, *n, f); },
        },
    }
}
