use crate::keys::{BinderKey, Chain, NodeKey};
use crate::syntax::{Ast, BinOp, ExprKind, Node, NodeId, Span};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct ResolveError {
    pub message: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct VarRef {
    pub depth: usize,
    pub binder: BinderKey,
}

#[derive(Clone)]
pub struct RNode {
    pub id: NodeId,
    pub kind: RExpr,
    pub span: Span,
    pub key: NodeKey,
}

#[derive(Clone)]
pub enum RExpr {
    Err,
    Unit,
    Int(i64),
    Bool(bool),
    Str(String),
    Var { name: String, r: VarRef },
    Unresolved(String),
    Fun { param: BinderKey, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Bin { op: &'static str, lhs: NodeId, rhs: NodeId },
    Let { rec: bool, name: BinderKey, value_form: bool, bound: NodeId, body: NodeId },
    If { cond: NodeId, then_e: NodeId, else_e: NodeId },
    Record { fields: Vec<(String, NodeId)>, ext: Option<NodeId> },
    Field { obj: NodeId, label: String },
    Tag { label: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<RArm> },
}

#[derive(Clone)]
pub struct RArm {
    pub span: Span,
    pub label: String,
    pub binder: Option<BinderKey>,
    pub body: NodeId,
}

pub struct ResolvedAst {
    pub nodes: Vec<Rc<RNode>>,
    pub root: NodeId,
}

impl ResolvedAst {
    pub fn node(&self, id: NodeId) -> &Rc<RNode> { &self.nodes[id] }
}

struct Frame {
    name: String,
    binder: BinderKey,
}

// Structural description used to compute rename-insensitive keys before output ids.
#[derive(Clone)]
enum D {
    Err, Unit, Int(i64), Bool(bool), Str(String),
    Var(String),
    Fun(String, Box<D>),
    App(Box<D>, Box<D>),
    Bin(&'static str, Box<D>, Box<D>),
    Let(bool, String, Box<D>, Box<D>),
    If(Box<D>, Box<D>, Box<D>),
    Record(Vec<(String, D)>, Option<Box<D>>),
    Field(Box<D>, String),
    Tag(String, Option<Box<D>>),
    Match(Box<D>, Vec<DA>),
}
struct DA { label: String, binder: Option<String>, span: Span, body: D }

struct Resolver<'a> {
    ast: &'a Ast,
    out: Vec<Rc<RNode>>,
    scope: Vec<Frame>,
    errors: Vec<ResolveError>,
}

pub fn is_value(kind: &RExpr) -> bool {
    matches!(kind,
        RExpr::Fun { .. } | RExpr::Int(_) | RExpr::Bool(_) | RExpr::Str(_)
        | RExpr::Tag { payload, .. } if payload.is_none())
}

pub fn resolve(ast: &Ast) -> (ResolvedAst, Vec<ResolveError>) {
    let mut r = Resolver { ast, out: Vec::with_capacity(ast.nodes.len()), scope: Vec::new(), errors: Vec::new() };
    let root = r.gen(ast.root, &NodeKey("root".into()));
    (ResolvedAst { nodes: r.out, root }, r.errors)
}

fn h(parent: &NodeKey, role: &str, occ: u64, more: &[u8]) -> NodeKey {
    let mut ch = Chain::new();
    ch.tag("p"); ch.str(&parent.0);
    ch.tag("role"); ch.str(role); ch.u64(occ);
    for b in more { ch.byte(*b); }
    NodeKey(ch.key())
}

fn op_name(op: BinOp) -> &'static str {
    match op { BinOp::Add => "+", BinOp::Sub => "-", BinOp::Mul => "*", BinOp::Eq => "==" }
}

// Build structural descriptor and its NodeKey simultaneously (rename-insensitive).
impl<'a> Resolver<'a> {
    fn node(&self, id: NodeId) -> &Rc<Node> { self.ast.node(id) }

    fn lookup(&self, name: &str) -> Option<VarRef> {
        for (idx, f) in self.scope.iter().enumerate().rev() {
            if f.name == name {
                return Some(VarRef { depth: self.scope.len() - 1 - idx, binder: f.binder.clone() });
            }
        }
        None
    }

    fn scoped<F>(&mut self, name: String, binder: BinderKey, f: F) -> NodeId
    where F: FnOnce(&mut Resolver<'a>) -> NodeId {
        self.scope.push(Frame { name, binder });
        let id = f(self);
        self.scope.pop();
        id
    }

    fn content(node: &Node) -> Vec<u8> {
        let mut v = Vec::new();
        let push = |v: &mut Vec<u8>, t: &[u8]| { v.extend_from_slice(t); v.push(0); };
        match &node.kind {
            ExprKind::Err => push(&mut v, b"err"),
            ExprKind::Unit => push(&mut v, b"unit"),
            ExprKind::Int(n) => { push(&mut v, b"int"); v.extend_from_slice(n.to_string().as_bytes()); }
            ExprKind::Bool(b) => push(&mut v, if *b { b"bool1" } else { b"bool0" }),
            ExprKind::Str(s) => { push(&mut v, b"str"); v.extend_from_slice(s.as_bytes()); }
            ExprKind::Var(n) => { push(&mut v, b"var"); v.extend_from_slice(n.as_bytes()); }
            ExprKind::Fun { .. } => push(&mut v, b"fun"),
            ExprKind::App { .. } => push(&mut v, b"app"),
            ExprKind::Bin { op, .. } => push(&mut v, match op {
                BinOp::Add => b"bin+", BinOp::Sub => b"bin-", BinOp::Mul => b"bin*", BinOp::Eq => b"bin==" }),
            ExprKind::Let { rec, .. } => push(&mut v, if *rec { b"letrec" } else { b"let" }),
            ExprKind::If { .. } => push(&mut v, b"if"),
            ExprKind::Record { .. } => push(&mut v, b"record"),
            ExprKind::Field { label, .. } => { push(&mut v, b"field"); v.extend_from_slice(label.as_bytes()); }
            ExprKind::Tag { label, .. } => { push(&mut v, b"tag"); v.extend_from_slice(label.as_bytes()); }
            ExprKind::Match { .. } => push(&mut v, b"match"),
        }
        v
    }

    fn gen(&mut self, id: NodeId, key: &NodeKey) -> NodeId {
        let node = self.ast.node(id).clone();
        let mut ch = Chain::new();
        ch.str(&key.0);
        ch.tag("content");
        for b in Self::content(&node) { ch.byte(b); }
        let key = NodeKey(ch.key());
        let rid = self.out.len();
        self.out.push(Rc::new(RNode { id: rid, kind: RExpr::Err, span: node.span, key: key.clone() }));
        let kind = self.build(&node, rid, &key);
        self.out[rid] = Rc::new(RNode { id: rid, kind, span: node.span, key: key.clone() });
        rid
    }

    fn key_of(&self, id: NodeId) -> NodeKey { self.out[id].key.clone() }

    fn binder_for(&self, parent: &NodeKey, role: &str, occ: u64) -> BinderKey {
        BinderKey(h(parent, role, occ, b"binder").0)
    }

    fn build(&mut self, node: &Rc<Node>, rid: NodeId, key: &NodeKey) -> RExpr {
        match &node.kind {
            ExprKind::Err => RExpr::Err,
            ExprKind::Unit => RExpr::Unit,
            ExprKind::Int(n) => RExpr::Int(*n),
            ExprKind::Bool(b) => RExpr::Bool(*b),
            ExprKind::Str(s) => RExpr::Str(s.clone()),
            ExprKind::Var(name) => match self.lookup(name) {
                Some(r) => RExpr::Var { name: name.clone(), r },
                None => {
                    self.errors.push(ResolveError { message: format!("未绑定的变量 `{}`", name), span: node.span });
                    RExpr::Unresolved(name.clone())
                }
            },
            ExprKind::Fun { param, body } => {
                let binder = self.binder_for(key, "param", 0);
                let body_key = h(key, "body", 0, b"");
                let body = self.scoped(param.clone(), binder.clone(), |r| r.gen(*body, &body_key));
                RExpr::Fun { param: binder, body }
            }
            ExprKind::App { fun, arg } => {
                let fk = h(key, "fun", 0, b""); let ak = h(key, "arg", 0, b"");
                let f = self.gen(*fun, &fk);
                let a = self.gen(*arg, &ak);
                RExpr::App { fun: f, arg: a }
            }
            ExprKind::Bin { op, lhs, rhs } => {
                let n = op_name(*op);
                let l = self.gen(*lhs, &h(key, "l", 0, n.as_bytes()));
                let r2 = self.gen(*rhs, &h(key, "r", 0, n.as_bytes()));
                RExpr::Bin { op: n, lhs: l, rhs: r2 }
            }
            ExprKind::Let { rec, name, bound, body } => {
                let binder = self.binder_for(key, if *rec { "recname" } else { "name" }, 0);
                let bk = h(key, "bound", 0, if *rec { b"rec" } else { b"" });
                let bound = if *rec {
                    self.scoped(name.clone(), binder.clone(), |r| r.gen(*bound, &bk))
                } else {
                    self.gen(*bound, &bk)
                };
                let value_form = is_value(&self.out[bound].kind);
                let body_key = h(key, "body", 0,
                    if value_form { b"gen" } else { b"nogen" });
                let body = self.scoped(name.clone(), binder.clone(), |r| r.gen(*body, &body_key));
                RExpr::Let { rec: *rec, name: binder, value_form, bound, body }
            }
            ExprKind::If { cond, then_e, else_e } => {
                let c = self.gen(*cond, &h(key, "c", 0, b""));
                let t = self.gen(*then_e, &h(key, "t", 0, b""));
                let e = self.gen(*else_e, &h(key, "e", 0, b""));
                RExpr::If { cond: c, then_e: t, else_e: e }
            }
            ExprKind::Record { fields, ext } => {
                let mut fs = Vec::new();
                for (i, (l, v)) in fields.iter().enumerate() {
                    let cid = self.gen(*v, &h(key, "field", i as u64, l.as_bytes()));
                    fs.push((l.clone(), cid));
                }
                let ext = if let Some(e) = ext {
                    Some(self.gen(*e, &h(key, "ext", 0, b"")))
                } else { None };
                RExpr::Record { fields: fs, ext }
            }
            ExprKind::Field { obj, label } => {
                let o = self.gen(*obj, &h(key, "obj", 0, label.as_bytes()));
                RExpr::Field { obj: o, label: label.clone() }
            }
            ExprKind::Tag { label, payload } => {
                let p = if let Some(p) = payload {
                    Some(self.gen(*p, &h(key, "payload", 0, label.as_bytes())))
                } else { None };
                RExpr::Tag { label: label.clone(), payload: p }
            }
            ExprKind::Match { scrut, arms } => {
                let s = self.gen(*scrut, &h(key, "scrut", 0, b""));
                let mut rarms = Vec::new();
                for (i, arm) in arms.iter().enumerate() {
                    let (binder_opt, body) = if arm.label == "_" {
                        let b = self.gen(arm.body, &h(key, "wild", i as u64, b""));
                        (None, b)
                    } else {
                        match &arm.binder {
                            None => {
                                let b = self.gen(arm.body, &h(key, "arm", i as u64, arm.label.as_bytes()));
                                (None, b)
                            }
                            Some(name) => {
                                let bk = self.binder_for(key, "armbinder", i as u64);
                                let bkey = h(key, "armbody", i as u64, arm.label.as_bytes());
                                let b = self.scoped(name.clone(), bk.clone(), |r| r.gen(arm.body, &bkey));
                                (Some(bk), b)
                            }
                        }
                    };
                    rarms.push(RArm { span: arm.span, label: arm.label.clone(), binder: binder_opt, body });
                }
                RExpr::Match { scrut: s, arms: rarms }
            }
        }
    }
}
