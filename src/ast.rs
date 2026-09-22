//! Concrete syntax tree with stable, content-addressed node ids.

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug, Serialize, Deserialize)]
pub struct NodeId(pub u64);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
    pub fn merge(&self, other: &Span) -> Span {
        Span { start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    Str(String),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Pattern {
    Name(String),
    Tag(String, Option<String>, Span),
    Wild,
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub pat: Pattern,
    pub body: ExprNode,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Lit(Literal),
    Var(String),
    Fun { params: Vec<Param>, body: Box<ExprNode> },
    App { fun: Box<ExprNode>, arg: Box<ExprNode> },
    Let { rec: bool, name: String, value: Box<ExprNode>, body: Box<ExprNode> },
    If { cond: Box<ExprNode>, then_e: Box<ExprNode>, else_e: Box<ExprNode> },
    BinOp { op: String, left: Box<ExprNode>, right: Box<ExprNode>, op_span: Span },
    Record { fields: Vec<(String, ExprNode)>, open: bool },
    Field { object: Box<ExprNode>, label: String },
    Tag { label: String, payload: Option<Box<ExprNode>> },
    Match { scrut: Box<ExprNode>, arms: Vec<Arm> },
}

#[derive(Clone, Debug)]
pub struct ExprNode {
    pub id: NodeId,
    pub expr: Expr,
    pub span: Span,
    pub local_name: Option<String>,
}

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub root: Option<ExprNode>,
    pub source: String,
    pub issues: Vec<ParseIssue>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseIssue {
    pub message: String,
    pub span: Span,
}

impl Serialize for Span {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        (self.start, self.end).serialize(s)
    }
}

// ---------- stable identity hashing (FNV-1a 64) ----------

pub fn hash_str(seed: u64, s: &str) -> u64 {
    let mut h = seed;
    for b in s.bytes() {
        h ^= b as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

pub fn hash_u64(mut h: u64, mut x: u64) -> u64 {
    for _ in 0..8 {
        h ^= x & 0xff;
        h = h.wrapping_mul(0x100000001b3);
        x >>= 8;
    }
    h
}

fn hash_i64(h: u64, x: i64) -> u64 {
    hash_u64(h, x as u64)
}

/// Stable identity for a node, independent of descendant node ids. The path
/// distinguishes repeated structurally-identical nodes (record fields,
/// repeated literals, parameters, arms).
fn node_fingerprint(expr: &Expr, span: Span, path: &str) -> u64 {
    let h = hash_str(0xcbf29ce484222325, path);
    let h = hash_u64(h, span.start as u64);
    let h = hash_u64(h, span.end as u64);
    match expr {
        Expr::Lit(l) => match l {
            Literal::Int(i) => hash_i64(hash_str(h, "lit:i"), *i),
            Literal::Bool(b) => hash_u64(hash_str(h, "lit:b"), *b as u64),
            Literal::Str(s) => hash_str(hash_str(h, "lit:s"), s),
        },
        Expr::Var(name) => hash_str(hash_str(h, "var"), name),
        Expr::Fun { params, .. } => {
            let mut hh = hash_str(h, "fun");
            hh = hash_u64(hh, params.len() as u64);
            for p in params {
                hh = hash_str(hh, &p.name);
            }
            hh
        }
        Expr::App { .. } => hash_str(h, "app"),
        Expr::Let { rec, name, .. } => {
            hash_u64(hash_str(hash_str(h, "let"), name), *rec as u64)
        }
        Expr::If { .. } => hash_str(h, "if"),
        Expr::BinOp { op, .. } => hash_str(hash_str(h, "bin:"), op),
        Expr::Record { fields, open } => {
            let mut hh = hash_u64(hash_str(h, "record"), *open as u64);
            for (l, _) in fields {
                hh = hash_str(hh, l);
            }
            hh
        }
        Expr::Field { label, .. } => hash_str(hash_str(h, "field"), label),
        Expr::Tag { label, .. } => hash_str(hash_str(h, "tag"), label),
        Expr::Match { arms, .. } => hash_u64(hash_str(h, "match"), arms.len() as u64),
    }
}

struct Assign {
    used: std::collections::HashSet<u64>,
}

impl Assign {
    fn new() -> Self {
        Assign { used: std::collections::HashSet::new() }
    }

    fn allocate(&mut self, fp: u64) -> NodeId {
        let mut candidate = fp;
        let mut j = 1u64;
        while self.used.contains(&candidate) {
            j += 1;
            candidate = hash_u64(fp ^ 0x9e3779b97f4a7c15, j);
        }
        self.used.insert(candidate);
        NodeId(candidate)
    }

    fn walk(&mut self, node: &mut ExprNode, path: &str) {
        match &mut node.expr {
            Expr::Lit(_) | Expr::Var(_) | Expr::Tag { payload: None, .. } => {}
            Expr::Fun { params: _, body } => {
                self.walk(body, &format!("{path}/body"));
            }
            Expr::App { fun, arg } => {
                self.walk(fun, &format!("{path}/fun"));
                self.walk(arg, &format!("{path}/arg"));
            }
            Expr::Let { value, body, .. } => {
                self.walk(value, &format!("{path}/val"));
                self.walk(body, &format!("{path}/let"));
            }
            Expr::If { cond, then_e, else_e } => {
                self.walk(cond, &format!("{path}/c"));
                self.walk(then_e, &format!("{path}/t"));
                self.walk(else_e, &format!("{path}/e"));
            }
            Expr::BinOp { left, right, .. } => {
                self.walk(left, &format!("{path}/l"));
                self.walk(right, &format!("{path}/r"));
            }
            Expr::Record { fields, .. } => {
                for (i, (_, e)) in fields.iter_mut().enumerate() {
                    self.walk(e, &format!("{path}/f{i}"));
                }
            }
            Expr::Field { object, .. } => {
                self.walk(object, &format!("{path}/obj"));
            }
            Expr::Tag { payload: Some(p), .. } => {
                self.walk(p, &format!("{path}/p"));
            }
            Expr::Match { scrut, arms } => {
                self.walk(scrut, &format!("{path}/s"));
                for (i, arm) in arms.iter_mut().enumerate() {
                    self.walk(&mut arm.body, &format!("{path}/a{i}"));
                }
            }
        }
        let fp = node_fingerprint(&node.expr, node.span, path);
        node.id = self.allocate(fp);
    }
}

/// Assign stable ids to every node, bottom-up.
pub fn assign_ids(program: &mut Program) {
    let mut assign = Assign::new();
    if let Some(root) = program.root.as_mut() {
        assign.walk(root, "root");
    }
}

/// Collect every node (depth-first, pre-order).
pub fn collect_nodes(root: &ExprNode) -> Vec<&ExprNode> {
    let mut out = Vec::new();
    fn go<'a>(n: &'a ExprNode, out: &mut Vec<&'a ExprNode>) {
        out.push(n);
        match &n.expr {
            Expr::Lit(_) | Expr::Var(_) => {}
            Expr::Fun { body, .. } => go(body, out),
            Expr::App { fun, arg } => {
                go(fun, out);
                go(arg, out);
            }
            Expr::Let { value, body, .. } => {
                go(value, out);
                go(body, out);
            }
            Expr::If { cond, then_e, else_e } => {
                go(cond, out);
                go(then_e, out);
                go(else_e, out);
            }
            Expr::BinOp { left, right, .. } => {
                go(left, out);
                go(right, out);
            }
            Expr::Record { fields, .. } => {
                for (_, e) in fields {
                    go(e, out);
                }
            }
            Expr::Field { object, .. } => go(object, out),
            Expr::Tag { payload, .. } => {
                if let Some(p) = payload {
                    go(p, out)
                }
            }
            Expr::Match { scrut, arms } => {
                go(scrut, out);
                for a in arms {
                    go(&a.body, out);
                }
            }
        }
    }
    go(root, &mut out);
    out
}
