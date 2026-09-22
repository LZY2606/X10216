// Resolved arena. Raw parsed expressions are flattened into version-local
// indices (`NId`). Each node also carries a content/scope based identity
// (`ident`) that survives unrelated edits and even variable renaming.
//
// Identity scheme
// ---------------
// Every node's identity is a structural path rooted at the program:
//   root#hash(shape)
//   <parent-ident>::<slot>#hash(shape)
// `slot` is the position under the parent (`fun`, `arg`, `field:<label>`,
// `arm[2].body`, ...). `shape` is the node's *own* discriminator plus its
// literals, labels and the binder it references — never source positions and
// never a user chosen variable name. Binders have identity
// `<owner-ident>::<binder-slot>`. Renaming a variable keeps every binder and
// reference identity stable; moving/deleting code only changes the subtree at
// that slot.

use crate::syntax::{BinOp, Clause, Expr, FieldInit, Pattern, Span, Spanned};
use std::collections::HashMap;

pub type NId = u32;

#[derive(Clone, Debug)]
pub enum NodeKind {
    Int(i64),
    Bool(bool),
    Str(String),
    Unit,
    Var { ref_ord: u32 },
    Lam { param: NId, body: NId },
    App { fun: NId, arg: NId },
    Bin { op: BinOp, l: NId, r: NId },
    Neg(NId),
    Not(NId),
    Let { rec: bool, name: NId, value: NId, body: NId },
    If { cond: NId, then_e: NId, else_e: NId },
    Record(Vec<(String, NId)>),
    Extend { fields: Vec<(String, NId)>, base: NId },
    Select { obj: NId, label: String },
    Variant { tag: String, payload: Option<NId> },
    Match { scrut: NId, arms: Vec<MatchArm>, has_default: bool },
    Binder { name: String },
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pat: Pat,
    pub body: NId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Pat {
    Wild,
    Binder(NId),
    Tag { tag: String, payload: Option<NId> },
}

#[derive(Clone, Debug)]
pub struct RNode {
    pub id: NId,
    pub ident: String,
    pub kind: NodeKind,
    pub span: Span,
    pub text: String,
    // Which binder a Var resolves to (None => builtin or unbound).
    pub refers: Option<NId>,
    pub builtin: Option<String>,
}

#[derive(Clone, Debug)]
pub struct BinderInfo {
    pub node: NId,
    pub name: String,
    pub ident: String,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct Arena {
    pub nodes: Vec<RNode>,
    pub binders: Vec<BinderInfo>,
    pub root: NId,
}

impl Arena {
    pub fn node(&self, id: NId) -> &RNode {
        &self.nodes[id as usize]
    }
}

pub fn short_hash(s: &str) -> String {
    let mut h: u64 = 0xcbf29ce484222325;
    for b in s.bytes() {
        h ^= b as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    format!("{:x}", h)
}

struct Entry {
    name: String,
    binder: NId,
}
struct Frame {
    entries: Vec<Entry>,
}

struct R {
    arena: Arena,
    frames: Vec<Frame>,
    builtins: std::collections::HashSet<String>,
    ref_counts: HashMap<NId, u32>,
}

pub fn resolve(src: &str, expr: &Spanned<Expr>, builtins: &[String]) -> Arena {
    let mut r = R {
        arena: Arena::default(),
        frames: vec![Frame { entries: Vec::new() }],
        builtins: builtins.iter().cloned().collect(),
        ref_counts: HashMap::new(),
    };
    let _ = src;
    let shape = self_shape(expr);
    let root_path = format!("root#{}", short_hash(&shape));
    let root = r.lower(expr, &root_path, "root");
    r.arena.root = root;
    r.arena
}

// Local shape of a raw expression, excluding children and variable names.
fn self_shape(e: &Spanned<Expr>) -> String {
    match &e.node {
        Expr::Int(n) => format!("Int({})", n),
        Expr::Bool(b) => format!("Bool({})", b),
        Expr::Str(_) => "Str".into(),
        Expr::Unit => "()".into(),
        Expr::Var(_) => "Var".into(),
        Expr::Bin { op, .. } => format!("Bin({:?})", op),
        Expr::Neg(_) => "Neg".into(),
        Expr::Not(_) => "Not".into(),
        Expr::Lam { args, .. } => format!("Lam({})", args.len()),
        Expr::App { .. } => "App".into(),
        Expr::Let { rec, .. } => format!("Let(rec={})", rec),
        Expr::If { .. } => "If".into(),
        Expr::Record(fields) => {
            let mut names: Vec<String> = fields.iter().map(field_name).collect();
            names.sort();
            format!("Rec{{{}}}", names.join(","))
        }
        Expr::Extend { fields, .. } => {
            let mut names: Vec<String> = fields.iter().map(field_name).collect();
            names.sort();
            format!("Ext{{{}}}", names.join(","))
        }
        Expr::Select { label, .. } => format!("Sel(.{})", label),
        Expr::Variant { tag, payload } => {
            format!("Tag({},{})", tag, if payload.is_some() { 1 } else { 0 })
        }
        Expr::Match { clauses, has_default, .. } => {
            format!("Match(arms={},def={})", clauses.len(), has_default)
        }
    }
}

fn field_name(f: &FieldInit) -> String {
    match f {
        FieldInit::Punct { name, .. } => name.clone(),
        FieldInit::Shorthand { name, .. } => name.clone(),
        FieldInit::Spread { .. } => "..".into(),
    }
}

impl R {
    fn child_path(&self, parent_path: &str, slot: &str, child: &Spanned<Expr>) -> String {
        format!("{}::{}#{}", parent_path, slot, short_hash(&self_shape(child)))
    }

    fn new_node(&mut self, kind: NodeKind, path: String, span: Span, text: String) -> NId {
        let id = self.arena.nodes.len() as NId;
        self.arena.nodes.push(RNode {
            id,
            ident: path,
            kind,
            span,
            text,
            refers: None,
            builtin: None,
        });
        id
    }

    fn make_binder(&mut self, name: &str, owner_path: &str, bslot: &str, span: Span) -> NId {
        let path = format!("{}::{}", owner_path, bslot);
        let id = self.arena.nodes.len() as NId;
        self.arena.nodes.push(RNode {
            id,
            ident: path.clone(),
            kind: NodeKind::Binder { name: name.to_string() },
            span,
            text: name.to_string(),
            refers: None,
            builtin: None,
        });
        self.frames.last_mut().unwrap().entries.push(Entry { name: name.to_string(), binder: id });
        self.arena.binders.push(BinderInfo {
            node: id,
            name: name.to_string(),
            ident: path,
            span,
        });
        id
    }

    fn lookup(&self, name: &str) -> Option<NId> {
        for f in self.frames.iter().rev() {
            for e in f.entries.iter().rev() {
                if e.name == name {
                    return Some(e.binder);
                }
            }
        }
        None
    }

    fn lower(&mut self, e: &Spanned<Expr>, path: &str, _slot: &str) -> NId {
        let span = e.span;
        let text = expr_text(e);
        match &e.node {
            Expr::Int(n) => self.new_node(NodeKind::Int(*n), path.into(), span, text),
            Expr::Bool(b) => self.new_node(NodeKind::Bool(*b), path.into(), span, text),
            Expr::Str(s) => self.new_node(NodeKind::Str(s.clone()), path.into(), span, text),
            Expr::Unit => self.new_node(NodeKind::Unit, path.into(), span, text),
            Expr::Var(name) => {
                let mut n = self.new_node(
                    NodeKind::Var { ref_ord: 0 },
                    path.into(),
                    span,
                    name.clone(),
                );
                if let Some(b) = self.lookup(name) {
                    let ord = self.ref_counts.get(&b).copied().unwrap_or(0);
                    *self.ref_counts.entry(b).or_insert(0) += 1;
                    let node = &mut self.arena.nodes[n as usize];
                    node.refers = Some(b);
                    if let NodeKind::Var { ref_ord } = &mut node.kind {
                        *ref_ord = ord;
                    }
                } else if self.builtins.contains(name) {
                    self.arena.nodes[n as usize].builtin = Some(name.clone());
                }
                let _ = &mut n;
                n
            }
            Expr::Neg(inner) => {
                let c = self.lower(inner, &self.child_path(path, "operand", inner), "operand");
                self.new_node(NodeKind::Neg(c), path.into(), span, text)
            }
            Expr::Not(inner) => {
                let c = self.lower(inner, &self.child_path(path, "operand", inner), "operand");
                self.new_node(NodeKind::Not(c), path.into(), span, text)
            }
            Expr::Bin { op, l, r } => {
                let lp = self.child_path(path, "l", l);
                let rp = self.child_path(path, "r", r);
                let l = self.lower(l, &lp, "l");
                let r = self.lower(r, &rp, "r");
                self.new_node(NodeKind::Bin { op: *op, l, r }, path.into(), span, text)
            }
            Expr::App { fun, arg } => {
                let fp = self.child_path(path, "fun", fun);
                let ap = self.child_path(path, "arg", arg);
                let fun = self.lower(fun, &fp, "fun");
                let arg = self.lower(arg, &ap, "arg");
                self.new_node(NodeKind::App { fun, arg }, path.into(), span, text)
            }
            Expr::Lam { args, body } => {
                debug_assert_eq!(args.len(), 1, "parser folds multi-arg lambdas");
                self.frames.push(Frame { entries: Vec::new() });
                let param = self.make_binder(&args[0], path, "param", span);
                let body_id = self.lower(body, &self.child_path(path, "body", body), "body");
                self.frames.pop();
                self.new_node(NodeKind::Lam { param, body: body_id }, path.into(), span, text)
            }
            Expr::Let { rec, name, value, body, .. } => {
                self.frames.push(Frame { entries: Vec::new() });
                let bnode = self.make_binder(name, path, "binder", span);
                let vp = self.child_path(path, "value", value);
                let bp = self.child_path(path, "body", body);
                let value_id = self.lower(value, &vp, "value");
                let body_id = self.lower(body, &bp, "body");
                self.frames.pop();
                self.new_node(
                    NodeKind::Let { rec: *rec, name: bnode, value: value_id, body: body_id },
                    path.into(),
                    span,
                    text,
                )
            }
            Expr::If { cond, then_e, else_e } => {
                let cond_id = self.lower(cond, &self.child_path(path, "cond", cond), "cond");
                let t = self.lower(then_e, &self.child_path(path, "then", then_e), "then");
                let el = self.lower(else_e, &self.child_path(path, "else", else_e), "else");
                self.new_node(NodeKind::If { cond: cond_id, then_e: t, else_e: el }, path.into(), span, text)
            }
            Expr::Record(fields) => {
                let mut out = Vec::new();
                for f in fields {
                    let (name, val) = match f {
                        FieldInit::Punct { name, val } => (name.clone(), val.clone()),
                        FieldInit::Shorthand { name, span } => (
                            name.clone(),
                            Spanned { node: Expr::Var(name.clone()), span: *span },
                        ),
                        FieldInit::Spread { .. } => unreachable!("spread lowered as Extend"),
                    };
                    let cp = self.child_path(path, &format!("field:{}", name), &val);
                    let v = self.lower(&val, &cp, "field");
                    out.push((name, v));
                }
                self.new_node(NodeKind::Record(out), path.into(), span, text)
            }
            Expr::Extend { fields, base, .. } => {
                let mut out = Vec::new();
                for f in fields {
                    let (name, val) = match f {
                        FieldInit::Punct { name, val } => (name.clone(), val.clone()),
                        FieldInit::Shorthand { name, span } => (
                            name.clone(),
                            Spanned { node: Expr::Var(name.clone()), span: *span },
                        ),
                        FieldInit::Spread { .. } => unreachable!(),
                    };
                    let cp = self.child_path(path, &format!("field:{}", name), &val);
                    let v = self.lower(&val, &cp, "field");
                    out.push((name, v));
                }
                let bp = self.child_path(path, "base", base);
                let base_id = self.lower(base, &bp, "base");
                self.new_node(NodeKind::Extend { fields: out, base: base_id }, path.into(), span, text)
            }
            Expr::Select { obj, label } => {
                let o = self.lower(obj, &self.child_path(path, "obj", obj), "obj");
                self.new_node(NodeKind::Select { obj: o, label: label.clone() }, path.into(), span, text)
            }
            Expr::Variant { tag, payload } => {
                let p = match payload {
                    Some(pe) => Some(self.lower(pe, &self.child_path(path, "payload", pe), "payload")),
                    None => None,
                };
                self.new_node(NodeKind::Variant { tag: tag.clone(), payload: p }, path.into(), span, text)
            }
            Expr::Match { scrut, clauses, has_default } => {
                let s = self.lower(scrut, &self.child_path(path, "scrut", scrut), "scrut");
                let mut arms = Vec::new();
                for (i, Clause { pat, body, span: aspan }) in clauses.iter().enumerate() {
                    self.frames.push(Frame { entries: Vec::new() });
                    let pat = self.lower_pat(pat, path, i as u32);
                    let bpath = {
                        // child path from raw clause body shape
                        let h = short_hash(&self_shape(body));
                        format!("{}::arm[{}].body#{}", path, i, h)
                    };
                    let body_id = self.lower(body, &bpath, "body");
                    self.frames.pop();
                    arms.push(MatchArm { pat, body: body_id, span: *aspan });
                }
                self.new_node(
                    NodeKind::Match { scrut: s, arms, has_default: *has_default },
                    path.into(),
                    span,
                    text,
                )
            }
        }
    }

    fn lower_pat(&mut self, p: &Spanned<Pattern>, owner_path: &str, arm_idx: u32) -> Pat {
        match &p.node {
            Pattern::Wild => {
                let _ = self.make_binder("_", owner_path, &format!("arm[{}].wild", arm_idx), p.span);
                Pat::Wild
            }
            Pattern::Var(name) => {
                let b = self.make_binder(name, owner_path, &format!("arm[{}].binder", arm_idx), p.span);
                Pat::Binder(b)
            }
            Pattern::Tag { tag, payload } => {
                let payload_b = match payload {
                    Some(inner) => match &inner.node {
                        Pattern::Var(name) => Some(self.make_binder(
                            name,
                            owner_path,
                            &format!("arm[{}].payload", arm_idx),
                            inner.span,
                        )),
                        Pattern::Wild => Some(self.make_binder(
                            "_",
                            owner_path,
                            &format!("arm[{}].payload-wild", arm_idx),
                            inner.span,
                        )),
                        Pattern::Tag { .. } => None,
                    },
                    None => None,
                };
                Pat::Tag { tag: tag.clone(), payload: payload_b }
            }
        }
    }
}

fn expr_text(e: &Spanned<Expr>) -> String {
    match &e.node {
        Expr::Int(n) => n.to_string(),
        Expr::Bool(b) => b.to_string(),
        Expr::Str(s) => format!("\"{}\"", s),
        Expr::Unit => "()".into(),
        Expr::Var(n) => n.clone(),
        Expr::Neg(_) => "-".into(),
        Expr::Not(_) => "!".into(),
        Expr::Bin { op, .. } => match op {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Neq => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
        }
        .into(),
        Expr::Lam { .. } => "fn".into(),
        Expr::App { .. } => "app".into(),
        Expr::Let { .. } => "let".into(),
        Expr::If { .. } => "if".into(),
        Expr::Record(_) => "{}".into(),
        Expr::Extend { .. } => "{..}".into(),
        Expr::Select { label, .. } => format!(".{}", label),
        Expr::Variant { tag, .. } => format!("`{}", tag),
        Expr::Match { .. } => "match".into(),
    }
}
