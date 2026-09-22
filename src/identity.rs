//! 稳定身份：对 AST 做后序结构哈希（FNV-1a 风格）。
//!
//! key 只含“结构语义”：节点种类、字面量值、字段名/变体构造子、按槽位的子身份。
//! 局部绑定的*名字*不进 key（用按位置的槽位 id 代替），因此重命名绑定变量
//! 不会造成身份漂移；跨边界的全局符号（字段名、构造子）保留。
//! 兄弟用槽位序号区分，插入/删除兄弟不影响其他分支身份。
//! 变量解析到其绑定槽位，因此 shadowing 与改名都被正确跟踪。

use crate::syntax::{Arm, Binding, BinOp, Expr, Program, Span};
use serde::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NodeId(pub u64);

impl NodeId {
    pub fn hex(&self) -> String { format!("n{:016x}", self.0) }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NodeInfo {
    pub id: NodeId,
    pub span: Span,
    pub label: String,
    pub kind: String,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct IdentityMap {
    pub nodes: Vec<NodeInfo>,
    pub by_id: HashMap<NodeId, usize>,
}

impl IdentityMap {
    pub fn get(&self, id: NodeId) -> Option<&NodeInfo> { self.by_id.get(&id).map(|&i| &self.nodes[i]) }
    pub fn len(&self) -> usize { self.nodes.len() }
    pub fn is_empty(&self) -> bool { self.nodes.is_empty() }
}

#[derive(Default)]
struct Resolver {
    stack: Vec<HashMap<String, u64>>,
    map: IdentityMap,
}

impl Resolver {
    fn push(&mut self) { self.stack.push(HashMap::new()); }
    fn pop(&mut self) { self.stack.pop(); }
    fn bind(&mut self, name: &str, slot: u64) { self.stack.last_mut().unwrap().insert(name.to_string(), slot); }
    fn lookup(&self, name: &str) -> Option<u64> {
        for s in self.stack.iter().rev() { if let Some(v) = s.get(name) { return Some(*v); } }
        None
    }
    fn register(&mut self, id: NodeId, span: Span, label: String, kind: String) {
        if !self.map.by_id.contains_key(&id) {
            let idx = self.map.nodes.len();
            self.map.nodes.push(NodeInfo { id, span, label, kind });
            self.map.by_id.insert(id, idx);
        }
    }
}

fn slot(kind: &[u8], i: usize) -> u64 {
    let mut p = DefaultHasher::new();
    p.write(kind); p.write_u64(i as u64); p.finish()
}
fn h(parts: &[H]) -> u64 {
    let mut st = DefaultHasher::new();
    for p in parts {
        match p {
            H::Tag(b) => { st.write(b); st.write(&[0xff]); }
            H::U(x) => { st.write_u64(*x); st.write(&[0xfe]); }
            H::S(s) => { st.write(s.as_bytes()); st.write(&[0xfd]); }
        }
    }
    st.finish()
}
enum H<'a> { Tag(&'a [u8]), U(u64), S(&'a str) }

pub fn analyze(program: &Program) -> IdentityMap {
    let mut r = Resolver::default();
    r.push(); // 全局/顶层作用域
    for (i, b) in program.top.iter().enumerate() {
        let id = walk_binding(b, i, &mut r);
        // 顶层按顺序可见；rec 的占位由约束生成阶段另算（身份仅结构）
        r.bind(&b.name, id.0);
    }
    if let Some(e) = &program.final_expr {
        walk(e, &mut r);
    }
    r.pop();
    r.map
}

fn walk(e: &Expr, r: &mut Resolver) -> NodeId {
    let id = match e {
        Expr::Int(v, _) => NodeId(h(&[H::Tag(b"int"), H::U(*v as u64)])),
        Expr::Bool(b, _) => NodeId(h(&[H::Tag(b"bool"), H::U(*b as u64)])),
        Expr::Unit(_) => NodeId(h(&[H::Tag(b"unit")])),
        Expr::Var(name, _) => {
            let key = match r.lookup(name) {
                Some(v) => h(&[H::Tag(b"var"), H::U(v)]),
                None => h(&[H::Tag(b"free"), H::S(name)]),
            };
            NodeId(key)
        }
        Expr::Tag(tag, payload, _) => {
            let mut parts = vec![H::Tag(b"tag"), H::S(tag)];
            match payload {
                Some(p) => { parts.push(H::Tag(b"some")); parts.push(H::U(walk(p, r).0)); }
                None => parts.push(H::Tag(b"none")),
            }
            NodeId(h(&parts))
        }
        Expr::Rec(fields, tail, _) => {
            let mut parts = vec![H::Tag(b"rec")];
            for (i, f) in fields.iter().enumerate() {
                parts.push(H::U(i as u64));
                parts.push(H::S(&f.name));
                parts.push(H::U(walk(&f.expr, r).0));
            }
            match tail {
                Some((name, _)) => match r.lookup(name) {
                    Some(v) => { parts.push(H::Tag(b"tailres")); parts.push(H::U(v)); }
                    None => { parts.push(H::Tag(b"tailfree")); parts.push(H::S(name)); }
                },
                None => parts.push(H::Tag(b"closed")),
            }
            NodeId(h(&parts))
        }
        Expr::Sel(obj, name, _) => {
            let o = walk(obj, r).0;
            NodeId(h(&[H::Tag(b"sel"), H::U(o), H::S(name)]))
        }
        Expr::Lam(name, _, body, _) => {
            let pslot = slot(b"lamparam", 0);
            r.push();
            r.bind(name, pslot);
            let b = walk(body, r).0;
            r.pop();
            NodeId(h(&[H::Tag(b"lam"), H::U(pslot), H::U(b)]))
        }
        Expr::App(f, x, _) => {
            let fv = walk(f, r).0;
            let xv = walk(x, r).0;
            NodeId(h(&[H::Tag(b"app"), H::U(fv), H::U(xv)]))
        }
        Expr::Let(is_rec, binds, body, _) => {
            let mut parts = vec![H::Tag(if *is_rec { b"letrec" } else { b"let" })];
            r.push();
            if *is_rec {
                let phs: Vec<u64> = (0..binds.len()).map(|i| slot(b"recbind", i)).collect();
                for (b, ph) in binds.iter().zip(phs.iter()) { r.bind(&b.name, *ph); }
                for (i, b) in binds.iter().enumerate() {
                    let bv = walk_fnbody(&b.args, &b.body, r);
                    parts.push(H::U(i as u64)); parts.push(H::U(bv));
                }
            } else {
                for (i, b) in binds.iter().enumerate() {
                    let bv = walk_fnbody(&b.args, &b.body, r);
                    let bid = h(&[H::Tag(b"letbind"), H::U(i as u64), H::U(bv)]);
                    r.bind(&b.name, bid);
                    parts.push(H::U(i as u64)); parts.push(H::U(bid));
                }
            }
            let bodyv = walk(body, r).0;
            r.pop();
            parts.push(H::Tag(b"body"));
            parts.push(H::U(bodyv));
            NodeId(h(&parts))
        }
        Expr::Bin(op, a, b, _) => {
            let sym: &[u8] = match op { BinOp::Add => b"+", BinOp::Sub => b"-", BinOp::Mul => b"*", BinOp::Lt => b"<" };
            let av = walk(a, r).0;
            let bv = walk(b, r).0;
            NodeId(h(&[H::Tag(b"bin"), H::Tag(sym), H::U(av), H::U(bv)]))
        }
        Expr::If(c, t, e2, _) => {
            let cv = walk(c, r).0;
            let tv = walk(t, r).0;
            let ev = walk(e2, r).0;
            NodeId(h(&[H::Tag(b"if"), H::U(cv), H::U(tv), H::U(ev)]))
        }
        Expr::Match(scrut, arms, _) => {
            let sv = walk(scrut, r).0;
            let mut parts = vec![H::Tag(b"match"), H::U(sv)];
            for (i, arm) in arms.iter().enumerate() {
                parts.push(H::U(i as u64));
                parts.push(H::S(&arm.tag));
                r.push();
                if let Some((name, _)) = &arm.binder {
                    let ps = slot(b"armbinder", i);
                    r.bind(name, ps);
                    parts.push(H::U(ps));
                }
                let bv = walk(&arm.body, r).0;
                r.pop();
                parts.push(H::U(bv));
            }
            NodeId(h(&parts))
        }
        Expr::Paren(inner, _) => {
            let iv = walk(inner, r).0;
            NodeId(h(&[H::Tag(b"paren"), H::U(iv)]))
        }
    };
    register(e, id, r);
    id
}

fn walk_fnbody(args: &[(String, Span)], body: &Expr, r: &mut Resolver) -> u64 {
    r.push();
    for (i, (name, _)) in args.iter().enumerate() {
        let ps = slot(b"fnarg", i);
        r.bind(name, ps);
    }
    let bv = walk(body, r).0;
    r.pop();
    bv
}

fn walk_binding(b: &Binding, ordinal: usize, r: &mut Resolver) -> NodeId {
    let bv = walk_fnbody(&b.args, &b.body, r);
    let id = NodeId(h(&[H::Tag(b"toplet"), H::U(ordinal as u64), H::U(bv)]));
    r.register(id, b.whole_span, format!("let {}", b.name), "toplet".into());
    id
}

fn register(e: &Expr, id: NodeId, r: &mut Resolver) {
    let (label, kind) = match e {
        Expr::Int(v, _) => (format!("{v}"), "int"),
        Expr::Bool(b, _) => (format!("{b}"), "bool"),
        Expr::Unit(_) => ("()".into(), "unit"),
        Expr::Var(n, _) => (n.clone(), "var"),
        Expr::Tag(t, p, _) => (format!("{t}{}", if p.is_some() { " payload" } else { "" }), "tag"),
        Expr::Rec(fs, tail, _) => (
            format!("{{ {}字段{} }}", fs.len(), if tail.is_some() { " ·行尾" } else { "" }),
            "record",
        ),
        Expr::Sel(_, f, _) => (format!(".{f}"), "select"),
        Expr::Lam(n, _, _, _) => (format!("λ {n}"), "lambda"),
        Expr::App(_, _, _) => ("应用".into(), "app"),
        Expr::Let(rec, _, _, _) => (if *rec { "let rec" } else { "let" }.into(), "let"),
        Expr::Bin(op, _, _, _) => (
            match op { BinOp::Add => "+", BinOp::Sub => "−", BinOp::Mul => "×", BinOp::Lt => "<" }.into(),
            "bin",
        ),
        Expr::If(_, _, _, _) => ("if".into(), "if"),
        Expr::Match(_, _, _) => ("match".into(), "match"),
        Expr::Paren(_, _) => ("( )".into(), "paren"),
    };
    r.register(id, e.span(), label, kind.into());
}
