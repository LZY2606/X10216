//! 类型表示：带“来源”的类型节点、行、方案与可读化。

use crate::syntax::Span;
use serde::Serialize;
use std::collections::{BTreeMap, HashMap};

pub type TVid = u32;
pub type RVid = u32;
pub type TyId = u32;

/// 一个类型子部分的来源（追到表达式）。
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Origin {
    pub node: Option<u32>,
    pub span: Option<Span>,
    pub label: String,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TyKind {
    Var(TVid),
    Con(String),
    Fun(TyId, TyId),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct TyNode {
    pub id: TyId,
    pub origin: Origin,
    #[serde(flatten)]
    pub kind: TyKind,
}

/// 行：一组已知字段，尾部为 None（封闭）或 Some(行变量)（开放）。
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Row {
    pub fields: BTreeMap<String, TyId>,
    pub tail: Option<RVid>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Scheme {
    pub vars: Vec<TVid>,
    pub rvars: Vec<RVid>,
    pub body: TyId,
}

#[derive(Default)]
pub struct Arena {
    pub nodes: Vec<TyNode>,
}

impl Arena {
    pub fn new() -> Self { Arena { nodes: Vec::new() } }
    pub fn make(&mut self, kind: TyKind, origin: Origin) -> TyId {
        let id = self.nodes.len() as TyId;
        self.nodes.push(TyNode { id, origin, kind });
        id
    }
    pub fn node(&self, id: TyId) -> &TyNode { &self.nodes[id as usize] }
    pub fn row_fields_mut(&mut self, row: &Row) -> HashMap<String, TyId> {
        row.fields.iter().map(|(k, v)| (k.clone(), *v)).collect()
    }
}

#[derive(Clone, Debug, Default)]
pub struct Supply {
    next_t: TVid,
    next_r: RVid,
}

impl Supply {
    pub fn fresh_t(&mut self) -> TVid { let v = self.next_t; self.next_t += 1; v }
    pub fn fresh_r(&mut self) -> RVid { let v = self.next_r; self.next_r += 1; v }
}

// ---------------- 自由变量收集 ----------------

pub fn free_vars(arena: &Arena, t: TyId, sub: &Subst) -> Vec<TVid> {
    let mut out = Vec::new();
    let mut stack = vec![t];
    let mut seen = std::collections::HashSet::new();
    while let Some(id) = stack.pop() {
        let id = sub.resolve(arena, id);
        match &arena.node(id).kind {
            TyKind::Var(v) => {
                if seen.insert(*v) { out.push(*v); }
            }
            TyKind::Con(_) => {}
            TyKind::Fun(a, b) => { stack.push(*a); stack.push(*b); }
            TyKind::Record(r) | TyKind::Variant(r) => {
                for (_, f) in &r.fields { stack.push(*f); }
                if let Some(rv) = r.tail {
                    if let Some(bound) = sub.rget(rv) {
                        for (_, f) in &bound.fields { stack.push(*f); }
                    }
                }
            }
        }
    }
    out
}

pub fn free_rvars(arena: &Arena, t: TyId, sub: &Subst) -> Vec<RVid> {
    let mut out = Vec::new();
    let mut stack = vec![t];
    let mut seen = std::collections::HashSet::new();
    while let Some(id) = stack.pop() {
        let id = sub.resolve(arena, id);
        match &arena.node(id).kind {
            TyKind::Record(r) | TyKind::Variant(r) => {
                for (_, f) in &r.fields { stack.push(*f); }
                match r.tail {
                    None => {}
                    Some(rv) => {
                        if let Some(bound) = sub.rget(rv) {
                            stack_row(&bound, &mut stack);
                        } else if seen.insert(rv) {
                            out.push(rv);
                        }
                    }
                }
            }
            TyKind::Fun(a, b) => { stack.push(*a); stack.push(*b); }
            _ => {}
        }
    }
    out
}

fn stack_row(r: &Row, stack: &mut Vec<TyId>) {
    for (_, f) in &r.fields { stack.push(*f); }
}

// ---------------- 替换 ----------------

#[derive(Clone, Debug, Default)]
pub struct Binding {
    /// 触发该绑定的约束 id 集合（用于不可满足核心）。
    pub support: Vec<u32>,
}

#[derive(Clone, Debug, Default)]
pub struct Subst {
    tmap: HashMap<TVid, TyId>,
    rmap: HashMap<RVid, Row>,
    /// 每个绑定所依赖的约束（按目标类型节点记录不便，改为并行 map）。
    tbind: HashMap<TVid, Vec<u32>>,
    rbind: HashMap<RVid, Vec<u32>>,
}

impl Subst {
    pub fn new() -> Self { Self::default() }

    pub fn tbind_var(&mut self, v: TVid, t: TyId, support: Vec<u32>) {
        self.tmap.insert(v, t);
        self.tbind.insert(v, support);
    }
    pub fn rbind_var(&mut self, rv: RVid, row: Row, support: Vec<u32>) {
        self.rmap.insert(rv, row);
        self.rbind.insert(rv, support);
    }
    pub fn tget(&self, v: TVid) -> Option<TyId> { self.tmap.get(&v).copied() }
    pub fn rget(&self, rv: RVid) -> Option<Row> { self.rmap.get(&rv).copied() }
    pub fn tsupport(&self, v: TVid) -> Vec<u32> { self.tbind.get(&v).cloned().unwrap_or_default() }
    pub fn rsupport(&self, rv: RVid) -> Vec<u32> { self.rbind.get(&rv).cloned().unwrap_or_default() }

    /// 沿类型变量替换链走到根。
    pub fn resolve(&self, arena: &Arena, mut t: TyId) -> TyId {
        loop {
            match &arena.node(t).kind {
                TyKind::Var(v) => match self.tmap.get(v) {
                    Some(next) => t = *next,
                    None => return t,
                },
                _ => return t,
            }
        }
    }

    /// 把行展开成最终形态（沿尾部变量链）。
    pub fn resolve_row(&self, row: &Row) -> Row {
        let mut fields = BTreeMap::new();
        let mut tail = row.tail;
        for (k, v) in &row.fields {
            fields.insert(k.clone(), *v);
        }
        let mut guard = 0;
        while let Some(rv) = tail {
            guard += 1;
            if guard > 10_000 { break; }
            match self.rmap.get(&rv) {
                Some(next) => {
                    for (k, v) in &next.fields {
                        fields.entry(k.clone()).or_insert(*v);
                    }
                    tail = next.tail;
                }
                None => break,
            }
        }
        Row { fields, tail }
    }
}

// ---------------- 可读化 / 规范化 ----------------

/// 以确定性的变量重命名把类型渲染成字符串（用于等价比较与缓存）。
pub fn canonical(arena: &Arena, sub: &Subst, t: TyId) -> String {
    let mut tnames: HashMap<TVid, String> = HashMap::new();
    let mut rnames: HashMap<RVid, String> = HashMap::new();
    let mut ti = 0;
    let mut ri = 0;
    fn tname(m: &mut HashMap<TVid, String>, i: &mut usize, v: TVid) -> String {
        if let Some(s) = m.get(&v) { return s.clone(); }
        let s = format!("t{}", *i);
        *i += 1;
        m.insert(v, s.clone());
        s
    }
    fn render(arena: &Arena, sub: &Subst, t: TyId,
              tn: &mut HashMap<TVid, String>, rn: &mut HashMap<RVid, String>,
              ti: &mut usize, ri: &mut usize) -> String {
        let t = sub.resolve(arena, t);
        match &arena.node(t).kind {
            TyKind::Var(v) => tname(tn, ti, *v),
            TyKind::Con(c) => c.clone(),
            TyKind::Fun(a, b) => {
                let l = render(arena, sub, *a, tn, rn, ti, ri);
                let r = render(arena, sub, *b, tn, rn, ti, ri);
                let l = if matches!(arena.node(sub.resolve(arena, *a)).kind, TyKind::Fun(_, _)) {
                    format!("({l})")
                } else { l };
                format!("{l} -> {r}")
            }
            TyKind::Record(row) => render_row(arena, sub, row, tn, rn, ti, ri, "{", "}"),
            TyKind::Variant(row) => render_row(arena, sub, row, tn, rn, ti, ri, "[|", "|]"),
        }
    }
    fn render_row(arena: &Arena, sub: &Subst, row: &Row,
                  tn: &mut HashMap<TVid, String>, rn: &mut HashMap<RVid, String>,
                  ti: &mut usize, ri: &mut usize, open: &str, close: &str) -> String {
        let full = sub.resolve_row(row);
        let mut parts: Vec<String> = full.fields.iter()
            .map(|(k, v)| format!("{k}: {}", render(arena, sub, *v, tn, rn, ti, ri)))
            .collect();
        if let Some(rv) = full.tail {
            if let Some(s) = rn.get(&rv) {
                parts.push(s.clone());
            } else {
                let s = format!("r{}", *ri);
                *ri += 1;
                rn.insert(rv, s.clone());
                parts.push(s);
            }
        }
        format!("{open}{}{close}", parts.join(", "))
    }
    render(arena, sub, t, &mut tnames, &mut rnames, &mut ti, &mut ri)
}

/// 构造一个带源码来源的 Origin。
pub fn origin(node: Option<u32>, span: Option<Span>, label: impl Into<String>) -> Origin {
    Origin { node, span, label: label.into() }
}
