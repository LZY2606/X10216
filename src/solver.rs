use crate::ast::*;
use crate::ty::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Event {
    Infer { node: NodeId, ordinal: u32 },
    Equal { node: NodeId, ordinal: u32 },
    Generalize { node: NodeId },
    Instantiate { node: NodeId },
    ScopeEnter { node: NodeId },
    ScopeExit { node: NodeId },
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Constraint {
    pub id: String,
    pub node: NodeId,
    pub ordinal: u32,
    /// structural key including scope/gen-env hash; identical keys reuse identity.
    pub key: String,
    pub left: Ty,
    pub right: Ty,
    pub origin: String,
    #[serde(default)]
    pub disabled: bool,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StepDetail {
    InferNode { node: NodeId, fresh: String, origin: String },
    Unify { left: String, right: String },
    Bind { var: String, ty: String },
    RowBind { var: String, present: Vec<String>, kind: String },
    Instantiate { name: String, scheme: String, fresh: String },
    Generalize { name: String, quantified: Vec<String>, body: String },
    ScopeEnter { node: NodeId },
    ScopeExit { node: NodeId },
    Error { message: String },
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Step {
    pub index: usize,
    pub node: NodeId,
    pub cid: Option<String>,
    pub detail: StepDetail,
    pub description: String,
    pub level: u32,
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TypeError {
    Mismatch {
        message: String,
        span: crate::span::Span,
        left: String,
        right: String,
        cid: Option<String>,
        detail: MismatchDetail,
    },
    Occurs {
        message: String,
        span: crate::span::Span,
        var: String,
        ty: String,
        cid: Option<String>,
    },
    Unbound { message: String, span: crate::span::Span },
}

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MismatchDetail {
    Con,
    Arrow,
    RecordClosed { missing: Vec<String>, extra: Vec<String> },
    VariantClosed { missing: Vec<String>, extra: Vec<String> },
    RowKind,
    Other,
}

struct Binding { ty: Ty, val_restricted: bool, scheme: Option<Scheme> }

pub struct Solver {
    events: Vec<Event>,
    env: Vec<Binding>,
    var_level: BTreeMap<u32, u32>,
    subst: BTreeMap<u32, Ty>,
    row_present: BTreeMap<u32, BTreeSet<String>>,
    row_kind: BTreeMap<u32, RowKind>,
    next_var: u32,
    current_level: u32,
    steps: Vec<Step>,
    disabled: BTreeSet<String>,
    pub error: Option<TypeError>,
    node_ty: BTreeMap<NodeId, Ty>,
    schemes: Vec<(String, NodeId, Scheme)>,
    // per-node ordinal counters during generation
    infer_ord: BTreeMap<NodeId, u32>,
    equal_ord: BTreeMap<NodeId, u32>,
    cid_stack: Vec<String>,
    scope_stack: Vec<NodeId>,
    scope_hash: u64,
    gen_hash: u64,
    constraint_keys: BTreeMap<String, String>, // cid -> key
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RowKind { Record, Variant }

fn fnv_str(h: &mut u64, s: &str) {
    for b in s.bytes() {
        *h ^= b as u64;
        *h = h.wrapping_mul(0x100000001b3);
    }
}

fn fnv_ty(h: &mut u64, t: &Ty) {
    match t {
        Ty::Var(SVar(i)) => { fnv_str(h, "v"); fnv_str(h, &i.to_string()); }
        Ty::Con(s) => { fnv_str(h, "c"); fnv_str(h, s); }
        Ty::Arrow(a, b) => { fnv_str(h, "->"); fnv_ty(h, a); fnv_ty(h, b); }
        Ty::Record(r) => {
            fnv_str(h, "R{");
            for (k, v) in &r.fields { fnv_str(h, k); fnv_str(h, "="); fnv_ty(h, v); }
            match &r.tail { RowTail::Closed => fnv_str(h, "]"), RowTail::Open(SVar(i)) => { fnv_str(h, "|"); fnv_str(h, &i.to_string()); } }
        }
        Ty::Variant(r) => {
            fnv_str(h, "V[");
            for (k, v) in &r.fields { fnv_str(h, k); fnv_str(h, ":"); fnv_ty(h, v); }
            match &r.tail { RowTail::Closed => fnv_str(h, "]"), RowTail::Open(SVar(i)) => { fnv_str(h, "|"); fnv_str(h, &i.to_string()); } }
        }
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct GeneralizedLet { pub name: String, pub node: NodeId, pub scheme: String }

#[derive(Clone, Serialize, Deserialize, Debug, Default)]
pub struct SolveResult {
    pub steps: Vec<Step>,
    pub constraints: Vec<Constraint>,
    pub events: Vec<Event>,
    pub root_ty: Option<Ty>,
    pub schemes: Vec<GeneralizedLet>,
    pub error: Option<TypeError>,
    pub constraint_keys: BTreeMap<String, String>,
}

impl Solver {
    pub fn new(events: Vec<Event>, disabled: BTreeSet<String>) -> Self {
        Solver {
            events, env: vec![], var_level: BTreeMap::new(), subst: BTreeMap::new(),
            row_present: BTreeMap::new(), row_kind: BTreeMap::new(),
            next_var: 0, current_level: 0, steps: vec![], disabled, error: None,
            node_ty: BTreeMap::new(), schemes: vec![],
            infer_ord: BTreeMap::new(), equal_ord: BTreeMap::new(),
            cid_stack: vec![], scope_stack: vec![], scope_hash: 0xcbf29ce484222325,
            gen_hash: 0xcbf29ce484222325, constraint_keys: BTreeMap::new(),
        }
    }

    fn fresh(&mut self) -> Ty {
        let i = self.next_var;
        self.next_var += 1;
        self.var_level.insert(i, self.current_level);
        Ty::Var(SVar(i))
    }

    fn fresh_row(&mut self, kind: RowKind) -> Ty {
        let i = self.next_var;
        self.next_var += 1;
        self.var_level.insert(i, self.current_level);
        self.row_kind.insert(i, kind);
        self.row_present.insert(i, BTreeSet::new());
        Ty::Var(SVar(i))
    }

    fn log(&mut self, node: NodeId, detail: StepDetail, description: String) {
        let index = self.steps.len();
        self.steps.push(Step { index, node, cid: self.cid_stack.last().cloned(), detail, description, level: self.current_level });
    }

    fn resolve(&mut self, t: Ty) -> Ty {
        match t {
            Ty::Var(SVar(i)) => match self.subst.get(&i).cloned() {
                Some(t2) => {
                    let r = self.resolve(t2);
                    if r != t2 { self.subst.insert(i, r.clone()); }
                    r
                }
                None => Ty::Var(SVar(i)),
            },
            t => t,
        }
    }

    fn occurs(&self, v: u32, t: &Ty) -> bool {
        match t {
            Ty::Var(SVar(i)) => {
                if *i == v { return true; }
                match self.subst.get(i) { Some(t2) => self.occurs(v, t2), None => false }
            }
            Ty::Con(_) => false,
            Ty::Arrow(a, b) => self.occurs(v, a) || self.occurs(v, b),
            Ty::Record(r) | Ty::Variant(r) => {
                r.fields.values().any(|t| self.occurs(v, t))
                    || match &r.tail { RowTail::Closed => false, RowTail::Open(SVar(i)) => self.occurs(v, &Ty::Var(SVar(*i))) }
            }
        }
    }

    fn free_vars_levels(&self, t: &Ty, out: &mut BTreeMap<u32, u32>) {
        match t {
            Ty::Var(SVar(i)) => {
                if let Some(t2) = self.subst.get(i) {
                    self.free_vars_levels(t2, out);
                } else if !out.contains_key(i) {
                    out.insert(*i, *self.var_level.get(i).unwrap_or(&0));
                }
            }
            Ty::Con(_) => {}
            Ty::Arrow(a, b) => { self.free_vars_levels(a, out); self.free_vars_levels(b, out); }
            Ty::Record(r) | Ty::Variant(r) => {
                for t in r.fields.values() { self.free_vars_levels(t, out); }
                if let RowTail::Open(SVar(i)) = &r.tail { self.free_vars_levels(&Ty::Var(SVar(*i)), out); }
            }
        }
    }
}

impl Solver {
    fn unify(&mut self, left: Ty, right: Ty, node: NodeId) -> Result<(), TypeError> {
        let l = self.resolve(left.clone());
        let r = self.resolve(right.clone());
        self.log(node, StepDetail::Unify { left: l.pretty(), right: r.pretty() },
            format!("统一 {} ≔ {}", l.pretty(), r.pretty()));
        match (l.clone(), r.clone()) {
            (Ty::Var(a), Ty::Var(b)) if a == b => Ok(()),
            (Ty::Var(SVar(i)), t) | (t, Ty::Var(SVar(i))) => {
                if self.occurs(i, &t) {
                    let err = TypeError::Occurs {
                        message: format!("出现检查失败：类型变量无限递归展开到 {}（拒绝无限类型）", t.pretty()),
                        span: self.ast_span(node), var: format!("t{}", i), ty: t.pretty(),
                        cid: self.cid_stack.last().cloned(),
                    };
                    self.log(node, StepDetail::Error { message: err.message_text() }, "发生 occurs-check 冲突".into());
                    return Err(err);
                }
                // level = min
                let lv = *self.var_level.get(&i).unwrap_or(&0);
                let mut fv = BTreeMap::new();
                self.free_vars_levels(&t, &mut fv);
                if let Some((_, min_other)) = fv.iter().map(|(v, lv2)| (*v, *lv2)).min_by_key(|(_, l)| *l) {
                    let new_lv = lv.min(min_other);
                    self.var_level.insert(i, new_lv);
                    for v in fv.keys() {
                        let cur = *self.var_level.get(v).unwrap_or(&0);
                        if cur > new_lv { self.var_level.insert(*v, new_lv); }
                    }
                }
                // row variable bookkeeping: only bind when shape compatible
                if let Some(kind) = self.row_kind.get(&i).copied() {
                    let compatible = match &t {
                        Ty::Record(_) => kind == RowKind::Record,
                        Ty::Variant(_) => kind == RowKind::Variant,
                        Ty::Var(SVar(j)) => self.row_kind.get(j).copied().map(|k2| k2 == kind).unwrap_or(true),
                        _ => false,
                    };
                    if !compatible {
                        return self.mismatch(node, l.clone(), t.clone(), MismatchDetail::RowKind);
                    }
                }
                self.subst.insert(i, t.clone());
                self.log(node, StepDetail::Bind { var: format!("t{}", i), ty: t.pretty() },
                    format!("绑定 t{} ≔ {}", i, t.pretty()));
                Ok(())
            }
            (Ty::Con(a), Ty::Con(b)) if a == b => Ok(()),
            (Ty::Con(a), Ty::Con(b)) => {
                self.mismatch(node, Ty::Con(a), Ty::Con(b), MismatchDetail::Con)
            }
            (Ty::Arrow(a1, b1), Ty::Arrow(a2, b2)) => {
                self.unify(*a1, *a2, node).and_then(|_| self.unify(*b1, *b2, node))
            }
            (Ty::Record(r1), Ty::Record(r2)) => self.unify_rows(r1, r2, node, RowKind::Record),
            (Ty::Variant(r1), Ty::Variant(r2)) => self.unify_rows(r1, r2, node, RowKind::Variant),
            _ => self.mismatch(node, l, r, MismatchDetail::Other),
        }
    }

    fn mismatch(&self, node: NodeId, left: Ty, right: Ty, detail: MismatchDetail) -> Result<(), TypeError> {
        let msg = match &detail {
            MismatchDetail::Con => format!("类型不兼容：{} 与 {} 是不同的基础类型", left.pretty(), right.pretty()),
            MismatchDetail::Arrow => format!("函数类型不兼容：{} 与 {}", left.pretty(), right.pretty()),
            MismatchDetail::RecordClosed { missing, extra } | MismatchDetail::VariantClosed { missing, extra } => {
                let what = if matches!(detail, MismatchDetail::RecordClosed { .. }) { "记录" } else { "变体" };
                let mut bits = vec![format!("{} 类型不兼容：{} 与 {}", what, left.pretty(), right.pretty())];
                if !missing.is_empty() { bits.push(format!("缺少标签 {}", missing.join(", "))); }
                if !extra.is_empty() { bits.push(format!("多余标签 {}", extra.join(")) }
                bits.join("；")
            }
            MismatchDetail::RowKind => format!("行类型种类不兼容：记录行与变体行不能统一（{} 与 {}）", left.pretty(), right.pretty()),
            MismatchDetail::Other => format!("类型构造不兼容：{} 与 {}", left.pretty(), right.pretty()),
        };
        Err(TypeError::Mismatch {
            message: msg,
            span: self.ast_span(node),
            left: left.pretty(),
            right: right.pretty(),
            cid: self.cid_stack.last().cloned(),
            detail,
        })
    }

    fn ast_span(&self, node: NodeId) -> crate::span::Span {
        // AST is attached during run via thread-local-free field below.
        self.span_of(node)
    }
}
