//! 并查集求解内核：类型变量与行变量两套 union-find，
//! 含 occurs-check、基于环境自由变量的泛化与 value restriction。
use crate::ast::NodeId;
use crate::types::*;
use std::collections::{BTreeMap, BTreeSet};

/// 约束编号（一次分析内稳定）。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Polarity {
    /// 左侧是“期望”，右侧是“实际”。
    ExpectedActual,
    /// 内部等价（无方向）。
    Equal,
}

#[derive(Clone, Debug)]
enum TStruct {
    Int,
    Bool,
    Unit,
    Arrow(TV, TV),
    Record(RV),
    Variant(RV),
}

#[derive(Clone)]
enum TDesc {
    Unbound { rank: u32 },
    Struct(TStruct),
}

#[derive(Clone)]
struct RDesc {
    /// 开放行的字段与尾行；closed=true 时 tail 无意义。
    fields: Vec<(String, TV)>,
    closed: bool,
    tail: RV,
    bound: bool,
}

impl RDesc {
    fn unbound() -> Self { RDesc { fields: vec![], closed: false, tail: RV(u32::MAX), bound: false } }
}

/// 来源追踪：一个类型/行变量由哪条约束引入，又由哪条约束绑定。
#[derive(Clone, Copy, Debug)]
pub struct Origin {
    pub introduced: CId,
    pub node: NodeId,
}

/// 求解过程中的统一步骤。
#[derive(Clone, Debug)]
pub enum Step {
    Unify { cid: CId, left: String, right: String, result: String, ok: bool, detail: String },
    Instantiate { cid: CId, name: String, scheme: String, instance: String },
    Generalize { cid: CId, name: String, scheme: String, vars: usize, rvars: usize, restricted: bool },
    RecBind { cid: CId, name: String, ty: String },
    Scope { cid: CId, open: bool, label: String },
    Fail { cid: CId, message: String },
    Skip { cid: CId, label: String },
}

#[derive(Clone, Debug)]
pub enum TypeErrorKind {
    TypeMismatch,
    InfiniteType,
    RowMismatch,
    UnboundVariable,
    DuplicateField,
    ArityShape,
}

#[derive(Clone, Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub cid: CId,
    pub message: String,
    pub prov: Vec<CId>,
    pub expected: Option<RType>,
    pub actual: Option<RType>,
    pub missing: Vec<String>,
    pub extra: Vec<String>,
    pub var_name: Option<String>,
}

#[derive(Clone)]
enum Local {
    Mono(TV),
    Poly(Scheme),
    /// 递归绑定：指向一个（单态）TV，泛化推迟到 GenEnd。
    Rec(TV),
}

#[derive(Clone)]
pub struct Solver {
    t_parent: Vec<u32>,
    t_rank: Vec<u32>,
    t_desc: Vec<TDesc>,
    t_origin: Vec<Origin>,
    r_parent: Vec<u32>,
    r_desc: Vec<RDesc>,
    r_origin: Vec<Origin>,
    r_rank: Vec<u32>,
    tcur: u32,
    rcur: u32,
    /// 当前词法层级（用于 rank 泛化）。
    level: u32,
    pub steps: Vec<Step>,
    pub errors: Vec<TypeError>,
    pub disabled: BTreeSet<CId>,
    locals: Vec<(String, Local)>,
    /// 每一层递归绑定使用的 TV，用于排除泛化。
    rec_anchors: Vec<Vec<TV>>,
    /// 每个词法层级内由 BindLocal 压入的绑定数，ScopeEnd 时弹出。
    bind_stack: Vec<u32>,
    local_bind_depth: Vec<u32>,
    /// 待 GenEnd 泛化的递归绑定。
    pending_rec: Vec<(String, TV, CId)>,
    node_spans: Vec<crate::ast::Span>,
    pending_error: Option<TypeError>,
}

impl Solver {
    pub fn new(node_count: usize) -> Self {
        Solver {
            t_parent: vec![], t_rank: vec![], t_desc: vec![], t_origin: vec![],
            r_parent: vec![], r_desc: vec![], r_origin: vec![], r_rank: vec![],
            tcur: 0, rcur: 0, level: 0,
            steps: vec![], errors: vec![], disabled: BTreeSet::new(),
            locals: vec![], rec_anchors: vec![], pending_rec: vec![],
            bind_stack: vec![], local_bind_depth: vec![],
            node_spans: vec![crate::ast::Span::new(0, 0); node_count],
            pending_error: None,
        }
    }

    pub fn set_span(&mut self, nid: NodeId, span: crate::ast::Span) {
        let i = nid.0 as usize;
        if i < self.node_spans.len() { self.node_spans[i] = span; }
    }

    fn fresh_t(&mut self, rank: u32, o: Origin) -> TV {
        let id = self.tcur; self.tcur += 1;
        self.t_parent.push(id);
        self.t_rank.push(rank);
        self.t_desc.push(TDesc::Unbound { rank });
        self.t_origin.push(o);
        TV(id)
    }

    /// 在当前词法层级新建类型变量。
    fn fresh_at_level(&mut self, o: Origin) -> TV {
        self.fresh_t(self.cur_level(), o)
    }

    fn fresh_r(&mut self, o: Origin) -> RV {
        let id = self.rcur; self.rcur += 1;
        self.r_parent.push(id);
        self.r_desc.push(RDesc::unbound());
        self.r_origin.push(o);
        self.r_rank.push(self.cur_level());
        RV(id)
    }

    fn cur_level(&self) -> u32 { self.level }

    fn find_t(&mut self, t: TV) -> TV {
        let p = self.t_parent[t.0 as usize];
        if p != t.0 {
            let r = self.find_t(TV(p));
            self.t_parent[t.0 as usize] = r.0;
            r
        } else { t }
    }

    fn find_r(&mut self, r: RV) -> RV {
        let p = self.r_parent[r.0 as usize];
        if p != r.0 {
            let rr = self.find_r(RV(p));
            self.r_parent[r.0 as usize] = rr.0;
            rr
        } else { r }
    }

    fn desc_t(&self, t: TV) -> &TDesc { &self.t_desc[t.0 as usize] }
    fn desc_r(&self, r: RV) -> &RDesc { &self.r_desc[r.0 as usize] }

    fn span_of(&self, n: NodeId) -> crate::ast::Span {
        self.node_spans.get(n.0 as usize).copied().unwrap_or(crate::ast::Span::new(0,0))
    }

    // ---------- 类型统一 ----------

    /// 统一两个类型。`pol` 控制错误的“期望/实际”方向。
    /// `cid` 为直接约束，`trail` 为参与来源约束（包含 cid）。
    pub fn unify(&mut self, a: TV, b: TV, pol: Polarity, cid: CId, prov: &mut BTreeSet<CId>) -> bool {
        let a = self.find_t(a);
        let b = self.find_t(b);
        if a == b { return true; }
        match (self.desc_t(a).clone(), self.desc_t(b).clone()) {
            (TDesc::Unbound { .. }, _) => {
                if self.occurs_t(b, a, prov) {
                    self.fail_infinite(a, b, cid, prov, pol);
                    return false;
                }
                self.link_t(a, b, cid);
                true
            }
            (_, TDesc::Unbound { .. }) => {
                if self.occurs_t(a, b, prov) {
                    self.fail_infinite(b, a, cid, prov, pol);
                    return false;
                }
                self.link_t(b, a, cid);
                true
            }
            (TDesc::Struct(sa), TDesc::Struct(sb)) => self.unify_struct(sa, a, sb, b, cid, prov, pol),
        }
    }

    /// occurs-check：变量 `v` 是否出现在类型 `in_ty` 中（含展开后的行）。
    fn occurs_t(&mut self, root: TV, v: TV, _prov: &mut BTreeSet<CId>) -> bool {
        let root = self.find_t(root);
        if root == v { return true; }
        match self.desc_t(root).clone() {
            TDesc::Unbound { .. } => false,
            TDesc::Struct(st) => match st {
                TStruct::Int | TStruct::Bool | TStruct::Unit => false,
                TStruct::Arrow(x, y) => self.occurs_t(x, v, _prov) || self.occurs_t(y, v, _prov),
                TStruct::Record(r) | TStruct::Variant(r) => self.occurs_row(r, v, _prov),
            },
        }
    }

    fn occurs_row(&mut self, r: RV, v: TV, prov: &mut BTreeSet<CId>) -> bool {
        let r = self.find_r(r);
        let d = self.r_desc[r.0 as usize].clone();
        for (_, fv) in &d.fields {
            if self.occurs_t(*fv, v, prov) { return true; }
        }
        if d.bound && !d.closed {
            let t = self.find_r(d.tail);
            if t != r { return self.occurs_row(t, v, prov); }
        }
        false
    }

    fn link_t(&mut self, v: TV, target: TV, cid: CId) {
        // 以 target 为根；rank 取较小值，供展示与调试。
        let rv = self.t_rank[v.0 as usize];
        let rt = self.t_rank[target.0 as usize];
        self.t_rank[target.0 as usize] = rv.min(rt);
        self.t_parent[v.0 as usize] = target.0;
        // 记录绑定来源
        self.t_origin[v.0 as usize].introduced = cid;
    }

    fn unify_struct(&mut self, sa: TStruct, ra: TV, sb: TStruct, rb: TV,
                    cid: CId, prov: &mut BTreeSet<CId>, pol: Polarity) -> bool {
        use TStruct::*;
        match (sa, sb) {
            (Int, Int) | (Bool, Bool) | (Unit, Unit) => true,
            (Arrow(a1, a2), Arrow(b1, b2)) => {
                self.unify(a1, b1, pol, cid, prov) && self.unify(a2, b2, pol, cid, prov)
            }
            (Record(x), Record(y)) => self.unify_row(x, y, cid, prov, pol, true),
            (Variant(x), Variant(y)) => self.unify_row(x, y, cid, prov, pol, false),
            _ => {
                self.fail_mismatch(ra, rb, cid, prov, pol);
                false
            }
        }
    }

    // ---------- 行统一 ----------

    fn link_r_unbound(&mut self, v: RV, target: RV) {
        self.r_parent[v.0 as usize] = target.0;
        self.r_origin[v.0 as usize].introduced = self.r_origin[target.0 as usize].introduced;
    }

    fn bind_r(&mut self, v: RV, fields: Vec<(String, TV)>, closed: bool, tail: RV, cid: CId) {
        self.r_desc[v.0 as usize] = RDesc { fields, closed, tail, bound: true };
        self.r_origin[v.0 as usize].introduced = cid;
    }

    fn fresh_r_bound(&mut self, fields: Vec<(String, TV)>, closed: bool, tail: RV, o: Origin) -> RV {
        let r = self.fresh_r(o);
        self.r_desc[r.0 as usize] = RDesc { fields, closed, tail, bound: true };
        r
    }

    fn row_occurs(&mut self, v: RV, fields: &[(String, TV)], prov: &mut BTreeSet<CId>) -> bool {
        for (_, fv) in fields {
            if self.occurs_t(*fv, TV(u32::MAX), &mut BTreeSet::new()) { } // 占位无操作
            // 检查行变量 v 是否出现在字段类型内部（间接经过行构造）
            if self.ty_contains_row(*fv, v, prov) { return true; }
        }
        false
    }

    /// 类型中是否（通过行构造）包含行变量 v。
    fn ty_contains_row(&mut self, t: TV, v: RV, prov: &mut BTreeSet<CId>) -> bool {
        let t = self.find_t(t);
        match self.desc_t(t).clone() {
            TDesc::Unbound { .. } => false,
            TDesc::Struct(s) => {
                prov.insert(self.t_origin[t.0 as usize].introduced);
                match s {
                    TStruct::Int | TStruct::Bool | TStruct::Unit => false,
                    TStruct::Arrow(x, y) => {
                        self.ty_contains_row(x, v, prov) || self.ty_contains_row(y, v, prov)
                    }
                    TStruct::Record(r) | TStruct::Variant(r) => self.row_contains_row(r, v, prov),
                }
            }
        }
    }

    fn row_contains_row(&mut self, r: RV, v: RV, prov: &mut BTreeSet<CId>) -> bool {
        let r = self.find_r(r);
        if r == v { return true; }
        let d = self.r_desc[r.0 as usize].clone();
        prov.insert(self.r_origin[r.0 as usize].introduced);
        for (_, fv) in d.fields {
            if self.ty_contains_row(fv, v, prov) { return true; }
        }
        if d.bound && !d.closed {
            let t = self.find_r(d.tail);
            if t != r && self.row_contains_row(t, v, prov) { return true; }
        }
        false
    }

    fn unify_row(&mut self, r1in: RV, r2in: RV, cid: CId, prov: &mut BTreeSet<CId>,
                 pol: Polarity, _is_record: bool) -> bool {
        let r1 = self.find_r(r1in);
        let r2 = self.find_r(r2in);
        if r1 == r2 { return true; }
        let d1 = self.r_desc[r1.0 as usize].clone();
        let d2 = self.r_desc[r2.0 as usize].clone();
        prov.insert(self.r_origin[r1.0 as usize].introduced);
        prov.insert(self.r_origin[r2.0 as usize].introduced);

        if !d1.bound && !d2.bound {
            self.link_r_unbound(r1, r2);
            return true;
        }

        // 归一化尾部。
        let tail1 = self.norm_tail(&d1, r1);
        let tail2 = self.norm_tail(&d2, r2);

        let mut map1: BTreeMap<String, TV> = d1.fields.into_iter().collect();
        let mut map2: BTreeMap<String, TV> = d2.fields.into_iter().collect();

        // 共有字段：逐一统一。
        let mut field_ok = true;
        let common: Vec<String> = map1.keys().filter(|k| map2.contains_key(*k)).cloned().collect();
        for k in &common {
            let f1 = map1.remove(k).unwrap();
            let f2 = map2.remove(k).unwrap();
            if !self.unify(f1, f2, pol, cid, prov) { field_ok = false; }
        }
        if !field_ok {
            // 字段内部类型不一致：mismatch 已经记录（方向相同）。
            return false;
        }

        let only1: Vec<(String, TV)> = map1.into_iter().collect();
        let only2: Vec<(String, TV)> = map2.into_iter().collect();
        let names1: Vec<String> = only1.iter().map(|(n, _)| n.clone()).collect();
        let names2: Vec<String> = only2.iter().map(|(n, _)| n.clone()).collect();

        // 缺/余字段按极性排列：missing = 期望有而实际没有；extra = 实际有而期望没有。
        let (missing, extra) = match pol {
            Polarity::Equal => (names1.clone(), names2.clone()),
            Polarity::ExpectedActual => (names1.clone(), names2.clone()),
        };

        let mut ok = true;
        match (&tail1, &tail2) {
            (RowTail::Closed, RowTail::Closed) => {
                if !only1.is_empty() || !only2.is_empty() {
                    self.fail_row(cid, prov, missing, extra);
                    ok = false;
                }
            }
            (RowTail::Closed, RowTail::Open(t2)) => {
                if !only2.is_empty() {
                    self.fail_row(cid, prov, missing, extra);
                    ok = false;
                } else if self.row_occurs(*t2, &only1, prov) {
                    self.fail_infinite_row(*t2, cid, prov);
                    ok = false;
                } else {
                    self.bind_r(*t2, only1, true, *t2, cid);
                }
            }
            (RowTail::Open(t1), RowTail::Closed) => {
                if !only1.is_empty() {
                    self.fail_row(cid, prov, missing, extra);
                    ok = false;
                } else if self.row_occurs(*t1, &only2, prov) {
                    self.fail_infinite_row(*t1, cid, prov);
                    ok = false;
                } else {
                    self.bind_r(*t1, only2, true, *t1, cid);
                }
            }
            (RowTail::Open(t1), RowTail::Open(t2)) => {
                if t1 == t2 {
                    // 同尾：多余字段直接冲突
                    if !only1.is_empty() || !only2.is_empty() {
                        self.fail_row(cid, prov, missing, extra);
                        ok = false;
                    }
                } else {
                    let o = Origin { introduced: cid, node: self.r_origin[r1.0 as usize].node };
                    let t3 = self.fresh_r(o);
                    if self.row_occurs(*t1, &only2, prov) {
                        self.fail_infinite_row(*t1, cid, prov); ok = false;
                    } else if self.row_occurs(*t2, &only1, prov) {
                        self.fail_infinite_row(*t2, cid, prov); ok = false;
                    } else {
                        self.bind_r(*t1, only2, false, t3, cid);
                        self.bind_r(*t2, only1, false, t3, cid);
                    }
                }
            }
        }
        ok
    }

    fn norm_tail(&self, d: &RDesc, root: RV) -> RowTail {
        if !d.bound || d.closed {
            if d.bound { RowTail::Closed } else { RowTail::Open(root) }
        } else {
            RowTail::Open(RV(self.r_parent[d.tail.0 as usize]))
        }
    }

    // ---------- 错误构造 ----------

    fn prov_list(prov: &BTreeSet<CId>, cid: CId) -> Vec<CId> {
        let mut v: Vec<CId> = prov.iter().copied().collect();
        if !v.contains(&cid) { v.push(cid); }
        v.sort();
        v
    }

    fn fail_mismatch(&mut self, a: TV, b: TV, cid: CId, prov: &BTreeSet<CId>, pol: Polarity) {
        let (exp, act) = match pol {
            Polarity::Equal => (a, b),
            Polarity::ExpectedActual => (a, b),
        };
        let et = self.freeze_t(exp, &mut Default::default());
        let at = self.freeze_t(act, &mut Default::default());
        let err = TypeError {
            kind: TypeErrorKind::TypeMismatch,
            cid,
            message: format!("类型不匹配：期望 {}，实际 {}", Printer::print(&et),
                             Printer::print(&at)),
            prov: Self::prov_list(prov, cid),
            expected: Some(et),
            actual: Some(at),
            missing: vec![], extra: vec![], var_name: None,
        };
        self.errors.push(err);
    }

    fn fail_infinite(&mut self, v: TV, _root: TV, cid: CId, prov: &BTreeSet<CId>, _pol: Polarity) {
        let ty = self.freeze_t(_root, &mut Default::default());
        let shown = Printer::print(&ty);
        let err = TypeError {
            kind: TypeErrorKind::InfiniteType,
            cid,
            message: format!("发生 occurs-check：类型变量会构造出无限类型（如 t ~ {} → ...）", shown),
            prov: Self::prov_list(prov, cid),
            expected: None, actual: Some(ty),
            missing: vec![], extra: vec![], var_name: None,
        };
        self.errors.push(err);
        let _ = v;
    }

    fn fail_infinite_row(&mut self, rv: RV, cid: CId, prov: &BTreeSet<CId>) {
        let mut names = vec![];
        let mut cur = Some(rv);
        let mut guard = 0;
        while let Some(r) = cur {
            guard += 1;
            if guard > 64 { break; }
            let r = self.find_r(r);
            let d = self.r_desc[r.0 as usize].clone();
            for (n, _) in &d.fields { names.push(n.clone()); }
            cur = if d.bound && !d.closed {
                let t = self.find_r(d.tail);
                if t == r { None } else { Some(t) }
            } else { None };
        }
        names.sort();
        names.dedup();
        self.errors.push(TypeError {
            kind: TypeErrorKind::InfiniteType,
            cid,
            message: format!("行变量发生 occurs-check：字段 {} 会无限重复", names.join(", ")),
            prov: Self::prov_list(prov, cid),
            expected: None, actual: None,
            missing: vec![], extra: vec![], var_name: None,
        });
    }

    fn fail_row(&mut self, cid: CId, prov: &BTreeSet<CId>, missing: Vec<String>, extra: Vec<String>) {
        let mut parts = vec![];
        if !missing.is_empty() { parts.push(format!("缺少字段 {}", missing.join(", "))); }
        if !extra.is_empty() { parts.push(format!("多余字段 {}", extra.join(" ", ))); }
        self.errors.push(TypeError {
            kind: TypeErrorKind::RowMismatch,
            cid,
            message: format!("记录/变体形状不一致：{}", parts.join("；")),
            prov: Self::prov_list(prov, cid),
            expected: None, actual: None,
            missing, extra, var_name: None,
        });
    }

    pub fn fail_unbound(&mut self, name: &str, cid: CId, nid: NodeId) {
        self.errors.push(TypeError {
            kind: TypeErrorKind::UnboundVariable,
            cid,
            message: format!("未绑定的变量 '{}'", name),
            prov: vec![cid],
            expected: None, actual: None,
            missing: vec![], extra: vec![], var_name: Some(name.to_string()),
        });
        let _ = nid;
    }

    pub fn fail_dup_field(&mut self, field: &str, cid: CId) {
        self.errors.push(TypeError {
            kind: TypeErrorKind::DuplicateField,
            cid,
            message: format!("记录中字段 '{}' 重复出现", field),
            prov: vec![cid],
            expected: None, actual: None,
            missing: vec![], extra: vec![], var_name: Some(field.to_string()),
        });
    }
}

#[derive(Clone, Copy)]
enum RowTail { Closed, Open(RV) }

impl Solver {
    // ---------- 局部环境 ----------

    pub fn push_mono(&mut self, name: String, tv: TV) { self.locals.push((name, Local::Mono(tv))); }
    pub fn push_poly(&mut self, name: String, sch: Scheme) { self.locals.push((name, Local::Poly(sch))); }
    pub fn push_rec(&mut self, name: String, tv: TV) {
        let top = self.rec_anchors.last_mut();
        if let Some(v) = top { v.push(tv); }
        self.locals.push((name, Local::Rec(tv)));
    }
    pub fn pop_local(&mut self) { self.locals.pop(); }
    pub fn enter_scope(&mut self) {
        self.rec_anchors.push(vec![]);
        self.bind_stack.push(0);
    }
    pub fn exit_scope(&mut self) {
        let n = self.bind_stack.pop().unwrap_or(0);
        for _ in 0..n { self.locals.pop(); }
        self.rec_anchors.pop();
    }

    fn lookup(&self, name: &str) -> Option<&Local> {
        self.locals.iter().rev().find(|(n, _)| n == name).map(|(_, l)| l)
    }

    // ---------- 自由变量收集 ----------

    fn free_tv(&mut self, t: TV, out: &mut BTreeSet<TV>) {
        let t = self.find_t(t);
        if !out.insert(t) { return; }
        if let TDesc::Struct(s) = self.desc_t(t).clone() {
            match s {
                TStruct::Int | TStruct::Bool | TStruct::Unit => {}
                TStruct::Arrow(a, b) => { self.free_tv(a, out); self.free_tv(b, out); }
                TStruct::Record(r) | TStruct::Variant(r) => self.free_rv_fields(r, out, &mut BTreeSet::new()),
            }
        }
    }

    fn free_rv_fields(&mut self, r: RV, tvs: &mut BTreeSet<TV>, rvs: &mut BTreeSet<RV>) {
        let r = self.find_r(r);
        if !rvs.insert(r) { return; }
        let d = self.r_desc[r.0 as usize].clone();
        for (_, fv) in d.fields { self.free_tv(fv, tvs); }
        if d.bound && !d.closed {
            let t = self.find_r(d.tail);
            if t != r { self.free_rv_fields(t, tvs, rvs); }
        }
    }

    fn env_free_vars(&self) -> (BTreeSet<TV>, BTreeSet<RV>) {
        let mut tvs = BTreeSet::new();
        let rvs = BTreeSet::new();
        for (_, l) in &self.locals {
            match l {
                Local::Mono(tv) => { /* 在外部实时收集，见 env_free */ }
                Local::Rec(tv) => { tvs.insert(*tv); }
                Local::Poly(sch) => {
                    let mut m: BTreeMap<TV, TV> = Default::default();
                    let _ = &mut m;
                    Self::scheme_free(&sch.body, &mut tvs);
                }
            }
        }
        // Mono 直接指向当前并查集：单独一遍
        let mut m2: BTreeMap<u32, ()> = Default::default();
        let _ = &mut m2;
        (tvs, rvs)
    }

    fn scheme_free(t: &RType, out: &mut BTreeSet<TV>) {
        match t {
            RType::Var(v) => { out.insert(*v); }
            RType::Int | RType::Bool | RType::Unit => {}
            RType::Arrow(a, b) => { Self::scheme_free(a, out); Self::scheme_free(b, out); }
            RType::Record(r) | RType::Variant(r) => match r {
                Row::Closed(fs) | Row::Open(fs, _) => for (_, ft) in fs { Self::scheme_free(ft, out); },
            },
        }
    }

    // ---------- 泛化与实例化 ----------

    pub fn enter_level(&mut self) { self.level += 1; }
    pub fn exit_level(&mut self) { self.level = self.level.saturating_sub(1); }

    pub fn freeze_for_generalize(&mut self, tv: TV) -> RType {
        self.freeze_t(tv, &mut Default::default())
    }

    /// 收集环境中单态可见（Mono/Rec 绑定与递归锚点）的自由变量。
    fn env_free(&mut self) -> (BTreeSet<TV>, BTreeSet<RV>) {
        let mut tvs = BTreeSet::new();
        let mut rvs = BTreeSet::new();
        let seeds: Vec<TV> = self.locals.iter().filter_map(|(_, l)| match l {
            Local::Mono(tv) | Local::Rec(tv) => Some(*tv),
            Local::Poly(_) => None,
        }).collect();
        for tv in seeds { self.free_vars_at(tv, &mut tvs, &mut rvs); }
        let anchors: Vec<TV> = self.rec_anchors.iter().flatten().copied().collect();
        for tv in anchors { self.free_vars_at(tv, &mut tvs, &mut rvs); }
        (tvs, rvs)
    }

    /// 泛化：body 自由变量减去环境自由变量。
    /// value restriction：非值绑定若存在可泛化变量，则整体保持单态。
    pub fn generalize(&mut self, tv: TV, is_value: bool, _cid: CId) -> Scheme {
        let mut body_t = BTreeSet::new();
        let mut body_r = BTreeSet::new();
        self.free_vars_at(tv, &mut body_t, &mut body_r);
        let (env_t, env_r) = self.env_free();
        let gen_t: Vec<TV> = body_t.difference(&env_t).copied().collect();
        let gen_r: Vec<RV> = body_r.difference(&env_r).copied().collect();
        let body_frozen = self.freeze_t(tv, &mut Default::default());
        if !is_value && !(gen_t.is_empty() && gen_r.is_empty()) {
            Scheme { bvars: vec![], rbvars: vec![], body: body_frozen }
        } else {
            Scheme { bvars: gen_t, rbvars: gen_r, body: body_frozen }
        }
    }

    fn free_vars_at(&mut self, t: TV, tvs: &mut BTreeSet<TV>, rvs: &mut BTreeSet<RV>) {
        let t = self.find_t(t);
        if !tvs.insert(t) { return; }
        if let TDesc::Struct(st) = self.t_desc[t.0 as usize].clone() {
            match st {
                TStruct::Int | TStruct::Bool | TStruct::Unit => {}
                TStruct::Arrow(a, b) => { self.free_vars_at(a, tvs, rvs); self.free_vars_at(b, tvs, rvs); }
                TStruct::Record(r) | TStruct::Variant(r) => self.row_free(r, tvs, rvs),
            }
        }
    }

    fn row_free(&mut self, r: RV, tvs: &mut BTreeSet<TV>, rvs: &mut BTreeSet<RV>) {
        let r = self.find_r(r);
        if !rvs.insert(r) { return; }
        let d = self.r_desc[r.0 as usize].clone();
        for (_, fv) in d.fields { self.free_vars_at(fv, tvs, rvs); }
        if d.bound && !d.closed {
            let t = self.find_r(d.tail);
            if t != r {
                // 只沿“被绑定”的尾链继续；末端未绑定的存在性开口不参与泛化。
                let td_bound = self.r_desc[t.0 as usize].bound;
                if td_bound { self.row_free(t, tvs, rvs); }
            }
        }
    }

    fn r_rank_of(&mut self, rv: RV) -> u32 {
        let rv = self.find_r(rv);
        *self.r_rank.get(rv.0 as usize).unwrap_or(&0)
    }

    /// 实例化方案：为每个绑定变量创建新的新鲜变量。
    pub fn instantiate(&mut self, sch: &Scheme, o: Origin) -> TV {
        let mut tmap: BTreeMap<TV, TV> = BTreeMap::new();
        let mut rmap: BTreeMap<RV, RV> = BTreeMap::new();
        for gv in &sch.bvars { tmap.insert(*gv, self.fresh_t(0, o)); }
        for gr in &sch.rbvars { rmap.insert(*gr, self.fresh_r(o)); }
        self.inst_type(&sch.body, &tmap, &rmap, o)
    }

    fn inst_type(&mut self, t: &RType, tmap: &BTreeMap<TV, TV>, rmap: &BTreeMap<RV, RV>, o: Origin) -> TV {
        match t {
            RType::Var(v) => tmap.get(v).copied().unwrap_or_else(|| self.fresh_t(0, o)),
            RType::Int => self.mk_struct(TStruct::Int, o),
            RType::Bool => self.mk_struct(TStruct::Bool, o),
            RType::Unit => self.mk_struct(TStruct::Unit, o),
            RType::Arrow(a, b) => {
                let x = self.inst_type(a, tmap, rmap, o);
                let y = self.inst_type(b, tmap, rmap, o);
                self.mk_struct(TStruct::Arrow(x, y), o)
            }
            RType::Record(r) | RType::Variant(r) => {
                let rv = self.inst_row(r, tmap, rmap, o);
                let s = if matches!(t, RType::Record(_)) { TStruct::Record(rv) } else { TStruct::Variant(rv) };
                self.mk_struct(s, o)
            }
        }
    }

    fn inst_row(&mut self, r: &Row, tmap: &BTreeMap<TV, TV>, rmap: &BTreeMap<RV, RV>, o: Origin) -> RV {
        let (fields, closed, tail_src) = match r {
            Row::Closed(fs) => (fs, true, None),
            Row::Open(fs, rv) => (fs, false, Some(*rv)),
        };
        let mut new_fields = vec![];
        for (n, ft) in fields {
            new_fields.push((n.clone(), self.inst_type(ft, tmap, rmap, o)));
        }
        let tail = if closed { RV(u32::MAX) } else {
            let src = tail_src.unwrap();
            rmap.get(&src).copied().unwrap_or_else(|| self.fresh_r(o))
        };
        self.fresh_r_bound(new_fields, closed, tail, o)
    }

    fn mk_struct(&mut self, s: TStruct, o: Origin) -> TV {
        let tv = self.fresh_t(0, o);
        self.t_desc[tv.0 as usize] = TDesc::Struct(s);
        tv
    }

    // ---------- 冻结与展示 ----------

    pub fn freeze(&mut self, t: TV) -> RType {
        self.freeze_t(t, &mut Default::default())
    }

    pub fn freeze_t(&mut self, t: TV, _tn: &mut BTreeMap<TV, String>) -> RType {
        let t = self.find_t(t);
        match self.t_desc[t.0 as usize].clone() {
            TDesc::Unbound { .. } => RType::Var(t),
            TDesc::Struct(st) => match st {
                TStruct::Int => RType::Int,
                TStruct::Bool => RType::Bool,
                TStruct::Unit => RType::Unit,
                TStruct::Arrow(a, b) => RType::Arrow(
                    Box::new(self.freeze_t(a, &mut Default::default())),
                    Box::new(self.freeze_t(b, &mut Default::default())),
                ),
                TStruct::Record(r) => RType::Record(self.freeze_r(r)),
                TStruct::Variant(r) => RType::Variant(self.freeze_r(r)),
            },
        }
    }

    fn freeze_r(&mut self, r: RV) -> Row {
        let mut fields: BTreeMap<String, RType> = BTreeMap::new();
        let mut tail_closed = true;
        let mut tail_var = None;
        let mut cur = Some(r);
        let mut seen = BTreeSet::new();
        let mut guard = 0u32;
        while let Some(rv) = cur {
            guard += 1;
            if guard > 256 { break; }
            let rv = self.find_r(rv);
            if !seen.insert(rv) { break; }
            let d = self.r_desc[rv.0 as usize].clone();
            for (n, fv) in d.fields {
                fields.entry(n).or_insert_with(|| self.freeze_t(fv, &mut Default::default()));
            }
            if d.bound {
                if d.closed { cur = None; tail_closed = true; }
                else {
                    let t = self.find_r(d.tail);
                    if t == rv { tail_var = Some(t); tail_closed = false; cur = None; }
                    else { cur = Some(t); }
                }
            } else {
                tail_var = Some(rv);
                tail_closed = false;
                cur = None;
            }
        }
        if tail_closed { Row::Closed(fields) } else { Row::Open(fields, tail_var.unwrap_or(r)) }
    }

    pub fn show(&mut self, t: RType) -> String { Printer::print(&t) }

    pub fn show_scheme(&mut self, sch: &Scheme) -> String {
        if sch.bvars.is_empty() && sch.rbvars.is_empty() {
            return Printer::print(&sch.body);
        }
        let mut p2 = Printer::new();
        for (i, v) in sch.bvars.iter().enumerate() {
            p2.tn.insert(*v, format!("t{}", i));
        }
        p2.ti = sch.bvars.len() as u32;
        for (i, v) in sch.rbvars.iter().enumerate() {
            p2.rn.insert(*v, format!("r{}", i));
        }
        p2.ri = sch.rbvars.len() as u32;
        let body = p2.ty(&sch.body);
        let qs: Vec<String> = sch.bvars.iter().map(|v| p2.tn[v].clone())
            .chain(sch.rbvars.iter().map(|v| p2.rn[v].clone())).collect();
        format!("∀{}. {}", qs.join(" "), body)
    }

    pub fn pub_show(&self, t: &RType) -> String { Printer::print(t) }

    pub fn find_t_pub(&self, t: TV) -> TV {
        let mut cur = t;
        loop {
            let p = self.t_parent[cur.0 as usize];
            if p == cur.0 { return cur; }
            cur = TV(p);
        }
    }
    pub fn origin_of(&self, t: TV) -> Origin { self.t_origin[t.0 as usize] }
}

/// 确定性类型打印器：变量按首次出现命名 t0、t1…，行变量 r0、r1…。
struct Printer {
    tn: BTreeMap<TV, String>,
    rn: BTreeMap<RV, String>,
    ti: u32,
    ri: u32,
}

impl Printer {
    fn new() -> Self { Printer { tn: BTreeMap::new(), rn: BTreeMap::new(), ti: 0, ri: 0 } }
    fn print(t: &RType) -> String {
        let mut p = Printer::new();
        p.ty(t)
    }
    fn tv(&mut self, v: TV) -> String {
        if let Some(s) = self.tn.get(&v) { return s.clone(); }
        let s = format!("t{}", self.ti);
        self.ti += 1;
        self.tn.insert(v, s.clone());
        s
    }
    fn rv(&mut self, v: RV) -> String {
        if let Some(s) = self.rn.get(&v) { return s.clone(); }
        let s = format!("r{}", self.ri);
        self.ri += 1;
        self.rn.insert(v, s.clone());
        s
    }
    fn ty(&mut self, t: &RType) -> String {
        match t {
            RType::Var(v) => self.tv(*v),
            RType::Int => "int".into(),
            RType::Bool => "bool".into(),
            RType::Unit => "unit".into(),
            RType::Arrow(a, b) => {
                let sa = self.ty(a);
                let sa = if matches!(**a, RType::Arrow(_,_)) { format!("({})", sa) } else { sa };
                format!("{} -> {}", sa, self.ty(b))
            }
            RType::Record(r) => {
                let mut parts: Vec<String> = match r {
                    Row::Closed(fs) => fs.iter().map(|(k,v)| format!("{}: {}", k, self.ty(v))).collect(),
                    Row::Open(fs, rv) => {
                        let mut ps: Vec<String> = fs.iter().map(|(k,v)| format!("{}: {}", k, self.ty(v))).collect();
                        ps.push(self.rv(*rv));
                        ps
                    }
                };
                let tail = if let Row::Open(_, rv) = r { Some(self.rv(*rv)) } else { None };
                let _ = &mut parts;
                format!("{{{}}}", parts.join(", "))
            }
            RType::Variant(r) => {
                let (fs, tail) = match r {
                    Row::Closed(fs) => (fs, None),
                    Row::Open(fs, rv) => (fs, Some(self.rv(*rv))),
                };
                let mut parts: Vec<String> = fs.iter().map(|(k,v)| {
                    let sv = self.ty(v);
                    if sv == "unit" { format!("`{}", k) } else { format!("`{}({})", k, sv) }
                }).collect();
                if let Some(t) = tail { parts.push(t); }
                format!("[{}]", parts.join(" | "))
            }
        }
    }
}

// ---------- 约束执行（由 analyze 模块调用） ----------

use crate::constraints::{BindKind, Con, Prim};

/// 执行期每个节点的类型变量表。
pub struct NodeTypes {
    pub map: Vec<Option<TV>>,
    pub rows: Vec<Option<RV>>,
}

impl Solver {
    pub fn node_table(node_count: usize) -> NodeTypes {
        NodeTypes { map: vec![None; node_count], rows: vec![None; node_count] }
    }

    pub fn ensure_t(&mut self, nt: &mut NodeTypes, n: NodeId, cid: CId) -> TV {
        let i = n.0 as usize;
        if let Some(t) = nt.map.get(i).copied().flatten() { return t; }
        let t = self.fresh_at_level(Origin { introduced: cid, node: n });
        nt.map[i] = Some(t);
        t
    }

    pub fn ensure_r(&mut self, nt: &mut NodeTypes, n: NodeId, cid: CId) -> RV {
        let i = n.0 as usize;
        if let Some(r) = nt.rows.get(i).copied().flatten() { return r; }
        let r = self.fresh_r(Origin { introduced: cid, node: n });
        nt.rows[i] = Some(r);
        r
    }

    pub fn run_constraints(&mut self, cons: &[crate::constraints::Constraint], nt: &mut NodeTypes) {
        // 顶层词法层级：let 绑定进入更深层级，使 rank 泛化可用。
        self.enter_level();
        self.enter_scope();
        for c in cons {
            if self.disabled.contains(&c.id) {
                self.steps.push(Step::Skip { cid: c.id, label: c.label.clone() });
                continue;
            }
            self.run_one(&c.con, c.id, nt);
        }
    }

    fn origin(&self, cid: CId, n: NodeId) -> Origin { Origin { introduced: cid, node: n } }

    fn run_one(&mut self, con: &Con, cid: CId, nt: &mut NodeTypes) {
        match con {
            Con::ExpectPrim { node, prim, span: _ } => {
                let t = self.ensure_t(nt, *node, cid);
                let want = match prim {
                    Prim::Int => TStruct::Int, Prim::Bool => TStruct::Bool, Prim::Unit => TStruct::Unit,
                };
                let want_tv = self.mk_struct(want, self.origin(cid, *node));
                let before = self.freeze_t(t, &mut Default::default());
                let b = self.show(before.clone());
                let mut prov = BTreeSet::new();
                let ok = self.unify(t, want_tv, Polarity::ExpectedActual, cid, &mut prov);
                let after = self.freeze_t(t, &mut Default::default());
                let result_s = Printer::print(&after);
                let prim_s = match prim { Prim::Int => "int", Prim::Bool => "bool", Prim::Unit => "unit" };
                self.steps.push(Step::Unify { cid, left: prim_s.to_string(), right: b,
                    result: result_s, ok,
                    detail: format!("约束节点类型为 {}", prim_s) });
            }
            Con::MkArrow { node, arg, ret, span: _ } => {
                let t = self.ensure_t(nt, *node, cid);
                let a = self.ensure_t(nt, *arg, cid);
                let r = self.ensure_t(nt, *ret, cid);
                let arrow = self.mk_struct(TStruct::Arrow(a, r), self.origin(cid, *node));
                let mut prov = BTreeSet::new();
                let before = self.freeze_t(t, &mut Default::default());
                let bs = self.show(before.clone());
                let ar = RType::Arrow(
                    Box::new(self.freeze_t(a, &mut Default::default())),
                    Box::new(self.freeze_t(r, &mut Default::default())));
                let ars = self.show(ar.clone());
                let ok = self.unify(t, arrow, Polarity::Equal, cid, &mut prov);
                let after = self.freeze_t(t, &mut Default::default());
                let result_s = Printer::print(&after);
                self.steps.push(Step::Unify { cid, left: ars, right: bs,
                    result: result_s, ok, detail: "构造/匹配函数类型".into() });
            }
            Con::Eq { a, b, pol, label, span: _ } => {
                let ta = self.ensure_t(nt, *a, cid);
                let tb = self.ensure_t(nt, *b, cid);
                let fa = self.freeze_t(ta, &mut Default::default());
                let fb = self.freeze_t(tb, &mut Default::default());
                let sa = self.show(fa.clone());
                let sb = self.show(fb.clone());
                let mut prov = BTreeSet::new();
                let ok = self.unify(ta, tb, *pol, cid, &mut prov);
                let after = self.freeze_t(ta, &mut Default::default());
                let result_s = Printer::print(&after);
                self.steps.push(Step::Unify { cid, left: sa, right: sb,
                    result: result_s, ok, detail: label.clone() });
            }

            Con::MkRecord { node, fields, closed, span: _ } => {
                let t = self.ensure_t(nt, *node, cid);
                // node 可能已被约束为开放/封闭记录；这里直接对“该节点类型 = 这些字段的记录行”做统一。
                let mut fs: Vec<(String, TV)> = vec![];
                for (name, vn, _) in fields {
                    let fv = self.ensure_t(nt, *vn, cid);
                    fs.push((name.clone(), fv));
                }
                let tail = self.fresh_r(self.origin(cid, *node));
                let rv = self.fresh_r_bound(fs, *closed, tail, self.origin(cid, *node));
                let rec_tv = self.mk_struct(TStruct::Record(rv), self.origin(cid, *node));
                let want = self.freeze_t(rec_tv, &mut Default::default());
                let have = self.freeze_t(t, &mut Default::default());
                let ws = self.show(want.clone());
                let hs = self.show(have.clone());
                let mut prov = BTreeSet::new();
                let ok = self.unify(t, rec_tv, Polarity::ExpectedActual, cid, &mut prov);
                let after = self.freeze_t(t, &mut Default::default());
                let result_s = Printer::print(&after);
                self.steps.push(Step::Unify { cid, left: ws, right: hs,
                    result: result_s, ok,
                    detail: if *closed { "构造封闭记录".into() } else { "按字段投影（开放记录）".into() } });
            }

            Con::MkVariant { node, label, payload, open, span: _ } => {
                let t = self.ensure_t(nt, *node, cid);
                // 无载荷：payload=unit
                let pv = if payload.0 == u32::MAX {
                    self.mk_struct(TStruct::Unit, self.origin(cid, *node))
                } else {
                    self.ensure_t(nt, *payload, cid)
                };
                let tail = self.fresh_r(self.origin(cid, *node));
                let rv = self.fresh_r_bound(vec![(label.clone(), pv)], !*open, tail,
                    self.origin(cid, *node));
                let var_tv = self.mk_struct(TStruct::Variant(rv), self.origin(cid, *node));
                let want = self.freeze_t(var_tv, &mut Default::default());
                let have = self.freeze_t(t, &mut Default::default());
                let ws = self.show(want.clone());
                let hs = self.show(have.clone());
                let mut prov = BTreeSet::new();
                let ok = self.unify(t, var_tv, Polarity::Equal, cid, &mut prov);
                let after = self.freeze_t(t, &mut Default::default());
                let result_s = Printer::print(&after);
                self.steps.push(Step::Unify { cid, left: ws, right: hs,
                    result: result_s, ok,
                    detail: format!("变体标签 `{}", label) });
            }

            Con::VarUse { node, name, span: _ } => {
                let t = self.ensure_t(nt, *node, cid);
                match self.lookup(name).cloned() {
                    Some(Local::Poly(sch)) => {
                        let ss = self.show_scheme(&sch);
                        let inst = self.instantiate(&sch, self.origin(cid, *node));
                        let it = self.freeze_t(inst, &mut Default::default());
                        let its = self.show(it.clone());
                        let mut prov = BTreeSet::new();
                        let ok = self.unify(t, inst, Polarity::Equal, cid, &mut prov);
                        self.steps.push(Step::Instantiate { cid, name: name.clone(),
                            scheme: ss, instance: its });
                        let _ = ok;
                    }
                    Some(Local::Mono(bt)) | Some(Local::Rec(bt)) => {
                        let bt = self.find_t(bt);
                        let mut prov = BTreeSet::new();
                        let _ = self.unify(t, bt, Polarity::Equal, cid, &mut prov);
                        let after = self.freeze_t(t, &mut Default::default());
                        let result_s = Printer::print(&after);
                        self.steps.push(Step::Unify { cid, left: name.clone(), right: "单态别名".into(),
                            result: result_s, ok: true, detail: format!("引用单态绑定 {}", name) });
                    }
                    None => {
                        self.fail_unbound(name, cid, *node);
                        self.steps.push(Step::Fail { cid, message: format!("未绑定变量 '{}'", name) });
                    }
                }
            }

            Con::ScopeBegin { level_up, span: _ } => {
                if *level_up { self.enter_level(); }
                self.enter_scope();
                self.steps.push(Step::Scope { cid, open: true, label: String::new() });
            }
            Con::BindLocal { name, node, kind, span: _ } => {
                let tv = self.ensure_t(nt, *node, cid);
                match kind {
                    BindKind::LamParam | BindKind::ArmPayload => self.push_mono(name.clone(), tv),
                    BindKind::LetPoly | BindKind::LetMono | BindKind::Rec => {}
                }
                if let Some(top) = self.bind_stack.last_mut() { *top += 1; }
                self.steps.push(Step::Scope { cid, open: true, label: name.clone() });
            }
            Con::ScopeEnd { level_down, span: _ } => {
                self.exit_scope();
                if *level_down { self.exit_level(); }
                self.steps.push(Step::Scope { cid, open: false, label: String::new() });
            }

            Con::GenBind { name, value, is_value, span: _ } => {
                let vt = self.ensure_t(nt, *value, cid);
                let sch = self.generalize(vt, *is_value, cid);
                let ss = self.show_scheme(&sch);
                let nvars = sch.bvars.len();
                let nrvars = sch.rbvars.len();
                let restricted = nvars == 0 && nrvars == 0 && !*is_value;
                self.push_poly(name.clone(), sch);
                self.steps.push(Step::Generalize { cid, name: name.clone(), scheme: ss,
                    vars: nvars, rvars: nrvars, restricted });
            }

            Con::RecBegin { name, anchor, span: _ } => {
                // 递归锚点在当前层级；其内部的递归使用与锚点同一类型变量。
                let tv = self.ensure_t(nt, *anchor, cid);
                self.push_rec(name.clone(), tv);
                let ty = self.freeze_t(tv, &mut Default::default());
                let tys = Printer::print(&ty);
                self.steps.push(Step::RecBind { cid, name: name.clone(), ty: tys });
            }
            Con::RecEnd { name, anchor, is_value, span: _ } => {
                let tv = self.ensure_t(nt, *anchor, cid);
                // 递归锚点仍在环境中：generalize 计算 env 差集时会排除锚点自身，
                // 但锚点上的其它自由变量可被泛化。为实现多态递归，这里采用
                // “临时移出锚点 → 泛化 → 放回方案”。
                self.remove_binding(name);
                let sch = self.generalize(tv, *is_value, cid);
                let ss = self.show_scheme(&sch);
                let nvars = sch.bvars.len();
                let nrvars = sch.rbvars.len();
                self.push_poly(name.clone(), sch);
                self.steps.push(Step::Generalize { cid, name: name.clone(), scheme: ss,
                    vars: nvars, rvars: nrvars, restricted: false });
            }
            Con::Fail { message, span: _, node: _ } => {
                self.errors.push(TypeError {
                    kind: TypeErrorKind::ArityShape, cid, message: message.clone(),
                    prov: vec![cid], expected: None, actual: None,
                    missing: vec![], extra: vec![], var_name: None,
                });
                self.steps.push(Step::Fail { cid, message: message.clone() });
            }
        }
    }

    fn remove_binding(&mut self, name: &str) {
        if let Some(pos) = self.locals.iter().rposition(|(n, _)| n == name) {
            self.locals.remove(pos);
        }
    }

    fn replace_top_with_poly(&mut self, name: &str, sch: Scheme) {
        if let Some((n, _)) = self.locals.last() {
            if n == name {
                self.locals.last_mut().unwrap().1 = Local::Poly(sch);
                return;
            }
        }
        // 兜底：在栈顶附近找同名 Rec 绑定
        for i in (0..self.locals.len()).rev() {
            if self.locals[i].0 == name && matches!(self.locals[i].1, Local::Rec(_)) {
                self.locals[i].1 = Local::Poly(sch);
                return;
            }
        }
    }
}
