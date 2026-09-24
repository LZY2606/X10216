//! 求解器：按约束顺序执行实例化、泛化与统一；
//! 每条边记录“证明约束”，任何错误都可回溯到保持不可满足的小核心。

use crate::ast::{NodeId, Span};
use crate::constraints::*;
use crate::types::*;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, PartialEq)]
pub enum RowErrorKind {
    Missing,
    Extra,
    Mismatch,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RowFieldError {
    pub kind: RowErrorKind,
    pub label: String,
    /// 字段两端类型（用于展示）
    pub left: u32,
    pub right: u32,
    /// 解释文字
    pub detail: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnifyFailure {
    Mismatch {
        expected: String,
        actual: String,
    },
    Infinite {
        var_desc: String,
        in_ty: String,
    },
    Record {
        fields: Vec<RowFieldError>,
    },
    Variant {
        tags: Vec<RowFieldError>,
    },
}

#[derive(Clone, Debug)]
pub struct Failure {
    pub failure: UnifyFailure,
    pub proof: BTreeSet<Cid>,
    pub cid: Cid,
    pub span: Span,
    pub note: String,
}

#[derive(Clone, Debug)]
pub enum StepDetail {
    Unify { a: u32, b: u32, ok: bool },
    Instantiate { name: String, fresh_vars: Vec<u32> },
    Generalize { name: String, quantified: Vec<u32>, value_restricted: bool },
    Primitive { name: String },
    Unresolved { name: String },
    Disabled,
}

#[derive(Clone, Debug)]
pub struct Step {
    pub seq: u32,
    pub cid: Cid,
    pub detail: StepDetail,
    pub ok: bool,
    /// 每一步之后，该约束涉及类型的最终描述
    pub after: String,
}

#[derive(Clone, Debug)]
pub struct SolvedTy {
    pub name: String,
    pub node_id: String,
    pub ty: String,
    pub quantified: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct ErrorReport {
    pub cid: Cid,
    pub node_id: String,
    pub span: Span,
    pub message: String,
    pub detail: String,
    pub core: Vec<Cid>,
    pub cores: Vec<Vec<Cid>>,
}

pub struct SolveOutcome {
    pub steps: Vec<Step>,
    pub errors: Vec<ErrorReport>,
    pub types: Vec<SolvedTy>,
    pub schemes: BTreeMap<String, crate::constraints::SchemeTy>,
}

/// 工作态类型表：union-find 边带“证明约束 id”。
struct Work {
    store: TypeStore,
    /// 与生成态 id 的位置对应（0..gen_len 同序）。
    remap: Vec<u32>,
    ranks: Vec<u32>,
    proof_edge: Vec<Option<Cid>>,
}

impl Work {
    fn new(gen: &TypeStore, rank0: u32) -> Self {
        let mut store = TypeStore::new();
        let mut remap = vec![0u32; gen.nodes.len()];
        // 必须按生成顺序重建以保持身份对应；origin 照搬
        for (i, n) in gen.nodes.iter().enumerate() {
            let id = match n.origin {
                VarOrigin::Generalized(_) | VarOrigin::RowTail(_) => store.fresh(n.origin),
                VarOrigin::Node { .. } => store.fresh(n.origin),
            };
            remap[i] = id;
        }
        let ranks = vec![rank0; store.nodes.len()];
        Work { store, ranks, proof_edge: vec![None; store.nodes.len()], remap }
    }

    fn w(&self, t: u32) -> u32 {
        self.remap[t as usize]
    }

    fn find_path(&self, mut t: u32, mut acc: &mut Vec<Cid>) -> u32 {
        let mut chain = Vec::new();
        loop {
            match self.store.bind[t as usize] {
                Some(p) => {
                    chain.push(t);
                    t = p;
                }
                None => break,
            }
        }
        for node in chain {
            if let Some(c) = &self.proof_edge[node as usize] {
                acc.extend(decode_set(c));
            }
        }
        t
    }

    fn find(&self, t: u32) -> u32 {
        self.find_path(t, &mut Vec::new())
    }

    fn proof_of(&self, t: u32) -> BTreeSet<Cid> {
        let mut v = Vec::new();
        self.find_path(t, &mut v);
        v.into_iter().collect()
    }

    /// 把根 a 指向根 b（roots 已求），边证明 = 两侧证明 + cid。
    fn link_roots(&mut self, a: u32, b: u32, cid: &Cid, extra: BTreeSet<Cid>) {
        // 按 rank 合并；把边挂到被合并的根上
        let (child, parent) = if self.ranks[a as usize] > self.ranks[b as usize] {
            (b, a)
        } else if self.ranks[a as usize] < self.ranks[b as usize] {
            (a, b)
        } else {
            self.ranks[b as usize] += 1;
            (a, b)
        };
        // child 的证明 = child 路径 + parent 路径 + cid + extra
        let mut e = self.proof_of(child);
        e.extend(self.proof_of(parent));
        e.insert(cid.clone());
        e.extend(extra);
        self.store.bind[child as usize] = Some(parent);
        // proof_edge 只存一个 cid 不足以合并集合：用 `+` 语法集编码
        self.proof_edge[child as usize] = Some(encode_set(&e));
        self.ranks[parent as usize] = self.ranks[parent as usize].max(self.ranks[child as usize]);
    }
}

fn encode_set(set: &BTreeSet<Cid>) -> Cid {
    set.iter().cloned().collect::<Vec<_>>().join("+")
}
fn decode_set(s: &str) -> BTreeSet<Cid> {
    s.split('+').filter(|x| !x.is_empty()).map(|x| x.to_string()).collect()
}

struct GeneralizeResult {
    detail: StepDetail,
}

struct Solver<'a> {
    gen: &'a CGen,
    disabled: &'a BTreeSet<Cid>,
    work: Work,
    schemes: BTreeMap<NodeId, (String, SchemeTy)>,
    steps: Vec<Step>,
    failures: Vec<Failure>,
    seq: u32,
}

fn prim_scheme(name: &str) -> Option<SchemeTy> {
    use SchemeTy::*;
    let i = || Int;
    let b = || Bool;
    let s = || Str;
    let f = |a: SchemeTy, b: SchemeTy| Fun(Box::new(a), Box::new(b));
    let f3 = |a: SchemeTy, b: SchemeTy, c: SchemeTy| f(a, f(b, c));
    match name {
        "int_add" | "int_sub" | "int_mul" => Some(f3(i(), i(), i())),
        "int_eq" | "int_lt" => Some(f3(i(), i(), b())),
        "bool_and" | "bool_or" => Some(f3(b(), b(), b())),
        "bool_not" => Some(f(b(), b())),
        "str_eq" => Some(f3(s(), s(), b())),
        "str_cat" => Some(f3(s(), s(), s())),
        _ => None,
    }
}

impl<'a> Solver<'a> {
    fn run(&mut self) {
        for c in &self.gen.cons {
            self.process(c);
        }
    }

    fn push_step(&mut self, c: &Constraint, detail: StepDetail, ok: bool, after: String) {
        let seq = self.seq;
        self.seq += 1;
        self.steps.push(Step { seq, cid: c.id.clone(), detail, ok, after });
    }

    fn process(&mut self, c: &Constraint) {
        if self.disabled.contains(&c.id) {
            self.push_step(c, StepDetail::Disabled, true, "（该约束被临时禁用，求解器跳过）".to_string());
            return;
        }
        match &c.kind {
            CKind::Primitive { name, target } => {
                let scheme = prim_scheme(name).expect("内置方案存在");
                let fresh = self.instantiate_scheme(&scheme, c);
                let ok = self.unify(self.work.w(*target), fresh, c);
                self.push_step(c, StepDetail::Primitive { name: name.clone() }, ok.is_ok(), c.note.clone());
                if let Err(f) = ok.err() {
                    self.failures.push(f);
                }
            }
            CKind::Instantiate { name, target, bound_at } => {
                let mut fresh_vars = Vec::new();
                let result = match self.schemes.get(bound_at) {
                    Some((_, scheme)) => {
                        let fresh = self.instantiate_scheme_tracked(scheme, c, &mut fresh_vars);
                        self.unify(self.work.w(*target), fresh, c)
                    }
                    None => Ok(()),
                };
                self.push_step(
                    c,
                    StepDetail::Instantiate { name: name.clone(), fresh_vars },
                    result.is_ok(),
                    c.note.clone(),
                );
                if let Err(f) = result.err() {
                    self.failures.push(f);
                }
            }
            CKind::Generalize { name, bound_at, value_ty, syntactic_value, scope_level: _ } => {
                let result = self.generalize(name, bound_at, self.work.w(*value_ty), *syntactic_value, c);
                self.push_step(
                    c,
                    result.detail,
                    true,
                    c.note.clone(),
                );
            }
            CKind::Unify { a, b, .. } => {
                let result = self.unify(self.work.w(*a), self.work.w(*b), c);
                let after = self.describe(self.work.w(*a));
                self.push_step(c, StepDetail::Unify { a: self.work.w(*a), b: self.work.w(*b), ok: result.is_ok() }, result.is_ok(), after);
                if let Err(f) = result.err() {
                    self.failures.push(f);
                }
            }
            CKind::Unresolved { name } => {
                let proof = BTreeSet::from([c.id.clone()]);
                self.push_step(c, StepDetail::Unresolved { name: name.clone() }, false, c.note.clone());
                self.failures.push(Failure {
                    failure: UnifyFailure::Mismatch {
                        expected: format!("一个已绑定的名字"),
                        actual: format!("未定义名字 `{}`", name),
                    },
                    proof,
                    cid: c.id.clone(),
                    span: c.span,
                    note: c.note.clone(),
                });
            }
        }
    }

    /// 收集可达的工作态类型变量（未绑定），用于确定量词。
    fn reachable_vars(&self, root: u32, out: &mut BTreeSet<u32>) {
        let mut stack = vec![root];
        while let Some(t) = stack.pop() {
            let r = self.work.find(t);
            if !out.insert(r) {
                continue;
            }
            match self.work.store.nodes[r as usize].value.clone() {
                Type_::TVar(_) => {}
                Type_::TInt | Type_::TBool | Type_::TStr => {}
                Type_::TFun(a, b) => {
                    stack.push(a);
                    stack.push(b);
                }
                Type_::Record(fields, tail) => {
                    for (_, ft) in fields {
                        stack.push(ft);
                    }
                    if let Some(Var(rv)) = tail {
                        stack.push(rv);
                    }
                }
                Type_::Variant(tags, tail) => {
                    for (_, pt) in tags {
                        if let Some(pt) = pt {
                            stack.push(pt);
                        }
                    }
                    if let Some(Var(rv)) = tail {
                        stack.push(rv);
                    }
                }
            }
        }
    }

    fn generalize(
        &mut self,
        name: &str,
        bound_at: &str,
        root: u32,
        syntactic_value: bool,
        c: &Constraint,
    ) -> GeneralizeResult {
        let _ = c;
        let mut reachable = BTreeSet::new();
        self.reachable_vars(root, &mut reachable);
        let value_restricted = !syntactic_value;
        // value restriction：不产生量词，所有自由变量都按 Ambient（回指自身）保存
        let mut quant: BTreeMap<u32, u32> = BTreeMap::new();
        let mut quantified = Vec::new();
        if !value_restricted {
            for (idx, v) in reachable.iter().enumerate() {
                quant.insert(*v, idx as u32);
                quantified.push(*v);
            }
        }
        let scheme = self.build_scheme(root, &quant);
        self.schemes.insert(bound_at.to_string(), (name.to_string(), scheme));
        GeneralizeResult {
            detail: StepDetail::Generalize {
                name: name.to_string(),
                quantified,
                value_restricted,
            },
        }
    }

    fn build_scheme(&self, t: u32, quant: &BTreeMap<u32, u32>) -> SchemeTy {
        let r = self.work.find(t);
        if let Some(idx) = quant.get(&r) {
            return SchemeTy::SchemeVar(*idx);
        }
        match self.work.store.nodes[r as usize].value.clone() {
            Type_::TVar(_) => SchemeTy::Ambient(r),
            Type_::TInt => SchemeTy::Int,
            Type_::TBool => SchemeTy::Bool,
            Type_::TStr => SchemeTy::Str,
            Type_::TFun(a, b) => SchemeTy::Fun(
                Box::new(self.build_scheme(a, quant)),
                Box::new(self.build_scheme(b, quant)),
            ),
            Type_::Record(fields, tail) => {
                let fs = fields.into_iter().map(|(k, v)| (k, self.build_scheme(v, quant))).collect();
                let tail = tail.map(|Var(rv)| self.build_scheme(rv, quant));
                SchemeTy::Record(fs, tail.map(Box::new))
            }
            Type_::Variant(tags, tail) => {
                let ts = tags
                    .into_iter()
                    .map(|(k, v)| (k, v.map(|v| self.build_scheme(v, quant))))
                    .collect();
                let tail = tail.map(|Var(rv)| self.build_scheme(rv, quant));
                SchemeTy::Variant(ts, tail.map(Box::new))
            }
        }
    }

    fn instantiate_scheme(&mut self, scheme: &SchemeTy, c: &Constraint) -> u32 {
        let mut tracked = Vec::new();
        self.instantiate_scheme_tracked(scheme, c, &mut tracked)
    }

    fn instantiate_scheme_tracked(
        &mut self,
        scheme: &SchemeTy,
        c: &Constraint,
        tracked: &mut Vec<u32>,
    ) -> u32 {
        let _ = c;
        let mut mapping: BTreeMap<u32, u32> = BTreeMap::new();
        self.inst_rec(scheme, tracked, &mut mapping)
    }

    fn inst_rec(
        &mut self,
        scheme: &SchemeTy,
        tracked: &mut Vec<u32>,
        mapping: &mut BTreeMap<u32, u32>,
    ) -> u32 {
        match scheme {
            SchemeTy::Int => self.work.store.make(Type_::TInt, VarOrigin::Node { node_id_id: u32::MAX }),
            SchemeTy::Bool => self.work.store.make(Type_::TBool, VarOrigin::Node { node_id_id: u32::MAX }),
            SchemeTy::Str => self.work.store.make(Type_::TStr, VarOrigin::Node { node_id_id: u32::MAX }),
            SchemeTy::Fun(a, b) => {
                let a2 = self.inst_rec(a, tracked, mapping);
                let b2 = self.inst_rec(b, tracked, mapping);
                self.work.store.make(Type_::TFun(a2, b2), VarOrigin::Node { node_id_id: u32::MAX })
            }
            SchemeTy::Record(fields, tail) => {
                let fs = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.inst_rec(v, tracked, mapping)))
                    .collect();
                let tail = tail.as_deref().map(|v| self.inst_rec(v, tracked, mapping));
                self.work.store.make(Type_::Record(fs, tail.map(Var)), VarOrigin::Node { node_id_id: u32::MAX })
            }
            SchemeTy::Variant(tags, tail) => {
                let ts = tags
                    .iter()
                    .map(|(k, v)| (k.clone(), v.as_ref().map(|v| self.inst_rec(v, tracked, mapping))))
                    .collect();
                let tail = tail.as_deref().map(|v| self.inst_rec(v, tracked, mapping));
                self.work.store.make(Type_::Variant(ts, tail.map(Var)), VarOrigin::Node { node_id_id: u32::MAX })
            }
            SchemeTy::SchemeVar(key) => {
                if let Some(v) = mapping.get(key) {
                    *v
                } else {
                    let v = self.work.store.fresh(VarOrigin::Node { node_id_id: u32::MAX });
                    mapping.insert(*key, v);
                    tracked.push(v);
                    v
                }
            }
            SchemeTy::Ambient(gid) => *gid,
        }
    }


    fn proof_union(&self, a: u32, b: u32, cid: &Cid) -> BTreeSet<Cid> {
        let mut p = self.work.proof_of(a);
        p.extend(self.work.proof_of(b));
        p.insert(cid.clone());
        p
    }

    fn unify(&mut self, a: u32, b: u32, c: &Constraint) -> Result<(), Failure> {
        let ra = self.work.find(a);
        let rb = self.work.find(b);
        if ra == rb {
            return Ok(());
        }
        let va = self.work.store.nodes[ra as usize].value.clone();
        let vb = self.work.store.nodes[rb as usize].value.clone();
        match (va.clone(), vb.clone()) {
            (Type_::TVar(_), _) => {
                if self.work.store.occurs(ra, rb) {
                    return Err(self.infinite_failure(ra, rb, c));
                }
                let extra = self.proof_union(ra, rb, &c.id);
                self.work.link_roots(ra, rb, &c.id, extra);
                Ok(())
            }
            (_, Type_::TVar(_)) => {
                if self.work.store.occurs(rb, ra) {
                    return Err(self.infinite_failure(rb, ra, c));
                }
                let extra = self.proof_union(ra, rb, &c.id);
                self.work.link_roots(rb, ra, &c.id, extra);
                Ok(())
            }
            (Type_::TInt, Type_::TInt)
            | (Type_::TBool, Type_::TBool)
            | (Type_::TStr, Type_::TStr) => Ok(()),
            (Type_::TFun(a1, a2), Type_::TFun(b1, b2)) => {
                // 先把两树根连一条“形状一致”边，再统一子节点，保证证明传播
                let extra = self.proof_union(ra, rb, &c.id);
                self.work.link_roots(ra, rb, &c.id, extra);
                self.unify(a1, b1, c)?;
                self.unify(a2, b2, c)?;
                Ok(())
            }
            (Type_::Record(fa, ta), Type_::Record(fb, tb)) => {
                let errors = self.unify_row(&fa, &fb, ta, tb, c, true)?;
                let extra = self.proof_union(ra, rb, &c.id);
                self.work.link_roots(ra, rb, &c.id, extra);
                let _ = errors;
                Ok(())
            }
            (Type_::Variant(fa, ta), Type_::Variant(fb, tb)) => {
                let errors = self.unify_row_variant(&fa, &fb, ta, tb, c)?;
                let extra = self.proof_union(ra, rb, &c.id);
                self.work.link_roots(ra, rb, &c.id, extra);
                let _ = errors;
                Ok(())
            }
            _ => Err(self.mismatch_failure(ra, rb, &va, &vb, c)),
        }
    }

    fn infinite_failure(&self, var: u32, in_ty: u32, c: &Constraint) -> Failure {
        let var_desc = self.describe(var);
        let in_desc = self.describe(in_ty);
        let mut proof = self.proof_union(var, in_ty, &c.id);
        proof.insert(c.id.clone());
        Failure {
            failure: UnifyFailure::Infinite { var_desc, in_ty: in_desc },
            proof,
            cid: c.id.clone(),
            span: c.span,
            note: c.note.clone(),
        }
    }

    fn mismatch_failure(
        &self,
        ra: u32,
        rb: u32,
        va: &Type_,
        vb: &Type_,
        c: &Constraint,
    ) -> Failure {
        let expected = describe_type(va, self);
        let actual = describe_type(vb, self);
        let mut proof = self.proof_union(ra, rb, &c.id);
        proof.insert(c.id.clone());
        Failure {
            failure: UnifyFailure::Mismatch { expected, actual },
            proof,
            cid: c.id.clone(),
            span: c.span,
            note: c.note.clone(),
        }
    }

    /// 行统一（记录），标准吸收算法：
    /// 公共字段统一；仅一侧存在的字段由对侧“开放行尾”吸收；
    /// 只有行尾闭合（无空位）时才分别记为缺字段 / 多字段。
    fn unify_row(
        &mut self,
        fa: &[(String, u32)],
        fb: &[(String, u32)],
        ta: Option<Var>,
        tb: Option<Var>,
        c: &Constraint,
        _is_record: bool,
    ) -> Result<(), Failure> {
        let map_b: BTreeMap<&String, u32> = fb.iter().map(|(k, v)| (k, *v)).collect();
        let map_a: BTreeMap<&String, u32> = fa.iter().map(|(k, v)| (k, *v)).collect();
        let mut mismatch: Vec<RowFieldError> = Vec::new();

        for (label, &la) in fa {
            if let Some(&lb) = map_b.get(label) {
                if self.unify(la, lb, c).is_err() {
                    mismatch.push(RowFieldError {
                        kind: RowErrorKind::Mismatch,
                        label: label.clone(),
                        left: la,
                        right: lb,
                        detail: format!("字段 `{}` 的类型冲突", label),
                    });
                }
            }
        }
        let only_a: Vec<(String, u32)> =
            fa.iter().filter(|(k, _)| !map_b.contains_key(k)).cloned().collect();
        let only_b: Vec<(String, u32)> =
            fb.iter().filter(|(k, _)| !map_a.contains_key(k)).cloned().collect();

        let mut missing: Vec<RowFieldError> = Vec::new();
        let mut extra: Vec<RowFieldError> = Vec::new();

        // 决定 only_a 字段由右侧行尾吸收；only_b 由左侧行尾吸收。
        match (ta, tb) {
            (Some(Var(a_tail)), Some(Var(b_tail))) => {
                let fresh = self.work.store.fresh(VarOrigin::Node { node_id_id: u32::MAX });
                let row_b = self
                    .work
                    .store
                    .make(Type_::Record(only_a.clone(), Some(Var(fresh))), VarOrigin::Node { node_id_id: u32::MAX });
                let row_a = self
                    .work
                    .store
                    .make(Type_::Record(only_b.clone(), Some(Var(fresh))), VarOrigin::Node { node_id_id: u32::MAX });
                self.unify(b_tail, row_b, c)?;
                self.unify(a_tail, row_a, c)?;
            }
            (None, Some(Var(b_tail))) => {
                // 左闭合：右只能补上 only_a；only_b 无位置 -> 右行多余标签
                for (label, ft) in &only_b {
                    extra.push(RowFieldError {
                        kind: RowErrorKind::Extra,
                        label: label.clone(),
                        left: *ft,
                        right: *ft,
                        detail: format!("多余字段 `{}`：另一侧行已闭合", label),
                    });
                }
                let row_b = self.work.store.make(
                    Type_::Record(only_a.clone(), None),
                    VarOrigin::Node { node_id_id: u32::MAX },
                );
                self.unify(b_tail, row_b, c)?;
            }
            (Some(Var(a_tail)), None) => {
                for (label, ft) in &only_a {
                    extra.push(RowFieldError {
                        kind: RowErrorKind::Extra,
                        label: label.clone(),
                        left: *ft,
                        right: *ft,
                        detail: format!("多余字段 `{}`：另一侧行已闭合", label),
                    });
                }
                let row_a = self.work.store.make(
                    Type_::Record(only_b.clone(), None),
                    VarOrigin::Node { node_id_id: u32::MAX },
                );
                self.unify(a_tail, row_a, c)?;
            }
            (None, None) => {
                for (label, ft) in &only_a {
                    extra.push(RowFieldError {
                        kind: RowErrorKind::Extra,
                        label: label.clone(),
                        left: *ft,
                        right: *ft,
                        detail: format!("多余字段 `{}`", label),
                    });
                }
                for (label, ft) in &only_b {
                    missing.push(RowFieldError {
                        kind: RowErrorKind::Missing,
                        label: label.clone(),
                        left: *ft,
                        right: *ft,
                        detail: format!("缺少字段 `{}`", label),
                    });
                }
            }
        }

        let mut fields = missing;
        fields.extend(extra);
        fields.extend(mismatch);
        if !fields.is_empty() {
            let proof = BTreeSet::from([c.id.clone()]);
            return Err(Failure {
                failure: UnifyFailure::Record { fields },
                proof,
                cid: c.id.clone(),
                span: c.span,
                note: c.note.clone(),
            });
        }
        Ok(())
    }

    fn unify_row_variant(
        &mut self,
        fa: &[(String, Option<u32>)],
        fb: &[(String, Option<u32>)],
        ta: Option<Var>,
        tb: Option<Var>,
        c: &Constraint,
    ) -> Result<(), Failure> {
        let map_b: BTreeMap<&String, Option<u32>> = fb.iter().map(|(k, v)| (k, *v)).collect();
        let map_a: BTreeMap<&String, Option<u32>> = fa.iter().map(|(k, v)| (k, *v)).collect();
        let mut mismatch: Vec<RowFieldError> = Vec::new();

        for (label, pa) in fa {
            if let Some(pb) = map_b.get(label) {
                match (pa, pb) {
                    (Some(x), Some(y)) => {
                        if self.unify(*x, *y, c).is_err() {
                            mismatch.push(RowFieldError {
                                kind: RowErrorKind::Mismatch,
                                label: label.clone(),
                                left: *x,
                                right: *y,
                                detail: format!("变体 `{}` 载荷类型冲突", label),
                            });
                        }
                    }
                    (None, Some(_)) | (Some(_), None) => mismatch.push(RowFieldError {
                        kind: RowErrorKind::Mismatch,
                        label: label.clone(),
                        left: pa.unwrap_or(0),
                        right: pb.unwrap_or(0),
                        detail: format!("变体 `{}` 一边有载荷、一边无载荷", label),
                    }),
                    (None, None) => {}
                }
            }
        }
        let only_a: Vec<(String, Option<u32>)> =
            fa.iter().filter(|(k, _)| !map_b.contains_key(k)).cloned().collect();
        let only_b: Vec<(String, Option<u32>)> =
            fb.iter().filter(|(k, _)| !map_a.contains_key(k)).cloned().collect();

        let mut errors = Vec::new();
        match (ta, tb) {
            (Some(Var(a_tail)), Some(Var(b_tail))) => {
                let fresh = self.work.store.fresh(VarOrigin::Node { node_id_id: u32::MAX });
                let row_b = self.work.store.make(
                    Type_::Variant(only_a.clone(), Some(Var(fresh))),
                    VarOrigin::Node { node_id_id: u32::MAX },
                );
                let row_a = self.work.store.make(
                    Type_::Variant(only_b.clone(), Some(Var(fresh))),
                    VarOrigin::Node { node_id_id: u32::MAX },
                );
                self.unify(b_tail, row_b, c)?;
                self.unify(a_tail, row_a, c)?;
            }
            (None, Some(_)) => {
                for (label, pa) in &only_b {
                    errors.push(RowFieldError {
                        kind: RowErrorKind::Extra,
                        label: label.clone(),
                        left: pa.unwrap_or(0),
                        right: pa.unwrap_or(0),
                        detail: format!("多余变体标签 `{}`（另一侧行已闭合）", label),
                    });
                }
            }
            (Some(_), None) => {
                for (label, pa) in &only_a {
                    errors.push(RowFieldError {
                        kind: RowErrorKind::Extra,
                        label: label.clone(),
                        left: pa.unwrap_or(0),
                        right: pa.unwrap_or(0),
                        detail: format!("多余变体标签 `{}`（另一侧行已闭合）", label),
                    });
                }
            }
            (None, None) => {
                for (label, pa) in &only_a {
                    errors.push(RowFieldError {
                        kind: RowErrorKind::Extra,
                        label: label.clone(),
                        left: pa.unwrap_or(0),
                        right: pa.unwrap_or(0),
                        detail: format!("多余变体标签 `{}`", label),
                    });
                }
                for (label, pb) in &only_b {
                    errors.push(RowFieldError {
                        kind: RowErrorKind::Missing,
                        label: label.clone(),
                        left: pb.unwrap_or(0),
                        right: pb.unwrap_or(0),
                        detail: format!("缺少变体标签 `{}`", label),
                    });
                }
            }
        }
        errors.extend(mismatch);
        if !errors.is_empty() {
            let proof = BTreeSet::from([c.id.clone()]);
            return Err(Failure {
                failure: UnifyFailure::Variant { tags: errors },
                proof,
                cid: c.id.clone(),
                span: c.span,
                note: c.note.clone(),
            });
        }
        Ok(())
    }


    fn describe(&self, t: u32) -> String {
        let r = self.work.find(t);
        let value = self.work.store.nodes[r as usize].value.clone();
        describe_type(&value, self)
    }
}

struct Namer {
    map: BTreeMap<u32, char>,
    next: u8,
}
impl Namer {
    fn new() -> Self {
        Namer { map: BTreeMap::new(), next: b'a' }
    }
    fn name(&mut self, id: u32) -> char {
        if let Some(c) = self.map.get(&id) {
            *c
        } else {
            let c = (self.next) as char;
            self.next += 1;
            *self.map.entry(id).or_insert(c)
        }
    }
}

trait Describer {
    fn find_root(&self, t: u32) -> u32;
    fn value_of(&self, r: u32) -> Type_;
}
impl<'a> Describer for Solver<'a> {
    fn find_root(&self, t: u32) -> u32 {
        self.work.find(t)
    }
    fn value_of(&self, r: u32) -> Type_ {
        self.work.store.nodes[r as usize].value.clone()
    }
}

fn describe_type<D: Describer>(v: &Type_, d: &D) -> String {
    let mut namer = Namer::new();
    render(v, d, &mut namer, false)
}

fn render<D: Describer>(v: &Type_, d: &D, n: &mut Namer, paren: bool) -> String {
    match v {
        Type_::TVar(Var(id)) => {
            let r = d.find_root(*id);
            match d.value_of(r) {
                Type_::TVar(_) => format!("'{}", n.name(r)),
                other => render(&other, d, n, paren),
            }
        }
        Type_::TInt => "Int".to_string(),
        Type_::TBool => "Bool".to_string(),
        Type_::TStr => "Str".to_string(),
        Type_::TFun(a, b) => {
            let inner = format!("{} -> {}", render(&d.value_of(d.find_root(a)), d, n, true), render(&d.value_of(d.find_root(b)), d, n, false));
            if paren {
                format!("({})", inner)
            } else {
                inner
            }
        }
        Type_::Record(fields, tail) => {
            let mut parts: Vec<String> = fields
                .iter()
                .map(|(k, ft)| format!("{}: {}", k, render(&d.value_of(d.find_root(*ft)), d, n, false)))
                .collect();
            if let Some(Var(rv)) = tail {
                let rr = d.find_root(rv);
                if let Type_::TVar(_) = d.value_of(rr) {
                    parts.push(format!("..'{}", n.name(rr)));
                }
            }
            format!("{{ {} }}", parts.join(", "))
        }
        Type_::Variant(tags, tail) => {
            let mut parts: Vec<String> = tags
                .iter()
                .map(|(k, pt)| match pt {
                    Some(t) => format!("{}({})", k, render(&d.value_of(d.find_root(*t)), d, n, false)),
                    None => k.clone(),
                })
                .collect();
            if let Some(Var(rv)) = tail {
                let rr = d.find_root(rv);
                if let Type_::TVar(_) = d.value_of(rr) {
                    parts.push(format!("..'{}", n.name(rr)));
                }
            }
            format!("[ {} ]", parts.join(" | "))
        }
    }
}

fn failure_message(f: &Failure) -> (String, String) {
    match &f.failure {
        UnifyFailure::Mismatch { expected, actual } => (
            format!("类型不匹配：期望 {}，实际 {}", expected, actual),
            format!("该约束要求两边相等，但结构构造子不同。期望: {}，实际: {}", expected, actual),
        ),
        UnifyFailure::Infinite { var_desc, in_ty } => (
            format!("无限类型：{} 出现在 {} 中", var_desc, in_ty),
            "occurs-check 失败：统一会构造出无限展开的递归类型".to_string(),
        ),
        UnifyFailure::Record { fields } => {
            let mut missing: Vec<&String> = Vec::new();
            let mut extra: Vec<&String> = Vec::new();
            let mut mismatch: Vec<&String> = Vec::new();
            for e in fields {
                match e.kind {
                    RowErrorKind::Missing => missing.push(&e.label),
                    RowErrorKind::Extra => extra.push(&e.label),
                    RowErrorKind::Mismatch => mismatch.push(&e.label),
                }
            }
            let mut parts = Vec::new();
            if !missing.is_empty() {
                parts.push(format!("缺少字段 {}", missing.iter().cloned().collect::<Vec<_>>().join(", ")));
            }
            if !extra.is_empty() {
                parts.push(format!("多余字段 {}", extra.iter().cloned().collect::<Vec<_>>().join(", ")));
            }
            if !mismatch.is_empty() {
                parts.push(format!("字段类型冲突 {}", mismatch.iter().cloned().collect::<Vec<_>>().join(", ")));
            }
            (format!("记录行不兼容：{}", parts.join("；")), fields.iter().map(|e| e.detail.clone()).collect::<Vec<_>>().join("；"))
        }
        UnifyFailure::Variant { tags } => {
            let mut missing: Vec<&String> = Vec::new();
            let mut extra: Vec<&String> = Vec::new();
            let mut mismatch: Vec<&String> = Vec::new();
            for e in tags {
                match e.kind {
                    RowErrorKind::Missing => missing.push(&e.label),
                    RowErrorKind::Extra => extra.push(&e.label),
                    RowErrorKind::Mismatch => mismatch.push(&e.label),
                }
            }
            let mut parts = Vec::new();
            if !missing.is_empty() {
                parts.push(format!("缺少标签 {}", missing.iter().cloned().collect::<Vec<_>>().join(", ")));
            }
            if !extra.is_empty() {
                parts.push(format!("多余标签 {}", extra.iter().cloned().collect::<Vec<_>>().join(", ")));
            }
            if !mismatch.is_empty() {
                parts.push(format!("载荷类型冲突 {}", mismatch.iter().cloned().collect::<Vec<_>>().join(", ")));
            }
            (format!("变体行不兼容：{}", parts.join("；")), tags.iter().map(|e| e.detail.clone()).collect::<Vec<_>>().join("；"))
        }
    }
}

/// 仅在给定约束集合（按原顺序）上跑一遍，返回失败列表。
pub fn solve_subset(gen: &CGen, active: &BTreeSet<Cid>, disabled: &BTreeSet<Cid>) -> Vec<Failure> {
    let mut solver = Solver {
        gen,
        disabled,
        work: Work::new(&gen.store, 1),
        schemes: BTreeMap::new(),
        steps: Vec::new(),
        failures: Vec::new(),
        seq: 0,
    };
    for c in &gen.cons {
        if active.contains(&c.id) {
            solver.process(c);
        }
    }
    solver.failures
}

/// 为每个失败寻找极小不可满足核心（保留多个同样小的候选）。
fn compute_cores(gen: &CGen, failures: &[Failure]) -> Vec<Vec<Vec<Cid>>> {
    let all: BTreeSet<Cid> = gen.cons.iter().map(|c| c.id.clone()).collect();
    failures
        .iter()
        .map(|f| {
            let target = f.cid.clone();
            // 初始候选：证明集合 ∪ 触发约束
            let mut seed = f.proof.clone();
            seed.insert(target.clone());
            // 贪心删除最小化（对 seed 做）：得到一个极小核
            let minimize = |seed: &BTreeSet<Cid>| -> BTreeSet<Cid> {
                let mut core = seed.clone();
                loop {
                    let mut changed = false;
                    let candidates: Vec<Cid> = core.iter().cloned().collect();
                    for cand in candidates {
                        if cand == target {
                            continue;
                        }
                        let mut trial = core.clone();
                        trial.remove(&cand);
                        if !solve_subset(gen, &trial, &BTreeSet::new()).is_empty() {
                            // 仍然不可满足 => 可删
                            core.remove(&cand);
                            changed = true;
                        }
                    }
                    if !changed {
                        break;
                    }
                }
                core
            };
            let core = minimize(&seed);
            // 尝试从不同初始集合寻找同样大小的其他候选（证明的超集扰动）
            let mut candidates: BTreeSet<Vec<Cid>> = BTreeSet::new();
            let size = core.len();
            candidates.insert(core.iter().cloned().collect());
            // 用“全集中逐步删除”的另一方向求第二个候选
            let mut from_all = all.clone();
            let order: Vec<Cid> = gen.cons.iter().rev().map(|c| c.id.clone()).collect();
            for cand in order {
                if cand == target {
                    continue;
                }
                let mut trial = from_all.clone();
                trial.remove(&cand);
                if !solve_subset(gen, &trial, &BTreeSet::new()).is_empty() {
                    from_all.remove(&cand);
                }
            }
            if from_all.len() == size {
                candidates.insert(from_all.into_iter().collect());
            }
            candidates.into_iter().collect()
        })
        .collect()
}


pub fn solve(gen: &CGen, disabled: &BTreeSet<Cid>) -> SolveOutcome {
    let mut solver = Solver {
        gen,
        disabled,
        work: Work::new(&gen.store, 1),
        schemes: BTreeMap::new(),
        steps: Vec::new(),
        failures: Vec::new(),
        seq: 0,
    };
    solver.run();
    let failures = solver.failures.clone();

    // 类型结果（禁用约束时也能看到漂移后的推断）
    let mut types = Vec::new();
    for (node_id, gt) in &gen.node_tys {
        let wt = solver.work.w(*gt);
        types.push(SolvedTy {
            name: node_id.clone(),
            node_id: node_id.clone(),
            ty: solver.describe(wt),
            quantified: Vec::new(),
        });
    }
    for (decl_id, gt) in &gen.decl_tys {
        if decl_id == "result" {
            continue;
        }
        let wt = solver.work.w(*gt);
        types.push(SolvedTy {
            name: format!("decl:{}", decl_id),
            node_id: decl_id.clone(),
            ty: solver.describe(wt),
            quantified: Vec::new(),
        });
    }

    let cores = compute_cores(gen, &failures);
    let errors = failures
        .iter()
        .zip(cores.into_iter())
        .map(|(f, cores)| {
            let (message, detail) = failure_message(f);
            let c = gen.cons.iter().find(|c| c.id == f.cid).cloned();
            ErrorReport {
                cid: f.cid.clone(),
                node_id: c.as_ref().map(|c| c.node_id.clone()).unwrap_or_default(),
                span: f.span,
                message,
                detail,
                core: cores.get(0).cloned().unwrap_or_default(),
                cores,
            }
        })
        .collect();

    // 方案快照（供展示）
    let mut schemes = BTreeMap::new();
    for (k, (name, st)) in &solver.schemes {
        schemes.insert(k.clone(), st.clone());
        let _ = name;
    }

    SolveOutcome { steps: solver.steps, errors, types, schemes }
}
