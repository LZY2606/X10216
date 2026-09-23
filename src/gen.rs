use crate::ast::*;
use crate::constraint::{Action, Cid, Origin};
use crate::span::{Label, Span};
use crate::types::{SField, SRow, SType};
use std::collections::{BTreeMap, BTreeSet, HashMap};

const FNV_OFFSET: u64 = 0xcbf29ce484222325;
const FNV_PRIME: u64 = 0x100000001b3;

fn mix(acc: &mut u64, bytes: &[u8]) {
    for &x in bytes {
        *acc ^= x as u64;
        *acc = acc.wrapping_mul(FNV_PRIME);
    }
}
fn mix_u64(h: &mut u64, x: u64) { mix(h, &x.to_le_bytes()); }
fn mix_str(h: &mut u64, s: &str) { mix(h, &[0xFF]); mix(h, s.as_bytes()); }

fn hash_ty(h: &mut u64, t: &SType) {
    match t {
        SType::V(k) => { mix(h, b"V"); mix_str(h, k); }
        SType::Con(n) => { mix(h, b"C"); mix_str(h, n); }
        SType::Fun(a, b) => { mix(h, b"F"); hash_ty(h, a); hash_ty(h, b); }
        SType::Record(fs, r) => {
            mix(h, b"R");
            for f in fs { mix_str(h, &f.label); hash_ty(h, &f.ty); }
            hash_row(h, r);
        }
        SType::Variant(fs, r) => {
            mix(h, b"X");
            for f in fs { mix_str(h, &f.label); hash_ty(h, &f.ty); }
            hash_row(h, r);
        }
    }
}
fn hash_row(h: &mut u64, r: &SRow) {
    match r {
        SRow::Closed => mix(h, b"cl"),
        SRow::Open(k) => { mix(h, b"op"); mix_str(h, k); }
    }
}

#[derive(Clone)]
struct Entry {
    def_node: NodeId,
    /// semantic type of the definition value
    body: SType,
    /// static outer-ambient keys
    ambient: BTreeSet<String>,
    expansive: bool,
    recursive: bool,
    name: String,
}

impl Entry {
    fn fp(&self) -> u64 {
        let mut h = FNV_OFFSET;
        mix_u64(&mut h, self.def_node);
        hash_ty(&mut h, &self.body);
        let mut amb: Vec<&str> = self.ambient.iter().map(|s| s.as_str()).collect();
        amb.sort();
        for a in amb { mix_str(&mut h, a); }
        mix(&mut h, if self.expansive { b"exp" } else { b"val" });
        h
    }
}

pub struct Generator<'a> {
    src_len: usize,
    actions: Vec<Action>,
    scope: u64,
    env: BTreeMap<String, Entry>,
    binders: BTreeSet<String>,
    _src: &'a str,
}

pub fn node_var(n: NodeId) -> String { format!("v:{}", n) }
pub fn tail_var(n: NodeId, kind: &str) -> String { format!("r:{}:{}", kind, n) }

/// An expression is "expansive" under the value restriction unless it is a
/// syntactic value: literals, lambda, record/variant of values, annotated value.
fn is_value(e: &Expr) -> bool {
    match &e.data {
        ExprData::Lit(..) | ExprData::Lam { .. } => true,
        ExprData::Record { fields, .. } => fields.iter().all(|f| is_value(&f.value)),
        ExprData::Extend { value, rest, .. } => is_value(value) && is_value(rest),
        ExprData::Variant { payload, .. } => payload.as_ref().map_or(true, |p| is_value(p)),
        ExprData::Ann { expr, .. } => is_value(expr),
        _ => false,
    }
}

impl<'a> Generator<'a> {
    pub fn new(src: &'a str) -> Self {
        Generator {
            src_len: src.len(),
            actions: Vec::new(),
            scope: FNV_OFFSET,
            env: BTreeMap::new(),
            binders: BTreeSet::new(),
            _src: src,
        }
    }

    pub fn generate(mut self, prog: &Program) -> Vec<Action> {
        let _ = self.gen(&prog.expr);
        // node-type declarations last; they enumerate every node for the graph
        let mut nts = Vec::new();
        self.collect_nodes(&prog.expr, &mut nts);
        for (n, t) in nts {
            let cid = self.cid_global("nt", n, &t);
            self.actions.push(Action::NodeType { cid, node: n, ty: t });
        }
        self.actions
    }

    fn span_of(&self, n: NodeId) -> Span { Span::DUMMY }

    fn env_fp(&self) -> u64 {
        let mut h = FNV_OFFSET;
        for (name, e) in &self.env {
            mix_str(&mut h, name);
            mix_u64(&mut h, e.fp());
        }
        h
    }

    fn cid(&self, tag: &str, node: Option<NodeId>, node2: Option<NodeId>,
           extra: impl FnOnce(&mut u64)) -> Cid {
        let mut h = self.scope;
        mix(&mut h, tag.as_bytes());
        if let Some(n) = node { mix_u64(&mut h, n); }
        if let Some(n) = node2 { mix_u64(&mut h, n | 0x8000_0000_0000_0000); }
        mix_u64(&mut h, self.env_fp());
        extra(&mut h);
        h
    }
    fn cid_global(&self, tag: &str, node: NodeId, t: &SType) -> Cid {
        let mut h = FNV_OFFSET;
        mix(&mut h, tag.as_bytes());
        mix_u64(&mut h, node);
        hash_ty(&mut h, t);
        h
    }

    fn push_eq(&mut self, expected: SType, actual: SType, origin: Origin, reason: String) {
        let cid = self.cid("eq", origin.node, origin.node2, |h| {
            hash_ty(h, &expected);
            hash_ty(h, &actual);
        });
        self.actions.push(Action::Eq { cid, expected, actual, origin, reason });
    }

    fn fresh_nv(&self, n: NodeId) -> SType { SType::V(node_var(n)) }

    fn collect_nodes(&self, e: &Expr, out: &mut Vec<(NodeId, SType)>) {
        match &e.data {
            ExprData::Lit(..) | ExprData::Var(_) => {}
            ExprData::Lam { body, .. } => self.collect_nodes(body, out),
            ExprData::App { fun, arg } => { self.collect_nodes(fun, out); self.collect_nodes(arg, out); }
            ExprData::Let(b) => { self.collect_nodes(&b.value, out); self.collect_nodes(&b.body, out); }
            ExprData::Record { fields, .. } => for f in fields { self.collect_nodes(&f.value, out); },
            ExprData::Proj { record, .. } => self.collect_nodes(record, out),
            ExprData::Extend { value, rest, .. } => { self.collect_nodes(value, out); self.collect_nodes(rest, out); }
            ExprData::Variant { payload, .. } => if let Some(p) = payload { self.collect_nodes(p, out); },
            ExprData::Match { scrut, arms } => {
                self.collect_nodes(scrut, out);
                for a in arms { self.collect_nodes(&a.body, out); }
            }
            ExprData::Ann { expr, .. } => self.collect_nodes(expr, out),
            ExprData::If { cond, then_, else_ } => {
                self.collect_nodes(cond, out); self.collect_nodes(then_, out); self.collect_nodes(else_, out);
            }
            ExprData::Bin { lhs, rhs, .. } => { self.collect_nodes(lhs, out); self.collect_nodes(rhs, out); }
        }
        out.push((e.id, self.fresh_nv(e.id)));
    }
}

pub fn builtin_scheme(name: &str) -> Option<SType> {
    let int = || SType::Con("int".into());
    let boolean = || SType::Con("bool".into());
    Some(match name {
        "+" | "-" => SType::fun(int(), SType::fun(int(), int())),
        "=" | "<" => SType::fun(int(), SType::fun(int(), boolean())),
        _ => return None,
    })
}

fn builtin_fp(name: &str) -> u64 {
    let mut h = FNV_OFFSET;
    mix_str(&mut h, "#builtin");
    mix_str(&mut h, name);
    if let Some(t) = builtin_scheme(name) { hash_ty(&mut h, &t); }
    h
}

impl<'a> Generator<'a> {
    fn gen(&mut self, e: &Expr) -> SType {
        let nvar = self.fresh_nv(e.id);
        match &e.data {
            ExprData::Lit(n, lit) => {
                let con = match lit { Lit::Int => "int", Lit::Bool => "bool" };
                let _ = n;
                self.push_eq(SType::Con(con.into()), nvar.clone(),
                    Origin::new(e.span, Some(e.id), "字面量"),
                    format!("字面量 {}", n));
            }
            ExprData::Var(name) => {
                if let Some(scheme) = builtin_scheme(name) {
                    let cid = self.cid("inst", Some(e.id), None, |h| mix_str(h, name));
                    let fresh = instantiate_scheme_keys(&scheme, &format!("bi:{:x}", cid));
                    self.actions.push(Action::Instantiate {
                        cid, scheme_name: name.clone(), target: nvar.clone(),
                        builtin: Some(fresh),
                        origin: Origin::new(e.span, Some(e.id), "内建多态值"),
                        reason: format!("内建 {} 的一次新实例", name),
                    });
                } else if let Some(entry) = self.env.get(name).cloned() {
                    let cid = self.cid("inst", Some(e.id), Some(entry.def_node), |h| mix_str(h, &entry.name));
                    self.actions.push(Action::Instantiate {
                        cid, scheme_name: name.clone(),
                        target: nvar.clone(), builtin: None,
                        origin: Origin::new(e.span, Some(e.id), format!("变量 {}", name)),
                        reason: format!("以名字 {} 引用 let 绑定（必要时多态实例化）", name),
                    });
                } else {
                    let cid = self.cid("ub", Some(e.id), None, |h| mix_str(h, name));
                    self.actions.push(Action::Unbound {
                        cid, name: name.clone(),
                        origin: Origin::new(e.span, Some(e.id), "未绑定变量"),
                    });
                }
            }
            ExprData::Lam { param, body, param_span } => {
                let saved_scope = self.scope;
                let saved_env = self.env.clone();
                let saved_binders = self.binders.clone();
                mix_u64(&mut self.scope, e.id);
                let pkey = format!("p:lam:{}", e.id);
                self.binders.insert(pkey.clone());
                self.env.insert(param.clone(), Entry {
                    def_node: e.id, body: SType::V(pkey.clone()),
                    ambient: BTreeSet::new(), expansive: false, recursive: false,
                    name: param.clone(),
                });
                let bty = self.gen(body);
                let funty = SType::fun(SType::V(pkey.clone()), bty);
                let _ = param_span;
                self.push_eq(funty, nvar.clone(),
                    Origin::new(e.span, Some(e.id), "函数"),
                    format!("函数 \\{} -> ...", param));
                self.scope = saved_scope;
                self.env = saved_env;
                self.binders = saved_binders;
            }
            ExprData::App { fun, arg } => {
                let fty = self.gen(fun);
                let aty = self.gen(arg);
                let reskey = format!("w:{}:appresult", e.id);
                self.binders.insert(reskey.clone());
                let funty = SType::fun(aty, SType::V(reskey.clone()));
                self.push_eq(funty, fty,
                    Origin::new(e.span, Some(e.id), "函数应用"),
                    "函数类型必须是 参数类型 -> 结果类型".into());
                self.push_eq(SType::V(reskey), nvar.clone(),
                    Origin::new(e.span, Some(e.id), "应用结果"),
                    "应用结果类型".into());
            }
            ExprData::Let(b) => {
                let saved_scope = self.scope;
                let saved_env = self.env.clone();
                let saved_binders = self.binders.clone();
                mix_u64(&mut self.scope, b.value.id);
                if !b.rec {
                    let vty = self.gen(&b.value);
                    if let Some(ann) = &b.ann {
                        let sty = self.ast_type(ann, e.id);
                        self.push_eq(sty, vty.clone(),
                            Origin::new(ann.span(), Some(e.id), "类型标注"),
                            format!("let {} 的标注", b.name));
                    }
                    let ambient: Vec<String> = self.binders.iter().cloned().collect();
                    let cid = self.cid("gen", Some(e.id), Some(b.value.id), |h| mix_str(h, &b.name));
                    self.actions.push(Action::Generalize {
                        cid, let_node: e.id, name: b.name.clone(),
                        body_node: b.value.id, ambient,
                        recursive: false,
                        origin: Origin::new(b.name_span, Some(e.id), format!("泛化 {}", b.name)),
                    });
                    self.env.insert(b.name.clone(), Entry {
                        def_node: e.id, body: vty,
                        ambient: self.binders.iter().cloned().collect(),
                        expansive: !is_value(&b.value), recursive: false,
                        name: b.name.clone(),
                    });
                } else {
                    let vty = self.fresh_nv(b.value.id);
                    self.binders.insert(node_var(b.value.id));
                    self.env.insert(b.name.clone(), Entry {
                        def_node: e.id, body: vty.clone(),
                        ambient: self.binders.iter().cloned().collect(),
                        expansive: true, recursive: true,
                        name: b.name.clone(),
                    });
                    let genvty = self.gen(&b.value);
                    self.push_eq(vty.clone(), genvty,
                        Origin::new(b.name_span, Some(e.id), "递归绑定"),
                        format!("递归名字 {} 与其定义同类型（不泛化）", b.name));
                    if let Some(ann) = &b.ann {
                        let sty = self.ast_type(ann, e.id);
                        self.push_eq(sty, vty.clone(),
                            Origin::new(ann.span(), Some(e.id), "类型标注"),
                            format!("let rec {} 的标注", b.name));
                    }
                    let ambient: Vec<String> = self.binders.iter().cloned().collect();
                    let cid = self.cid("gen", Some(e.id), Some(b.value.id), |h| mix_str(h, &b.name));
                    self.actions.push(Action::Generalize {
                        cid, let_node: e.id, name: b.name.clone(),
                        body_node: b.value.id, ambient,
                        recursive: true,
                        origin: Origin::new(b.name_span, Some(e.id), format!("泛化 {}", b.name)),
                    });
                }
                let bodyty = self.gen(&b.body);
                self.push_eq(bodyty, nvar.clone(),
                    Origin::new(b.span, Some(e.id), "let 体"),
                    format!("let {} in ... 的结果", b.name));
                self.scope = saved_scope;
                self.env = saved_env;
                self.binders = saved_binders;
            }
            _ => self.gen_rows(e, nvar),
        }
        nvar
    }
}

/// Replace quantified variables of a scheme with deterministic fresh keys.
fn instantiate_scheme_keys(scheme: &SType, prefix: &str) -> SType {
    let qs = scheme.free_vars();
    let mut mapping = HashMap::new();
    for (i, q) in qs.into_iter().enumerate() {
        mapping.insert(q, format!("i:{}:{}", prefix, i));
    }
    scheme.with_mapping(&mapping)
}

impl<'a> Generator<'a> {
    fn gen_rows(&mut self, e: &Expr, nvar: SType) {
        match &e.data {
            ExprData::Record { fields, row } => {
                let mut sfields = Vec::new();
                for f in fields {
                    let ty = self.gen(&f.value);
                    sfields.push(SField { label: f.label.0.clone(), ty });
                }
                let tail = match row {
                    None => SRow::Closed,
                    Some(name) if name == "*" => SRow::Open(tail_var(e.id, "rec")),
                    Some(name) => SRow::Open(format!("a:{}:{}", e.id, name)),
                };
                let rty = SType::Record(sfields, tail);
                self.push_eq(rty, nvar, Origin::new(e.span, Some(e.id), "记录"),
                    "记录字面量逐字段建立行类型".into());
            }
            ExprData::Proj { record, label, label_span } => {
                let rty = self.gen(record);
                let fty = SType::V(format!("f:{}:{}", e.id, label.0));
                self.binders.insert(format!("f:{}:{}", e.id, label.0));
                let rowrec = SType::Record(
                    vec![SField { label: label.0.clone(), ty: fty.clone() }],
                    SRow::Open(tail_var(e.id, "rec")),
                );
                self.binders.insert(tail_var(e.id, "rec"));
                self.push_eq(rowrec, rty,
                    Origin::new(e.span, Some(e.id), format!("字段 .{}", label.0))
                        .with2(Some(record.id)),
                    format!("投影需要记录含字段 {}，其余字段保持多态", label.0));
                self.push_eq(fty, nvar,
                    Origin::new(*label_span, Some(e.id), format!("字段 {}", label.0)),
                    format!("字段 {} 的类型成为投影结果", label.0));
            }
            ExprData::Extend { label, label_span, value, rest } => {
                let vty = self.gen(value);
                let rty = self.gen(rest);
                self.binders.insert(format!("f:{}:{}", e.id, label.0));
                let extrow = SType::Record(
                    Vec::new(),
                    SRow::Open(format!("xt:{}:{}:{}", e.id, label.0, "k")),
                );
                // equation A: rest must be a record lacking label, tail carries label=vty
                let need = SType::Record(
                    vec![SField { label: label.0.clone(), ty: vty }],
                    SRow::Open(format!("xt:{}:{}:{}", e.id, label.0, "k")),
                );
                self.binders.insert(format!("xt:{}:{}:{}", e.id, label.0, "k"));
                self.push_eq(need, rty,
                    Origin::new(*label_span, Some(e.id), format!("扩展字段 {}", label.0))
                        .with2(Some(rest.id)),
                    format!("扩展要求原记录尚无字段 {}", label.0));
                self.push_eq(extrow, nvar,
                    Origin::new(e.span, Some(e.id), "扩展结果"),
                    format!("{{ {} = .. | rest }} 的结果行", label.0));
            }
            ExprData::Variant { tag, payload, tag_span } => {
                let pty = match payload {
                    Some(p) => self.gen(p),
                    None => SType::Con("unit".into()),
                };
                let vrow = SType::Variant(
                    vec![SField { label: tag.0.clone(), ty: pty }],
                    SRow::Open(tail_var(e.id, "var")),
                );
                self.binders.insert(tail_var(e.id, "var"));
                self.push_eq(vrow, nvar,
                    Origin::new(*tag_span, Some(e.id), format!("变体 {}", tag.0)),
                    format!("构造变体 {}，其它分支保持多态", tag.0));
            }
            ExprData::Match { scrut, arms } => {
                let sty = self.gen(scrut);
                let result_t = nvar.clone();
                for (i, arm) in arms.iter().enumerate() {
                    let saved_scope = self.scope;
                    let saved_env = self.env.clone();
                    let saved_binders = self.binders.clone();
                    mix_u64(&mut self.scope, ((e.id as u128) << 40 | i as u128) as u64);
                    let (pty, binds) = self.gen_pat(&arm.pat, e.id, i);
                    for (k, _) in &binds { self.binders.insert(k.clone()); }
                    let mut row_fields = Vec::new();
                    for (tag, pk) in &binds_tags(&arm.pat, e.id, i) {
                        row_fields.push(match pk {
                            Some(k) => SField { label: tag.clone(), ty: SType::V(k.clone()) },
                            None => SField { label: tag.clone(), ty: SType::Con("unit".into()) },
                        });
                    }
                    if !row_fields.is_empty() {
                        let expected = SType::Variant(row_fields, SRow::Open(tail_var(e.id, "match")));
                        self.binders.insert(tail_var(e.id, "match"));
                        self.push_eq(expected, sty.clone(),
                            Origin::new(arm.pat.span(), Some(e.id), format!("分支模式 {}", i + 1)),
                            format!("第 {} 个分支要求被匹配值是相应变体", i + 1));
                    } else {
                        self.push_eq(pty, sty.clone(),
                            Origin::new(arm.pat.span(), Some(e.id), format!("分支模式 {}", i + 1)),
                            "变量/通配分支接受任意类型".into());
                    }
                    for (k, node) in binds {
                        let span = match &arm.pat {
                            Pat::Var(_, s) => *s,
                            _ => arm.pat.span(),
                        };
                        let _ = node;
                        self.env.insert(binder_name(&arm.pat, &k).unwrap_or_else(|| k.clone()), Entry {
                            def_node: arm.body.id, body: SType::V(k),
                            ambient: BTreeSet::new(), expansive: false, recursive: false,
                            name: binder_name(&arm.pat, &k).unwrap_or_default(),
                        });
                        let _ = span;
                    }
                    let bty = self.gen(&arm.body);
                    self.push_eq(result_t.clone(), bty,
                        Origin::new(arm.span, Some(e.id), format!("分支 {} 结果", i + 1)),
                        "所有分支结果类型一致".into());
                    self.scope = saved_scope;
                    self.env = saved_env;
                    self.binders = saved_binders;
                }
            }
            ExprData::Ann { expr, ty } => {
                let sty = self.ast_type(ty, e.id);
                let ety = self.gen(expr);
                self.push_eq(sty, ety,
                    Origin::new(ty.span(), Some(e.id), "类型标注"),
                    "标注类型必须与表达式一致".into());
                self.push_eq(ety, nvar,
                    Origin::new(e.span, Some(e.id), "标注表达式"),
                    "标注表达式继承内部类型".into());
            }
            ExprData::If { cond, then_, else_ } => {
                let cty = self.gen(cond);
                self.push_eq(SType::Con("bool".into()), cty,
                    Origin::new(e.span, Some(e.id), "if 条件"),
                    "if 条件必须是 bool".into());
                let tty = self.gen(then_);
                let ety = self.gen(else_);
                self.push_eq(nvar.clone(), tty,
                    Origin::new(e.span, Some(then_.id), "then 分支"),
                    "then 分支类型".into());
                self.push_eq(nvar, ety,
                    Origin::new(e.span, Some(else_.id), "else 分支"),
                    "else 分支类型必须与 then 一致".into());
            }
            ExprData::Bin { op, lhs, rhs } => {
                let lty = self.gen(lhs);
                let rty = self.gen(rhs);
                let (res, name) = match op {
                    Bin::Add | Bin::Sub => (SType::Con("int".into()), op.symbol()),
                    Bin::Eq | Bin::Lt => (SType::Con("bool".into()), op.symbol()),
                };
                self.push_eq(SType::Con("int".into()), lty,
                    Origin::new(e.span, Some(lhs.id), format!("{} 左操作数", name)),
                    format!("{} 左操作数是 int", name));
                self.push_eq(SType::Con("int".into()), rty,
                    Origin::new(e.span, Some(rhs.id), format!("{} 右操作数", name)),
                    format!("{} 右操作数是 int", name));
                self.push_eq(res, nvar,
                    Origin::new(e.span, Some(e.id), format!("{} 结果", name)),
                    format!("{} 的结果类型", name));
            }
            _ => unreachable!("gen_rows covered"),
        }
    }

    fn gen_pat(&mut self, pat: &Pat, match_node: NodeId, arm: usize)
        -> (SType, Vec<(String, NodeId)>) {
        match pat {
            Pat::Var(_, _) => {
                let k = format!("pm:{}:{}:x", match_node, arm);
                self.binders.insert(k.clone());
                (SType::V(k.clone()), vec![(k, match_node)])
            }
            Pat::Wild(_) => {
                let k = format!("pm:{}:{}:w", match_node, arm);
                self.binders.insert(k.clone());
                (SType::V(k), vec![])
            }
            Pat::Tag(name, inner, _) => match inner {
                None => {
                    let k = format!("pm:{}:{}:tag", match_node, arm);
                    (SType::Variant(vec![SField {
                        label: name.clone(), ty: SType::Con("unit".into())
                    }], SRow::Open(tail_var(match_node, "pat"))), vec![])
                }
                Some(inner) => {
                    let (pty, binds) = self.gen_pat(inner, match_node, arm);
                    let payload_key = match &pty { SType::V(k) => k.clone(), _ => format!("pm:{}:{}:p", match_node, arm) };
                    (SType::Variant(vec![SField {
                        label: name.clone(), ty: SType::V(payload_key)
                    }], SRow::Open(tail_var(match_node, "pat"))), binds)
                }
            },
        }
    }

    fn ast_type(&mut self, t: &Type, ctx: NodeId) -> SType {
        match t {
            Type::TCon(n, _) => SType::Con(n.clone()),
            Type::TVar(n, _) => SType::V(format!("a:{}:{}", ctx, n)),
            Type::TFun(a, b, _) => SType::fun(self.ast_type(a, ctx), self.ast_type(b, ctx)),
            Type::TRecord(fs, row, _) => {
                let sfs: Vec<SField> = fs.iter().map(|(l, _, t)| SField {
                    label: l.0.clone(), ty: self.ast_type(t, ctx),
                }).collect();
                let r = match row {
                    None => SRow::Closed,
                    Some(n) if n == "*" => SRow::Open(tail_var(ctx, "annrec")),
                    Some(n) => SRow::Open(format!("a:{}:{}", ctx, n)),
                };
                SType::Record(sfs, r)
            }
            Type::TVariant(fs, row, _) => {
                let sfs: Vec<SField> = fs.iter().map(|(l, _, t)| SField {
                    label: l.0.clone(),
                    ty: match t { Some(t) => self.ast_type(t, ctx), None => SType::Con("unit".into()) },
                }).collect();
                let r = match row {
                    None => SRow::Closed,
                    Some(n) if n == "*" => SRow::Open(tail_var(ctx, "annvar")),
                    Some(n) => SRow::Open(format!("a:{}:{}", ctx, n)),
                };
                SType::Variant(sfs, r)
            }
        }
    }
}

fn binds_tags(pat: &Pat, match_node: NodeId, arm: usize) -> Vec<(String, Option<String>)> {
    match pat {
        Pat::Tag(name, inner, _) => vec![(name.clone(), inner.as_ref().map(|_| format!("pm:{}:{}:x", match_node, arm)))],
        _ => vec![],
    }
}
fn binder_name(pat: &Pat, key: &str) -> Option<String> {
    if let Pat::Var(n, _) = pat { Some(n.clone()) } else { let _ = key; None }
}

impl Origin {
    fn with2(mut self, node2: Option<NodeId>) -> Self { self.node2 = node2; self }
}
