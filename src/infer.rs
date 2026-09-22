use crate::ast::*;
use crate::solve::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConstraintKind {
    Construct,
    Equal,
    Generalize,
    Instantiate,
    Unbound,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Constraint {
    pub id: u64,
    pub kind: ConstraintKind,
    pub node: usize,
    pub span: Span,
    pub label: String,
    pub cache_key: String,
    pub deps: Vec<u64>,
    pub left: Option<Ty>,
    pub right: Option<Ty>,
    pub active_vars: Vec<Var>,
    pub gen_binder: Option<usize>,
    pub status: ConstraintStatus,
    pub origin_constraint: Option<u64>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConstraintStatus {
    New,
    Reused,
    Invalidated,
    Resolved,
    Disabled,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeTrace {
    pub node: usize,
    pub ty: Ty,
    pub display: String,
    pub origins: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Analysis {
    pub constraints: Vec<Constraint>,
    pub steps: Vec<Step>,
    pub errors: Vec<TypeError>,
    pub node_types: BTreeMap<usize, TypeTrace>,
    pub schemes: BTreeMap<usize, String>,
    pub reused: Vec<u64>,
    pub invalidated: Vec<u64>,
    pub resolved: Vec<u64>,
    pub cores: Vec<ErrorCore>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ErrorCore {
    pub error_index: usize,
    pub size: usize,
    pub candidates: Vec<Vec<u64>>,
}

#[derive(Clone)]
enum Entry { Mono(Var), Poly(usize) }

pub struct Analyzer<'a> {
    program: &'a Program,
    uf: UnionFind,
    constraints: Vec<Constraint>,
    steps: Vec<Step>,
    errors: Vec<TypeError>,
    node_types: BTreeMap<usize, Ty>,
    schemes: BTreeMap<usize, Scheme>,
    active: HashSet<Var>,
    disabled: HashSet<u64>,
    scope_path: Vec<String>,
    previous: HashMap<String, ConstraintSnapshot>,
    reused: HashSet<u64>,
    invalidated: HashSet<u64>,
    local_id: HashMap<usize, u16>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConstraintSnapshot {
    fingerprint: String,
    id: u64,
}

pub fn analyze(program: &Program, previous: Option<&Analysis>, disabled: HashSet<u64>) -> Analysis {
    let mut prev = HashMap::new();
    if let Some(previous) = previous {
        for c in &previous.constraints {
            prev.insert(c.cache_key.clone(), ConstraintSnapshot { fingerprint: fingerprint(c), id: c.id });
        }
    }
    let mut a = Analyzer {
        program,
        uf: UnionFind::new(),
        constraints: vec![],
        steps: vec![],
        errors: vec![],
        node_types: BTreeMap::new(),
        schemes: BTreeMap::new(),
        active: HashSet::new(),
        disabled,
        scope_path: vec!["root".into()],
        previous: prev,
        reused: HashSet::new(),
        invalidated: HashSet::new(),
        local_id: HashMap::new(),
    };
    let mut global_env: Vec<(String, Entry, usize)> = vec![];
    for (idx, root) in program.roots.iter().enumerate() {
        a.scope_path = vec!["root".into(), format!("r{idx}")];
        match &root.kind {
            RootKind::Expr(e) => { a.infer(*e, &global_env); }
            RootKind::Let { rec, name, binder, value, body } => {
                let ty = if *rec { a.infer_rec(*binder, *value, &global_env) } else { a.infer(*value, &global_env) };
                let safe = *rec || matches!(program.nodes[value].kind, Kind::Abs { .. });
                a.emit_generalize(*binder, *value, ty, &global_env, safe, name);
                let entry = if a.disabled.contains(&a.cid(*binder, 2)) || !safe {
                    if let TyKind::Var(v) = a.uf.prune(&ty).kind { Entry::Mono(v) } else { a.uf.fresh_var_entry(&ty) }
                } else { Entry::Poly(*binder) };
                let pushed = global_env.len();
                global_env.push((name.clone(), entry, a.scope_path.len()));
                if let Some(body) = body { a.infer(*body, &global_env); }
                if body.is_none() && !safe {
                    if let Some((_, Entry::Mono(v), _)) = global_env.last() { a.active.insert(a.uf.find(*v)); }
                }
                global_env.truncate(pushed + 1);
            }
        }
    }
    a.finish()
}

impl UnionFind {
    fn fresh_var_entry(&mut self, ty: &Ty) -> Var {
        let v = self.fresh(ty.origin);
        self.bind_ty(v, self.prune(ty));
        v
    }
}

impl<'a> Analyzer<'a> {
    fn origin(&self, node: usize) -> Origin { Origin::Node(node) }
    fn fresh(&mut self, node: usize) -> Var { self.uf.fresh(Origin::Node(node)) }

    fn cid(&self, node: usize, slot: u16) -> u64 { ((node as u64 + 1) << 8) | slot as u64 }

    fn next_slot(&mut self, node: usize) -> u16 {
        let v = self.local_id.entry(node).or_insert(0);
        let r = *v;
        *v += 1;
        r
    }

    fn env_key(&self, env: &[(String, Entry, usize)], extra: &str) -> String {
        let mut gens: Vec<String> = env.iter().filter_map(|(name, entry, depth)| {
            if *depth > 1 {
                match entry {
                    Entry::Poly(id) => Some(format!("p:{id}:{name}")),
                    Entry::Mono(_) => Some(format!("m:{name}@{}", depth)),
                }
            } else { None }
        }).collect();
        gens.sort();
        format!("{}|{}|{}", self.scope_path.join("/"), extra, gens.join(","))
    }

    fn record_ty(&mut self, node: usize, children: &[(String, usize)], env: &[(String, Entry, usize)]) -> Ty {
        let mut row = Row::empty(self.origin(node));
        let mut fields = vec![];
        for (_, field_id) in children.iter().filter(|(role,_)| role == "field") {
            let name = match &self.program.nodes[field_id].kind { Kind::RecordField { name } => name.clone(), _ => continue };
            let value_id = self.program.nodes[field_id].child("value").unwrap_or(*field_id);
            let value = self.infer(value_id, env);
            fields.push((name, value));
        }
        for (name, value) in fields.into_iter().rev() { row = Row::extend(name, value, row, self.origin(node)); }
        Ty::record(row, self.origin(node))
    }

    fn infer(&mut self, node: usize, outer: &[(String, Entry, usize)]) -> Ty {
        let n = self.program.nodes[&node].clone();
        let mut env = outer.to_vec();
        match &n.kind {
            Kind::Int { .. } => {
                let ty = Ty::int(self.origin(node));
                self.emit_construct(node, n.span, "整数字面量", Some(ty.clone()), None, vec![]);
                self.node_types.insert(node, ty.clone()); ty
            }
            Kind::Bool { .. } => {
                let ty = Ty::bool(self.origin(node));
                self.emit_construct(node, n.span, "布尔字面量", Some(ty.clone()), None, vec![]);
                self.node_types.insert(node, ty.clone()); ty
            }
            Kind::Var { name, binder } => self.infer_var(node, n.span, name, *binder, &env),
            Kind::Abs { param } => {
                let param_id = n.child("binder").unwrap_or(node);
                let body_id = n.child("body").unwrap_or(node);
                let p = self.fresh(param_id);
                let r = self.fresh(node);
                self.active.insert(p);
                env.push((param.clone(), Entry::Mono(p), self.scope_path.len() + 1));
                self.scope_path.push(format!("abs{node}"));
                let body = self.infer(body_id, &env);
                self.scope_path.pop();
                self.active.remove(&p);
                let ty = Ty::fun(Ty::var(p, self.origin(param_id)), body, self.origin(node));
                self.emit_construct(node, n.span, &format!("函数 {param}"), Some(ty.clone()), None, vec![]);
                self.emit_equal(node, n.span, "函数结果", Ty::var(r, self.origin(node)), ty.clone(), vec![]);
                let result = Ty::var(r, self.origin(node));
                self.node_types.insert(node, result.clone()); result
            }
            Kind::App => {
                let func_id = n.child("func").unwrap();
                let arg_id = n.child("arg").unwrap();
                let func = self.infer(func_id, outer);
                let arg = self.infer(arg_id, outer);
                let result_v = self.fresh(node);
                let expected = Ty::fun(arg, Ty::var(result_v, self.origin(node)), self.origin(node));
                self.emit_equal(node, n.span, "函数应用", func, expected, vec![]);
                let result = Ty::var(result_v, self.origin(node));
                self.node_types.insert(node, result.clone()); result
            }
            Kind::Let { rec, name } => {
                let binder_id = n.child("binder").unwrap_or(node);
                let value_id = n.child("value").unwrap();
                let body_id = n.child("body").unwrap();
                let ty = if *rec { self.infer_rec(binder_id, value_id, outer) } else { self.infer(value_id, outer) };
                let safe = *rec || matches!(self.program.nodes[&value_id].kind, Kind::Abs { .. });
                self.emit_generalize(binder_id, value_id, ty, outer, safe, name);
                let entry = self.generalized_entry(binder_id, ty.clone(), safe);
                env.push((name.clone(), entry, self.scope_path.len() + 1));
                self.scope_path.push(format!("let{node}"));
                let body = self.infer(body_id, &env);
                self.scope_path.pop();
                self.node_types.insert(node, body.clone()); body
            }
            Kind::Record => {
                let ty = self.record_ty(node, &n.children, outer);
                self.emit_construct(node, n.span, "记录字面量", Some(ty.clone()), None, vec![]);
                self.node_types.insert(node, ty.clone()); ty
            }
            Kind::RecordField { .. } => {
                let value = n.child("value").map(|v| self.infer(v, outer)).unwrap_or_else(|| Ty::var(self.fresh(node), self.origin(node)));
                self.node_types.insert(node, value.clone()); value
            }
            Kind::Field { name } => {
                let record_id = n.child("record").unwrap();
                let record = self.infer(record_id, outer);
                let field_v = self.fresh(node);
                let rest_v = self.fresh(node);
                let row = Row::extend(name.clone(), Ty::var(field_v, self.origin(node)), Row::var(rest_v, self.origin(node)), self.origin(node));
                let expected = Ty::record(row, self.origin(node));
                self.emit_construct(node, n.span, &format!("字段投影 .{name}"), Some(Ty::var(field_v, self.origin(node))), Some(expected.clone()), vec![]);
                self.emit_equal(node, n.span, &format!("投影字段 {name}"), record, expected, vec![]);
                let result = Ty::var(field_v, self.origin(node));
                self.node_types.insert(node, result.clone()); result
            }
            Kind::Variant { tag } => {
                let payload_id = n.child("payload").unwrap();
                let payload = self.infer(payload_id, outer);
                let rest_v = self.fresh(node);
                let row = Row::extend(tag.clone(), payload.clone(), Row::var(rest_v, self.origin(node)), self.origin(node));
                let ty = Ty::variant(row, self.origin(node));
                self.emit_construct(node, n.span, &format!("变体构造 {tag}"), Some(ty.clone()), None, vec![]);
                self.node_types.insert(node, ty.clone()); ty
            }
            Kind::Match => {
                let scrut_id = n.child("scrutinee").unwrap();
                let scrut = self.infer(scrut_id, outer);
                let result_v = self.fresh(node);
                let mut row = Row::var(self.fresh(node), self.origin(node));
                let arm_entries = n.children.iter().filter(|(r,_)| r == "arm").map(|(_,id)| *id).collect::<Vec<_>>();
                let mut arm_types = vec![];
                for arm_id in arm_entries.iter().rev() {
                    let arm = self.program.nodes[arm_id].clone();
                    let (tag, binding) = match arm.kind { Kind::MatchArm { tag, binding } => (tag,binding), _ => continue };
                    let payload_v = self.fresh(*arm_id);
                    row = Row::extend(tag, Ty::var(payload_v, self.origin(*arm_id)), row, self.origin(node));
                    arm_types.push((*arm_id, binding, payload_v));
                }
                self.emit_equal(node, n.span, "match 判变体", scrut, Ty::variant(row, self.origin(node)), vec![]);
                let mut result = Ty::var(result_v, self.origin(node));
                for (arm_id, binding, payload_v) in arm_types.into_iter().rev() {
                    let arm = &self.program.nodes[&arm_id];
                    let value_id = arm.child("value").unwrap();
                    let mut local = outer.to_vec();
                    local.push((binding, Entry::Mono(payload_v), self.scope_path.len() + 1));
                    self.scope_path.push(format!("arm{arm_id}"));
                    let value = self.infer(value_id, &local);
                    self.scope_path.pop();
                    self.emit_equal(arm_id, arm.span, "分支结果一致", result.clone(), value, vec![]);
                    result = Ty::var(result_v, self.origin(node));
                }
                self.node_types.insert(node, result.clone()); result
            }
            Kind::MatchArm { .. } => Ty::var(self.fresh(node), self.origin(node)),
        }
    }

    fn infer_rec(&mut self, binder: usize, value: usize, outer: &[(String, Entry, usize)]) -> Ty {
        let v = self.fresh(binder);
        self.active.insert(v);
        let name = if let Some(root) = self.program.roots.iter().find(|r| matches!(&r.kind, RootKind::Let { binder: b, value: x, .. } if *b == binder && *x == value)) {
            match &root.kind { RootKind::Let { name, .. } => name.clone(), _ => format!("rec{binder}") }
        } else if let Kind::Let { name, .. } = &self.program.nodes.get(&binder).map(|n| n.kind.clone()).unwrap_or(Kind::Int { value: "0".into() }) {
            name.clone()
        } else { format!("rec{binder}") };
        let mut env = outer.to_vec();
        env.push((name, Entry::Mono(v), self.scope_path.len()));
        let actual = self.infer(value, &env);
        self.emit_equal(binder, self.program.nodes[&binder].span, "递归绑定", Ty::var(v, self.origin(binder)), actual, vec![]);
        self.active.remove(&v);
        Ty::var(v, self.origin(binder))
    }

    fn infer_var(&mut self, node: usize, span: Span, name: &str, binder: Option<usize>, env: &[(String, Entry, usize)]) -> Ty {
        for (entry_name, entry, _) in env.iter().rev() {
            if entry_name == name {
                return match entry {
                    Entry::Mono(v) => {
                        let ty = Ty::var(*v, self.origin(node));
                        self.emit_construct(node, span, &format!("变量 {name}"), Some(ty.clone()), None, vec![]);
                        self.node_types.insert(node, ty.clone()); ty
                    }
                    Entry::Poly(binder_id) => {
                        let scheme = self.schemes.get(binder_id).cloned();
                        let ty = if let Some(scheme) = scheme { scheme.instantiate(&mut self.uf, self.origin(node)) } else { Ty::var(self.fresh(node), self.origin(node)) };
                        self.emit_instantiate(node, span, name, *binder_id, ty.clone(), env);
                        self.node_types.insert(node, ty.clone()); ty
                    }
                };
            }
        }
        let v = self.fresh(node);
        let ty = Ty::var(v, self.origin(node));
        let id = self.cid(node, self.next_slot(node));
        let key = self.env_key(env, &format!("unbound:{node}:{name}"));
        let error = TypeError { kind: TypeErrorKind::UnboundVariable, message: format!("未绑定变量 {name}"), constraint_id: id, node, spans: vec![node], expected: None, actual: None, missing: vec![], extra: vec![] };
        self.push_constraint(id, ConstraintKind::Unbound, node, span, &format!("未绑定变量 {name}"), key, Some(ty.clone()), None, vec![], vec![], Some(id), None);
        self.errors.push(error);
        self.node_types.insert(node, ty.clone()); ty
    }

    fn generalized_entry(&mut self, binder: usize, ty: Ty, safe: bool) -> Entry {
        let id = self.cid(binder, 2);
        if self.disabled.contains(&id) || !safe {
            let pruned = self.uf.prune(&ty);
            if let TyKind::Var(v) = pruned.kind { Entry::Mono(v) } else { Entry::Mono(self.uf.fresh_var_entry(&pruned)) }
        } else { Entry::Poly(binder) }
    }

    fn emit_generalize(&mut self, binder: usize, value: usize, ty: Ty, env: &[(String, Entry, usize)], safe: bool, name: &str) {
        let id = self.cid(binder, 2);
        let active: Vec<Var> = self.active_active(env);
        let key = self.env_key(env, &format!("gen:{binder}:{name}"));
        let pruned = self.uf.prune(&ty);
        let label = if safe { format!("泛化 {name}") } else { format!("value restriction: {name} 保持单态") };
        self.push_constraint(id, ConstraintKind::Generalize, binder, self.program.nodes[&binder].span, &label, key, Some(pruned.clone()), None, active.clone(), vec![], Some(binder), None);
        if self.disabled.contains(&id) {
            self.step(id, StepKind::SkipDisabled, "泛化被临时禁用", None);
            return;
        }
        if safe {
            let active_set: HashSet<Var> = active.iter().copied().map(|v| self.uf.find(v)).collect();
            let scheme = generalize(&mut self.uf, &pruned, &active_set);
            self.schemes.insert(binder, scheme);
        } else {
            self.schemes.insert(binder, Scheme { vars: vec![], ty: pruned.clone() });
            if let TyKind::Var(v) = pruned.kind { self.active.insert(self.uf.find(v)); }
        }
        self.step(id, StepKind::Generalize, label, None);
    }

    fn active_active(&self, env: &[(String, Entry, usize)]) -> Vec<Var> {
        let mut out: Vec<Var> = self.active.iter().copied().collect();
        for (_, entry, _) in env { if let Entry::Mono(v) = entry { out.push(*v); } }
        out.sort(); out.dedup(); out
    }

    fn emit_instantiate(&mut self, node: usize, span: Span, name: &str, binder: usize, ty: Ty, env: &[(String, Entry, usize)]) {
        let id = self.cid(node, self.next_slot(node));
        let key = self.env_key(env, &format!("inst:{node}:{name}:{binder}"));
        let deps = vec![self.cid(binder, 2)];
        self.push_constraint(id, ConstraintKind::Instantiate, node, span, &format!("实例化 {name}"), key, Some(ty), None, vec![], deps, None, Some(self.cid(binder,2)));
        self.step(id, StepKind::Instantiate, format!("实例化多态绑定 {name}"), None);
    }

    fn emit_construct(&mut self, node: usize, span: Span, label: &str, left: Option<Ty>, right: Option<Ty>, deps: Vec<u64>) {
        let id = self.cid(node, self.next_slot(node));
        let key = format!("construct:{}:{}:{}", self.scope_path.join("/"), node, label);
        self.push_constraint(id, ConstraintKind::Construct, node, span, label, key, left, right, vec![], deps, None, None);
        self.step(id, StepKind::Begin, format!("构造 {label}"), None);
    }

    fn emit_equal(&mut self, node: usize, span: Span, label: &str, left: Ty, right: Ty, deps: Vec<u64>) {
        let id = self.cid(node, self.next_slot(node));
        let key = format!("equal:{}:{}:{}", self.scope_path.join("/"), node, label);
        self.push_constraint(id, ConstraintKind::Equal, node, span, label, key, Some(left.clone()), Some(right.clone()), vec![], deps, None, None);
        if self.disabled.contains(&id) { self.step(id, StepKind::SkipDisabled, format!("约束 {id} 已禁用"), None); return; }
        let errors = unify(&mut self.uf, &left, &right, id, node);
        if errors.is_empty() { self.step(id, StepKind::Bind, format!("统一 {left_disp} 与 {right_disp}", left_disp=left.display(&mut self.uf), right_disp=right.display(&mut self.uf)), None); }
        else { for e in &errors { self.step(id, StepKind::Error, e.message.clone(), Some(e.clone())); self.errors.push(e.clone()); } }
    }

    fn push_constraint(&mut self, id: u64, kind: ConstraintKind, node: usize, span: Span, label: &str, cache_key: String, left: Option<Ty>, right: Option<Ty>, active_vars: Vec<Var>, deps: Vec<u64>, gen_binder: Option<usize>, origin_constraint: Option<u64>) {
        let c = Constraint { id, kind, node, span, label: label.to_string(), cache_key, deps, left, right, active_vars, gen_binder, status: ConstraintStatus::New, origin_constraint };
        if let Some(prev) = self.previous.get(&c.cache_key) {
            if prev.fingerprint == fingerprint(&c) { self.reused.insert(id); }
            else { self.invalidated.insert(id); }
        }
        self.constraints.push(c);
    }

    fn step(&mut self, id: u64, kind: StepKind, detail: String, error: Option<TypeError>) {
        self.steps.push(Step { index: self.steps.len(), constraint_id: id, kind, detail, error });
    }

    fn finish(mut self) -> Analysis {
        let mut invalid_prev: HashSet<u64> = HashSet::new();
        for c in &self.constraints {
            if self.invalidated.contains(&c.id) {
                if let Some(snap) = self.previous.get(&c.cache_key) { invalid_prev.insert(snap.id); }
            }
        }
        for c in &mut self.constraints {
            c.status = if self.reused.contains(&c.id) { ConstraintStatus::Reused }
                else if self.invalidated.contains(&c.id) { ConstraintStatus::Resolved }
                else if self.disabled.contains(&c.id) { ConstraintStatus::Disabled }
                else { ConstraintStatus::Resolved };
        }
        let mut node_types = BTreeMap::new();
        for (&node, ty) in &self.node_types {
            let resolved = self.uf.prune(ty);
            let mut origins = HashSet::new();
            collect_origins(&resolved, &mut origins);
            node_types.insert(node, TypeTrace { node, ty: resolved.clone(), display: resolved.display(&mut self.uf), origins: origins.into_iter().collect() });
        }
        let schemes = self.schemes.iter().map(|(k,s)| (*k, s.ty.display(&mut self.uf))).collect();
        let cores = compute_cores(&self.program, &self.constraints, &self.errors, &self.disabled);
        Analysis {
            constraints: self.constraints,
            steps: self.steps,
            errors: self.errors,
            node_types,
            schemes,
            reused: self.reused.iter().copied().collect(),
            invalidated: self.invalidated.iter().copied().collect(),
            resolved: vec![],
            cores,
        }
    }
}

fn collect_origins(ty: &Ty, out: &mut HashSet<usize>) {
    if let Origin::Node(n) = ty.origin { out.insert(n); }
    match &ty.kind {
        TyKind::Fun(a,b) => { collect_origins(a,out); collect_origins(b,out); }
        TyKind::Record(r) | TyKind::Variant(r) => collect_row_origins(r,out),
        _ => {}
    }
}
fn collect_row_origins(row: &Row, out: &mut HashSet<usize>) {
    if let Origin::Node(n) = row.origin { out.insert(n); }
    if let RowKind::Extend { value, rest, .. } = { collect_origins(value,out); collect_row_origins(rest,out); }
}
fn fingerprint(c: &Constraint) -> String {
    format!("{:?}:{:?}:{:?}:{:?}:{:?}:{:?}", c.kind,c.node,c.label,c.left,c.right,c.active_vars)
}
fn compute_cores(program: &Program, constraints: &[Constraint], errors: &[TypeError], globally_disabled: &HashSet<u64>) -> Vec<ErrorCore> {
    let relevant: Vec<u64> = constraints.iter().filter(|c| c.kind == ConstraintKind::Equal && !globally_disabled.contains(&c.id)).map(|c| c.id).collect();
    let mut out = vec![];
    for (idx, error) in errors.iter().enumerate() {
        if !relevant.contains(&error.constraint_id) { continue; }
        let candidates = minimal_cores(program, constraints, &relevant, error.constraint_id);
        if !candidates.is_empty() {
            let size = candidates[0].len();
            out.push(ErrorCore { error_index: idx, size, candidates });
        }
    }
    out
}

fn minimal_cores(_program: &Program, constraints: &[Constraint], relevant: &[u64], target: u64) -> Vec<Vec<u64>> {
    if !subset_unsat(constraints, relevant, target) { return vec![]; }
    let mut greedy = relevant.to_vec();
    let mut i = 0;
    while i < greedy.len() {
        let id = greedy[i];
        if id != target {
            let trial: Vec<u64> = greedy.iter().copied().filter(|x| *x != id).collect();
            if subset_unsat(constraints, &trial, target) { greedy = trial; continue; }
        }
        i += 1;
    }
    let size = greedy.len();
    if size == 1 { return vec![vec![target]]; }
    let others: Vec<u64> = relevant.iter().copied().filter(|x| *x != target).collect();
    let need = size.saturating_sub(1);
    let mut candidates: Vec<Vec<u64>> = vec![greedy.clone()];
    let mut seen: HashSet<Vec<u64>> = candidates.iter().cloned().collect();
    let limit = if need <= 2 { 5000 } else { 1000 };
    let mut tried = 0;
    'outer: for combo in combinations(others.len(), need) {
        tried += 1;
        if tried > limit { break; }
        let mut ids: Vec<u64> = combo.iter().map(|p| others[*p]).collect();
        ids.push(target);
        ids.sort();
        if subset_unsat(constraints, &ids, target) {
            let mut reduced = ids.clone();
            let mut j = 0;
            while j < reduced.len() {
                let id = reduced[j];
                if id != target {
                    let trial: Vec<u64> = reduced.iter().copied().filter(|x| *x != id).collect();
                    if subset_unsat(constraints, &trial, target) { reduced = trial; continue; }
                }
                j += 1;
            }
            reduced.sort();
            if reduced.len() == size && !seen.contains(&reduced) {
                seen.insert(reduced.clone());
                candidates.push(reduced);
                if candidates.len() >= 8 { break 'outer; }
            }
        }
    }
    candidates
}

fn combinations(n: usize, k: usize) -> Vec<Vec<usize>> {
    if k == 0 || k > n { return vec![]; }
    if k == 1 { return (0..n).map(|x| vec![x]).collect(); }
    let mut out = vec![];
    let mut cur: Vec<usize> = (0..k).collect();
    loop {
        out.push(cur.clone());
        let mut i = k;
        while i > 0 {
            i -= 1;
            if cur[i] != i + n - k {
                cur[i] += 1;
                for j in i+1..k { cur[j] = cur[j-1] + 1; }
                break;
            }
            if i == 0 { return out; }
        }
    }
}

fn subset_unsat(constraints: &[Constraint], enabled: &[u64], target: u64) -> bool {
    if !enabled.contains(&target) { return false; }
    let enabled: HashSet<u64> = enabled.iter().copied().collect();
    let errors = solve_equal_subset(constraints, Some(&enabled));
    errors.iter().any(|e| e.constraint_id == target)
}

fn solve_equal_subset(constraints: &[Constraint], enabled: Option<&HashSet<u64>>) -> Vec<TypeError> {
    let mut old_vars = HashSet::new();
    for c in constraints {
        if let Some(t) = &c.left { t.vars(&mut old_vars); }
        if let Some(t) = &c.right { t.vars(&mut old_vars); }
    }
    let mut uf = UnionFind::new();
    let mut remap: HashMap<Var, Var> = HashMap::new();
    for v in old_vars { remap.insert(v, uf.fresh(Origin::Constraint(0))); }
    let mut errors = vec![];
    for c in constraints {
        if c.kind != ConstraintKind::Equal { continue; }
        if enabled.is_some_and(|set| !set.contains(&c.id)) { continue; }
        if let (Some(l), Some(r)) = (&c.left, &c.right) {
            let l = l.map_vars(&mut |v| Ty::var(*remap.get(&v).unwrap_or(&v), l.origin));
            let r = r.map_vars(&mut |v| Ty::var(*remap.get(&v).unwrap_or(&v), r.origin));
            errors.extend(unify(&mut uf, &l, &r, c.id, c.node));
        }
    }
    errors
}

pub fn analyze_with_disabled(program: &Program, disabled: &HashSet<u64>) -> Analysis {
    analyze(program, None, disabled.clone())
}

fn fingerprint(c: &Constraint) -> String {
    format!("{:?}:{:?}:{:?}:{:?}:{:?}:{:?}", c.kind,c.node,c.label,c.left,c.right,c.active_vars)
}
