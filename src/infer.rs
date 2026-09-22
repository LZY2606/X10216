use crate::syntax::*;
use crate::types::*;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

#[derive(Debug, Clone)]
struct Entry {
    node_id: usize,
    binding: usize,
    scheme: Scheme,
    scope: String,
    gen_env: String,
}

#[derive(Debug, Clone)]
struct ScopeNode {
    node_id: usize,
    label: String,
    env_len: usize,
}

#[derive(Debug, Clone, Default)]
struct Frame {
    mono: BTreeSet<VarId>,
}

#[derive(Debug, Clone)]
enum PendingTy {
    None,
    Var(VarId),
    Ty(Ty),
}

#[derive(Debug, Clone)]
struct ErrorSeed {
    kind: ErrorKind,
    message: String,
    span: Span,
    path: String,
    expected: Ty,
    actual: Ty,
    reasons: BTreeSet<String>,
}

#[derive(Debug, Clone)]
struct RawStep {
    kind: StepKind,
    constraint_id: String,
    description: String,
    before: String,
    after: String,
    error: Option<usize>,
}

pub struct Engine {
    source: String,
    nodes: Vec<AstNode>,
    ids: BTreeMap<usize, usize>,
    paths: BTreeMap<usize, String>,
    parents: BTreeMap<usize, Option<usize>>,
    constraints: Vec<Constraint>,
    constraints_seen: BTreeSet<String>,
    steps: Vec<RawStep>,
    traces: BTreeMap<usize, Vec<TracePart>>,
    errors: Vec<TypeError>,
    finals: Vec<FinalType>,
    fresh: VarId,
    env: Vec<(String, Entry)>,
    scopes: Vec<ScopeNode>,
    frames: Vec<Frame>,
    tsubst: BTreeMap<VarId, Ty>,
    rsubst: BTreeMap<VarId, RowTail>,
    reasons: BTreeMap<VarId, BTreeSet<String>>,
    row_reasons: BTreeMap<VarId, BTreeSet<String>>,
    slot_counts: BTreeMap<String, usize>,
    node_slots: BTreeMap<usize, Vec<String>>,
}

pub fn analyze(source: &str, program: &Program, disabled: &BTreeSet<String>) -> Analysis {
    let mut engine = Engine {
        source: source.into(),
        nodes: Vec::new(),
        ids: BTreeMap::new(),
        paths: BTreeMap::new(),
        parents: BTreeMap::new(),
        constraints: Vec::new(),
        constraints_seen: BTreeSet::new(),
        steps: Vec::new(),
        traces: BTreeMap::new(),
        errors: Vec::new(),
        finals: Vec::new(),
        fresh: 0,
        env: Vec::new(),
        scopes: vec![ScopeNode { node_id: 0, label: "root".into(), env_len: 0 }],
        frames: vec![Frame::default()],
        tsubst: BTreeMap::new(),
        rsubst: BTreeMap::new(),
        reasons: BTreeMap::new(),
        row_reasons: BTreeMap::new(),
        slot_counts: BTreeMap::new(),
        node_slots: BTreeMap::new(),
    };
    engine.index_program(program);
    let mut result_ty = engine.con(0, "Unit");
    engine.push_scope(0, "program");
    for item in &program.items {
        match item {
            Item::Let(bind) => engine.infer_top_let(bind),
            Item::Expr(expr) => result_ty = engine.infer_expr(expr),
        }
    }
    engine.pop_scope();
    engine.solve_all(disabled);
    engine.finalize_program(program);
    let steps = engine.steps.into_iter().enumerate().map(|(i, s)| Step {
        index: i,
        kind: s.kind,
        constraint_id: s.constraint_id,
        description: s.description,
        before: s.before,
        after: s.after,
        error_id: s.error,
    }).collect();
    Analysis {
        source: source.into(),
        nodes: engine.nodes,
        constraints: engine.constraints,
        steps,
        errors: engine.errors,
        finals: engine.finals,
        traces: engine.traces,
    }
}

impl Engine {
    fn fresh_var(&mut self, origin: Origin) -> Ty {
        let id = self.fresh;
        self.fresh += 1;
        Ty::Var(id, origin)
    }

    fn fresh_row_var(&mut self, origin: Origin) -> VarId {
        let id = self.fresh;
        self.fresh += 1;
        id
    }

    fn origin(&self, node_id: usize, part: &'static str) -> Origin {
        let span = self.nodes.iter().find(|n| n.id == node_id).map_or_else(|| Span::new(0, 0), |n| n.span);
        Origin { node_id, part, span }
    }

    fn con(&mut self, node_id: usize, name: &str) -> Ty {
        Ty::Con(name.into(), self.origin(node_id, name))
    }

    fn record(&mut self, node_id: usize, fields: Vec<RowField>, tail: RowTail) -> Ty {
        Ty::Record(Row { fields, tail, origin: self.origin(node_id, "record") }, self.origin(node_id, "record"))
    }

    fn variant_row(&mut self, node_id: usize, label: &str, payload: Ty, tail: RowTail) -> Ty {
        let field = RowField { name: label.into(), name_span: payload.origin().span, value: payload };
        Ty::Variant(Row { fields: vec![field], tail, origin: self.origin(node_id, "variant") }, self.origin(node_id, "variant"))
    }

    fn trace(&mut self, ty: &Ty, node_id: usize, reason: TraceReason, cid: Option<String>) {
        let part = TracePart { node_id, span: ty.origin().span, reason, constraint_id: cid };
        self.traces.entry(ty.origin().node_id).or_default().push(part.clone());
        let mut work = vec![ty.clone()];
        while let Some(t) = work.pop() {
            self.traces.entry(t.origin().node_id).or_default().push(part.clone());
            match t {
                Ty::Arrow(a, b, _) => { work.push(*a); work.push(*b); }
                Ty::Record(row, _) | Ty::Variant(row, _) => for f in row.fields { work.push(f.value); },
                _ => {}
            }
        }
    }

    fn index_program(&mut self, program: &Program) {
        let mut bind_stack: Vec<String> = Vec::new();
        for (idx, item) in program.items.iter().enumerate() {
            self.index_item(item, None, format!("root/item#{idx}"), &mut bind_stack);
        }
    }

    fn index_item(&mut self, item: &Item, parent: Option<usize>, path: String, binds: &mut Vec<String>) {
        match item {
            Item::Expr(e) => self.index_expr(e, parent, path, binds),
            Item::Let(b) => {
                let id = self.register(
                    b.span,
                    "let",
                    format!("let {}", b.name),
                    format!("let({})", stable_name(&b.name, binds)),
                    parent,
                    path.clone(),
                );
                binds.push(b.name.clone());
                self.index_expr(&b.value, Some(id), format!("{path}/value"), binds);
                self.index_item(&b.body, Some(id), format!("{path}/body"), binds);
                binds.pop();
            }
        }
    }

    fn index_expr(&mut self, expr: &Expr, parent: Option<usize>, path: String, binds: &mut Vec<String>) -> usize {
        let (kind, label, canonical) = match &expr.kind {
            ExprKind::Int(n) => ("int", n.to_string(), format!("int:{n}")),
            ExprKind::Str(s) => ("string", format!("{s:?}"), format!("str:{}", s.len())),
            ExprKind::Bool(b) => ("bool", b.to_string(), format!("bool:{b}")),
            ExprKind::Var(name) => {
                let depth = binds.iter().rposition(|b| b == name).map(|p| binds.len() - p - 1).unwrap_or(usize::MAX);
                ("var", name.clone(), format!("var@{depth}"))
            }
            ExprKind::Lam(..) => ("fun", "fun".into(), "fun".into()),
            ExprKind::App(..) => ("app", "application".into(), "app".into()),
            ExprKind::Let(..) => ("let", "let".into(), "let".into()),
            ExprKind::If(..) => ("if", "if".into(), "if".into()),
            ExprKind::Record(..) => ("record", "record".into(), "record".into()),
            ExprKind::Field(_, l, _) => ("field", format!(".{l}"), format!("field:{l}")),
            ExprKind::Extend(..) => ("extend", "extend".into(), "extend".into()),
            ExprKind::Variant(l, _) => ("variant", format!("`{l}"), format!("variant:{l}")),
            ExprKind::Match(..) => ("match", "match".into(), "match".into()),
            ExprKind::Ascribe(..) => ("ascribe", "ascribe".into(), "ascribe".into()),
            ExprKind::Tuple(ts) => ("tuple", format!("tuple {}", ts.len()), format!("tuple:{}", ts.len())),
        };
        let id = self.register(expr.span, kind, label, canonical, parent, path.clone());
        match &expr.kind {
            ExprKind::Lam(params, body) => {
                let before = binds.len();
                for (i, p) in params.iter().enumerate() {
                    binds.push(p.name.clone());
                    self.index_expr(&Expr { span: p.span, kind: ExprKind::Var(p.name.clone()) }, Some(id), format!("{path}/param#{i}"), binds);
                }
                self.index_expr(body, Some(id), format!("{path}/body"), binds);
                binds.truncate(before);
            }
            ExprKind::App(f, args) => {
                self.index_expr(f, Some(id), format!("{path}/callee"), binds);
                for (i, a) in args.iter().enumerate() { self.index_expr(a, Some(id), format!("{path}/arg#{i}"), binds); }
            }
            ExprKind::Let(b) => {
                self.index_item(&Item::Let((**b).clone()), Some(id), format!("{path}/let"), binds);
            }
            ExprKind::If(a,b,c) => {
                self.index_expr(a, Some(id), format!("{path}/cond"), binds);
                self.index_expr(b, Some(id), format!("{path}/then"), binds);
                self.index_expr(c, Some(id), format!("{path}/else"), binds);
            }
            ExprKind::Record(fs, _) => for f in fs { self.index_expr(&f.value, Some(id), format!("{path}/field:{}", f.name), binds); },
            ExprKind::Field(e, _, _) => self.index_expr(e, Some(id), format!("{path}/record"), binds),
            ExprKind::Extend(e, fs) => {
                self.index_expr(e, Some(id), format!("{path}/base"), binds);
                for f in fs { self.index_expr(&f.value, Some(id), format!("{path}/field:{}", f.name), binds); }
            }
            ExprKind::Variant(_, e) => self.index_expr(e, Some(id), format!("{path}/payload"), binds),
            ExprKind::Match(e, arms) => {
                self.index_expr(e, Some(id), format!("{path}/scrutinee"), binds);
                for (i, arm) in arms.iter().enumerate() {
                    self.index_expr(&Expr { span: arm.binding_span, kind: ExprKind::Var(arm.binding.clone()) }, Some(id), format!("{path}/arm#{i}/binding"), binds);
                    self.index_expr(&arm.body, Some(id), format!("{path}/arm#{i}/body"), binds);
                }
            }
            ExprKind::Ascribe(e, ty) => {
                self.index_expr(e, Some(id), format!("{path}/expr"), binds);
                self.index_type(ty, Some(id), format!("{path}/type"), binds);
            }
            ExprKind::Tuple(ts) => for (i,e) in ts.iter().enumerate() { self.index_expr(e, Some(id), format!("{path}/item#{i}"), binds); },
            _ => {}
        }
        id
    }

    fn index_type(&mut self, ty: &TypeExpr, parent: Option<usize>, path: String, binds: &mut Vec<String>) -> usize {
        let (kind,label,canonical) = match &ty.kind {
            TypeExprKind::Con(c) => ("type-con", c.clone(), format!("con:{c}")),
            TypeExprKind::Var(v) => ("type-var", v.clone(), format!("tyvar:{}", binds.iter().rposition(|b| b==v).map(|p| binds.len()-p-1).unwrap_or(usize::MAX))),
            TypeExprKind::Arrow(..) => ("type-arrow", "->".into(), "arrow".into()),
            TypeExprKind::Record(..) => ("type-record", "record".into(), "record".into()),
            TypeExprKind::Variant(..) => ("type-variant", "variant".into(), "variant".into()),
        };
        let id = self.register(ty.span, kind, label, canonical, parent, path);
        match &ty.kind {
            TypeExprKind::Arrow(a,b) => { self.index_type(a, Some(id), format!("{path}/from"), binds); self.index_type(b, Some(id), format!("{path}/to"), binds); }
            TypeExprKind::Record(fs,_) | TypeExprKind::Variant(fs,_) => for f in fs { self.index_type(&f.value, Some(id), format!("{path}/field:{}", f.name), binds); },
            _ => {}
        }
        id
    }

    fn register(&mut self, span: Span, kind: &str, label: String, canonical: String, parent: Option<usize>, path: String) -> usize {
        let id = self.nodes.len();
        let stable = format!("{:016x}", fx_like_hash(&format!("{canonical}@{path}")));
        self.ids.insert(id, id);
        self.paths.insert(id, path.clone());
        self.parents.insert(id, parent);
        self.nodes.push(AstNode { id, stable, kind: kind.into(), label, span, parent, path });
        id
    }
}

fn stable_name(name: &str, binds: &[String]) -> String {
    binds.iter().rposition(|b| b == name).map(|p| format!("binder#{}", binds.len() - p - 1)).unwrap_or_else(|| format!("free:{name}"))
}

fn fx_lish_hash(s: &str) -> u64 { 0 }
fn fx_like_hash(s: &str) -> u64 {
    let mut h = 1469598103934665603u64;
    for b in s.as_bytes() {
        h ^= *b as u64;
        h = h.wrapping_mul(1099511628211);
    }
    h
}

impl Engine {
    fn node_path(&self, id: usize) -> String {
        self.paths.get(&id).cloned().unwrap_or_else(|| format!("n{id}"))
    }

    fn current_scope(&self) -> String {
        self.scopes.iter().map(|s| s.label.clone()).collect::<Vec<_>>().join(">")
    }

    fn push_scope(&mut self, node_id: usize, label: &str) {
        self.scopes.push(ScopeNode { node_id, label: label.into(), env_len: self.env.len() });
        self.frames.push(Frame::default());
    }

    fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.last() {
            self.env.truncate(scope.env_len);
        }
        self.scopes.pop();
        self.frames.pop();
    }

    fn remember_mono(&mut self, v: VarId) {
        if let Some(frame) = self.frames.last_mut() { frame.mono.insert(v); }
    }

    fn is_mono(&self, v: VarId) -> bool { self.frames.iter().any(|f| f.mono.contains(&v)) }

    fn slot(&mut self, node_id: usize, name: &str) -> String {
        let key = format!("{node_id}:{name}");
        let count = self.slot_counts.entry(key.clone()).or_insert(0);
        let n = *count;
        *count += 1;
        let value = if n == 0 { name.into() } else { format!("{name}#{n}") };
        self.node_slots.entry(node_id).or_default().push(value.clone());
        value
    }

    fn add_constraint(&mut self, kind: ConstraintKind, node_id: usize, label: &str, left: Ty, right: Ty) {
        let slot = self.slot(node_id, match kind {
            ConstraintKind::Equal => "eq",
            ConstraintKind::FieldPresent => "field",
            ConstraintKind::FieldAbsent => "absent",
            ConstraintKind::RowExtend => "extend",
            ConstraintKind::VariantMatch => "variant",
            ConstraintKind::Instantiate => "instantiate",
            ConstraintKind::Generalize => "generalize",
            ConstraintKind::Ascribe => "ascribe",
        });
        let id = format!("{}:{}:{}:{}", self.nodes[node_id].stable, slot, self.current_scope(), self.gen_env_fingerprint()).replace(' ', "_");
        if self.constraints_seen.insert(id.clone()) {
            self.constraints.push(Constraint { id, kind, label: label.into(), span: self.nodes[node_id].span, node_id, scope: self.current_scope(), gen_env: self.gen_env_fingerprint(), left, right });
        }
    }

    fn gen_env_fingerprint(&self) -> String {
        let mut visible: BTreeMap<String, (usize, SchemeShape)> = BTreeMap::new();
        for (name, entry) in &self.env {
            visible.entry(name.clone()).or_insert_with(|| (entry.binding, scheme_shape(&entry.scheme)));
        }
        let mut parts = visible.values().map(|(binding, shape)| format!("{}={}", self.nodes.get(*binding).map_or_else(|| format!("n{binding}"), |n| n.stable.clone()), shape.0)).collect::<Vec<_>>();
        parts.sort();
        parts.join("|")

    fn infer_top_let(&mut self, bind: &LetBind) {
        self.infer_let_bind(bind, None, true);
    }

    fn infer_let_bind(&mut self, bind: &LetBind, parent_node: Option<usize>, keep_binding: bool) -> Ty {
        let let_node = self.resolve_node(bind.name_span.start);
        let scope_path = self.nodes.get(let_node).map_or_else(|| format!("let@{}", bind.name_span.start), |n| n.path.clone());
        self.push_scope(let_node, &scope_path);
        let mut param_tys = Vec::new();
        for p in &bind.params {
            let node = self.resolve_node(p.span.start);
            let origin = self.origin(node, "param");
            let ty = if let Some(annot) = &p.annot {
                let ty = self.translate_type(annot, BTreeMap::new());
                let v = self.fresh_var(origin);
                self.add_constraint(ConstraintKind::Equal, node, "parameter annotation", v.clone(), ty.clone());
                v
            } else { self.fresh_var(origin) };
            param_tys.push(ty.clone());
            self.env.push((p.name.clone(), Entry { node_id: node, binding: node, scheme: Scheme { vars: BTreeSet::new(), monomorphic: BTreeSet::new(), body: ty.clone(), value_restricted: false }, scope: self.current_scope(), gen_env: self.gen_env_fingerprint() }));
        }
        let value_node = self.nodes.iter().filter(|n| n.path.ends_with("/value") && n.span == bind.value.span).map(|n| n.id).next().unwrap_or(bind.value.span.start);
        let value_ty = if bind.rec {
            let origin = self.origin(value_node, "recursive");
            let recv = self.fresh_var(origin);
            if let Ty::Var(v,_) = &recv { self.remember_mono(*v); }
            self.env.push((bind.name.clone(), Entry { node_id: value_node, binding: let_node, scheme: Scheme { vars: BTreeSet::new(), monomorphic: self.active_mono(), body: recv.clone(), value_restricted: true }, scope: self.current_scope(), gen_env: self.gen_env_fingerprint() }));
            let inferred = self.infer_expr(&bind.value);
            self.add_constraint(ConstraintKind::Equal, value_node, "recursive definition", recv, inferred.clone());
            inferred
        } else {
            self.infer_expr(&bind.value)
        };
        let value_ty = if let Some(annot) = &bind.annot {
            let translated = self.translate_type(annot, BTreeMap::new());
            self.add_constraint(ConstraintKind::Ascribe, value_node, "let annotation", value_ty, translated.clone());
            translated
        } else { value_ty };
        let value_restricted = !is_value(&bind.value) || bind.rec || !bind.params.is_empty();
        let scheme = self.make_scheme(value_ty, value_restricted);
        self.env.retain(|(name, entry)| !(name == &bind.name && entry.scope == self.current_scope()));
        let scheme_entry = Entry { node_id: value_node, binding: let_node, scheme, scope: self.current_scope(), gen_env: self.gen_env_fingerprint() };
        self.env.push((bind.name.clone(), scheme_entry.clone()));
        let result = match *bind.body.clone() {
            Item::Let(next) => self.infer_let_bind(&next, parent_node),
            Item::Expr(e) => self.infer_expr(&e),
        };
        self.pop_scope();
        if keep_binding {
            self.env.push((bind.name.clone(), scheme_entry));
        }
        result
    }

    fn active_mono(&self) -> BTreeSet<VarId> { self.frames.iter().flat_map(|f| f.mono.iter().copied()).collect() }

    fn nodes_by_span(&self, span: Span) -> Option<&AstNode> { self.nodes.iter().find(|n| n.span == span) }
    fn find_child_label(&self, _label: &str, _parent: Option<usize>) -> Option<AstNode> { None }
}

#[derive(Clone)]
struct SchemeShape(String);

fn scheme_shape(scheme: &Scheme) -> SchemeShape { SchemeShape(display_scheme(scheme)) }

fn is_value(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Int(_) | ExprKind::Str(_) | ExprKind::Bool(_) | ExprKind::Var(_) | ExprKind::Lam(..) => true,
        ExprKind::Record(fs, None) => fs.iter().all(|f| is_value(&f.value)),
        ExprKind::Variant(_, e) | ExprKind::Ascribe(e, _) | ExprKind::Field(e, _, _) => is_value(e),
        _ => false,
    }
}

impl Engine {
    fn infer_expr(&mut self, expr: &Expr) -> Ty {
        match &expr.kind {
            ExprKind::Int(_) => self.con(expr.span.start, "Int"),
            ExprKind::Str(_) => self.con(expr.span.start, "String"),
            ExprKind::Bool(_) => self.con(expr.span.start, "Bool"),
            ExprKind::Var(name) => {
                if let Some(pos) = self.env.iter().rposition(|(n, _)| n == name) {
                    let entry = self.env[pos].1.clone();
                    let instantiated = self.instantiate(&entry.scheme, expr.span.start, entry.node_id);
                    self.add_constraint(ConstraintKind::Instantiate, expr.span.start, format!("use {name}"), entry.scheme.body.clone(), instantiated.clone());
                    self.trace(&instantiated, expr.span.start, TraceReason::Instantiated, None);
                    instantiated
                } else {
                    let expected = self.fresh_var(self.origin(expr.span.start, "unbound"));
                    self.report_seed(ErrorSeed {
                        kind: ErrorKind::UnboundVariable,
                        message: format!("unbound variable '{name}'"),
                        span: expr.span,
                        path: self.node_path(expr.span.start),
                        expected: expected.clone(),
                        actual: expected,
                        reasons: [format!("n{}:var", expr.span.start)].into_iter().collect(),
                    });
                    self.con(expr.span.start, "Error")
                }
            }
            ExprKind::Lam(params, body) => {
                let mut arg_tys = Vec::new();
                for p in params.iter() {
                    let ty = self.fresh_var(self.origin(expr.span.start, "param"));
                    if let Ty::Var(v,_) = &ty { self.remember_mono(*v); }
                    self.env.push((p.name.clone(), Entry { node_id: expr.span.start, binding: expr.span.start, scheme: mono_scheme(ty.clone()), scope: self.current_scope(), gen_env: self.gen_env_fingerprint() }));
                    arg_tys.push(ty);
                }
                let result = self.infer_expr(body);
                arg_tys.into_iter().rev().fold(result, |acc, arg| Ty::Arrow(Box::new(arg), Box::new(acc), self.origin(expr.span.start, "arrow")))
            }
            ExprKind::App(callee, args) => {
                let mut func = self.infer_expr(callee);
                let result = self.fresh_var(self.origin(expr.span.start, "result"));
                for arg_expr in args {
                    let arg = self.infer_expr(arg_expr);
                    let next = self.fresh_var(self.origin(expr.span.start, "result"));
                    let arrow = Ty::Arrow(Box::new(arg), Box::new(next.clone()), self.origin(expr.span.start, "arrow"));
                    self.add_constraint(ConstraintKind::Equal, expr.span.start, "application", func, arrow);
                    func = next;
                }
                self.add_constraint(ConstraintKind::Equal, expr.span.start, "application result", func, result.clone());
                result
            }
            ExprKind::Let(bind) => self.infer_let_bind(bind, Some(expr.span.start), false),
            ExprKind::If(c,t,e) => {
                let cv = self.infer_expr(c);
                self.add_constraint(ConstraintKind::Equal, expr.span.start, "if condition", cv, self.con(expr.span.start, "Bool"));
                let tv = self.infer_expr(t);
                let ev = self.infer_expr(e);
                self.add_constraint(ConstraintKind::Equal, expr.span.start, "if branches", tv.clone(), ev);
                tv
            }
            ExprKind::Record(fields, _) => {
                let tail = self.fresh_row_var(self.origin(expr.span.start, "row-tail"));
                self.row_reasons.entry(tail).or_default().insert(format!("n{}:record", expr.span.start));
                let fs = fields.iter().map(|f| RowField { name: f.name.clone(), name_span: f.name_span, value: self.infer_expr(&f.value) }).collect();
                self.record(expr.span.start, fs, RowTail::Open(tail))
            }
            ExprKind::Field(base, label, label_span) => {
                let record_ty = self.infer_expr(base);
                let payload = self.fresh_var(self.origin(expr.span.start, "field"));
                let tail = self.fresh_row_var(self.origin(expr.span.start, "row-tail"));
                let row = Row { fields: vec![RowField { name: label.clone(), name_span: *label_span, value: payload.clone() }], tail: RowTail::Open(tail), origin: self.origin(expr.span.start, "record") };
                self.add_constraint(ConstraintKind::Equal, expr.span.start, format!("field .{label}"), record_ty, Ty::Record(row, self.origin(expr.span.start, "record")));
                payload
            }
            ExprKind::Extend(base, fields) => {
                let base_ty = self.infer_expr(base);
                let fs = fields.iter().map(|f| RowField { name: f.name.clone(), name_span: f.name_span, value: self.infer_expr(&f.value) }).collect();
                let extended = self.record(expr.span.start, fs, RowTail::Open(self.fresh_row_var(self.origin(expr.span.start, "row-tail"))));
                self.add_constraint(ConstraintKind::RowExtend, expr.span.start, "record extension", extended, base_ty);
                extended
            }
            ExprKind::Variant(label, payload_expr) => {
                let payload = self.infer_expr(payload_expr);
                self.variant_row(expr.span.start, label, payload, RowTail::Open(self.fresh_row_var(self.origin(expr.span.start, "row-tail"))))
            }
            ExprKind::Match(scrut, arms) => {
                let scrut_ty = self.infer_expr(scrut);
                let result = self.fresh_var(self.origin(expr.span.start, "match-result"));
                for arm in arms {
                    let payload = self.fresh_var(self.origin(expr.span.start, "variant-payload"));
                    let tail = self.fresh_row_var(self.origin(expr.span.start, "row-tail"));
                    let variant = Ty::Variant(Row { fields: vec![RowField { name: arm.label.clone(), name_span: arm.label_span, value: payload.clone() }], tail: RowTail::Open(tail), origin: self.origin(expr.span.start, "variant") }, self.origin(expr.span.start, "variant"));
                    self.add_constraint(ConstraintKind::VariantMatch, expr.span.start, format!("case `{arm.label}"), scrut_ty.clone(), variant);
                    self.env.push((arm.binding.clone(), Entry { node_id: expr.span.start, binding: expr.span.start, scheme: mono_scheme(payload), scope: self.current_scope(), gen_env: self.gen_env_fingerprint() }));
                    let body_ty = self.infer_expr(&arm.body);
                    self.add_constraint(ConstraintKind::Equal, expr.span.start, "match branches", result.clone(), body_ty);
                }
                result
            }
            ExprKind::Ascribe(inner, ty) => {
                let inner_ty = self.infer_expr(inner);
                let ascribed = self.translate_type(ty, BTreeMap::new());
                self.add_constraint(ConstraintKind::Ascribe, expr.span.start, "type annotation", inner_ty, ascribed.clone());
                ascribed
            }
            ExprKind::Tuple(items) => {
                let mut fs = Vec::new();
                for (i, item) in items.iter().enumerate() {
                    fs.push(RowField { name: format!("{i}"), name_span: expr.span, value: self.infer_expr(item) });
                }
                self.record(expr.span.start, fs, RowTail::Closed)
            }
        }
    }

    fn report_seed(&mut self, seed: ErrorSeed) {
        let reasons = seed.reasons.iter().cloned().collect::<Vec<_>>();
        let core = reasons.iter().filter_map(|r| self.constraints.iter().find(|c| c.id.contains(r)).map(|c| c.id.clone())).collect();
        let id = self.errors.len();
        self.errors.push(TypeError {
            id,
            kind: seed.kind,
            message: seed.message,
            span: seed.span,
            path: seed.path,
            expected: seed.expected,
            actual: seed.actual,
            core,
            alternate_cores: vec![],
        });
    }
}

fn mono_scheme(ty: Ty) -> Scheme {
    let vars = collect_vars(&ty);
    Scheme { vars: BTreeSet::new(), monomorphic: vars, body: ty, value_restricted: true }
}

fn collect_vars(ty: &Ty) -> BTreeSet<VarId> {
    let mut out = BTreeSet::new();
    collect_vars_rec(ty, &mut out);
    out
}

fn collect_vars_rec(ty: &Ty, out: &mut BTreeSet<VarId>) {
    match ty {
        Ty::Var(v,_) => { out.insert(*v); }
        Ty::Arrow(a,b,_) => { collect_vars_rec(a,out); collect_vars_rec(b,out); }
        Ty::Record(row,_) | Ty::Variant(row,_) => {
            for f in &row.fields { collect_vars_rec(&f.value,out); }
            if let RowTail::Open(v) = row.tail { out.insert(v); }
        }
        Ty::Con(..) => {}
    }
}

impl Engine {
    fn resolve_node(&self, node_id: usize) -> usize {
        self.nodes.iter().find(|n| n.id == node_id || n.span.start == node_id).map(|n| n.id).unwrap_or(0)
    }

    fn origin_for(&mut self, node_id: usize, part: &'static str) -> Origin {
        let id = self.resolve_node(node_id);
        self.origin(id, part)
    }

    fn translate_type(&mut self, ty: &TypeExpr, mut vars: BTreeMap<String, VarId>) -> Ty {
        self.translate_type_rec(ty, &mut vars)
    }

    fn translate_type_rec(&mut self, ty: &TypeExpr, vars: &mut BTreeMap<String, VarId>) -> Ty {
        let node_id = self.resolve_node(ty.span.start);
        let origin = self.origin(node_id, "ascribed");
        match &ty.kind {
            TypeExprKind::Con(name) => Ty::Con(name.clone(), origin),
            TypeExprKind::Var(name) => {
                let next = self.fresh;
                let id = *vars.entry(name.clone()).or_insert_with(|| { self.fresh += 1; next });
                Ty::Var(id, origin)
            }
            TypeExprKind::Arrow(a,b) => {
                let av = self.translate_type_rec(a, vars);
                let bv = self.translate_type_rec(b, vars);
                Ty::Arrow(Box::new(av), Box::new(bv), origin)
            }
            TypeExprKind::Record(fields, tail) => {
                let fs = fields.iter().map(|f| RowField { name: f.name.clone(), name_span: f.name_span, value: self.translate_type_rec(&f.value, vars) }).collect();
                let row_tail = match tail {
                    Some(name) => RowTail::Open(*vars.entry(name.clone()).or_insert_with(|| { let v = self.fresh; self.fresh += 1; v })),
                    None => RowTail::Closed,
                };
                Ty::Record(Row { fields: fs, tail: row_tail, origin }, origin)
            }
            TypeExprKind::Variant(fields, tail) => {
                let fs = fields.iter().map(|f| RowField { name: f.name.clone(), name_span: f.name_span, value: self.translate_type_rec(&f.value, vars) }).collect();
                let row_tail = match tail {
                    Some(name) => RowTail::Open(*vars.entry(name.clone()).or_insert_with(|| { let v = self.fresh; self.fresh += 1; v })),
                    None => RowTail::Closed,
                };
                Ty::Variant(Row { fields: fs, tail: row_tail, origin }, origin)
            }
        }
    }

    fn instantiate(&mut self, scheme: &Scheme, node_id: usize, _binding: usize) -> Ty {
        let origin = self.origin(self.resolve_node(node_id), "instantiated");
        let mut mapping = BTreeMap::new();
        for v in &scheme.vars {
            mapping.insert(*v, self.fresh);
            self.fresh += 1;
        }
        self.subst_ty(&scheme.body, &mapping, origin)
    }

    fn subst_ty(&mut self, ty: &Ty, mapping: &BTreeMap<VarId, VarId>, origin: Origin) -> Ty {
        match ty {
            Ty::Var(v, old) => mapping.get(v).map_or_else(|| ty.clone(), |n| Ty::Var(*n, origin)),
            Ty::Con(..) => ty.clone(),
            Ty::Arrow(a,b,_) => Ty::Arrow(Box::new(self.subst_ty(a, mapping, origin)), Box::new(self.subst_ty(b, mapping, origin)), origin),
            Ty::Record(row,_) => Ty::Record(self.subst_row(row, mapping, origin), origin),
            Ty::Variant(row,_) => Ty::Variant(self.subst_row(row, mapping, origin), origin),
        }
    }

    fn subst_row(&mut self, row: &Row, mapping: &BTreeMap<VarId, VarId>, origin: Origin) -> Row {
        Row {
            fields: row.fields.iter().map(|f| RowField { name: f.name.clone(), name_span: f.name_span, value: self.subst_ty(&f.value, mapping, origin) }).collect(),
            tail: match row.tail {
                RowTail::Closed => RowTail::Closed,
                RowTail::Open(v) => RowTail::Open(mapping.get(&v).copied().unwrap_or(v)),
            },
            origin,
        }
    }

    fn make_scheme(&mut self, ty: Ty, value_restricted: bool) -> Scheme {
        let mut ambient = BTreeSet::new();
        for (_, entry) in &self.env {
            ambient.extend(collect_vars(&entry.scheme.body));
            ambient.extend(entry.scheme.vars.iter().copied());
        }
        let all = collect_vars(&ty);
        let vars = if value_restricted {
            BTreeSet::new()
        } else {
            all.difference(&ambient).copied().collect()
        };
        Scheme { vars, monomorphic: if value_restricted { all } else { BTreeSet::new() }, body: ty, value_restricted }
    }

    fn normalize(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(v, o) => match self.tsubst.get(v) {
                Some(next) => self.normalize(next),
                None => Ty::Var(*v, *o),
            },
            Ty::Con(..) => ty.clone(),
            Ty::Arrow(a,b,o) => Ty::Arrow(Box::new(self.normalize(a)), Box::new(self.normalize(b)), *o),
            Ty::Record(row,o) => Ty::Record(self.normalize_row(row), *o),
            Ty::Variant(row,o) => Ty::Variant(self.normalize_row(row), *o),
        }
    }

    fn normalize_row(&self, row: &Row) -> Row {
        let mut fields = row.fields.iter().map(|f| RowField { name: f.name.clone(), name_span: f.name_span, value: self.normalize(&f.value) }).collect::<Vec<_>>();
        let mut tail = self.normalize_tail(row.tail);
        loop {
            match tail {
                RowTail::Open(v) => match self.rsubst.get(&v) {
                    Some(next) => tail = self.normalize_tail(*next),
                    None => break,
                },
                RowTail::Closed => break,
            }
        }
        if let RowTail::Open(v) = tail {
            if let Some(Ty::Record(extra, _) | Ty::Variant(extra, _)) = self.tsubst.get(&v) {
                for f in extra.fields.clone() {
                    if !fields.iter().any(|x| x.name == f.name) { fields.push(RowField { ..f }); }
                }
                tail = self.normalize_tail(extra.tail);
            }
        }
        fields.sort_by(|a,b| a.name.cmp(&b.name));
        Row { fields, tail, origin: row.origin }
    }

    fn normalize_tail(&self, tail: RowTail) -> RowTail {
        match tail {
            RowTail::Open(v) => match self.tsubst.get(&v) {
                Some(Ty::Record(r,_) | Ty::Variant(r,_)) => self.normalize_tail(r.tail),
                _ => match self.rsubst.get(&v) { Some(t) => *t, None => RowTail::Open(v) },
            },
            closed => closed,
        }
    }
}

#[derive(Debug, Clone)]
struct UError {
    kind: ErrorKind,
    path: String,
    left: Ty,
    right: Ty,
    reasons: BTreeSet<String>,
}

impl Engine {
    fn finalize_program(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Let(bind) = item {
                if let Some(entry) = self.env.iter().rev().find(|(name, _)| *name == bind.name).map(|x| x.1.clone()) {
                    let body = self.normalize(&entry.scheme.body);
                    let scheme = Scheme { vars: entry.scheme.vars, monomorphic: entry.scheme.monomorphic, body, value_restricted: entry.scheme.value_restricted };
                    self.finals.push(FinalType { name: bind.name.clone(), span: bind.name_span, scheme });
                }
            }
        }
    }

    fn solve_all(&mut self, disabled: &BTreeSet<String>) {
        let constraints = self.constraints.clone();
        self.tsubst.clear();
        self.rsubst.clear();
        self.reasons.clear();
        self.row_reasons.clear();
        self.steps.clear();
        self.errors.clear();
        let mut raw_errors: Vec<(usize, UError)> = Vec::new();
        for c in constraints.iter() {
            if disabled.contains(&c.id) {
                self.steps.push(RawStep { kind: StepKind::Unify, constraint_id: c.id.clone(), description: format!("{} (disabled)", c.label), before: "constraint disabled".into(), after: "skipped".into(), error: None });
                continue;
            }
            let before = format!("{} ~ {}", display_ty(&c.left), display_ty(&c.right));
            if c.kind == ConstraintKind::Instantiate {
                self.steps.push(RawStep { kind: StepKind::Instantiate, constraint_id: c.id.clone(), description: c.label.clone(), before, after: "polymorphic occurrence uses fresh variables".into(), error: None });
                continue;
            }
            if c.kind == ConstraintKind::Generalize {
                self.steps.push(RawStep { kind: StepKind::Generalize, constraint_id: c.id.clone(), description: c.label.clone(), before, after: "generalized".into(), error: None });
                continue;
            }
            let result = self.unify(c.id.clone(), c.left.clone(), c.right.clone(), "$", c.kind.clone());
            let after = match &result {
                Ok(()) => "unified".into(),
                Err(e) => e.message_kind(),
            };
            if let Err(err) = result {
                let id = self.errors.len();
                let span = c.span;
                self.errors.push(TypeError {
                    id,
                    kind: err.kind.clone(),
                    message: err.message_kind(),
                    span,
                    path: err.path.clone(),
                    expected: err.left.clone(),
                    actual: err.right.clone(),
                    core: vec![c.id.clone()],
                    alternate_cores: vec![],
                });
                raw_errors.push((id, err));
            }
            self.steps.push(RawStep { kind: StepKind::Unify, constraint_id: c.id.clone(), description: c.label.clone(), before, after, error: raw_errors.last().filter(|(_, e)| false).map(|(id,_)| *id).or_else(|| if matches!(self.steps.last(), Some(_)) { None } else { None }) });
        }
        self.minimize_cores(constraints, disabled, raw_errors);
    }
}

impl UError {
    fn message_kind(&self) -> String {
        match self.kind {
            ErrorKind::ConstructorMismatch => format!("cannot unify constructors: {} versus {}", display_ty(&self.left), display_ty(&self.right)),
            ErrorKind::OccursCheck => format!("infinite type: {} occurs in {}", display_ty(&self.left), display_ty(&self.right)),
            ErrorKind::MissingFields => format!("missing fields while unifying rows: {}", display_ty(&self.left)),
            ErrorKind::ExtraFields => format!("extra fields while unifying rows: {}", display_ty(&self.right)),
            ErrorKind::FieldTypeMismatch => format!("field types differ: {} versus {}", display_ty(&self.left), display_ty(&self.right)),
            ErrorKind::VariantMissing => format!("variant case is missing: {}", display_ty(&self.right)),
            ErrorKind::VariantExtra => format!("unexpected variant case: {}", display_ty(&self.right)),
            ErrorKind::ArityMismatch => "function arity mismatch".into(),
            ErrorKind::UnboundVariable => "unbound variable".into(),
            ErrorKind::DuplicateField => "duplicate field through row extension".into(),
        }
    }
}
