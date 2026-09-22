use super::core::*;
use crate::ast::*;
use crate::engine::{Analysis, AnalysisOptions, ConstraintView, ErrorView, FinalType, StepView};
use crate::incremental::stable_hash;
use crate::types::*;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone)]
struct EnvEntry {
    scheme: Scheme,
    source_node: NodeId,
    scope_hash: u64,
}

struct InferState {
    factory: TypeFactory,
    subst: Subst,
    env: Vec<(String, EnvEntry)>,
    actions: Vec<Action>,
    constraint_number: u64,
    level: u32,
    immediate_errors: Vec<SolveError>,
}

pub fn analyze_program(program: Program, options: AnalysisOptions) -> Analysis {
    let mut state = InferState {
        factory: TypeFactory::new(),
        subst: Subst::default(),
        env: Vec::new(),
        actions: Vec::new(),
        constraint_number: 1,
        level: 1,
        immediate_errors: Vec::new(),
    };
    state.install_primitives();
    let mut result = SolveResult::default();
    let mut final_types = BTreeMap::new();

    for statement in &program.statements {
        state.level += 1;
        match &statement.kind {
            StmtKind::Let(bind) => state.bind_top_level(bind, &mut result),
            StmtKind::Expr(expr) => {
                state.infer_expr(expr, &mut result);
            }
        }
    }

    for (name, entry) in &state.env {
        let typ = state.subst.apply_type(&entry.scheme.body);
        final_types.insert(name.clone(), FinalType {
            name: name.clone(),
            node_id: entry.source_node,
            typ: display(&typ),
            scheme: entry.scheme.generalized,
            value_restricted: entry.scheme.value_restricted,
        });
    }

    let all_actions = state.actions.clone();
    let mut full_result = run_actions(&all_actions, &options.disabled_constraints);
    let existing_keys: BTreeSet<_> = full_result.errors.iter().map(|error| error.key.clone()).collect();
    for immediate in state.immediate_errors.clone() {
        if !existing_keys.contains(&immediate.key) {
            full_result.errors.push(immediate);
        }
    }
    let all_errors = full_result.errors.clone();
    let errors = build_error_views(&program, &all_actions, all_errors, &options.disabled_constraints);
    let steps = replay_steps(&all_actions, &options);
    let constraints = build_constraint_views(&all_actions, &full_result, &options.disabled_constraints);
    let versions = constraints.iter().map(|item| crate::incremental::ConstraintVersion {
        fingerprint: item.fingerprint.clone(),
        cache_key: item.cache_key.clone(),
    }).collect();

    Analysis {
        ok: errors.is_empty(),
        parse_error: None,
        program: Some(program),
        constraints,
        steps,
        errors,
        type_graph: state.factory.nodes().values().map(serde_json::to_value).flatten().collect(),
        final_types,
        constraint_versions: versions,
        diff: crate::incremental::ConstraintDiff::default(),
        node_reuse: crate::engine::NodeReuse::default(),
    }
}

impl InferState {
    fn install_primitives(&mut self) {
        for name in ["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">="] {
            let int = Type::Con("Int".into());
            let bool = Type::Con("Bool".into());
            let comparison = name == "==" || name == "!=" || name == "<" || name == "<=" || name == ">" || name == ">=";
            let body = Type::arrow(int.clone(), Type::arrow(int.clone(), if comparison { bool } else { int }));
            let scheme = Scheme { params: Vec::new(), body, generalized: false, value_restricted: false };
            self.env.push((name.into(), EnvEntry { scheme, source_node: 0, scope_hash: 0 }));
        }
    }

    fn bind_top_level(&mut self, bind: &LetBind, result: &mut SolveResult) {
        let before = self.env.len();
        if bind.rec.is_some() {
            let (var, _) = self.fresh(bind.value.node_id, format!("recursive binding {}", bind.name), self.level);
            self.env.push((bind.name.clone(), EnvEntry {
                scheme: Scheme { params: Vec::new(), body: var, generalized: false, value_restricted: !bind.is_value },
                source_node: bind.value.node_id,
                scope_hash: 0,
            }));
        }
        let inferred = self.infer_expr(&bind.value, result).0;
        self.run_pending(result);
        let scheme = self.generalize(inferred, bind.is_value);
        self.env.truncate(before);
        self.env.push((bind.name.clone(), EnvEntry { scheme, source_node: bind.value.node_id, scope_hash: 0 }));
    }

    fn run_pending(&mut self, result: &mut SolveResult) {
        let _ = result;
    }

    fn generalize(&self, typ: Type, is_value: bool) -> Scheme {
        let typ = self.subst.apply_type(&typ);
        let mut ambient_types = BTreeSet::new();
        let mut ambient_rows = BTreeSet::new();
        for (_, entry) in &self.env {
            let body = self.subst.apply_type(&entry.scheme.body);
            body.type_vars(&mut ambient_types);
            body.row_vars(&mut ambient_rows);
        }
        make_scheme(typ, true, &ambient_types, &ambient_rows, !is_value)
    }

    fn fresh(&mut self, source: NodeId, label: String, level: u32) -> (Type, TypeNodeId) {
        self.factory.fresh_type_var(BTreeSet::from([source]), label, level)
    }

    fn fresh_row(&mut self, source: NodeId, label: String) -> (RVar, TypeNodeId) {
        self.factory.fresh_row_var(BTreeSet::from([source]), label)
    }

    fn con(&mut self, name: &str, source: NodeId) -> (Type, TypeNodeId) {
        self.factory.constructor(name, BTreeSet::from([source]))
    }

    fn infer_expr(&mut self, expr: &Expr, result: &mut SolveResult) -> (Type, TypeNodeId) {
        match &expr.kind {
            ExprKind::Int(_) => self.con("Int", expr.node_id),
            ExprKind::Str(_) => self.con("String", expr.node_id),
            ExprKind::Bool(_) => self.con("Bool", expr.node_id),
            ExprKind::Var(name) => self.infer_var(name, expr.node_id),
            ExprKind::Lam { param, body } => self.infer_lam(param, body, expr.node_id),
            ExprKind::App { fun, arg } => self.infer_app(fun, arg, expr.node_id),
            ExprKind::Let { bind, body } => self.infer_let(bind, body, result),
            ExprKind::If { cond, then_branch, else_branch } => self.infer_if(cond, then_branch, else_branch, expr.node_id, result),
            ExprKind::Binary { op, left, right } => self.infer_binop(op, left, right, result),
            ExprKind::Record { fields, closed } => self.infer_record(fields, *closed, expr.node_id, result),
            ExprKind::RecordFields { record, fields, closed } => self.infer_extend(record, fields, *closed, expr.node_id, result),
            ExprKind::Field { record, name } => self.infer_field(record, name, expr.node_id, expr.span, result),
            ExprKind::Variant { label, payload, closed } => self.infer_variant(label, payload.as_deref(), closed.is_some(), expr.node_id, result),
            ExprKind::Match { scrutinee, arms } => self.infer_match(scrutinee, arms, expr.node_id, result),
        }
    }
}

impl InferState {
    fn infer_var(&mut self, name: &str, source: NodeId) -> (Type, TypeNodeId) {
        if let Some(position) = self.env.iter().rposition(|(bound, _)| bound == name) {
            let entry = self.env.get(position).unwrap().1.clone();
            let instantiated = self.instantiate(&entry.scheme, source);
            let id = stable_hash(&format!("instantiate:{}:{}", source, env_fingerprint(&self.env)));
            self.actions.push(Action::Instantiate(InstantiationStep {
                id: format!("instantiate-{id:x}"),
                name: name.into(),
                source_node: source,
                scheme: entry.scheme,
                typ: instantiated.clone(),
            }));
            (instantiated, self.node_for_origin(source))
        } else {
            let (var, node) = self.fresh(source, format!("unresolved variable {name}"), self.level);
            let id = stable_hash(&format!("unresolved:{}:{}", source, name));
            self.actions.push(Action::Unresolved { id: format!("unresolved-{id:x}"), node: source, name: name.into() });
            (var, node)
        }
    }

    fn instantiate(&mut self, scheme: &Scheme, source: NodeId) -> Type {
        let mut type_map = BTreeMap::new();
        let mut row_map = BTreeMap::new();
        for param in &scheme.params {
            match param {
                Param::Type(old) => {
                    let (new, node) = self.fresh(source, format!("instantiated {}", var_name(*old)), self.level);
                    if let Type::Var(new_var) = new { type_map.insert(*old, (new_var, node)); }
                }
                Param::Row(old) => {
                    let (new, node) = self.fresh_row(source, format!("instantiated row {}", row_name(*old)));
                    row_map.insert(*old, (new, node));
                }
            }
        }
        instantiate_type(&scheme.body, &type_map, &row_map)
    }

    fn infer_lam(&mut self, param: &str, body: &Expr, source: NodeId) -> (Type, TypeNodeId) {
        let (param_type, param_node) = self.fresh(body.node_id, format!("parameter {param}"), self.level);
        self.env.push((param.into(), EnvEntry {
            scheme: Scheme { params: Vec::new(), body: param_type.clone(), generalized: false, value_restricted: false },
            source_node: body.node_id,
            scope_hash: body.scope_hash.unwrap_or(0),
        }));
        let (body_type, body_node) = self.infer_expr(body, &mut SolveResult::default());
        self.env.pop();
        let origins = BTreeSet::from([source, body.node_id]);
        let outer = Type::App(Box::new(Type::Con("->".into())), Box::new(param_type));
        let outer_node = self.add_constructor_application("->", param_node, &origins);
        let (arrow, node) = self.factory.application(outer, body_type, origins, outer_node, body_node);
        (arrow, node)
    }

    fn infer_app(&mut self, fun_expr: &Expr, arg_expr: &Expr, source: NodeId) -> (Type, TypeNodeId) {
        let (fun_type, fun_node) = self.infer_expr(fun_expr, &mut SolveResult::default());
        let (arg_type, arg_node) = self.infer_expr(arg_expr, &mut SolveResult::default());
        let (result_type, result_node) = self.fresh(source, "application result".into(), self.level);
        let expected = Type::arrow(arg_type.clone(), result_type.clone());
        let origins = BTreeSet::from([source, fun_expr.node_id, arg_expr.node_id]);
        let outer_node = self.add_constructor_application("->", arg_node, &origins);
        let expected_node = self.factory.application(
            Type::App(Box::new(Type::Con("->".into())), Box::new(arg_type)),
            result_type.clone(),
            origins.clone(),
            outer_node,
            result_node,
        ).1;
        self.emit_equal(expected, fun_type, "function application".into(), format!("{} expects a compatible argument", source_text(fun_expr)), origins.clone(), fun_node, expected_node);
        let _ = (fun_node, expected_node);
        (self.subst.apply_type(&result_type), result_node)
    }

    fn infer_let(&mut self, bind: &LetBind, body: &Expr, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let before = self.env.len();
        if bind.rec.is_some() {
            let (var, _) = self.fresh(bind.value.node_id, format!("recursive {}", bind.name), self.level);
            self.env.push((bind.name.clone(), EnvEntry {
                scheme: Scheme { params: Vec::new(), body: var, generalized: false, value_restricted: !bind.is_value },
                source_node: bind.value.node_id,
                scope_hash: bind.value.scope_hash.unwrap_or(0),
            }));
        }
        let value_type = self.infer_expr(&bind.value, result).0;
        self.run_pending(result);
        let scheme = self.generalize(value_type, bind.is_value);
        let action_id = stable_hash(&format!("generalize:{}:{}", bind.value.node_id, env_fingerprint(&self.env)));
        self.actions.push(Action::Generalize(GeneralizationStep {
            id: format!("generalize-{action_id:x}"),
            name: bind.name.clone(),
            source_node: bind.value.node_id,
            typ: self.subst.apply_type(&scheme.body),
            scheme: scheme.clone(),
            reason: if scheme.value_restricted { "value restriction: non-generalized".into() } else { "syntactic value generalized".into() },
        }));
        self.env.truncate(before);
        self.env.push((bind.name.clone(), EnvEntry { scheme, source_node: bind.value.node_id, scope_hash: bind.value.scope_hash.unwrap_or(0) }));
        let inferred = self.infer_expr(body, result);
        self.env.truncate(before);
        inferred
    }

    fn infer_if(&mut self, cond: &Expr, then_branch: &Expr, else_branch: &Expr, source: NodeId, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let (cond_ty, _) = self.infer_expr(cond, result);
        let (then_ty, then_node) = self.infer_expr(then_branch, result);
        let (else_ty, else_node) = self.infer_expr(else_branch, result);
        let bool_ty = Type::Con("Bool".into());
        self.emit_equal(bool_ty, cond_ty, "if condition".into(), "condition must be Bool".into(), BTreeSet::from([source, cond.node_id]), 0, 0);
        self.emit_equal(then_ty.clone(), else_ty, "if branches".into(), "both branches must agree".into(), BTreeSet::from([source, then_branch.node_id, else_branch.node_id]), then_node, else_node);
        (then_ty, then_node)
    }

    fn infer_binop(&mut self, op: &str, left_expr: &Expr, right_expr: &Expr, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let (left, left_node) = self.infer_expr(left_expr, result);
        let (right, right_node) = self.infer_expr(right_expr, result);
        let is_comparison = matches!(op, "==" | "!=" | "<" | "<=" | ">" | ">=");
        let result_name = if is_comparison { "Bool" } else { "Int" };
        let result_ty = Type::Con(result_name.into());
        self.emit_equal(Type::Con("Int".into()), left.clone(), format!("left operand {op}"), "integer operand".into(), BTreeSet::from([left_expr.node_id]), left_node, 0);
        self.emit_equal(Type::Con("Int".into()), right.clone(), format!("right operand {op}"), "integer operand".into(), BTreeSet::from([right_expr.node_id]), right_node, 0);
        let node = self.factory.constructor(if is_comparison { "Bool" } else { "Int" }, BTreeSet::from([left_expr.node_id, right_expr.node_id])).1;
        let _ = (left, right, result_ty);
        self.con(if is_comparison { "Bool" } else { "Int" }, left_expr.node_id)
    }
}

impl InferState {
    fn infer_record(&mut self, fields: &[(String, Expr)], closed: bool, source: NodeId, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let mut entries = Vec::new();
        for (name, value) in fields {
            let (ty, node) = self.infer_expr(value, result);
            entries.push((name.clone(), ty, node));
        }
        let tail = if closed { None } else {
            let (row_var, _) = self.fresh_row(source, "record tail".into());
            Some(row_var)
        };
        self.factory.row_type(RowKind::Record, entries, tail, BTreeSet::from([source]))
    }

    fn infer_extend(&mut self, record_expr: &Expr, fields: &[(String, Expr)], closed: bool, source: NodeId, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let (record_ty, record_node) = self.infer_expr(record_expr, result);
        let mut entries = Vec::new();
        for (name, value) in fields {
            let (ty, node) = self.infer_expr(value, result);
            entries.push((name.clone(), ty, node));
        }
        let own_tail = if closed { None } else {
            let (rv, _) = self.fresh_row(source, "extended record tail".into());
            Some(rv)
        };
        let (own_ty, own_node) = self.factory.row_type(RowKind::Record, entries, own_tail, BTreeSet::from([source]));
        let (base_tail, _) = self.fresh_row(source, "base record tail".into());
        let base_row = Type::Record(Row {
            fields: BTreeMap::new(),
            tail: RowTail::Open(base_tail),
        });
        self.emit_equal(record_ty, base_row, "record extension base".into(), "extended value must be a record".into(), BTreeSet::from([source, record_expr.node_id]), record_node, own_node);
        (own_ty, own_node)
    }

    fn infer_field(&mut self, record_expr: &Expr, name: &str, source: NodeId, field_span: crate::lexer::Span, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let (record_ty, record_node) = self.infer_expr(record_expr, result);
        let (field_ty, field_node) = self.fresh(source, format!("field {name}"), self.level);
        let (tail, _) = self.fresh_row(source, "remaining record fields".into());
        let expected = Type::Record(Row {
            fields: BTreeMap::from([(name.into(), field_ty.clone())]),
            tail: RowTail::Open(tail),
        });
        self.emit_equal(expected, record_ty, format!("field access .{name}"), format!("record must contain field {name}"), BTreeSet::from([source, record_expr.node_id, span_node(field_span)]), 0, record_node);
        let _ = field_node;
        (field_ty, self.node_for_origin(source))
    }

    fn infer_variant(&mut self, label: &str, payload: Option<&Expr>, closed: bool, source: NodeId, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let mut entries = Vec::new();
        match payload {
            Some(expr) => {
                let (ty, node) = self.infer_expr(expr, result);
                entries.push((label.into(), ty, node));
            }
            None => {
                let (unit, node) = self.factory.row_type(RowKind::Record, Vec::new(), None, BTreeSet::from([source]));
                let _ = unit;
                entries.push((label.into(), Type::Record(Row { fields: BTreeMap::new(), tail: RowTail::Empty }), node));
            }
        }
        let tail = if closed { None } else {
            let (rv, _) = self.fresh_row(source, "variant alternatives".into());
            Some(rv)
        };
        self.factory.row_type(RowKind::Variant, entries, tail, BTreeSet::from([source]))
    }

    fn infer_match(&mut self, scrutinee_expr: &Expr, arms: &[MatchArm], source: NodeId, result: &mut SolveResult) -> (Type, TypeNodeId) {
        let (scrutinee_ty, scrutinee_node) = self.infer_expr(scrutinee_expr, result);
        let mut row_fields: BTreeMap<String, Type> = BTreeMap::new();
        let mut arm_result_types = Vec::new();
        for arm in arms {
            match &arm.pattern {
                Pattern::Wildcard(_) => {}
                Pattern::Variant { label, payload, .. } => {
                    let payload_ty = if let Some(name) = payload {
                        let (ty, _) = self.fresh(arm.body.node_id, format!("variant payload {name}"), self.level);
                        let before = self.env.len();
                        self.env.push((name.clone(), EnvEntry {
                            scheme: Scheme { params: Vec::new(), body: ty.clone(), generalized: false, value_restricted: false },
                            source_node: arm.body.node_id,
                            scope_hash: arm.body.scope_hash.unwrap_or(0),
                        }));
                        let body = self.infer_expr(&arm.body, result);
                        self.env.truncate(before);
                        arm_result_types.push(body);
                        ty
                    } else {
                        let body = self.infer_expr(&arm.body, result);
                        arm_result_types.push(body);
                        Type::Record(Row { fields: BTreeMap::new(), tail: RowTail::Empty })
                    };
                    row_fields.insert(label.clone(), payload_ty);
                }
            }
        }
        let (tail, _) = self.fresh_row(source, "unmatched alternatives".into());
        let expected_variant = Type::Variant(Row { fields: row_fields, tail: RowTail::Open(tail) });
        self.emit_equal(expected_variant, scrutinee_ty, "match scrutinee".into(), "match patterns must be constructors of the scrutinee variant".into(), BTreeSet::from([source, scrutinee_expr.node_id]), 0, scrutinee_node);
        let result_ty = if let Some((ty, node)) = arm_result_types.first() {
            for (other, other_node) in arm_result_types.iter().skip(1) {
                self.emit_equal(ty.clone(), other.clone(), "match arms".into(), "match arms must agree".into(), BTreeSet::from([source]), *node, *other_node);
            }
            ty.clone()
        } else {
            self.fresh(source, "empty match result".into(), self.level).0
        };
        (result_ty, arm_result_types.first().map(|item| item.1).unwrap_or_else(|| self.node_for_origin(source)))
    }

    fn add_constructor_application(&mut self, name: &str, arg_node: TypeNodeId, origins: &BTreeSet<NodeId>) -> TypeNodeId {
        let con_node = self.factory.constructor(name, origins.clone()).1;
        let term_node = self.factory.application(Type::Con(name.into()), Type::Var(0), origins.clone(), con_node, arg_node).1;
        term_node
    }

    fn node_for_origin(&self, origin: NodeId) -> TypeNodeId {
        self.factory.nodes().values().find(|node| node.origins.contains(&origin)).map(|node| node.id).unwrap_or(1)
    }

    fn emit_equal(&mut self, left: Type, right: Type, title: String, detail: String, sources: BTreeSet<NodeId>, _left_node: TypeNodeId, _right_node: TypeNodeId) {
        let numeric_id = self.constraint_number;
        self.constraint_number += 1;
        let left_stable = stable_type(&left);
        let right_stable = stable_type(&right);
        let primary = *sources.iter().next().unwrap_or(&0);
        let scope = sources.iter().filter_map(|node| self.env_scope_for(*node)).max().unwrap_or(0);
        let genv = env_fingerprint(&self.env);
        let fingerprint = stable_hash(&format!("eq:{primary}:{title}:{left_stable}:{right_stable}:{scope}"));
        let current_level = self.level;
        let cache_key = stable_hash(&format!("eq:{primary}:{title}:{left_stable}:{right_stable}:{scope}:{genv}:{current_level}"));
        let action = Action::Equal(EqualityConstraint {
            id: format!("eq-{fingerprint:x}"),
            numeric_id,
            cache_key: format!("cache-{cache_key:x}"),
            fingerprint: format!("fp-{fingerprint:x}"),
            left,
            right,
            title,
            detail,
            source_nodes: sources,
            disabled: false,
        });
        let mut immediate = SolveResult::default();
        execute_action(&mut self.factory, &mut self.subst, &action, &mut immediate, true, &BTreeSet::new());
        self.immediate_errors.extend(immediate.errors);
        self.actions.push(action);
    }

    fn env_scope_for(&self, node: NodeId) -> Option<u64> {
        let _ = node;
        None
    }
}

fn span_node(span: crate::lexer::Span) -> NodeId {
    stable_hash(&format!("span:{}:{}", span.start, span.end))
}

fn env_fingerprint(env: &[(String, EnvEntry)]) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for (name, entry) in env {
        let mut text = format!("{name}:{}:{}", entry.source_node, stable_type(&entry.scheme.body));
        for param in &entry.scheme.params {
            text.push(':');
            text.push_str(&format!("{param:?}"));
        }
        hash ^= stable_hash(&text);
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn source_text(expr: &Expr) -> String {
    match &expr.kind {
        ExprKind::Var(name) => name.clone(),
        _ => "function".into(),
    }
}

fn instantiate_type(ty: &Type, type_map: &BTreeMap<TVar, (TVar, TypeNodeId)>, row_map: &BTreeMap<RVar, (RVar, TypeNodeId)>) -> Type {
    match ty {
        Type::Var(var) => type_map.get(var).map(|(new, _)| Type::Var(*new)).unwrap_or_else(|| ty.clone()),
        Type::Con(_) => ty.clone(),
        Type::App(left, right) => Type::App(Box::new(instantiate_type(left, type_map, row_map)), Box::new(instantiate_type(right, type_map, row_map))),
        Type::Record(row) | Type::Variant(row) => {
            let fields = row.fields.iter().map(|(name, ty)| (name.clone(), instantiate_type(ty, type_map, row_map))).collect();
            let tail = match row.tail {
                RowTail::Empty => RowTail::Empty,
                RowTail::Open(var) => RowTail::Open(row_map.get(&var).map(|(new, _)| *new).unwrap_or(var)),
            };
            if matches!(ty, Type::Record(_)) { Type::Record(Row { fields, tail }) } else { Type::Variant(Row { fields, tail }) }
        }
    }
}

fn execute_action(
    factory: &mut TypeFactory,
    subst: &mut Subst,
    action: &Action,
    result: &mut SolveResult,
    enabled: bool,
    user_disabled: &BTreeSet<String>,
) {
    match action {
        Action::Equal(constraint) => {
            let disabled = !enabled || user_disabled.contains(&constraint.id);
            if disabled {
                result.disabled.insert(constraint.id.clone());
            } else {
                match unify(factory, subst, &constraint.left, &constraint.right, &constraint.source_nodes) {
                    Ok(()) => { result.successful.insert(constraint.id.clone()); }
                    Err(mut error) => {
                        error.constraint_id = Some(constraint.id.clone());
                        error.focus_span = None;
                        result.errors.push(error);
                        result.skipped.insert(constraint.id.clone());
                    }
                }
            }
        }
        Action::Unresolved { id, node, name } => {
            if !enabled || user_disabled.contains(id) {
                result.disabled.insert(id.clone());
            } else {
                result.errors.push(SolveError {
                    key: format!("unresolved:{}:{}", node, name),
                    kind: SolveErrorKind::Unresolved,
                    message: format!("unbound name `{name}`"),
                    source_nodes: BTreeSet::from([*node]),
                    constraint_id: Some(id.clone()),
                    missing_fields: Vec::new(),
                    extra_fields: Vec::new(),
                    expected: None,
                    actual: None,
                    focus_span: None,
                    trace_type_vars: BTreeSet::new(),
                    trace_row_vars: BTreeSet::new(),
                });
                result.skipped.insert(id.clone());
            }
        }
        Action::Instantiate(_) | Action::Generalize(_) => {}
    }
}

fn run_actions(actions: &[Action], user_disabled: &BTreeSet<String>) -> SolveResult {
    let mut factory = TypeFactory::new();
    let mut subst = Subst::default();
    let mut result = SolveResult::default();
    for action in actions {
        execute_action(&mut factory, &mut subst, action, &mut result, true, user_disabled);
    }
    result
}

fn build_constraint_views(actions: &[Action], result: &SolveResult, user_disabled: &BTreeSet<String>) -> Vec<ConstraintView> {
    let mut views = Vec::new();
    for action in actions {
        match action {
            Action::Equal(constraint) => {
                let id = constraint.id.clone();
                let status = if user_disabled.contains(&id) || result.disabled.contains(&id) {
                    "disabled"
                } else if result.skipped.contains(&id) {
                    "failed"
                } else {
                    "solved"
                };
                views.push(ConstraintView {
                    id,
                    numeric_id: constraint.numeric_id,
                    kind: "equality".into(),
                    title: constraint.title.clone(),
                    detail: constraint.detail.clone(),
                    status: status.into(),
                    source_nodes: constraint.source_nodes.clone(),
                    cache_key: constraint.cache_key.clone(),
                    fingerprint: constraint.fingerprint.clone(),
                    disabled: user_disabled.contains(&constraint.id),
        });
            }
            Action::Instantiate(step) => views.push(ConstraintView {
                id: step.id.clone(),
                numeric_id: 0,
                kind: "instantiation".into(),
                title: format!("instantiate {}", step.name),
                detail: display(&step.typ),
                status: "instantiated".into(),
                source_nodes: BTreeSet::from([step.source_node]),
                cache_key: format!("gen-{}", step.id),
                fingerprint: format!("fp-{}", step.id),
                disabled: false,
            }),
            Action::Generalize(step) => views.push(ConstraintView {
                id: step.id.clone(),
                numeric_id: 0,
                kind: "generalization".into(),
                title: format!("generalize {}", step.name),
                detail: step.reason.clone(),
                status: if step.scheme.value_restricted { "value_restricted" } else { "generalized" }.into(),
                source_nodes: BTreeSet::from([step.source_node]),
                cache_key: format!("gen-{}", step.id),
                fingerprint: format!("fp-{}", step.id),
                disabled: false,
            }),
            Action::Unresolved { id, node, name } => views.push(ConstraintView {
                id: id.clone(),
                numeric_id: 0,
                kind: "unresolved".into(),
                title: format!("unbound {name}"),
                detail: format!("{name} is not in scope"),
                status: if user_disabled.contains(id) { "disabled" } else { "failed" }.into(),
                source_nodes: BTreeSet::from([*node]),
                cache_key: format!("unresolved-{node}-{name}"),
                fingerprint: format!("fp-unresolved-{node}-{name}"),
                disabled: user_disabled.contains(id),
            }),
        }
    }
    views
}

fn error_span(program: &Program, nodes: &BTreeSet<NodeId>) -> Option<crate::lexer::Span> {
    nodes.iter().filter_map(|node| program.node_ids.get(node)).map(|info| info.span).min_by_key(|span| (span.start, span.end))
}

fn related_constraints_for(actions: &[Action], error: &SolveError) -> Vec<String> {
    actions.iter().filter_map(|action| {
        let id = action.id().to_string();
        let relevant_nodes = match action {
            Action::Equal(constraint) => constraint.source_nodes.intersection(&error.source_nodes).next().is_some(),
            Action::Instantiate(step) => error.source_nodes.contains(&step.source_node),
            Action::Generalize(step) => error.source_nodes.contains(&step.source_node),
            Action::Unresolved { node, .. } => error.source_nodes.contains(node),
        };
        let relevant_types = match action {
            Action::Equal(constraint) => {
                let mut vars = BTreeSet::new();
                constraint.left.type_vars(&mut vars);
                constraint.right.type_vars(&mut vars);
                vars.intersection(&error.trace_type_vars).next().is_some()
                    || row_var_intersection(constraint, &error.trace_row_vars)
            }
            _ => false,
        };
        (relevant_nodes || relevant_types).then_some(id)
    }).collect()
}

fn row_var_intersection(constraint: &EqualityConstraint, wanted: &BTreeSet<RVar>) -> bool {
    let mut vars = BTreeSet::new();
    constraint.left.row_vars(&mut vars);
    constraint.right.row_vars(&mut vars);
    vars.intersection(wanted).next().is_some()
}

fn find_cores(actions: &[Action], error: &SolveError, user_disabled: &BTreeSet<String>) -> Vec<Vec<String>> {
    let equality_ids: Vec<String> = actions.iter().filter_map(|action| match action {
        Action::Equal(constraint) if !user_disabled.contains(&constraint.id) => Some(constraint.id.clone()),
        Action::Unresolved { id, .. } if !user_disabled.contains(id) => Some(id.clone()),
        _ => None,
    }).collect();
    let related = related_constraints_for(actions, error);
    let mut universe: Vec<String> = related.into_iter().filter(|id| equality_ids.contains(id)).collect();
    universe.sort();
    universe.dedup();
    if universe.is_empty() {
        if let Some(id) = &error.constraint_id { return vec![vec![id.clone()]]; }
        return Vec::new();
    }
    if universe.len() > 14 {
        universe.truncate(14);
    }

    let mut seed = universe.clone();
    for id in universe.clone() {
        let trial: BTreeSet<String> = seed.iter().filter(|candidate| **candidate != id).chain(user_disabled.iter()).cloned().collect();
        let trial_result = run_actions(actions, &trial);
        if trial_result.errors.iter().any(|other| other.key == error.key) {
            seed.retain(|candidate| candidate != &id);
        }
    }

    let mut cores = Vec::new();
    'outer: for size in 1..=seed.len() {
        let mut indices = vec![0usize; size];
        loop {
            let candidate: Vec<String> = indices.iter().map(|index| seed[*index].clone()).collect();
            let disabled: BTreeSet<String> = universe.iter().filter(|id| !candidate.contains(id)).cloned().chain(user_disabled.iter().cloned()).collect();
            let trial = run_actions(actions, &disabled);
            if trial.errors.iter().any(|other| other.key == error.key) {
                if !cores.contains(&candidate) {
                    cores.push(candidate);
                }
            }
            if cores.len() >= 8 { break 'outer; }
            if !increment_indices(&mut indices, seed.len()) { break; }
        }
    }
    if cores.is_empty() {
        if let Some(id) = error.constraint_id.clone() { cores.push(vec![id]); }
    }
    cores
}

fn increment_indices(indices: &mut [usize], len: usize) -> bool {
    let mut position = indices.len();
    while position > 0 {
        position -= 1;
        indices[position] += 1;
        if indices[position] < len && indices[position] + indices.len() - position <= len {
            for later in position + 1..indices.len() {
                indices[later] = indices[later - 1] + 1;
            }
            if indices.iter().all(|index| *index < len) { return true; }
        }
    }
    false
}

fn build_error_views(program: &Program, actions: &[Action], errors: Vec<SolveError>, user_disabled: &BTreeSet<String>) -> Vec<ErrorView> {
    let mut grouped = BTreeMap::new();
    for error in errors {
        grouped.entry(error.key.clone()).or_insert_with(Vec::new).push(error);
    }
    grouped.into_values().map(|mut group| {
        let primary = group.remove(0);
        let mut source_nodes = primary.source_nodes.clone();
        for other in &group { source_nodes.extend(other.source_nodes.iter().copied()); }
        let related = related_constraints_for(actions, &primary);
        let cores = find_cores(actions, &primary, user_disabled);
        ErrorView {
            key: primary.key,
            kind: format!("{:?}", primary.kind).to_lowercase(),
            message: primary.message,
            span: error_span(program, &source_nodes),
            source_nodes,
            missing_fields: primary.missing_fields,
            extra_fields: primary.extra_fields,
            expected: primary.expected,
            actual: primary.actual,
            cores,
            related_constraints: related,
        }
    }).collect()
}

fn replay_steps(actions: &[Action], options: &AnalysisOptions) -> Vec<StepView> {
    let mut steps = Vec::new();
    let mut factory = TypeFactory::new();
    let mut subst = Subst::default();
    let mut result = SolveResult::default();
    for action in actions {
        if options.until_step.is_some_and(|limit| steps.len() >= limit) {
            break;
        }
        let index = steps.len() + 1;
        match action {
            Action::Equal(constraint) => {
                execute_action(&mut factory, &mut subst, action, &mut result, true, &options.disabled_constraints);
                let status = if options.disabled_constraints.contains(&constraint.id) {
                    "disabled"
                } else if result.skipped.contains(&constraint.id) {
                    "failed"
                } else {
                    "unified"
                };
                steps.push(StepView {
                    index,
                    kind: "unify".into(),
                    constraint_id: Some(constraint.id.clone()),
                    title: constraint.title.clone(),
                    detail: format!("{} ≡ {}", display(&subst.apply_type(&constraint.left)), display(&subst.apply_type(&constraint.right))),
                    status: status.into(),
                });
            }
            Action::Instantiate(step) => steps.push(StepView {
                index,
                kind: "instantiate".into(),
                constraint_id: Some(step.id.clone()),
                title: format!("instantiate {}", step.name),
                detail: display(&step.typ),
                status: "instantiated".into(),
            }),
            Action::Generalize(step) => steps.push(StepView {
                index,
                kind: "generalize".into(),
                constraint_id: Some(step.id.clone()),
                title: format!("generalize {}", step.name),
                detail: format!("{} — {}", display(&step.scheme.body), step.reason),
                status: if step.scheme.value_restricted { "value_restricted" } else { "generalized" }.into(),
            }),
            Action::Unresolved { id, name, .. } => {
                execute_action(&mut factory, &mut subst, action, &mut result, true, &options.disabled_constraints);
                steps.push(StepView {
                    index,
                    kind: "unresolved".into(),
                    constraint_id: Some(id.clone()),
                    title: format!("unbound {name}"),
                    detail: format!("{name} is not in scope"),
                    status: if options.disabled_constraints.contains(id) { "disabled" } else { "failed" }.into(),
                });
            }
        }
    }
    steps
}
