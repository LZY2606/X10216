use crate::ast::*;
use crate::incremental::stable_hash;
use std::collections::BTreeMap;

struct Entry {
    id: BinderId,
    name: String,
}

pub struct RawProgram {
    pub statements: Vec<Stmt>,
}

pub fn prepare_program(raw: RawProgram, source: &str) -> Program {
    let RawProgram { mut statements } = raw;
    let mut binder_names = BTreeMap::new();
    for (index, stmt) in statements.iter_mut().enumerate() {
        assign_stmt(&format!("s{index}"), stmt, &mut binder_names);
    }

    let mut nodes = BTreeMap::new();
    let mut scope_hashes = BTreeMap::new();
    let mut scope = Vec::new();
    for (index, stmt) in statements.iter_mut().enumerate() {
        resolve_stmt(&format!("s{index}"), stmt, &mut scope, source, &mut nodes, &mut scope_hashes);
    }
    Program { statements, node_ids: nodes, binders: binder_names, scope_hashes }
}

fn assign_stmt(path: &str, stmt: &mut Stmt, names: &mut BTreeMap<BinderId, String>) {
    match &mut stmt.kind {
        StmtKind::Let(bind) => {
            let key = format!("{path}.let.name");
            let id = stable_hash(&key);
            bind.value.binder_id = Some(id);
            bind.value.binder_key = Some(key);
            names.insert(id, bind.name.clone());
            let mut scope = Vec::new();
            if bind.rec.is_some() {
                scope.push(id);
            }
            assign_expr(&format!("{path}.value"), &mut bind.value, names, &scope);
        }
        StmtKind::Expr(expr) => assign_expr(path, expr, names, &[]),
    }
}

fn assign_expr(path: &str, expr: &mut Expr, names: &mut BTreeMap<BinderId, String>, scope: &[BinderId]) {
    let mut next_scope = scope.to_vec();
    match &mut expr.kind {
        ExprKind::Lam { param, body } => {
            let key = format!("{path}.lam.param");
            let id = stable_hash(&key);
            expr.binder_id = Some(id);
            expr.binder_key = Some(key);
            names.insert(id, param.clone());
            next_scope.push(id);
            assign_expr(&format!("{path}.body"), body, names, &next_scope);
        }
        ExprKind::Let { bind, body } => {
            let key = format!("{path}.let.name");
            let id = stable_hash(&key);
            bind.value.binder_id = Some(id);
            bind.value.binder_key = Some(key);
            names.insert(id, bind.name.clone());
            let mut value_scope = next_scope.clone();
            if bind.rec.is_some() {
                value_scope.push(id);
            }
            assign_expr(&format!("{path}.value"), &mut bind.value, names, &value_scope);
            next_scope.push(id);
            assign_expr(&format!("{path}.body"), body, names, &next_scope);
        }
        ExprKind::App { fun, arg } => {
            assign_expr(&format!("{path}.fun"), fun, names, &next_scope);
            assign_expr(&format!("{path}.arg"), arg, names, &next_scope);
        }
        ExprKind::If { cond, then_branch, else_branch } => {
            assign_expr(&format!("{path}.cond"), cond, names, &next_scope);
            assign_expr(&format!("{path}.then"), then_branch, names, &next_scope);
            assign_expr(&format!("{path}.else"), else_branch, names, &next_scope);
        }
        ExprKind::Binary { left, right, .. } => {
            assign_expr(&format!("{path}.left"), left, names, &next_scope);
            assign_expr(&format!("{path}.right"), right, names, &next_scope);
        }
        ExprKind::Record { fields, .. } => {
            for (index, (_, value)) in fields.iter_mut().enumerate() {
                assign_expr(&format!("{path}.field{index}.value"), value, names, &next_scope);
            }
        }
        ExprKind::RecordFields { record, fields, .. } => {
            assign_expr(&format!("{path}.record"), record, names, &next_scope);
            for (index, (_, value)) in fields.iter_mut().enumerate() {
                assign_expr(&format!("{path}.field{index}.value"), value, names, &next_scope);
            }
        }
        ExprKind::Field { record, .. } => {
            assign_expr(&format!("{path}.record"), record, names, &next_scope);
        }
        ExprKind::Variant { payload, .. } => {
            if let Some(value) = payload {
                assign_expr(&format!("{path}.payload"), value, names, &next_scope);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            assign_expr(&format!("{path}.scrutinee"), scrutinee, names, &next_scope);
            for (index, arm) in arms.iter_mut().enumerate() {
                let mut arm_scope = next_scope.clone();
                if let Pattern::Variant { payload: Some(name), .. } = &arm.pattern {
                    let key = format!("{path}.arm{index}.payload");
                    let id = stable_hash(&key);
                    arm_scope.push(id);
                    names.insert(id, name.clone());
                }
                assign_expr(&format!("{path}.arm{index}.body"), &mut arm.body, names, &arm_scope);
            }
        }
        ExprKind::Int(_) | ExprKind::Str(_) | ExprKind::Bool(_) | ExprKind::Var(_) => {}
    }
}

fn resolve_stmt(
    path: &str,
    stmt: &mut Stmt,
    scope: &mut Vec<Entry>,
    source: &str,
    nodes: &mut BTreeMap<NodeId, NodeInfo>,
    hashes: &mut BTreeMap<NodeId, u64>,
) {
    match &mut stmt.kind {
        StmtKind::Let(bind) => {
            let before = scope.len();
            if bind.rec.is_some() {
                if let Some(id) = bind.value.binder_id {
                    scope.push(Entry { id, name: bind.name.clone() });
                }
            }
            resolve_expr(&format!("{path}.value"), &mut bind.value, scope, source, nodes, hashes);
            scope.truncate(before);
            if let Some(id) = bind.value.binder_id {
                scope.push(Entry { id, name: bind.name.clone() });
            }
        }
        StmtKind::Expr(expr) => resolve_expr(path, expr, scope, source, nodes, hashes),
    }
    let scope_hash = scope_hash(scope);
    let id = stable_hash(&format!("{path}:stmt:{scope_hash}"));
    stmt.node_id = id;
    hashes.insert(id, scope_hash);
    nodes.insert(
        id,
        NodeInfo {
            span: stmt.span,
            kind: if matches!(stmt.kind, StmtKind::Let(_)) { "let" } else { "statement" }.into(),
            text: String::new(),
            binder_id: None,
            ref_binder: None,
            scope_hash: Some(scope_hash),
        },
    );
}

fn resolve_expr_internal(
    path: &str,
    expr: &mut Expr,
    scope: &mut Vec<Entry>,
    source: &str,
    nodes: &mut BTreeMap<NodeId, NodeInfo>,
    hashes: &mut BTreeMap<NodeId, u64>,
) {
    resolve_expr_with_restore(path, expr, scope, source, nodes, hashes, true);
}

fn resolve_expr_with_restore(
    path: &str,
    expr: &mut Expr,
    scope: &mut Vec<Entry>,
    source: &str,
    nodes: &mut BTreeMap<NodeId, NodeInfo>,
    hashes: &mut BTreeMap<NodeId, u64>,
    restore: bool,
) {
    let before = scope.len();
    match &mut expr.kind {
        ExprKind::Lam { param, body } => {
            if let Some(id) = expr.binder_id {
                scope.push(Entry { id, name: param.clone() });
            }
            resolve_child(&format!("{path}.body"), body, scope, source, nodes, hashes);
        }
        ExprKind::Let { bind, body } => {
            let value_before = scope.len();
            if bind.rec.is_some() {
                if let Some(id) = bind.value.binder_id {
                    scope.push(Entry { id, name: bind.name.clone() });
                }
            }
            resolve_child(&format!("{path}.value"), &mut bind.value, scope, source, nodes, hashes);
            scope.truncate(value_before);
            if let Some(id) = bind.value.binder_id {
                scope.push(Entry { id, name: bind.name.clone() });
            }
            resolve_child(&format!("{path}.body"), body, scope, source, nodes, hashes);
        }
        ExprKind::App { fun, arg } => {
            resolve_child(&format!("{path}.fun"), fun, scope, source, nodes, hashes);
            resolve_child(&format!("{path}.arg"), arg, scope, source, nodes, hashes);
        }
        ExprKind::If { cond, then_branch, else_branch } => {
            resolve_child(&format!("{path}.cond"), cond, scope, source, nodes, hashes);
            resolve_child(&format!("{path}.then"), then_branch, scope, source, nodes, hashes);
            resolve_child(&format!("{path}.else"), else_branch, scope, source, nodes, hashes);
        }
        ExprKind::Binary { left, right, .. } => {
            resolve_child(&format!("{path}.left"), left, scope, source, nodes, hashes);
            resolve_child(&format!("{path}.right"), right, scope, source, nodes, hashes);
        }
        ExprKind::Record { fields, .. } => {
            for (index, (_, value)) in fields.iter_mut().enumerate() {
                resolve_child(&format!("{path}.field{index}.value"), value, scope, source, nodes, hashes);
            }
        }
        ExprKind::RecordFields { record, fields, .. } => {
            resolve_child(&format!("{path}.record"), record, scope, source, nodes, hashes);
            for (index, (_, value)) in fields.iter_mut().enumerate() {
                resolve_child(&format!("{path}.field{index}.value"), value, scope, source, nodes, hashes);
            }
        }
        ExprKind::Field { record, .. } => {
            resolve_child(&format!("{path}.record"), record, scope, source, nodes, hashes);
        }
        ExprKind::Variant { payload, .. } => {
            if let Some(value) = payload {
                resolve_child(&format!("{path}.payload"), value, scope, source, nodes, hashes);
            }
        }
        ExprKind::Match { scrutinee, arms } => {
            resolve_child(&format!("{path}.scrutinee"), scrutinee, scope, source, nodes, hashes);
            for (index, arm) in arms.iter_mut().enumerate() {
                let arm_before = scope.len();
                if let Pattern::Variant { payload: Some(name), .. } = &arm.pattern {
                    let key = format!("{path}.arm{index}.payload");
                    scope.push(Entry { id: stable_hash(&key), name: name.clone() });
                }
                resolve_child(&format!("{path}.arm{index}.body"), &mut arm.body, scope, source, nodes, hashes);
                scope.truncate(arm_before);
            }
        }
        ExprKind::Var(name) => {
            expr.ref_binder = scope.iter().rev().find(|entry| &entry.name == name).map(|entry| entry.id);
        }
        ExprKind::Int(_) | ExprKind::Str(_) | ExprKind::Bool(_) => {}
    }

    let scope_h = scope_hash(scope);
    expr.scope_hash = Some(scope_h);
    let descriptor = descriptor(path, expr, scope_h);
    let id = stable_hash(&descriptor);
    expr.node_id = id;
    hashes.insert(id, scope_h);
    nodes.insert(
        id,
        NodeInfo {
            span: expr.span,
            kind: kind_name(expr).to_string(),
            text: source[expr.span.start..expr.span.end].to_string(),
            binder_id: expr.binder_id,
            ref_binder: expr.ref_binder,
            scope_hash: Some(scope_h),
        },
    );
    if restore {
        scope.truncate(before);
    }
}

fn resolve_child(
    path: &str,
    expr: &mut Expr,
    scope: &mut Vec<Entry>,
    source: &str,
    nodes: &mut BTreeMap<NodeId, NodeInfo>,
    hashes: &mut BTreeMap<NodeId, u64>,
) {
    resolve_expr(path, expr, scope, source, nodes, hashes);
}

fn descriptor(path: &str, expr: &Expr, scope_h: u64) -> String {
    let mut text = format!("{path}|{}|scope={scope_h}", kind_name(expr));
    if let Some(id) = expr.binder_id {
        text.push_str(&format!("|binder={id}"));
    }
    if let Some(id) = expr.ref_binder {
        text.push_str(&format!("|ref={id}"));
    }
    match &expr.kind {
        ExprKind::Int(value) => text.push_str(&format!("|int={value}")),
        ExprKind::Str(value) => text.push_str(&format!("|str={}", stable_hash(value))),
        ExprKind::Bool(value) => text.push_str(&format!("|bool={value}")),
        ExprKind::Var(name) if expr.ref_binder.is_none() => text.push_str(&format!("|unresolved={name}")),
        ExprKind::Binary { op, .. } => text.push_str(&format!("|op={op}")),
        ExprKind::Field { name, .. } => text.push_str(&format!("|field={name}")),
        ExprKind::Record { fields, closed } => {
            for (name, _) in fields {
                text.push_str(&format!("|f={name}"));
            }
            text.push_str(&format!("|closed={closed}"));
        }
        ExprKind::RecordFields { fields, closed, .. } => {
            for (name, _) in fields {
                text.push_str(&format!("|ef={name}"));
            }
            text.push_str(&format!("|eclosed={closed}"));
        }
        ExprKind::Variant { label, closed, .. } => {
            text.push_str(&format!("|variant={label}|vclosed={}", closed.is_some()));
        }
        _ => {}
    }
    text
}

fn kind_name(expr: &Expr) -> &'static str {
    match expr.kind {
        ExprKind::Int(_) => "int",
        ExprKind::Str(_) => "string",
        ExprKind::Bool(_) => "bool",
        ExprKind::Var(_) => "var",
        ExprKind::Lam { .. } => "lambda",
        ExprKind::App { .. } => "application",
        ExprKind::Let { .. } => "let",
        ExprKind::If { .. } => "if",
        ExprKind::Binary { .. } => "binary",
        ExprKind::Record { .. } => "record",
        ExprKind::RecordFields { .. } => "record_extension",
        ExprKind::Field { .. } => "field_access",
        ExprKind::Variant { .. } => "variant",
        ExprKind::Match { .. } => "match",
    }
}

fn scope_hash(scope: &[Entry]) -> u64 {
    let mut combined = 0xcbf29ce484222325u64;
    for entry in scope {
        combined ^= entry.id;
        combined = combined.wrapping_mul(0x100000001b3);
    }
    combined
}
