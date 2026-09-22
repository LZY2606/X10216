//! End-to-end analysis pipeline: parse -> generate -> solve -> cores,
//! plus the JSON view models consumed by the web UI.

use std::collections::{HashMap, HashSet};

use serde::Serialize;
use serde_json::Value;

use crate::ast::{collect_nodes, Expr, ExprNode, NodeId, Program};
use crate::constraints::{CacheKey, Chunk, Op, Reason, Segment};
use crate::cores::compute_cores;
use crate::gen::{self, GeneratedProgram};
use crate::solver::{Solver, TypeErrorKind, Step};
use crate::types::{Row, Type, TV};

/// Persistent incremental cache, versioned independently of solving.
#[derive(Default)]
pub struct IncrementalCache {
    pub chunks: HashMap<CacheKey, Chunk>,
}

#[derive(Serialize, Clone, Debug)]
pub struct TypeNodeJSON {
    pub kind: String,
    pub name: Option<String>,
    pub label: Option<String>,
    pub children: Vec<TypeNodeJSON>,
    /// provenance: expression nodes that produced this part
    pub origins: Vec<u64>,
}

#[derive(Serialize, Clone, Debug)]
pub struct ConstraintNode {
    pub id: u64,
    pub owner: u64,
    pub kind: String,
    pub label: String,
    pub status: String,
    pub vars: Vec<String>,
}

#[derive(Serialize, Clone, Debug)]
pub struct ConstraintEdge {
    pub from: u64,
    pub to: u64,
    pub shared: String,
}

#[derive(Serialize, Clone, Debug)]
pub struct ErrorJSON {
    pub anchor: u64,
    pub kind: String,
    pub message: String,
    pub detail: String,
    pub nodes: Vec<u64>,
    pub spans: Vec<[usize; 2]>,
    pub cores: Vec<Vec<u64>>,
}

#[derive(Serialize, Clone, Debug)]
pub struct VersionDiff {
    pub reused: Vec<u64>,
    pub invalidated: Vec<u64>,
    pub added: Vec<u64>,
    /// constraint ids grouped by status for the two-version comparison
    pub constraint_reuse: HashMap<String, Vec<u64>>,
}

#[derive(Serialize, Clone, Debug)]
pub struct AnalysisResult {
    pub ast: Value,
    pub nodes: Vec<Value>,
    pub constraint_nodes: Vec<ConstraintNode>,
    pub constraint_edges: Vec<ConstraintEdge>,
    pub steps: Vec<Step>,
    pub errors: Vec<ErrorJSON>,
    pub final_type: TypeNodeJSON,
    pub final_type_string: String,
    pub var_names: HashMap<String, String>,
    pub root_result: u32,
    pub diff_from_previous: Option<VersionDiff>,
    pub total_ops: usize,
    pub disabled: Vec<u64>,
}

struct Pipeline {
    program: Program,
    generated: GeneratedProgram,
    membership: HashMap<u64, NodeId>,
    node_spans: HashMap<NodeId, [usize; 2]>,
}

fn run_pipeline(program: Program, cache: &mut IncrementalCache) -> Pipeline {
    let (generated, new_cache) = gen::generate(&program, std::mem::take(&mut cache.chunks));
    cache.chunks = new_cache;
    let membership = generated.membership.clone();
    let mut node_spans = HashMap::new();
    if let Some(root) = program.root.as_ref() {
        for n in collect_nodes(root) {
            node_spans.insert(n.id, [n.span.start, n.span.end]);
        }
    }
    Pipeline { program, generated, membership, node_spans }
}

/// Full analysis at a given step cursor with a set of disabled constraints.
pub fn analyze(
    source: &str,
    cache: &mut IncrementalCache,
    disabled: &HashSet<u64>,
    cursor: Option<usize>,
    previous_constraint_ids: Option<&HashSet<u64>>,
) -> AnalysisResult {
    let program = crate::parser::parse(source);
    let pipe = run_pipeline(program, cache);

    let mut solver = Solver::new();
    let run = solver.run(&pipe.generated.flat_ops, disabled, cursor);

    // final type
    let root_tv = crate::types::TV(pipe.generated.root_result);
    let final_ty = solver.final_type(root_tv);
    let names = solver.names();
    let origins_map = build_origins(&solver, &pipe);
    let final_type_json = type_json(&final_ty, &origins_map);
    let tv_names: HashMap<TV, String> = names.iter().map(|(k, v)| (TV(*k), v.clone())).collect();
    let final_type_string = final_ty.render(&tv_names);

    // errors + cores
    let anchors: Vec<u64> = run.errors.iter().map(|e| e.anchor).collect();
    let mut cores_all = compute_cores(&pipe.generated.flat_ops, &anchors, disabled);
    if cores_all.is_empty() && !anchors.is_empty() {
        cores_all = anchors.iter().map(|a| vec![*a]).collect();
    }

    let mut errors = Vec::new();
    for err in &run.errors {
        let kind = match &err.kind {
            TypeErrorKind::Mismatch { .. } => "mismatch",
            TypeErrorKind::InfiniteType { .. } => "infinite",
            TypeErrorKind::RowMissing { .. } => "row-missing",
            TypeErrorKind::RowExtra { .. } => "row-extra",
            TypeErrorKind::Arity { .. } => "arity",
        };
        let message = match &err.kind {
            TypeErrorKind::Mismatch { detail, .. } => detail.clone(),
            TypeErrorKind::InfiniteType { detail, .. } => detail.clone(),
            TypeErrorKind::RowMissing { label, present } => format!(
                "缺少字段或分支 '{label}'（当前有: {}）",
                present.join(", ")
            ),
            TypeErrorKind::RowExtra { label, present } => format!(
                "存在多余字段或分支 '{label}'（当前有: {}）",
                present.join(", ")
            ),
            TypeErrorKind::Arity { expected, actual } => {
                format!("参数数量不符: 需要 {expected}, 得到 {actual}")
            }
        };
        let spans = err
            .nodes
            .iter()
            .filter_map(|n| pipe.node_spans.get(n).copied())
            .collect();
        let related: Vec<Vec<u64>> = cores_all
            .iter()
            .filter(|c| c.contains(&err.anchor))
            .cloned()
            .collect();
        errors.push(ErrorJSON {
            anchor: err.anchor,
            kind: kind.into(),
            message,
            detail: err.detail.clone(),
            nodes: err.nodes.iter().map(|n| n.0).collect(),
            spans,
            cores: related,
        });
    }

    // constraint graph
    let (constraint_nodes, constraint_edges) = build_graph(
        &pipe,
        &run.failed,
        disabled,
        &names,
        cursor.unwrap_or(pipe.generated.flat_ops.len()),
    );

    // ast view
    let ast_view = pipe.program.root.as_ref().map(|r| ast_json(r)).unwrap_or(serde_json::json!({}));
    let mut nodes = Vec::new();
    if let Some(root) = pipe.program.root.as_ref() {
        for n in collect_nodes(root) {
            nodes.push(serde_json::json!({
                "id": n.id.0,
                "span": [n.span.start, n.span.end],
                "kind": expr_kind(&n.expr),
                "label": expr_label(&n.expr),
            }));
        }
    }

    // diff vs previous version
    let diff_from_previous = previous_constraint_ids.map(|prev| diff(prev, &pipe, disabled));

    let var_names = names.into_iter().map(|(k, v)| (k.to_string(), v)).collect();

    AnalysisResult {
        ast: ast_view,
        nodes,
        constraint_nodes,
        constraint_edges,
        steps: run.steps,
        errors,
        final_type: final_type_json,
        final_type_string,
        var_names,
        root_result: pipe.generated.root_result,
        diff_from_previous,
        total_ops: pipe.generated.flat_ops.len(),
        disabled: disabled.iter().copied().collect(),
    }
}

fn diff(prev: &HashSet<u64>, pipe: &Pipeline, disabled: &HashSet<u64>) -> VersionDiff {
    let current: HashSet<u64> = pipe
        .generated
        .flat_ops
        .iter()
        .filter_map(|o| if let Op::Unify { id, .. } = o { Some(*id) } else { None })
        .collect();
    let reused: Vec<u64> = current.intersection(prev).copied().collect();
    let invalidated: Vec<u64> = prev.difference(&current).copied().collect();
    let added: Vec<u64> = current.difference(prev).copied().collect();
    let mut constraint_reuse = HashMap::new();
    constraint_reuse.insert("reused".into(), reused.clone());
    constraint_reuse.insert("invalidated".into(), invalidated.clone());
    constraint_reuse.insert("added".into(), added.clone());
    let _ = disabled;
    VersionDiff { reused, invalidated, added, constraint_reuse }
}

fn build_origins(solver: &Solver, pipe: &Pipeline) -> HashMap<u32, Vec<u64>> {
    // inspect solver descriptors via resolve of all known vars
    let mut map = HashMap::new();
    for (tv, owner, _role) in &pipe.generated.var_info {
        let mut ids = Vec::new();
        if let Some(d) = solver.public_desc(crate::types::TV(*tv)) {
            for o in d {
                if let Some(n) = o.node {
                    if !ids.contains(&n.0) {
                        ids.push(n.0);
                    }
                }
            }
        }
        if !ids.contains(&owner.0) {
            ids.push(owner.0);
        }
        map.insert(*tv, ids);
    }
    map
}

fn type_json(t: &Type, origins: &HashMap<u32, Vec<u64>>) -> TypeNodeJSON {
    match t {
        Type::Var(v) => TypeNodeJSON {
            kind: "var".into(),
            name: None,
            label: None,
            children: vec![],
            origins: origins.get(&v.0).cloned().unwrap_or_default(),
        },
        Type::Con(n, ts) => TypeNodeJSON {
            kind: "con".into(),
            name: Some(n.clone()),
            label: None,
            children: ts.iter().map(|c| type_json(c, origins)).collect(),
            origins: ts.iter().flat_map(|c| type_origins(c, origins)).collect(),
        },
        Type::Arrow(a, b) => TypeNodeJSON {
            kind: "arrow".into(),
            name: None,
            label: None,
            children: vec![type_json(a, origins), type_json(b, origins)],
            origins: type_origins(t, origins),
        },
        Type::Record(r) => TypeNodeJSON {
            kind: "record".into(),
            name: None,
            label: None,
            children: r
                .fields
                .iter()
                .map(|(l, t)| TypeNodeJSON {
                    kind: "field".into(),
                    name: None,
                    label: Some(l.clone()),
                    children: vec![type_json(t, origins)],
                    origins: type_origins(t, origins),
                })
                .collect(),
            origins: row_origins(r, origins),
        },
        Type::Variant(r) => TypeNodeJSON {
            kind: "variant".into(),
            name: None,
            label: None,
            children: r
                .fields
                .iter()
                .map(|(l, t)| TypeNodeJSON {
                    kind: "tag".into(),
                    name: None,
                    label: Some(l.clone()),
                    children: vec![type_json(t, origins)],
                    origins: type_origins(t, origins),
                })
                .collect(),
            origins: row_origins(r, origins),
        },
    }
}

fn type_origins(t: &Type, origins: &HashMap<u32, Vec<u64>>) -> Vec<u64> {
    let mut out = Vec::new();
    for v in t.vars() {
        if let Some(os) = origins.get(&v.0) {
            for o in os {
                if !out.contains(o) {
                    out.push(*o);
                }
            }
        }
    }
    out
}

fn row_origins(r: &Row, origins: &HashMap<u32, Vec<u64>>) -> Vec<u64> {
    let mut out = Vec::new();
    for (_, t) in &r.fields {
        for o in type_origins(t, origins) {
            if !out.contains(&o) {
                out.push(o);
            }
        }
    }
    if let Some(v) = r.tail {
        if let Some(os) = origins.get(&v.0) {
            for o in os {
                if !out.contains(o) {
                    out.push(*o);
                }
            }
        }
    }
    out
}

fn build_graph(
    pipe: &Pipeline,
    failed: &HashSet<u64>,
    disabled: &HashSet<u64>,
    names: &HashMap<u32, String>,
    cursor: usize,
) -> (Vec<ConstraintNode>, Vec<ConstraintEdge>) {
    let mut nodes = Vec::new();
    let mut seen: HashSet<u64> = HashSet::new();
    let mut var_last: HashMap<u32, u64> = HashMap::new();
    let mut edges = Vec::new();
    let mut executed: HashSet<u64> = HashSet::new();

    for (i, op) in pipe.generated.flat_ops.iter().enumerate().take(cursor) {
        if let Op::Unify { id, expected, actual, reason, .. } = op {
            executed.insert(*id);
            if seen.insert(*id) {
                let owner = pipe.membership.get(id).copied().unwrap_or(NodeId(0));
                let mut vars: Vec<String> = expected
                    .vars()
                    .into_iter()
                    .chain(actual.vars())
                    .map(|v| names.get(&v.0).cloned().unwrap_or_else(|| format!("t{}", v.0)))
                    .collect();
                vars.sort();
                vars.dedup();
                let status = if disabled.contains(id) {
                    "disabled"
                } else if failed.contains(id) {
                    "failed"
                } else if i < cursor {
                    "solved"
                } else {
                    "pending"
                };
                nodes.push(ConstraintNode {
                    id: *id,
                    owner: owner.0,
                    kind: reason_kind(reason),
                    label: reason.describe(),
                    status: status.into(),
                    vars,
                });
            }
            // edges via shared vars with the previous constraint touching them
            let mut involved: Vec<u32> = expected.vars().into_iter().map(|v| v.0).collect();
            involved.extend(actual.vars().into_iter().map(|v| v.0));
            involved.sort();
            involved.dedup();
            for v in involved {
                if let Some(prev_id) = var_last.get(&v).copied() {
                    if prev_id != *id {
                        edges.push(ConstraintEdge {
                            from: prev_id,
                            to: *id,
                            shared: names.get(&v).cloned().unwrap_or_else(|| format!("t{}", v)),
                        });
                    }
                }
                var_last.insert(v, *id);
            }
        }
    }
    // include pending constraints (after cursor) for a complete graph
    for op in pipe.generated.flat_ops.iter().skip(cursor) {
        if let Op::Unify { id, expected, actual, reason, .. } = op {
            if seen.insert(*id) {
                let owner = pipe.membership.get(id).copied().unwrap_or(NodeId(0));
                let mut vars: Vec<String> = expected
                    .vars()
                    .into_iter()
                    .chain(actual.vars())
                    .map(|v| names.get(&v.0).cloned().unwrap_or_else(|| format!("t{}", v.0)))
                    .collect();
                vars.sort();
                vars.dedup();
                nodes.push(ConstraintNode {
                    id: *id,
                    owner: owner.0,
                    kind: reason_kind(reason),
                    label: reason.describe(),
                    status: if disabled.contains(id) { "disabled".into() } else { "pending".into() },
                    vars,
                });
            }
        }
    }
    let _ = executed;
    (nodes, edges)
}

fn reason_kind(r: &Reason) -> String {
    match r {
        Reason::AppArg(_) => "app-arg",
        Reason::AppResult(_) => "app-result",
        Reason::FunParam(_) | Reason::FunResult(_) => "fun",
        Reason::IfCond(_) | Reason::IfBranches(_) => "if",
        Reason::LetBind(_) => "let",
        Reason::VarOccur { .. } => "var",
        Reason::Literal(_) => "literal",
        Reason::BinOpLhs(_) | Reason::BinOpRhs(_) | Reason::BinOpResult(_) => "binop",
        Reason::RecordField(_, _) => "record-field",
        Reason::FieldProject(_, _) => "field",
        Reason::VariantTag(_, _) => "tag",
        Reason::MatchPayload { .. }
        | Reason::MatchArmResult(_, _)
        | Reason::MatchScrutTag(_, _)
        | Reason::MatchCoverage(_) => "match",
        Reason::Synthetic(_) => "synthetic",
    }
    .into()
}

fn expr_kind(e: &Expr) -> String {
    match e {
        Expr::Lit(_) => "lit",
        Expr::Var(_) => "var",
        Expr::Fun { .. } => "fun",
        Expr::App { .. } => "app",
        Expr::Let { .. } => "let",
        Expr::If { .. } => "if",
        Expr::BinOp { .. } => "binop",
        Expr::Record { .. } => "record",
        Expr::Field { .. } => "field",
        Expr::Tag { .. } => "tag",
        Expr::Match { .. } => "match",
    }
    .into()
}

fn expr_label(e: &Expr) -> String {
    match e {
        Expr::Lit(l) => format!("{l:?}"),
        Expr::Var(n) => n.clone(),
        Expr::Fun { params, .. } => format!("fun {}", params.iter().map(|p| p.name.as_str()).collect::<Vec<_>>().join(" ")),
        Expr::App { .. } => "@".into(),
        Expr::Let { name, rec, .. } => format!("{}let {name}", if *rec { "rec " } else { "" }),
        Expr::If { .. } => "if".into(),
        Expr::BinOp { op, .. } => op.clone(),
        Expr::Record { fields, open } => format!("{}{{ {} }}", if *open { "|" } else { "" }, fields.iter().map(|(l, _)| l.as_str()).collect::<Vec<_>>().join(", ")),
        Expr::Field { label, .. } => format!(".{label}"),
        Expr::Tag { label, payload } => format!("{label}{}", if payload.is_some() { "(..)" } else { "" }),
        Expr::Match { .. } => "match".into(),
    }
}

fn ast_json(n: &ExprNode) -> Value {
    let children: Vec<Value> = match &n.expr {
        Expr::Lit(_) | Expr::Var(_) => vec![],
        Expr::Fun { body, .. } => vec![ast_json(body)],
        Expr::App { fun, arg } => vec![ast_json(fun), ast_json(arg)],
        Expr::Let { value, body, .. } => vec![ast_json(value), ast_json(body)],
        Expr::If { cond, then_e, else_e } => vec![ast_json(cond), ast_json(then_e), ast_json(else_e)],
        Expr::BinOp { left, right, .. } => vec![ast_json(left), ast_json(right)],
        Expr::Record { fields, .. } => fields.iter().map(|(_, e)| ast_json(e)).collect(),
        Expr::Field { object, .. } => vec![ast_json(object)],
        Expr::Tag { payload, .. } => payload.iter().map(|p| ast_json(p)).collect(),
        Expr::Match { scrut, arms } => {
            let mut v = vec![ast_json(scrut)];
            for a in arms {
                v.push(ast_json(&a.body));
            }
            v
        }
    };
    serde_json::json!({
        "id": n.id.0,
        "span": [n.span.start, n.span.end],
        "kind": expr_kind(&n.expr),
        "label": expr_label(&n.expr),
        "children": children,
    })
}

/// Parse-only view used to surface syntax errors before solving.
pub fn parse_issues(source: &str) -> Vec<Value> {
    let program = crate::parser::parse(source);
    program
        .issues
        .iter()
        .map(|i| serde_json::json!({"message": i.message, "span": [i.span.start, i.span.end]}))
        .collect()
}

/// Constraint ids present in a source version (for diffing).
pub fn constraint_ids(source: &str, cache: &mut IncrementalCache) -> HashSet<u64> {
    let program = crate::parser::parse(source);
    let pipe = run_pipeline(program, cache);
    pipe.generated
        .flat_ops
        .iter()
        .filter_map(|o| if let Op::Unify { id, .. } = o { Some(*id) } else { None })
        .collect()
}

/// Return which node ids were reused from the cache during the last analysis.
pub fn reused_nodes(source: &str, cache: &mut IncrementalCache) -> HashSet<NodeId> {
    let program = crate::parser::parse(source);
    let (generated, new_cache) = gen::generate(&program, std::mem::take(&mut cache.chunks));
    cache.chunks = new_cache;
    generated.reused
}
