//! Full front-to-back analysis and incremental bookkeeping between edits.

use crate::generator::generate;
use crate::model::*;
use crate::solver::{solve, SolveResult};
use crate::syntax::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AstNodeDTO {
    pub id: u64,
    pub kind: String,
    pub label: String,
    pub span: Span,
    pub children: Vec<AstNodeDTO>,
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct ReuseReport {
    pub reused_actions: Vec<String>,
    pub invalid_actions: Vec<String>,
    pub resolved_actions: Vec<String>,
    pub new_actions: Vec<String>,
    pub reused_nodes: Vec<u64>,
    pub lost_nodes: Vec<u64>,
    pub new_nodes: Vec<u64>,
    /// One entry per action in the new program: reused | resolved | new.
    pub action_labels: Vec<(String, String)>,
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct GraphDTO {
    pub nodes: Vec<GraphNode>,
    pub links: Vec<GraphLink>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GraphNode {
    pub id: String,
    pub title: String,
    pub kind: String,
    pub node: u64,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GraphLink {
    pub source: String,
    pub target: String,
    pub label: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParseErrorDTO {
    pub message: String,
    pub start: u32,
    pub end: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct Analysis {
    pub parse_error: Option<ParseErrorDTO>,
    pub ast: Option<AstNodeDTO>,
    pub nodes: Vec<(u64, u32, u32, String)>,
    pub program: Option<ConstraintProgram>,
    pub result: Option<SolveResult>,
    pub reuse: ReuseReport,
    pub graph: GraphDTO,
}

fn ast_dto(expr: &NExpr) -> AstNodeDTO {
    let (kind, label, children) = match &expr.inner {
        Expr::Unit => ("unit".to_string(), "()".to_string(), vec![]),
        Expr::Int(n) => ("int".to_string(), n.to_string(), vec![]),
        Expr::Bool(b) => ("bool".to_string(), b.to_string(), vec![]),
        Expr::Str(s) => ("string".to_string(), s.clone(), vec![]),
        Expr::Var(n) => ("var".to_string(), n.clone(), vec![]),
        Expr::Lam { param, body } => {
            ("lambda".to_string(), format!("fun {param}"), vec![ast_dto(body)])
        }
        Expr::App { fun, arg } => ("app".to_string(), "@".into(), vec![ast_dto(fun), ast_dto(arg)]),
        Expr::Let { rec, name, rhs, body } => (
            "let".to_string(),
            format!("{}{name}", if *rec { "let rec " } else { "let " }),
            vec![ast_dto(rhs), ast_dto(body)],
        ),
        Expr::If { cond, then_, else_ } => {
            ("if".to_string(), "if".into(), vec![ast_dto(cond), ast_dto(then_), ast_dto(else_)])
        }
        Expr::Record { fields, rest } => {
            let mut ch: Vec<AstNodeDTO> = fields
                .iter()
                .map(|(l, e)| AstNodeDTO {
                    id: e.id.0,
                    kind: "field".into(),
                    label: l.clone(),
                    span: e.span,
                    children: vec![ast_dto(e)],
                })
                .collect();
            if let Some(r) = rest {
                ch.push(ast_dto(r));
            }
            ("record".to_string(), "{ ... }".into(), ch)
        }
        Expr::Proj { obj, field } => ("proj".to_string(), format!(".{field}"), vec![ast_dto(obj)]),
        Expr::Tag { label, payload } => (
            "tag".to_string(),
            format!("`{label}"),
            payload.as_ref().map(|p| vec![ast_dto(p)]).unwrap_or_default(),
        ),
        Expr::Match { scrut, arms } => {
            let mut ch = vec![ast_dto(scrut)];
            for a in arms {
                let (plabel, pbinder) = match &a.inner.pat.inner {
                    Pat::Wild => ("_".to_string(), None),
                    Pat::Tag(l, b) => (format!("`{l}"), b.clone()),
                };
                let mut children = vec![ast_dto(&a.inner.body)];
                let patnode = AstNodeDTO {
                    id: a.inner.pat.id.0,
                    kind: "pattern".into(),
                    label: match pbinder {
                        Some(b) if !b.is_empty() => format!("{plabel} {b}"),
                        _ => plabel,
                    },
                    span: a.inner.pat.span,
                    children: vec![],
                };
                children.insert(0, patnode);
                ch.push(AstNodeDTO {
                    id: a.id.0,
                    kind: "arm".into(),
                    label: "arm".into(),
                    span: a.span,
                    children,
                });
            }
            ("match".to_string(), "match".into(), ch)
        }
        Expr::Bin { op, left, right } => {
            ("bin".to_string(), op.symbol().to_string(), vec![ast_dto(left), ast_dto(right)])
        }
    };
    AstNodeDTO { id: expr.id.0, kind, label, span: expr.span, children }
}

fn action_kind_name(k: &ActionKind) -> &'static str {
    match k {
        ActionKind::Intro { .. } => "intro",
        ActionKind::EnterScope { .. } => "scope",
        ActionKind::ExitScope => "scope",
        ActionKind::Unify { .. } => "unify",
        ActionKind::Prim { .. } => "prim",
        ActionKind::Match { .. } => "match",
        ActionKind::Generalize { .. } => "generalize",
        ActionKind::Instantiate { .. } => "instantiate",
        ActionKind::Error { .. } => "error",
    }
}

fn term_metas(t: &STerm, out: &mut Vec<MetaKey>) {
    match t {
        STerm::V(k) => out.push(k.clone()),
        STerm::Q(_) | STerm::C(_) => {}
        STerm::Arrow(a, b) => {
            term_metas(a, out);
            term_metas(b, out);
        }
        STerm::Rec(r) | STerm::Variant(r) => {
            for (_, f) in &r.fields {
                term_metas(f, out);
            }
            if let SRowEnd::Open(t) = &r.end {
                term_metas(t, out);
            }
        }
    }
}

fn action_metas(a: &Action, out: &mut Vec<MetaKey>) {
    match &a.kind {
        ActionKind::Intro { key, .. } => out.push(key.clone()),
        ActionKind::Unify { left, right, .. } => {
            term_metas(left, out);
            term_metas(right, out);
        }
        ActionKind::Prim { operands, result, .. } => {
            out.extend(operands.iter().cloned());
            out.push(result.clone());
        }
        ActionKind::Match { scrut, result, arms, tail } => {
            out.push(scrut.clone());
            out.push(result.clone());
            out.push(tail.clone());
            for arm in arms {
                out.push(arm.body.clone());
                if let Some(p) = &arm.payload {
                    out.push(p.clone());
                }
            }
        }
        ActionKind::Generalize { result, .. } => out.push(result.clone()),
        ActionKind::Instantiate { result, .. } => out.push(result.clone()),
        ActionKind::EnterScope { .. } | ActionKind::ExitScope | ActionKind::Error { .. } => {}
    }
}

fn build_graph(prog: &ConstraintProgram) -> GraphDTO {
    let mut nodes = Vec::new();
    let mut by_meta: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut by_scheme: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for a in &prog.actions {
        nodes.push(GraphNode {
            id: a.id.0.clone(),
            title: a.title.clone(),
            kind: action_kind_name(&a.kind).to_string(),
            node: a.node.0,
            span: a.span,
        });
        let mut metas = Vec::new();
        action_metas(a, &mut metas);
        for m in metas {
            by_meta.entry(m.0).or_default().push(a.id.0.clone());
        }
        match &a.kind {
            ActionKind::Generalize { scheme, .. } => {
                by_scheme.entry(scheme.0.clone()).or_default().push(a.id.0.clone());
            }
            ActionKind::Instantiate { scheme: SchemeRef::Local(s), .. } => {
                by_scheme.entry(s.0.clone()).or_default().push(a.id.0.clone());
            }
            _ => {}
        }
    }
    let mut links = Vec::new();
    let mut seen = BTreeSet::new();
    for (_meta, actions) in by_meta {
        if actions.len() > 1 {
            for a in &actions[1..] {
                let key = (actions[0].clone(), a.clone(), "meta".to_string());
                if seen.insert(key) {
                    links.push(GraphLink { source: actions[0].clone(), target: a.clone(), label: "共享变量".into() });
                }
            }
        }
    }
    for (_scheme, actions) in by_scheme {
        if actions.len() > 1 {
            for a in &actions[1..] {
                links.push(GraphLink { source: actions[0].clone(), target: a.clone(), label: "方案".into() });
            }
        }
    }
    GraphDTO { nodes, links }
}

pub fn analyze(src: &str, prev: Option<&ConstraintProgram>, disabled: &BTreeSet<String>) -> Analysis {
    let parsed = parse(src);
    match parsed {
        Err(e) => Analysis {
            parse_error: Some(ParseErrorDTO {
                message: e.msg,
                start: e.span.start,
                end: e.span.end,
            }),
            ..Default::default()
        },
        Ok(root) => {
            let mut collected = Vec::new();
            collect_nodes(&root, &mut collected);
            let program = generate(&root);
            let result = solve(&program, disabled);
            let graph = build_graph(&program);

            let mut reuse = ReuseReport::default();
            if let Some(prev_prog) = prev {
                let prev_map: BTreeMap<String, &Action> =
                    prev_prog.actions.iter().map(|a| (a.id.0.clone(), a)).collect();
                let new_map: BTreeMap<String, &Action> =
                    program.actions.iter().map(|a| (a.id.0.clone(), a)).collect();

                for a in &program.actions {
                    let label = match prev_map.get(&a.id.0) {
                        None => {
                            reuse.new_actions.push(a.id.0.clone());
                            "new"
                        }
                        Some(old)
                            if old.scope_key == a.scope_key
                                && old.genv_key == a.genv_key
                                && old.kind == a.kind =>
                        {
                            reuse.reused_actions.push(a.id.0.clone());
                            "reused"
                        }
                        Some(_) => {
                            reuse.resolved_actions.push(a.id.0.clone());
                            "resolved"
                        }
                    };
                    reuse.action_labels.push((a.id.0.clone(), label.to_string()));
                }
                for a in &prev_prog.actions {
                    if !new_map.contains_key(&a.id.0) {
                        reuse.invalid_actions.push(a.id.0.clone());
                    }
                }

                let prev_nodes: BTreeSet<u64> = {
                    let mut v: Vec<u64> = Vec::new();
                    for a in &prev_prog.actions {
                        v.push(a.node.0);
                    }
                    for (n, _) in &prev_prog.node_types {
                        v.push(n.0);
                    }
                    v.into_iter().collect()
                };
                let new_nodes: BTreeSet<u64> = {
                    let mut v: Vec<u64> = program.actions.iter().map(|a| a.node.0).collect();
                    for (n, _) in &program.node_types {
                        v.push(n.0);
                    }
                    v.into_iter().collect()
                };
                reuse.reused_nodes = prev_nodes.intersection(&new_nodes).copied().collect();
                reuse.lost_nodes = prev_nodes.difference(&new_nodes).copied().collect();
                reuse.new_nodes = new_nodes.difference(&prev_nodes).copied().collect();
            }

            Analysis {
                parse_error: None,
                ast: Some(ast_dto(&root)),
                nodes: collected
                    .into_iter()
                    .map(|(id, span, kind)| (id.0, span.start, span.end, kind.to_string()))
                    .collect(),
                program: Some(program),
                result: Some(result),
                reuse,
                graph,
            }
        }
    }
}
