// 分析编排：源码 + 旧版本上下文 -> 完整分析结果。
use crate::constraints::{Event, Generator};
use crate::identity;
use crate::solver::{SolveResult, Solver};
use crate::syntax::{Expr, ParseError, Parser};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize)]
pub struct GraphNode {
    pub id: String,
    pub label: String,
    pub nid: Option<u32>,
    pub kind: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct GraphEdge {
    pub from: String,
    pub to: String,
    pub label: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstraintInfo {
    pub key: String,
    pub cid: String,
    pub nid: u32,
    pub detail: String,
    pub left: String,
    pub right: String,
    pub left_role: String,
    pub right_role: String,
    pub scope: String,
    pub gen_env: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct AnalysisView {
    pub parse_error: Option<ParseError>,
    pub ast: Option<Expr>,
    pub max_id: u32,
    pub events: Vec<Event>,
    pub constraints: Vec<ConstraintInfo>,
    pub graph_nodes: Vec<GraphNode>,
    pub graph_edges: Vec<GraphEdge>,
    pub result: SolveResult,
    pub identity_reused: u32,
    pub identity_fresh: u32,
    pub node_spans: BTreeMap<u32, (usize, usize)>,
}

#[derive(Debug, Clone, Default)]
pub struct PriorContext {
    pub old_tree: Option<Expr>,
    pub old_max: u32,
}

pub fn analyze(
    source: &str,
    prior: Option<&PriorContext>,
    disabled: &BTreeSet<String>,
) -> AnalysisView {
    let (old_tree, old_max) = match prior {
        Some(p) => (p.old_tree.clone(), p.old_max),
        None => (None, 0),
    };
    let parsed = Parser::parse_program(source, 0);
    match parsed {
        Err(err) => AnalysisView {
            parse_error: Some(err),
            ast: None,
            max_id: old_max,
            events: vec![],
            constraints: vec![],
            graph_nodes: vec![],
            graph_edges: vec![],
            result: Solver::solve(&[], disabled),
            identity_reused: 0,
            identity_fresh: 0,
            node_spans: BTreeMap::new(),
        },
        Ok(outcome) => {
            let remapped = identity::remap(outcome.root, old_tree.as_ref(), old_max);
            let root = remapped.expr;
            let node_spans: BTreeMap<u32, (usize, usize)> = root
                .nid_map()
                .into_iter()
                .map(|(k, s)| (k, (s.start, s.end)))
                .collect();
            let events = Generator::run(&root);
            let constraints = events
                .iter()
                .filter_map(|e| match e {
                    Event::Equate {
                        key,
                        cid,
                        left,
                        right,
                        left_role,
                        right_role,
                        nid,
                        scope,
                        gen_env,
                        detail,
                    } => Some(ConstraintInfo {
                        key: key.clone(),
                        cid: cid.clone(),
                        nid: *nid,
                        detail: detail.clone(),
                        left: left.render(),
                        right: right.render(),
                        left_role: left_role.clone(),
                        right_role: right_role.clone(),
                        scope: scope.clone(),
                        gen_env: gen_env.clone(),
                    }),
                    _ => None,
                })
                .collect();
            let result = Solver::solve(&events, disabled);
            let (graph_nodes, graph_edges) = build_graph(&root, &events);
            AnalysisView {
                parse_error: None,
                ast: Some(root),
                max_id: remapped.max_id,
                events,
                constraints,
                graph_nodes,
                graph_edges,
                result,
                identity_reused: remapped.reused,
                identity_fresh: remapped.fresh,
                node_spans,
            }
        }
    }
}

fn build_graph(root: &Expr, events: &[Event]) -> (Vec<GraphNode>, Vec<GraphEdge>) {
    let mut nodes: Vec<GraphNode> = Vec::new();
    let mut edges: Vec<GraphEdge> = Vec::new();

    // 表达式节点
    root.walk(&mut |e| {
        nodes.push(GraphNode {
            id: format!("e{}", e.nid()),
            label: short_expr(e),
            nid: Some(e.nid()),
            kind: "expr".into(),
        });
    });
    // 父子边
    let mut add_child_edges = |e: &Expr| {
        let mut child = |c: &Expr| {
            edges.push(GraphEdge {
                from: format!("e{}", e.nid()),
                to: format!("e{}", c.nid()),
                label: "子表达式".into(),
            });
        };
        e.children(&mut child);
    };
    root.walk(&mut add_child_edges);

    // 约束节点 + 与表达式的边；约束之间若共享类型变量则相连
    for ev in events {
        if let Event::Equate { key, cid, nid, left, right, .. } = ev {
            let node_id = format!("c:{key}");
            nodes.push(GraphNode {
                id: node_id.clone(),
                label: format!("{cid} {}", left.render()),
                nid: Some(*nid),
                kind: "constraint".into(),
            });
            edges.push(GraphEdge {
                from: node_id.clone(),
                to: format!("e{nid}"),
                label: "来源".into(),
            });
            edges.push(GraphEdge {
                from: format!("e{nid}"),
                to: node_id.clone(),
                label: "产生".into(),
            });
        }
    }
    // 共享变量边
    let mut vars_to_constraints: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for ev in events {
        if let Event::Equate { key, left, right, .. } = ev {
            let mut vars = Vec::new();
            crate::solver::collect_vars_public(left, &mut vars);
            crate::solver::collect_vars_public(right, &mut vars);
            for v in vars {
                vars_to_constraints.entry(v).or_default().push(key.clone());
            }
        }
    }
    for (_, keys) in vars_to_constraints {
        let mut uniq: Vec<String> = keys.into_iter().collect();
        uniq.sort();
        uniq.dedup();
        if uniq.len() > 1 {
            for w in uniq.windows(2) {
                edges.push(GraphEdge {
                    from: format!("c:{}", w[0]),
                    to: format!("c:{}", w[1]),
                    label: "共享变量".into(),
                });
            }
        }
    }
    (nodes, edges)
}

fn short_expr(e: &Expr) -> String {
    match e {
        Expr::Int { value, .. } => value.to_string(),
        Expr::Bool { value, .. } => value.to_string(),
        Expr::Str { value, .. } => format!("\"{}\"", truncate(value, 10)),
        Expr::Var { name, .. } => name.clone(),
        Expr::Lam { .. } => "fn".into(),
        Expr::App { .. } => "应用".into(),
        Expr::Let { is_rec, .. } => if *is_rec { "let rec".into() } else { "let".into() },
        Expr::If { .. } => "if".into(),
        Expr::Record { .. } => "{记录}".into(),
        Expr::Field { label, .. } => format!(".{label}"),
        Expr::Variant { label, .. } => format!("`{label}"),
        Expr::Match { .. } => "match".into(),
    }
}

fn truncate(s: &str, n: usize) -> String {
    if s.chars().count() <= n {
        s.to_string()
    } else {
        s.chars().take(n).collect::<String>() + "…"
    }
}
