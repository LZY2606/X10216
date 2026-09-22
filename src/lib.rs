pub mod constraints;
pub mod cores;
pub mod generate;
pub mod incremental;
pub mod parser;
pub mod solver;
pub mod syntax;
pub mod term;
pub mod web;
pub mod types;

use constraints::*;
use serde::{Deserialize, Serialize};
use solver::*;
use std::collections::HashSet;
use syntax::Span;

#[derive(Clone, Debug, Serialize)]
pub struct ErrorDto {
    pub id: u32,
    pub kind: String,
    pub message: String,
    pub spans: Vec<(usize, usize)>,
    pub cids: Vec<u32>,
    pub missing: Vec<String>,
    pub extra: Vec<String>,
    pub sort: String,
    pub signature: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct CoreDto {
    pub members: Vec<u32>,
    pub size: usize,
}

#[derive(Clone, Debug, Serialize)]
pub struct AnalysisResponse {
    pub ok: bool,
    pub parse_error: Option<ParseErrorDto>,
    pub source: String,
    pub ast: Vec<constraints::serde_graph::ConstraintNodeDto>,
    pub type_nodes: Vec<TypeNodeDto>,
    pub ast_nodes: Vec<AstNodeDto>,
    pub expr_type: Vec<(u32, u32)>,
    pub root_constraint: u32,
    pub steps: Vec<StepRecord>,
    pub errors: Vec<ErrorDto>,
    pub cores: Vec<CoreDto>,
    pub final_types: Vec<VarSnapshot>,
    pub root_type: Option<TermFDto>,
    pub stats: StatsDto,
}

#[derive(Clone, Debug, Serialize)]
pub struct StatsDto {
    pub instantiations: u32,
    pub generalizations: u32,
    pub unifications: u32,
    pub errors: u32,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseErrorDto {
    pub message: String,
    pub span: (usize, usize),
}

#[derive(Clone, Debug, Serialize)]
pub struct TypeNodeDto {
    pub id: u32,
    pub expr: Option<u32>,
    pub span: Option<(usize, usize)>,
    pub hint: String,
    pub flex_row: bool,
    pub rigid: Option<String>,
    pub concrete: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct AstNodeDto {
    pub id: u32,
    pub path: String,
    pub span: (usize, usize),
    pub kind: String,
    pub label: String,
    pub children: Vec<u32>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TermFDto {
    pub root: u32,
    pub kind: serde_json::Value,
    pub name: String,
    pub rendered: String,
}

fn term_dto(t: &types::TermF) -> TermFDto {
    TermFDto {
        root: t.root,
        kind: serde_json::to_value(&t.kind).unwrap_or(serde_json::Value::Null),
        name: t.name.clone(),
        rendered: types::render(t),
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct IncrementalResponse {
    pub current: AnalysisResponse,
    pub reuse: incremental::ReuseReport,
}

pub struct BuiltAnalysis {
    pub graph: ConstraintGraph,
    pub bases: std::collections::HashMap<NodeIdT, term::BaseS>,
    pub flexes: std::collections::HashMap<NodeIdT, term::FlexDesc>,
    pub ambient: std::collections::HashMap<CId, NodeIdT>,
    pub schemes: std::collections::HashMap<CId, (Vec<NodeIdT>, NodeIdT)>,
    pub response: AnalysisResponse,
    pub solved: HashSet<CId>,
}

pub fn build_response(source: &str, disabled: &[u32]) -> Result<BuiltAnalysis, ParseErrorDto> {
    let expr = parser::parse(source).map_err(|e| ParseErrorDto {
        message: e.msg,
        span: (e.span.start, e.span.end),
    })?;
    let gen = generate::Generator::new(source);
    let generated = gen.generate(&expr);
    let ambient = generated.ambient_inst_roots;
    let schemes = generated.scheme_insts;
    let bases = generated.bases;
    let flexes = generated.flexes;
    let graph = generated.graph;

    let mut cfg = SolveConfig::default();
    cfg.disabled = disabled.iter().map(|x| CId(*x)).collect();
    let a = Solver::analyze(&graph, &bases, &flexes, &ambient, &schemes, &cfg);

    let root_c = graph.root.0;
    let ast_dtos: Vec<AstNodeDto> = graph
        .ast_nodes
        .iter()
        .map(|n| AstNodeDto {
            id: n.id.0,
            path: n.path.clone(),
            span: (n.span.start, n.span.end),
            kind: n.kind.clone(),
            label: n.label.clone(),
            children: n.children.iter().map(|c| c.0).collect(),
        })
        .collect();
    let type_dtos: Vec<TypeNodeDto> = graph
        .type_nodes
        .iter()
        .map(|t| TypeNodeDto {
            id: t.id.0,
            expr: t.expr.map(|n| n.0),
            span: t.span.map(|s| (s.start, s.end)),
            hint: t.hint.clone(),
            flex_row: t.flex_row,
            rigid: t.rigid.clone(),
            concrete: t.concrete,
        })
        .collect();
    let expr_type: Vec<(u32, u32)> = graph.expr_type.iter().map(|(k, v)| (k.0, v.0)).collect();
    let cdtos: Vec<constraints::serde_graph::ConstraintNodeDto> =
        graph.nodes.iter().map(constraints::serde_graph::to_dto).collect();
    let errors = a
        .errors
        .iter()
        .map(|e| ErrorDto {
            id: e.id,
            kind: format!("{:?}", e.kind),
            message: e.message.clone(),
            spans: e.spans.clone(),
            cids: e.cids.iter().map(|c| c.0).collect(),
            missing: e.missing.clone(),
            extra: e.extra.clone(),
            sort: e.sort.into(),
            signature: e.signature.clone(),
        })
        .collect();
    let cores = a
        .cores
        .iter()
        .map(|c| CoreDto { members: c.iter().map(|x| x.0).collect(), size: c.len() })
        .collect();
    let ok = a.errors.is_empty();
    let response = AnalysisResponse {
        ok,
        parse_error: None,
        source: source.into(),
        ast: cdtos,
        type_nodes: type_dtos,
        ast_nodes: ast_dtos,
        expr_type,
        root_constraint: root_c,
        steps: a.steps,
        errors,
        cores,
        final_types: a.final_vars,
        root_type: a.root_type.as_ref().map(term_dto),
        stats: StatsDto {
            instantiations: a.stats.0,
            generalizations: a.stats.1,
            unifications: a.stats.2,
            errors: a.stats.3,
        },
    };
    let mut solved = HashSet::new();
    for n in &graph.nodes {
        solved.insert(n.id);
    }
    Ok(BuiltAnalysis { graph, bases, flexes, ambient, schemes, response, solved })
}

pub fn parse_error_response(source: &str, err: &ParseErrorDto) -> AnalysisResponse {
    AnalysisResponse {
        ok: false,
        parse_error: Some(err.clone()),
        source: source.into(),
        ast: Vec::new(),
        type_nodes: Vec::new(),
        ast_nodes: Vec::new(),
        expr_type: Vec::new(),
        root_constraint: 0,
        steps: Vec::new(),
        errors: vec![ErrorDto {
            id: 0,
            kind: "Parse".into(),
            message: err.message.clone(),
            spans: vec![err.span],
            cids: Vec::new(),
            missing: Vec::new(),
            extra: Vec::new(),
            sort: "parse".into(),
            signature: "parse".into(),
        }],
        cores: Vec::new(),
        final_types: Vec::new(),
        root_type: None,
        stats: StatsDto { instantiations: 0, generalizations: 0, unifications: 0, errors: 1 },
    }
}

#[allow(dead_code)]
fn force_span(_s: Span) {}

pub fn compare_versions(
    old: &BuiltAnalysis,
    new_source: &str,
    disabled: &[u32],
) -> Result<IncrementalResponse, ParseErrorDto> {
    let built = build_response(new_source, disabled)?;
    let reuse = incremental::compare(&old.graph, &built.graph, &old.solved);
    Ok(IncrementalResponse { current: built.response, reuse })
}
