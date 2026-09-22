pub mod infer;
pub mod unify;
pub mod core;

use crate::ast::Program;
use crate::incremental::{diff_constraints, ConstraintDiff, ConstraintVersion};
use crate::parser::parse;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

pub use infer::analyze_program;

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct AnalysisOptions {
    #[serde(default)]
    pub disabled_constraints: BTreeSet<String>,
    #[serde(default)]
    pub until_step: Option<usize>,
}

#[derive(Clone, Debug, Serialize)]
pub struct Analysis {
    pub ok: bool,
    pub parse_error: Option<String>,
    pub program: Option<Program>,
    pub constraints: Vec<ConstraintView>,
    pub steps: Vec<StepView>,
    pub errors: Vec<ErrorView>,
    pub type_graph: Vec<serde_json::Value>,
    pub final_types: BTreeMap<String, FinalType>,
    pub constraint_versions: Vec<ConstraintVersion>,
    pub diff: ConstraintDiff,
    pub node_reuse: NodeReuse,
}

#[derive(Clone, Debug, Serialize)]
pub struct ConstraintView {
    pub id: String,
    pub numeric_id: u64,
    pub kind: String,
    pub title: String,
    pub detail: String,
    pub status: String,
    pub source_nodes: BTreeSet<u64>,
    pub cache_key: String,
    pub fingerprint: String,
    pub disabled: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct StepView {
    pub index: usize,
    pub kind: String,
    pub constraint_id: Option<String>,
    pub title: String,
    pub detail: String,
    pub status: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct ErrorView {
    pub key: String,
    pub kind: String,
    pub message: String,
    pub span: Option<crate::lexer::Span>,
    pub source_nodes: BTreeSet<u64>,
    pub missing_fields: Vec<String>,
    pub extra_fields: Vec<String>,
    pub expected: Option<String>,
    pub actual: Option<String>,
    pub cores: Vec<Vec<String>>,
    pub related_constraints: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct FinalType {
    pub name: String,
    pub node_id: u64,
    pub typ: String,
    pub scheme: bool,
    pub value_restricted: bool,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct NodeReuse {
    pub reused: BTreeSet<u64>,
    pub invalidated: BTreeSet<u64>,
    pub new: BTreeSet<u64>,
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct PreviousVersion {
    pub constraint_versions: Vec<ConstraintVersion>,
    pub node_ids: Vec<u64>,
}

pub fn analyze_source(source: &str, options: AnalysisOptions) -> Analysis {
    match parse(source) {
        Ok(program) => analyze_program(program, options),
        Err(parse_error) => Analysis {
            ok: false,
            parse_error: Some(parse_error),
            program: None,
            constraints: Vec::new(),
            steps: Vec::new(),
            errors: Vec::new(),
            type_graph: Vec::new(),
            final_types: BTreeMap::new(),
            constraint_versions: Vec::new(),
            diff: ConstraintDiff::default(),
            node_reuse: NodeReuse::default(),
        },
    }
}

pub fn analyze_with_previous(source: &str, options: AnalysisOptions, previous: Option<&PreviousVersion>) -> Analysis {
    let mut analysis = analyze_source(source, options);
    if let Some(previous) = previous {
        analysis.diff = diff_constraints(&previous.constraint_versions, &analysis.constraint_versions);
        let old_nodes: BTreeSet<u64> = previous.node_ids.iter().copied().collect();
        let new_nodes: BTreeSet<u64> = analysis.program.iter().flat_map(|program| program.node_ids.keys().copied()).collect();
        analysis.node_reuse = NodeReuse {
            reused: old_nodes.intersection(&new_nodes).copied().collect(),
            invalidated: old_nodes.difference(&new_nodes).copied().collect(),
            new: new_nodes.difference(&old_nodes).copied().collect(),
        };
    }
    analysis
}
