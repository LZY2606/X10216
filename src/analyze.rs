//! End-to-end analysis pipeline: parse -> constraint generation -> solve ->
//! unsat cores. Also computes incremental reuse diffs between versions.

use crate::constrain::{generate, Constraint};
use crate::core::unsat_cores;
use crate::solve::{Solver, Step, TracedError};
use crate::syntax::{collect_keys, Node};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorReport {
    pub error: TracedError,
    /// all minimal-size unsat core candidates (sets of constraint ids)
    pub cores: Vec<BTreeSet<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Analysis {
    pub constraints: Vec<Constraint>,
    pub steps: Vec<Step>,
    pub errors: Vec<ErrorReport>,
    /// resolved type of each AST node (node id -> pretty type)
    pub node_types: BTreeMap<u32, String>,
    pub root_type: Option<String>,
    pub ast: Node,
    /// node id -> (alpha-invariant key, binder scope)
    pub node_keys: BTreeMap<u32, (String, Vec<String>)>,
}

pub fn analyze(source: &str) -> Result<Analysis, String> {
    let ast = crate::syntax::parse(source)?;
    let gen = generate(&ast);
    let mut solver = Solver::new(gen.next_var);
    solver.recover = true;
    solver.run(&gen.constraints, &BTreeSet::new());

    // compute an unsat core for each distinct failing constraint
    let mut errors = Vec::new();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    for e in &solver.errors {
        if !seen.insert(e.constraint_id.clone()) {
            continue;
        }
        let idx = gen
            .constraints
            .iter()
            .position(|c| c.id == e.constraint_id)
            .unwrap();
        let cores = unsat_cores(&gen.constraints, idx, gen.next_var);
        errors.push(ErrorReport { error: e.clone(), cores });
    }

    let mut node_types = BTreeMap::new();
    for (node, var) in &gen.node_ty {
        node_types.insert(*node, solver.show(&crate::types::Ty::Var(*var)));
    }
    let root_type = if solver.errors.is_empty() {
        Some(solver.show(&crate::types::Ty::Var(gen.root_ty)))
    } else {
        Some(solver.show(&crate::types::Ty::Var(gen.root_ty)))
    };

    Ok(Analysis {
        constraints: gen.constraints,
        steps: solver.steps,
        errors,
        node_types,
        root_type,
        node_keys: collect_keys(&ast),
        ast,
    })
}

/// Re-solve with one constraint disabled and report the consequence.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DisableResult {
    pub disabled: String,
    pub still_errors: Vec<String>,
    pub root_type: Option<String>,
}

pub fn analyze_with_disabled(source: &str, disabled_id: &str) -> Result<DisableResult, String> {
    let ast = crate::syntax::parse(source)?;
    let gen = generate(&ast);
    let mut disabled = BTreeSet::new();
    disabled.insert(disabled_id.to_string());
    let mut solver = Solver::new(gen.next_var);
    solver.recover = true;
    solver.run(&gen.constraints, &disabled);
    Ok(DisableResult {
        disabled: disabled_id.to_string(),
        still_errors: solver.errors.iter().map(|e| e.error.message()).collect(),
        root_type: if solver.errors.is_empty() {
            Some(solver.show(&crate::types::Ty::Var(gen.root_ty)))
        } else {
            None
        },
    })
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diff {
    /// constraint ids present in both versions (reused, not re-solved)
    pub reused: Vec<String>,
    /// constraint ids only in the old version (invalidated by the edit)
    pub invalidated: Vec<String>,
    /// constraint ids only in the new version (newly generated / re-solved)
    pub added: Vec<String>,
}

pub fn diff(old: &Analysis, new: &Analysis) -> Diff {
    let old_ids: BTreeSet<&str> = old.constraints.iter().map(|c| c.id.as_str()).collect();
    let new_ids: BTreeSet<&str> = new.constraints.iter().map(|c| c.id.as_str()).collect();
    Diff {
        reused: old_ids.intersection(&new_ids).map(|s| s.to_string()).collect(),
        invalidated: old_ids.difference(&new_ids).map(|s| s.to_string()).collect(),
        added: new_ids.difference(&old_ids).map(|s| s.to_string()).collect(),
    }
}
