//! Minimal unsat cores. For each reported error we search a small set of
//! constraints that still reproduces the failure, and keep all distinct
//! smallest cores found through different reduction orders.

use crate::model::*;
use crate::solver::{Executor, TypeErrorData};
use std::collections::BTreeSet;

const MAX_CORES: usize = 6;

fn is_constraint(k: &ActionKind) -> bool {
    matches!(
        k,
        ActionKind::Unify { .. }
            | ActionKind::Prim { .. }
            | ActionKind::Match { .. }
            | ActionKind::Generalize { .. }
            | ActionKind::Instantiate { .. }
            | ActionKind::Error { .. }
    )
}

fn candidate_ids(prog: &ConstraintProgram) -> Vec<String> {
    prog.actions
        .iter()
        .filter(|a| is_constraint(&a.kind))
        .map(|a| a.id.0.clone())
        .collect()
}

fn fails_at(prog: &ConstraintProgram, active: &BTreeSet<String>, target: &str) -> bool {
    let disabled: BTreeSet<String> = candidate_ids(prog)
        .into_iter()
        .filter(|id| !active.contains(id))
        .collect();
    let mut exec = Executor::new(prog, disabled);
    exec.run_all();
    exec.failed_action_ids().iter().any(|id| id == target)
}

fn seed(prog: &ConstraintProgram, candidates: &[String], target: &str) -> BTreeSet<String> {
    let target_action = prog.actions.iter().find(|a| a.id.0 == target);
    let mut set: BTreeSet<String> = BTreeSet::new();
    set.insert(target.to_string());
    if let Some(ta) = target_action {
        for a in &prog.actions {
            if is_constraint(&a.kind)
                && (a.node == ta.node
                    || (a.span.start <= ta.span.start && a.span.end >= ta.span.end))
            {
                set.insert(a.id.0.clone());
            }
        }
    }
    let _ = candidates;
    set
}

/// Remove constraints greedily; stable core containing the target.
fn ddmin(prog: &ConstraintProgram, start: BTreeSet<String>, target: &str, order: &[String]) -> BTreeSet<String> {
    let mut cur = start;
    let order: Vec<String> = order.iter().filter(|id| cur.contains(*id)).cloned().collect();
    let mut changed = true;
    while changed {
        changed = false;
        for id in &order {
            if cur.len() <= 1 {
                break;
            }
            if !cur.contains(id) || id == target {
                continue;
            }
            let mut trial = cur.clone();
            trial.remove(id);
            if fails_at(prog, &trial, target) {
                cur.remove(id);
                changed = true;
            }
        }
    }
    cur
}

fn rotations(v: Vec<String>) -> Vec<Vec<String>> {
    let mut orders = vec![v.clone(), v.iter().rev().cloned().collect()];
    if !v.is_empty() {
        let mid = v.len() / 2;
        let mut r1: Vec<String> = v[mid..].to_vec();
        r1.extend_from_slice(&v[..mid]);
        let mut r2 = r1.iter().rev().cloned().collect::<Vec<_>>();
        orders.push(r1);
        orders.push(r2.drain(..).collect());
    }
    orders
}

fn cores_for(prog: &ConstraintProgram, candidates: Vec<String>, error: &TypeErrorData) -> Vec<Vec<String>> {
    let target = error.action_id.clone();
    let s0 = seed(prog, &candidates, &target);
    let start = if fails_at(prog, &s0, &target) {
        s0
    } else {
        candidates.iter().cloned().collect()
    };

    let mut found: Vec<BTreeSet<String>> = Vec::new();
    for order in rotations(candidates) {
        let core = ddmin(prog, start.clone(), &target, &order);
        if !found.contains(&core) {
            found.push(core);
        }
    }
    let min_size = found.iter().map(BTreeSet::len).min().unwrap_or(0);
    found
        .into_iter()
        .filter(|c| c.len() == min_size)
        .take(MAX_CORES)
        .map(|c| c.into_iter().collect())
        .collect()
}

pub fn compute_cores(prog: &ConstraintProgram, errors: &[TypeErrorData]) -> Vec<Vec<Vec<String>>> {
    let candidates = candidate_ids(prog);
    errors.iter().map(|e| cores_for(prog, candidates.clone(), e)).collect()
}
