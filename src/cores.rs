// Unsatisfiable cores: for each reported error, find small constraint subsets
// whose removal makes that specific error disappear. Several equally-small
// cores are retained as distinct candidates.

use crate::constraints::*;
use crate::solver::{ErrorKind, SolveConfig, Solver, TypeError};
use std::collections::{BTreeSet, HashSet};

pub fn find_cores(
    graph: &ConstraintGraph,
    bases: &std::collections::HashMap<NodeIdT, crate::term::BaseS>,
    flexes: &std::collections::HashMap<NodeIdT, crate::term::FlexDesc>,
    ambient: &std::collections::HashMap<CId, NodeIdT>,
    scheme_insts: &std::collections::HashMap<CId, (Vec<NodeIdT>, NodeIdT)>,
    errors: &[TypeError],
) -> Vec<Vec<CId>> {
    // Group errors by kind so probing detects "the same error kind at the
    // same signature". Each error yields one or more minimal cores.
    let mut result: Vec<Vec<CId>> = Vec::new();
    let mut seen: BTreeSet<Vec<u32>> = BTreeSet::new();

    for err in errors {
        let candidates = core_for_error(graph, bases, flexes, ambient, scheme_insts, err);
        for c in candidates {
            let mut key: Vec<u32> = c.iter().map(|x| x.0).collect();
            key.sort_unstable();
            if seen.insert(key) {
                result.push(c);
            }
        }
    }
    result.sort_by_key(|c| (c.len(), c.iter().map(|x| x.0).collect::<Vec<_>>()));
    result
}

fn all_actionable(graph: &ConstraintGraph) -> Vec<CId> {
    graph
        .nodes
        .iter()
        .filter(|n| matches!(n.kind, CKind::Eq(_) | CKind::Inst(_) | CKind::Gen(_) | CKind::EmptyRow(_)))
        .map(|n| n.id)
        .collect()
}

fn core_for_error(
    graph: &ConstraintGraph,
    bases: &std::collections::HashMap<NodeIdT, crate::term::BaseS>,
    flexes: &std::collections::HashMap<NodeIdT, crate::term::FlexDesc>,
    ambient: &std::collections::HashMap<CId, NodeIdT>,
    scheme_insts: &std::collections::HashMap<CId, (Vec<NodeIdT>, NodeIdT)>,
    target: &TypeError,
) -> Vec<Vec<CId>> {
    let actionable = all_actionable(graph);
    let run = |disabled: &HashSet<CId>| -> Vec<TypeError> {
        let cfg = SolveConfig { disabled: disabled.clone(), probe: true };
        let a = Solver::analyze(graph, bases, flexes, ambient, scheme_insts, &cfg);
        a.errors
    };
    let persists = |es: &[TypeError]| -> bool {
        es.iter().any(|e| e.signature == target.signature)
    };

    // Baseline: error must exist.
    let base = run(&HashSet::new());
    if !persists(&base) {
        return Vec::new();
    }

    // Candidate starting sets: the reported blame plus several structural
    // neighborhoods, to find multiple minimal cores of the same size.
    let mut seeds: Vec<Vec<CId>> = Vec::new();
    let blame: BTreeSet<CId> = target.cids.iter().copied().collect();

    // Seed 1: blame constraints only.
    seeds.push(blame.iter().copied().collect());

    // Seed 2: blame plus every constraint sharing the same origin span.
    let spans: BTreeSet<(usize, usize)> = target.spans.iter().copied().collect();
    let neighborhood: Vec<CId> = actionable
        .iter()
        .copied()
        .filter(|c| {
            if let Some(n) = graph.nodes.iter().find(|n| n.id == *c) {
                if n.span.end > n.span.start && spans.contains(&(n.span.start, n.span.end)) {
                    return true;
                }
            }
            false
        })
        .collect();
    let mut nb: BTreeSet<CId> = blame.clone();
    nb.extend(neighborhood);
    seeds.push(nb.into_iter().collect());

    // Seed 3: all constraints (guarantees a core exists).
    seeds.push(actionable.clone());

    // Seed 4: same-origin expression constraints.
    if let Some(first) = graph.nodes.iter().find(|n| n.id == *target.cids.first().unwrap_or(&CId(0))) {
        if let Some(origin) = first.origin {
            let group: Vec<CId> = actionable
                .iter()
                .copied()
                .filter(|c| graph.nodes.iter().any(|n| n.id == *c && n.origin == Some(origin)))
                .collect();
            let mut g: BTreeSet<CId> = blame.clone();
            g.extend(group);
            seeds.push(g.into_iter().collect());
        }
    }

    let mut best: Option<Vec<CId>> = None;
    let mut candidates: Vec<Vec<CId>> = Vec::new();

    for seed in seeds {
        // Minimize: the seed is a set of "kept" constraints. A core is a
        // subset S such that disabling everything *except* S still reproduces
        // the error, and S is inclusion-minimal.
        let kept0: BTreeSet<CId> = seed.into_iter().collect();
        // First ensure this kept set reproduces the error.
        let disabled0: HashSet<CId> = actionable
            .iter()
            .copied()
            .filter(|c| !kept0.contains(c))
            .collect();
        if !persists(&run(&disabled0)) {
            continue;
        }
        // Delta-debug style greedy removal with different orders.
        let orders = vec![
            kept0.iter().copied().collect::<Vec<CId>>(),
            kept0.iter().rev().copied().collect::<Vec<CId>>(),
        ];
        for order in orders {
            let mut kept: BTreeSet<CId> = kept0.clone();
            for c in order {
                kept.remove(&c);
                let disabled: HashSet<CId> = actionable
                    .iter()
                    .copied()
                    .filter(|x| !kept.contains(x))
                    .collect();
                if persists(&run(&disabled)) {
                    // removal safe
                } else {
                    kept.insert(c);
                }
            }
            let mut corev: Vec<CId> = kept.into_iter().collect();
            corev.sort_by_key(|c| c.0);
            match &best {
                None => best = Some(corev.clone()),
                Some(b) => {
                    if corev.len() < b.len() {
                        best = Some(corev.clone());
                        candidates.clear();
                    }
                }
            }
            if Some(corev.len()) == best.as_ref().map(|b| b.len()) {
                if !candidates.contains(&corev) {
                    candidates.push(corev);
                }
            }
        }
    }

    // Keep only cores at the best size; cap to 6 to avoid flooding.
    let best_len = best.map(|b| b.len()).unwrap_or(0);
    candidates.retain(|c| c.len() == best_len && best_len > 0);
    candidates.sort_by_key(|c| c.iter().map(|x| x.0).collect::<Vec<_>>());
    candidates.dedup();
    candidates.truncate(6);

    // The target error must be an actual error core; guarantee at least one
    // result even when seeds above could not shrink below the blame chain.
    if candidates.is_empty() {
        let mut v: Vec<CId> = target.cids.clone();
        v.sort_by_key(|c| c.0);
        v.dedup();
        if v.is_empty() {
            v = actionable.iter().take(1).copied().collect();
        }
        candidates.push(v);
    }

    let _ = ErrorKind::Mismatch;
    candidates
}
