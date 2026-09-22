//! Minimal unsat cores for constraint sets.
//!
//! A core is a small set of unify constraints that is itself unsatisfiable
//! and which contains the failing "anchor" constraint of one reported error.
//! We compute inclusion-minimal cores by greedy deletion and retain every
//! distinct smallest core found across deterministic shuffles.

use std::collections::HashSet;

use crate::constraints::Op;
use crate::solver::Solver;


/// Replay only the given unify constraint ids; return true if the set is
/// satisfiable.
fn satisfiable(ops: &[Op], enabled: &HashSet<u64>, disabled: &HashSet<u64>) -> bool {
    let mut solver = Solver::new();
    for op in ops {
        if let Op::Unify { id, expected, actual, .. } = op {
            if !enabled.contains(id) || disabled.contains(id) {
                continue;
            }
            for v in expected.vars().into_iter().chain(actual.vars()) {
                solver.ensure_pure(v);
            }
            if solver.unify(expected, actual).is_err() {
                return false;
            }
        }
    }
    true
}

/// Find an inclusion-minimal unsatisfiable subset containing `anchor`,
/// starting from `seed` and deleting in `order`.
fn minimize(
    ops: &[Op],
    seed: &HashSet<u64>,
    anchor: u64,
    disabled: &HashSet<u64>,
    order: impl Iterator<Item = u64>,
) -> Option<HashSet<u64>> {
    let mut core: HashSet<u64> = seed.iter().copied().filter(|id| !disabled.contains(id)).collect();
    if !core.contains(&anchor) {
        return None;
    }
    for candidate in order {
        if candidate == anchor || !core.contains(&candidate) {
            continue;
        }
        let mut trial = core.clone();
        trial.remove(&candidate);
        if satisfiable(ops, &trial, disabled) {
            // cannot remove: keep
        } else {
            core.remove(&candidate);
        }
    }
    if satisfiable(ops, &core, disabled) {
        None
    } else {
        Some(core)
    }
}

/// Compute cores for every distinct failing anchor. Returns the smallest
/// distinct cores (multiple retained when tied).
pub fn compute_cores(
    ops: &[Op],
    anchors: &[u64],
    disabled: &HashSet<u64>,
) -> Vec<Vec<u64>> {
    let all_unify: Vec<u64> = ops
        .iter()
        .filter_map(|o| if let Op::Unify { id, .. } = o { Some(*id) } else { None })
        .filter(|id| !disabled.contains(id))
        .collect();
    let all: HashSet<u64> = all_unify.iter().copied().collect();

    let mut unique_anchors: Vec<u64> = anchors.iter().copied().collect();
    unique_anchors.sort();
    unique_anchors.dedup();

    let mut anchor_cores: Vec<HashSet<u64>> = Vec::new();
    for anchor in unique_anchors {
        if disabled.contains(&anchor) || !all.contains(&anchor) {
            continue;
        }
        let mut orders: Vec<Vec<u64>> = Vec::new();
        orders.push(all_unify.clone());
        let mut reversed = all_unify.clone();
        reversed.reverse();
        orders.push(reversed);
        // deterministic shuffled orders (splitmix64), giving greedy
        // deletion reproducible but different search orders.
        for seed in [0x9e37_79b9_7f4a_7c15u64, 0xbf58_476d_1ce4_e5b9, 0x94d0_49bb_1331_11eb] {
            let mut ord = all_unify.clone();
            let mut state = seed ^ (anchor ^ 0xd1b5_4a42_78b4_2b57);
            for i in (1..ord.len()).rev() {
                state = state.wrapping_add(0x9e37_79b9_7f4a_7c15);
                let mut z = state;
                z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
                z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
                z ^= z >> 31;
                let j = (z as usize) % (i + 1);
                ord.swap(i, j);
            }
            orders.push(ord);
        }

        let mut best_size = usize::MAX;
        let mut best: Vec<HashSet<u64>> = Vec::new();
        for order in orders {
            if let Some(core) = minimize(ops, &all, anchor, disabled, order.into_iter()) {
                if core.len() < best_size {
                    best_size = core.len();
                    best = vec![core];
                } else if core.len() == best_size && !best.contains(&core) {
                    best.push(core);
                }
            }
        }
        anchor_cores.extend(best);
    }

    let mut found: Vec<HashSet<u64>> = Vec::new();
    for c in anchor_cores {
        if !found.contains(&c) {
            found.push(c);
        }
    }

    // normalize: keep all cores of the global minimum size
    let min_size = found.iter().map(|c| c.len()).min().unwrap_or(0);
    let mut out: Vec<Vec<u64>> = found
        .into_iter()
        .filter(|c| c.len() == min_size)
        .map(|c| {
            let mut v: Vec<u64> = c.into_iter().collect();
            v.sort();
            v
        })
        .collect();
    out.sort();
    out.dedup();
    out
}
