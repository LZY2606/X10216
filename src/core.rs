//! Minimal unsatisfiable cores: shrink the constraint prefix that leads to a
//! failure by greedy deletion. Several deletion orders are tried and all
//! distinct cores of the minimal size are kept as candidates.

use crate::constrain::Constraint;
use crate::solve::Solver;
use std::collections::BTreeSet;

fn is_unsat(
    constraints: &[Constraint],
    disabled: &BTreeSet<String>,
    next_var: u32,
    target: &str,
) -> bool {
    // "Unsatisfiable" means the *target* constraint still fails with a type
    // error. Unbound variables are scoping artifacts of removed constraints
    // and are ignored.
    let mut s = Solver::new(next_var);
    s.recover = true;
    s.run(constraints, disabled);
    s.errors.iter().any(|e| {
        e.constraint_id == target
            && !matches!(e.error, crate::solve::SolveError::Unbound { .. })
    })
}

fn minimize_with_order(
    constraints: &[Constraint],
    prefix_len: usize,
    order: &[usize],
    next_var: u32,
    target: &str,
) -> BTreeSet<String> {
    let mut removed: BTreeSet<usize> = BTreeSet::new();
    for &i in order {
        if i >= prefix_len || removed.contains(&i) {
            continue;
        }
        let mut trial: BTreeSet<String> = removed
            .iter()
            .map(|&j| constraints[j].id.clone())
            .collect();
        trial.insert(constraints[i].id.clone());
        if is_unsat(constraints, &trial, next_var, target) {
            removed.insert(i);
        }
    }
    // core = prefix minus removed
    (0..prefix_len)
        .filter(|i| !removed.contains(i))
        .map(|i| constraints[i].id.clone())
        .collect()
}

/// Compute minimal unsat cores for the error that fired at constraint
/// `fail_index`. Returns all distinct minimal-size cores found.
pub fn unsat_cores(
    constraints: &[Constraint],
    fail_index: usize,
    next_var: u32,
) -> Vec<BTreeSet<String>> {
    let n = fail_index + 1;
    let target = constraints[fail_index].id.clone();
    let forward: Vec<usize> = (0..n).collect();
    let reverse: Vec<usize> = (0..n).rev().collect();
    // a few deterministic shuffles for diversity
    let mut orders = vec![forward, reverse];
    for seed in 1u64..4 {
        let mut o: Vec<usize> = (0..n).collect();
        let mut state = seed.wrapping_mul(0x9E3779B97F4A7C15);
        for i in (1..o.len()).rev() {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            let j = (state % (i as u64 + 1)) as usize;
            o.swap(i, j);
        }
        orders.push(o);
    }
    let mut cores: Vec<BTreeSet<String>> = Vec::new();
    for order in &orders {
        let core = minimize_with_order(constraints, n, order, next_var, &target);
        if !cores.contains(&core) {
            cores.push(core);
        }
    }
    if cores.is_empty() {
        return cores;
    }
    let min = cores.iter().map(|c| c.len()).min().unwrap();
    cores.retain(|c| c.len() == min);
    cores
}
