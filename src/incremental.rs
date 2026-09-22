// Incremental analysis reuse between two edits.
//
// Parse nodes are matched by structural path (stable under renaming of
// binders). Constraints are matched by a key that includes the path, the
// enclosing binder scope and the generalization-environment fingerprint.
// Therefore renaming a variable cannot make identities drift, while a change
// of polymorphism at an enclosing let deliberately invalidates downstream
// uses.

use crate::constraints::*;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReuseReport {
    pub reused_constraints: Vec<u32>,
    pub invalidated_constraints: Vec<u32>,
    pub new_constraints: Vec<u32>,
    pub reused_constraint_keys: Vec<String>,
    pub invalidated_constraint_keys: Vec<String>,
    pub new_constraint_keys: Vec<String>,
    pub reused_ast_nodes: Vec<u32>,
    pub invalidated_ast_nodes: Vec<u32>,
    pub new_ast_nodes: Vec<u32>,
    /// Counts of constraints that still need solving vs. reused as already
    /// solved in the previous version.
    pub solved_reuse_count: usize,
}

pub fn compare(
    old: &ConstraintGraph,
    new: &ConstraintGraph,
    previously_solved: &HashSet<CId>,
) -> ReuseReport {
    // AST reuse by path.
    let old_paths: HashMap<&str, NodeIdT> = old
        .ast_nodes
        .iter()
        .map(|n| (n.path.as_str(), n.id))
        .collect();
    let new_paths: HashMap<&str, NodeIdT> = new
        .ast_nodes
        .iter()
        .map(|n| (n.path.as_str(), n.id))
        .collect();
    let mut reused_ast = Vec::new();
    let mut new_ast = Vec::new();
    for (p, id) in &new_paths {
        if old_paths.contains_key(p) {
            reused_ast.push(id.0);
        } else {
            new_ast.push(id.0);
        }
    }
    let mut invalid_ast = Vec::new();
    for (p, id) in &old_paths {
        if !new_paths.contains_key(p) {
            invalid_ast.push(id.0);
        }
    }

    // Constraint reuse by stable key.
    let old_keys: HashMap<String, (CId, &ConstraintNode)> = old
        .nodes
        .iter()
        .map(|n| (n.key.stable(), (n.id, n)))
        .collect();
    let mut reused = Vec::new();
    let mut reused_keys = Vec::new();
    let mut fresh = Vec::new();
    let mut fresh_keys = Vec::new();
    let mut solved_reuse_count = 0usize;
    for n in &new.nodes {
        let key = n.key.stable();
        if let Some((old_id, _)) = old_keys.get(&key) {
            reused.push(n.id.0);
            reused_keys.push(key.clone());
            if previously_solved.contains(old_id) {
                solved_reuse_count += 1;
            }
        } else {
            fresh.push(n.id.0);
            fresh_keys.push(key.clone());
        }
    }
    let new_key_set: HashSet<String> = new.nodes.iter().map(|n| n.key.stable()).collect();
    let mut invalid = Vec::new();
    let mut invalid_keys = Vec::new();
    for n in &old.nodes {
        let key = n.key.stable();
        if !new_key_set.contains(&key) {
            invalid.push(n.id.0);
            invalid_keys.push(key);
        }
    }

    reused_ast.sort_unstable();
    new_ast.sort_unstable();
    invalid_ast.sort_unstable();
    reused.sort_unstable();
    fresh.sort_unstable();
    invalid.sort_unstable();
    reused_keys.sort();
    fresh_keys.sort();
    invalid_keys.sort();

    ReuseReport {
        reused_constraints: reused,
        invalidated_constraints: invalid,
        new_constraints: fresh,
        reused_constraint_keys: reused_keys,
        invalidated_constraint_keys: invalid_keys,
        new_constraint_keys: fresh_keys,
        reused_ast_nodes: reused_ast,
        invalidated_ast_nodes: invalid_ast,
        new_ast_nodes: new_ast,
        solved_reuse_count,
    }
}
