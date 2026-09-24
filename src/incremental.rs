//! Incremental editing: reuse surviving AST nodes/constraints, mark the rest.

use serde::Serialize;
use std::collections::{HashMap, HashSet};

use crate::constraints::{Cid, Constraint};
use crate::infer::Analysis;
use crate::syntax::NodeId;

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub enum ConstraintStatus {
    Reused,
    Invalidated,
    Resolved,
}

#[derive(Debug, Clone, Serialize)]
pub struct DiffEntry {
    pub cid: Cid,
    pub old_node: NodeId,
    pub new_node: NodeId,
    pub status: ConstraintStatus,
    pub detail: String,
    pub scope_key: String,
    pub cache_hit: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct IncrementalDiff {
    pub reused: Vec<DiffEntry>,
    pub invalidated: Vec<DiffEntry>,
    pub resolved: Vec<DiffEntry>,
    pub reused_nodes: Vec<NodeId>,
    pub invalidated_nodes: Vec<NodeId>,
    pub new_nodes: Vec<NodeId>,
    /// Pairing cache key: scope + generalization environment + constraint key.
    pub cache_key_example: String,
}

/// Compare two analyses (old, new) and classify constraints.
///
/// A constraint is *reused* when it stems from the same stable node id and
/// its structural key (kinds + type sides rendered) is unchanged *and* the
/// scope/generalization fingerprint that produced it matches. Otherwise it is
/// either invalidated (its node disappeared) or re-solved (same node, changed
/// surroundings). Constraints that genuinely need to run unification again
/// (because their answer changed) land in `resolved`.
pub fn diff_analyses(old: &Analysis, new: &Analysis) -> IncrementalDiff {
    let old_by_node: HashMap<NodeId, Vec<&Constraint>> = group(&old.constraints);
    let new_by_node: HashMap<NodeId, Vec<&Constraint>> = group(&new.constraints);

    let old_node_set: HashSet<NodeId> = old.nodes.iter().map(|n| n.id.clone()).collect();
    let new_node_set: HashSet<NodeId> = new.nodes.iter().map(|n| n.id.clone()).collect();

    let mut reused = Vec::new();
    let mut invalidated = Vec::new();
    let mut resolved = Vec::new();

    // Classify old constraints whose node no longer exists.
    for c in &old.constraints {
        if !new_node_set.contains(&c.origin.node_id) {
            invalidated.push(DiffEntry {
                cid: c.id.clone(),
                old_node: c.origin.node_id.clone(),
                new_node: String::new(),
                status: ConstraintStatus::Invalidated,
                detail: c.detail.clone(),
                scope_key: old
                    .scopes
                    .iter()
                    .find(|s| s.node_id == c.origin.node_id)
                    .map(|s| s.scope_key.clone())
                    .unwrap_or_default(),
                cache_hit: false,
            });
        }
    }

    for c in &new.constraints {
        let node = &c.origin.node_id;
        if !old_node_set.contains(node) {
            resolved.push(DiffEntry {
                cid: c.id.clone(),
                old_node: String::new(),
                new_node: node.clone(),
                status: ConstraintStatus::Resolved,
                detail: c.detail.clone(),
                scope_key: new
                    .scopes
                    .iter()
                    .find(|s| s.node_id == *node)
                    .map(|s| s.scope_key.clone())
                    .unwrap_or_default(),
                cache_hit: false,
            });
            continue;
        }
        // Node survives: compare against old constraints of the same node.
        let olds = old_by_node.get(node).cloned().unwrap_or_default();
        let key = structural_key(c, new);
        let matching = olds
            .iter()
            .find(|o| structural_key(o, old) == key && scope_fingerprint(node, old) == scope_fingerprint(node, new));
        match matching {
            Some(o) => {
                let cache_hit = o.detail == c.detail && genv_compatible(old, new);
                reused.push(DiffEntry {
                    cid: c.id.clone(),
                    old_node: o.origin.node_id.clone(),
                    new_node: node.clone(),
                    status: ConstraintStatus::Reused,
                    detail: c.detail.clone(),
                    scope_key: scope_fingerprint(node, new),
                    cache_hit,
                });
            }
            None => resolved.push(DiffEntry {
                cid: c.id.clone(),
                old_node: node.clone(),
                new_node: node.clone(),
                status: ConstraintStatus::Resolved,
                detail: c.detail.clone(),
                scope_key: scope_fingerprint(node, new),
                cache_hit: false,
            }),
        }
    }

    let reused_nodes: Vec<NodeId> = new
        .nodes
        .iter()
        .map(|n| n.id.clone())
        .filter(|id| old_node_set.contains(id))
        .collect();
    let invalidated_nodes: Vec<NodeId> = old
        .nodes
        .iter()
        .map(|n| n.id.clone())
        .filter(|id| !new_node_set.contains(id))
        .collect();
    let new_nodes: Vec<NodeId> = new
        .nodes
        .iter()
        .map(|n| n.id.clone())
        .filter(|id| !old_node_set.contains(id))
        .collect();

    let cache_key_example =
        "scope=<node-scope>|genv=<generalization-fingerprint>|ck=<kind:lhs~rhs>".to_string();

    IncrementalDiff {
        reused,
        invalidated,
        resolved,
        reused_nodes,
        invalidated_nodes,
        new_nodes,
        cache_key_example,
    }
}

fn group(cs: &[Constraint]) -> HashMap<NodeId, Vec<&Constraint>> {
    let mut m: HashMap<NodeId, Vec<&Constraint>> = HashMap::new();
    for c in cs {
        m.entry(c.origin.node_id.clone()).or_default().push(c);
    }
    m
}

fn structural_key(c: &Constraint, a: &Analysis) -> String {
    // Render the sides from the final snapshot; when the structure changed the
    // key changes, forcing re-solve even when the node id survived.
    let render = |vid: &str| -> String {
        a.final_snapshot
            .iter()
            .find(|tv| tv.vid == vid)
            .map(|tv| tv.rendered.short())
            .unwrap_or_else(|| vid.to_string())
    };
    format!("{:?}:{}~{}", c.kind, render(&c.left), render(&c.right))
}

fn scope_fingerprint(node: &str, a: &Analysis) -> String {
    a.scopes
        .iter()
        .filter(|s| s.node_id == *node)
        .map(|s| format!("{}:{}", s.scope_key, s.polymorphic))
        .collect::<Vec<_>>()
        .join(",")
}

fn genv_compatible(old: &Analysis, new: &Analysis) -> bool {
    old.genv_fingerprint == new.genv_fingerprint
}
