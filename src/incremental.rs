//! 增量编辑对齐：按结构位置对齐 AST 节点，按缓存键对齐约束。
use crate::analyze::{analyze_program, Analysis};
use crate::ast::*;
use crate::constraints::Constraint;
use crate::solve::CId;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Serialize)]
pub struct NodePair { pub old: u32, pub new: u32 }

#[derive(Clone, Debug, Serialize)]
pub struct ConstraintPair {
    pub old_id: u32,
    pub new_id: u32,
    pub old_key: String,
    pub new_key: String,
    pub node_old: u32,
    pub node_new: u32,
}

#[derive(Clone, Debug, Serialize)]
pub struct DiffResult {
    /// 位置对齐成功的节点对（id 可能变化，但结构身份保持）。
    pub aligned_nodes: Vec<NodePair>,
    pub reused_nodes: u32,
    pub total_new_nodes: u32,
    /// key 完全一致：解析节点与约束均复用。
    pub reused: Vec<ConstraintPair>,
    /// 旧约束无对应新键：失效。
    pub invalidated: Vec<ConstraintPair>,
    /// 节点对齐但 key 改变（作用域/泛化环境受影响）：重新求解。
    pub resolved: Vec<ConstraintPair>,
    /// 全新节点带来的约束。
    pub fresh: Vec<ConstraintPair>,
    pub summary: DiffSummary,
}

#[derive(Clone, Debug, Serialize)]
pub struct DiffSummary {
    pub reused: usize,
    pub invalidated: usize,
    pub resolved: usize,
    pub fresh: usize,
}

/// 按结构位置对齐两棵树：返回 old_nid -> new_nid。
fn align_trees(a: &Expr, b: &Expr) -> BTreeMap<NodeId, NodeId> {
    let mut map = BTreeMap::new();
    align_rec(a, b, &mut map);
    map
}

fn align_rec(a: &Expr, b: &Expr, map: &mut BTreeMap<NodeId, NodeId>) {
    // 结构种类一致才在该位置对齐（忽略 span 与绑定名）。
    if a.kind() != b.kind() { return; }
    map.insert(a.nid(), b.nid());
    let mut ca = vec![];
    let mut cb = vec![];
    a.children(&mut ca);
    b.children(&mut cb);
    // match 的 arm 按标签顺序对齐已由 children 顺序保证；
    // record 字段也按顺序对齐。若数量不同，按可对齐前缀处理。
    let n = ca.len().min(cb.len());
    for i in 0..n {
        // 对于 record/match，额外要求标签一致，避免错位。
        if compatible_slot(a, b, i, ca[i], cb[i]) {
            align_rec(ca[i], cb[i], map);
        }
    }
}

fn compatible_slot(parent_a: &Expr, parent_b: &Expr, i: usize, a: &Expr, b: &Expr) -> bool {
    if a.kind() != b.kind() { return false; }
    match (parent_a, parent_b) {
        (Expr::Record { fields: fa, .. }, Expr::Record { fields: fb, .. }) => {
            fa.get(i).zip(fb.get(i)).map(|(x, y)| x.name == y.name).unwrap_or(false)
        }
        (Expr::Match { arms: aa, .. }, Expr::Match { arms: ab, .. }) => {
            // children 顺序为 scrutinee 然后各 arm body；i=0 为 scrutinee
            if i == 0 { true }
            else {
                aa.get(i - 1).zip(ab.get(i - 1)).map(|(x, y)| x.label == y.label).unwrap_or(false)
            }
        }
        _ => true,
    }
}

pub fn diff(old: &Analysis, old_program: &Program, new_program: &Program,
            disabled: &BTreeSet<CId>) -> DiffResult {
    let new = analyze_program(new_program, disabled);

    let node_map = align_trees(&old_program.root, &new_program.root);
    let aligned: Vec<NodePair> = node_map.iter().map(|(o, n)| NodePair { old: o.0, new: n.0 }).collect();
    let reused_nodes = node_map.len() as u32;

    // 约束对齐：优先按 key 全等；否则按 (种类, 对齐节点) 归为 resolved。
    let new_key_map: BTreeMap<u128, Vec<&Constraint>> = {
        let mut m: BTreeMap<u128, Vec<&Constraint>> = BTreeMap::new();
        for c in &new.cons { m.entry(c.key).or_default().push(c); }
        m
    };
    let old_key_map: BTreeMap<u128, Vec<&Constraint>> = {
        let mut m: BTreeMap<u128, Vec<&Constraint>> = BTreeMap::new();
        for c in &old.cons { m.entry(c.key).or_default().push(c); }
        m
    };

    let mut reused = vec![];
    let mut matched_new: BTreeSet<CId> = BTreeSet::new();
    let mut matched_old: BTreeSet<CId> = BTreeSet::new();

    for (key, olds) in &old_key_map {
        if let Some(news) = new_key_map.get(key) {
            let n = olds.len().min(news.len());
            for i in 0..n {
                let o = olds[i];
                let w = news[i];
                matched_old.insert(o.id);
                matched_new.insert(w.id);
                reused.push(pair(o, w));
            }
        }
    }

    // 剩余旧约束：按 (kind, 对齐后的 node) 寻找新约束 -> resolved，否则 invalidated。
    let mut resolved = vec![];
    let mut invalidated = vec![];
    let mut new_pending: BTreeMap<(String, u32), Vec<&Constraint>> = BTreeMap::new();
    for c in &new.cons {
        if !matched_new.contains(&c.id) {
            new_pending.entry((c.kind.clone(), c.node.0)).or_default().push(c);
        }
    }
    let mut new_pending_used: BTreeSet<CId> = BTreeSet::new();
    for oc in &old.cons {
        if matched_old.contains(&oc.id) { continue; }
        let mapped_node = node_map.get(&oc.node).copied();
        let hit = mapped_node.and_then(|nn| {
            new_pending.get_mut(&(oc.kind.clone(), nn.0)).and_then(|v| {
                if v.is_empty() { None } else {
                    let w = v.remove(0);
                    new_pending_used.insert(w.id);
                    Some(w)
                }
            })
        });
        match hit {
            Some(w) => {
                matched_new.insert(w.id);
                resolved.push(pair(oc, w));
            }
            None => invalidated.push(pair(oc, oc)),
        }
    }

    let mut fresh = vec![];
    for c in &new.cons {
        if !matched_new.contains(&c.id) {
            fresh.push(pair(c, c));
        }
    }

    let summary = DiffSummary {
        reused: reused.len(), invalidated: invalidated.len(),
        resolved: resolved.len(), fresh: fresh.len(),
    };
    DiffResult {
        aligned_nodes: aligned, reused_nodes, total_new_nodes: new_program.nodes.len() as u32,
        reused, invalidated, resolved, fresh, summary,
    }
}

fn pair(o: &Constraint, w: &Constraint) -> ConstraintPair {
    ConstraintPair {
        old_id: o.id.0, new_id: w.id.0,
        old_key: format!("{:032x}", o.key), new_key: format!("{:032x}", w.key),
        node_old: o.node.0, node_new: w.node.0,
    }
}
