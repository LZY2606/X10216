//! 最小不可满足核心:贪心删除法得到 1-极小核心,并搜索其他等小候选。
use crate::constrain::{Constraint, ConstraintKind};
use crate::solve::Solver;
use crate::types::ConId;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashSet};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Core {
    pub error_con: Option<ConId>,
    pub members: Vec<ConId>,
    pub size: usize,
    pub minimal: bool,
    pub spans: Vec<crate::syntax::Span>,
    pub notes: Vec<String>,
}

fn run_unsat(constraints: &[Constraint], enabled: &HashSet<ConId>, next_var: u32) -> bool {
    let mut s = Solver::new(next_var);
    s.eq_filter = Some(enabled.clone());
    s.run(constraints);
    !s.errors.is_empty()
}

fn greedy_minimize(
    constraints: &[Constraint],
    mut keep: HashSet<ConId>,
    next_var: u32,
) -> HashSet<ConId> {
    if !run_unsat(constraints, &keep, next_var) {
        return HashSet::new();
    }
    let order: Vec<ConId> = constraints.iter().map(|c| c.id).filter(|id| keep.contains(id)).collect();
    for c in order {
        if keep.len() <= 1 {
            break;
        }
        let mut trial = keep.clone();
        trial.remove(&c);
        if run_unsat(constraints, &trial, next_var) {
            keep = trial;
        }
    }
    keep
}

/// 为某个错误找核心;保留多个同样小(或更小)的候选。
pub fn find_cores(
    constraints: &[Constraint],
    error_con: Option<ConId>,
    max_cores: usize,
    next_var: u32,
) -> Vec<Core> {
    let error_con = match error_con {
        Some(c) => c,
        None => return vec![],
    };
    let by_id: BTreeMap<ConId, &Constraint> =
        constraints.iter().map(|c| (c.id, c)).collect();
    let err_idx = constraints.iter().position(|c| c.id == error_con).unwrap_or(constraints.len());
    // 候选:错误之前(含)的所有 Eq 约束
    let mut candidate: HashSet<ConId> = constraints[..=err_idx.min(constraints.len() - 1)]
        .iter()
        .filter(|c| c.is_eq())
        .map(|c| c.id)
        .collect();
    if !run_unsat(constraints, &candidate, next_var) {
        return vec![];
    }
    let mut found: Vec<HashSet<ConId>> = Vec::new();
    let first = greedy_minimize(constraints, candidate.clone(), next_var);
    if !first.is_empty() {
        found.push(first.clone());
    }
    // 对已发现核心的每个成员,尝试绕开它再求一个核心 → 不同的极小核心
    for m in first.iter().copied().collect::<Vec<_>>() {
        if found.len() >= max_cores {
            break;
        }
        let mut alt = candidate.clone();
        alt.remove(&m);
        if run_unsat(constraints, &alt, next_var) {
            let core = greedy_minimize(constraints, alt, next_var);
            if !core.is_empty() && !found.contains(&core) {
                found.push(core);
            }
        }
    }
    let min_size = found.iter().map(|s| s.len()).min().unwrap_or(0);
    let mut cores: Vec<Core> = found
        .into_iter()
        .map(|s| {
            let size = s.len();
            let mut members: Vec<ConId> = s.into_iter().collect();
            members.sort_by_key(|id| constraints.iter().position(|c| c.id == *id));
            let mut spans = Vec::new();
            let mut notes = Vec::new();
            for id in &members {
                if let Some(c) = by_id.get(id) {
                    spans.push(c.origin.span);
                    notes.push(c.origin.note.clone());
                }
            }
            Core {
                error_con: Some(error_con),
                members,
                size,
                minimal: size == min_size,
                spans,
                notes,
            }
        })
        .collect();
    cores.sort_by_key(|c| c.size);
    // 标记:同等最小候选(minimal=true 的可能有多个)
    let _ = &mut candidate;
    let _ = BTreeSet::<u32>::new();
    cores
}
