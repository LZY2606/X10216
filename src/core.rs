//! Minimal unsatisfiable core extraction.

use crate::infer::{analyze, AnalyzeOptions, Analysis};
use crate::constraints::{CoreInfo, CoreMember, Cid};
use std::collections::HashSet;

pub fn base_errors(a: &Analysis) -> Vec<Cid> {
    a.errors.iter().map(|e| e.cid.clone()).collect()
}

pub fn disable_set(extra: &[Cid], user_disabled: &HashSet<Cid>) -> HashSet<Cid> {
    let mut s = user_disabled.clone();
    for c in extra {
        s.insert(c.clone());
    }
    s
}

/// Find up to `keep` distinct *minimum-size* unsat cores by greedy deletion.
/// `user_disabled` holds constraints already switched off in the UI; the
/// search only considers active constraints.
pub fn minimal_cores(src: &str, user_disabled: &HashSet<Cid>, keep: usize) -> Vec<CoreInfo> {
    let baseline = analyze(src, AnalyzeOptions { disabled: user_disabled.clone(), run_label: String::new() });
    if baseline.errors.is_empty() {
        return vec![];
    }
    let failing: Vec<Cid> = baseline
        .errors
        .iter()
        .map(|e| e.cid.clone())
        .collect();

    let constraint_detail: std::collections::HashMap<Cid, _> = baseline
        .constraints
        .iter()
        .map(|c| (c.id.clone(), c.clone()))
        .collect();

    // Try every failing constraint as the first "seed" to keep; greedy shrink.
    let mut found: Vec<(usize, Vec<Cid>)> = Vec::new();
    for seed in &failing {
        let mut removed: HashSet<Cid> = failing.iter().cloned().collect();
        removed.remove(seed);
        // Keep seed; attempt to re-add (i.e. un-disable) other constraints one
        // by one while the program remains failing. Order by constraint order.
        let mut active: Vec<Cid> = vec![seed.clone()];
        let mut others: Vec<Cid> = baseline
            .constraints
            .iter()
            .map(|c| c.id.clone())
            .filter(|c| c != seed && !user_disabled.contains(c))
            .collect();
        others.sort_by_key(|c| {
            constraint_detail.get(c).map(|x| x.order).unwrap_or(0)
        });
        for c in others {
            // Candidate active set = active + c; compute its complement among
            // *all* constraints to disable. Program must remain unsatisfiable
            // for c to be removable from the core.
            let mut candidate = active.clone();
            candidate.push(c.clone());
            let cand_set: HashSet<Cid> = candidate.iter().cloned().collect();
            let mut disable: HashSet<Cid> = user_disabled.clone();
            for all in baseline.constraints.iter().map(|x| x.id.clone()) {
                if !cand_set.contains(&all) {
                    disable.insert(all);
                }
            }
            let probe = analyze(
                src,
                AnalyzeOptions { disabled: disable, run_label: String::new() },
            );
            if !probe.errors.is_empty() {
                active = candidate;
            }
        }
        // active is a (locally) minimal hitting active set: every member needed.
        active.sort_by_key(|c| constraint_detail.get(c).map(|x| x.order).unwrap_or(0));
        let key = (active.len(), active.clone());
        if !found.contains(&key) {
            found.push(key);
        }
        if found.len() >= keep * 4 {
            break;
        }
    }
    let min_size = found.iter().map(|(n, _)| *n).min().unwrap_or(0);
    let mut cores: Vec<CoreInfo> = found
        .into_iter()
        .filter(|(n, _)| *n == min_size)
        .take(keep)
        .enumerate()
        .map(|(i, (_, members))| build_core(&members, &constraint_detail, i + 1))
        .collect();
    cores.truncate(keep);
    cores
}

fn build_core(members: &[Cid], detail: &std::collections::HashMap<Cid, crate::constraints::Constraint>, attempt: usize) -> CoreInfo {
    let reasons = members
        .iter()
        .map(|cid| {
            let c = detail.get(cid).cloned().unwrap_or_else(|| panic!("missing constraint {cid}"));
            CoreMember {
                cid: cid.clone(),
                node_id: c.origin.node_id,
                node_kind: c.origin.node_kind,
                detail: c.detail,
            }
        })
        .collect();
    CoreInfo {
        members: members.to_vec(),
        removable: members.to_vec(),
        size: members.len(),
        reasons,
        found_at_try: attempt,
    }
}
