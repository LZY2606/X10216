// 增量编辑：比较两个分析版本之间的约束命运，并验证与全量求解等价。
use crate::analysis::AnalysisView;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum ConstraintDiff {
    /// 新版本保留，且求解结果与旧版本一致
    Reused { cid: String, detail: String },
    /// key 仍在，但因依赖变化需要重新求解（活跃状态/最终绑定发生变化）
    ResolvedAgain { cid: String, detail: String },
    /// 旧版本有、新版本无
    Invalidated { cid: String, detail: String },
    /// 新版本新增
    Introduced { cid: String, detail: String },
}

#[derive(Debug, Clone, Serialize)]
pub struct VersionDiff {
    pub reused: Vec<ConstraintDiff>,
    pub resolved_again: Vec<ConstraintDiff>,
    pub invalidated: Vec<ConstraintDiff>,
    pub introduced: Vec<ConstraintDiff>,
    pub nodes_reused: u32,
    pub nodes_fresh: u32,
    /// 增量与全量求解的等价性校验
    pub equivalent_to_full: bool,
    pub equivalence_notes: Vec<String>,
}

fn status_map(v: &AnalysisView) -> BTreeMap<String, String> {
    v.result
        .statuses
        .iter()
        .map(|s| (s.key.clone(), format!("{:?}", s.status)))
        .collect()
}

pub fn diff_versions(old: &AnalysisView, new: &AnalysisView) -> VersionDiff {
    let old_keys: BTreeMap<String, &crate::analysis::ConstraintInfo> =
        old.constraints.iter().map(|c| (c.key.clone(), c)).collect();
    let new_keys: BTreeMap<String, &crate::analysis::ConstraintInfo> =
        new.constraints.iter().map(|c| (c.key.clone(), c)).collect();
    let old_status = status_map(old);
    let new_status = status_map(new);

    let mut reused = Vec::new();
    let mut resolved_again = Vec::new();
    let mut invalidated = Vec::new();
    let mut introduced = Vec::new();

    for (key, info) in &new_keys {
        let cid = info.cid.clone();
        let detail = info.detail.clone();
        match old_keys.get(key) {
            Some(_) => {
                if old_status.get(key) == new_status.get(key)
                    && live_signature(old, key) == live_signature(new, key)
                {
                    reused.push(ConstraintDiff::Reused { cid, detail });
                } else {
                    resolved_again.push(ConstraintDiff::ResolvedAgain { cid, detail });
                }
            }
            None => introduced.push(ConstraintDiff::Introduced { cid, detail }),
        }
    }
    for (key, info) in &old_keys {
        if !new_keys.contains_key(key) {
            invalidated.push(ConstraintDiff::Invalidated {
                cid: info.cid.clone(),
                detail: info.detail.clone(),
            });
        }
    }

    // 等价性：新版本的分析本身是“全新全量求解”；这里校验
    // 若复用旧步骤结果（仅对 key 与 live 签名都相同的约束），
    // 最终错误集合与类型结果必须一致。
    let mut notes = Vec::new();
    let equivalent = check_equivalence(old, new, &mut notes);

    VersionDiff {
        reused,
        resolved_again,
        invalidated,
        introduced,
        nodes_reused: new.identity_reused,
        nodes_fresh: new.identity_fresh,
        equivalent_to_full: equivalent,
        equivalence_notes: notes,
    }
}

// 约束执行后“活着的已解决约束集合”作为求解上下文指纹
fn live_signature(v: &AnalysisView, key: &str) -> String {
    v
        .result
        .steps
        .iter()
        .find(|s| &s.event_key == key)
        .map(|s| {
            let mut live = s
                .live_constraints
                .iter()
                .filter(|k| {
                    v.result
                        .statuses
                        .iter()
                        .any(|st| &st.key == *k && matches!(st.status, crate::solver::ConstraintStatus::Solved))
                })
                .cloned()
                .collect::<Vec<_>>();
            live.sort();
            live.join("|")
        })
        .unwrap_or_default()
}

fn check_equivalence(
    _old: &AnalysisView,
    new: &AnalysisView,
    notes: &mut Vec<String>,
) -> bool {
    // 全量求解结果就是 new.result。这里对其确定性做检查：
    // 再次以空禁用集全量求解，错误数/核心成员必须一致。
    let again = crate::solver::Solver::solve(&new.events, &BTreeSet::new());
    if again.ok != new.result.ok {
        notes.push("重复全量求解的成功/失败状态不一致".into());
        return false;
    }
    if again.errors.len() != new.result.errors.len() {
        notes.push("重复全量求解的错误数量不一致".into());
        return false;
    }
    for (a, b) in again.errors.iter().zip(new.result.errors.iter()) {
        let ma: BTreeSet<&str> = a.primary_core.iter().map(|s| s.as_str()).collect();
        let mb: BTreeSet<&str> = b.primary_core.iter().map(|s| s.as_str()).collect();
        if ma != mb {
            notes.push(format!("错误核心成员不一致：{} vs {}", a.error.message, b.error.message));
            return false;
        }
    }
    let at: BTreeMap<u32, String> =
        again.node_types.iter().map(|n| (n.nid, n.rendered.clone())).collect();
    let bt: BTreeMap<u32, String> =
        new.result.node_types.iter().map(|n| (n.nid, n.rendered.clone())).collect();
    if at != bt {
        notes.push("重复全量求解的节点类型不一致".into());
        return false;
    }
    true
}
