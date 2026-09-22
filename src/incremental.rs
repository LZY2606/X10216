pub mod prepare;

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct ConstraintVersion {
    pub fingerprint: String,
    pub cache_key: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct ConstraintDiff {
    pub reused: BTreeSet<String>,
    pub invalidated: BTreeSet<String>,
    pub new: BTreeSet<String>,
    pub re_solved: BTreeSet<String>,
}

pub fn diff_constraints(old: &[ConstraintVersion], new: &[ConstraintVersion]) -> ConstraintDiff {
    let old_fps: BTreeSet<_> = old.iter().map(|item| item.fingerprint.clone()).collect();
    let new_fps: BTreeSet<_> = new.iter().map(|item| item.fingerprint.clone()).collect();
    let old_keys: BTreeSet<_> = old.iter().map(|item| item.cache_key.clone()).collect();
    let new_keys: BTreeSet<_> = new.iter().map(|item| item.cache_key.clone()).collect();
    let reused = old_fps.intersection(&new_fps).cloned().collect();
    let invalidated = old_fps.difference(&new_fps).cloned().collect();
    let new_only: BTreeSet<String> = new_fps.difference(&old_fps).cloned().collect();
    let old_by_key: std::collections::BTreeMap<_, _> = old.iter().map(|item| (item.cache_key.clone(), item.fingerprint.clone())).collect();
    let new_by_key: std::collections::BTreeMap<_, _> = new.iter().map(|item| (item.cache_key.clone(), item.fingerprint.clone())).collect();
    let re_solved = old_by_key.iter().filter_map(|(key, old_fp)| {
        let new_fp = new_by_key.get(key)?;
        (new_fp != old_fp).then(|| new_fp.clone())
    }).collect();
    ConstraintDiff { reused, invalidated, new: new_only, re_solved }
}

pub fn stable_hash(text: &str) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in text.as_bytes() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash | 1
}
