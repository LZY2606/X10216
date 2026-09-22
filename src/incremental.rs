use crate::ast::{NodeId, Span};
use crate::infer::{Constraint, InferError, Step};
use crate::types::{scheme_fingerprint, Scheme, Type};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

/// 单个语法节点的生成缓存:结果类型 + 该子树产生的约束与步骤。
/// 缓存只跳过"生成",约束在重放时仍会逐条重新统一,
/// 因此增量求解与全量求解严格等价。
#[derive(Debug, Clone)]
pub struct CacheEntry {
    pub ty: Type,
    pub constraints: Vec<Constraint>,
    pub steps: Vec<Step>,
    /// 生成阶段产生、但不依附于任何约束的错误(如未绑定变量)。
    pub local_errors: Vec<InferError>,
}

#[derive(Debug, Clone, Default)]
pub struct GenCache {
    /// key: (节点身份, 子树源码哈希, 环境指纹)
    pub map: HashMap<(NodeId, u64, u64), CacheEntry>,
    /// 本版本全部节点的 (身份, 源码哈希),用于统计解析节点复用。
    pub nodes: HashSet<(NodeId, u64)>,
    /// 本版本约束摘要: (约束 id, 内容哈希),用于版本间 diff。
    pub constraints: Vec<(String, u64)>,
}

pub fn hash_str(s: &str) -> u64 {
    let mut h = DefaultHasher::new();
    s.hash(&mut h);
    h.finish()
}

pub fn src_hash(src: &str, span: Span) -> u64 {
    hash_str(&src[span.start..span.end])
}

/// 环境指纹:包含作用域中每个绑定的 (名字, 绑定者身份, 泛化方案指纹)。
/// 方案指纹是结构化的(量化变量归一化),因此缓存 key 同时覆盖
/// 作用域与泛化环境;节点身份与名字无关,变量改名只会让
/// 作用域内的节点重新生成,其余节点身份与缓存全部保持。
pub fn env_fp(env: &[(String, NodeId, Scheme)]) -> u64 {
    let mut h = DefaultHasher::new();
    for (name, binder, scheme) in env {
        name.hash(&mut h);
        binder.hash(&mut h);
        scheme_fingerprint(scheme).hash(&mut h);
    }
    h.finish()
}

pub fn content_hash(c: &Constraint) -> u64 {
    hash_str(&format!(
        "{:?}",
        (&c.kind, &c.lhs, &c.rhs, c.span, &c.note)
    ))
}

#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct Diff {
    /// 两版中身份与内容都一致的约束(被复用)。
    pub reused: Vec<String>,
    /// 旧版有、新版消失或内容变化的约束(失效)。
    pub invalidated: Vec<String>,
    /// 新版新增或内容变化的约束(重新求解)。
    pub resolved: Vec<String>,
}

pub fn diff(old: &[(String, u64)], new: &[(String, u64)]) -> Diff {
    let oldm: HashMap<&str, u64> = old.iter().map(|(k, v)| (k.as_str(), *v)).collect();
    let newm: HashMap<&str, u64> = new.iter().map(|(k, v)| (k.as_str(), *v)).collect();
    let mut d = Diff::default();
    for (id, h) in old {
        match newm.get(id.as_str()) {
            Some(nh) if nh == h => d.reused.push(id.clone()),
            _ => d.invalidated.push(id.clone()),
        }
    }
    for (id, h) in new {
        match oldm.get(id.as_str()) {
            Some(oh) if oh == h => {}
            _ => d.resolved.push(id.clone()),
        }
    }
    d.reused.sort();
    d.invalidated.sort();
    d.resolved.sort();
    d
}
