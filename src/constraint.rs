//! 约束的扁平表示：每条约束有稳定身份 cid、缓存 key 与来源范围。

use crate::syntax::ast::{NodeId, Span};
use crate::types::{ConstraintKind, Scheme, TypeTerm};
use serde::Serialize;

/// 稳定约束身份（结构路径 + 子角色），不依赖变量名。
pub type Cid = String;

#[derive(Clone, PartialEq, Debug)]
pub enum Constraint {
    /// 把某个类型项绑定到节点结果类型（由 Con 规则引入结构）。
    Con { term: TypeTerm, target: u32 },
    /// 两个类型相等。
    Unify { left: u32, right: u32 },
    /// 使用一个已泛化绑定：target = fresh instance(scheme)。
    Inst {
        scheme: Scheme,
        target: u32,
        /// 引用所在节点（用于图边）。
        node: NodeId,
    },
    /// 泛化点：value = root；非递归 let 与顶层 item。
    Gen {
        value_node: NodeId,
        root: u32,
        depth: u32,
        non_expansive: bool,
        rec: bool,
        /// 递归绑定自身的变量（rec=true 时有效，不参与量化）。
        rec_vars: Vec<u32>,
        /// 生成的绑定名（仅展示用）。
        name: String,
    },
    /// 不可直接解决的错误（未绑定标识符等）。
    Error { message: String },
}

#[derive(Clone, PartialEq, Debug)]
pub struct CInfo {
    pub cid: Cid,
    /// 缓存 key：cid 作用域 + 泛化环境摘要 + 结构签名。
    pub key: String,
    pub cons: Constraint,
    pub kind: ConstraintKind,
    pub span: Span,
    pub node: NodeId,
    pub desc: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct ConSummary {
    pub cid: String,
    pub key: String,
    pub kind: String,
    pub desc: String,
    pub span: [u32; 2],
    pub node: NodeId,
    pub disabled: bool,
}

impl CInfo {
    pub fn summary(&self, disabled: bool) -> ConSummary {
        ConSummary {
            cid: self.cid.clone(),
            key: self.key.clone(),
            kind: crate::types::kind_name(self.kind).to_string(),
            desc: self.desc.clone(),
            span: [self.span.start, self.span.end],
            node: self.node,
            disabled,
        }
    }
}

/// 生成器/求解器共用的内部序号（按生成顺序，稠密）。
pub type CIndex = usize;
