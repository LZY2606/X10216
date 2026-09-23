//! 带来源范围的类型约束。

use crate::ast::NodeId;
use crate::span::Span;
use crate::types::TyId;
use serde::Serialize;
use twox_hash::XxHash64;
use std::hash::{Hash, Hasher};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct CId(pub u64);

/// 构造约束 id。内容寻址：包含结构槽位（由 ast 节点派生）、左右角色与
/// 「解析时的环境/泛化指纹」，满足「缓存 key 含作用域与泛化环境」的要求。
pub fn make_cid(nid: NodeId, role: &str, side: &str, env_fp: u64) -> CId {
    let mut h = XxHash64::with_seed(0xC0_FE_51_47_00_00_00_01);
    nid.hash(&mut h);
    h.write(role.as_bytes());
    h.write(side.as_bytes());
    h.write_u64(env_fp);
    CId(h.finish())
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Constraint {
    /// 两个类型相等（统一）。
    Eq {
        cid: CId,
        nid: NodeId,
        span: Span,
        /// 人类可读的角色，如 "fun 参数 0"。
        role: String,
        left: TyId,
        right: TyId,
    },
    /// 打开一个新的词法层级（let/in 与 lambda）。
    EnterScope {
        cid: CId,
        nid: NodeId,
        span: Span,
        level: u32,
        label: String,
    },
    ExitScope {
        cid: CId,
        nid: NodeId,
        span: Span,
        level: u32,
        /// 是否在此处泛化（let 绑定）。
        generalize: bool,
        /// 泛化后绑定到环境的名字（用于缓存 key 与展示）。
        name: Option<String>,
        /// 绑定表达式是否为「值」（value restriction）。
        is_value: bool,
        /// 被泛化的变量类型。
        root: Option<TyId>,
        label: String,
    },
    /// 把一个已在环境中的名字（方案）实例化到使用点。
    Instantiate {
        cid: CId,
        nid: NodeId,
        span: Span,
        name: String,
        scheme_span: Span,
        scheme_generics: u32,
        to: TyId,
    },
    /// 将名字以方案形式登记到环境（let 绑定点）。
    Bind {
        cid: CId,
        nid: NodeId,
        span: Span,
        name: String,
        scheme_span: Span,
        generics: u32,
        root: TyId,
        generalized: bool,
        is_value: bool,
    },
}

impl Constraint {
    pub fn cid(&self) -> CId {
        match self {
            Constraint::Eq { cid, .. }
            | Constraint::EnterScope { cid, .. }
            | Constraint::ExitScope { cid, .. }
            | Constraint::Instantiate { cid, .. }
            | Constraint::Bind { cid, .. } => *cid,
        }
    }
    pub fn nid(&self) -> NodeId {
        match self {
            Constraint::Eq { nid, .. }
            | Constraint::EnterScope { nid, .. }
            | Constraint::ExitScope { nid, .. }
            | Constraint::Instantiate { nid, .. }
            | Constraint::Bind { nid, .. } => *nid,
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Constraint::Eq { span, .. }
            | Constraint::EnterScope { span, .. }
            | Constraint::ExitScope { span, .. }
            | Constraint::Instantiate { span, .. }
            | Constraint::Bind { span, .. } => *span,
        }
    }
    pub fn kind_name(&self) -> &'static str {
        match self {
            Constraint::Eq { .. } => "eq",
            Constraint::EnterScope { .. } => "enter",
            Constraint::ExitScope { .. } => "exit",
            Constraint::Instantiate { .. } => "instantiate",
            Constraint::Bind { .. } => "bind",
        }
    }
}
