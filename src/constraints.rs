//! 约束与标记的定义。求解器按编号顺序执行这张“切片表”。
use crate::ast::{NodeId, Span};
use crate::solve::{CId, Polarity};

#[derive(Clone, Copy, Debug)]
pub enum Prim { Int, Bool, Unit }

/// 一条约束或一次作用域/泛化标记。所有条目按执行顺序编号。
#[derive(Clone, Debug)]
pub enum Con {
    /// node : prim
    ExpectPrim { node: NodeId, prim: Prim, span: Span },
    /// node = a -> b
    MkArrow { node: NodeId, arg: NodeId, ret: NodeId, span: Span },
    /// node 为记录；closed=false 表示只要求其中列出的字段（开放）。
    MkRecord { node: NodeId, fields: Vec<(String, NodeId, Span)>, closed: bool, span: Span },
    /// node 为变体行，含一个标签。
    MkVariant { node: NodeId, label: String, payload: NodeId, open: bool, span: Span },
    /// a 与 b 一致；pol 决定错误时的期望/实际方向。
    Eq { a: NodeId, b: NodeId, pol: Polarity, label: String, span: Span },
    /// 变量引用（实例化多态绑定）。
    VarUse { node: NodeId, name: String, span: Span },
    /// 进入词法层级（不负责绑定压栈）。
    ScopeBegin { level_up: bool, span: Span },
    ScopeEnd { level_down: bool, span: Span },
    /// 压入一个词法绑定（lambda 参数 / match 载荷），到 ScopeEnd 弹出。
    BindLocal { name: String, node: NodeId, kind: BindKind, span: Span },
    /// 普通 let：对 value 泛化后以 name 绑定。
    GenBind { name: String, value: NodeId, is_value: bool, span: Span },
    /// 递归 let：先压单态锚点。
    RecBegin { name: String, anchor: NodeId, span: Span },
    /// 递归 let 结束：对锚点泛化。
    RecEnd { name: String, anchor: NodeId, is_value: bool, span: Span },
    /// 显式失败约束（如重复字段）。
    Fail { message: String, span: Span, node: NodeId },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BindKind { LamParam, LetPoly, LetMono, Rec, ArmPayload }

#[derive(Clone, Debug)]
pub struct Constraint {
    pub id: CId,
    pub node: NodeId,
    pub span: Span,
    pub kind: String,
    pub label: String,
    pub con: Con,
    /// 约束缓存键：结构指纹 + 作用域 + 泛化环境。
    pub key: u128,
    /// 所属约束是否可在“禁用一条”中被用户切换（Eq/Expect/Mk*/Fail/VarUse）。
    pub toggleable: bool,
}
