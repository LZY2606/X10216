//! 语法树与稳定身份。
//!
//! `NodeId` 是“结构路径哈希”：由父节点 id 与角色（参数位置、字段名等）
//! 迭代混合得到，不依赖绑定的标识符名字。因此变量改名不会导致整棵树身份漂移，
//! 而字段名/变体标签/结构位置改变会使对应子树 id 改变。

use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u64);

impl Serialize for NodeId {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&self.to_hex())
    }
}
impl<'de> Deserialize<'de> for NodeId {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let s = String::deserialize(d)?;
        u64::from_str_radix(s.trim_start_matches("n"), 16)
            .map(NodeId)
            .map_err(|_| serde::de::Error::custom("bad node id"))
    }
}

impl NodeId {
    pub const ROOT: NodeId = NodeId(0xcbf29ce484222325);
    /// 由父 id + 角色常量 + 可选索引派生子节点身份（FNV-1a 迭代混合）。
    pub fn child(self, role: u64, index: u64) -> NodeId {
        let mut h = self.0 ^ 0xdead_c0de;
        for mut x in [role, index] {
            for _ in 0..8 {
                h ^= x & 0xff;
                h = h.wrapping_mul(0x100000001b3);
                x >>= 8;
            }
        }
        NodeId(h)
    }
    pub fn to_hex(self) -> String {
        format!("n{:016x}", self.0)
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}
impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

/// 源码区间，字节偏移，半开区间。
#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Debug)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    /// 标识符引用（名字解析在生成阶段完成）。
    Var(String),
    Lam { pats: Vec<NodeId>, body: NodeId },
    App { fun: NodeId, args: Vec<NodeId> },
    Let { binds: Vec<LetBind>, rec_: bool, body: NodeId },
    Rec { fields: Vec<(String, NodeId)> },
    /// `` `Tag payload ``；payload 为 None 表示无载荷变体。
    Variant { label: String, payload: Option<NodeId> },
    Field { obj: NodeId, label: String },
    Match { scrut: NodeId, arms: Vec<MatchArm> },
    If { cond: NodeId, then_: NodeId, else_: NodeId },
    /// `ref e`
    NewRef(NodeId),
    /// `!e`
    Deref(NodeId),
    /// `(e : T)`
    Ann { expr: NodeId, ty: NodeId },
    /// 位于赋值左侧的字段路径，如 `r.x := e`（仅作展示/位置用）。
    Assign { target: NodeId, value: NodeId },
    /// 整个程序（隐式序列容器）。
    Program { items: Vec<NodeId>, result: Option<NodeId> },
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct LetBind {
    pub pat: NodeId,
    pub rhs: NodeId,
    pub ann: Option<NodeId>,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct MatchArm {
    pub pat: NodeId,
    pub expr: NodeId,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum Pat {
    Wild,
    Var(String),
    Int(i64),
    Bool(bool),
    Str(String),
    Variant { label: String, payload: Option<NodeId> },
    Rec { fields: Vec<(String, NodeId)>, open: bool },
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub enum TyAst {
    Con(String),
    Var(String),
    Fun(Vec<NodeId>, NodeId),
    Rec { fields: Vec<(String, NodeId)>, open: bool },
    Variant { fields: Vec<(String, Option<NodeId>)>, open: bool },
    Ref(NodeId),
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct AstNode<T> {
    pub id: NodeId,
    pub span: Span,
    pub node: T,
}

pub type ExprNode = AstNode<Expr>;
pub type PatNode = AstNode<Pat>;
pub type TyNode = AstNode<TyAst>;

/// 一条顶层绑定（无 `in`，程序级）。
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug)]
pub struct TopBind {
    pub bind: LetBind,
    pub rec_: bool,
}

#[derive(Clone, PartialEq, Serialize, Deserialize, Debug, Default)]
pub struct Program {
    pub tops: Vec<TopBind>,
    pub result: Option<NodeId>,
}

/// 一次解析的完整产物。
#[derive(Clone, PartialEq, Serialize, Deserialize, Debug, Default)]
pub struct Ast {
    pub program: Program,
    pub exprs: Vec<ExprNode>,
    pub pats: Vec<PatNode>,
    pub tys: Vec<TyNode>,
}

impl Ast {
    pub fn enode(&self, id: NodeId) -> Option<&ExprNode> {
        self.exprs.iter().find(|n| n.id == id)
    }
    pub fn pnode(&self, id: NodeId) -> Option<&PatNode> {
        self.pats.iter().find(|n| n.id == id)
    }
    pub fn tynode(&self, id: NodeId) -> Option<&TyNode> {
        self.tys.iter().find(|n| n.id == id)
    }
}
