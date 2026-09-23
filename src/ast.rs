//! 抽象语法树。每个节点带源码范围与 Merkle 内容哈希。
//!
//! 节点“身份” `NodeId` 是对 (种类, 子节点身份, 绑定名/字面量) 做的内容哈希，
//! 因此重命名某个变量只会改变引用该名字的节点，其它节点身份不变。

use crate::hash::H64;
use crate::span::Span;
use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(transparent)]
pub struct NodeId(pub H64);
impl std::fmt::Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "N{}", self.0)
    }
}
impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum NodeKind {
    Int(i64),
    Str(String),
    Bool(bool),
    Unit,
    Var(String),
    Lam { param: String, ann: Option<TId>, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Let { name: String, rec_: bool, ann: Option<TId>, bound: NodeId, body: NodeId },
    Record { open: bool, fields: Vec<(String, NodeId)> },
    Proj { field: String, obj: NodeId },
    Tag { label: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<Arm> },
    Ann { expr: NodeId, ann: TId },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Arm {
    pub label: String,
    pub binding: Option<String>,
    pub body: NodeId,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub span: Span,
}

// ---------- 类型标注的小型 AST ----------

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(transparent)]
pub struct TId(pub H64);
impl std::fmt::Debug for TId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "T{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TKind {
    TVar(String),
    TCon(String),
    TArrow(TId, TId),
    TRecord { closed: bool, fields: Vec<(String, TId)>, row: Option<String> },
    TVariant { closed: bool, fields: Vec<(String, TId)>, row: Option<String> },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct TNode {
    pub id: TId,
    pub kind: TKind,
    pub span: Span,
}

// ---------- 程序 ----------

#[derive(Clone, Debug, Default, Serialize)]
pub struct Program {
    pub nodes: Vec<Node>,
    pub types: Vec<TNode>,
    /// 顶层按顺序排列；最后一个是结果表达式。
    pub top: Vec<NodeId>,
    pub ann_of_node: std::collections::HashMap<usize, TId>,
}

impl Program {
    pub fn new() -> Self {
        Program::default()
    }
    pub fn node(&self, id: NodeId) -> &Node {
        self.nodes.iter().find(|n| n.id == id).expect("node")
    }
    pub fn node_idx(&self, idx: usize) -> &Node {
        &self.nodes[idx]
    }
    pub fn tnode(&self, id: TId) -> &TNode {
        self.types.iter().find(|n| n.id == id).expect("tnode")
    }
}

/// 构建 AST 时计算 Merkle 身份。
pub struct ArenaBuilder {
    pub prog: Program,
}

impl ArenaBuilder {
    pub fn new() -> Self {
        ArenaBuilder { prog: Program::new() }
    }

    fn intern_type(&mut self, kind: TKind, span: Span) -> TId {
        let mut h = H64::new().byte(b't');
        match &kind {
            TKind::TVar(n) => h = h.tag(1).str(n),
            TKind::TCon(n) => h = h.tag(2).str(n),
            TKind::TArrow(a, b) => h = h.tag(3).mix(a.0).mix(b.0),
            TKind::TRecord { closed, fields, row } => {
                h = h.tag(4).byte(if *closed { 1 } else { 0 });
                for (f, t) in fields {
                    h = h.str(f).mix(t.0);
                }
                if let Some(r) = row {
                    h = h.str(r);
                }
            }
            TKind::TVariant { closed, fields, row } => {
                h = h.tag(5).byte(if *closed { 1 } else { 0 });
                for (f, t) in fields {
                    h = h.str(f).mix(t.0);
                }
                if let Some(r) = row {
                    h = h.str(r);
                }
            }
        }
        let id = TId(h);
        if !self.prog.types.iter().any(|n| n.id == id) {
            self.prog.types.push(TNode { id, kind, span });
        }
        id
    }

    pub fn mk_type(&mut self, kind: TKind, span: Span) -> TId {
        self.intern_type(kind, span)
    }

    fn mk(&mut self, kind: NodeKind, span: Span) -> NodeId {
        let mut h = H64::new().byte(b'n');
        match &kind {
            NodeKind::Int(i) => h = h.tag(1).u64(*i as u64),
            NodeKind::Str(s) => h = h.tag(2).str(s),
            NodeKind::Bool(b) => h = h.tag(3).byte(*b as u8),
            NodeKind::Unit => h = h.tag(4),
            NodeKind::Var(n) => h = h.tag(5).str(n),
            NodeKind::Lam { param, ann, body } => {
                h = h.tag(6).str(param);
                if let Some(a) = ann {
                    h = h.mix(a.0);
                }
                h = h.mix(body.0);
            }
            NodeKind::App { fun, arg } => h = h.tag(7).mix(fun.0).mix(arg.0),
            NodeKind::Let { name, rec_, ann, bound, body } => {
                h = h.tag(8).str(name).byte(*rec_ as u8);
                if let Some(a) = ann {
                    h = h.mix(a.0);
                }
                h = h.mix(bound.0).mix(body.0);
            }
            NodeKind::Record { open, fields } => {
                h = h.tag(9).byte(*open as u8);
                for (f, n) in fields {
                    h = h.str(f).mix(n.0);
                }
            }
            NodeKind::Proj { field, obj } => h = h.tag(10).str(field).mix(obj.0),
            NodeKind::Tag { label, payload } => {
                h = h.tag(11).str(label);
                if let Some(p) = payload {
                    h = h.mix(p.0);
                }
            }
            NodeKind::Match { scrut, arms } => {
                h = h.tag(12).mix(scrut.0);
                for a in arms {
                    h = h.str(a.label);
                    if let Some(b) = a.binding {
                        h = h.str(b);
                    }
                    h = h.mix(a.body.0);
                }
            }
            NodeKind::Ann { expr, ann } => h = h.tag(13).mix(expr.0).mix(ann.0),
        }
        let id = NodeId(h);
        if let Some(existing) = self.prog.nodes.iter().find(|n| n.id == id) {
            return existing.id;
        }
        self.prog.nodes.push(Node { id, kind, span });
        id
    }

    // 便利构造器（解析器使用）
    pub fn int(&mut self, i: i64, sp: Span) -> NodeId {
        self.mk(NodeKind::Int(i), sp)
    }
    pub fn str(&mut self, s: String, sp: Span) -> NodeId {
        self.mk(NodeKind::Str(s), sp)
    }
    pub fn bool(&mut self, b: bool, sp: Span) -> NodeId {
        self.mk(NodeKind::Bool(b), sp)
    }
    pub fn unit(&mut self, sp: Span) -> NodeId {
        self.mk(NodeKind::Unit, sp)
    }
    pub fn var(&mut self, n: String, sp: Span) -> NodeId {
        self.mk(NodeKind::Var(n), sp)
    }
    pub fn lam(&mut self, param: String, ann: Option<TId>, body: NodeId, sp: Span) -> NodeId {
        self.mk(NodeKind::Lam { param, ann, body }, sp)
    }
    pub fn app(&mut self, fun: NodeId, arg: NodeId, sp: Span) -> NodeId {
        self.mk(NodeKind::App { fun, arg }, sp)
    }
    pub fn let_(
        &mut self,
        name: String,
        rec_: bool,
        ann: Option<TId>,
        bound: NodeId,
        body: NodeId,
        sp: Span,
    ) -> NodeId {
        self.mk(NodeKind::Let { name, rec_, ann, bound, body }, sp)
    }
    pub fn record(&mut self, open: bool, fs: Vec<(String, NodeId)>, sp: Span) -> NodeId {
        self.mk(NodeKind::Record { open, fields: fs }, sp)
    }
    pub fn proj(&mut self, field: String, obj: NodeId, sp: Span) -> NodeId {
        self.mk(NodeKind::Proj { field, obj }, sp)
    }
    pub fn tag(&mut self, label: String, payload: Option<NodeId>, sp: Span) -> NodeId {
        self.mk(NodeKind::Tag { label, payload }, sp)
    }
    pub fn match_(&mut self, scrut: NodeId, arms: Vec<Arm>, sp: Span) -> NodeId {
        self.mk(NodeKind::Match { scrut, arms }, sp)
    }
    pub fn ann(&mut self, expr: NodeId, at: TId, sp: Span) -> NodeId {
        self.mk(NodeKind::Ann { expr, ann: at }, sp)
    }

    pub fn finish(mut self, top: Vec<NodeId>) -> Program {
        self.prog.top = top;
        self.prog
    }
}
