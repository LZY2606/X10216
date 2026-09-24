//! 源码范围、AST 节点与带稳定身份的解析树。

use serde::Serialize;

/// 半开字节区间 [lo, hi)，指向源码 UTF-8 字节偏移。
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Default)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub const fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
    pub fn join(self, other: Span) -> Span {
        Span {
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.lo, self.hi)
    }
}

/// 解析局部的节点编号；一次解析内唯一，跨编辑由 `stable` 路径承担身份。
pub type NodeId = usize;

/// 平面化的解析节点，供前端绘制 AST。
#[derive(Clone, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub stable: String,
    pub span: Span,
    pub kind: &'static str,
    pub label: Option<String>,
    pub children: Vec<NodeId>,
}

/// 解析器在构造树时累积节点。
#[derive(Default)]
pub struct NodeStore {
    pub nodes: Vec<Node>,
}

impl NodeStore {
    pub fn new() -> Self {
        Self::default()
    }

    /// 登记一个节点。`stable` 是基于结构位置（而非名字）的路径，
    /// 因此变量改名不会改变同一结构位置节点的身份。
    pub fn push(
        &mut self,
        parent: Option<NodeId>,
        span: Span,
        kind: &'static str,
        label: Option<String>,
        stable: String,
    ) -> NodeId {
        let id = self.nodes.len();
        self.nodes.push(Node {
            id,
            stable,
            span,
            kind,
            label,
            children: Vec::new(),
        });
        if let Some(p) = parent {
            self.nodes[p].children.push(id);
        }
        id
    }
}

/// 模式：目前仅支持带可选类型标注的标识符。
#[derive(Clone, Serialize)]
pub struct Pat {
    pub nid: NodeId,
    pub span: Span,
    pub name: String,
    pub annot: Option<TyExpr>,
}

/// 单个 let 绑定。
#[derive(Clone, Serialize)]
pub struct Bind {
    pub nid: NodeId,
    pub name_span: Span,
    pub name: String,
    pub annot: Option<TyExpr>,
    pub val: Expr,
}

#[derive(Clone, Serialize)]
pub struct RecordField {
    pub nid: NodeId,
    pub label_span: Span,
    pub label: String,
    pub val: Expr,
}

#[derive(Clone, Serialize)]
pub struct VariantAlt {
    pub nid: NodeId,
    pub tag_span: Span,
    pub tag: String,
    pub pat: Option<Pat>,
    pub body: Expr,
}

#[derive(Clone, Serialize)]
pub enum ExprKind {
    Int(i64),
    Str(String),
    Bool(bool),
    Unit,
    Var(String),
    Lam {
        param: Pat,
        body: Box<Expr>,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    Let {
        rec: bool,
        binds: Vec<Bind>,
        body: Option<Box<Expr>>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
    },
    Record {
        fields: Vec<RecordField>,
        /// `..rest` 形式的行扩展来源。
        tail: Option<Box<Expr>>,
    },
    Field {
        record: Box<Expr>,
        label: String,
        label_span: Span,
    },
    Variant {
        tag: String,
        payload: Option<Box<Expr>>,
    },
    Match {
        scrut: Box<Expr>,
        alts: Vec<VariantAlt>,
    },
}

#[derive(Clone, Serialize)]
pub struct Expr {
    pub nid: NodeId,
    pub span: Span,
    pub stable: String,
    pub kind: ExprKind,
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize)]
pub enum RowTailFlavor {
    Closed,
    /// 匿名开放行：`{ .. }`。
    Open,
    /// 具名行变量：`{ ..r }`。
    Named,
}

#[derive(Clone, Serialize)]
pub enum TyKind {
    Con(String),
    Var(String),
    Arrow(Box<TyExpr>, Box<TyExpr>),
    Record(Vec<(String, TyExpr)>, RowTailFlavor, Option<String>),
    Variant(Vec<(String, TyExpr)>, RowTailFlavor, Option<String>),
    /// 括号仅用于解析，约束生成时会穿透。
    Paren(Box<TyExpr>),
}

#[derive(Clone, Serialize)]
pub struct TyExpr {
    pub nid: NodeId,
    pub span: Span,
    pub kind: TyKind,
}

pub enum Item {
    /// 顶层 let 组（rec 时互为作用域）。
    Let {
        nid: NodeId,
        rec: bool,
        binds: Vec<Bind>,
    },
    /// 顶层待求值表达式。
    Eval(Expr),
}

pub struct Program {
    pub source: String,
    pub nodes: Vec<Node>,
    pub items: Vec<Item>,
}
