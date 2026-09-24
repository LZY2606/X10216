/// 源码位置（字节偏移，半开区间）。
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub const ZERO: Span = Span { start: 0, end: 0 };
    pub fn union(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// 稳定解析节点身份：位置 + 语法角色。变量改名不影响 id。
pub type NodeId = u64;

#[derive(Clone, PartialEq, Debug)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
    pub nid: NodeId,
}

pub type SExpr = Spanned<Expr>;
pub type SPat = Spanned<Pat>;

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Unit,
    /// 标识符引用（局部变量或绑定名）。
    Ident(String),
    /// 二元内建运算：+, -, *, =, <。
    Binop {
        op: char,
        lhs: Box<SExpr>,
        rhs: Box<SExpr>,
    },
    /// if e then e else e
    If {
        cond: Box<SExpr>,
        then_e: Box<SExpr>,
        else_e: Box<SExpr>,
    },
    Fn {
        param: String,
        body: Box<SExpr>,
    },
    App {
        fun: Box<SExpr>,
        arg: Box<SExpr>,
    },
    /// 非递归 let；`rec` 字段为 true 时为递归（支持多名字互相引用）。
    Let {
        rec: bool,
        name: String,
        bound: Box<SExpr>,
        body: Box<SExpr>,
    },
    /// 闭记录字面量 { f = e; g = e }，也包括空记录 { }。
    Record { fields: Vec<(String, SExpr)> },
    /// 行扩展 { f = e | rest }
    Extend {
        field: String,
        value: Box<SExpr>,
        rest: Box<SExpr>,
    },
 /// 字段投影 e.field
    Project {
        record: Box<SExpr>,
        field: String,
    },
    /// 变体构造 `Tag payload`（应用形式）。
    Variant { tag: String, payload: Box<SExpr> },
    /// 单行 match：match e with Tag p -> b; _ -> d
    Match {
        scrut: Box<SExpr>,
        clause_tag: String,
        clause_pat: SPat,
        clause_body: Box<SExpr>,
        default: Box<SExpr>,
    },
    /// 括号分组；保留为独立节点但不影响类型。
    Paren(Box<SExpr>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Pat {
    Wild,
    Binding(String),
    Unit,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Item {
    pub name: String,
    pub rec: bool,
    pub expr: SExpr,
}

#[derive(Clone, PartialEq, Debug, Default)]
pub struct Program {
    pub items: Vec<Item>,
}

impl Expr {
    /// 是否为非膨胀值（value restriction 放宽条件）。
    pub fn is_value(&self) -> bool {
        match self {
            Expr::Int(_) | Expr::Bool(_) | Expr::Unit => true,
            Expr::Fn { .. } => true,
            Expr::Record { fields } => fields.iter().all(|(_, e)| e.node.is_value()),
            Expr::Paren(e) => e.node.is_value(),
            _ => false,
        }
    }

    pub fn describe(&self) -> &'static str {
        match self {
            Expr::Int(_) => "int literal",
            Expr::Bool(_) => "bool literal",
            Expr::Unit => "unit literal",
            Expr::Ident(_) => "identifier",
            Expr::Binop { .. } => "binary operator",
            Expr::If { .. } => "if expression",
            Expr::Fn { .. } => "function",
            Expr::App { .. } => "application",
            Expr::Let { rec, .. } => {
                if *rec {
                    "recursive let"
                } else {
                    "let binding"
                }
            }
            Expr::Record { .. } => "record literal",
            Expr::Extend { .. } => "row extension",
            Expr::Project { .. } => "field projection",
            Expr::Variant { .. } => "variant construction",
            Expr::Match { .. } => "match",
            Expr::Paren(_) => "parentheses",
        }
    }
}

impl SExpr {
    pub fn new(node: Expr, span: Span, nid: NodeId) -> Self {
        Spanned { node, span, nid }
    }
}

impl SPat {
    pub fn new(node: Pat, span: Span, nid: NodeId) -> Self {
        Spanned { node, span, nid }
    }
}
