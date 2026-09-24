use crate::span::Span;

/// Local parse-order id assigned during parsing; replaced by structural
/// identities later, but useful for bookkeeping during name resolution.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NId(pub u32);

#[derive(Clone, Debug, PartialEq)]
pub struct Node<T> {
    pub nid: NId,
    pub span: Span,
    pub node: T,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Program {
    Program(Vec<Node<Item>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Let {
        rec: bool,
        pat: Node<Pat>,
        body: Node<Expr>,
    },
    Expr(Node<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pat {
    Var(String),
    Wild,
    Lit(Lit),
    Record(Vec<(String, Node<Pat>)>),
    Variant(String, Option<Box<Node<Pat>>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lit {
    Int(i64),
    Str(String),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Lit(Lit),
    /// Resolved reference; ordinal distinguishes repeated uses of the same binder.
    VarRef(String, u32),
    Lam {
        param: Node<Pat>,
        body: Box<Node<Expr>>,
    },
    App {
        fun: Box<Node<Expr>>,
        arg: Box<Node<Expr>>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<Node<Expr>>,
        rhs: Box<Node<Expr>>,
    },
    Let {
        rec: bool,
        pat: Node<Pat>,
        value: Box<Node<Expr>>,
        body: Box<Node<Expr>>,
    },
    Record(Vec<(String, Node<Expr>)>),
    Field(Box<Node<Expr>>, String),
    Variant(String, Option<Box<Node<Expr>>>),
    Match {
        scrut: Box<Node<Expr>>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pat: Node<Pat>,
    pub body: Node<Expr>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
}
