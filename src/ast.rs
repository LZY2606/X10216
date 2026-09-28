use crate::span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PathStep {
    Root,
    LetDef,
    LetBody,
    LetRecDef,
    FunParam,
    FunBody,
    AppFn,
    AppArg(u32),
    RecordField(u32),
    FieldAccess,
    VariantPayload,
    Annot,
    MatchScrutinee,
    MatchArmPat(u32),
    MatchArmBody(u32),
    PatPayload,
    Paren,
    Decl(u32),
}

impl PathStep {
    pub fn tag(&self) -> String {
        match self {
            PathStep::Root => "root".into(),
            PathStep::LetDef => "let.def".into(),
            PathStep::LetBody => "let.body".into(),
            PathStep::LetRecDef => "letrec.def".into(),
            PathStep::FunParam => "fun.param".into(),
            PathStep::FunBody => "fun.body".into(),
            PathStep::AppFn => "app.fn".into(),
            PathStep::AppArg(k) => format!("app.arg{}", k),
            PathStep::RecordField(k) => format!("rec.f{}", k),
            PathStep::FieldAccess => "field.recv".into(),
            PathStep::VariantPayload => "variant.payload".into(),
            PathStep::Annot => "annot".into(),
            PathStep::MatchScrutinee => "match.scrut".into(),
            PathStep::MatchArmPat(k) => format!("arm{}.pat", k),
            PathStep::MatchArmBody(k) => format!("arm{}.body", k),
            PathStep::PatPayload => "pat.payload".into(),
            PathStep::Paren => "paren".into(),
            PathStep::Decl(k) => format!("decl{}", k),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodePath(pub Vec<PathStep>);

impl NodePath {
    pub fn push(&self, step: PathStep) -> NodePath {
        let mut p = self.0.clone();
        p.push(step);
        NodePath(p)
    }
    pub fn key(&self) -> String {
        self.0.iter().map(|s| s.tag()).collect::<Vec<_>>().join("/")
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Fun { param: String, body: NodeId },
    App { fun: NodeId, args: Vec<NodeId> },
    Let { rec: bool, name: String, def: NodeId, body: NodeId },
    Record { fields: Vec<(String, NodeId)> },
    Field { recv: NodeId, field: String },
    Variant { tag: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<(Pat, NodeId)> },
    Annot { expr: NodeId, ty: TypeAst },
}

#[derive(Clone, Debug)]
pub enum Pat {
    Wild,
    Var(String),
    Tag { tag: String, payload: Option<Box<Pat>> },
}

#[derive(Clone, Debug)]
pub enum TypeAst {
    TName(String),
    TFun(Box<TypeAst>, Box<TypeAst>),
    TRecord(Vec<(String, TypeAst)>, bool), // bool => closed
    TVariant(Vec<(String, Option<TypeAst>)>, bool),
    TRowVar(String),
}

#[derive(Clone, Debug)]
pub struct Node {
    pub id: NodeId,
    pub path: NodePath,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub nodes: Vec<Node>,
    pub root: NodeId,
}

impl Program {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0 as usize]
    }
}
