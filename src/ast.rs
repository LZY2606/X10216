use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Kind {
    Int { value: String },
    Bool { value: bool },
    Var { name: String, binder: Option<usize> },
    Abs { param: String },
    App,
    Let { rec: bool, name: String },
    Record,
    RecordField { name: String },
    Field { name: String },
    Variant { tag: String },
    Match,
    MatchArm { tag: String, binding: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RawNode {
    pub uid: usize,
    pub kind: Kind,
    pub span: Span,
    pub children: Vec<(String, usize)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Node {
    pub id: usize,
    pub kind: Kind,
    pub span: Span,
    pub children: Vec<(String, usize)>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RootKind {
    Let { rec: bool, name: String, value: usize, body: Option<usize> },
    Expr(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RawRoot {
    pub kind: RootKind,
    pub span: Span,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct RawProgram {
    pub nodes: HashMap<usize, RawNode>,
    pub roots: Vec<RawRoot>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Program {
    pub nodes: HashMap<usize, Node>,
    pub roots: Vec<RawRoot>,
}

impl Kind {
    pub fn discriminant(&self) -> usize {
        match self {
            Kind::Int { .. } => 0,
            Kind::Bool { .. } => 1,
            Kind::Var { .. } => 2,
            Kind::Abs { .. } => 3,
            Kind::App => 4,
            Kind::Let { .. } => 5,
            Kind::Record => 6,
            Kind::RecordField { .. } => 7,
            Kind::Field { .. } => 8,
            Kind::Variant { .. } => 9,
            Kind::Match => 10,
            Kind::MatchArm { .. } => 11,
        }
    }

    pub fn label(&self) -> Option<&str> {
        match self {
            Kind::RecordField { name } | Kind::Field { name } => Some(name),
            Kind::Variant { tag } | Kind::MatchArm { tag, .. } => Some(tag),
            _ => None,
        }
    }

    pub fn binder_name(&self) -> Option<&str> {
        match self {
            Kind::Abs { param } => Some(param),
            Kind::Let { name, .. } => Some(name),
            Kind::MatchArm { binding, .. } => Some(binding),
            _ => None,
        }
    }
}

impl RawNode {
    pub fn child(&self, role: &str) -> Option<usize> {
        self.children.iter().find(|(r, _)| r == role).map(|(_, id)| *id)
    }
}

impl Node {
    pub fn child(&self, role: &str) -> Option<usize> {
        self.children.iter().find(|(r, _)| r == role).map(|(_, id)| *id)
    }
}

pub fn root_children(root: &RawRoot) -> Vec<usize> {
    match &root.kind {
        RootKind::Let { value, body, .. } => {
            let mut out = vec![*value];
            out.extend(body);
            out
        }
        RootKind::Expr(e) => vec![*e],
    }
}

pub fn raw_node_children(program: &RawProgram, id: usize) -> Vec<(String, usize)> {
    program.nodes.get(&id).map(|n| n.children.clone()).unwrap_or_default()
}

pub fn node_children(program: &Program, id: usize) -> Vec<(String, usize)> {
    program.nodes.get(&id).map(|n| n.children.clone()).unwrap_or_default()
}

pub fn visit_raw(program: &RawProgram, mut f: impl FnMut(usize, &RawNode)) {
    fn walk(p: &RawProgram, id: usize, seen: &mut HashMap<usize, ()>, f: &mut impl FnMut(usize, &RawNode)) {
        if seen.contains_key(&id) {
            return;
        }
        seen.insert(id, ());
        if let Some(node) = p.nodes.get(&id) {
            for (_, child) in &node.children {
                walk(p, *child, seen, f);
            }
            f(id, node);
        }
    }
    let mut seen = HashMap::new();
    for root in &program.roots {
        for child in root_children(root) {
            walk(program, child, &mut seen, &mut f);
        }
    }
}

pub fn visit(program: &Program, mut f: impl FnMut(usize, &Node)) {
    fn walk(p: &Program, id: usize, seen: &mut HashMap<usize, ()>, f: &mut impl FnMut(usize, &Node)) {
        if seen.contains_key(&id) {
            return;
        }
        seen.insert(id, ());
        if let Some(node) = p.nodes.get(&id) {
            for (_, child) in &node.children {
                walk(p, *child, seen, f);
            }
            f(id, node);
        }
    }
    let mut seen = HashMap::new();
    for root in &program.roots {
        for child in root_children(root) {
            walk(program, child, &mut seen, &mut f);
        }
    }
}
