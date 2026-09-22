//! Core type representation: Hindley-Milner types plus record/variant rows.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::ast::NodeId;

/// A type-variable identifier, allocated per solving run but rendered with
/// stable, deterministic names.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub struct TV(pub u32);

/// Provenance point: something in the program or the constraint trace that
/// contributed to a type variable.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct OrigPoint {
    pub node: Option<NodeId>,
    pub constraint: Option<u64>,
    pub label: String,
}

impl OrigPoint {
    pub fn node(id: NodeId, label: impl Into<String>) -> Self {
        OrigPoint { node: Some(id), constraint: None, label: label.into() }
    }
    pub fn constraint(cid: u64, label: impl Into<String>) -> Self {
        OrigPoint { node: None, constraint: Some(cid), label: label.into() }
    }
}

/// A row: an ordered collection of labelled fields, optionally open through a
/// tail variable.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub struct Row {
    pub fields: Vec<(String, Type)>,
    /// `None` means the row is closed (no more fields possible).
    pub tail: Option<TV>,
}

impl Row {
    pub fn empty_closed() -> Self {
        Row { fields: Vec::new(), tail: None }
    }
    pub fn empty_open(tv: TV) -> Self {
        Row { fields: Vec::new(), tail: Some(tv) }
    }
    pub fn find(&self, label: &str) -> Option<&Type> {
        self.fields.iter().find(|(l, _)| l == label).map(|(_, t)| t)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub enum Type {
    Var(TV),
    Con(String, Vec<Type>),
    Arrow(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
}

impl Type {
    pub fn con(name: &str) -> Type {
        Type::Con(name.to_string(), Vec::new())
    }
    pub fn arrow(from: Type, to: Type) -> Type {
        Type::Arrow(Box::new(from), Box::new(to))
    }
    pub fn record(fields: Vec<(&str, Type)>, tail: Option<TV>) -> Type {
        Type::Record(Row {
            fields: fields.into_iter().map(|(l, t)| (l.to_string(), t)).collect(),
            tail,
        })
    }
    pub fn variant(fields: Vec<(&str, Type)>, tail: Option<TV>) -> Type {
        Type::Variant(Row {
            fields: fields.into_iter().map(|(l, t)| (l.to_string(), t)).collect(),
            tail,
        })
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Type::Var(_))
    }

    /// All type variables occurring in this type, in deterministic traversal
    /// order (deduplicated).
    pub fn vars(&self) -> Vec<TV> {
        let mut out = Vec::new();
        let mut push = |v: TV| {
            if !out.contains(&v) {
                out.push(v);
            }
        };
        fn row_vars(r: &Row, push: &mut impl FnMut(TV)) {
            for (_, t) in &r.fields {
                t.walk(push);
            }
            if let Some(v) = r.tail {
                push(v);
            }
        }
        match self {
            Type::Var(v) => push(*v),
            Type::Con(_, ts) => {
                for t in ts {
                    t.walk(&mut push);
                }
            }
            Type::Arrow(a, b) => {
                a.walk(&mut push);
                b.walk(&mut push);
            }
            Type::Record(r) | Type::Variant(r) => row_vars(r, &mut push),
        }
        out
    }

    fn walk(&self, push: &mut impl FnMut(TV)) {
        match self {
            Type::Var(v) => push(*v),
            Type::Con(_, ts) => ts.iter().for_each(|t| t.walk(push)),
            Type::Arrow(a, b) => {
                a.walk(push);
                b.walk(push);
            }
            Type::Record(r) | Type::Variant(r) => {
                for (_, t) in &r.fields {
                    t.walk(push);
                }
                if let Some(v) = r.tail {
                    push(v);
                }
            }
        }
    }

    /// Render with a mapping for variable names.
    pub fn render(&self, names: &HashMap<TV, String>) -> String {
        let mut s = String::new();
        self.write(&mut s, names, false);
        s
    }

    fn write(&self, out: &mut String, names: &HashMap<TV, String>, lhs: bool) {
        match self {
            Type::Var(v) => out.push_str(names.get(v).map_or("?", |n| n.as_str())),
            Type::Con(n, _) => out.push_str(n),
            Type::Arrow(a, b) => {
                if lhs {
                    out.push('(');
                }
                a.write(out, names, true);
                out.push_str(" -> ");
                b.write(out, names, false);
                if lhs {
                    out.push(')');
                }
            }
            Type::Record(r) => write_row(out, '{', '}', r, names),
            Type::Variant(r) => write_row(out, '<', '>', r, names),
        }
    }
}

fn write_row(out: &mut String, open: char, close: char, r: &Row, names: &HashMap<TV, String>) {
    out.push(open);
    for (i, (l, t)) in r.fields.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(l);
        out.push_str(": ");
        t.write(out, names, false);
    }
    if let Some(v) = r.tail {
        if !r.fields.is_empty() {
            out.push_str(" | ");
        }
        out.push_str(names.get(&v).map_or("?", |n| n.as_str()));
    }
    out.push(close);
}

/// A polytype: quantified type with its bound variables.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Debug)]
pub struct Scheme {
    pub bounds: Vec<TV>,
    pub body: Type,
}
