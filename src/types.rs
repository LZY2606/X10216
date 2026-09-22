// Type representation, serialized snapshots, schemes and rendering.

use serde::{Deserialize, Serialize};

pub type VarId = u32;
pub type CIdRaw = u32;
pub type NodeRaw = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FlexKind {
    Type,
    Row,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RowFieldF {
    pub label: String,
    pub ty: Box<TermF>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TailF {
    Closed,
    Open(u32),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TermKind {
    Flex { kind: FlexKind, hint: String },
    Rigid(String),
    Con(String),
    Arrow(Box<TermF>, Box<TermF>),
    Record(Vec<RowFieldF>, TailF),
    Variant(Vec<RowFieldF>, TailF),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TermF {
    pub root: u32,
    pub kind: TermKind,
    pub name: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum RowSort {
    Record,
    Variant,
}

impl RowSort {
    pub fn word(&self) -> &'static str {
        match self {
            RowSort::Record => "record",
            RowSort::Variant => "variant",
        }
    }
}

pub fn render(term: &TermF) -> String {
    match &term.kind {
        TermKind::Flex { hint, .. } => {
            if hint.is_empty() {
                format!("?{}", term.name)
            } else {
                format!("'{}", hint)
            }
        }
        TermKind::Rigid(n) => format!("'{}", n),
        TermKind::Con(n) => n.clone(),
        TermKind::Arrow(a, b) => {
            let l = match &a.kind {
                TermKind::Arrow(..) => format!("({})", render(a)),
                _ => render(a),
            };
            format!("{} -> {}", l, render(b))
        }
        TermKind::Record(fs, tail) => {
            let mut s = String::from("{");
            for (i, f) in fs.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&f.label);
                s.push_str(": ");
                s.push_str(&render(&f.ty));
            }
            match tail {
                TailF::Closed => {}
                TailF::Open(v) => {
                    if !fs.is_empty() {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("..r{}", v));
                }
            }
            s.push('}');
            s
        }
        TermKind::Variant(fs, tail) => {
            let mut s = String::from("[");
            for (i, f) in fs.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }
                s.push_str(&f.label);
                if !matches!(f.ty.kind, TermKind::Con(ref u) if u == "Unit") {
                    s.push_str(": ");
                    s.push_str(&render(&f.ty));
                }
            }
            match tail {
                TailF::Closed => {}
                TailF::Open(v) => {
                    if !fs.is_empty() {
                        s.push_str(", ");
                    }
                    s.push_str(&format!("..r{}", v));
                }
            }
            s.push(']');
            s
        }
    }
}
