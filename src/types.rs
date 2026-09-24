// Type representation with row polymorphism.

use crate::ast::{fnv1a, NodeId};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct MetaVar(pub u32);

/// Origin of a fresh metavariable. Deterministic and structural, so two
/// identical program pieces produce identical constraint shape.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct MetaOrigin {
    pub node: NodeId,
    pub tag: OriginTag,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum OriginTag {
    Arg,
    Ret,
    Inst,
    FieldOut,
    RowTail,
    MatchRow,
    Payload,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Ty {
    Meta(MetaVar),
    Con(String, Vec<Ty>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct RowField {
    pub label: String,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Row {
    pub fields: Vec<RowField>,
    /// None means a closed row.
    pub tail: Option<MetaVar>,
}

impl Row {
    pub fn empty_closed() -> Self {
        Row { fields: Vec::new(), tail: None }
    }
    pub fn empty_open(tail: MetaVar) -> Self {
        Row { fields: Vec::new(), tail: Some(tail) }
    }
    pub fn find(&self, label: &str) -> Option<usize> {
        self.fields.iter().position(|f| f.label == label)
    }
}

impl Ty {
    pub fn unit() -> Ty {
        Ty::Con("Unit".to_string(), Vec::new())
    }
    pub fn int() -> Ty {
        Ty::Con("Int".to_string(), Vec::new())
    }
    pub fn str() -> Ty {
        Ty::Con("String".to_string(), Vec::new())
    }
    pub fn arrow(arg: Ty, ret: Ty) -> Ty {
        Ty::Con("->".to_string(), vec![arg, ret])
    }

    pub fn metas(&self, out: &mut Vec<MetaVar>) {
        match self {
            Ty::Meta(m) => {
                if !out.contains(m) {
                    out.push(*m);
                }
            }
            Ty::Con(_, ts) => ts.iter().for_each(|t| t.metas(out)),
            Ty::Record(r) | Ty::Variant(r) => {
                for f in &r.fields {
                    f.ty.metas(out);
                }
                if let Some(t) = r.tail {
                    if !out.contains(&t) {
                        out.push(t);
                    }
                }
            }
        }
    }

    /// Structural fingerprint ignoring concrete metavariable numbers; instead
    /// origins are reconstructed through the solver's origin table.
    pub fn shape(&self, origins: &[(MetaVar, MetaOrigin)], out: &mut String) {
        match self {
            Ty::Meta(m) => match origins.iter().find(|(mv, _)| mv == m) {
                Some((_, o)) => {
                    out.push('?');
                    out.push_str(&o.node.hex());
                    out.push(':');
                    out.push_str(origin_tag_name(o.tag));
                }
                None => out.push_str("?pruned"),
            },
            Ty::Con(name, ts) => {
                out.push_str(name);
                if !ts.is_empty() {
                    out.push('(');
                    for (i, t) in ts.iter().enumerate() {
                        if i > 0 {
                            out.push(',');
                        }
                        t.shape(origins, out);
                    }
                    out.push(')');
                }
            }
            Ty::Record(r) => {
                out.push('{');
                row_shape(r, origins, out);
                out.push('}');
            }
            Ty::Variant(r) => {
                out.push('[');
                row_shape(r, origins, out);
                out.push(']');
            }
        }
    }
}

fn row_shape(row: &Row, origins: &[(MetaVar, MetaOrigin)], out: &mut String) {
    for (i, f) in row.fields.iter().enumerate() {
        if i > 0 {
            out.push(',');
        }
        out.push_str(&f.label);
        out.push(':');
        f.ty.shape(origins, out);
    }
    if let Some(tail) = row.tail {
        if !row.fields.is_empty() {
            out.push(',');
        }
        out.push_str("..?");
        if let Some((_, o)) = origins.iter().find(|(mv, _)| mv == &tail) {
            out.push_str(&o.node.hex());
        } else {
            out.push_str("tail");
        }
    }
}

pub fn origin_tag_name(tag: OriginTag) -> &'static str {
    match tag {
        OriginTag::Arg => "arg",
        OriginTag::Ret => "ret",
        OriginTag::Inst => "inst",
        OriginTag::FieldOut => "fieldout",
        OriginTag::RowTail => "rowtail",
        OriginTag::MatchRow => "matchrow",
        OriginTag::Payload => "payload",
    }
}

pub fn origin_key(origin: MetaOrigin, index: u32) -> u32 {
    let tag_byte = match origin.tag {
        OriginTag::Arg => 1,
        OriginTag::Ret => 2,
        OriginTag::Inst => 3,
        OriginTag::FieldOut => 4,
        OriginTag::RowTail => 5,
        OriginTag::MatchRow => 6,
        OriginTag::Payload => 7,
    };
    let mut h = fnv1a(origin.node.0, &[tag_byte, b'[']);
    h = fnv1a(h, &index.to_le_bytes());
    (h % 1_000_000_007) as u32
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Scheme {
    pub quantified: Vec<MetaVar>,
    pub body: Ty,
    pub origin: NodeId,
}

impl Scheme {
    pub fn monomorphic(body: Ty, origin: NodeId) -> Self {
        Scheme { quantified: Vec::new(), body, origin }
    }
}
