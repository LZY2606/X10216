use crate::syntax::Span;
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct VarId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FlexKind { Type, Row }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub struct FlexId {
    pub kind: FlexKind,
    pub id: VarId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Provenance {
    pub expression: Option<usize>,
    pub spans: Vec<Span>,
    pub constraints: Vec<String>,
    pub reason: String,
}

impl Provenance {
    pub fn expr(id: usize, span: Span, reason: impl Into<String>) -> Self {
        Self { expression: Some(id), spans: vec![span], constraints: Vec::new(), reason: reason.into() }
    }

    pub fn builtin(reason: impl Into<String>) -> Self {
        Self { expression: None, spans: Vec::new(), constraints: Vec::new(), reason: reason.into() }
    }

    pub fn with_constraint(mut self, id: impl Into<String>) -> Self {
        self.constraints.push(id.into());
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Row {
    Fields { entries: BTreeMap<String, (Type, Provenance)>, tail: Option<(VarId, Provenance)> },
    Flex(VarId, Provenance),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[derive(Serialize)]
pub enum TypeKind {
    Var(VarId),
    Prim(String),
    Arrow(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Type {
    pub kind: TypeKind,
    pub prov: Provenance,
}

impl Type {
    pub fn new(kind: TypeKind, prov: Provenance) -> Self { Self { kind, prov } }
    pub fn var(id: VarId, prov: Provenance) -> Self { Self::new(TypeKind::Var(id), prov) }
    pub fn prim(name: &str, prov: Provenance) -> Self { Self::new(TypeKind::Prim(name.into()), prov) }
    pub fn arrow(left: Type, right: Type, prov: Provenance) -> Self {
        Self::new(TypeKind::Arrow(Box::new(left), Box::new(right)), prov)
    }
    pub fn record(row: Row, prov: Provenance) -> Self { Self::new(TypeKind::Record(row), prov) }
    pub fn variant(row: Row, prov: Provenance) -> Self { Self::new(TypeKind::Variant(row), prov) }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Scheme {
    pub quantifiers: Vec<FlexId>,
    pub typ: Type,
}

pub fn row_fields(row: &Row) -> Option<&BTreeMap<String, (Type, Provenance)>> {
    match row { Row::Fields { entries, .. } => Some(entries), Row::Flex(..) => None }
}

pub fn row_field_names(row: &Row) -> Vec<String> {
    match row {
        Row::Fields { entries, .. } => entries.keys().cloned().collect(),
        Row::Flex(..) => Vec::new(),
    }
}

pub fn row_tail(row: &Row) -> Option<(VarId, Provenance)> {
    match row {
        Row::Fields { tail, .. } => tail.clone(),
        Row::Flex(..) => None,
    }
}
