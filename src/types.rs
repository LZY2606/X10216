use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

pub type Vid = String;
pub type Proof = BTreeSet<String>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum Ty {
    Unit { proof: Proof },
    Int { proof: Proof },
    Bool { proof: Proof },
    Var { vid: Vid, proof: Proof },
    Fun { from: Box<Ty>, to: Box<Ty>, proof: Proof },
    Record { row: Row, proof: Proof },
    Variant { row: Row, proof: Proof },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Row {
    pub fields: BTreeMap<String, Ty>,
    pub extension: Option<Vid>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Scheme {
    pub id: String,
    pub quantifiers: Vec<Vid>,
    pub body: Ty,
    pub monomorphic: bool,
    pub value_restricted: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Constraint {
    pub id: String,
    pub origin: String,
    pub span: crate::syntax::Span,
    pub left: Ty,
    pub right: Ty,
    pub role: String,
    pub cache_key: String,
    pub scope_key: String,
    pub genv_key: String,
}

impl Ty {
    pub fn proof(&self) -> &Proof {
        match self {
            Ty::Unit { proof } | Ty::Int { proof } | Ty::Bool { proof } | Ty::Var { proof, .. }
            | Ty::Fun { proof, .. } | Ty::Record { proof, .. } | Ty::Variant { proof, .. } => proof,
        }
    }
    pub fn proof_mut(&mut self) -> &mut Proof {
        match self {
            Ty::Unit { proof } | Ty::Int { proof } | Ty::Bool { proof } | Ty::Var { proof, .. }
            | Ty::Fun { proof, .. } | Ty::Record { proof, .. } | Ty::Variant { proof, .. } => proof,
        }
    }
    pub fn unit() -> Ty { Ty::Unit { proof: Proof::new() } }
    pub fn int() -> Ty { Ty::Int { proof: Proof::new() } }
    pub fn bool() -> Ty { Ty::Bool { proof: Proof::new() } }
    pub fn var(vid: impl Into<String>) -> Ty { Ty::Var { vid: vid.into(), proof: Proof::new() } }
    pub fn fun(from: Ty, to: Ty) -> Ty {
        let mut proof = from.proof().clone();
        proof.extend(to.proof().iter().cloned());
        Ty::Fun { from: Box::new(from), to: Box::new(to), proof }
    }
    pub fn with(mut self, p: impl Into<String>) -> Ty {
        self.proof_mut().insert(p.into());
        self
    }
    pub fn add_proofs(&mut self, ps: &Proof) {
        self.proof_mut().extend(ps.iter().cloned());
        match self {
            Ty::Fun { from, to, .. } => { from.add_proofs(ps); to.add_proofs(ps); }
            Ty::Record { row, .. } | Ty::Variant { row, .. } => for t in row.fields.values_mut() { t.add_proofs(ps); },
            _ => {}
        }
    }
}

impl Row {
    pub fn closed(fields: BTreeMap<String, Ty>) -> Row { Row { fields, extension: None } }
    pub fn open(fields: BTreeMap<String, Ty>, extension: Vid) -> Row { Row { fields, extension: Some(extension) } }
    pub fn empty_closed() -> Row { Row { fields: BTreeMap::new(), extension: None } }
}
