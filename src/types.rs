use crate::syntax::Span;
use serde::{Deserialize, Serialize};

pub type VarId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Trace {
    pub node_id: usize,
    pub span: Span,
    pub role: TraceRole,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceRole {
    Expr,
    Function,
    Argument,
    Result,
    Param,
    FieldValue,
    Payload,
    Scrutinee,
    MatchResult,
    Variant,
    Record,
    Instantiation,
    Generalization,
    Unification,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum RowTail {
    Closed,
    Open(VarId),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Ty {
    Var(VarId),
    Int,
    String,
    Fun(Box<Ty>, Box<Ty>),
    Record(Vec<RowField>, RowTail),
    Variant(Vec<RowField>, RowTail),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RowField {
    pub label: String,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Scheme {
    pub quantified: Vec<VarId>,
    pub body: Ty,
}

impl Ty {
    pub fn fun(from: Ty, to: Ty) -> Ty { Ty::Fun(Box::new(from), Box::new(to)) }
    pub fn record(fields: Vec<(&str, Ty)>) -> Ty {
        Ty::Record(fields.into_iter().map(|(l,t)| RowField { label:l.into(), ty:t }).collect(), RowTail::Closed)
    }
    pub fn variant(fields: Vec<(&str, Ty)>) -> Ty {
        Ty::Variant(fields.into_iter().map(|(l,t)| RowField { label:l.into(), ty:t }).collect(), RowTail::Closed)
    }
    pub fn vars(&self) -> Vec<VarId> {
        let mut out = Vec::new();
        collect_vars(self, &mut out);
        out.sort_unstable();
        out.dedup();
        out
    }
}

fn collect_vars(ty: &Ty, out: &mut Vec<VarId>) {
    match ty {
        Ty::Var(v) => out.push(*v),
        Ty::Int | Ty::String => {}
        Ty::Fun(a,b) => { collect_vars(a,out); collect_vars(b,out); }
        Ty::Record(fs,tail) | Ty::Variant(fs,tail) => {
            for f in fs { collect_vars(&f.ty,out); }
            if let RowTail::Open(v) = tail { collect_vars(&Ty::Var(*v),out); }
        }
    }
}

#[derive(Default)]
pub struct VarGen { next: VarId, traces: std::collections::BTreeMap<VarId, Trace> }
impl VarGen {
    pub fn new() -> Self { Self { next: 0 } }
    pub fn fresh(&mut self, node_id: usize, span: Span, role: TraceRole) -> Ty {
        let v = self.next;
        self.next += 1;
        self.traces.insert(v, Trace { node_id, span, role });
        Ty::Var(v)
    }
    pub fn next_id(&self) -> VarId { self.next }
    pub fn trace_for(&self, v: VarId) -> Option<Trace> { self.traces.get(&v).copied() }
    pub fn traces(&self) -> &std::collections::BTreeMap<VarId, Trace> { &self.traces }
}
