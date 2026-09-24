use crate::syntax::Span;
use crate::types::Ty;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "kind")]
pub enum EventKind {
    Assert { left: Ty, right: Ty, reason: AssertReason },
    Instantiate { name: String, binder: String, target: Ty, polymorphic: bool },
    BeginLet { rec: bool, name: String, rhs_ty: Ty, value_restriction: bool },
    Generalize { name: String, ty: Ty, value_form: bool },
    EndLet { name: String },
    BeginArm { bindings: Vec<ArmBinding> },
    EndArm { names: Vec<String> },
    Note { text: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AssertReason {
    Lit,
    Var,
    ParamBody,
    AppFun,
    AppArg,
    AppResult,
    LetRhs,
    FieldType,
    RecordValue,
    ExtendBase,
    ExtendValue,
    VariantPayload,
    MatchScrutinee,
    MatchTagPayload,
    MatchResult,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArmBinding {
    pub name: String,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Event {
    pub id: String,
    pub kind: EventKind,
    pub span: Span,
    pub node_id: String,
    pub constraint_id: Option<String>,
    pub disabled: bool,
}

impl Event {
    pub fn is_constraint(&self) -> bool {
        matches!(self.kind, EventKind::Assert { .. } | EventKind::Instantiate { .. })
    }
    pub fn fingerprint_parts(&self) -> Vec<String> {
        match &self.kind {
            EventKind::Assert { left, right, reason } => vec!["assert".into(), format!("{reason:?}"), format!("{left:?}"), format!("{right:?}")],
            EventKind::Instantiate { name, binder, target, polymorphic } => vec!["inst".into(), name.clone(), binder.clone(), format!("{target:?}"), polymorphic.to_string()],
            EventKind::BeginLet { rec, name, rhs_ty, value_restriction } => vec!["begin".into(), rec.to_string(), name.clone(), format!("{rhs_ty:?}"), value_restriction.to_string()],
            EventKind::Generalize { name, ty, value_form } => vec!["gen".into(), name.clone(), format!("{ty:?}"), value_form.to_string()],
            EventKind::EndLet { name } => vec!["end".into(), name.clone()],
            EventKind::BeginArm { bindings } => bindings.iter().flat_map(|b| [b.name.clone(), format!("{:?}", b.ty)]).collect(),
            EventKind::EndArm { names } => { let mut v = vec!["endarm".into()]; v.extend(names.clone()); v }
            EventKind::Note { text } => vec!["note".into(), text.clone()],
        }
    }
}

pub fn is_value_form(e: &crate::syntax::Expr) -> bool {
    use crate::syntax::Expr::*;
    matches!(e, Lit(_) | Var(_) | Lam(_) | Tag(_) | Record(_))
}
