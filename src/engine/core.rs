use crate::ast::NodeId;
use crate::lexer::Span;
use crate::types::{Param, RVar, Row, RowKind, RowTail, Scheme, Type, TypeFactory, TVar};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug)]
pub struct EqualityConstraint {
    pub id: String,
    pub numeric_id: u64,
    pub cache_key: String,
    pub fingerprint: String,
    pub left: Type,
    pub right: Type,
    pub title: String,
    pub detail: String,
    pub source_nodes: BTreeSet<NodeId>,
    pub disabled: bool,
}

#[derive(Clone, Debug)]
pub struct InstantiationStep {
    pub id: String,
    pub name: String,
    pub source_node: NodeId,
    pub scheme: Scheme,
    pub typ: Type,
}

#[derive(Clone, Debug)]
pub struct GeneralizationStep {
    pub id: String,
    pub name: String,
    pub source_node: NodeId,
    pub typ: Type,
    pub scheme: Scheme,
    pub reason: String,
}

#[derive(Clone, Debug)]
pub enum Action {
    Instantiate(InstantiationStep),
    Generalize(GeneralizationStep),
    Equal(EqualityConstraint),
    Unresolved { id: String, node: NodeId, name: String },
}

impl Action {
    pub fn id(&self) -> &str {
        match self {
            Action::Instantiate(step) => &step.id,
            Action::Generalize(step) => &step.id,
            Action::Equal(constraint) => &constraint.id,
            Action::Unresolved { id, .. } => id,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct SolveResult {
    pub errors: Vec<SolveError>,
    pub successful: BTreeSet<String>,
    pub skipped: BTreeSet<String>,
    pub disabled: BTreeSet<String>,
}

#[derive(Clone, Debug)]
pub struct SolveError {
    pub key: String,
    pub kind: SolveErrorKind,
    pub message: String,
    pub source_nodes: BTreeSet<NodeId>,
    pub constraint_id: Option<String>,
    pub missing_fields: Vec<String>,
    pub extra_fields: Vec<String>,
    pub expected: Option<String>,
    pub actual: Option<String>,
    pub focus_span: Option<Span>,
    pub trace_type_vars: BTreeSet<TVar>,
    pub trace_row_vars: BTreeSet<RVar>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SolveErrorKind {
    Mismatch,
    Occurs,
    RowFields,
    RowTail,
    Unresolved,
}

pub fn unify(
    factory: &mut TypeFactory,
    subst: &mut crate::types::Subst,
    left: &Type,
    right: &Type,
    sources: &BTreeSet<NodeId>,
) -> Result<(), SolveError> {
    let left = subst.apply_type(left);
    let right = subst.apply_type(right);
    match (&left, &right) {
        (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
        (Type::Var(var), other) | (other, Type::Var(var)) => {
            if other.occurs(*var) || other.row_occurs_marker(*var) {
                Err(SolveError {
                    key: format!("occurs:{}:{}", var, stable_type(other)),
                    kind: SolveErrorKind::Occurs,
                    message: format!("infinite type: {} occurs in {}", crate::types::var_name(*var), crate::types::display(other)),
                    source_nodes: sources.clone(),
                    constraint_id: None,
                    missing_fields: Vec::new(),
                    extra_fields: Vec::new(),
                    expected: Some(crate::types::display(other)),
                    actual: Some(crate::types::var_name(*var)),
                    focus_span: None,
                    trace_type_vars: std::iter::once(*var).chain(type_vars_of(other)).collect(),
                    trace_row_vars: BTreeSet::new(),
                })
            } else {
                subst.bind_type(*var, other.clone());
                Ok(())
            }
        }
        (Type::Con(a), Type::Con(b)) if a == b => Ok(()),
        (Type::App(a_left, a_right), Type::App(b_left, b_right)) => {
            unify(factory, subst, a_left, b_left, sources)?;
            unify(factory, subst, a_right, b_right, sources)
        }
        (Type::Record(left_row), Type::Record(right_row)) => {
            unify_rows(factory, subst, RowKind::Record, left_row, right_row, sources)
        }
        (Type::Variant(left_row), Type::Variant(right_row)) => {
            unify_rows(factory, subst, RowKind::Variant, left_row, right_row, sources)
        }
        _ => Err(mismatch(&left, &right, sources)),
    }
}

pub fn unify_rows(
    _factory: &mut TypeFactory,
    subst: &mut crate::types::Subst,
    kind: RowKind,
    left: &Row,
    right: &Row,
    sources: &BTreeSet<NodeId>,
) -> Result<(), SolveError> {
    let left = subst.apply_row(left);
    let right = subst.apply_row(right);
    let left_names: BTreeSet<_> = left.fields.keys().cloned().collect();
    let right_names: BTreeSet<_> = right.fields.keys().cloned().collect();
    for name in left_names.intersection(&right_names) {
        let left_ty = left.fields.get(name).unwrap().clone();
        let right_ty = right.fields.get(name).unwrap().clone();
        unify(_factory, subst, &left_ty, &right_ty, sources)?;
    }

    let only_left: BTreeMap<_, _> = left_names.difference(&right_names).map(|name| (name.clone(), left.fields.get(name).unwrap().clone())).collect();
    let only_right: BTreeMap<_, _> = right_names.difference(&left_names).map(|name| (name.clone(), right.fields.get(name).unwrap().clone())).collect();
    let missing_from_right: Vec<String> = only_left.keys().cloned().collect();
    let extra_on_right: Vec<String> = only_right.keys().cloned().collect();
    let kind_name = if matches!(kind, RowKind::Record) { "record" } else { "variant" };

    match (&left.tail, &right.tail) {
        (RowTail::Empty, RowTail::Empty) => {
            if only_left.is_empty() && only_right.is_empty() {
                Ok(())
            } else {
                Err(row_error(kind_name, &missing_from_right, &extra_on_right, &left, &right, sources))
            }
        }
        (RowTail::Empty, RowTail::Open(var)) => {
            if only_left.is_empty() {
                subst.bind_row(*var, Row { fields: only_right, tail: RowTail::Empty });
                Ok(())
            } else {
                Err(row_error(kind_name, &missing_from_right, &extra_on_right, &left, &right, sources))
            }
        }
        (RowTail::Open(var), RowTail::Empty) => {
            if only_right.is_empty() {
                subst.bind_row(*var, Row { fields: only_left, tail: RowTail::Empty });
                Ok(())
            } else {
                Err(row_error(kind_name, &missing_from_right, &extra_on_right, &left, &right, sources))
            }
        }
        (RowTail::Open(left_var), RowTail::Open(right_var)) => {
            if left_var == right_var {
                return Err(SolveError {
                    key: format!("same-open-row:{}", left_var),
                    kind: SolveErrorKind::RowTail,
                    message: "same open row tail cannot account for distinct fields".into(),
                    source_nodes: sources.clone(),
                    constraint_id: None,
                    missing_fields: missing_from_right,
                    extra_fields: extra_on_right,
                    expected: Some(crate::types::display(&Type::Record(left.clone()))),
                    actual: Some(crate::types::display(&Type::Record(right.clone()))),
                    focus_span: None,
                    trace_type_vars: vars_of_rows(&[&left, &right]),
                    trace_row_vars: row_vars_of_rows(&[&left, &right]),
                });
            }
            let (fresh_var, _) = _factory.fresh_row_var(BTreeSet::new(), "unified open row".into());
            subst.bind_row(*left_var, Row { fields: only_left, tail: RowTail::Open(fresh_var) });
            subst.bind_row(*right_var, Row { fields: only_right, tail: RowTail::Open(fresh_var) });
            Ok(())
        }
    }
}

fn mismatch(left: &Type, right: &Type, sources: &BTreeSet<NodeId>) -> SolveError {
    SolveError {
        key: format!("mismatch:{}:{}", stable_type(left), stable_type(right)),
        kind: SolveErrorKind::Mismatch,
        message: format!("cannot unify {} with {}", crate::types::display(left), crate::types::display(right)),
        source_nodes: sources.clone(),
        constraint_id: None,
        missing_fields: Vec::new(),
        extra_fields: Vec::new(),
        expected: Some(crate::types::display(left)),
        actual: Some(crate::types::display(right)),
        focus_span: None,
        trace_type_vars: std::iter::empty().chain(type_vars_of(left)).chain(type_vars_of(right)).collect(),
        trace_row_vars: std::iter::empty().chain(row_vars_of(left)).chain(row_vars_of(right)).collect(),
    }
}

fn row_error(kind_name: &str, missing: &[String], extra: &[String], left: &Row, right: &Row, sources: &BTreeSet<NodeId>) -> SolveError {
    SolveError {
        key: format!("row:{}:{}:{}", kind_name, missing.join(","), extra.join(",")),
        kind: SolveErrorKind::RowFields,
        message: format!(
            "{kind_name} fields differ: missing [{}], extra [{}]",
            missing.join(", "),
            extra.join(", ")
        ),
        source_nodes: sources.clone(),
        constraint_id: None,
        missing_fields: missing.to_vec(),
        extra_fields: extra.to_vec(),
        expected: Some(crate::types::display(&Type::Record(left.clone()))),
        actual: Some(crate::types::display(&Type::Record(right.clone()))),
        focus_span: None,
        trace_type_vars: vars_of_rows(&[left, right]),
        trace_row_vars: row_vars_of_rows(&[left, right]),
    }
}

pub fn stable_type(ty: &Type) -> String {
    match ty {
        Type::Var(var) => format!("t{var}"),
        Type::Con(name) => name.clone(),
        Type::App(left, right) => format!("({} {})", stable_type(left), stable_type(right)),
        Type::Record(row) | Type::Variant(row) => {
            let prefix = if matches!(ty, Type::Record(_)) { "R" } else { "V" };
            let fields = row.fields.iter().map(|(name, ty)| format!("{name}:{}", stable_type(ty))).collect::<Vec<_>>().join(",");
            let tail = match row.tail { RowTail::Empty => "_".into(), RowTail::Open(var) => format!("r{var}") };
            format!("{prefix}({fields}|{tail})")
        }
    }
}

impl Type {
    pub fn row_occurs_marker(&self, _var: TVar) -> bool { false }
}

pub fn row_var_from_tail(tail: &RowTail) -> Option<RVar> {
    if let RowTail::Open(var) = tail { Some(*var) } else { None }
}

pub fn make_scheme(ty: Type, generalized: bool, ambient_types: &BTreeSet<TVar>, ambient_rows: &BTreeSet<RVar>, value_restricted: bool) -> Scheme {
    let mut type_vars = BTreeSet::new();
    ty.type_vars(&mut type_vars);
    let mut row_vars = BTreeSet::new();
    ty.row_vars(&mut row_vars);
    let params = if generalized && !value_restricted {
        type_vars.difference(ambient_types).copied().map(Param::Type)
            .chain(row_vars.difference(ambient_rows).copied().map(Param::Row))
            .collect()
    } else {
        Vec::new()
    };
    Scheme { params, body: ty, generalized: generalized && !value_restricted, value_restricted }
}

fn type_vars_of(ty: &Type) -> BTreeSet<TVar> {
    let mut vars = BTreeSet::new();
    ty.type_vars(&mut vars);
    vars
}

fn row_vars_of(ty: &Type) -> BTreeSet<RVar> {
    let mut vars = BTreeSet::new();
    ty.row_vars(&mut vars);
    vars
}

fn vars_of_rows(rows: &[&Row]) -> BTreeSet<TVar> {
    let mut vars = BTreeSet::new();
    for row in rows {
        for ty in row.fields.values() {
            ty.type_vars(&mut vars);
        }
    }
    vars
}

fn row_vars_of_rows(rows: &[&Row]) -> BTreeSet<RVar> {
    let mut vars = BTreeSet::new();
    for row in rows {
        if let RowTail::Open(var) = row.tail {
            vars.insert(var);
        }
        for ty in row.fields.values() {
            ty.row_vars(&mut vars);
        }
    }
    vars
}
