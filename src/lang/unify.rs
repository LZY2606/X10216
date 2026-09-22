//! Structural unification with occurs check and row polymorphism.
//!
//! Row mismatches are split into *missing* and *extra* fields relative to a
//! expected shape, so diagnostics can say exactly which side each label is on.

use crate::lang::span::Span;
use crate::lang::types::{Kind, Prim, Row, RowField, Store, Ty, VarId};
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub enum UError {
    Clash {
        left: Ty,
        right: Ty,
        at: Span,
    },
    Occurs {
        var: VarId,
        kind: Kind,
        in_ty: Ty,
        at: Span,
    },
    RowMismatch {
        flavor: RowFlavor,
        missing: Vec<FieldDiff>,
        extra: Vec<FieldDiff>,
        at: Span,
    },
    DuplicateLabel {
        flavor: RowFlavor,
        label: String,
        at: Span,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RowFlavor {
    Record,
    Variant,
}

impl RowFlavor {
    pub fn word(self) -> &'static str {
        match self {
            RowFlavor::Record => "记录",
            RowFlavor::Variant => "变体",
        }
    }
}

#[derive(Clone, Debug)]
pub struct FieldDiff {
    pub label: String,
    pub ty: Ty,
    pub at: Span,
}

fn duplicate_labels(row: &Row, flavor: RowFlavor) -> Option<UError> {
    let mut seen: HashSet<&str> = HashSet::new();
    for f in &row.fields {
        if !seen.insert(f.label.as_str()) {
            return Some(UError::DuplicateLabel {
                flavor,
                label: f.label.clone(),
                at: f.at,
            });
        }
    }
    None
}

pub fn unify(store: &mut Store, a: &Ty, b: &Ty, at: Span) -> Result<(), UError> {
    let a = store.prune(a);
    let b = store.prune(b);
    match (a, b) {
        (Ty::Var(u), Ty::Var(v)) if u == v => Ok(()),
        (Ty::Var(v), other) | (other, Ty::Var(v)) => {
            if occurs_ty(store, v, &other) {
                Err(UError::Occurs {
                    var: v,
                    kind: Kind::Type,
                    in_ty: other,
                    at,
                })
            } else {
                store.bind_ty(v, other);
                Ok(())
            }
        }
        (Ty::Con(p), Ty::Con(q)) if p == q => Ok(()),
        (Ty::Con(p), other) => Err(UError::Clash {
            left: Ty::Con(p),
            right: other,
            at,
        }),
        (Ty::Arrow(a1, a2), Ty::Arrow(b1, b2)) => {
            unify(store, &a1, &b1, at)?;
            unify(store, &a2, &b2, at)
        }
        (Ty::Record(r1), Ty::Record(r2)) => unify_rows(store, &r1, &r2, RowFlavor::Record, at),
        (Ty::Variant(r1), Ty::Variant(r2)) => unify_rows(store, &r1, &r2, RowFlavor::Variant, at),
        (left, right) => Err(UError::Clash {
            left,
            right,
            at,
        }),
    }
}

fn occurs_ty(store: &Store, v: VarId, ty: &Ty) -> bool {
    let mut found = false;
    let mut stack = vec![store.prune(ty)];
    while let Some(t) = stack.pop() {
        match t {
            Ty::Var(w) => {
                if w == v {
                    found = true;
                }
            }
            Ty::Con(_) => {}
            Ty::Arrow(a, b) => {
                stack.push(store.prune(&a));
                stack.push(store.prune(&b));
            }
            Ty::Record(r) | Ty::Variant(r) => {
                for f in &r.fields {
                    stack.push(store.prune(&f.ty));
                }
            }
        }
    }
    found
}

fn occurs_row(store: &Store, v: VarId, row: &Row) -> bool {
    let flat = store.prune_row(row);
    for f in &flat.fields {
        if occurs_ty(store, v, &f.ty) {
            return true;
        }
    }
    false
}

/// Sorted-merge row unification (Remy-style open rows).
fn unify_rows(
    store: &mut Store,
    r1: &Row,
    r2: &Row,
    flavor: RowFlavor,
    at: Span,
) -> Result<(), UError> {
    let r1 = store.prune_row(r1);
    let r2 = store.prune_row(r2);
    duplicate_labels(&r1, flavor)?;
    duplicate_labels(&r2, flavor)?;

    let mut i = 0usize;
    let mut j = 0usize;
    let mut shared: Vec<RowField> = Vec::new();
    let mut only_left: Vec<RowField> = Vec::new();
    let mut only_right: Vec<RowField> = Vec::new();

    while i < r1.fields.len() || j < r2.fields.len() {
        let lf = r1.fields.get(i);
        let rf = r2.fields.get(j);
        match (lf, rf) {
            (Some(l), Some(r)) if l.label == r.label => {
                shared.push(RowField {
                    label: l.label.clone(),
                    ty: store.prune(&l.ty),
                    at: l.at.merge(r.at),
                });
                unify(store, &l.ty, &r.ty, at)?;
                i += 1;
                j += 1;
            }
            (Some(l), Some(r)) if l.label < r.label => {
                only_left.push(l.clone());
                i += 1;
            }
            (Some(l), Some(r)) => {
                only_right.push(r.clone());
                j += 1;
            }
            (Some(l), None) => {
                only_left.push(l.clone());
                i += 1;
            }
            (None, Some(r)) => {
                only_right.push(r.clone());
                j += 1;
            }
            _ => unreachable!(),
        }
    }

    let t1 = r1.tail.map(|v| store.root_row(v));
    let t2 = r2.tail.map(|v| store.root_row(v));

    // Fields the closed side does not have are extra; fields it requires but
    // the other side lacks are missing. With two closed rows every asymmetry
    // is therefore reported.
    match (t1, t2) {
        (None, None) => {
            if only_left.is_empty() && only_right.is_empty() {
                Ok(())
            } else {
                Err(row_error(flavor, only_right, only_left, at))
            }
        }
        (None, Some(v2)) => {
            // right row variable must absorb left-only fields; right-only are missing.
            bind_open(store, v2, flavor, only_left.clone(), at)?;
            if only_right.is_empty() {
                Ok(())
            } else {
                Err(row_error(flavor, only_right, Vec::new(), at))
            }
        }
        (Some(v1), None) => {
            bind_open(store, v1, flavor, only_right.clone(), at)?;
            if only_left.is_empty() {
                Ok(())
            } else {
                Err(row_error(flavor, Vec::new(), only_left, at))
            }
        }
        (Some(v1), Some(v2)) if v1 == v2 => {
            if only_left.is_empty() && only_right.is_empty() {
                Ok(())
            } else {
                // Remaining differences can only live outside the shared tail,
                // which must stay common: split into two fresh extension vars.
                let ext1 = store.fresh(Kind::Row);
                let ext2 = store.fresh(Kind::Row);
                let new_tail = store.fresh(Kind::Row);
                bind_open(store, ext1, flavor, only_left, at)?;
                bind_open(store, ext2, flavor, only_right, at)?;
                store.bind_row(
                    v1,
                    Row::open(Vec::new(), ext1),
                );
                // After v1 is bound, point ext1 at new_tail as well by chaining.
                store.bind_row(ext1, Row::empty_open(new_tail));
                store.bind_row(v2, Row::empty_open(ext2));
                store.bind_row(ext2, Row::empty_open(new_tail));
                let _ = shared;
                Ok(())
            }
        }
        (Some(v1), Some(v2)) => {
            // Distinct open tails: left-only fields go to v1, right-only to v2,
            // tails are merged via a fresh common variable.
            let common = store.fresh(Kind::Row);
            bind_open(store, v1, flavor, only_left, at)?;
            bind_open(store, v2, flavor, only_right, at)?;
            // Re-open: bind_open may have closed the var when the list is empty.
            store.bind_row(v1, Row::empty_open(common));
            store.bind_row(v2, Row::empty_open(common));
            let _ = shared;
            Ok(())
        }
    }
}

fn bind_open(
    store: &mut Store,
    v: VarId,
    flavor: RowFlavor,
    fields: Vec<RowField>,
    at: Span,
) -> Result<(), UError> {
    // Guard against cycles introduced via a bound tail.
    if occurs_row(store, v, &Row::open(fields.clone(), v)) {
        return Err(UError::Occurs {
            var: v,
            kind: Kind::Row,
            in_ty: match flavor {
                RowFlavor::Record => Ty::Record(Row::open(fields, v)),
                RowFlavor::Variant => Ty::Variant(Row::open(fields, v)),
            },
            at,
        });
    }
    let row = Row::open(fields, v);
    store.bind_row(v, row);
    Ok(())
}

fn row_error(
    flavor: RowFlavor,
    missing: Vec<RowField>,
    extra: Vec<RowField>,
    at: Span,
) -> UError {
    UError::RowMismatch {
        flavor,
        missing: missing
            .into_iter()
            .map(|f| FieldDiff {
                label: f.label,
                ty: f.ty,
                at: f.at,
            })
            .collect(),
        extra: extra
            .into_iter()
            .map(|f| FieldDiff {
                label: f.label,
                ty: f.ty,
                at: f.at,
            })
            .collect(),
        at,
    }
}

#[allow(dead_code)]
fn prim(p: Prim) -> Ty {
    Ty::Con(p)
}
