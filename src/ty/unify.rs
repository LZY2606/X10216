//! Unification: occurs-check, arrow/con clash, row missing/extra separation.

use super::ty::*;

#[derive(Clone, Debug)]
pub enum UErr {
    Occurs { var: VarId, ty: Ty, kind: RowKind },
    RowOccurs { rvar: RVarId, kind: RowKind },
    ConMismatch { a: Con, b: Con },
    ConArrow { con: Con },
    ArrowCon { con: Con },
    RowCon { con: Con, kind: RowKind },
    ConRow { con: Con, kind: RowKind },
    ArrowRow { kind: RowKind },
    RowArrow { kind: RowKind },
    RecordVariant { a: RowKind, b: RowKind },
    RowClosed {
        kind: RowKind,
        missing: Vec<(String, Ty)>,
        extra: Vec<(String, Ty)>,
    },
    FieldMismatch {
        kind: RowKind,
        field: String,
        err: Box<UErr>,
    },
}

fn merge_prov_ty(e: &Engine, from: &Ty, target_v: VarId) {
    if let Ty::Var(v) = from {
        let p = e.prov_t(*v);
        e.merge_prov_t(target_v, &p);
    }
}

fn bind_var(e: &Engine, v: VarId, t: Ty, rank: u32) {
    merge_prov_ty(e, &t, v);
    e.link_t(v, t);
    e.set_tvar_rank(v, rank);
}

/// Occurs check for a type variable inside a (resolved) type.
fn occurs_ty(e: &Engine, v: VarId, t: &Ty) -> bool {
    match t {
        Ty::Var(v2) => v == *v2,
        Ty::Con(_) => false,
        Ty::Arrow(a, b) => occurs_ty(e, v, a) || occurs_ty(e, v, b),
        Ty::Row(_, fs, tail) => {
            for f in fs { if occurs_ty(e, v, &f.ty) { return true; } }
            // v occurring inside a free row variable's accumulated fields
            if let Tail::Open(rv) = tail {
                if let RVar::Free { entry, .. } = e.rvar(*rv) {
                    for f in entry.fields { if occurs_ty(e, v, &f.ty) { return true; } }
                }
            }
            false
        }
    }
}

/// Occurs check for a row variable inside a row entry.
fn occurs_row(e: &Engine, rv: RVarId, entry: &REntry) -> bool {
    match &entry.tail {
        Tail::Open(rv2) if *rv2 == rv => return true,
        _ => {}
    }
    if let Tail::Open(rv2) = entry.tail {
        if let RVar::Free { entry: e2, .. } = e.rvar(rv2) {
            return occurs_row(e, rv, &e2);
        }
    }
    false
}

fn link_r_chain(e: &Engine, rv: RVarId, kind: RowKind, fields: Vec<Field>, tail: Tail) {
    e.bind_r(rv, REntry { fields, tail });
    let _ = kind;
}

fn err_field(kind: RowKind, f: String, err: UErr) -> UErr {
    UErr::FieldMismatch { kind, field: f, err: Box::new(err) }
}

/// Unify two fully resolved types.
pub fn unify(e: &Engine, x: &Ty, y: &Ty) -> Result<(), UErr> {
    let a = e.resolve(x);
    let b = e.resolve(y);
    match (&a, &b) {
        (Ty::Var(v1), Ty::Var(v2)) if v1 == v2 => Ok(()),
        (Ty::Var(v), other) | (other, Ty::Var(v)) => {
            let v = *v;
            if occurs_ty(e, v, other) {
                return Err(match other {
                    Ty::Row(kind, _, _) => UErr::Occurs { var: v, ty: other.clone(), kind: *kind },
                    _ => UErr::Occurs { var: v, ty: other.clone(), kind: RowKind::Record },
                });
            }
            let r1 = e.tvar_rank(v);
            let r2 = if let Ty::Var(v2) = other { e.tvar_rank(*v2) } else { r1 };
            let rank = r1.min(r2);
            bind_var(e, v, other.clone(), rank);
            Ok(())
        }
        (Ty::Con(c1), Ty::Con(c2)) => {
            if c1 == c2 { Ok(()) } else { Err(UErr::ConMismatch { a: *c1, b: *c2 }) }
        }
        (Ty::Con(c), Ty::Arrow(..)) => Err(UErr::ConArrow { con: *c }),
        (Ty::Arrow(..), Ty::Con(c)) => Err(UErr::ArrowCon { con: *c }),
        (Ty::Con(c), Ty::Row(k, _, _)) => Err(UErr::ConRow { con: *c, kind: *k }),
        (Ty::Row(k, _, _), Ty::Con(c)) => Err(UErr::RowCon { con: *c, kind: *k }),
        (Ty::Arrow(..), Ty::Row(k, _, _)) => Err(UErr::ArrowRow { kind: *k }),
        (Ty::Row(k, _, _), Ty::Arrow(..)) => Err(UErr::RowArrow { kind: *k }),
        (Ty::Arrow(a1, b1), Ty::Arrow(a2, b2)) => {
            unify(e, a1, a2)?;
            unify(e, b1, b2)
        }
        (Ty::Row(k1, _, _), Ty::Row(k2, _, _)) if k1 != k2 => {
            Err(UErr::RecordVariant { a: *k1, b: *k2 })
        }
        (Ty::Row(kind, f1, t1), Ty::Row(_, f2, t2)) => {
            unify_rows(e, *kind, f1.clone(), t1.clone(), f2.clone(), t2.clone())
        }
    }
}

/// Normalize a row to (fields, tail), dereferencing one free/closed rvar chain.
fn normalize(e: &Engine, kind: RowKind, fields: Vec<Field>, tail: Tail) -> (Vec<Field>, Tail) {
    let _ = kind;
    match tail {
        Tail::Closed => (fields, Tail::Closed),
        Tail::Open(rv) => {
            match e.rvar(rv) {
                RVar::Bound(entry) => {
                    let (mut fs, tl) = normalize(e, kind, entry.fields, entry.tail);
                    let mut all = fields;
                    all.append(&mut fs);
                    (all, tl)
                }
                RVar::Free { .. } => (fields, Tail::Open(rv))
            }
        }
    }
}

fn unify_rows(
    e: &Engine,
    kind: RowKind,
    f1: Vec<Field>,
    t1: Tail,
    f2: Vec<Field>,
    t2: Tail,
) -> Result<(), UErr> {
    let (fs1, tl1) = normalize(e, kind, f1, t1);
    let (fs2, tl2) = normalize(e, kind, f2, t2);

    // Unify common-named fields; partition the rest.
    // missing: named by row2 only (row1 lacks them)
    // extra:   named by row1 only (row1 has too many)
    let mut missing: Vec<(String, Ty)> = Vec::new();
    let mut extra: Vec<Field> = Vec::new();
    let mut taken = vec![false; fs2.len()];
    for f in &fs1 {
        match fs2.iter().position(|g| g.name == f.name) {
            Some(j) => {
                taken[j] = true;
                unify(e, &f.ty, &fs2[j].ty).map_err(|er| err_field(kind, f.name.clone(), er))?;
            }
            None => extra.push(f.clone()),
        }
    }
    for (j, g) in fs2.iter().enumerate() {
        if !taken[j] { missing.push((g.name.clone(), g.ty.clone())); }
    }

    match (&tl1, &tl2) {
        (Tail::Closed, Tail::Closed) => {
            if missing.is_empty() && extra.is_empty() { Ok(()) }
            else { Err(UErr::RowClosed { kind, missing, extra: extra.into_iter().map(|f| (f.name, f.ty)).collect() }) }
        }
        (Tail::Open(r1v), Tail::Closed) => {
            if !missing.is_empty() {
                return Err(UErr::RowClosed { kind, missing, extra: Vec::new() });
            }
            let r1v = *r1v;
            let entry = REntry { fields: extra, tail: Tail::Closed };
            if occurs_row(e, r1v, &entry) { return Err(UErr::RowOccurs { rvar: r1v, kind }); }
            link_r_chain(e, r1v, kind, entry.fields, entry.tail);
            Ok(())
        }
        (Tail::Closed, Tail::Open(r2v)) => {
            if !extra.is_empty() {
                return Err(UErr::RowClosed { kind, missing: Vec::new(), extra: extra.into_iter().map(|f| (f.name, f.ty)).collect() });
            }
            let r2v = *r2v;
            let entry = REntry { fields: missing.into_iter().map(|(n, t)| Field { name: n, ty: t }).collect(), tail: Tail::Closed };
            if occurs_row(e, r2v, &entry) { return Err(UErr::RowOccurs { rvar: r2v, kind }); }
            link_r_chain(e, r2v, kind, entry.fields, entry.tail);
            Ok(())
        }
        (Tail::Open(r1v), Tail::Open(r2v)) => {
            let r1v = *r1v;
            let r2v = *r2v;
            let rank = match (e.rvar(r1v), e.rvar(r2v)) {
                (RVar::Free { rank: a, .. }, RVar::Free { rank: b, .. }) => a.min(b),
                _ => return Err(UErr::RowClosed { kind, missing: Vec::new(), extra: Vec::new() }),
            };
            let rho = e.fresh_row_var(rank, kind, None);
            let e1 = REntry { fields: extra, tail: Tail::Open(rho) };
            let e2 = REntry { fields: missing.into_iter().map(|(n, t)| Field { name: n, ty: t }).collect(), tail: Tail::Open(rho) };
            if occurs_row(e, r1v, &e1) { return Err(UErr::RowOccurs { rvar: r1v, kind }); }
            if occurs_row(e, r2v, &e2) { return Err(UErr::RowOccurs { rvar: r2v, kind }); }
            link_r_chain(e, r1v, kind, e1.fields, e1.tail);
            link_r_chain(e, r2v, kind, e2.fields, e2.tail);
            Ok(())
        }
    }
}
