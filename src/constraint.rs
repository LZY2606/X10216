//! Constraint generation.
//!
//! Constraints form a flat, ordered list (a DAG):
//!
//! - [`CKind::Inst`] instantiates a (possibly generalized) scheme at a use
//! - [`CKind::Gen`]  generalizes a type at a let binding and binds a name
//! - [`CKind::Rec`]  links a recursive name to its (monomorphic) RHS type
//! - [`CKind::Eq`]   unifies two types
//!
//! Each constraint carries the source node responsible for it and a stable id
//! so constraints can be matched across edited versions of a program.

use crate::span::{NodeId, Span};
use crate::syntax::{Binding, Expr, FieldArm, TagArm};
use crate::ty::{Ty, VarId, VarKey};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum CKind {
    Eq,
    Inst,
    Gen,
    Rec,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Constraint {
    pub cid: String,
    pub kind: CKind,
    pub label: String,
    pub node: NodeId,
    pub span: Span,
    /// Free variable ids present in left/right, for graph visualization.
    pub vars: Vec<VarId>,
    pub left: Ty,
    pub right: Ty,
    /// For Inst/Gen/Rec: the name being instantiated/generalized/rec-bound.
    pub name: Option<String>,
    /// Whether generalization is suppressed by the value restriction.
    pub restricted: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VarInfoOut {
    pub id: VarId,
    pub key: VarKey,
    pub origin: NodeId,
    pub span: Span,
}

#[derive(Clone)]
struct Entry {
    ty: Ty,
    /// Generalized variable keys at the binding point (empty = monomorphic).
    quantified: Vec<VarKey>,
    value_restricted: bool,
}

pub struct Ctx {
    pub constraints: Vec<Constraint>,
    pub var_infos: BTreeMap<VarId, (VarKey, NodeId, Span)>,
    id_of_key: BTreeMap<VarKey, VarId>,
    fresh: VarId,
    counter: u32,
    scope: Vec<(String, Entry)>,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            constraints: Vec::new(),
            var_infos: BTreeMap::new(),
            id_of_key: BTreeMap::new(),
            fresh: 1,
            counter: 0,
            scope: Vec::new(),
        }
    }

    /// Create (or reuse) a type variable with a parse-stable identity.
    fn stable_var(&mut self, node: &NodeId, span: Span, suffix: &str) -> Ty {
        let key = VarKey::stable(node, suffix);
        if let Some(id) = self.id_of_key.get(&key) {
            return Ty::Var(*id, key);
        }
        let id = self.fresh;
        self.fresh += 1;
        self.id_of_key.insert(key.clone(), id);
        self.var_infos.insert(id, (key.clone(), node.clone(), span));
        Ty::Var(id, key)
    }

    /// Internal fresh variable (instantiation copies, etc.).
    fn fresh_var(&mut self) -> Ty {
        let id = self.fresh;
        self.fresh += 1;
        let key = VarKey::Fresh(id);
        self.var_infos.insert(
            id,
            (key.clone(), NodeId::new(u32::MAX as usize, 0, 0), Span::DUMMY),
        );
        Ty::Var(id, key)
    }

    fn emit(
        &mut self,
        kind: CKind,
        node: NodeId,
        span: Span,
        label: String,
        left: Ty,
        right: Ty,
        name: Option<String>,
        restricted: bool,
    ) {
        let mut vars = BTreeSet::new();
        collect_vars(&left, &mut vars);
        collect_vars(&right, &mut vars);
        self.counter += 1;
        let cid = format!("c{:04}-{}", self.counter, node.key());
        self.constraints.push(Constraint {
            cid,
            kind,
            label,
            node,
            span,
            vars: vars.into_iter().collect(),
            left,
            right,
            name,
            restricted,
        });
    }

    fn lookup(&self, name: &str) -> Option<&Entry> {
        self.scope.iter().rev().find(|(n, _)| n == name).map(|(_, e)| e)
    }
    fn push(&mut self, name: String, entry: Entry) {
        self.scope.push((name, entry));
    }
    fn pop_to(&mut self, depth: usize) {
        self.scope.truncate(depth);
    }
}

pub fn collect_vars(ty: &Ty, out: &mut BTreeSet<VarId>) {
    match ty {
        Ty::Var(v, _) => {
            out.insert(*v);
        }
        Ty::Int(_) | Ty::Bool(_) => {}
        Ty::Fun(a, b, _) => {
            collect_vars(a, out);
            collect_vars(b, out);
        }
        Ty::Record(row, _) | Ty::Variant(row, _) => {
            for (_, t) in &row.fields {
                collect_vars(t, out);
            }
            if let Some(e) = row.ext {
                out.insert(e);
            }
        }
    }
}

/// Immutable syntactic value eligible for polymorphic generalization.
pub fn is_syntactic_value(e: &Expr) -> bool {
    match e {
        Expr::Lam { .. } | Expr::LitInt(..) | Expr::LitBool(..) | Expr::Var(..) => true,
        Expr::Tag { payload, .. } => {
            payload.as_ref().map(|p| is_syntactic_value(p)).unwrap_or(true)
        }
        Expr::Record { fields, .. } => fields.iter().all(|(_, e)| is_syntactic_value(e)),
        Expr::Let { rhs, body, .. } => is_syntactic_value(rhs) && is_syntactic_value(body),
        _ => false,
    }
}

pub fn generate(bindings: &[Binding]) -> (Vec<Constraint>, Vec<VarInfoOut>) {
    let mut ctx = Ctx::new();
    for b in bindings {
        gen_binding(&mut ctx, b);
    }
    let vars = ctx
        .var_infos
        .iter()
        .map(|(id, (key, node, span))| VarInfoOut {
            id: *id,
            key: key.clone(),
            origin: node.clone(),
            span: *span,
        })
        .collect();
    (ctx.constraints, vars)
}

fn gen_binding(ctx: &mut Ctx, b: &Binding) {
    if b.rec {
        // Recursive names are monomorphic: bind the name to the RHS type via a
        // Rec constraint, then Gen with empty quantification.
        let ty = gen_expr(ctx, &b.expr);
        let name_ty = ctx.stable_var(
            &b.id,
            b.span,
            if b.params.is_empty() { "rec" } else { "recfn" },
        );
        ctx.emit(
            CKind::Rec,
            b.id.clone(),
            b.span,
            format!("recursive binding `{}`", b.name),
            name_ty.clone(),
            ty,
            Some(b.name.clone()),
            false,
        );
        ctx.emit(
            CKind::Gen,
            b.id.clone(),
            b.span,
            format!("generalize `{}` (recursive, monomorphic)", b.name),
            name_ty,
            Ty::Int(None), // unused placeholder for Gen's right side
            Some(b.name.clone()),
            true,
        );
        ctx.push(
            b.name.clone(),
            Entry { ty: Ty::Int(None), quantified: Vec::new(), value_restricted: true },
        );
    } else {
        let ty = gen_expr(ctx, &b.expr);
        let restricted = !is_syntactic_value(&b.expr);
        ctx.emit(
            CKind::Gen,
            b.id.clone(),
            b.span,
            format!("generalize `{}`", b.name),
            ty,
            Ty::Int(None),
            Some(b.name.clone()),
            restricted,
        );
        ctx.push(
            b.name.clone(),
            Entry { ty: Ty::Int(None), quantified: Vec::new(), value_restricted: restricted },
        );
    }
}

fn gen_expr(ctx: &mut Ctx, e: &Expr) -> Ty {
    match e {
        Expr::LitInt(_, node, span) => {
            let tv = ctx.stable_var(node, *span, "ty");
            ctx.emit(
                CKind::Eq,
                node.clone(),
                *span,
                "integer literal : Int".into(),
                tv.clone(),
                Ty::Int(Some(node.clone())),
                None,
                false,
            );
            tv
        }
        Expr::LitBool(_, node, span) => {
            let tv = ctx.stable_var(node, *span, "ty");
            ctx.emit(
                CKind::Eq,
                node.clone(),
                *span,
                "boolean literal : Bool".into(),
                tv.clone(),
                Ty::Bool(Some(node.clone())),
                None,
                false,
            );
            tv
        }
        Expr::Var(name, node, span) => {
            let tv = ctx.stable_var(node, *span, "ty");
            match ctx.lookup(name) {
                None => {
                    ctx.emit(
                        CKind::Eq,
                        node.clone(),
                        *span,
                        format!("unbound variable `{}`", name),
                        tv.clone(),
                        // A dedicated opaque marker type makes this fail clearly.
                        Ty::Variant(
                            crate::ty::Row {
                                fields: vec![("__unbound__".into(), Ty::Int(None))],
                                ext: None,
                                origin: Some(node.clone()),
                            },
                            Some(node.clone()),
                        ),
                        Some(name.clone()),
                        false,
                    );
                }
                Some(entry) => {
                    let inst = if entry.quantified.is_empty() {
                        entry.ty.clone()
                    } else {
                        instantiate(ctx, entry)
                    };
                    ctx.emit(
                        CKind::Inst,
                        node.clone(),
                        *span,
                        format!("instantiate `{}`", name),
                        tv.clone(),
                        inst,
                        Some(name.clone()),
                        false,
                    );
                }
            }
            tv
        }
        Expr::Lam { param, param_id, param_span, body, id, span } => {
            let pt = ctx.stable_var(param_id, *param_span, "param");
            let depth = ctx.scope.len();
            ctx.push(
                param.clone(),
                Entry { ty: pt.clone(), quantified: Vec::new(), value_restricted: true },
            );
            let bt = gen_expr(ctx, body);
            ctx.pop_to(depth);
            let tv = ctx.stable_var(id, *span, "ty");
            let fun = Ty::Fun(Box::new(pt), Box::new(bt), Some(id.clone()));
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                format!("lambda `{}` : {} -> body", "\\", param),
                tv.clone(),
                fun,
                None,
                false,
            );
            tv
        }
        Expr::App { fun, arg, id, span } => {
            let ft = gen_expr(ctx, fun);
            let at = gen_expr(ctx, arg);
            let rt = ctx.stable_var(id, *span, "result");
            let arrow = Ty::Fun(Box::new(at), Box::new(rt.clone()), Some(id.clone()));
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                "application: function type".into(),
                ft,
                arrow,
                None,
                false,
            );
            rt
        }
        Expr::Let { name, binding_id, rhs, body, id, span } => {
            let rt = gen_expr(ctx, rhs);
            let restricted = !is_syntactic_value(rhs);
            ctx.emit(
                CKind::Gen,
                binding_id.clone(),
                rhs.span(),
                format!("generalize `{}`", name),
                rt.clone(),
                Ty::Int(None),
                Some(name.clone()),
                restricted,
            );
            let depth = ctx.scope.len();
            ctx.push(
                name.clone(),
                Entry { ty: rt, quantified: Vec::new(), value_restricted: restricted },
            );
            let bt = gen_expr(ctx, body);
            ctx.pop_to(depth);
            let tv = ctx.stable_var(id, *span, "ty");
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                "let body type".into(),
                tv.clone(),
                bt,
                None,
                false,
            );
            tv
        }
        Expr::If { cond, then_e, else_e, id, span } => {
            let ct = gen_expr(ctx, cond);
            ctx.emit(
                CKind::Eq,
                id.clone(),
                cond.span(),
                "if condition : Bool".into(),
                ct,
                Ty::Bool(Some(id.clone())),
                None,
                false,
            );
            let tt = gen_expr(ctx, then_e);
            let et = gen_expr(ctx, else_e);
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                "if branches agree".into(),
                tt.clone(),
                et,
                None,
                false,
            );
            let tv = ctx.stable_var(id, *span, "ty");
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                "if result type".into(),
                tv.clone(),
                tt,
                None,
                false,
            );
            tv
        }
        Expr::Record { fields, id, span } => {
            let mut ftys = Vec::new();
            for (label, fe) in fields {
                ftys.push((label.clone(), gen_expr(ctx, fe)));
            }
            let tv = ctx.stable_var(id, *span, "ty");
            let row = Ty::Record(
                crate::ty::Row { fields: ftys, ext: None, origin: Some(id.clone()) },
                Some(id.clone()),
            );
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                "record literal".into(),
                tv.clone(),
                row,
                None,
                false,
            );
            tv
        }
        Expr::Field { record, label, id, span } => {
            let rt = gen_expr(ctx, record);
            let ft = ctx.stable_var(id, *span, "field");
            let rest = ctx.fresh_var();
            let row_ty = Ty::Record(
                crate::ty::Row {
                    fields: vec![(label.clone(), ft.clone())],
                    ext: Some(as_var(&rest)),
                    origin: Some(id.clone()),
                },
                Some(id.clone()),
            );
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                format!("field access `.{}` (record must contain it; other fields free)", label),
                rt,
                row_ty,
                None,
                false,
            );
            ft
        }
        Expr::With { record, label, value, id, span } => {
            let rt = gen_expr(ctx, record);
            let vt = gen_expr(ctx, value);
            let tv = ctx.stable_var(id, *span, "ty");
            // input record: { label: old | rest }
            let old = ctx.stable_var(id, *span, "oldfield");
            let rest = ctx.fresh_var();
            let in_row = Ty::Record(
                crate::ty::Row {
                    fields: vec![(label.clone(), old)],
                    ext: Some(as_var(&rest)),
                    origin: Some(id.clone()),
                },
                Some(id.clone()),
            );
            ctx.emit(
                CKind::Eq,
                id.clone(),
                record.span(),
                format!("`with` input contains `.{}`", label),
                rt,
                in_row,
                None,
                false,
            );
            // output record: { label: vt | rest } (same extension => all else shared)
            let out_row = Ty::Record(
                crate::ty::Row {
                    fields: vec![(label.clone(), vt)],
                    ext: Some(as_var(&rest)),
                    origin: Some(id.clone()),
                },
                Some(id.clone()),
            );
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                format!("`with` updates `.{}`; other fields preserved", label),
                tv.clone(),
                out_row,
                None,
                false,
            );
            tv
        }
        Expr::Tag { tag, payload, id, span } => {
            let tv = ctx.stable_var(id, *span, "ty");
            let pt = payload.as_ref().map(|p| gen_expr(ctx, p));
            let rest = ctx.fresh_var();
            let row = Ty::Variant(
                crate::ty::Row {
                    fields: vec![(tag.clone(), pt.unwrap_or_else(|| unit_ty(id)))],
                    ext: Some(as_var(&rest)),
                    origin: Some(id.clone()),
                },
                Some(id.clone()),
            );
            ctx.emit(
                CKind::Eq,
                id.clone(),
                *span,
                format!("variant tag `{}`", tag),
                tv.clone(),
                row,
                None,
                false,
            );
            tv
        }
        Expr::Match { scrut, tag_arms, field_arms, id, span } => {
            let st = gen_expr(ctx, scrut);
            let result = ctx.stable_var(id, *span, "result");

            // Variant arms: scrutinee must be a variant carrying every tag.
            if !tag_arms.is_empty() {
                let mut row_fields = Vec::new();
                for arm in tag_arms {
                    let payload_ty = arm_payload_ty(ctx, arm, id);
                    row_fields.push((arm.tag.clone(), payload_ty));
                }
                let ext = ctx.fresh_var();
                let variant_ty = Ty::Variant(
                    crate::ty::Row {
                        fields: row_fields,
                        ext: Some(as_var(&ext)),
                        origin: Some(id.clone()),
                    },
                    Some(id.clone()),
                );
                ctx.emit(
                    CKind::Eq,
                    id.clone(),
                    scrut.span(),
                    format!(
                        "match on variant with tag(s) {}",
                        tag_arms
                            .iter()
                            .map(|a| format!("`{}`", a.tag))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ),
                    st.clone(),
                    variant_ty,
                    None,
                    false,
                );
            }

            // Field-view arms: scrutinee must additionally be a record with
            // every referenced label (row polymorphism leaves the rest open).
            for farm in field_arms {
                let bound_ty = ctx.stable_var(&farm.bound_id, farm.bound_span, "view");
                let rest = ctx.fresh_var();
                let rty = Ty::Record(
                    crate::ty::Row {
                        fields: vec![(farm.label.clone(), bound_ty.clone())],
                        ext: Some(as_var(&rest)),
                        origin: Some(id.clone()),
                    },
                    Some(id.clone()),
                );
                ctx.emit(
                    CKind::Eq,
                    id.clone(),
                    farm.span,
                    format!(
                        "field view `{}(..)` requires record field `.{}`",
                        farm.label, farm.label
                    ),
                    st.clone(),
                    rty,
                    None,
                    false,
                );
                ctx.push(
                    farm.bound.clone(),
                    Entry { ty: bound_ty, quantified: Vec::new(), value_restricted: true },
                );
            }

            // Bodies, with payload/bound variables in scope.
            for arm in tag_arms {
                let bty = arm_body_ty(ctx, id, arm);
                ctx.emit(
                    CKind::Eq,
                    id.clone(),
                    *span,
                    "match arms share a result type".into(),
                    result.clone(),
                    bty,
                    None,
                    false,
                );
            }
            for farm in field_arms {
                let bty = gen_expr(ctx, &farm.body);
                ctx.emit(
                    CKind::Eq,
                    id.clone(),
                    *span,
                    "match arms share a result type".into(),
                    result.clone(),
                    bty,
                    None,
                    false,
                );
            }
            result
        }

    }
}

fn as_var(t: &Ty) -> VarId {
    match t {
        Ty::Var(v, _) => *v,
        _ => panic!("expected fresh variable"),
    }
}

/// Unit payload for nullary tags is represented as an empty record type.
fn unit_ty(node: &NodeId) -> Ty {
    Ty::Record(crate::ty::Row::empty(Some(node.clone())), Some(node.clone()))
}

fn arm_payload_ty(ctx: &mut Ctx, arm: &TagArm, match_id: &NodeId) -> Ty {
    match &arm.payload {
        None => unit_ty(match_id),
        Some((_name, pid, pspan)) => ctx.stable_var(pid, *pspan, "payload"),
    }
}

fn arm_body_ty(ctx: &mut Ctx, match_id: &NodeId, arm: &TagArm, payload_ty: Ty) -> Ty {
    let depth = ctx.scope.len();
    if let Some((name, pid, pspan)) = &arm.payload {
        let pt = ctx.stable_var(pid, *pspan, "payload");
        let _ = payload_ty; // same variable identity by key
        ctx.push(name.clone(), Entry { ty: pt, quantified: Vec::new(), value_restricted: true });
    }
    let bty = gen_expr(ctx, &arm.body);
    let _ = match_id;
    ctx.pop_to(depth);
    bty
}

fn record_view_constraints(ctx: &mut Ctx, farm: &FieldArm, match_id: &NodeId) {
    // Reconstructed from the match node: scrutinee type was already generated,
    // but we need a fresh link, so require record { label: bound | r }.
    let bound_ty = ctx.stable_var(&farm.bound_id, farm.bound_span, "view");
    let rest = ctx.fresh_var();
    let rty = Ty::Record(
        crate::ty::Row {
            fields: vec![(farm.label.clone(), bound_ty)],
            ext: Some(as_var(&rest)),
            origin: Some(match_id.clone()),
        },
        Some(match_id.clone()),
    );
    // The scrutinee type variable is not directly available here; instead we
    // emit against a stable node-derived variable keyed to this view arm, and
    // the solver links it because field-view shares the match's scrutinee var.
    let scrut_link = ctx.stable_var(match_id, farm.span, "scrut");
    ctx.emit(
        CKind::Eq,
        match_id.clone(),
        farm.span,
        format!("field view `{}(..)` requires record field `.{}`", farm.label, farm.label),
        scrut_link,
        rty,
        None,
        false,
    );
    ctx.push(
        farm.bound.clone(),
        Entry {
            ty: ctx.stable_var(&farm.bound_id, farm.bound_span, "view"),
            quantified: Vec::new(),
            value_restricted: true,
        },
    );
}

/// Freshen all generalized variables of a scheme for one instantiation site.
fn instantiate(ctx: &mut Ctx, entry: &Entry) -> Ty {
    let mut mapping: BTreeMap<VarKey, Ty> = BTreeMap::new();
    for k in &entry.quantified {
        mapping.insert(k.clone(), ctx.fresh_var());
    }
    substitute_keys(&entry.ty, &mapping)
}

fn substitute_keys(ty: &Ty, mapping: &BTreeMap<VarKey, Ty>) -> Ty {
    match ty {
        Ty::Var(v, k) => mapping.get(k).cloned().unwrap_or_else(|| Ty::Var(*v, k.clone())),
        Ty::Int(o) => Ty::Int(o.clone()),
        Ty::Bool(o) => Ty::Bool(o.clone()),
        Ty::Fun(a, b, o) => Ty::Fun(
            Box::new(substitute_keys(a, mapping)),
            Box::new(substitute_keys(b, mapping)),
            o.clone(),
        ),
        Ty::Record(row, o) => Ty::Record(
            crate::ty::Row {
                fields: row
                    .fields
                    .iter()
                    .map(|(l, t)| (l.clone(), substitute_keys(t, mapping)))
                    .collect(),
                ext: row.ext,
                origin: row.origin.clone(),
            },
            o.clone(),
        ),
        Ty::Variant(row, o) => Ty::Variant(
            crate::ty::Row {
                fields: row
                    .fields
                    .iter()
                    .map(|(l, t)| (l.clone(), substitute_keys(t, mapping)))
                    .collect(),
                ext: row.ext,
                origin: row.origin.clone(),
            },
            o.clone(),
        ),
    }
}
