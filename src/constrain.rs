//! Constraint generation with provenance and incremental-friendly identities.

use crate::syntax::{collect_keys, ExprK, Node, Span};
use crate::types::{Row, Ty};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Prov {
    pub span: Span,
    pub node: u32,
    pub desc: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CKind {
    /// Unify two types.
    Unify { a: Ty, b: Ty },
    /// Instantiate the scheme bound to `name` and unify it with `expect`.
    Inst { name: String, expect: Ty },
    /// Generalize `ty` (subject to the value restriction) and bind `name`.
    Gen { name: String, ty: Ty, expansive: bool },
    /// Bind `name` monomorphically (parameters, `let rec` self-reference,
    /// match binders). Pushes onto the solver's binding stack.
    BindMono { name: String, ty: Ty },
    /// Pop the most recent binding of `name` (restores shadowed bindings).
    Unbind { name: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Constraint {
    pub id: String,
    pub kind: CKind,
    pub prov: Prov,
}

pub struct GenOut {
    pub constraints: Vec<Constraint>,
    /// type variable assigned to each AST node id
    pub node_ty: BTreeMap<u32, u32>,
    /// type variable of the root expression
    pub root_ty: u32,
    /// next fresh variable
    pub next_var: u32,
}

struct Gen<'a> {
    keys: &'a BTreeMap<u32, (String, Vec<String>)>,
    constraints: Vec<Constraint>,
    node_ty: BTreeMap<u32, u32>,
    next_var: u32,
    /// per-node ordinal counters, keyed by (node key, scope, gen-env fingerprint)
    ordinals: BTreeMap<String, u32>,
    scope: Vec<String>,
    /// names generalized at enclosing lets -> gen-env fingerprint
    gen_env: Vec<String>,
}

impl<'a> Gen<'a> {
    fn fresh(&mut self) -> u32 {
        let v = self.next_var;
        self.next_var += 1;
        v
    }

    fn ty_of(&mut self, n: &Node) -> Ty {
        if let Some(v) = self.node_ty.get(&n.id) { return Ty::Var(*v); }
        let v = self.fresh();
        self.node_ty.insert(n.id, v);
        Ty::Var(v)
    }

    fn ctx_key(&self, n: &Node) -> String {
        let (key, _) = &self.keys[&n.id];
        // cache key contains: subtree identity + binder scope + generalization env
        format!("{}|scope:{}|gen:{}", key, self.scope.join(","), self.gen_env.join(","))
    }

    fn push(&mut self, n: &Node, kind: CKind, desc: String) {
        let base = self.ctx_key(n);
        let ord = self.ordinals.entry(base.clone()).or_insert(0);
        let id = format!("{}#{}", base, ord);
        *ord += 1;
        self.constraints.push(Constraint {
            id,
            kind,
            prov: Prov { span: n.span, node: n.id, desc },
        });
    }

    fn scoped_ret<T>(&mut self, label: &str, f: impl FnOnce(&mut Self) -> T) -> T {
        self.scope.push(label.to_string());
        let r = f(self);
        self.scope.pop();
        r
    }
}

fn expansive(n: &Node) -> bool {
    match &n.kind {
        ExprK::Int(_) | ExprK::Bool(_) | ExprK::Var(_) => false,
        ExprK::Fun { .. } => false,
        ExprK::Record(fs) => fs.iter().any(|(_, v)| expansive(v)),
        ExprK::Variant(_, p) => expansive(p),
        ExprK::App(..) => true,
        ExprK::If(c, t, e) => expansive(c) || expansive(t) || expansive(e),
        ExprK::Proj(e, _) => expansive(e),
        ExprK::Let { value, body, .. } => expansive(value) || expansive(body),
        ExprK::Match(s, arms) => expansive(s) || arms.iter().any(|(_, _, b)| expansive(b)),
    }
}

pub fn generate(root: &Node) -> GenOut {
    let keys = collect_keys(root);
    let mut g = Gen {
        keys: &keys,
        constraints: Vec::new(),
        node_ty: BTreeMap::new(),
        next_var: 0,
        ordinals: BTreeMap::new(),
        scope: Vec::new(),
        gen_env: Vec::new(),
    };
    let t = gen_expr(&mut g, root);
    let root_ty = match t { Ty::Var(v) => v, _ => unreachable!() };
    GenOut {
        constraints: g.constraints,
        node_ty: g.node_ty,
        root_ty,
        next_var: g.next_var,
    }
}

fn gen_expr(g: &mut Gen, n: &Node) -> Ty {
    let me = g.ty_of(n);
    match &n.kind {
        ExprK::Int(_) => {
            g.push(n, CKind::Unify { a: me.clone(), b: Ty::Int }, "integer literal".into());
        }
        ExprK::Bool(_) => {
            g.push(n, CKind::Unify { a: me.clone(), b: Ty::Bool }, "boolean literal".into());
        }
        ExprK::Var(name) => {
            g.push(
                n,
                CKind::Inst { name: name.clone(), expect: me.clone() },
                format!("use of variable `{}`", name),
            );
        }
        ExprK::Fun { param, body } => {
            let pt = Ty::Var(g.fresh());
            g.push(
                n,
                CKind::BindMono { name: param.clone(), ty: pt.clone() },
                format!("parameter `{}`", param),
            );
            let bt = g.scoped_ret("fun", |g| gen_expr(g, body));
            g.push(
                n,
                CKind::Unbind { name: param.clone() },
                format!("leave scope of parameter `{}`", param),
            );
            g.push(
                n,
                CKind::Unify { a: me.clone(), b: Ty::Fun(Box::new(pt), Box::new(bt)) },
                format!("function `{}`", param),
            );
        }
        ExprK::App(f, a) => {
            let ft = gen_expr(g, f);
            let at = gen_expr(g, a);
            let rt = Ty::Var(g.fresh());
            g.push(
                n,
                CKind::Unify { a: ft, b: Ty::Fun(Box::new(at), Box::new(rt.clone())) },
                "application: function must have arrow type".into(),
            );
            g.push(
                n,
                CKind::Unify { a: me.clone(), b: rt },
                "application result".into(),
            );
        }
        ExprK::Let { name, rec_, value, body } => {
            if *rec_ {
                let ft = Ty::Var(g.fresh());
                g.push(
                    n,
                    CKind::BindMono { name: name.clone(), ty: ft.clone() },
                    format!("recursive binding `{}`", name),
                );
                let vt = g.scoped_ret("letrec", |g| gen_expr(g, value));
                g.push(
                    n,
                    CKind::Unify { a: ft.clone(), b: vt },
                    format!("recursive binding `{}` self-reference", name),
                );
                g.push(
                    n,
                    CKind::Unbind { name: name.clone() },
                    format!("leave recursive scope of `{}`", name),
                );
                g.push(
                    n,
                    CKind::Gen { name: name.clone(), ty: ft, expansive: true },
                    format!("generalize recursive binding `{}`", name),
                );
            } else {
                let vt = gen_expr(g, value);
                g.push(
                    n,
                    CKind::Gen { name: name.clone(), ty: vt, expansive: expansive(value) },
                    format!("generalize let binding `{}`", name),
                );
            }
            g.gen_env.push(name.clone());
            let bt = g.scoped_ret("let", |g| gen_expr(g, body));
            g.gen_env.pop();
            g.push(
                n,
                CKind::Unbind { name: name.clone() },
                format!("leave scope of let binding `{}`", name),
            );
            g.push(
                n,
                CKind::Unify { a: me.clone(), b: bt },
                format!("let body of `{}`", name),
            );
        }
        ExprK::If(c, t, e) => {
            let ct = gen_expr(g, c);
            g.push(
                n,
                CKind::Unify { a: ct, b: Ty::Bool },
                "if condition must be Bool".into(),
            );
            let tt = gen_expr(g, t);
            let et = gen_expr(g, e);
            g.push(
                n,
                CKind::Unify { a: tt.clone(), b: et },
                "if branches must agree".into(),
            );
            g.push(
                n,
                CKind::Unify { a: me.clone(), b: tt },
                "if result".into(),
            );
        }
        ExprK::Record(fields) => {
            let mut row = BTreeMap::new();
            for (label, val) in fields {
                let vt = gen_expr(g, val);
                row.insert(label.clone(), vt);
            }
            g.push(
                n,
                CKind::Unify { a: me.clone(), b: Ty::Record(Row::closed(row)) },
                "record literal".into(),
            );
        }
        ExprK::Proj(e, label) => {
            let et = gen_expr(g, e);
            let tail = g.fresh();
            let mut row = BTreeMap::new();
            row.insert(label.clone(), me.clone());
            g.push(
                n,
                CKind::Unify {
                    a: et,
                    b: Ty::Record(Row { fields: row, tail: Some(tail) }),
                },
                format!("projection `.{}` requires a record with that field", label),
            );
        }
        ExprK::Variant(label, payload) => {
            let pt = gen_expr(g, payload);
            let tail = g.fresh();
            let mut row = BTreeMap::new();
            row.insert(label.clone(), pt);
            g.push(
                n,
                CKind::Unify {
                    a: me.clone(),
                    b: Ty::Variant(Row { fields: row, tail: Some(tail) }),
                },
                format!("variant constructor `{}`", label),
            );
        }
        ExprK::Match(scrut, arms) => {
            let st = gen_expr(g, scrut);
            let tail = g.fresh();
            let mut row = BTreeMap::new();
            let mut binder_tys = Vec::new();
            for (label, binder, _) in arms {
                let bt = Ty::Var(g.fresh());
                row.insert(label.clone(), bt.clone());
                binder_tys.push((label.clone(), binder.clone(), bt));
            }
            g.push(
                n,
                CKind::Unify {
                    a: st,
                    b: Ty::Variant(Row { fields: row, tail: Some(tail) }),
                },
                "match scrutinee must be a variant covering the arms".into(),
            );
            for ((label, binder, body), (_, _, bt)) in arms.iter().zip(binder_tys.iter()) {
                g.push(
                    n,
                    CKind::BindMono { name: binder.clone(), ty: bt.clone() },
                    format!("match arm `{}` binder `{}`", label, binder),
                );
                let arm_t = g.scoped_ret(&format!("arm:`{}", label), |g| gen_expr(g, body));
                g.push(
                    n,
                    CKind::Unbind { name: binder.clone() },
                    format!("leave scope of match binder `{}`", binder),
                );
                g.push(
                    n,
                    CKind::Unify { a: me.clone(), b: arm_t },
                    format!("match arm `{}` result", label),
                );
            }
        }
    }
    me
}
