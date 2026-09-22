use crate::syntax::{BinOp, Expr, Span};
use crate::typing::*;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

pub type CId = String;

#[derive(Clone, Debug, Serialize)]
pub struct Constraint {
    pub id: CId,
    pub kind: String,
    pub detail: String,
    pub node: String,
    pub span: Span,
    pub cache_key: String,
    pub scope_fp: String,
    pub gen_fp: String,
    pub status: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct AstNodeInfo {
    pub path: String,
    pub kind: String,
    pub label: String,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Step {
    FreshVar { at: usize, node: String, name: String, reason: String, span: Span },
    Unify {
        at: usize,
        cid: CId,
        left: String,
        right: String,
        result: String,
        reused: bool,
        span: Span,
    },
    Instantiate {
        at: usize,
        node: String,
        name: String,
        scheme: String,
        fresh: Vec<String>,
        span: Span,
    },
    Generalize {
        at: usize,
        node: String,
        name: String,
        vars: Vec<String>,
        monomorphic: bool,
        value_restricted: bool,
        span: Span,
    },
    Record { at: usize, node: String, fields: Vec<String>, span: Span },
    Variant { at: usize, node: String, label: String, span: Span },
    Error { at: usize, cid: Option<CId>, message: String, span: Span },
}

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Conflict {
    Occurs {
        var: String,
        ty: String,
        span: Span,
        cid: CId,
        proof: BTreeSet<CId>,
    },
    ConMismatch {
        expected: String,
        actual: String,
        span: Span,
        cid: CId,
        proof: BTreeSet<CId>,
    },
    RowMismatch {
        shape: String,
        missing: Vec<String>,
        extra: Vec<String>,
        field_clash: Option<(String, String, String)>,
        span: Span,
        cid: CId,
        proof: BTreeSet<CId>,
    },
    Unbound { name: String, span: Span },
}

impl Conflict {
    pub fn span(&self) -> Span {
        match self {
            Conflict::Occurs { span, .. }
            | Conflict::ConMismatch { span, .. }
            | Conflict::RowMismatch { span, .. }
            | Conflict::Unbound { span, .. } => *span,
        }
    }
    pub fn cid(&self) -> Option<&CId> {
        match self {
            Conflict::Occurs { cid, .. }
            | Conflict::ConMismatch { cid, .. }
            | Conflict::RowMismatch { cid, .. } => Some(cid),
            Conflict::Unbound { .. } => None,
        }
    }
    pub fn proof(&self) -> BTreeSet<CId> {
        match self {
            Conflict::Occurs { proof, .. }
            | Conflict::ConMismatch { proof, .. }
            | Conflict::RowMismatch { proof, .. } => proof.clone(),
            Conflict::Unbound { .. } => BTreeSet::new(),
        }
    }
    pub fn message(&self) -> String {
        match self {
            Conflict::Occurs { var, ty, .. } => {
                format!("发生 occurs-check：变量 {var} 出现在类型 {ty} 中，会构成无限类型")
            }
            Conflict::ConMismatch { expected, actual, .. } => {
                format!("类型构造不一致：期望 {expected}，实际得到 {actual}")
            }
            Conflict::RowMismatch { shape, missing, extra, field_clash, .. } => {
                let mut parts = vec![format!("行类型不匹配（{shape}）")];
                if let Some((f, a, b)) = field_clash {
                    parts.push(format!("字段 {f} 类型冲突：{a} 与 {b}"));
                }
                if !missing.is_empty() {
                    parts.push(format!("缺少字段：{}", missing.join(", ")));
                }
                if !extra.is_empty() {
                    parts.push(format!("多余字段：{}", extra.join(", ")));
                }
                parts.join("；")
            }
            Conflict::Unbound { name, .. } => format!("未绑定的标识符：{name}"),
        }
    }
}

#[derive(Clone)]
struct Binding {
    scheme: Scheme,
    depth: u32,
    gen_seq: u32,
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub vars: BTreeSet<Var>,
    pub body: Ty,
}

impl Scheme {
    pub fn monomorphic(body: Ty) -> Self {
        Self { vars: BTreeSet::new(), body }
    }
    pub fn show(&self) -> String {
        let resolved = resolve_static(&self.body);
        let mut r = Renamer::new();
        if self.vars.is_empty() {
            r.show(&resolved)
        } else {
            let names: Vec<String> = {
                let mut fv = BTreeSet::new();
                free_vars(&resolved, &mut fv);
                fv.iter().map(|v| r.name(v)).collect()
            };
            format!("∀{}. {}", names.join(" "), r.show(&resolved))
        }
    }
}

fn resolve_static(ty: &Ty) -> Ty {
    ty.clone()
}

pub struct Inferer<'a> {
    uf: &'a Uf,
    env: Vec<(String, Binding)>,
    depth: u32,
    gen_seq: u32,
    pub constraints: Vec<Constraint>,
    pub steps: Vec<Step>,
    pub nodes: Vec<AstNodeInfo>,
    pub conflicts: Vec<Conflict>,
    reuse: BTreeSet<CId>,
    tick: usize,
    var_origin: BTreeMap<Var, VarOrigin>,
}

impl<'a> Inferer<'a> {
    pub fn new(uf: &'a Uf) -> Self {
        Inferer {
            uf,
            env: Vec::new(),
            depth: 0,
            gen_seq: 0,
            constraints: Vec::new(),
            steps: Vec::new(),
            nodes: Vec::new(),
            conflicts: Vec::new(),
            reuse: BTreeSet::new(),
            tick: 0,
            var_origin: BTreeMap::new(),
        }
    }

    pub fn with_reuse(uf: &'a Uf, reuse: BTreeSet<CId>) -> Self {
        let mut s = Self::new(uf);
        s.reuse = reuse;
        s
    }

    fn at(&mut self) -> usize {
        let t = self.tick;
        self.tick += 1;
        t
    }

    fn fresh(&mut self, node: &str, reason: &str, span: Span) -> Var {
        let v = self.uf.fresh();
        let at = self.at();
        self.steps.push(Step::FreshVar {
            at,
            node: node.into(),
            name: format!("t{}", v.0),
            reason: reason.into(),
            span,
        });
        v
    }

    fn lookup(&self, name: &str) -> Option<&Binding> {
        self.env.iter().rev().find(|(n, _)| n == name).map(|(_, b)| b)
    }

    fn scope_fp(&self, path: &str) -> String {
        let live: Vec<String> = self
            .env
            .iter()
            .map(|(n, b)| format!("{}@d{}/g{}", n, b.depth, b.gen_seq))
            .collect();
        format!("{path}|{}", live.join(","))
    }

    fn gen_fp(&self) -> String {
        let mut g: Vec<String> = self
            .env
            .iter()
            .filter(|(_, b)| !b.scheme.vars.is_empty())
            .map(|(n, b)| format!("{}:g{}", n, b.gen_seq))
            .collect();
        g.sort();
        g.join(";")
    }

    fn push_constraint(
        &mut self,
        id: String,
        kind: &str,
        detail: String,
        node: String,
        span: Span,
    ) {
        let scope_fp = self.scope_fp(&node);
        let gen_fp = self.gen_fp();
        let cache_key = format!("{kind}|{id}|scope={scope_fp}|gen={gen_fp}");
        let reused = self.reuse.contains(&id);
        self.constraints.push(Constraint {
            id,
            kind: kind.into(),
            detail,
            node,
            span,
            cache_key,
            scope_fp,
            gen_fp,
            status: if reused { "reused" } else { "new" }.into(),
        });
    }
}

#[derive(Default, Clone, Debug)]
struct VarOrigin {
    /// cids whose constraint produced / shaped this variable
    cids: BTreeSet<CId>,
}

impl<'a> Inferer<'a> {
    fn ty_proof(&self, ty: &Ty, extra: &BTreeSet<CId>) -> BTreeSet<CId> {
        let mut fv = BTreeSet::new();
        free_vars(&resolve(self.uf, ty), &mut fv);
        let mut proof = BTreeSet::new();
        for v in fv {
            if let Some(o) = self.var_origin.get(&v) {
                proof.extend(o.cids.iter().cloned());
            }
        }
        proof.extend(extra.iter().cloned());
        proof
    }

    fn mark_var(&mut self, v: &Var, cid: &CId) {
        self.var_origin
            .entry(self.uf.find(v))
            .or_default()
            .cids
            .insert(cid.clone());
    }

    fn mark_ty(&mut self, ty: &Ty, cid: &CId) {
        let mut fv = BTreeSet::new();
        free_vars(ty, &mut fv);
        for v in fv {
            self.mark_var(&v, cid);
        }
    }

    fn merge_origins(&mut self, a: &Var, b: &Var) {
        let ra = self.uf.find(a);
        let rb = self.uf.find(b);
        if ra == rb {
            return;
        }
        let take = self.var_origin.get(&rb).cloned();
        if let Some(o) = take {
            self.var_origin.entry(ra).or_default().cids.extend(o.cids);
            self.var_origin.remove(&rb);
        }
    }

    /// Core unification. Records a step on success and a conflict on failure.
    pub fn unify(&mut self, a: &Ty, b: &Ty, cid: CId, span: Span) -> bool {
        let a0 = resolve(self.uf, a);
        let b0 = resolve(self.uf, b);
        let left = show_ty(&a0);
        let right = show_ty(&b0);
        let reused = self.reuse.contains(&cid);

        let res = self.unify_inner(&a0, &b0, &cid, span);
        let resolved = resolve(self.uf, a);
        let result = show_ty(&resolved);
        let at = self.at();
        self.steps.push(Step::Unify {
            at,
            cid: cid.clone(),
            left,
            right,
            result,
            reused,
            span,
        });
        if let Err(c) = res {
            let at = self.at();
            self.steps.push(Step::Error {
                at,
                cid: Some(cid),
                message: c.message(),
                span,
            });
            self.conflicts.push(c);
            false
        } else {
            true
        }
    }

    fn unify_inner(&mut self, a: &Ty, b: &Ty, cid: &CId, span: Span) -> Result<(), Conflict> {
        match (a, b) {
            (Ty::Var(x), Ty::Var(y)) => {
                let rx = self.uf.find(x);
                let ry = self.uf.find(y);
                if rx != ry {
                    self.merge_origins(&rx, &ry);
                    self.uf.union(&rx, &ry);
                    self.mark_var(&rx, cid);
                }
                Ok(())
            }
            (Ty::Var(v), other) => self.unify_var_ty(v, other, true, cid, span),
            (other, Ty::Var(v)) => self.unify_var_ty(v, other, false, cid, span),
            (Ty::Con(n1, a1), Ty::Con(n2, a2)) => {
                if n1 != n2 || a1.len() != a2.len() {
                    let proof = {
                        let mut p = self.ty_proof(a, &BTreeSet::new());
                        p.extend(self.ty_proof(b, &BTreeSet::new()));
                        p.insert(cid.clone());
                        p
                    };
                    return Err(Conflict::ConMismatch {
                        expected: show_ty(a),
                        actual: show_ty(b),
                        span,
                        cid: cid.clone(),
                        proof,
                    });
                }
                for (x, y) in a1.iter().zip(a2.iter()) {
                    self.unify_inner(x, y, cid, span)?;
                }
                Ok(())
            }
            (Ty::Record(r1), Ty::Record(r2)) => self.unify_rows("record", r1, r2, cid, span),
            (Ty::Variant(r1), Ty::Variant(r2)) => self.unify_rows("variant", r1, r2, cid, span),
            _ => Err(Conflict::ConMismatch {
                expected: show_ty(a),
                actual: show_ty(b),
                span,
                cid: cid.clone(),
                proof: {
                    let mut p = self.ty_proof(a, &BTreeSet::new());
                    p.extend(self.ty_proof(b, &BTreeSet::new()));
                    p.insert(cid.clone());
                    p
                },
            }),
        }
    }

    #[allow(clippy::nonminimal_bool)]
    fn unify_var_ty(
        &mut self,
        v: &Var,
        other: &Ty,
        var_is_expected: bool,
        cid: &CId,
        span: Span,
    ) -> Result<(), Conflict> {
        let rv = self.uf.find(v);
        if occurs(self.uf, &rv, other) {
            let proof = self.ty_proof(other, &BTreeSet::from([cid.clone()]));
            return Err(Conflict::Occurs {
                var: show_ty(&Ty::Var(rv)),
                ty: show_ty(&resolve(self.uf, other)),
                span,
                cid: cid.clone(),
                proof,
            });
        }
        let shape = match other {
            Ty::Record(_) => Some("record"),
            Ty::Variant(_) => Some("variant"),
            _ => None,
        };
        if let Some(shape) = shape {
            let var_row = Row::empty_open(rv);
            let res = if var_is_expected {
                let other_row = match other {
                    Ty::Record(r) | Ty::Variant(r) => r.clone(),
                    _ => unreachable!(),
                };
                self.unify_rows(shape, &var_row, &other_row, cid, span)
            } else {
                let other_row = match other {
                    Ty::Record(r) | Ty::Variant(r) => r.clone(),
                    _ => unreachable!(),
                };
                self.unify_rows(shape, &other_row, &var_row, cid, span)
            };
            return res;
        }
        self.uf.set_desc(&rv, other.clone());
        self.mark_ty(other, cid);
        self.mark_var(&rv, cid);
        Ok(())
    }

    fn unify_rows(
        &mut self,
        shape: &str,
        exp: &Row,
        act: &Row,
        cid: &CId,
        span: Span,
    ) -> Result<(), Conflict> {
        // Directional: exp is the expected row (left), act the actual row (right).
        let missing: Vec<String> = exp
            .fields
            .keys()
            .filter(|k| !act.fields.contains_key(*k))
            .cloned()
            .collect();
        let extra: Vec<String> = act
            .fields
            .keys()
            .filter(|k| !exp.fields.contains_key(*k))
            .cloned()
            .collect();

        let exp_closed = exp.ext.is_none();
        let act_closed = act.ext.is_none();

        // `missing` are fields expected but absent on the actual side.
        // They are only fatal when the actual side cannot absorb them via
        // its extension variable; a closed actual row can never gain fields.
        let true_missing: Vec<String> = if act.ext.is_none() {
            missing.clone()
        } else {
            // An open actual row can absorb any missing field via its tail.
            Vec::new()
        };
        if !true_missing.is_empty() {
            return Err(self.row_conflict(shape, &true_missing, &[], None, exp, act, cid, span));
        }
        // A closed expected row refuses fields present only on the actual side.
        if exp_closed && !extra.is_empty() {
            return Err(self.row_conflict(shape, &[], &extra, None, exp, act, cid, span));
        }

        // Unify the types of shared fields first, recording clashes distinctly.
        for k in exp.fields.keys() {
            if let Some(ta) = act.fields.get(k) {
                let te = &exp.fields[k];
                if let Err(c) = self.unify_inner(te, ta, cid, span) {
                    let proof = match c {
                        Conflict::Occurs { proof, .. }
                        | Conflict::ConMismatch { proof, .. }
                        | Conflict::RowMismatch { proof, .. } => proof,
                        Conflict::Unbound { .. } => BTreeSet::new(),
                    };
                    return Err(Conflict::RowMismatch {
                        shape: shape.into(),
                        missing: vec![],
                        extra: vec![],
                        field_clash: Some((
                            k.clone(),
                            show_ty(&resolve(self.uf, te)),
                            show_ty(&resolve(self.uf, ta)),
                        )),
                        span,
                        cid: cid.clone(),
                        proof,
                    });
                }
            }
        }

        match (&exp.ext, &act.ext) {
            (Some(ee), Some(ae)) => {
                // Merge the two extension variables; any asymmetric fields have
                // already been accepted above (open rows absorb them).
                self.unify_inner(&Ty::Var(*ee), &Ty::Var(*ae), cid, span)?;
            }
            (Some(ee), None) => {
                // Actual is closed: the expected extension variable describes
                // precisely the fields that appear only on the expected side.
                let mut tail_fields = BTreeMap::new();
                for k in &extra {
                    tail_fields.insert(k.clone(), act.fields[k].clone());
                }
                let root = self.uf.find(ee);
                let tail_ty = if shape == "variant" {
                    Ty::Variant(Row::closed(tail_fields))
                } else {
                    Ty::Record(Row::closed(tail_fields))
                };
                self.uf.set_desc(&root, tail_ty);
                self.mark_var(&root, cid);
            }
            (None, Some(ae)) => {
                // Expected is closed: actual extension absorbs expected-only fields.
                let mut tail_fields = BTreeMap::new();
                for k in &missing {
                    tail_fields.insert(k.clone(), exp.fields[k].clone());
                }
                let root = self.uf.find(ae);
                let tail_ty = if shape == "variant" {
                    Ty::Variant(Row::closed(tail_fields))
                } else {
                    Ty::Record(Row::closed(tail_fields))
                };
                self.uf.set_desc(&root, tail_ty);
                self.mark_var(&root, cid);
            }
            (None, None) => {}
        }
        Ok(())
    }

    fn row_conflict(
        &self,
        shape: &str,
        missing: &[String],
        extra: &[String],
        field_clash: Option<(String, String, String)>,
        r1: &Row,
        r2: &Row,
        cid: &CId,
        span: Span,
    ) -> Conflict {
        let mut proof = BTreeSet::from([cid.clone()]);
        for t in r1.fields.values().chain(r2.fields.values()) {
            let mut fv = BTreeSet::new();
            free_vars(&resolve(self.uf, t), &mut fv);
            for v in fv {
                if let Some(o) = self.var_origin.get(&v) {
                    proof.extend(o.cids.iter().cloned());
                }
            }
        }
        Conflict::RowMismatch {
            shape: shape.into(),
            missing: missing.to_vec(),
            extra: extra.to_vec(),
            field_clash,
            span,
            cid: cid.clone(),
            proof,
        }
    }
}

fn is_value(e: &Expr) -> bool {
    matches!(
        e,
        Expr::Int(..)
            | Expr::Bool(..)
            | Expr::Str(..)
            | Expr::Fun { .. }
            | Expr::Record { .. }
            | Expr::Variant { .. }
    )
}

impl<'a> Inferer<'a> {
    fn generalize(
        &mut self,
        name: &str,
        ty: &Ty,
        node: &str,
        span: Span,
        value_restricted: bool,
    ) -> Scheme {
        let resolved = resolve(self.uf, ty);
        let mut fv = BTreeSet::new();
        free_vars(&resolved, &mut fv);
        let mut ambient = BTreeSet::new();
        for (_, b) in &self.env {
            for v in &b.scheme.vars {
                ambient.insert(self.uf.find(v));
            }
            free_vars(&resolve(self.uf, &b.scheme.body), &mut ambient);
        }
        let vars: BTreeSet<Var> = fv.iter().filter(|v| !ambient.contains(v)).cloned().collect();
        let monomorphic = value_restricted || vars.is_empty();
        let shown: Vec<String> = if monomorphic {
            vec![]
        } else {
            let mut r = Renamer::new();
            let mut vs: Vec<Var> = vars.iter().cloned().collect();
            vs.sort();
            vs.iter().map(|v| r.name(v)).collect()
        };
        let at = self.at();
        self.steps.push(Step::Generalize {
            at,
            node: node.into(),
            name: name.into(),
            vars: shown,
            monomorphic,
            value_restricted,
            span,
        });
        if monomorphic {
            Scheme::monomorphic(resolved)
        } else {
            Scheme { vars, body: resolved }
        }
    }

    fn instantiate(&mut self, scheme: &Scheme, node: &str, name: &str, span: Span) -> Ty {
        if scheme.vars.is_empty() {
            return scheme.body.clone();
        }
        let mut mapping: BTreeMap<Var, Ty> = BTreeMap::new();
        let mut fresh_names = Vec::new();
        let mut vs: Vec<Var> = scheme.vars.iter().cloned().collect();
        vs.sort();
        for v in vs {
            let nv = self.fresh(node, &format!("实例化 {name} 的泛型变量"), span);
            fresh_names.push(format!("t{}", nv.0));
            mapping.insert(v, Ty::Var(nv));
        }
        let body = subst(&scheme.body, &mapping);
        let at = self.at();
        self.steps.push(Step::Instantiate {
            at,
            node: node.into(),
            name: name.into(),
            scheme: scheme.show(),
            fresh: fresh_names,
            span,
        });
        body
    }

    fn register_node(&mut self, path: &str, kind: &str, label: String, span: Span) {
        self.nodes.push(AstNodeInfo {
            path: path.into(),
            kind: kind.into(),
            label,
            span,
        });
    }

    pub fn infer_program(&mut self, e: &Expr) -> (Ty, Vec<Conflict>) {
        let t = self.walk(e, "root");
        let final_ty = resolve(self.uf, &t);
        (final_ty, self.conflicts.clone())
    }

    fn walk(&mut self, e: &Expr, path: &str) -> Ty {
        match e {
            Expr::Let { .. }
            | Expr::Record { .. }
            | Expr::Proj { .. }
            | Expr::Variant { .. }
            | Expr::Match { .. } => self.walk_full(e, path),
            Expr::Int(n, s) => {
                self.register_node(path, "int", n.to_string(), *s);
                t_int()
            }
            Expr::Bool(b, s) => {
                self.register_node(path, "bool", b.to_string(), *s);
                t_bool()
            }
            Expr::Str(x, s) => {
                self.register_node(path, "str", x.clone(), *s);
                t_str()
            }
            Expr::Var(name, s) => {
                self.register_node(path, "var", name.clone(), *s);
                match self.lookup(name).cloned() {
                    Some(b) => {
                        let cid = format!("inst:{path}");
                        let ty = self.instantiate(&b.scheme, path, name, *s);
                        self.push_constraint(
                            cid,
                            "instantiate",
                            format!("使用 {name} : {}", b.scheme.show()),
                            path.into(),
                            *s,
                        );
                        let cid = self.constraints.last().unwrap().id.clone();
                        self.mark_ty(&ty, &cid);
                        ty
                    }
                    None => {
                        let fallback = self.fresh(path, "未绑定标识符的占位", *s);
                        self.conflicts.push(Conflict::Unbound { name: name.clone(), span: *s });
                        let at = self.at();
                        self.steps.push(Step::Error {
                            at,
                            cid: None,
                            message: format!("未绑定的标识符：{name}"),
                            span: *s,
                        });
                        Ty::Var(fallback)
                    }
                }
            }
            Expr::Fun { param, body, span } => {
                self.register_node(path, "fun", format!("fun {param}"), *span);
                let pv = self.fresh(path, &format!("参数 {param}"), *span);
                let cid = format!("param:{path}");
                self.push_constraint(
                    cid.clone(),
                    "param",
                    format!("参数 {param} 引入类型变量"),
                    path.into(),
                    *span,
                );
                self.mark_var(&pv, &cid);
                self.env.push((
                    param.clone(),
                    Binding {
                        scheme: Scheme::monomorphic(Ty::Var(pv.clone())),
                        depth: self.depth,
                        gen_seq: 0,
                    },
                ));
                let bt = self.walk(body, &format!("{path}/b"));
                self.env.pop();
                t_fun(Ty::Var(pv), bt)
            }
            Expr::App { fun, arg, span } => {
                self.register_node(path, "app", "应用".into(), *span);
                let ft = self.walk(fun, &format!("{path}/f"));
                let at = self.walk(arg, &format!("{path}/a"));
                let rv = self.fresh(path, "应用结果", *span);
                let cid = format!("app:{path}");
                self.push_constraint(
                    cid.clone(),
                    "unify",
                    format!("函数类型须为 参数 -> 结果"),
                    path.into(),
                    *span,
                );
                let expect = t_fun(at, Ty::Var(rv));
                let ok = self.unify(&ft, &expect, cid, *span);
                let _ = ok;
                Ty::Var(rv)
            }
            Expr::Bin { op, lhs, rhs, span } => {
                let name = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Eq => "==",
                };
                self.register_node(path, "bin", name.into(), *span);
                let lt = self.walk(lhs, &format!("{path}/l"));
                let rt = self.walk(rhs, &format!("{path}/r"));
                let (res_ty, cid) = match op {
                    BinOp::Add | BinOp::Sub => {
                        let c = format!("arith:{path}");
                        (t_int(), c)
                    }
                    BinOp::Eq => {
                        let c = format!("eq:{path}");
                        (t_bool(), c)
                    }
                };
                self.push_constraint(
                    cid.clone(),
                    "unify",
                    format!("{name} 的两侧类型须一致"),
                    path.into(),
                    *span,
                );
                self.unify(&lt, &rt, cid.clone(), *span);
                let operand = match op {
                    BinOp::Add | BinOp::Sub => t_int(),
                    BinOp::Eq => lt.clone(),
                };
                let cid2 = format!("optype:{path}");
                self.push_constraint(
                    cid2.clone(),
                    "unify",
                    format!("{name} 的操作数类型约束"),
                    path.into(),
                    *span,
                );
                self.unify(&lt, &operand, cid2, *span);
                res_ty
            }
        }
    }
}

fn subst(ty: &Ty, map: &BTreeMap<Var, Ty>) -> Ty {
    match ty {
        Ty::Var(v) => map.get(v).cloned().unwrap_or_else(|| Ty::Var(v.clone())),
        Ty::Con(n, args) => Ty::Con(n.clone(), args.iter().map(|a| subst(a, map)).collect()),
        Ty::Record(r) => Ty::Record(subst_row(r, map)),
        Ty::Variant(r) => Ty::Variant(subst_row(r, map)),
    }
}

fn subst_row(r: &Row, map: &BTreeMap<Var, Ty>) -> Row {
    Row {
        fields: r.fields.iter().map(|(k, v)| (k.clone(), subst(v, map))).collect(),
        ext: r.ext.clone(),
    }
}

impl<'a> Inferer<'a> {
    #[allow(clippy::too_many_lines)]
    fn walk_full(&mut self, e: &Expr, path: &str) -> Ty {
        match e {
            Expr::Let { name, rec, value, body, span } => {
                self.register_node(path, "let", format!("let {name}"), *span);
                if *rec {
                    self.gen_seq += 1;
                    let vv = self.fresh(path, &format!("递归绑定 {name} 的类型"), *span);
                    let cid = format!("rec:{path}");
                    self.push_constraint(
                        cid.clone(),
                        "rec",
                        format!("递归绑定 {name}"),
                        path.into(),
                        *span,
                    );
                    self.mark_var(&vv, &cid);
                    self.env.push((
                        name.clone(),
                        Binding {
                            scheme: Scheme::monomorphic(Ty::Var(vv.clone())),
                            depth: self.depth,
                            gen_seq: self.gen_seq,
                        },
                    ));
                    let vt = self.walk(value, &format!("{path}/v"));
                    self.unify(
                        &Ty::Var(vv.clone()),
                        &vt,
                        format!("recunify:{path}"),
                        *span,
                    );
                    let resolved = resolve(self.uf, &Ty::Var(vv));
                    let scheme = Scheme::monomorphic(resolved);
                    if let Some(slot) =
                        self.env.iter_mut().rev().find(|(n, _)| n == name)
                    {
                        slot.1.scheme = scheme;
                    }
                    let bt = self.walk(body, &format!("{path}/body"));
                    self.env.pop();
                    bt
                } else {
                    let vt = self.walk(value, &format!("{path}/v"));
                    let vr = !is_value(value);
                    let scheme =
                        self.generalize(name, &vt, path, *span, vr);
                    self.gen_seq += 1;
                    self.env.push((
                        name.clone(),
                        Binding { scheme, depth: self.depth, gen_seq: self.gen_seq },
                    ));
                    let bt = self.walk(body, &format!("{path}/body"));
                    self.env.pop();
                    bt
                }
            }
            Expr::Record { fields, span } => {
                self.register_node(path, "record", "{ ... }".into(), *span);
                let mut map = BTreeMap::new();
                let mut names = Vec::new();
                for (i, (k, vexpr)) in fields.iter().enumerate() {
                    let vt = self.walk(vexpr, &format!("{path}/f{i}"));
                    map.insert(k.clone(), vt);
                    names.push(k.clone());
                    let cid = format!("field:{path}/{k}");
                    self.push_constraint(
                        cid,
                        "field",
                        format!("记录字段 {k}"),
                        format!("{path}/f{i}"),
                        vexpr.span(),
                    );
                }
                let at = self.at();
                self.steps.push(Step::Record {
                    at,
                    node: path.into(),
                    fields: names,
                    span: *span,
                });
                Ty::Record(Row::closed(map))
            }
            Expr::Proj { obj, field, span } => {
                self.register_node(path, "proj", format!(".{field}"), *span);
                let ot = self.walk(obj, &format!("{path}/o"));
                let rv = self.fresh(path, &format!("字段 {field} 的结果"), *span);
                let mut m = BTreeMap::new();
                m.insert(field.clone(), Ty::Var(rv.clone()));
                let ext = self.fresh(path, "记录其余字段", *span);
                let expected = Ty::Record(Row::open(m, ext));
                let cid = format!("proj:{path}");
                self.push_constraint(
                    cid.clone(),
                    "unify",
                    format!("投影 .{field}：对象须含该字段"),
                    path.into(),
                    *span,
                );
                self.unify(&ot, &expected, cid, *span);
                Ty::Var(rv)
            }
            Expr::Variant { label, payload, span } => {
                self.register_node(path, "variant", format!("#{label}"), *span);
                let inner = if let Some(pe) = payload {
                    self.walk(pe, &format!("{path}/p"))
                } else {
                    Ty::Con("()".into(), vec![])
                };
                let ext = self.fresh(path, "变体其余标签", *span);
                let mut m = BTreeMap::new();
                m.insert(label.clone(), inner);
                let at = self.at();
                self.steps.push(Step::Variant {
                    at,
                    node: path.into(),
                    label: label.clone(),
                    span: *span,
                });
                Ty::Variant(Row::open(m, ext))
            }
            Expr::Match { scrut, arms, span } => {
                self.register_node(path, "match", "match".into(), *span);
                let st = self.walk(scrut, &format!("{path}/s"));
                let result = self.fresh(path, "match 结果", *span);
                let mut field_map: BTreeMap<String, Ty> = BTreeMap::new();
                let mut binders: Vec<(String, Option<String>)> = Vec::new();
                for (i, (label, binder, body_expr)) in arms.iter().enumerate() {
                    let pv = binder.as_ref().map(|bn| {
                        let v = self.fresh(
                            &format!("{path}/a{i}"),
                            &format!("分支 {label} 载荷"),
                            body_expr.span(),
                        );
                        v
                    });
                    let payload_ty = pv
                        .clone()
                        .map(Ty::Var)
                        .unwrap_or_else(|| Ty::Con("()".into(), vec![]));
                    field_map.insert(label.clone(), payload_ty.clone());
                    binders.push((label.clone(), binder.clone()));
                    if let (Some(bn), Some(pv)) = (binder, pv) {
                        let cid = format!("binder:{path}/a{i}");
                        self.push_constraint(
                            cid.clone(),
                            "binder",
                            format!("分支绑定 {bn}"),
                            format!("{path}/a{i}"),
                            body_expr.span(),
                        );
                        self.mark_var(&pv, &cid);
                        self.env.push((
                            bn.clone(),
                            Binding {
                                scheme: Scheme::monomorphic(Ty::Var(pv)),
                                depth: self.depth,
                                gen_seq: 0,
                            },
                        ));
                    }
                    let bt = self.walk(body_expr, &format!("{path}/a{i}"));
                    if binder.is_some() {
                        self.env.pop();
                    }
                    let cid = format!("arm:{path}/a{i}");
                    self.push_constraint(
                        cid.clone(),
                        "unify",
                        format!("分支 #{label} 的结果类型一致"),
                        format!("{path}/a{i}"),
                        body_expr.span(),
                    );
                    self.unify(&bt, &Ty::Var(result.clone()), cid, body_expr.span());
                }
                let row_ext = self.fresh(path, "变体未覆盖标签", *span);
                let expected = Ty::Variant(Row::open(field_map, row_ext));
                let cid = format!("match:{path}");
                self.push_constraint(
                    cid.clone(),
                    "unify",
                    "被匹配值须包含这些变体标签".into(),
                    path.into(),
                    *span,
                );
                self.unify(&st, &expected, cid, *span);
                Ty::Var(result)
            }
            other => self.walk_leaf(other, path),
        }
    }

    fn walk_leaf(&mut self, e: &Expr, path: &str) -> Ty {
        match e {
            Expr::Int(..)
            | Expr::Bool(..)
            | Expr::Str(..)
            | Expr::Var(..)
            | Expr::Fun { .. }
            | Expr::App { .. }
            | Expr::Bin { .. } => self.walk(e, path),
            _ => unreachable!("walk_leaf 只处理叶子类节点"),
        }
    }
}
