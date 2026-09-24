//! Constraint-generating Hindley-Milner inference with provenance.
//!
//! Generation and solving are interleaved (algorithm W order) so every
//! unification, instantiation and generalization becomes a scrubbable step.
//! Record and variant annotations introduce explicit row-restriction
//! constraints; inference without annotations leaves open rows.

use std::collections::{HashMap, HashSet};

use serde::Serialize;

use crate::constraints::*;
use crate::syntax::*;
use crate::types::*;

#[derive(Debug, Clone, Default)]
pub struct AnalyzeOptions {
    /// Equality constraint ids temporarily disabled by the user.
    pub disabled: HashSet<Cid>,
    /// Stable name seed (used when analyzing edited versions side by side).
    pub run_label: String,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct NodeBrief {
    pub id: NodeId,
    pub kind: String,
    pub label: String,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct ScopeEntry {
    pub name: String,
    pub scope_key: String,
    pub node_id: NodeId,
    pub polymorphic: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct Analysis {
    pub ok: bool,
    pub parse_error: Option<String>,
    pub program_type: Option<RTy>,
    pub constraints: Vec<Constraint>,
    pub edges: Vec<GraphEdge>,
    pub steps: Vec<Step>,
    pub errors: Vec<TyError>,
    pub nodes: Vec<NodeBrief>,
    pub scopes: Vec<ScopeEntry>,
    pub free_vars: Vec<TypeView>,
    pub disabled: Vec<Cid>,
    pub genv_fingerprint: String,
    pub final_snapshot: Vec<TypeView>,
}

#[derive(Debug, Clone)]
struct Scheme {
    quantified: Vec<Vid>,
    body: Vid,
}

#[derive(Debug, Clone)]
struct EnvEntry {
    scheme: Scheme,
    node_id: NodeId,
    scope_key: String,
    polymorphic: bool,
}

struct InferCtx {
    u: Unifier,
    env: Vec<(String, EnvEntry)>,
    scope_depth: u32,
    order: usize,
    constraints: Vec<Constraint>,
    steps: Vec<Step>,
    errors: Vec<TyError>,
    edges: Vec<GraphEdge>,
    nodes: Vec<NodeBrief>,
    scopes: Vec<ScopeEntry>,
    disabled: HashSet<Cid>,
    /// globally assigned display names (a, b, ...)
    display: HashMap<Vid, String>,
    display_counter: usize,
    fresh_counter: u64,
    run_label: String,
    instantiation_occasions: HashMap<String, u32>,
}

pub fn analyze(src: &str, opts: AnalyzeOptions) -> Analysis {
    let mut empty = Analysis {
        ok: false,
        parse_error: None,
        program_type: None,
        constraints: vec![],
        edges: vec![],
        steps: vec![],
        errors: vec![],
        nodes: vec![],
        scopes: vec![],
        free_vars: vec![],
        disabled: opts.disabled.iter().cloned().collect(),
        genv_fingerprint: String::new(),
        final_snapshot: vec![],
    };
    let program = match parse(src) {
        Ok(p) => p,
        Err(e) => {
            empty.parse_error = Some(e);
            return empty;
        }
    };

    let mut ctx = InferCtx {
        u: Unifier::new(),
        env: Vec::new(),
        scope_depth: 0,
        order: 0,
        constraints: Vec::new(),
        steps: Vec::new(),
        errors: Vec::new(),
        edges: Vec::new(),
        nodes: Vec::new(),
        scopes: Vec::new(),
        disabled: opts.disabled,
        display: HashMap::new(),
        display_counter: 0,
        fresh_counter: 0,
        run_label: opts.run_label,
        instantiation_occasions: HashMap::new(),
    };

    install_prelude(&mut ctx);

    let root_v = ctx.infer(&program.body).unwrap_or_else(|_| {
        ctx.anonymous_var(
            PartOrigin::new("root", "program", program.body.span(), "program result"),
            "root",
        )
    });

    ctx.finish(root_v)
}

// ---------------------------------------------------------------------------
// Utilities
// ---------------------------------------------------------------------------

fn kind_of_lit(l: &Lit) -> &'static str {
    match l {
        Lit::Int(_) => "lit-int",
        Lit::Bool(_) => "lit-bool",
        Lit::Str(_) => "lit-str",
        Lit::Unit => "lit-unit",
    }
}

impl InferCtx {
    fn origin_of(&self, e: &Expr, reason: impl Into<String>) -> PartOrigin {
        PartOrigin::new(e.id().to_string(), e.kind(), e.span(), reason)
    }

    fn register_node(&mut self, e: &Expr) {
        self.nodes.push(NodeBrief {
            id: e.id().to_string(),
            kind: e.kind().to_string(),
            label: e.label(),
            start: e.span().start,
            end: e.span().end,
        });
    }

    /// Fresh variable for an expression node; stable across re-parses when the
    /// node path survives, so renaming a binding does not drift its identity.
    fn node_var(&mut self, e: &Expr) -> Vid {
        let key = format!("{}::{}", self.run_label, e.id());
        let key = key.trim_start_matches(':').to_string();
        let v = self.u.fresh(key, Some(self.origin_of(e, "type of this expression")));
        self.register_node(e);
        v
    }

    fn anonymous_var(&mut self, origin: PartOrigin, hint: &str) -> Vid {
        self.fresh_counter += 1;
        let key = format!("{}::{}#{}", self.run_label, hint, self.fresh_counter);
        self.u.fresh(key, Some(origin))
    }

    fn display_name(&mut self, v: &Vid) -> String {
        let r = self.u.find(v);
        if let Some(n) = self.display.get(&r) {
            return n.clone();
        }
        const ABC: &[u8] = b"abcdefghijklmnopqrstuvwxyz";
        let n = if self.display_counter < ABC.len() {
            (ABC[self.display_counter] as char).to_string()
        } else {
            format!("t{}", self.display_counter - ABC.len())
        };
        self.display_counter += 1;
        self.display.insert(r, n.clone());
        n
    }

    fn scope_key(&self, name: &str) -> String {
        format!("d{}:{}", self.scope_depth, name)
    }

    fn push_scope(&mut self) {
        self.scope_depth += 1;
    }
    fn pop_scope(&mut self, depth: usize) {
        self.env.retain(|(_, e)| depth_of(&e.scope_key) < self.scope_depth);
        self.scope_depth -= 1;
    }

    fn lookup(&self, name: &str) -> Option<&EnvEntry> {
        self.env.iter().rev().find(|(n, _)| n == name).map(|(_, e)| e)
    }

    fn bind_raw(&mut self, name: String, entry: EnvEntry) {
        self.scopes.push(ScopeEntry {
            name: name.clone(),
            scope_key: entry.scope_key.clone(),
            node_id: entry.node_id.clone(),
            polymorphic: entry.polymorphic,
        });
        self.env.push((name, entry));
    }
}

fn depth_of(key: &str) -> u32 {
    key.trim_start_matches('d')
        .split(':')
        .next()
        .and_then(|x| x.parse().ok())
        .unwrap_or(0)
}


// ---------------------------------------------------------------------------
// Constraints / steps
// ---------------------------------------------------------------------------

impl InferCtx {
    fn snapshot(&mut self) -> Vec<TypeView> {
        let roots: Vec<Vid> = {
            let mut seen: HashSet<Vid> = HashSet::new();
            let mut out = Vec::new();
            let keys: Vec<Vid> = self.u.parent.keys().cloned().collect();
            for v in keys {
                let r = self.u.find(&v);
                if seen.insert(r.clone()) {
                    out.push(r);
                }
            }
            out
        };
        let mut views = Vec::new();
        for r in roots {
            let rt = self.u.resolve_rty(&r);
            views.push(TypeView { vid: r, rendered: rt });
        }
        views
    }

    fn record_constraint(
        &mut self,
        kind: ConstraintKind,
        left: String,
        right: String,
        origin: PartOrigin,
        detail: String,
    ) -> Cid {
        self.order += 1;
        let id = format!("c{}", self.order);
        self.constraints.push(Constraint {
            id: id.clone(),
            kind: kind.clone(),
            left: left.clone(),
            right: right.clone(),
            origin: origin.clone(),
            detail: detail.clone(),
            order: self.order,
        });
        // constraint graph edges: nodes involved
        self.edges.push(GraphEdge {
            from: origin.node_id.clone(),
            to: id.clone(),
            label: "produces".into(),
        });
        self.edges.push(GraphEdge {
            from: id,
            to: format!("var:{left}"),
            label: "touches".into(),
        });
        if left != right {
            self.edges.push(GraphEdge {
                from: format!("c{}", self.order),
                to: format!("var:{right}"),
                label: "touches".into(),
            });
        }
        format!("c{}", self.order)
    }

    fn record_eq(&mut self, a: &Vid, b: &Vid, origin: PartOrigin, detail: String) {
        let ra = self.u.find(a);
        let rb = self.u.find(b);
        let cid = self.record_constraint(
            ConstraintKind::Eq,
            ra,
            rb,
            origin.clone(),
            detail.clone(),
        );
        let idx = self.steps.len();
        if self.disabled.contains(&cid) {
            let snap = self.snapshot();
            self.steps.push(Step {
                index: idx,
                cid,
                kind: ConstraintKind::Eq,
                title: detail.clone(),
                detail,
                status: StepStatus::Skipped,
                error: None,
                snapshot: snap,
                fresh: vec![],
                span: origin.span,
            });
            return;
        }
        let before = self.u.resolve_rty(a);
        let after_short_for_b = self.u.resolve_rty(b).short();
        let result = self.u.unify(a, b);
        match result {
            Ok(()) => {
                let after = self.u.resolve_rty(b).short();
                let snap = self.snapshot();
                self.steps.push(Step {
                    index: idx,
                    cid,
                    kind: ConstraintKind::Eq,
                    title: detail,
                    detail: format!("{} ~ {} ✓", before.short(), after),
                    status: StepStatus::Ok,
                    error: None,
                    snapshot: snap,
                    fresh: vec![],
                    span: origin.span,
                });
            }
            Err(err) => {
                let msg = render_uerr(&err);
                let snap = self.snapshot();
                self.steps.push(Step {
                    index: idx,
                    cid: cid.clone(),
                    kind: ConstraintKind::Eq,
                    title: detail.clone(),
                    detail: format!("{} ✗ {}", before.short(), msg),
                    status: StepStatus::Failed,
                    error: Some(err.clone()),
                    snapshot: snap,
                    fresh: vec![],
                    span: origin.span,
                });
                let mut spans: Vec<Span> = err.origins().iter().map(|o| o.span).collect();
                spans.push(origin.span);
                spans.sort_by_key(|sp| sp.start);
                spans.dedup();
                self.errors.push(TyError {
                    step_index: idx,
                    cid,
                    message: msg,
                    kind: uerr_kind(&err).to_string(),
                    spans,
                    error: err,
                });
            }
        }
        let _ = after_short_for_b;
    }

    fn note_step(
        &mut self,
        cid: Cid,
        kind: ConstraintKind,
        title: String,
        detail: String,
        fresh: Vec<String>,
        origin: PartOrigin,
    ) {
        let idx = self.steps.len();
        let snap = self.snapshot();
        self.steps.push(Step {
            index: idx,
            cid,
            kind,
            title,
            detail,
            status: StepStatus::Ok,
            error: None,
            snapshot: snap,
            fresh,
            span: origin.span,
        });
    }
}

pub fn render_uerr(e: &UErr) -> String {
    match e {
        UErr::Occurs { var, in_type, .. } => {
            format!("无限类型：变量 {var} 出现在它自身 {ty} 中（occurs-check 拒绝）", ty = in_type.short())
        }
        UErr::Mismatch { left, right, .. } => {
            format!("类型不匹配：{} 与 {} 无法统一", left.short(), right.short())
        }
        UErr::MissingFields { side, fields, row, expected, .. } => {
            let side_txt = match side {
                RowSide::Left => "前者",
                RowSide::Right => "后者",
            };
            let miss = fields.join(", ");
            format!(
                "行字段不一致：{} {}，缺少字段 {}；实际 {}，期望 {}",
                side_txt,
                if fields.len() == 1 { "缺少一个字段" } else { "缺少多个字段" },
                miss,
                row.short(),
                expected.short()
            )
        }
    }
}

pub fn uerr_kind(e: &UErr) -> &'static str {
    match e {
        UErr::Occurs { .. } => "occurs",
        UErr::Mismatch { .. } => "mismatch",
        UErr::MissingFields { .. } => "row-fields",
    }
}

// ---------------------------------------------------------------------------
// Prelude
// ---------------------------------------------------------------------------

fn install_prelude(ctx: &mut InferCtx) {
    // add : int -> int -> int
    let o = PartOrigin::new("prelude:add", "builtin", Span::new(0, 0), "builtin add");
    let a = ctx.anonymous_var(o.clone(), "pre");
    let b = ctx.anonymous_var(o.clone(), "pre");
    let r = ctx.anonymous_var(o.clone(), "pre");
    ctx.u.put_con(&a, "int", o.clone());
    ctx.u.put_con(&b, "int", o.clone());
    ctx.u.put_con(&r, "int", o.clone());
    let inner = ctx.anonymous_var(o.clone(), "pre");
    ctx.u.put_arrow(&inner, b, r, o.clone());
    let add_v = ctx.anonymous_var(o.clone(), "pre");
    ctx.u.put_arrow(&add_v, a, inner, o.clone());
    ctx.bind_raw(
        "add".into(),
        EnvEntry {
            scheme: Scheme { quantified: vec![], body: add_v },
            node_id: "prelude:add".into(),
            scope_key: "d0:add".into(),
            polymorphic: false,
        },
    );

    // id : 'a -> 'a, truly polymorphic
    let o = PartOrigin::new("prelude:id", "builtin", Span::new(0, 0), "builtin id");
    let x = ctx.anonymous_var(o.clone(), "pre");
    let id_v = ctx.anonymous_var(o.clone(), "pre");
    ctx.u.put_arrow(&id_v, x.clone(), x.clone(), o.clone());
    let xroot = ctx.u.find(&x);
    let scheme = Scheme { quantified: vec![xroot], body: id_v };
    ctx.bind_raw(
        "id".into(),
        EnvEntry {
            scheme,
            node_id: "prelude:id".into(),
            scope_key: "d0:id".into(),
            polymorphic: true,
        },
    );
}

// ---------------------------------------------------------------------------
// Generalization / instantiation
// ---------------------------------------------------------------------------

fn is_syntactic_value(e: &Expr) -> bool {
    match e {
        Expr::Lit(..) | Expr::Lambda { .. } | Expr::Tag { payload: None, .. } => true,
        Expr::Tag { payload: Some(p), .. } => is_syntactic_value(p),
        Expr::Record { fields, .. } => fields.iter().all(|f| is_syntactic_value(&f.value)),
        Expr::Var(..) => true,
        _ => false,
    }
}

impl InferCtx {
    fn free_vars(&self, root: &Vid) -> HashSet<Vid> {
        let mut out: HashSet<Vid> = HashSet::new();
        let mut stack = vec![self.u.find_imm(root)];
        while let Some(v) = stack.pop() {
            match self.u.nodes.get(&v).cloned() {
                Some(Node::Flex { .. }) | None => {
                    out.insert(v);
                }
                Some(n) => {
                    for c in type_children(&n) {
                        stack.push(self.u.find_imm(&c));
                    }
                }
            }
        }
        out
    }

    fn env_free_vars(&self) -> HashSet<Vid> {
        let mut out = HashSet::new();
        for (_, e) in &self.env {
            if !e.polymorphic {
                for v in self.free_vars(&e.scheme.body) {
                    out.insert(v);
                }
            }
        }
        out
    }

    fn generalize(
        &mut self,
        value_v: &Vid,
        origin: PartOrigin,
        is_value: bool,
        value_expr: &Expr,
    ) -> (Scheme, Vec<String>, bool) {
        let body = self.u.find(value_v);
        let env_fv = self.env_free_vars();
        let fv: Vec<Vid> = self
            .free_vars(&body)
            .into_iter()
            .filter(|v| !env_fv.contains(v))
            .collect();
        let polymorphic = is_value;
        let mut names = Vec::new();
        if polymorphic {
            for v in &fv {
                names.push(self.display_name(v));
            }
            names.sort();
        } else {
            // Value restriction: no quantification. Record why.
            names.clear();
        }
        let body_short = self.u.resolve_rty(&body).short();
        let detail = if polymorphic {
            format!("generalize: ∀{}. {}", names.join(" "), body_short)
        } else {
            format!(
                "generalize(skipped, value restriction): {} 不是语法值（{}）",
                value_expr.label(),
                value_expr.kind()
            )
        };
        let cid = self.record_constraint(
            ConstraintKind::Generalize,
            body.clone(),
            body.clone(),
            origin.clone(),
            detail,
        );
        let step_detail = if polymorphic {
            format!("∀{}. {}", names.join(" "), body_short)
        } else {
            format!("value restriction：{} 保持单态", body_short)
        };
        self.note_step(
            cid,
            ConstraintKind::Generalize,
            "泛化 let 绑定".into(),
            step_detail,
            vec![],
            origin,
        );
        (Scheme { quantified: fv, body }, names, polymorphic)
    }

    fn instantiate(&mut self, entry: &EnvEntry, use_origin: &PartOrigin, occasion: &str) -> Vid {
        let n = self.instantiation_occasions.entry(occasion.to_string()).or_insert(0);
        *n += 1;
        let occ = *n;
        let prefix = format!("{}#inst{}", occasion, occ);

        let mut mapping: HashMap<Vid, Vid> = HashMap::new();
        let quantified: HashSet<Vid> = entry
            .scheme
            .quantified
            .iter()
            .map(|v| self.u.find(v))
            .collect();
        let copy = self.deep_copy(
            &entry.scheme.body,
            &mut mapping,
            &prefix,
            use_origin,
            &quantified,
        );

        let mut fresh_names: Vec<String> = mapping.values().map(|v| self.display_name(v)).collect();
        fresh_names.sort();

        let body_before = self.u.resolve_rty(&entry.scheme.body).short();
        let copy_before = self.u.resolve_rty(&copy).short();
        let body_vid = entry.scheme.body.clone();
        let cid = self.record_constraint(
            ConstraintKind::Instantiate,
            body_vid,
            copy.clone(),
            use_origin.clone(),
            format!("instantiate {} -> {}", body_before, copy_before),
        );
        self.note_step(
            cid,
            ConstraintKind::Instantiate,
            format!("实例化 {} 的一次使用", entry.node_id),
            format!("fresh [{}]", fresh_names.join(", ")),
            mapping.values().cloned().collect(),
            use_origin.clone(),
        );
        copy
    }

    fn deep_copy(
        &mut self,
        v: &Vid,
        mapping: &mut HashMap<Vid, Vid>,
        prefix: &str,
        origin: &PartOrigin,
        quantified: &HashSet<Vid>,
    ) -> Vid {
        let r = self.u.find_imm(v);
        if let Some(copy) = mapping.get(&r) {
            return copy.clone();
        }
        match self.u.nodes.get(&r).cloned() {
            Some(Node::Flex { .. }) | None => {
                if !quantified.contains(&r) {
                    // Non-generalized (monomorphic) free variable: share it.
                    return r;
                }
                let nv = self.u.fresh(format!("{}@{}", prefix, r), Some(origin.clone()));
                mapping.insert(r, nv.clone());
                nv
            }
            Some(Node::Con(n, o)) => {
                let nv = self.u.fresh(format!("{}@{}", prefix, r), Some(o.clone()));
                self.u.put_con(&nv, &n, o);
                mapping.insert(r, nv.clone());
                nv
            }
            Some(Node::Arrow(d, c, o)) => {
                let nd = self.deep_copy(&d, mapping, prefix, origin, quantified);
                let nc = self.deep_copy(&c, mapping, prefix, origin, quantified);
                let nv = self.u.fresh(format!("{}@{}", prefix, r), Some(o.clone()));
                self.u.put_arrow(&nv, nd, nc, o);
                mapping.insert(r, nv.clone());
                nv
            }
            Some(Node::Record(row, o)) => {
                let nfs: Vec<(String, Vid)> = row
                    .fields
                    .iter()
                    .map(|(n, fv)| (n.clone(), self.deep_copy(fv, mapping, prefix, origin, quantified)))
                    .collect();
                let nt = self.deep_copy(&row.tail, mapping, prefix, origin, quantified);
                let nv = self.u.fresh(format!("{}@{}", prefix, r), Some(o.clone()));
                self.u.put_record(&nv, nfs, nt, o);
                mapping.insert(r, nv.clone());
                nv
            }
            Some(Node::Variant(row, o)) => {
                let nfs: Vec<(String, Vid)> = row
                    .fields
                    .iter()
                    .map(|(n, fv)| (n.clone(), self.deep_copy(fv, mapping, prefix, origin, quantified)))
                    .collect();
                let nt = self.deep_copy(&row.tail, mapping, prefix, origin, quantified);
                let nv = self.u.fresh(format!("{}@{}", prefix, r), Some(o.clone()));
                self.u.put_variant(&nv, nfs, nt, o);
                mapping.insert(r, nv.clone());
                nv
            }
            Some(Node::RowEmpty) => {
                let nv = self.u.fresh(format!("{}@{}", prefix, r), None);
                self.u.put_row_empty(&nv);
                mapping.insert(r, nv.clone());
                nv
            }
        }
    }
}

fn type_children(n: &Node) -> Vec<Vid> {
    match n {
        Node::Arrow(a, b, _) => vec![a.clone(), b.clone()],
        Node::Record(r, _) | Node::Variant(r, _) => {
            let mut v: Vec<Vid> = r.fields.iter().map(|(_, x)| x.clone()).collect();
            v.push(r.tail.clone());
            v
        }
        _ => vec![],
    }
}

// ---------------------------------------------------------------------------
// Annotation conversion
// ---------------------------------------------------------------------------

impl InferCtx {
    /// Convert an annotation into the unifier. Returns the root vid and a
    /// deterministic map of annotation type-variable names to vids, so the
    /// same variable is shared across the whole annotation.
    fn convert_type(
        &mut self,
        ty: &Type,
        tvars: &mut HashMap<String, Vid>,
        source_node: &str,
    ) -> Vid {
        let o = PartOrigin::new(
            source_node.to_string(),
            "annotation",
            ty.span(),
            format!("annotation {}", ty.render()),
        );
        match ty {
            Type::TCon(n, sp) => {
                let v = self.anonymous_var(
                    PartOrigin::new(source_node, "annotation", *sp, format!("type constructor {n}")),
                    "ann",
                );
                self.u.put_con(&v, n, o);
                v
            }
            Type::TVar(n, sp) => {
                if let Some(v) = tvars.get(n) {
                    return v.clone();
                }
                let v = self.anonymous_var(
                    PartOrigin::new(source_node, "annotation", *sp, format!("type variable '{n}")),
                    "ann",
                );
                tvars.insert(n.clone(), v.clone());
                v
            }
            Type::TArrow(a, b, _sp) => {
                let va = self.convert_type(a, tvars, source_node);
                let vb = self.convert_type(b, tvars, source_node);
                let v = self.anonymous_var(o.clone(), "ann");
                self.u.put_arrow(&v, va, vb, o);
                v
            }
            Type::TRecord(fs, open, sp) => {
                let mut fields: Vec<(String, Vid)> = Vec::new();
                for f in fs {
                    let fv = self.convert_type(&f.ty, tvars, source_node);
                    fields.push((f.name.clone(), fv));
                }
                let tail = if *open {
                    self.anonymous_var(
                        PartOrigin::new(source_node, "annotation", *sp, "open record row"),
                        "row",
                    )
                } else {
                    let t = self.anonymous_var(o.clone(), "row");
                    self.u.put_row_empty(&t);
                    t
                };
                let v = self.anonymous_var(o.clone(), "ann");
                self.u.put_record(&v, fields, tail, o);
                v
            }
            Type::TVariant(fs, open, sp) => {
                let mut fields: Vec<(String, Vid)> = Vec::new();
                for f in fs {
                    let fv = self.convert_type(&f.ty, tvars, source_node);
                    fields.push((f.name.clone(), fv));
                }
                let tail = if *open {
                    self.anonymous_var(
                        PartOrigin::new(source_node, "annotation", *sp, "open variant row"),
                        "row",
                    )
                } else {
                    let t = self.anonymous_var(o.clone(), "row");
                    self.u.put_row_empty(&t);
                    t
                };
                let v = self.anonymous_var(o.clone(), "ann");
                self.u.put_variant(&v, fields, tail, o);
                v
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Main inference
// ---------------------------------------------------------------------------

impl InferCtx {
    fn infer(&mut self, e: &Expr) -> Result<Vid, ()> {
        match e {
            Expr::Lit(l, _, _) => {
                let v = self.node_var(e);
                let (name, why) = match l {
                    Lit::Int(_) => ("int", "integer literal"),
                    Lit::Bool(_) => ("bool", "boolean literal"),
                    Lit::Str(_) => ("string", "string literal"),
                    Lit::Unit => ("unit", "unit literal"),
                };
                let c = self.anonymous_var(self.origin_of(e, why), "lit");
                self.u.put_con(&c, name, self.origin_of(e, why));
                self.record_eq(&v, &c, self.origin_of(e, why), format!("{} : {}", e.label(), name));
                Ok(v)
            }

            Expr::Var(name, _, _) => {
                let v = self.node_var(e);
                let entry = self.lookup(name).cloned();
                match entry {
                    Some(entry) => {
                        let occ = format!("{}@{}", name, e.id());
                        let inst = self.instantiate(
                            &entry,
                            &self.origin_of(e, format!("use of {name}")),
                            &occ,
                        );
                        self.record_eq(
                            &v,
                            &inst,
                            self.origin_of(e, format!("use of {name}")),
                            format!("{} = instantiation of {name}", e.label()),
                        );
                        Ok(v)
                    }
                    None => {
                        // Unbound name: constrain to a marker type so the error
                        // is structured rather than a panic.
                        let m = self.anonymous_var(self.origin_of(e, "unbound name"), "unbound");
                        self.u.put_con(&m, &format!("<unbound:{name}>"), self.origin_of(e, "unbound name"));
                        self.record_eq(
                            &v,
                            &m,
                            self.origin_of(e, "unbound name"),
                            format!("unbound name '{name}'"),
                        );
                        Ok(v)
                    }
                }
            }

            Expr::Lambda { param, body, .. } => self.infer_lambda(e, param, body),

            Expr::App { fun, arg, .. } => {
                let v = self.node_var(e);
                let fv = self.infer(fun)?;
                let av = self.infer(arg)?;
                let d = self.anonymous_var(self.origin_of(arg, "function parameter"), "dom");
                let c = self.anonymous_var(self.origin_of(e, "application result"), "cod");
                let arr = self.anonymous_var(self.origin_of(e, "function type"), "arr");
                self.u.put_arrow(&arr, d.clone(), c.clone(), self.origin_of(e, "function arrow"));
                self.record_eq(&fv, &arr, self.origin_of(fun, "applied as function"), "callee is a function".into());
                self.record_eq(&av, &d, self.origin_of(arg, "argument matches parameter"), "argument ~ parameter".into());
                self.record_eq(&v, &c, self.origin_of(e, "application result"), "result of application".into());
                Ok(v)
            }

            Expr::Bin { op, lhs, rhs, .. } => self.infer_binop(e, op, lhs, rhs),
            Expr::If { cond, then_e, else_e, .. } => {
                let v = self.node_var(e);
                let cv = self.infer(cond)?;
                let boolt = self.anonymous_var(self.origin_of(cond, "condition is bool"), "cond");
                self.u.put_con(&boolt, "bool", self.origin_of(cond, "condition is bool"));
                self.record_eq(&cv, &boolt, self.origin_of(cond, "if condition"), "condition : bool".into());
                let tv = self.infer(then_e)?;
                let ev = self.infer(else_e)?;
                self.record_eq(&tv, &ev, self.origin_of(else_e, "branches agree"), "then ~ else".into());
                self.record_eq(&v, &tv, self.origin_of(e, "if result"), "if result ~ then branch".into());
                Ok(v)
            }

            Expr::Let { rec, name, ann, value, body, .. } => {
                let v = self.node_var(e);
                self.infer_let(e, v, *rec, name, ann.as_ref(), value, body)
            }

            Expr::Record { fields, .. } => {
                let v = self.node_var(e);
                let mut fvids = Vec::new();
                for f in fields {
                    let fv = self.infer(&f.value)?;
                    fvids.push((f.name.clone(), fv));
                }
                // Inferred records are open (row polymorphism).
                let tail = self.anonymous_var(self.origin_of(e, "inferred open record tail"), "row");
                let rv = self.anonymous_var(self.origin_of(e, "record type"), "rec");
                self.u.put_record(&rv, fvids, tail, self.origin_of(e, "record literal"));
                self.record_eq(&v, &rv, self.origin_of(e, "record literal"), format!("record {}", e.label()));
                Ok(v)
            }

            Expr::Field { record, name, .. } => {
                let v = self.node_var(e);
                let rv = self.infer(record)?;
                let field_v = self.anonymous_var(self.origin_of(e, format!("field {name}")), "field");
                let tail = self.anonymous_var(self.origin_of(e, "open row after projection"), "row");
                let row_v = self.anonymous_var(self.origin_of(e, "record containing field"), "rec");
                self.u.put_record(
                    &row_v,
                    vec![(name.clone(), field_v.clone())],
                    tail,
                    self.origin_of(e, format!("has field {name}")),
                );
                self.record_eq(&rv, &row_v, self.origin_of(record, "record access"), format!("record has .{name}"));
                self.record_eq(&v, &field_v, self.origin_of(e, "projected field"), format!(".{name} type"));
                Ok(v)
            }

            Expr::Tag { tag, payload, .. } => {
                let v = self.node_var(e);
                let pv = if let Some(p) = payload { Some(self.infer(p)?) } else { None };
                let payload_v = match pv {
                    Some(x) => x,
                    None => {
                        let u = self.anonymous_var(self.origin_of(e, "unit payload"), "tag");
                        self.u.put_con(&u, "unit", self.origin_of(e, "nullary tag"));
                        u
                    }
                };
                let tail = self.anonymous_var(self.origin_of(e, "open variant tail"), "row");
                let vv = self.anonymous_var(self.origin_of(e, "variant type"), "var");
                self.u.put_variant(&vv, vec![(tag.clone(), payload_v)], tail, self.origin_of(e, "tag injection"));
                self.record_eq(&v, &vv, self.origin_of(e, "tag injection"), format!(":{tag} injection"));
                Ok(v)
            }

            Expr::Match { scrut, arms, .. } => self.infer_match(e, scrut, arms),
        }
    }
}

// ---------------------------------------------------------------------------
// Lambda, binop
// ---------------------------------------------------------------------------

impl InferCtx {
    fn infer_lambda(&mut self, e: &Expr, param: &Param, body: &Expr) -> Result<Vid, ()> {
        let v = self.node_var(e);
        let pv = self.anonymous_var(
            PartOrigin::new(e.id().to_string(), "lambda-param", e.span(), format!("parameter {}", param.name)),
            "param",
        );
        if let Some(ann) = &param.ann {
            let mut tvars = HashMap::new();
            let av = self.convert_type(ann, &mut tvars, e.id());
            // Parameter annotations are monomorphic within the function.
            self.record_eq(&pv, &av, self.origin_of(e, "parameter annotation"), format!("{} : {}", param.name, ann.render()));
        }
        self.push_scope();
        self.bind_raw(
            param.name.clone(),
            EnvEntry {
                scheme: Scheme { quantified: vec![], body: pv.clone() },
                node_id: e.id().to_string(),
                scope_key: self.scope_key(&param.name),
                polymorphic: false,
            },
        );
        let bv = self.infer(body)?;
        let depth = self.scope_depth as usize;
        self.pop_scope(depth);
        let arr = self.anonymous_var(self.origin_of(e, "function arrow"), "arr");
        self.u.put_arrow(&arr, pv, bv.clone(), self.origin_of(e, "lambda arrow"));
        self.record_eq(&v, &arr, self.origin_of(e, "function type"), "lambda : dom -> cod".into());
        Ok(v)
    }

    fn infer_binop(&mut self, e: &Expr, op: &str, lhs: &Expr, rhs: &Expr) -> Result<Vid, ()> {
        let v = self.node_var(e);
        let lv = self.infer(lhs)?;
        let rv = self.infer(rhs)?;
        let (res_con, side_con) = if op == "+" { ("int", Some("int")) } else { ("bool", Some("bool")) };
        if let Some(cn) = side_con {
            let la = self.anonymous_var(self.origin_of(lhs, format!("{op} operand")), "op");
            self.u.put_con(&la, cn, self.origin_of(lhs, "operand type"));
            self.record_eq(&lv, &la, self.origin_of(lhs, format!("left of {op}")), format!("left : {cn}"));
            let ra = self.anonymous_var(self.origin_of(rhs, format!("{op} operand")), "op");
            self.u.put_con(&ra, cn, self.origin_of(rhs, "operand type"));
            self.record_eq(&rv, &ra, self.origin_of(rhs, format!("right of {op}")), format!("right : {cn}"));
        }
        let res = self.anonymous_var(self.origin_of(e, "operator result"), "op");
        self.u.put_con(&res, res_con, self.origin_of(e, "operator result"));
        self.record_eq(&v, &res, self.origin_of(e, "binop result"), format!("{op} : {res_con}"));
        Ok(v)
    }
}

// ---------------------------------------------------------------------------
// Let (non-recursive, recursive, value restriction, annotation)
// ---------------------------------------------------------------------------

impl InferCtx {
    #[allow(clippy::too_many_arguments)]
    fn infer_let(
        &mut self,
        e: &Expr,
        result_v: Vid,
        rec: bool,
        name: &str,
        ann: Option<&Type>,
        value: &Expr,
        body: &Expr,
    ) -> Result<Vid, ()> {
        self.push_scope();

        // For recursive bindings, introduce a fresh monomorphic variable so
        // the RHS can refer to itself.
        let rec_v = if rec {
            let rv = self.anonymous_var(self.origin_of(value, format!("recursive binding {name}")), "rec");
            self.bind_raw(
                name.to_string(),
                EnvEntry {
                    scheme: Scheme { quantified: vec![], body: rv.clone() },
                    node_id: e.id().to_string(),
                    scope_key: self.scope_key(name),
                    polymorphic: false,
                },
            );
            Some(rv)
        } else {
            None
        };

        let value_v = self.infer(value)?;

        if let Some(rv) = &rec_v {
            self.record_eq(
                rv,
                &value_v,
                self.origin_of(value, "recursive fixed point"),
                format!("rec {name} fixed point"),
            );
        }

        // Annotation constraint (also used to close rows).
        if let Some(ann) = ann.cloned() {
            let ann_text = ann.render();
            let ann_span = ann.span();
            let mut tvars = HashMap::new();
            let av = self.convert_type(&ann, &mut tvars, e.id());
            let ascr_origin = self.origin_of(value, format!("ascribed type {ann_text}"));
            let cid = self.record_constraint(
                ConstraintKind::Restrict,
                value_v.clone(),
                av.clone(),
                ascr_origin.clone(),
                format!("ascription: _ as {ann_text}"),
            );
            let idx = self.steps.len();
            if self.disabled.contains(&cid) {
                self.note_step(
                    cid,
                    ConstraintKind::Restrict,
                    "类型标注（已禁用）".into(),
                    ann_text,
                    vec![],
                    ascr_origin,
                );
            } else {
                let res = self.u.unify(&value_v, &av);
                match res {
                    Ok(()) => self.note_step(
                        cid,
                        ConstraintKind::Restrict,
                        "类型标注一致".into(),
                        ann_text.clone(),
                        vec![],
                        self.origin_of(value, "ascription"),
                    ),
                    Err(err) => {
                        let msg = render_uerr(&err);
                        let snap = self.snapshot();
                        self.steps.push(Step {
                            index: idx,
                            cid: cid.clone(),
                            kind: ConstraintKind::Restrict,
                            title: "类型标注检查".into(),
                            detail: msg.clone(),
                            status: StepStatus::Failed,
                            error: Some(err.clone()),
                            snapshot: snap,
                            fresh: vec![],
                            span: value.span(),
                        });
                        let mut spans: Vec<Span> = err.origins().iter().map(|o| o.span).collect();
                        spans.push(ann_span);
                        spans.push(value.span());
                        spans.sort_by_key(|sp| sp.start);
                        spans.dedup();
                        self.errors.push(TyError {
                            step_index: idx,
                            cid,
                            message: msg,
                            kind: uerr_kind(&err).to_string(),
                            spans,
                            error: err,
                        });
                    }
                }
            }
        }

        // Generalize (recursive lets generalize over the fixed value type).
        // The RHS syntactic value check enforces the value restriction.
        let gen_root_v = if rec {
            self.u
                .find(rec_v.as_ref().unwrap_or(&value_v))
        } else {
            self.u.find(&value_v)
        };
        let is_value = is_syntactic_value(value);
        let (scheme, qnames, polymorphic) = self.generalize(
            &gen_root_v,
            self.origin_of(value, format!("generalize {name}")),
            is_value,
            value,
        );
        let _ = qnames;

        // Rebind the name in the current scope with the (possibly) poly scheme.
        // Remove the temporary recursive entry first if present.
        if rec {
            // drop the temporary monomorphic binding (last occurrence)
            if let Some(pos) = self.env.iter().rposition(|(n, en)| n == name && en.node_id == e.id()) {
                self.env.remove(pos);
            }
        }
        self.bind_raw(
            name.to_string(),
            EnvEntry {
                scheme,
                node_id: e.id().to_string(),
                scope_key: self.scope_key(name),
                polymorphic,
            },
        );

        let body_v = self.infer(body)?;
        let depth = self.scope_depth as usize;
        self.pop_scope(depth);

        self.record_eq(&result_v, &body_v, self.origin_of(e, "let body result"), format!("let {name} in body"));
        Ok(result_v)
    }
}

// ---------------------------------------------------------------------------
// Match / patterns
// ---------------------------------------------------------------------------

impl InferCtx {
    fn infer_match(&mut self, e: &Expr, scrut: &Expr, arms: &[Arm]) -> Result<Vid, ()> {
        let v = self.node_var(e);
        let sv = self.infer(scrut)?;

        // Open variant row shared by all arms; nullary tags get unit payloads.
        let scrut_row = self.anonymous_var(self.origin_of(e, "scrutinee variant row"), "row");
        let mut present: Vec<(String, Vid)> = Vec::new();
        let mut arm_results: Vec<Vid> = Vec::new();

        for (i, arm) in arms.iter().enumerate() {
            let (tag, bind, payload_v_opt, pspan) = match &arm.pat {
                Pattern::PTag(tag, payload, sp) => {
                    let bind = match payload.as_deref() {
                        Some(Pattern::PVar(n, ann, _)) => Some((n.clone(), ann.clone())),
                        Some(Pattern::PWild(_)) | None => None,
                        Some(Pattern::PTag(..)) => {
                            return Ok(v);
                        }
                    };
                    (tag.clone(), bind, None::<Vid>, *sp)
                }
                Pattern::PVar(n, ann, sp) => ("__catchall__".to_string(), Some((n.clone(), ann.clone())), None, *sp),
                Pattern::PWild(sp) => ("__catchall__".to_string(), None, None, *sp),
            };

            // Payload type variable for this arm's tag.
            let payload_v = self.anonymous_var(
                PartOrigin::new(e.id().to_string(), "match-payload", pspan, format!("payload of :{tag}")),
                "payload",
            );

            if tag != "__catchall__" {
                // Restrict scrutinee variant row to include this tag.
                let tail = self.anonymous_var(
                    PartOrigin::new(e.id().to_string(), "match-row", pspan, "open variant row"),
                    "row",
                );
                let vv = self.anonymous_var(
                    PartOrigin::new(e.id().to_string(), "match-row", pspan, format!("arm sees :{tag}")),
                    "var",
                );
                self.u.put_variant(
                    &vv,
                    vec![(tag.clone(), payload_v.clone())],
                    tail,
                    PartOrigin::new(e.id().to_string(), "match-row", pspan, format!("arm :{tag}")),
                );
                self.record_eq(
                    &scrut_row,
                    &vv,
                    PartOrigin::new(e.id().to_string(), "match", e.span(), format!("arm {} pattern", i)),
                    format!("scrutinee is :{tag}"),
                );
            }

            self.push_scope();
            if let Some((bind_name, bind_ann)) = bind {
                let bind_v = self.anonymous_var(
                    PartOrigin::new(e.id().to_string(), "pattern-binding", pspan, format!("binding {bind_name}")),
                    "bind",
                );
                if tag != "__catchall__" {
                    self.record_eq(&bind_v, &payload_v, PartOrigin::new(e.id().to_string(), "pattern-binding", pspan, "payload binding"), format!("{bind_name} <- :{tag} payload"));
                }
                if let Some(ann) = bind_ann {
                    let mut tvars = HashMap::new();
                    let av = self.convert_type(&ann, &mut tvars, e.id());
                    self.record_eq(&bind_v, &av, PartOrigin::new(e.id().to_string(), "pattern-binding", pspan, "pattern annotation"), ann.render());
                }
                self.bind_raw(
                    bind_name,
                    EnvEntry {
                        scheme: Scheme { quantified: vec![], body: bind_v },
                        node_id: e.id().to_string(),
                        scope_key: self.scope_key(&format!("arm{i}")),
                        polymorphic: false,
                    },
                );
            }
            let ab = self.infer(&arm.body)?;
            let depth = self.scope_depth as usize;
            self.pop_scope(depth);
            arm_results.push(ab);
            let _ = payload_v_opt;
        }

        // Scrutinee is a variant over the accumulated row.
        let scrut_variant = self.anonymous_var(self.origin_of(e, "scrutinee variant"), "var");
        self.u.put_variant(
            &scrut_variant,
            present,
            scrut_row.clone(),
            self.origin_of(e, "match scrutinee"),
        );
        self.record_eq(&sv, &scrut_variant, self.origin_of(scrut, "matched value"), "scrutinee : variant".into());

        // All arms agree.
        let first = arm_results[0].clone();
        for (i, ar) in arm_results.iter().enumerate().skip(1) {
            self.record_eq(
                &first,
                ar,
                self.origin_of(e, format!("arms {i} agree")),
                format!("match arm {i} result ~ arm 0"),
            );
        }
        self.record_eq(&v, &first, self.origin_of(e, "match result"), "match result".into());
        Ok(v)
    }
}

// ---------------------------------------------------------------------------
// Finish: fingerprints and final rendering
// ---------------------------------------------------------------------------

impl InferCtx {
    fn finish(mut self, root_v: Vid) -> Analysis {
        let root_v = self.u.find(&root_v);
        let program_type = self.u.resolve_rty(&root_v);

        let mut genv: Vec<String> = self
            .env
            .iter()
            .map(|(n, e)| {
                format!(
                    "{}|{}|poly={}",
                    e.scope_key,
                    n,
                    e.polymorphic
                )
            })
            .collect();
        genv.sort();
        let genv_fingerprint = genv.join(";");

        let final_snapshot = self.snapshot();
        let free_vars = final_snapshot
            .iter()
            .filter(|tv| matches!(tv.rendered, RTy::RVar(..)))
            .cloned()
            .collect();

        let ok = self.errors.is_empty();
        Analysis {
            ok,
            parse_error: None,
            program_type: Some(program_type),
            constraints: self.constraints,
            edges: self.edges,
            steps: self.steps,
            errors: self.errors,
            nodes: self.nodes,
            scopes: self.scopes,
            free_vars,
            disabled: self.disabled.into_iter().collect(),
            genv_fingerprint,
            final_snapshot,
        }
    }

}
