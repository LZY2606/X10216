use crate::infer::*;
use crate::syntax::*;
use crate::types::*;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum TypeError {
    Occurs { variable: Vid, ty: Ty, message: String, spans: Vec<Span>, proof: Proof },
    Mismatch { left: Ty, right: Ty, message: String, missing: BTreeSet<String>, extra: BTreeSet<String>, spans: Vec<Span>, proof: Proof },
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "status", rename_all = "camelCase")]
pub enum StepOutcome {
    Info,
    Unified { left: Ty, right: Ty, description: String },
    Failed(TypeError),
}

#[derive(Debug, Clone, Serialize)]
pub struct SolverStep {
    pub index: usize,
    pub event: StepSpec,
    pub outcome: StepOutcome,
}

#[derive(Debug, Clone, Serialize)]
pub struct SolveResult {
    pub ok: bool,
    pub root_type: Option<Ty>,
    pub node_types: BTreeMap<String, Ty>,
    pub steps: Vec<SolverStep>,
    pub error: Option<TypeError>,
    pub active_constraints: BTreeSet<String>,
    pub cores: Vec<Core>,
    pub preferred_core: Option<Core>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Core {
    pub members: Vec<String>,
    pub size: usize,
    pub explanation: String,
    pub equally_small: bool,
}

#[derive(Default, Clone)]
struct Link {
    parent: BTreeMap<Vid, Vid>,
    rank: BTreeMap<Vid, usize>,
    root_ty: BTreeMap<Vid, Ty>,
    levels: BTreeMap<Vid, u32>,
    row_links: BTreeMap<Vid, RowLink>,
    current_level: u32,
    schemes: BTreeMap<String, Scheme>,
}

pub fn solve(generated: &Generated, ast: &ExprNode, disabled: &BTreeSet<String>) -> SolveResult {
    let mut result = solve_once(generated, ast, disabled);
    let mut state = Link::default();
    let mut steps = Vec::new();
    let mut active = BTreeSet::new();
    let mut node_spans = BTreeMap::new();
    collect_spans(ast, &mut node_spans);
    let mut error = None;

    for (idx, event) in generated.steps.iter().enumerate() {
        if error.is_some() { break; }
        let outcome = match event {
            StepSpec::Introduce { variable, .. } => { state.ensure(variable); StepOutcome::Info }
            StepSpec::EnterScope { scope_key, .. } => {
                if scope_key.contains(":let-value") || scope_key.ends_with(":fun") { state.current_level += 1; }
                StepOutcome::Info
            }
            StepSpec::LeaveScope { scope_key, .. } => {
                if scope_key.contains(":let-value") || scope_key.ends_with(":fun") { state.current_level = state.current_level.saturating_sub(1); }
                StepOutcome::Info
            }
            StepSpec::Generalize { id, value, scheme, .. } => {
                let resolved = state.resolve_ty(value);
                let quantifiers = collect_vars(&resolved).into_iter().filter(|v| {
                    !v.starts_with("annot:") && state.levels.get(v).copied().unwrap_or(state.current_level) >= state.current_level
                }).collect::<Vec<_>>();
                let frozen = Scheme { quantifiers: quantifiers.clone(), body: resolved.clone(), monomorphic: false, value_restricted: scheme.value_restricted, id: scheme.id.clone() };
                let frozen = if scheme.value_restricted { Scheme { quantifiers: Vec::new(), body: resolved, monomorphic: true, value_restricted: true, id: scheme.id.clone() } } else { frozen };
                state.schemes.insert(scheme.id.clone(), frozen.clone());
                let _ = id;
                StepOutcome::Info
            }
            StepSpec::Instantiate { id, scheme_id, result, monomorphic, .. } => {
                if let Some(sid) = scheme_id {
                    if let Some(scheme) = state.schemes.get(sid).cloned() {
                        if !monomorphic && !scheme.monomorphic {
                            let mut mapping = BTreeMap::new();
                            for q in &scheme.quantifiers {
                                let fresh = Ty::var(format!("{}#{}", q, id));
                                state.levels.insert(var_name(&fresh), state.current_level);
                                mapping.insert(q.clone(), fresh);
                            }
                            let copied = substitute(&scheme.body, &mapping);
                            if let Err(e) = state.unify(result.clone(), copied, id, &Proof::from([id.clone()])) { error = Some(e); }
                        } else if let Err(e) = state.unify(result.clone(), scheme.body.clone(), id, &Proof::from([id.clone()])) { error = Some(e); }
                    }
                }
                StepOutcome::Info
            }
            StepSpec::Equal(c) => {
                if disabled.contains(&c.id) { StepOutcome::Info } else {
                    active.insert(c.id.clone());
                    let mut proof = Proof::from([c.id.clone(), c.origin.clone()]);
                    match state.unify(c.left.clone(), c.right.clone(), &c.id, &proof) {
                        Ok(()) => StepOutcome::Unified { left: state.resolve_ty(&c.left), right: state.resolve_ty(&c.right), description: c.role.clone() },
                        Err(mut e) => {
                            decorate_error(&mut e, &c.span, &node_spans);
                            proof.extend(error_proof(&e));
                            set_error_proof(&mut e, proof);
                            error = Some(e.clone());
                            StepOutcome::Failed(e)
                        }
                    }
                }
            }
        };
        steps.push(SolverStep { index: idx, event: event.clone(), outcome });
    }

    let mut node_types = BTreeMap::new();
    for (k, ty) in &generated.node_types { node_types.insert(k.clone(), state.resolve_ty(ty)); }
    let root_type = node_types.get("root").cloned();
    result
}

fn solve_once(generated: &Generated, ast: &ExprNode, disabled: &BTreeSet<String>) -> SolveResult {
    let mut result = SolveResult { ok: false, root_type, node_types, steps, error, active_constraints: active, cores: Vec::new(), preferred_core: None };
    if let Some(e) = &result.error {
        let ids = error_proof(e).into_iter().filter(|id| id.starts_with("eq:")).collect::<BTreeSet<_>>();
        let initial = ids.into_iter().collect::<Vec<_>>();
        let minimal = minimize_core(generated, ast, &initial, e, disabled);
        let min_size = minimal.iter().map(|c| c.len()).min().unwrap_or(0);
        result.cores = minimal.iter().map(|members| core_from(members, e, min_size)).collect();
        result.preferred_core = result.cores.first().cloned();
    } else {
        result.ok = true;
    }
    result
}

impl Link {
    fn ensure(&mut self, vid: &Vid) {
        self.parent.entry(vid.clone()).or_insert_with(|| vid.clone());
        self.rank.entry(vid.clone()).or_insert(0);
        self.levels.entry(vid.clone()).or_insert(self.current_level);
    }
    fn find(&mut self, vid: &Vid) -> Vid {
        self.ensure(vid);
        let p = self.parent[vid].clone();
        if &p == vid { return vid.clone(); }
        let root = self.find(&p);
        self.parent.insert(vid.clone(), root.clone());
        root
    }
    fn alias(&mut self, a: &Vid, b: &Vid) {
        self.ensure(a); self.ensure(b);
        let ra = self.find(a);
        let rb = self.find(b);
        if ra != rb {
            let la = self.rank[&ra]; let lb = self.rank[&rb];
            if la < lb { self.parent.insert(ra, rb.clone()); }
            else if la > lb { self.parent.insert(rb, ra.clone()); }
            else { self.parent.insert(rb.clone(), ra.clone()); self.rank.insert(ra.clone(), la + 1); }
        }
    }
    fn resolve_ty(&mut self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var { vid, proof } => {
                let root = self.find(vid);
                if let Some(bound) = self.root_ty.get(&root).cloned() {
                    let mut resolved = self.resolve_ty(&bound);
                    resolved.add_proofs(proof);
                    resolved
                } else if let Some(link) = self.row_links.get(&root).cloned() {
                    let is_variant = link.variant;
                    let row = self.resolve_row(&link.row);
                    let mut out = if is_variant { Ty::Variant { row, proof: proof.clone() } } else { Ty::Record { row, proof: proof.clone() } };
                    out.add_proofs(proof);
                    out
                } else { Ty::Var { vid: root, proof: proof.clone() } }
            }
            Ty::Fun { from, to, proof } => Ty::fun(self.resolve_ty(from), self.resolve_ty(to)).with_proof(proof.clone()),
            Ty::Record { row, proof } => Ty::Record { row: self.resolve_row(row), proof: proof.clone() },
            Ty::Variant { row, proof } => Ty::Variant { row: self.resolve_row(row), proof: proof.clone() },
            other => other.clone(),
        }
    }
    fn resolve_row(&mut self, row: &Row) -> Row {
        let mut fields = BTreeMap::new();
        for (k, v) in &row.fields { fields.insert(k.clone(), self.resolve_ty(v)); }
        let mut extension = row.extension.clone();
        if let Some(ext) = extension.clone() {
            let root = self.find(&ext);
            if let Some(link2) = self.row_links.get(&root).cloned() {
                for (k, v) in link2.row.fields { fields.entry(k).or_insert_with(|| self.resolve_ty(&v)); }
                extension = link2.row.extension;
            } else { extension = Some(root); }
        }
        Row { fields, extension }
    }
    fn unify(&mut self, left: Ty, right: Ty, why: &str, proof: &Proof) -> Result<(), TypeError> {
        let l = self.resolve_ty(&left);
        let r = self.resolve_ty(&right);
        match (l.clone(), r.clone()) {
            (Ty::Unit { .. }, Ty::Unit { .. }) | (Ty::Int { .. }, Ty::Int { .. }) | (Ty::Bool { .. }, Ty::Bool { .. }) => Ok(()),
            (Ty::Var { vid: a, .. }, Ty::Var { vid: b, .. }) => { self.alias(&a, &b); Ok(()) }
            (Ty::Var { vid, .. }, other) | (other, Ty::Var { vid, .. }) => {
                if matches!((&l,&r), (Ty::Var { .. }, _)) { self.bind_var(vid, other, why, proof) } else { self.bind_var(vid, other, why, proof) }
            }
            (Ty::Fun { from: a, to: b, .. }, Ty::Fun { from: c, to: d, .. }) => {
                self.unify(*a, *c, why, proof)?;
                self.unify(*b, *d, why, proof)
            }
            (Ty::Record { row: lr, proof: lp }, Ty::Record { row: rr, proof: rp }) => {
                let mut p = lp; p.extend(rp); p.extend(proof.clone());
                self.unify_rows(lr, rr, false, why, &p)
            }
            (Ty::Variant { row: lr, proof: lp }, Ty::Variant { row: rr, proof: rp }) => {
                let mut p = lp; p.extend(rp); p.extend(proof.clone());
                self.unify_rows(lr, rr, true, why, &p)
            }
            _ => Err(TypeError::Mismatch {
                left: l, right: r, message: format!("cannot unify {} with {}", display_ty(&l), display_ty(&r)),
                missing: BTreeSet::new(), extra: BTreeSet::new(), spans: Vec::new(), proof: proof.clone(),
            }),
        }
    }
    fn bind_var(&mut self, vid: Vid, ty: Ty, why: &str, proof: &Proof) -> Result<(), TypeError> {
        if let Ty::Var { vid: other, .. } = &ty { self.alias(&vid, other); return Ok(()); }
        if occurs(&vid, &ty) {
            return Err(TypeError::Occurs { variable: vid, ty, message: "occurs check failed: this would construct an infinite type".into(), spans: Vec::new(), proof: proof.clone() });
        }
        match &ty {
            Ty::Record { row, .. } => {
                let root = self.find(&vid);
                self.row_links.insert(root, RowLink { row: row.clone(), variant: false });
            }
            Ty::Variant { row, .. } => {
                let root = self.find(&vid);
                self.row_links.insert(root, RowLink { row: row.clone(), variant: true });
            }
            _ => { let root = self.find(&vid); self.root_ty.insert(root, ty); }
        }
        let _ = why;
        Ok(())
    }
    fn unify_rows(&mut self, left: Row, right: Row, variant: bool, why: &str, proof: &Proof) -> Result<(), TypeError> {
        let left = self.resolve_row(&left);
        let right = self.resolve_row(&right);
        let lkeys = left.fields.keys().cloned().collect::<BTreeSet<_>>();
        let rkeys = right.fields.keys().cloned().collect::<BTreeSet<_>>();
        let common = lkeys.intersection(&rkeys).cloned().collect::<Vec<_>>();
        for label in common {
            let a = left.fields[&label].clone();
            let b = right.fields[&label].clone();
            self.unify(a, b, why, proof)?;
        }
        let missing = rkeys.difference(&lkeys).cloned().collect::<BTreeSet<_>>();
        let extra = lkeys.difference(&rkeys).cloned().collect::<BTreeSet<_>>();
        match (left.extension, right.extension) {
            (None, None) => {
                if missing.is_empty() && extra.is_empty() { Ok(()) } else {
                    Err(row_error(&self.resolve_row(&left), &self.resolve_row(&right), missing, extra, proof))
                }
            }
            (Some(ext), None) => {
                if !extra.is_empty() { return Err(row_error(&self.resolve_row(&left), &self.resolve_row(&right), BTreeSet::new(), extra, proof)); }
                let root = self.find(&ext);
                let mut only = BTreeMap::new();
                for label in &missing { only.insert(label.clone(), right.fields[label].clone()); }
                self.row_links.insert(root, RowLink { row: Row { fields: only, extension: None }, variant });
                Ok(())
            }
            (None, Some(ext)) => {
                if !missing.is_empty() { return Err(row_error(&self.resolve_row(&left), &self.resolve_row(&right), missing, BTreeSet::new(), proof)); }
                let root = self.find(&ext);
                let mut only = BTreeMap::new();
                for label in &extra { only.insert(label.clone(), left.fields[label].clone()); }
                self.row_links.insert(root, RowLink { row: Row { fields: only, extension: None }, variant });
                Ok(())
            }
            (Some(a), Some(b)) => {
                self.alias(&a, &b);
                Ok(())
            }
        }
    }
}

#[derive(Clone)]
struct RowLink { row: Row, variant: bool }


impl Ty {
    fn with_proof(self, proof: Proof) -> Ty {
        match self {
            Ty::Fun { from, to, .. } => Ty::Fun { from, to, proof },
            other => { let mut x = other; x.add_proofs(&proof); x }
        }
    }
}

pub fn occurs(vid: &Vid, ty: &Ty) -> bool {
    match ty {
        Ty::Var { vid: other, .. } => vid == other,
        Ty::Fun { from, to, .. } => occurs(vid, from) || occurs(vid, to),
        Ty::Record { row, .. } | Ty::Variant { row, .. } => row.fields.values().any(|t| occurs(vid, t)),
        _ => false,
    }
}

pub fn collect_vars(ty: &Ty) -> BTreeSet<Vid> {
    let mut out = BTreeSet::new();
    collect_vars_inner(ty, &mut out);
    out
}
fn collect_vars_inner(ty: &Ty, out: &mut BTreeSet<Vid>) {
    match ty {
        Ty::Var { vid, .. } => { out.insert(vid.clone()); }
        Ty::Fun { from, to, .. } => { collect_vars_inner(from, out); collect_vars_inner(to, out); }
        Ty::Record { row, .. } | Ty::Variant { row, .. } => {
            for t in row.fields.values() { collect_vars_inner(t, out); }
            if let Some(ext) = &row.extension { out.insert(ext.clone()); }
        }
        _ => {}
    }
}

fn row_error(left: &Ty, right: &Ty, missing: BTreeSet<String>, extra: BTreeSet<String>, proof: &Proof) -> TypeError {
    let mut parts = Vec::new();
    if !missing.is_empty() { parts.push(format!("missing field(s): {}", missing.iter().cloned().collect::<Vec<_>>().join(", "))); }
    if !extra.is_empty() { parts.push(format!("unexpected extra field(s): {}", extra.iter().cloned().collect::<Vec<_>>().join(", "))); }
    TypeError::Mismatch {
        left: left.clone(), right: right.clone(),
        message: format!("record rows disagree: {}", parts.join("; ")),
        missing, extra, spans: Vec::new(), proof: proof.clone(),
    }
}

pub fn display_ty(ty: &Ty) -> String {
    match ty {
        Ty::Unit { .. } => "()".into(),
        Ty::Int { .. } => "int".into(),
        Ty::Bool { .. } => "bool".into(),
        Ty::Var { vid, .. } => vid.clone(),
        Ty::Fun { from, to, .. } => format!("({} -> {})", display_ty(from), display_ty(to)),
        Ty::Record { row, .. } => display_row("{", "}", row),
        Ty::Variant { row, .. } => display_row("[", "]", row),
    }
}
fn display_row(open: &str, close: &str, row: &Row) -> String {
    let mut parts = row.fields.iter().map(|(k, v)| format!("{k} = {}", display_ty(v))).collect::<Vec<_>>();
    if let Some(ext) = &row.extension { parts.push(format!("... {ext}")); }
    format!("{open}{}{close}", parts.join(", "))
}
fn collect_spans(e: &ExprNode, map: &mut BTreeMap<String, Span>) {
    map.insert(e.id.clone(), e.span);
    match &e.node {
        Expr::Fun { body, .. } => collect_spans(body, map),
        Expr::App { fun, arg } => { collect_spans(fun, map); collect_spans(arg, map); }
        Expr::Let { value, body, .. } => { collect_spans(value, map); collect_spans(body, map); }
        Expr::Record { fields } => for f in fields { collect_spans(&f.value, map); },
        Expr::Field { record, .. } | Expr::Extend { value: _, rest: Some(record), .. } => collect_spans(record, map),
        Expr::Extend { value, rest: None, .. } => collect_spans(value, map),
        Expr::Extend { value, rest: Some(rest), .. } => { collect_spans(value, map); collect_spans(rest, map); }
        Expr::Variant { payload: Some(p), .. } => collect_spans(p, map),
        Expr::Match { scrutinee, arms } => {
            collect_spans(scrutinee, map);
            for arm in arms { collect_spans(&arm.body, map); }
        }
        Expr::Annotation { expr, .. } => collect_spans(expr, map),
        _ => {}
    }
}
fn decorate_error(error: &mut TypeError, span: &Span, spans: &BTreeMap<String, Span>) {
    let proof = error_proof(error);
    let mut all = proof.into_iter().filter_map(|p| spans.get(p.split('#').next().unwrap_or(&p)).copied()).collect::<Vec<_>>();
    all.push(*span);
    all.sort_by_key(|s| (s.start, s.end));
    all.dedup();
    match error { TypeError::Occurs { spans: s, .. } | TypeError::Mismatch { spans: s, .. } => *s = all }
}
pub fn error_proof(e: &TypeError) -> Proof {
    match e { TypeError::Occurs { proof, .. } | TypeError::Mismatch { proof, .. } => proof.clone() }
}
fn set_error_proof(e: &mut TypeError, proof: Proof) {
    match e { TypeError::Occurs { proof: p, .. } | TypeError::Mismatch { proof: p, .. } => *p = proof }
}

fn error_signature(e: &TypeError) -> String {
    match e {
        TypeError::Occurs { variable, ty, .. } => format!("occurs:{variable}:{}", display_ty(ty)),
        TypeError::Mismatch { missing, extra, .. } => {
            let m = missing.iter().cloned().collect::<Vec<_>>().join(",");
            let x = extra.iter().cloned().collect::<Vec<_>>().join(",");
            format!("mismatch:missing={m};extra={x}")
        }
    }
}

fn same_failure(a: &TypeError, b: &TypeError) -> bool {
    error_signature(a) == error_signature(b)
}

fn minimize_core(generated: &Generated, ast: &ExprNode, initial: &[String], original: &TypeError, disabled: &BTreeSet<String>) -> Vec<Vec<String>> {
    let mut current: BTreeSet<String> = initial.iter().cloned().collect();
    loop {
        let mut changed = false;
        for id in current.clone().iter() {
            let trial = current.iter().filter(|x| *x != id).cloned().collect::<BTreeSet<_>>();
            let mut all_disabled = disabled.clone();
            all_disabled.extend(trial);
            let result = solve_quiet(generated, ast, &all_disabled);
            if let Some(e) = &result.error {
                if same_failure(e, original) {
                    current = trial;
                    changed = true;
                    break;
                }
            }
        }
        if !changed { break; }
    }
    let base = current.into_iter().collect::<Vec<_>>();
    if base.len() <= 10 {
        let mut candidates = vec![base.clone()];
        for subset_size in 0..base.len() {
            for drop in combinations(&base, subset_size) {
                let trial: BTreeSet<String> = base.iter().filter(|x| !drop.contains(x)).cloned().collect();
                let mut all_disabled = disabled.clone();
                all_disabled.extend(disabled.iter().cloned());
                all_disabled.extend(base.iter().filter(|x| drop.contains(x)).cloned());
                let result = solve_quiet(generated, ast, &all_disabled);
                if let Some(e) = &result.error {
                    if same_failure(e, original) {
                        let member_vec = trial.into_iter().collect();
                        if !candidates.contains(&member_vec) { candidates.push(member_vec); }
                    }
                }
            }
        }
        let min = candidates.iter().map(Vec::len).min().unwrap_or(0);
        candidates.into_iter().filter(|c| c.len() == min).collect()
    } else { vec![base] }
}

fn combinations<T: Clone + Ord>(items: &[T], drop_count: usize) -> Vec<Vec<T>> {
    if drop_count == 0 { return vec![Vec::new()]; }
    if items.len() < drop_count { return Vec::new(); }
    if drop_count == items.len() { return vec![items.to_vec()]; }
    let head = items[0].clone();
    let mut without = combinations(&items[1..], drop_count);
    let mut with = combinations(&items[1..], drop_count - 1);
    for v in &mut with { v.push(head.clone()); v.sort(); }
    without.extend(with);
    without
}

fn solve_quiet(generated: &Generated, ast: &ExprNode, disabled: &BTreeSet<String>) -> SolveResult {
    let result = solve_once(generated, ast, disabled);
    result.cores.clear();
    result.preferred_core = None;
    result
}

fn core_from(members: &[String], error: &TypeError, min_size: usize) -> Core {
    Core {
        size: members.len(),
        members: members.to_vec(),
        explanation: match error {
            TypeError::Occurs { message, .. } => format!("{message}. Temporarily disabling one member shows which constraint introduces the recursive cycle."),
            TypeError::Mismatch { message, missing, extra, .. } => {
                let mut details = vec![message.clone()];
                if !missing.is_empty() { details.push(format!("Missing: {}.", missing.iter().cloned().collect::<Vec<_>>().join(", "))); }
                if !extra.is_empty() { details.push(format!("Extra: {}.", extra.iter().cloned().collect::<Vec<_>>().join(", "))); }
                details.join(" ")
            }
        },
        equally_small: members.len() == min_size,
    }
}
