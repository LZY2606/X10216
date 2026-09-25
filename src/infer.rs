//! Constraint-based Hindley–Milner inference engine.
//!
//! Each constraint records:
//!   * its `ConstraintKind`,
//!   * the AST node and the constraint sequence index that produced it,
//!   * a stable symbolic label for the fresh variables involved,
//!   * a `cache_key` that includes the lexical scope chain and the
//!     generalization environment at creation time.
//!
//! The engine can be run in two modes: a normal run that records every
//! unification step, and a "checking" run used by the unsat-core search,
//! which only reports success/failure.

use std::collections::{HashMap, HashSet};

use crate::syntax::{Ast, BinOp, BindId, Expr, NId, Span};
use crate::types::{Provenance, RowField, TId, TyKind, TypeStore};

#[derive(Clone, Debug)]
pub struct Scheme {
    /// Generalized variable symbolic labels at generalization time.
    pub quantified: Vec<String>,
    /// The rendered type snapshot (structural, store-independent).
    pub body: TyTree,
    /// Type ids inside the store of the run that produced the scheme.
    pub root: TId,
    pub bound: Vec<TId>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct TyTree {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub args: Vec<TyTree>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub fields: Vec<FieldTree>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub tail: Option<Box<TyTree>>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub from: Option<Box<TyTree>>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub to: Option<Box<TyTree>>,
    pub origin_node: Option<NId>,
    pub via_constraint: Option<usize>,
    pub note: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub var: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub rank: Option<u32>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct FieldTree {
    pub label: String,
    pub value: TyTree,
}

#[derive(Clone, Debug)]
pub enum ConstraintKind {
    /// Expect a variable occurrence to have the type of its binding scheme
    /// (after instantiation).
    Instantiate { var_ty: TId },
    /// Unify two types.
    Unify { left: TId, right: TId, why: String },
    /// Generalize the type bound to a let/top-level identifier.
    Generalize {
        binder: BindId,
        var_ty: TId,
        /// true => the binder is only generalized if its initializer is an
        /// expansive expression under the value restriction.
        monomorphic_only: bool,
    },
    /// Occurs check is recorded per-unification, but explicit constraints for
    /// record field access / variant construction are also materialized.
    Project {
        record_ty: TId,
        field_ty: TId,
        label: String,
        is_record: bool,
    },
}

#[derive(Clone, Debug)]
pub struct Constraint {
    pub id: usize,
    pub kind: ConstraintKind,
    pub origin_node: NId,
    pub span: Span,
    pub label: String,
    /// Symbolic ids participating, for stable fingerprinting.
    pub symbols: Vec<String>,
    /// Cache key: node key + scope hash + generalization env hash.
    pub cache_key: String,
    /// Rendered snapshots of the symbolic operands at creation.
    pub snapshot_left: Option<TyTree>,
    pub snapshot_right: Option<TyTree>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct FieldMismatch {
    pub label: String,
    /// true if field present on left/expected side but missing on right.
    pub missing: bool,
    pub kind: String, // "missing" | "extra"
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct TypeErrorReport {
    pub kind: String,
    pub message: String,
    pub span: Span,
    pub origin_node: NId,
    pub constraints: Vec<usize>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub expected: Option<TyTree>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub actual: Option<TyTree>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub missing_fields: Vec<FieldMismatch>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub infinite_var: Option<String>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct StepRecord {
    pub index: usize,
    pub constraint_id: usize,
    pub action: String,
    pub detail: String,
    pub span: Span,
    pub origin_node: NId,
    pub before_left: Option<TyTree>,
    pub before_right: Option<TyTree>,
    pub after: Option<TyTree>,
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub error_index: Option<usize>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct SchemeInfo {
    pub binder: BindId,
    pub name: String,
    pub quantified: Vec<String>,
    pub body: TyTree,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct BindingType {
    pub binder: BindId,
    pub name: String,
    pub ty: TyTree,
}

#[derive(Clone, Debug)]
struct EnvEntry {
    scheme: Option<Scheme>,
    /// Before generalization (inside the let's own scope), a plain type.
    monotype: Option<TId>,
    binder: BindId,
}

pub struct Engine<'a> {
    pub ast: &'a Ast,
    pub store: TypeStore,
    pub constraints: Vec<Constraint>,
    pub steps: Vec<StepRecord>,
    pub errors: Vec<TypeErrorReport>,
    pub schemes: HashMap<BindId, Scheme>,
    pub final_types: HashMap<NId, TId>,
    pub binding_types: Vec<BindingType>,
    pub disabled: HashSet<usize>,
    checking: bool,
    level: u32,
    sym_counter: usize,
    scope_segments: Vec<String>,
    genv_log: Vec<String>,
    constraint_id_by_creating_index: HashMap<usize, usize>,
    pub top_level: u32,
}

#[derive(Clone, Debug)]
pub enum UErr {
    Clash {
        left: TId,
        right: TId,
        left_desc: String,
        right_desc: String,
        origin: NId,
    },
    Occurs {
        var: TId,
        other: TId,
        origin: NId,
    },
    RowMismatch {
        is_record: bool,
        missing: Vec<FieldMismatch>,
        extra: Vec<FieldMismatch>,
        origin: NId,
    },
}

impl<'a> Engine<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Engine {
            ast,
            store: TypeStore::new(),
            constraints: Vec::new(),
            steps: Vec::new(),
            errors: Vec::new(),
            schemes: HashMap::new(),
            final_types: HashMap::new(),
            binding_types: Vec::new(),
            disabled: HashSet::new(),
            checking: false,
            level: 1,
            sym_counter: 0,
            scope_segments: Vec::new(),
            genv_log: Vec::new(),
            constraint_id_by_creating_index: HashMap::new(),
            top_level: 1,
        }
    }

    pub fn with_disabled(ast: &'a Ast, disabled: HashSet<usize>) -> Self {
        let mut e = Engine::new(ast);
        e.disabled = disabled;
        e
    }

    fn sym(&mut self, node: NId, role: &str) -> String {
        self.sym_counter += 1;
        let key = self.ast.nodes.get(node).map(|n| n.key.clone()).unwrap_or_else(|| "g".to_string());
        // Stable symbolic id: node key + role + local sequence. Local sequence
        // is needed when the same node introduces multiple fresh variables.
        format!("{}#{}#{}", key, role, self.sym_counter)
    }

    fn fresh(&mut self, node: NId, role: &str, is_row: bool, note: &str) -> TId {
        let label = self.sym(node, role);
        let rank = self.level;
        self.store.fresh(
            rank,
            is_row,
            label.clone(),
            Provenance {
                origin_node: Some(node),
                via_constraint: None,
                span: Some(self.ast.nodes[node].span),
                note: note.to_string(),
            },
        )
    }

    fn fresh_note(&mut self, prov: Provenance, label: &str, is_row: bool) -> TId {
        self.store.fresh(self.level, is_row, label.to_string(), prov)
    }

    fn enter_scope(&mut self, binder_key: &str) {
        self.level += 1;
        self.scope_segments.push(binder_key.to_string());
    }

    fn leave_scope(&mut self) {
        self.level -= 1;
        self.scope_segments.pop();
    }

    fn scope_hash(&self) -> String {
        self.scope_segments.join(">")
    }

    fn genv_hash(&self) -> String {
        self.genv_log.join(";")
    }

    fn cache_key(&self, node: NId, kind_tag: &str, symbols: &[String]) -> String {
        // Cache key explicitly contains scope and generalization environment.
        format!(
            "{}|{}|scope={}|genv={}|syms={}",
            kind_tag,
            self.ast.nodes.get(node).map(|n| n.key.clone()).unwrap_or_default(),
            self.scope_hash(),
            self.genv_hash(),
            symbols.join(",")
        )
    }

    fn con(&mut self, name: &str, args: Vec<TId>, prov: Provenance) -> TId {
        self.store.mk(
            TyKind::Con { name: name.to_string(), args },
            prov,
        )
    }

    fn int_ty(&mut self, node: NId) -> TId {
        self.con("int", vec![], Provenance::node(node, "整数字面量"))
    }
    fn str_ty(&mut self, node: NId) -> TId {
        self.con("string", vec![], Provenance::node(node, "字符串字面量"))
    }
    fn bool_ty(&mut self, node: NId) -> TId {
        self.con("bool", vec![], Provenance::node(node, "布尔字面量"))
    }
}

struct ExpandedRow {
    is_record: bool,
    fields: Vec<RowField>,
    tail: Option<TId>,
}

impl<'a> Engine<'a> {
    fn expand_row(&mut self, t: TId) -> ExpandedRow {
        let t = self.store.resolve(t);
        match self.store.get(t).kind.clone() {
            TyKind::Row { is_record, fields, tail } => {
                let mut all = fields;
                let mut cur_tail = tail;
                // absorb linked closed/open rows in the tail
                loop {
                    match cur_tail {
                        Some(tt) => {
                            let r = self.store.resolve(tt);
                            match self.store.get(r).kind.clone() {
                                TyKind::Row { fields: mut more, tail: next, .. } => {
                                    all.append(&mut more);
                                    cur_tail = next;
                                }
                                TyKind::Var(v) if v.is_row && v.link.is_some() => {
                                    cur_tail = Some(r);
                                    break;
                                }
                                _ => {
                                    cur_tail = Some(r);
                                    break;
                                }
                            }
                        }
                        None => break,
                    }
                }
                ExpandedRow { is_record, fields: all, tail: cur_tail }
            }
            _ => ExpandedRow { is_record: true, fields: Vec::new(), tail: Some(t) },
        }
    }

    fn occurs(&mut self, var: TId, in_ty: TId, is_row_var: bool) -> bool {
        let root = self.store.resolve(in_ty);
        if root == var {
            return true;
        }
        match self.store.get(root).kind.clone() {
            TyKind::Var(_) => false,
            TyKind::Con { args, .. } => args.into_iter().any(|a| self.occurs(var, a, is_row_var)),
            TyKind::Fun { from, to } => {
                self.occurs(var, from, is_row_var) || self.occurs(var, to, is_row_var)
            }
            TyKind::Row { fields, tail, .. } => {
                for f in &fields {
                    if self.occurs(var, f.value, is_row_var) {
                        return true;
                    }
                }
                if let Some(t) = tail {
                    self.occurs(var, t, is_row_var)
                } else {
                    false
                }
            }
        }
    }

    fn bind_var(&mut self, var: TId, to: TId) {
        let vrank = match &self.store.get(var).kind {
            TyKind::Var(v) => v.rank,
            _ => unreachable!(),
        };
        let trank = match &self.store.get(to).kind {
            TyKind::Var(v) => v.rank,
            _ => u32::MAX,
        };
        if trank < vrank {
            // point the other way
            if let TyKind::Var(v) = &mut self.store.nodes[to.0].kind {
                v.link = Some(var);
            }
        } else {
            if let TyKind::Var(v) = &mut self.store.nodes[var.0].kind {
                v.link = Some(to);
            }
        }
        // merge provenance
        let other_prov = self.store.get(to).prov.clone();
        self.store.nodes[var.0].prov.merge(&other_prov);
    }

    pub fn unify(&mut self, a: TId, b: TId, origin: NId) -> Result<(), UErr> {
        let a = self.store.resolve(a);
        let b = self.store.resolve(b);
        if a == b {
            return Ok(());
        }
        let av = matches!(self.store.get(a).kind, TyKind::Var(_));
        let bv = matches!(self.store.get(b).kind, TyKind::Var(_));
        if av {
            let is_row = matches!(&self.store.get(a).kind, TyKind::Var(v) if v.is_row);
            if self.occurs(a, b, is_row) {
                return Err(UErr::Occurs { var: a, other: b, origin });
            }
            self.bind_var(a, b);
            return Ok(());
        }
        if bv {
            let is_row = matches!(&self.store.get(b).kind, TyKind::Var(v) if v.is_row);
            if self.occurs(b, a, is_row) {
                return Err(UErr::Occurs { var: b, other: a, origin });
            }
            self.bind_var(b, a);
            return Ok(());
        }
        let ka = self.store.get(a).kind.clone();
        let kb = self.store.get(b).kind.clone();
        match (ka, kb) {
            (TyKind::Con { name: n1, args: a1 }, TyKind::Con { name: n2, args: a2 }) => {
                if n1 != n2 {
                    return Err(UErr::Clash {
                        left: a,
                        right: b,
                        left_desc: n1,
                        right_desc: n2,
                        origin,
                    });
                }
                for (x, y) in a1.into_iter().zip(a2.into_iter()) {
                    self.unify(x, y, origin)?;
                }
                Ok(())
            }
            (TyKind::Fun { from: f1, to: t1 }, TyKind::Fun { from: f2, to: t2 }) => {
                self.unify(f1, f2, origin)?;
                self.unify(t1, t2, origin)
            }
            (TyKind::Row { .. }, TyKind::Row { .. }) => self.unify_rows(a, b, origin),
            (ka, kb) => Err(UErr::Clash {
                left: a,
                right: b,
                left_desc: type_head_name(&ka),
                right_desc: type_head_name(&kb),
                origin,
            }),
        }
    }

    fn unify_rows(&mut self, a: TId, b: TId, origin: NId) -> Result<(), UErr> {
        let ra = self.expand_row(a);
        let rb = self.expand_row(b);
        if ra.is_record != rb.is_record {
            return Err(UErr::Clash {
                left: a,
                right: b,
                left_desc: if ra.is_record { "record".to_string() } else { "variant".to_string() },
                right_desc: if rb.is_record { "record".to_string() } else { "variant".to_string() },
                origin,
            });
        }
        let is_record = ra.is_record;

        // Fields common to both rows are unified. Fields present on only one
        // side flow into the other side's residual row (its unique fields plus
        // its tail). A closed side (tail = None) cannot absorb anything, which
        // is reported separately as missing vs extra fields.
        let mut shared: Vec<(TId, TId)> = Vec::new();
        let mut only_a: Vec<RowField> = Vec::new();
        let mut only_b: Vec<RowField> = Vec::new();
        for fa in &ra.fields {
            match rb.fields.iter().find(|fb| fb.label == fa.label) {
                Some(fb) => shared.push((fa.value, fb.value)),
                None => only_a.push(fa.clone()),
            }
        }
        for fb in &rb.fields {
            if !ra.fields.iter().any(|fa| fa.label == fb.label) {
                only_b.push(fb.clone());
            }
        }

        let closed_a = ra.tail.is_none();
        let closed_b = rb.tail.is_none();

        if closed_a || closed_b {
            // Classify differences from A's point of view:
            //   only_b => fields B has but A lacks  => "missing" on A
            //   only_a => fields A has but B lacks  => "extra" on A
            let mut problems: Vec<FieldMismatch> = Vec::new();
            for f in &only_b {
                problems.push(FieldMismatch { label: f.label.clone(), missing: true, kind: "missing".to_string() });
            }
            for f in &only_a {
                problems.push(FieldMismatch { label: f.label.clone(), missing: false, kind: "extra".to_string() });
            }
            // Only a real error if the *closed* side is being asked to absorb
            // fields, i.e. there are leftovers on the opposite side.
            let conflict = match (closed_a, closed_b) {
                (true, true) => !only_a.is_empty() || !only_b.is_empty(),
                (true, false) => !only_b.is_empty(),
                (false, true) => !only_a.is_empty(),
                _ => false,
            };
            if conflict {
                let missing = problems.iter().filter(|p| p.kind == "missing").cloned().collect();
                let extra = problems.iter().filter(|p| p.kind == "extra").cloned().collect();
                return Err(UErr::RowMismatch { is_record, missing, extra, origin });
            }
        }

        // Unify shared field types.
        for (x, y) in shared {
            self.unify(x, y, origin)?;
        }

        // Rebuild residual rows and reconcile tails.
        let tail_residual = |eng: &mut Engine, fields: Vec<RowField>, tail: Option<TId>| -> Option<TId> {
            if fields.is_empty() && matches!(tail, Some(t) if !matches!(eng.store.get(t).kind, TyKind::Row { .. })) {
                return tail;
            }
            Some(eng.store.mk(
                TyKind::Row { is_record, fields, tail },
                Provenance::node(origin, "剩余行"),
            ))
        };

        // A's tail must accept B's unique fields (and vice versa).
        if let Some(ta) = ra.tail {
            let residual = tail_residual(self, only_b, rb.tail);
            match residual {
                Some(r) => self.unify(ta, r, origin)?,
                None => {}
            }
        }
        if let Some(tb) = rb.tail {
            let residual = tail_residual(self, only_a, ra.tail);
            if let Some(r) = residual {
                self.unify(r, tb, origin)?;
            }
        }
        Ok(())
    }

}

fn type_head_name(k: &TyKind) -> String {
    match k {
        TyKind::Var(_) => "变量".to_string(),
        TyKind::Con { name, .. } => name.clone(),
        TyKind::Fun { .. } => "函数".to_string(),
        TyKind::Row { is_record, .. } => if *is_record { "记录".to_string() } else { "变体".to_string() },
    }
}

fn var_name(idx: usize) -> String {
    // 'a, 'b, ... 'z, 'a1, ...
    const BASE: usize = 26;
    let letter = (idx % BASE) as u8 as char;
    let round = idx / BASE;
    if round == 0 {
        format!("'{}", letter)
    } else {
        format!("'{}{}", letter, round)
    }
}

impl<'a> Engine<'a> {
    fn render_with(&mut self, t: TId, names: &mut HashMap<String, String>) -> TyTree {
        let t = self.store.resolve(t);
        let node = self.store.get(t).clone();
        match node.kind {
            TyKind::Var(v) => {
                let sym = self.store.sym(t).to_string();
                if !names.contains_key(&sym) {
                    names.insert(sym.clone(), var_name(names.len()));
                }
                let nm = names.get(&sym).cloned().unwrap();
                TyTree {
                    kind: "var".to_string(),
                    name: None,
                    args: vec![],
                    fields: vec![],
                    tail: None,
                    from: None,
                    to: None,
                    origin_node: node.prov.origin_node,
                    via_constraint: node.prov.via_constraint,
                    note: node.prov.note,
                    var: Some(nm),
                    rank: Some(v.rank),
                }
            }
            TyKind::Con { name, args } => {
                let args = args.into_iter().map(|a| self.render_with(a, names)).collect();
                TyTree {
                    kind: "con".to_string(),
                    name: Some(name),
                    args,
                    fields: vec![],
                    tail: None,
                    from: None,
                    to: None,
                    origin_node: node.prov.origin_node,
                    via_constraint: node.prov.via_constraint,
                    note: node.prov.note,
                    var: None,
                    rank: None,
                }
            }
            TyKind::Fun { from, to } => {
                let f = self.render_with(from, names);
                let tt = self.render_with(to, names);
                TyTree {
                    kind: "fun".to_string(),
                    name: None,
                    args: vec![],
                    fields: vec![],
                    tail: None,
                    from: Some(Box::new(f)),
                    to: Some(Box::new(tt)),
                    origin_node: node.prov.origin_node,
                    via_constraint: node.prov.via_constraint,
                    note: node.prov.note,
                    var: None,
                    rank: None,
                }
            }
            TyKind::Row { is_record, fields, tail } => {
                let fs = fields
                    .into_iter()
                    .map(|f| FieldTree { label: f.label, value: self.render_with(f.value, names) })
                    .collect();
                let tail = tail.map(|t| Box::new(self.render_with(t, names)));
                TyTree {
                    kind: if is_record { "record".to_string() } else { "variant".to_string() },
                    name: None,
                    args: vec![],
                    fields: fs,
                    tail,
                    from: None,
                    to: None,
                    origin_node: node.prov.origin_node,
                    via_constraint: node.prov.via_constraint,
                    note: node.prov.note,
                    var: None,
                    rank: None,
                }
            }
        }
    }

    pub fn render(&mut self, t: TId) -> TyTree {
        let mut names = HashMap::new();
        self.render_with(t, &mut names)
    }

    fn free_vars(&mut self, t: TId, out: &mut Vec<TId>, seen: &mut HashSet<usize>) {
        let t = self.store.resolve(t);
        if !seen.insert(t.0) {
            return;
        }
        match self.store.get(t).kind.clone() {
            TyKind::Var(_) => out.push(t),
            TyKind::Con { args, .. } => {
                for a in args {
                    self.free_vars(a, out, seen);
                }
            }
            TyKind::Fun { from, to } => {
                self.free_vars(from, out, seen);
                self.free_vars(to, out, seen);
            }
            TyKind::Row { fields, tail, .. } => {
                for f in fields {
                    self.free_vars(f.value, out, seen);
                }
                if let Some(t) = tail {
                    self.free_vars(t, out, seen);
                }
            }
        }
    }

    fn generalize(&mut self, root: TId) -> (Vec<TId>, TyTree) {
        let mut fvs = Vec::new();
        let mut seen = HashSet::new();
        self.free_vars(root, &mut fvs, &mut seen);
        let mut bound = Vec::new();
        for v in fvs {
            let rank = match &self.store.get(v).kind {
                TyKind::Var(vv) => vv.rank,
                _ => continue,
            };
            if rank >= self.level {
                // Prune: generalized variable is now "bound" in the scheme.
                if let TyKind::Var(vv) = &mut self.store.nodes[v.0].kind {
                    vv.rank = 0;
                }
                bound.push(v);
            }
        }
        let snap = self.render(root);
        (bound, snap)
    }

    fn instantiate(&mut self, scheme: &Scheme, origin: NId) -> TId {
        let mut mapping: HashMap<usize, TId> = HashMap::new();
        let bound: HashSet<usize> = scheme.bound.iter().map(|t| t.0).collect();
        self.copy_ty(scheme.root, &bound, &mut mapping, origin)
    }

    fn copy_ty(
        &mut self,
        t: TId,
        bound: &HashSet<usize>,
        mapping: &mut HashMap<usize, TId>,
        origin: NId,
    ) -> TId {
        let root = self.store.resolve(t);
        if let TyKind::Var(v) = self.store.get(root).kind.clone() {
            if v.rank == 0 || bound.contains(&root.0) {
                if let Some(existing) = mapping.get(&root.0) {
                    return *existing;
                }
                let base_sym = self.store.sym(root).to_string();
                let fresh = self.store.fresh(
                    self.level,
                    v.is_row,
                    format!("{}@inst{}", base_sym, self.sym_counter),
                    Provenance::node(origin, "实例化的多态变量"),
                );
                self.sym_counter += 1;
                mapping.insert(root.0, fresh);
                return fresh;
            }
            return root;
        }
        match self.store.get(root).kind.clone() {
            TyKind::Var(_) => unreachable!(),
            TyKind::Con { name, args } => {
                let args = args
                    .into_iter()
                    .map(|a| self.copy_ty(a, bound, mapping, origin))
                    .collect();
                self.con(&name, args, self.store.get(root).prov.clone())
            }
            TyKind::Fun { from, to } => {
                let f = self.copy_ty(from, bound, mapping, origin);
                let tt = self.copy_ty(to, bound, mapping, origin);
                self.store.mk(TyKind::Fun { from: f, to: tt }, self.store.get(root).prov.clone())
            }
            TyKind::Row { is_record, fields, tail } => {
                let fields = fields
                    .into_iter()
                    .map(|f| RowField {
                        label: f.label,
                        value: self.copy_ty(f.value, bound, mapping, origin),
                        span: f.span,
                    })
                    .collect();
                let tail = tail.map(|t| self.copy_ty(t, bound, mapping, origin));
                self.store.mk(TyKind::Row { is_record, fields, tail }, self.store.get(root).prov.clone())
            }
        }
    }
}

fn is_syntactic_value(ast: &Ast, n: NId) -> bool {
    match &ast.nodes[n].expr {
        Expr::Var { .. } | Expr::Int(_) | Expr::Str(_) | Expr::Bool(_) | Expr::Fun { .. } => true,
        Expr::Record { fields, extends } => {
            fields.iter().all(|f| is_syntactic_value(ast, f.value))
                && extends.as_ref().map(|e| is_syntactic_value(ast, *e)).unwrap_or(true)
        }
        Expr::Variant { payload, .. } => payload.as_ref().map(|p| is_syntactic_value(ast, *p)).unwrap_or(true),
        // Tuples don't exist; everything remaining is expansive.
        _ => false,
    }
}

impl<'a> Engine<'a> {
    fn snapshot_symbols(&mut self, syms: &[String]) -> Vec<Option<TyTree>> {
        // Find type id whose symbolic label matches.
        let mut out = Vec::new();
        for s in syms {
            let mut found = None;
            for (id, node) in self.store.nodes.iter().enumerate() {
                if let TyKind::Var(_) = node.kind {
                    if self.store.syms[id] == *s {
                        found = Some(TId(id));
                        break;
                    }
                }
            }
            out.push(found.map(|t| self.render(t)));
        }
        out
    }

    fn record_constraint(
        &mut self,
        kind: ConstraintKind,
        origin: NId,
        label: String,
        symbols: Vec<String>,
        kind_tag: &str,
    ) -> usize {
        let span = self.ast.nodes[origin].span;
        let id = self.constraints.len();
        let cache_key = self.cache_key(origin, kind_tag, &symbols);
        let (sl, sr) = match &kind {
            ConstraintKind::Unify { left, right, .. } => {
                (Some(self.render(*left)), Some(self.render(*right)))
            }
            ConstraintKind::Project { record_ty, field_ty, .. } => {
                (Some(self.render(*record_ty)), Some(self.render(*field_ty)))
            }
            ConstraintKind::Instantiate { var_ty } => (Some(self.render(*var_ty)), None),
            ConstraintKind::Generalize { var_ty, .. } => (Some(self.render(*var_ty)), None),
        };
        self.constraints.push(Constraint {
            id,
            kind,
            origin_node: origin,
            span,
            label,
            symbols,
            cache_key,
            snapshot_left: sl,
            snapshot_right: sr,
        });
        id
    }

    fn enact(&mut self, cid: usize) -> Result<(), UErr> {
        if self.disabled.contains(&cid) {
            if !self.checking {
                let c = &self.constraints[cid];
                self.steps.push(StepRecord {
                    index: self.steps.len(),
                    constraint_id: cid,
                    action: "skip-disabled".to_string(),
                    detail: "该约束被临时禁用".to_string(),
                    span: c.span,
                    origin_node: c.origin_node,
                    before_left: c.snapshot_left.clone(),
                    before_right: c.snapshot_right.clone(),
                    after: None,
                    ok: true,
                    error_index: None,
                });
            }
            return Ok(());
        }
        let kind = self.constraints[cid].kind.clone();
        let result: Result<(), UErr> = match &kind {
            ConstraintKind::Unify { left, right, .. } => {
                let (l, r) = (*left, *right);
                self.unify(l, r, self.constraints[cid].origin_node)
            }
            ConstraintKind::Project { record_ty, field_ty, label, is_record } => self.project_unify(
                *record_ty,
                *field_ty,
                label,
                *is_record,
                self.constraints[cid].origin_node,
            ),
            ConstraintKind::Instantiate { .. } | ConstraintKind::Generalize { .. } => Ok(()),
        };

        if let Err(e) = &result {
            if !self.checking {
                let report = self.build_error(e.clone(), cid);
                let eidx = self.errors.len();
                self.errors.push(report);
                let c = &self.constraints[cid];
                self.steps.push(StepRecord {
                    index: self.steps.len(),
                    constraint_id: cid,
                    action: "unify".to_string(),
                    detail: "统一失败".to_string(),
                    span: c.span,
                    origin_node: c.origin_node,
                    before_left: c.snapshot_left.clone(),
                    before_right: c.snapshot_right.clone(),
                    after: None,
                    ok: false,
                    error_index: Some(eidx),
                });
            }
        } else if !self.checking {
            let (label, span, origin_node, sl, sr) = {
                let c = &self.constraints[cid];
                (c.label.clone(), c.span, c.origin_node, c.snapshot_left.clone(), c.snapshot_right.clone())
            };
            let after = match &kind {
                ConstraintKind::Unify { left, .. } => Some(self.render(*left)),
                ConstraintKind::Project { field_ty, .. } => Some(self.render(*field_ty)),
                ConstraintKind::Instantiate { .. } | ConstraintKind::Generalize { .. } => None,
            };
            let action = match kind {
                ConstraintKind::Unify { .. } => "unify".to_string(),
                ConstraintKind::Project { .. } => "project".to_string(),
                ConstraintKind::Instantiate { .. } => "instantiate".to_string(),
                ConstraintKind::Generalize { .. } => "generalize".to_string(),
            };
            self.steps.push(StepRecord {
                index: self.steps.len(),
                constraint_id: cid,
                action,
                detail: label,
                span,
                origin_node,
                before_left: sl,
                before_right: sr,
                after,
                ok: true,
                error_index: None,
            });
        }
        result
    }

    fn project_unify(
        &mut self,
        record_ty: TId,
        field_ty: TId,
        label: &str,
        is_record: bool,
        origin: NId,
    ) -> Result<(), UErr> {
        let sym = format!("{}#rest#{}", self.ast.nodes[origin].key, label);
        let rest = self.fresh_note(Provenance::node(origin, "投影后的剩余行"), &sym, true);
        let field_row = self.store.mk(
            TyKind::Row {
                is_record,
                fields: vec![RowField { label: label.to_string(), value: field_ty, span: None }],
                tail: Some(rest),
            },
            Provenance::node(origin, "投影行"),
        );
        self.unify(record_ty, field_row, origin)
    }
}

impl<'a> Engine<'a> {
    fn emit_unify(&mut self, left: TId, right: TId, why: impl Into<String>, origin: NId) -> Result<(), ()> {
        let sl = self.store.sym(left).to_string();
        let sr = self.store.sym(right).to_string();
        let symbols = vec![sl, sr];
        let why_s: String = why.into();
        let cid = self.record_constraint(
            ConstraintKind::Unify { left, right, why: why_s.clone() },
            origin,
            format!("统一：{}", why_s),
            symbols,
            "U",
        );
        self.enact(cid).map_err(|_| ())
    }

    fn emit_project(
        &mut self,
        record_ty: TId,
        field_ty: TId,
        label: &str,
        is_record: bool,
        origin: NId,
    ) -> Result<(), ()> {
        let symbols = vec![self.store.sym(record_ty).to_string(), self.store.sym(field_ty).to_string()];
        let cid = self.record_constraint(
            ConstraintKind::Project { record_ty, field_ty, label: label.to_string(), is_record },
            origin,
            format!(
                "{}字段/标签 `{}`",
                if is_record { "投影记录" } else { "约束变体" },
                label
            ),
            symbols,
            "P",
        );
        self.enact(cid).map_err(|_| ())
    }

    pub fn run(mut self) -> Engine<'a> {
        self.checking = false;
        for top in 0..self.ast.tops.len() {
            let (rec, binder, init) = {
                let t = &self.ast.tops[top];
                (t.rec, t.binder, t.init)
            };
            self.gen_top(rec, binder, init);
        }
        self
    }

    fn gen_top(&mut self, rec: bool, binder: BindId, init: NId) {
        if rec {
            let bt = self.fresh(init, "rec", false, "递归绑定的类型");
            self.enter_scope(&self.ast.binders[binder].key);
            let body = self.render(bt);
            self.schemes.insert(
                binder,
                Scheme { quantified: vec![], body, root: bt, bound: vec![] },
            );
            let it = self.gen(init);
            let _ = self.emit_unify(bt, it, "递归绑定与其定义同型", init);
            self.leave_scope();
            let final_t = self.store.resolve(bt);
            let bname = self.ast.binders[binder].name.clone();
            let ty = self.render(final_t);
            self.binding_types.push(BindingType { binder, name: bname, ty });
        } else {
            let it = self.gen(init);
            let expansive = !is_syntactic_value(self.ast, init);
            let (bound, body) = if expansive {
                (Vec::new(), self.render(it))
            } else {
                self.generalize(it)
            };
            let quant_syms: Vec<String> =
                bound.iter().map(|t| self.store.sym(*t).to_string()).collect();
            self.genv_log.push(format!("{}:{}", self.ast.binders[binder].key, quant_syms.join(",")));
            self.schemes.insert(
                binder,
                Scheme { quantified: quant_syms, body, root: it, bound },
            );
            let final_t = self.store.resolve(it);
            let bname = self.ast.binders[binder].name.clone();
            let ty = self.render(final_t);
            self.binding_types.push(BindingType { binder, name: bname, ty });
        }
    }
}

fn why_str(s: &str) -> String {
    s.to_string()
}

pub struct AnalysisTypes<'a> {
    pub engine: Engine<'a>,
}

impl<'a> Engine<'a> {
    fn gen(&mut self, n: NId) -> TId {
        let expr = self.ast.nodes[n].expr.clone();
        match expr {
            Expr::Int(_) => self.int_ty(n),
            Expr::Str(_) => self.str_ty(n),
            Expr::Bool(_) => self.bool_ty(n),
            Expr::Var { resolved, .. } => match resolved {
                Some(b) => {
                    let sc = self.schemes.get(&b).cloned();
                    let (vt, instantiated) = match sc {
                        Some(sc) => {
                            let vt = self.fresh(n, "var", false, "变量出现处的类型");
                            let copy = self.instantiate(&sc, n);
                            (vt, copy)
                        }
                        None => {
                            let vt = self.fresh(n, "var", false, "变量出现处的类型");
                            (vt, vt)
                        }
                    };
                    let syms = vec![self.store.sym(vt).to_string()];
                    let cid = self.record_constraint(
                        ConstraintKind::Instantiate { var_ty: vt },
                        n,
                        format!("实例化 {} 的类型方案", self.ast.binders[b].name),
                        syms,
                        "I",
                    );
                    let _ = self.enact(cid);
                    let _ = self.emit_unify(vt, instantiated, "变量 = 其实例化方案", n);
                    vt
                }
                None => self.fresh(n, "unbound", false, "未绑定变量"),
            },
            Expr::Fun { param, body } => {
                let pt = self.fresh(n, "param", false, "函数参数类型");
                self.enter_scope(&self.ast.binders[param].key);
                // param is a monomorphic scheme in scope
                {
                    let body = self.render(pt);
                    self.schemes.insert(
                        param,
                        Scheme { quantified: vec![], body, root: pt, bound: vec![] },
                    );
                }
                let bt = self.gen(body);
                self.leave_scope();
                self.store.mk(
                    TyKind::Fun { from: pt, to: bt },
                    Provenance::node(n, "函数类型"),
                )
            }
            Expr::App { fun, arg } => {
                let ft = self.gen(fun);
                let at = self.gen(arg);
                let rt = self.fresh(n, "ret", false, "应用结果类型");
                let want = self.store.mk(
                    TyKind::Fun { from: at, to: rt },
                    Provenance::node(n, "应用处期望的函数类型"),
                );
                let _ = self.emit_unify(ft, want, "应用：被调用者是函数", n);
                rt
            }
            Expr::BinOp { op, left, right } => {
                let lt = self.gen(left);
                let rt = self.gen(right);
                let result = match op {
                    BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Slash => {
                        let i1 = self.int_ty(n);
                        let i2 = self.int_ty(n);
                        let _ = self.emit_unify(lt, i1, "左操作数是 int", n);
                        let _ = self.emit_unify(rt, i2, "右操作数是 int", n);
                        self.int_ty(n)
                    }
                    BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                        let i1 = self.int_ty(n);
                        let i2 = self.int_ty(n);
                        let _ = self.emit_unify(lt, i1, "比较左操作数是 int", n);
                        let _ = self.emit_unify(rt, i2, "比较右操作数是 int", n);
                        self.bool_ty(n)
                    }
                    BinOp::EqEq => {
                        let _ = self.emit_unify(lt, rt, "相等比较两端同型", n);
                        self.bool_ty(n)
                    }
                };
                result
            }
            Expr::If { cond, then_e, else_e } => {
                let ct = self.gen(cond);
                let bt = self.bool_ty(n);
                let _ = self.emit_unify(ct, bt, "条件是 bool", n);
                let tt = self.gen(then_e);
                let et = self.gen(else_e);
                let _ = self.emit_unify(tt, et, "两个分支同型", n);
                tt
            }
            Expr::Record { fields, extends } => {
                let mut rf: Vec<RowField> = Vec::new();
                for f in &fields {
                    let vt = self.gen(f.value);
                    rf.push(RowField { label: f.label.clone(), value: vt, span: Some(f.span) });
                }
                let tail = match extends {
                    Some(e) => Some(self.gen(e)),
                    None => None,
                };
                self.store.mk(
                    TyKind::Row { is_record: true, fields: rf, tail },
                    Provenance::node(n, "记录类型"),
                )
            }
            Expr::FieldGet { record, label, label_span } => {
                let rt = self.gen(record);
                let ft = self.fresh(n, "field", false, &format!("字段 `{}` 的类型", label));
                let _ = self.emit_project(rt, ft, &label, true, n);
                let _ = label_span;
                ft
            }
            Expr::Variant { tag, tag_span, payload } => {
                let (pt, field) = match payload {
                    Some(p) => (Some(self.gen(p)), true),
                    None => (None, false),
                };
                // variant row: { Tag(p) | rho }, rho fresh open
                let rho = self.fresh_note(
                    Provenance::node(n, "变体的其余标签"),
                    &format!("{}#vrho", self.ast.nodes[n].key),
                    true,
                );
                let fields = match pt {
                    Some(pt) => vec![RowField { label: tag.clone(), value: pt, span: Some(tag_span) }],
                    None => vec![RowField {
                        label: tag.clone(),
                        value: self.con("unit", vec![], Provenance::node(n, "无载荷标签")),
                        span: Some(tag_span),
                    }],
                };
                let _ = field;
                self.store.mk(
                    TyKind::Row { is_record: false, fields, tail: Some(rho) },
                    Provenance::node(n, "变体类型"),
                )
            }
            Expr::Match { scrutinee, arms } => {
                let st = self.gen(scrutinee);
                let result = self.fresh(n, "match", false, "match 结果类型");
                let mut combined_fields: Vec<RowField> = Vec::new();
                let rho = self.fresh_note(
                    Provenance::node(n, "未匹配标签"),
                    &format!("{}#arho", self.ast.nodes[n].key),
                    true,
                );
                let mut first_body_ty = None;
                for arm in &arms {
                    let payload_ty = if arm.payload.is_some() {
                        Some(self.fresh(arm.body, "tagarg", false, "标签载荷类型"))
                    } else {
                        None
                    };
                    if let Some((_, _, bid)) = arm.payload {
                        let bid = bid;
                        let pt = payload_ty.unwrap();
                        self.enter_scope(&self.ast.binders[bid].key);
                        let body = self.render(pt);
                        self.schemes.insert(
                            bid,
                            Scheme { quantified: vec![], body, root: pt, bound: vec![] },
                        );
                    }
                    let body_ty = self.gen(arm.body);
                    if arm.payload.is_some() {
                        self.leave_scope();
                    }
                    if first_body_ty.is_none() {
                        first_body_ty = Some(body_ty);
                    }
                    let _ = self.emit_unify(result, body_ty, "所有 match 分支同型", arm.body);
                    let val = match payload_ty {
                        Some(pt) => pt,
                        None => self.con("unit", vec![], Provenance::node(n, "无载荷标签")),
                    };
                    combined_fields.push(RowField { label: arm.tag.clone(), value: val, span: Some(arm.span) });
                }
                let expected = self.store.mk(
                    TyKind::Row { is_record: false, fields: combined_fields, tail: Some(rho) },
                    Provenance::node(n, "期望的变体行"),
                );
                let _ = self.emit_unify(st, expected, "scrutinee 覆盖这些标签", n);
                result
            }
            Expr::Let { rec, binder, init, next } => {
                if rec {
                    let bt = self.fresh(init, "rec", false, "递归绑定类型");
                    self.enter_scope(&self.ast.binders[binder].key);
                    {
                        let body = self.render(bt);
                        self.schemes.insert(
                            binder,
                            Scheme { quantified: vec![], body, root: bt, bound: vec![] },
                        );
                    }
                    let it = self.gen(init);
                    let _ = self.emit_unify(bt, it, "递归绑定与其定义同型", init);
                    let nt = self.gen(next);
                    self.leave_scope();
                    nt
                } else {
                    let it = self.gen(init);
                    let expansive = !is_syntactic_value(self.ast, init);
                    let (bound, body) = if expansive {
                        (Vec::new(), self.render(it))
                    } else {
                        self.generalize(it)
                    };
                    let quant: Vec<String> =
                        bound.iter().map(|t| self.store.sym(*t).to_string()).collect();
                    // record generalization constraint
                    let cid = self.record_constraint(
                        ConstraintKind::Generalize { binder, var_ty: it, monomorphic_only: expansive },
                        init,
                        format!(
                            "泛化 {}（{}）",
                            self.ast.binders[binder].name,
                            if expansive { "value restriction：不泛化" } else { "泛化自由类型变量" }
                        ),
                        quant.clone(),
                        "G",
                    );
                    let _ = self.enact(cid);
                    self.genv_log
                        .push(format!("{}:{}", self.ast.binders[binder].key, quant.join(",")));
                    self.schemes.insert(
                        binder,
                        Scheme { quantified: quant, body, root: it, bound },
                    );
                    self.enter_scope(&self.ast.binders[binder].key);
                    let nt = self.gen(next);
                    self.leave_scope();
                    nt
                }
            }
        }
    }
}

impl<'a> Engine<'a> {
    fn build_error(&mut self, e: UErr, cid: usize) -> TypeErrorReport {
        let c_origin = self.constraints[cid].origin_node;
        let span = self.constraints[cid].span;
        match e {
            UErr::Clash { left, right, left_desc, right_desc, origin } => {
                let lt = self.render(left);
                let rt = self.render(right);
                TypeErrorReport {
                    kind: "clash".to_string(),
                    message: format!(
                        "无法把 {} 与 {} 统一：一边是 {}，另一边是 {}。",
                        render_type_compact(&lt),
                        render_type_compact(&rt),
                        describe_head(&left_desc),
                        describe_head(&right_desc)
                    ),
                    span,
                    origin_node: origin,
                    constraints: vec![cid],
                    expected: Some(lt),
                    actual: Some(rt),
                    missing_fields: vec![],
                    infinite_var: None,
                }
            }
            UErr::Occurs { var, other, origin } => {
                let vt = self.render(var);
                let ot = self.render(other);
                let vname = vt.var.clone().unwrap_or_else(|| "?".to_string());
                TypeErrorReport {
                    kind: "occurs".to_string(),
                    message: format!(
                        "无限类型：变量 {} 出现在它自身的类型 {} 中（occurs-check 失败）。这通常意味着自应用，例如把函数作用于自身。",
                        vname,
                        render_type_compact(&ot)
                    ),
                    span: self.ast.nodes[c_origin].span,
                    origin_node: origin,
                    constraints: vec![cid],
                    expected: Some(vt),
                    actual: Some(ot),
                    missing_fields: vec![],
                    infinite_var: Some(vname),
                }
            }
            UErr::RowMismatch { is_record, missing, extra, origin } => {
                let what = if is_record { "记录" } else { "变体" };
                let mut parts: Vec<String> = Vec::new();
                if !missing.is_empty() {
                    let labels = missing
                        .iter()
                        .map(|f| format!("`{}`", f.label))
                        .collect::<Vec<_>>()
                        .join("、");
                    parts.push(format!("缺少 {} 字段/标签 {}", what, labels));
                }
                if !extra.is_empty() {
                    let labels = extra
                        .iter()
                        .map(|f| format!("`{}`", f.label))
                        .collect::<Vec<_>>()
                        .join("、");
                    parts.push(format!("多出 {} 字段/标签 {}", what, labels));
                }
                TypeErrorReport {
                    kind: "row".to_string(),
                    message: format!("{} 的行不兼容：{}。", what, parts.join("；")),
                    span,
                    origin_node: origin,
                    constraints: vec![cid],
                    expected: None,
                    actual: None,
                    missing_fields: {
                        let mut all = missing;
                        all.extend(extra);
                        all
                    },
                    infinite_var: None,
                }
            }
        }
    }
}

pub fn render_type_compact(t: &TyTree) -> String {
    match t.kind.as_str() {
        "var" => t.var.clone().unwrap_or_else(|| "?".to_string()),
        "con" => t.name.clone().unwrap_or_else(|| "?".to_string()),
        "fun" => format!(
            "({} -> {})",
            t.from.as_ref().map(|f| render_type_compact(f)).unwrap_or_default(),
            t.to.as_ref().map(|f| render_type_compact(f)).unwrap_or_default()
        ),
        "record" | "variant" => {
            let open = matches!(t.tail, Some(_));
            let body = t
                .fields
                .iter()
                .map(|f| format!("{}: {}", f.label, render_type_compact(&f.value)))
                .collect::<Vec<_>>()
                .join(", ");
            if t.kind == "record" {
                if open {
                    format!("{{ {}{} }}", body, if body.is_empty() { "..".to_string() } else { ", ..".to_string() })
                } else {
                    format!("{{ {} }}", body)
                }
            } else if open {
                format!("[{}{}]", body, if body.is_empty() { "..".to_string() } else { ", ..".to_string() })
            } else {
                format!("[{}]", body)
            }
        }
        _ => "?".to_string(),
    }
}

fn describe_head(s: &str) -> String {
    match s {
        "record" => "记录".to_string(),
        "variant" => "变体".to_string(),
        "int" => "int".to_string(),
        "string" => "string".to_string(),
        "bool" => "bool".to_string(),
        "unit" => "unit".to_string(),
        other => other.to_string(),
    }
}
