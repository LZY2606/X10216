//! Union-find based unifier with occurs checking and row polymorphism.
//!
//! Every binding action is recorded as a [`Step`] so the UI can replay
//! inference. Unification failures carry structured, explainable payloads
//! rather than a plain "cannot unify" string.

use std::cell::RefCell;
use std::rc::Rc;

use crate::types::{Origin, Row, Span, TyKind, TypeArena, TypeId};

#[derive(Clone, Debug)]
pub enum StepKind {
    BindVar { var: u32, to: TypeId },
    Alias { a: u32, b: u32 },
    Equate { a: TypeId, b: TypeId },
    RowMerge { tail: u32, fields: Vec<String> },
    RowClose { tail: u32 },
}

#[derive(Clone, Debug)]
pub struct Step {
    pub index: u32,
    pub constraint: u32,
    pub kind: StepKind,
    pub summary: String,
}

#[derive(Clone, Debug)]
pub enum UnifyErrorKind {
    Mismatch,
    InfiniteType { var: u32, cycle_in: TypeId },
    RowMismatch { missing: Vec<String>, extra: Vec<String> },
    VariantMismatch { missing: Vec<String>, extra: Vec<String> },
    Arity,
}

#[derive(Clone, Debug)]
pub struct UnifyError {
    pub kind: UnifyErrorKind,
    pub left: TypeId,
    pub right: TypeId,
    pub left_span: Option<Span>,
    pub right_span: Option<Span>,
    pub step: u32,
    pub constraint: u32,
}

#[derive(Clone)]
struct Slot {
    parent: u32,
    rank: u32,
    /// Generalization level (lexical depth at which the var was created).
    level: u32,
}

pub struct Solver {
    slots: RefCell<Vec<Slot>>,
    pub steps: RefCell<Vec<Step>>,
    /// (var, representative type node it is forced to)
    var_nodes: RefCell<Vec<TypeId>>,
    /// Lazily allocated canonical node for a union-find root variable.
    synth: RefCell<std::collections::HashMap<u32, TypeId>>,
}

impl Solver {
    pub fn new() -> Self {
        Solver {
            slots: RefCell::new(Vec::new()),
            steps: RefCell::new(Vec::new()),
            var_nodes: RefCell::new(Vec::new()),
            synth: RefCell::new(std::collections::HashMap::new()),
        }
    }

    pub fn new_var(&self, level: u32) -> u32 {
        let mut slots = self.slots.borrow_mut();
        let id = slots.len() as u32;
        slots.push(Slot { parent: id, rank: 0, level });
        self.var_nodes.borrow_mut().push(TypeId(u32::MAX));
        id
    }

    fn find(&self, mut x: u32) -> u32 {
        let slots = self.slots.borrow();
        let mut root = x;
        while slots[root as usize].parent != root {
            root = slots[root as usize].parent;
        }
        drop(slots);
        // path compression
        let mut slots = self.slots.borrow_mut();
        while slots[x as usize].parent != x {
            let next = slots[x as usize].parent;
            slots[x as usize].parent = root;
            x = next;
        }
        root
    }

    fn level_of(&self, x: u32) -> u32 {
        let r = self.find(x);
        self.slots.borrow()[r as usize].level
    }

    /// Resolve a type node, following any unification variables.
    pub fn resolve(&self, arena: &TypeArena, id: TypeId) -> TypeId {
        let mut cur = id;
        for _ in 0..64 {
            let k = arena.view(cur, |n| n.kind.clone());
            match k {
                TyKind::Var(v) => {
                    let root = self.find(v);
                    let bound = self.var_nodes.borrow()[root as usize];
                    if bound.0 == u32::MAX {
                        if root == v {
                            return cur;
                        }
                        return *self.synth.borrow_mut().entry(root).or_insert_with(|| {
                            arena.alloc(TyKind::Var(root),
                                Origin::Solved { step: u32::MAX }, None)
                        });
                    }
                    let next = bound;
                    if next == cur {
                        return cur;
                    }
                    cur = next;
                }
                _ => return cur,
            }
        }
        cur
    }

    /// Resolve but return the root variable if unbound.
    fn resolve_var(&self, arena: &TypeArena, id: TypeId) -> (TypeId, Option<u32>) {
        let r = self.resolve(arena, id);
        if let TyKind::Var(v) = arena.view(r, |n| n.kind.clone()) {
            (r, Some(v))
        } else {
            (r, None)
        }
    }

    pub fn record_step(&self, constraint: u32, kind: StepKind, summary: String) -> u32 {
        let mut steps = self.steps.borrow_mut();
        let index = steps.len() as u32;
        steps.push(Step { index, constraint, kind, summary });
        index
    }

    fn set_node(&self, root: u32, node: TypeId) {
        let r = self.find(root);
        self.var_nodes.borrow_mut()[r as usize] = node;
    }

    fn alias(&self, a: u32, b: u32, constraint: u32) {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb { return; }
        let (lo, hi) = {
            let slots = self.slots.borrow();
            let sa = &slots[ra as usize];
            let sb = &slots[rb as usize];
            if sa.rank > sb.rank { (rb, ra) }
            else if sa.rank < sb.rank { (ra, rb) }
            else { (ra, rb) }
        };
        // merge node binding
        let bound_lo = self.var_nodes.borrow()[lo as usize];
        let bound_hi = self.var_nodes.borrow()[hi as usize];
        {
            let mut slots = self.slots.borrow_mut();
            slots[lo as usize].parent = hi;
            if slots[lo as usize].rank == slots[hi as usize].rank {
                slots[hi as usize].rank += 1;
            }
            slots[hi as usize].level = slots[hi as usize].level.min(slots[lo as usize].level);
        }
        if bound_hi.0 == u32::MAX && bound_lo.0 != u32::MAX {
            self.var_nodes.borrow_mut()[hi as usize] = bound_lo;
        }
        self.record_step(constraint, StepKind::Alias { a: ra, b: rb },
            format!("?{ra} ≡ ?{rb}"));
    }

    /// Bind unbound variable `v` (or its representative) to structural node.
    fn bind(&self, arena: &TypeArena, v: u32, node: TypeId,
            constraint: u32) -> Result<(), UnifyError> {
        let root = self.find(v);
        let existing = self.var_nodes.borrow()[root as usize];
        if existing.0 != u32::MAX {
            return self.unify(arena, existing, node, constraint);
        }
        if self.occurs(arena, root, node) {
            return Err(UnifyError {
                kind: UnifyErrorKind::InfiniteType { var: root, cycle_in: node },
                left: arena.alloc(TyKind::Var(root), Origin::Solved { step: 0 }, None),
                right: node,
                left_span: None,
                right_span: arena.view(node, |n| n.span),
                step: self.steps.borrow().len() as u32,
                constraint,
            });
        }
        self.set_node(root, node);
        let desc = arena.view(node, |n| describe_kind(n));
        self.record_step(constraint, StepKind::BindVar { var: root, to: node },
            format!("?{root} := {desc}"));
        Ok(())
    }

    fn occurs(&self, arena: &TypeArena, v: u32, id: TypeId) -> bool {
        self.occurs_depth(arena, v, id, 0)
    }

    fn occurs_depth(&self, arena: &TypeArena, v: u32, id: TypeId, depth: u32) -> bool {
        if depth > 128 { return false; }
        let k = arena.view(id, |n| n.kind.clone());
        match k {
            TyKind::Var(w) => {
                let root = self.find(w);
                if root == v { return true; }
                let bound = self.var_nodes.borrow()[root as usize];
                bound.0 != u32::MAX && self.occurs_depth(arena, v, bound, depth + 1)
            }
            TyKind::Arr(a, b) =>
                self.occurs_depth(arena, v, a, depth + 1)
                    || self.occurs_depth(arena, v, b, depth + 1),
            TyKind::Record(r) | TyKind::Variant(r) => {
                // Row tails may legitimately knot back to the same row
                // variable (open row closure), so they do not participate in
                // the occurs check; only carried payload types do.
                r.fields.iter().any(|(_, t)| self.occurs_depth(arena, v, *t, depth + 1))
            }
            TyKind::Con(_) => false,
        }
    }

    pub fn unify(&self, arena: &TypeArena, a: TypeId, b: TypeId,
            constraint: u32) -> Result<(), UnifyError> {
        let (ar, av) = self.resolve_var(arena, a);
        let (br, bv) = self.resolve_var(arena, b);
        match (av, bv) {
            (Some(x), Some(y)) => {
                let bx = self.var_nodes.borrow()[self.find(x) as usize];
                let by = self.var_nodes.borrow()[self.find(y) as usize];
                match (bx.0 != u32::MAX, by.0 != u32::MAX) {
                    (true, true) => self.unify(arena, bx, by, constraint),
                    (true, false) => self.bind(arena, y, bx, constraint),
                    (false, true) => self.bind(arena, x, by, constraint),
                    (false, false) => { self.alias(x, y, constraint); Ok(()) }
                }
            }
            (Some(x), None) => self.bind(arena, x, br, constraint),
            (None, Some(y)) => self.bind(arena, y, ar, constraint),
            (None, None) => {
                let ka = arena.view(ar, |n| n.kind.clone());
                let kb = arena.view(br, |n| n.kind.clone());
                match (&ka, &kb) {
                    (TyKind::Con(x), TyKind::Con(y)) if x == y => {
                        self.record_step(constraint,
                            StepKind::Equate { a: ar, b: br },
                            format!("{x} ≡ {y}"));
                        Ok(())
                    }
                    (TyKind::Arr(a1, a2), TyKind::Arr(b1, b2)) => {
                        self.record_step(constraint,
                            StepKind::Equate { a: ar, b: br },
                            "function types".into());
                        self.unify(arena, *a1, *b1, constraint)?;
                        self.unify(arena, *a2, *b2, constraint)
                    }
                    (TyKind::Record(ra), TyKind::Record(rb)) => {
                        self.unify_rows(arena, ra, rb, false, constraint)
                    }
                    (TyKind::Variant(ra), TyKind::Variant(rb)) => {
                        self.unify_rows(arena, ra, rb, true, constraint)
                    }
                    _ => {
                        let ls = arena.view(ar, |n| n.span);
                        let rs = arena.view(br, |n| n.span);
                        Err(UnifyError {
                            kind: UnifyErrorKind::Mismatch,
                            left: ar, right: br,
                            left_span: ls, right_span: rs,
                            step: self.steps.borrow().len() as u32,
                            constraint,
                        })
                    }
                }
            }
        }
    }

    /// Row unification that keeps missing vs extra fields distinct.
    ///
    /// Convention: `left` is the value provided, `right` the shape expected.
    /// Fields present in `right` but absent (and impossible) in `left` are
    /// `missing`; fields present only in `left` are `extra`. Two open rows
    /// always unify (rows are polymorphic); clashes are reported for closed
    /// boundaries.
    fn unify_rows(&self, arena: &TypeArena, ra: &Row, rb: &Row, variant: bool,
            constraint: u32) -> Result<(), UnifyError> {
        let (fa, tail_a) = self.flatten_row(arena, ra);
        let (fb, tail_b) = self.flatten_row(arena, rb);

        let name_a: std::collections::BTreeSet<&str> =
            fa.iter().map(|(n, _)| n.as_ref()).collect();
        let name_b: std::collections::BTreeSet<&str> =
            fb.iter().map(|(n, _)| n.as_ref()).collect();

        // Shared fields: field types must unify.
        for (n, ta) in &fa {
            if let Some(tb) = fb.iter().find(|(m, _)| m == n).map(|(_, t)| *t) {
                self.unify(arena, *ta, tb, constraint)?;
            }
        }

        let only_b: Vec<String> =
            name_b.difference(&name_a).map(|s| s.to_string()).collect();
        let only_a: Vec<String> =
            name_a.difference(&name_b).map(|s| s.to_string()).collect();

        match (tail_a, tail_b) {
            (Some(ta), Some(tb)) => {
                // Both open: share a residual variable. Each tail denotes the
                // other side's exclusive fields plus the shared residual.
                let ra2 = self.find(ta);
                let rb2 = self.find(tb);
                let residual = if ra2 == rb2 {
                    ra2
                } else {
                    let fresh = self.new_var(self.level_of(ra2).min(self.level_of(rb2)));
                    self.alias(fresh, ra2, constraint);
                    self.alias(fresh, rb2, constraint);
                    fresh
                };
                let rest_for_a = self.row_of_names(arena, &fb, &only_b,
                    Some(self.find(residual)), variant, constraint);
                self.bind(arena, self.find(ra2), rest_for_a, constraint)?;
                let rest_for_b = self.row_of_names(arena, &fa, &only_a,
                    Some(self.find(residual)), variant, constraint);
                self.bind(arena, self.find(rb2), rest_for_b, constraint)?;
            }
            (Some(ta), None) => {
                if !only_a.is_empty() || !only_b.is_empty() {
                    return Err(self.row_err(arena, ra, rb, &only_b, &only_a,
                        variant, constraint));
                }
                self.close_tail(arena, ta, variant, constraint)?;
            }
            (None, Some(tb)) => {
                if !only_a.is_empty() || !only_b.is_empty() {
                    return Err(self.row_err(arena, ra, rb, &only_b, &only_a,
                        variant, constraint));
                }
                self.close_tail(arena, tb, variant, constraint)?;
            }
            (None, None) => {
                if !only_a.is_empty() || !only_b.is_empty() {
                    return Err(self.row_err(arena, ra, rb, &only_b, &only_a,
                        variant, constraint));
                }
            }
        }
        Ok(())
    }

    fn row_of_names(&self, arena: &TypeArena, all: &[(RcStr, TypeId)],
            pick: &[String], tail: Option<u32>, variant: bool,
            constraint: u32) -> TypeId {
        let fields: Vec<(RcStr, TypeId)> = all.iter()
            .filter(|(n, _)| pick.contains(&n.to_string()))
            .cloned().collect();
        let names: Vec<String> = fields.iter().map(|(n, _)| n.to_string()).collect();
        let row = Row { fields, tail,
            origin: Origin::Solved { step: self.steps.borrow().len() as u32 },
            span: None };
        let node = arena.alloc(
            if variant { TyKind::Variant(row.clone()) } else { TyKind::Record(row) },
            Origin::Solved { step: self.steps.borrow().len() as u32 }, None);
        self.record_step(constraint, StepKind::RowMerge {
            tail: tail.unwrap_or(u32::MAX), fields: names,
        }, "merge open rows".into());
        node
    }

    fn row_err(&self, arena: &TypeArena, ra: &Row, rb: &Row,
            missing: &[String], extra: &[String], variant: bool,
            constraint: u32) -> UnifyError {
        let left = arena.alloc(
            if variant { TyKind::Variant(ra.clone()) } else { TyKind::Record(ra.clone()) },
            ra.origin.clone(), ra.span);
        let right = arena.alloc(
            if variant { TyKind::Variant(rb.clone()) } else { TyKind::Record(rb.clone()) },
            rb.origin.clone(), rb.span);
        UnifyError {
            kind: if variant {
                UnifyErrorKind::VariantMismatch {
                    missing: missing.to_vec(), extra: extra.to_vec() }
            } else {
                UnifyErrorKind::RowMismatch {
                    missing: missing.to_vec(), extra: extra.to_vec() }
            },
            left, right,
            left_span: ra.span, right_span: rb.span,
            step: self.steps.borrow().len() as u32,
            constraint,
        }
    }

    /// Flatten a row through already-bound tail variables.
    fn flatten_row(&self, arena: &TypeArena, row: &Row)
            -> (Vec<(RcStr, TypeId)>, Option<u32>) {
        let mut fields = row.fields.clone();
        let mut tail = row.tail;
        let mut seen: std::collections::BTreeSet<u32> = std::collections::BTreeSet::new();
        while let Some(t) = tail {
            let root = self.find(t);
            if !seen.insert(root) { tail = Some(root); break; }
            let bound = self.var_nodes.borrow()[root as usize];
            if bound.0 == u32::MAX { tail = Some(root); break; }
            let more = arena.view(bound, |n| match &n.kind {
                TyKind::Record(r) | TyKind::Variant(r) => Some(r.clone()),
                _ => None,
            });
            match more {
                Some(r) => {
                    fields.extend(r.fields);
                    tail = r.tail;
                }
                None => { tail = Some(root); break; }
            }
        }
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        fields.dedup_by(|a, b| a.0 == b.0);
        (fields, tail)
    }

    fn close_tail(&self, arena: &TypeArena, tail: u32, variant: bool,
            constraint: u32) -> Result<(), UnifyError> {
        let row = Row { fields: Vec::new(), tail: None,
            origin: Origin::Solved { step: self.steps.borrow().len() as u32 },
            span: None };
        let node = arena.alloc(
            if variant { TyKind::Variant(row) } else { TyKind::Record(row) },
            Origin::Solved { step: self.steps.borrow().len() as u32 }, None);
        self.record_step(constraint, StepKind::RowClose { tail },
            format!("close row ?{tail}"));
        self.bind(arena, tail, node, constraint)
    }
}

fn describe_kind(n: &crate::types::TyNode) -> String {
    match &n.kind {
        TyKind::Var(v) => format!("?{v}"),
        TyKind::Con(c) => c.to_string(),
        TyKind::Arr(_, _) => "function".into(),
        TyKind::Record(_) => "{record}".into(),
        TyKind::Variant(_) => "[|variant|]".into(),
    }
}
