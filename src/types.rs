//! Types, ranks and union-find for Hindley-Milner inference with rows.

use std::collections::BTreeMap;

/// Origin traces every part of an inferred type back to AST nodes and/or the
/// constraint that synthesized that shape.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Origin {
    pub nodes: Vec<String>,
    pub constraint: Option<String>,
}

impl Origin {
    pub fn node(id: impl Into<String>) -> Self {
        Origin { nodes: vec![id.into()], constraint: None }
    }
    pub fn nodes(ns: &[String]) -> Self {
        Origin { nodes: ns.to_vec(), constraint: None }
    }
    pub fn constraint(id: impl Into<String>) -> Self {
        Origin { nodes: Vec::new(), constraint: Some(id.into()) }
    }
    pub fn merge(&mut self, other: &Origin) {
        for n in &other.nodes {
            if !self.nodes.contains(n) {
                self.nodes.push(n.clone());
            }
        }
        if self.constraint.is_none() {
            self.constraint = other.constraint.clone();
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TV(pub u32);
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RV(pub u32);

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Con(String, Origin),
    Arrow(TV, TV, Origin),
    Record(Row, Origin),
    Variant(Row, Origin),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Row {
    pub fields: BTreeMap<String, (TV, Origin)>,
    pub tail: RV,
    pub origin: Origin,
}

#[derive(Clone, Debug)]
pub enum TNode {
    Unbound { rank: u32 },
    Ref(TV),
    Term(Term),
}

#[derive(Clone, Debug)]
pub enum RNode {
    Unbound { rank: u32, fields: BTreeMap<String, (TV, Origin)>, closed: bool },
    Ref(RV),
}

#[derive(Clone, Debug)]
pub struct TypeStore {
    tnodes: Vec<TNode>,
    rnodes: Vec<RNode>,
    current_rank: u32,
}

impl Default for TypeStore {
    fn default() -> Self {
        TypeStore { tnodes: Vec::new(), rnodes: Vec::new(), current_rank: 0 }
    }
}

impl TypeStore {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn rank(&self) -> u32 {
        self.current_rank
    }
    pub fn enter(&mut self) -> u32 {
        self.current_rank += 1;
        self.current_rank
    }
    pub fn reset_rank(&mut self, r: u32) {
        self.current_rank = r;
    }

    pub fn fresh(&mut self) -> TV {
        let id = self.tnodes.len() as u32;
        self.tnodes.push(TNode::Unbound { rank: self.current_rank });
        TV(id)
    }
    pub fn fresh_row(&mut self, closed: bool) -> RV {
        let id = self.rnodes.len() as u32;
        self.rnodes.push(RNode::Unbound {
            rank: self.current_rank,
            fields: BTreeMap::new(),
            closed,
        });
        RV(id)
    }

    // constructors produce a TV holding a term directly
    pub fn con(&mut self, name: &str, o: Origin) -> TV {
        let id = self.tnodes.len() as u32;
        self.tnodes.push(TNode::Term(Term::Con(name.to_string(), o)));
        TV(id)
    }
    pub fn arrow(&mut self, a: TV, b: TV, o: Origin) -> TV {
        let id = self.tnodes.len() as u32;
        self.tnodes.push(TNode::Term(Term::Arrow(a, b, o)));
        TV(id)
    }
    pub fn record(&mut self, row: Row, o: Origin) -> TV {
        let id = self.tnodes.len() as u32;
        self.tnodes.push(TNode::Term(Term::Record(row, o)));
        TV(id)
    }
    pub fn variant(&mut self, row: Row, o: Origin) -> TV {
        let id = self.tnodes.len() as u32;
        self.tnodes.push(TNode::Term(Term::Variant(row, o)));
        TV(id)
    }
    pub fn row(&self, fields: BTreeMap<String, (TV, Origin)>, tail: RV, o: Origin) -> Row {
        Row { fields, tail, origin: o }
    }

    // ---------- resolution ----------

    pub fn resolve(&mut self, tv: TV) -> TV {
        match self.tnodes[tv.0 as usize].clone() {
            TNode::Ref(r) => {
                let root = self.resolve(r);
                self.tnodes[tv.0 as usize] = TNode::Ref(root);
                root
            }
            _ => tv,
        }
    }
    pub fn resolve_row(&mut self, rv: RV) -> RV {
        match self.rnodes[rv.0 as usize].clone() {
            RNode::Ref(r) => {
                let root = self.resolve_row(r);
                self.rnodes[rv.0 as usize] = RNode::Ref(root);
                root
            }
            _ => rv,
        }
    }

    pub fn term_of(&self, tv: TV) -> Option<Term> {
        match &self.tnodes[tv.0 as usize] {
            TNode::Term(t) => Some(t.clone()),
            _ => None,
        }
    }
    pub fn is_unbound(&self, tv: TV) -> bool {
        matches!(self.tnodes[tv.0 as usize], TNode::Unbound { .. })
    }
    pub fn t_rank(&self, tv: TV) -> u32 {
        match &self.tnodes[tv.0 as usize] {
            TNode::Unbound { rank } => *rank,
            TNode::Ref(_) => panic!("rank on unresolved"),
            TNode::Term(_) => u32::MAX,
        }
    }
    pub fn set_rank(&mut self, tv: TV, rank: u32) {
        if let TNode::Unbound { rank: r } = &mut self.tnodes[tv.0 as usize] {
            if rank < *r {
                *r = rank;
            }
        }
    }
}

// ---------- Errors produced during unification ----------

#[derive(Clone, Debug, PartialEq)]
pub enum UniError {
    Clash {
        expected: String,
        got: String,
        origin: Origin,
    },
    Infinite {
        var: String,
        structure: String,
        origin: Origin,
    },
    RowMismatch {
        kind: RowKind,
        /// Fields present on the *right* but missing from the *left* (required side).
        missing: Vec<(String, Origin)>,
        /// Fields present on the *left* that the right side does not declare.
        extra: Vec<(String, Origin)>,
        origin: Origin,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowKind {
    Record,
    Variant,
}

impl UniError {
    pub fn origin(&self) -> &Origin {
        match self {
            UniError::Clash { origin, .. }
            | UniError::Infinite { origin, .. }
            | UniError::RowMismatch { origin, .. } => origin,
        }
    }
    pub fn merge_origin(&mut self, o: &Origin) {
        match self {
            UniError::Clash { origin, .. }
            | UniError::Infinite { origin, .. }
            | UniError::RowMismatch { origin, .. } => origin.merge(o),
        }
    }
}

#[derive(Clone, Debug)]
pub struct UniEvent {
    pub left: String,
    pub right: String,
    pub result: String,
}

impl TypeStore {
    /// Occurs check: does variable `v` occur inside term `t` (through row tails)?
    fn occurs(&self, v: TV, t: &Term) -> bool {
        match t {
            Term::Con(..) => false,
            Term::Arrow(a, b, _) => *a == v || *b == v || self.occurs_in(v, *a) || self.occurs_in(v, *b),
            Term::Record(r, _) | Term::Variant(r, _) => {
                r.fields.values().any(|(fv, _)| *fv == v || self.occurs_in(v, *fv))
                    || self.row_occurs(v, r.tail)
            }
        }
    }
    fn occurs_in(&self, v: TV, x: TV) -> bool {
        if x == v {
            return true;
        }
        match &self.tnodes[x.0 as usize] {
            TNode::Term(t) => self.occurs(v, t),
            _ => false,
        }
    }
    fn row_occurs(&self, v: TV, rv: RV) -> bool {
        match &self.rnodes[rv.0 as usize] {
            RNode::Unbound { fields, .. } => {
                fields.values().any(|(fv, _)| *fv == v || self.occurs_in(v, *fv))
            }
            RNode::Ref(_) => false, // resolved before call
        }
    }
}

impl TypeStore {
    pub fn unify(&mut self, a: TV, b: TV) -> Result<(), UniError> {
        let a = self.resolve(a);
        let b = self.resolve(b);
        if a == b {
            return Ok(());
        }
        match (self.tnodes[a.0 as usize].clone(), self.tnodes[b.0 as usize].clone()) {
            (TNode::Unbound { rank: ra }, TNode::Unbound { rank: rb }) => {
                // union by id, rank becomes min (more general environment wins)
                let (from, to) = if ra >= rb { (a, b) } else { (b, a) };
                let nr = ra.min(rb);
                self.tnodes[from.0 as usize] = TNode::Ref(to);
                self.set_rank(to, nr);
                Ok(())
            }
            (TNode::Unbound { .. }, _) => self.bind_to_term(a, b),
            (_, TNode::Unbound { .. }) => self.bind_to_term(b, a),
            (TNode::Term(ta), TNode::Term(tb)) => self.unify_terms(ta, tb),
            (TNode::Ref(_), _) | (_, TNode::Ref(_)) => unreachable!("unresolved after resolve"),
        }
    }

    fn bind_to_term(&mut self, var: TV, term_root: TV) -> Result<(), UniError> {
        let term = self.term_of(term_root).expect("term root holds a term");
        if self.occurs(var, &term) {
            let mut o = self.term_origin(&term);
            return Err(UniError::Infinite {
                var: format!("?t{}", var.0),
                structure: self.render_term(&term),
                origin: std::mem::take(&mut o),
            });
        }
        // downward rank propagation: variables used inside a binding at an outer
        // rank must not be generalized at the inner let.
        let r = self.t_rank(var);
        self.adjust_term_rank(&term, r);
        self.tnodes[var.0 as usize] = TNode::Ref(term_root);
        Ok(())
    }

    fn adjust_term_rank(&mut self, term: &Term, rank: u32) {
        match term {
            Term::Con(..) => {}
            Term::Arrow(a, b, _) => {
                self.adjust_tv(*a, rank);
                self.adjust_tv(*b, rank);
            }
            Term::Record(row, _) | Term::Variant(row, _) => {
                for (fv, _) in row.fields.values() {
                    self.adjust_tv(*fv, rank);
                }
                self.adjust_row(row.tail, rank);
            }
        }
    }
    fn adjust_tv(&mut self, tv: TV, rank: u32) {
        let tv = self.resolve(tv);
        if let TNode::Unbound { .. } = self.tnodes[tv.0 as usize] {
            self.set_rank(tv, rank);
        } else if let Some(t) = self.term_of(tv) {
            self.adjust_term_rank(&t, rank);
        }
    }
    fn adjust_row(&mut self, rv: RV, rank: u32) {
        let rv = self.resolve_row(rv);
        if let RNode::Unbound { fields, .. } = self.rnodes[rv.0 as usize].clone() {
            for (fv, _) in fields.values() {
                self.adjust_tv(*fv, rank);
            }
        }
    }

    fn unify_terms(&mut self, a: Term, b: Term) -> Result<(), UniError> {
        match (a, b) {
            (Term::Con(x, ox), Term::Con(y, oy)) => {
                if x == y {
                    Ok(())
                } else {
                    let mut o = ox;
                    o.merge(&oy);
                    Err(UniError::Clash { expected: x, got: y, origin: o })
                }
            }
            (Term::Arrow(a1, a2, oa), Term::Arrow(b1, b2, ob)) => {
                self.unify(a1, b1).map_err(|e| self.attach(e, &oa, &ob))?;
                self.unify(a2, b2).map_err(|e| self.attach(e, &oa, &ob))
            }
            (Term::Record(r1, o1), Term::Record(r2, o2)) => {
                self.unify_rows(r1, r2, RowKind::Record, &o1, &o2)
            }
            (Term::Variant(r1, o1), Term::Variant(r2, o2)) => {
                self.unify_rows(r1, r2, RowKind::Variant, &o1, &o2)
            }
            (x, y) => {
                let mut o = self.term_origin(&x);
                o.merge(&self.term_origin(&y));
                Err(UniError::Clash {
                    expected: self.head_name(&x),
                    got: self.head_name(&y),
                    origin: o,
                })
            }
        }
    }

    fn attach(&self, mut e: UniError, o1: &Origin, o2: &Origin) -> UniError {
        e.merge_origin(o1);
        e.merge_origin(o2);
        e
    }
}

impl TypeStore {
    fn unify_rows(
        &mut self,
        left: Row,
        right: Row,
        kind: RowKind,
        o1: &Origin,
        o2: &Origin,
    ) -> Result<(), UniError> {
        let ltail = self.resolve_row(left.tail);
        let rtail = self.resolve_row(right.tail);

        let mut lfields = left.fields.clone();
        let mut rfields = right.fields.clone();
        if let RNode::Unbound { fields, .. } = &self.rnodes[ltail.0 as usize] {
            for (k, v) in fields {
                lfields.entry(k).or_insert(v.clone());
            }
        }
        if let RNode::Unbound { fields, .. } = &self.rnodes[rtail.0 as usize] {
            for (k, v) in fields {
                rfields.entry(k).or_insert(v.clone());
            }
        }

        // Fields common to both sides unify directly.
        let mut common: BTreeMap<String, (TV, Origin)> = BTreeMap::new();
        let mut left_only: BTreeMap<String, (TV, Origin)> = BTreeMap::new();
        let mut right_only: BTreeMap<String, (TV, Origin)> = BTreeMap::new();
        for (k, (tv, mut o)) in lfields {
            match rfields.remove(&k) {
                Some((tv2, o2)) => {
                    self.unify(tv, tv2).map_err(|e| self.attach(e, o1, o2))?;
                    o.merge(&o2);
                    let root = self.resolve(tv);
                    common.insert(k, (root, o));
                }
                None => {
                    left_only.insert(k, (tv, o));
                }
            }
        }
        for (k, v) in rfields {
            right_only.insert(k, v);
        }

        let lclosed = self.row_closed(ltail);
        let rclosed = self.row_closed(rtail);

        // Missing fields: required by the left side but absent on the right.
        let mut missing = Vec::new();
        let mut extra = Vec::new();
        if rclosed {
            for (k, (_, o)) in &left_only {
                missing.push((k.clone(), o.clone()));
            }
        }
        if lclosed {
            for (k, (_, o)) in &right_only {
                extra.push((k.clone(), o.clone()));
            }
        }
        if !missing.is_empty() || !extra.is_empty() {
            missing.sort();
            extra.sort();
            let mut o = o1.clone();
            o.merge(o2);
            return Err(UniError::RowMismatch { kind, missing, extra, origin: o });
        }

        // Recompute fields after unification (resolved roots).
        let mut merged: BTreeMap<String, (TV, Origin)> = BTreeMap::new();
        for (k, (tv, o)) in common {
            merged.insert(k, (self.resolve(tv), o));
        }
        for (k, (tv, o)) in &left_only {
            merged.insert(k.clone(), (self.resolve(*tv), o.clone()));
        }
        for (k, (tv, o)) in &right_only {
            merged.insert(k.clone(), (self.resolve(*tv), o.clone()));
        }

        let merged_closed = lclosed && rclosed;
        self.merge_row_vars(ltail, rtail, merged, merged_closed);
        Ok(())
    }

    fn row_closed(&self, rv: RV) -> bool {
        match &self.rnodes[rv.0 as usize] {
            RNode::Unbound { closed, .. } => *closed,
            RNode::Ref(_) => false,
        }
    }

    /// Merge two row variables. A closed row variable is only compatible with
    /// another closed row variable with no residual fields. Otherwise both point
    /// to an open unbound variable carrying the residual fields.
    fn merge_row_vars(
        &mut self,
        l: RV,
        r: RV,
        residual: BTreeMap<String, (TV, Origin)>,
        closed: bool,
    ) {
        if l == r {
            if let RNode::Unbound { fields, .. } = &mut self.rnodes[l.0 as usize] {
                for (k, v) in residual {
                    fields.entry(k).or_insert(v);
                }
            }
            return;
        }
        let rank = match (&self.rnodes[l.0 as usize], &self.rnodes[r.0 as usize]) {
            (RNode::Unbound { rank: a, .. }, RNode::Unbound { rank: b, .. }) => (*a).min(*b),
            _ => 0,
        };
        if closed && residual.is_empty() {
            // Both rows are closed: point both at a fresh closed empty variable.
            let z = self.fresh_row(true);
            self.set_row_rank(z, rank);
            self.rnodes[l.0 as usize] = RNode::Ref(z);
            self.rnodes[r.0 as usize] = RNode::Ref(z);
        } else {
            let z = self.fresh_row(closed);
            self.set_row_rank(z, rank);
            if let RNode::Unbound { fields, .. } = &mut self.rnodes[z.0 as usize] {
                *fields = residual;
            }
            self.rnodes[l.0 as usize] = RNode::Ref(z);
            self.rnodes[r.0 as usize] = RNode::Ref(z);
        }
    }

    fn set_row_rank(&mut self, rv: RV, rank: u32) {
        if let RNode::Unbound { rank: r, .. } = &mut self.rnodes[rv.0 as usize] {
            if rank < *r {
                *r = rank;
            }
        }
    }
}
