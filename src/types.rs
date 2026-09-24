//! Type representation, union-find, and unification.
//!
//! Unification understands structural record/variant *rows* with open tails,
//! performs an occurs check, and reports missing and extra row fields as
//! distinct error variants instead of a generic "cannot unify".

use serde::Serialize;
use std::collections::HashMap;

use crate::syntax::Span;

/// Stable identifier of a unification variable.
pub type Vid = String;

/// Where a piece of type structure came from.
#[derive(Debug, Clone, Serialize)]
pub struct PartOrigin {
    pub node_id: String,
    pub node_kind: String,
    pub span: Span,
    pub reason: String,
}

impl PartOrigin {
    pub fn new(node_id: impl Into<String>, node_kind: impl Into<String>, span: Span, reason: impl Into<String>) -> Self {
        PartOrigin {
            node_id: node_id.into(),
            node_kind: node_kind.into(),
            span,
            reason: reason.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Row {
    pub(crate) fields: Vec<(String, Vid)>,
    pub(crate) tail: Vid,
}

#[derive(Debug, Clone)]
pub(crate) enum Node {
    /// Unbound variable.
    Flex { rank: u32, origin: Option<PartOrigin> },
    Con(String, PartOrigin),
    Arrow(Vid, Vid, PartOrigin),
    Record(Row, PartOrigin),
    Variant(Row, PartOrigin),
    /// Closed row terminator.
    RowEmpty,
}

pub struct Unifier {
    pub(crate) parent: HashMap<Vid, Vid>,
    pub(crate) rank: HashMap<Vid, u32>,
    pub(crate) nodes: HashMap<Vid, Node>,
    auto: u64,
}

impl Default for Unifier {
    fn default() -> Self {
        Self::new()
    }
}

impl Unifier {
    pub fn new() -> Self {
        Unifier {
            parent: HashMap::new(),
            rank: HashMap::new(),
            nodes: HashMap::new(),
            auto: 0,
        }
    }

    pub fn fresh(&mut self, key: impl Into<String>, origin: Option<PartOrigin>) -> Vid {
        let v = key.into();
        if self.nodes.contains_key(&v) {
            // Reuse only happens deliberately across incremental runs; the
            // counter fallback keeps accidental collisions distinct.
            self.auto += 1;
            let v2 = format!("{v}#dup{}", self.auto);
            self.insert(v2.clone(), origin);
            return v2;
        }
        self.insert(v.clone(), origin);
        v
    }

    fn insert(&mut self, v: Vid, origin: Option<PartOrigin>) {
        self.parent.insert(v.clone(), v.clone());
        self.rank.insert(v.clone(), 0);
        self.nodes.insert(v, Node::Flex { rank: 0, origin });
    }

    pub fn find(&mut self, v: &Vid) -> Vid {
        let p = self.parent.get(v).cloned().unwrap_or_else(|| v.clone());
        if &p == v {
            p
        } else {
            let r = self.find(&p);
            self.parent.insert(v.clone(), r.clone());
            r
        }
    }

    fn union(&mut self, a: Vid, b: Vid) {
        // a and b are roots.
        let ra = *self.rank.get(&a).unwrap_or(&0);
        let rb = *self.rank.get(&b).unwrap_or(&0);
        if ra > rb {
            self.parent.insert(b, a);
        } else if ra < rb {
            self.parent.insert(a, b);
        } else {
            self.parent.insert(a, b.clone());
            self.rank.insert(b, rb + 1);
        }
    }

    pub fn put_con(&mut self, v: &Vid, name: &str, o: PartOrigin) {
        self.nodes.insert(v.clone(), Node::Con(name.to_string(), o));
    }
    pub fn put_arrow(&mut self, v: &Vid, d: Vid, c: Vid, o: PartOrigin) {
        self.nodes.insert(v.clone(), Node::Arrow(d, c, o));
    }
    pub fn put_record(&mut self, v: &Vid, fields: Vec<(String, Vid)>, tail: Vid, o: PartOrigin) {
        self.nodes.insert(v.clone(), Node::Record(Row { fields, tail }, o));
    }
    pub fn put_variant(&mut self, v: &Vid, fields: Vec<(String, Vid)>, tail: Vid, o: PartOrigin) {
        self.nodes.insert(v.clone(), Node::Variant(Row { fields, tail }, o));
    }
    pub fn put_row_empty(&mut self, v: &Vid) {
        self.nodes.insert(v.clone(), Node::RowEmpty);
    }

    pub fn set_origin(&mut self, v: &Vid, o: PartOrigin) {
        let r = self.find(v);
        match self.nodes.get_mut(&r) {
            Some(Node::Flex { origin, .. }) => *origin = Some(o),
            _ => {}
        }
    }

    /// All structural vids reachable from a descriptor (for occurs checking).
    fn children(&self, n: &Node) -> Vec<Vid> {
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
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub enum RowSide {
    Left,
    Right,
}

#[derive(Debug, Clone, Serialize)]
pub enum UErr {
    Occurs {
        var: String,
        in_type: Box<RTy>,
        origin: Option<PartOrigin>,
    },
    Mismatch {
        left: Box<RTy>,
        right: Box<RTy>,
        left_origin: Option<PartOrigin>,
        right_origin: Option<PartOrigin>,
    },
    MissingFields {
        /// Side whose row is missing the named fields.
        side: RowSide,
        fields: Vec<String>,
        row: Box<RTy>,
        expected: Box<RTy>,
        row_origin: Option<PartOrigin>,
        expected_origin: Option<PartOrigin>,
    },
}

impl UErr {
    pub fn origins(&self) -> Vec<PartOrigin> {
        let mut out = Vec::new();
        match self {
            UErr::Occurs { origin, .. } => {
                if let Some(o) = origin { out.push(o.clone()); }
            }
            UErr::Mismatch { left_origin, right_origin, .. } => {
                if let Some(o) = left_origin { out.push(o.clone()); }
                if let Some(o) = right_origin { out.push(o.clone()); }
            }
            UErr::MissingFields { row_origin, expected_origin, .. } => {
                if let Some(o) = row_origin { out.push(o.clone()); }
                if let Some(o) = expected_origin { out.push(o.clone()); }
            }
        }
        out
    }
}

// ---------------------------------------------------------------------------
// Resolved (read-only) types with provenance
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub enum RowEnd {
    Open(String),
    Closed,
}

#[derive(Debug, Clone, Serialize)]
pub struct RField {
    pub name: String,
    pub ty: RTy,
}

#[derive(Debug, Clone, Serialize)]
pub enum RTy {
    RVar(String, Option<PartOrigin>),
    Con(String, PartOrigin),
    Arrow(Box<RTy>, Box<RTy>, PartOrigin),
    Record(Vec<RField>, RowEnd, PartOrigin),
    Variant(Vec<RField>, RowEnd, PartOrigin),
}

impl RTy {
    pub fn short(&self) -> String {
        match self {
            RTy::RVar(n, _) => n.clone(),
            RTy::Con(n, _) => n.clone(),
            RTy::Arrow(a, b, _) => format!("({} -> {})", a.short(), b.short()),
            RTy::Record(fs, end, _) => {
                let mut s = String::from("{ ");
                for (i, f) in fs.iter().enumerate() {
                    if i > 0 { s.push_str(", "); }
                    s.push_str(&format!("{}: {}", f.name, f.ty.short()));
                }
                if let RowEnd::Open(v) = end {
                    if !fs.is_empty() { s.push_str(", "); }
                    s.push_str(&format!("..{v}"));
                }
                s.push_str(" }");
                s
            }
            RTy::Variant(fs, end, _) => {
                let mut s = String::from("[ ");
                for (i, f) in fs.iter().enumerate() {
                    if i > 0 { s.push_str(" | "); }
                    s.push_str(&format!(":{} {}", f.name, f.ty.short()));
                }
                if let RowEnd::Open(v) = end {
                    if !fs.is_empty() { s.push_str(" | "); }
                    s.push_str(&format!("..{v}"));
                }
                s.push_str(" ]");
                s
            }
        }
    }

    pub fn origin(&self) -> Option<&PartOrigin> {
        match self {
            RTy::RVar(_, o) => o.as_ref(),
            RTy::Con(_, o) | RTy::Arrow(_, _, o) | RTy::Record(_, _, o) | RTy::Variant(_, _, o) => Some(o),
        }
    }
}

pub struct Resolver<'a> {
    u: &'a mut Unifier,
    names: HashMap<Vid, String>,
    counter: usize,
    visiting: Vec<Vid>,
}

impl<'a> Resolver<'a> {
    pub fn new(u: &'a mut Unifier) -> Self {
        Resolver { u, names: HashMap::new(), counter: 0, visiting: vec![] }
    }

    fn var_name(&mut self, v: &Vid) -> String {
        if let Some(n) = self.names.get(v) {
            return n.clone();
        }
        // name free vars a, b, c ... then t0, t1
        const ABC: &[u8] = b"abcdefghijklmnopqrstuvwxyz";
        let n = if self.counter < ABC.len() {
            (ABC[self.counter] as char).to_string()
        } else {
            format!("t{}", self.counter - ABC.len())
        };
        self.counter += 1;
        self.names.insert(v.clone(), n.clone());
        n
    }

    pub fn resolve(&mut self, v: &Vid) -> RTy {
        let r = self.u.find(v);
        let node = self.u.nodes.get(&r).cloned();
        match node {
            None => RTy::RVar(self.var_name(&r), None),
            Some(Node::Flex { origin, .. }) => RTy::RVar(self.var_name(&r), origin),
            Some(Node::Con(n, o)) => RTy::Con(n, o),
            Some(Node::Arrow(d, c, o)) => {
                let d = self.resolve(&d);
                let c = self.resolve(&c);
                RTy::Arrow(Box::new(d), Box::new(c), o)
            }
            Some(Node::Record(row, o)) => {
                let (fs, end) = self.flatten(&row);
                RTy::Record(fs, end, o)
            }
            Some(Node::Variant(row, o)) => {
                let (fs, end) = self.flatten(&row);
                RTy::Variant(fs, end, o)
            }
            Some(Node::RowEmpty) => RTy::Record(vec![], RowEnd::Closed, PartOrigin::new("?", "row-empty", Span::new(0, 0), "closed row")),
        }
    }

    fn flatten(&mut self, row: &Row) -> (Vec<RField>, RowEnd) {
        let mut fields: Vec<RField> = Vec::new();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut cur = row.clone();
        loop {
            for (name, fv) in &cur.fields {
                if seen.insert(name.clone()) {
                    let ty = self.resolve(fv);
                    fields.push(RField { name: name.clone(), ty });
                }
            }
            let tail_root = self.u.find(&cur.tail);
            match self.u.nodes.get(&tail_root).cloned() {
                Some(Node::Record(r, _)) | Some(Node::Variant(r, _)) => {
                    cur = r;
                }
                Some(Node::RowEmpty) => return (fields, RowEnd::Closed),
                Some(Node::Flex { .. }) | None => {
                    return (fields, RowEnd::Open(self.var_name(&tail_root)));
                }
                _ => return (fields, RowEnd::Open(self.var_name(&tail_root))),
            }
        }
    }
}



// ---------------------------------------------------------------------------
// Unification
// ---------------------------------------------------------------------------

#[derive(Clone)]
struct RowView {
    fields: Vec<(String, Vid)>,
    tail: Vid,
    closed: bool,
}

impl Unifier {
    pub fn resolve_rty(&mut self, v: &Vid) -> RTy {
        let mut r = Resolver::new(self);
        r.resolve(v)
    }

    pub fn unify(&mut self, a0: &Vid, b0: &Vid) -> Result<(), UErr> {
        let a = self.find(a0);
        let b = self.find(b0);
        if a == b {
            return Ok(());
        }
        let na = self.nodes.get(&a).cloned();
        let nb = self.nodes.get(&b).cloned();
        match (na, nb) {
            (Some(Node::Flex { .. }), x) | (None, x) => self.bind_flex(&a, &b, x),
            (x, Some(Node::Flex { .. })) | (x, None) => self.bind_flex(&b, &a, x),
            (Some(Node::Con(c1, o1)), Some(Node::Con(c2, o2))) => {
                if c1 == c2 {
                    self.union(a, b);
                    Ok(())
                } else {
                    Err(self.mismatch_err(&a, &b, Some(o1), Some(o2)))
                }
            }
            (Some(Node::Arrow(d1, c1, o1)), Some(Node::Arrow(d2, c2, o2))) => {
                self.union(a.clone(), b.clone());
                if let Err(e) = self.unify(&d1, &d2) {
                    return Err(e);
                }
                self.unify(&c1, &c2).map_err(|e| {
                    let _ = (o1, o2);
                    e
                })
            }
            (Some(Node::Record(r1, o1)), Some(Node::Record(r2, o2))) => {
                self.unify_rows(true, &a, r1, o1, &b, r2, o2)
            }
            (Some(Node::Variant(r1, o1)), Some(Node::Variant(r2, o2))) => {
                self.unify_rows(false, &a, r1, o1, &b, r2, o2)
            }
            (Some(na), Some(nb)) => {
                Err(self.mismatch_err(
                    &a,
                    &b,
                    descriptor_origin(&na),
                    descriptor_origin(&nb),
                ))
            }
        }
    }

    fn mismatch_err(
        &mut self,
        a: &Vid,
        b: &Vid,
        left_origin: Option<PartOrigin>,
        right_origin: Option<PartOrigin>,
    ) -> UErr {
        let left = self.resolve_rty(a);
        let right = self.resolve_rty(b);
        UErr::Mismatch {
            left: Box::new(left),
            right: Box::new(right),
            left_origin,
            right_origin,
        }
    }

    fn bind_flex(&mut self, flex: &Vid, other: &Vid, other_node: Option<Node>) -> Result<(), UErr> {
        match other_node {
            Some(Node::Flex { .. }) | None => {
                self.union(flex.clone(), other.clone());
                Ok(())
            }
            Some(desc) => {
                if self.occurs_in(flex, &desc) {
                    let in_ty = self.resolve_rty(other);
                    let origin = match self.nodes.get(flex).cloned() {
                        Some(Node::Flex { origin, .. }) => origin,
                        _ => None,
                    };
                    return Err(UErr::Occurs {
                        var: flex.clone(),
                        in_type: Box::new(in_ty),
                        origin,
                    });
                }
                self.union(flex.clone(), other.clone());
                Ok(())
            }
        }
    }

    fn occurs_in(&self, target: &Vid, node: &Node) -> bool {
        let mut stack = self.children(node);
        let mut seen = std::collections::HashSet::new();
        while let Some(v) = stack.pop() {
            let root = self.find_imm(&v);
            if &root == target {
                return true;
            }
            if !seen.insert(root.clone()) {
                continue;
            }
            if let Some(n) = self.nodes.get(&root) {
                for c in self.children(n) {
                    stack.push(c);
                }
            }
        }
        false
    }

    pub fn find_imm(&self, v: &Vid) -> Vid {
        let mut cur = v.clone();
        loop {
            match self.parent.get(&cur) {
                Some(p) if p != &cur => cur = p.clone(),
                _ => return cur,
            }
        }
    }

    fn view_row(&mut self, row: Row) -> RowView {
        let mut fields: Vec<(String, Vid)> = Vec::new();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut cur = row;
        loop {
            for (n, v) in cur.fields {
                if seen.insert(n.clone()) {
                    fields.push((n, v));
                }
            }
            let tr = self.find(&cur.tail);
            match self.nodes.get(&tr).cloned() {
                Some(Node::Record(r, _)) | Some(Node::Variant(r, _)) => cur = r,
                Some(Node::RowEmpty) => return RowView { fields, tail: tr, closed: true },
                _ => return RowView { fields, tail: tr, closed: false },
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn unify_rows(
        &mut self,
        is_record: bool,
        ra: &Vid,
        row_a0: Row,
        oa: PartOrigin,
        rb: &Vid,
        row_b0: Row,
        ob: PartOrigin,
    ) -> Result<(), UErr> {
        let va = self.view_row(row_a0);
        let vb = self.view_row(row_b0);

        let mut map_a: HashMap<String, Vid> = HashMap::new();
        for (n, v) in &va.fields {
            map_a.entry(n.clone()).or_insert_with(|| v.clone());
        }
        let mut map_b: HashMap<String, Vid> = HashMap::new();
        for (n, v) in &vb.fields {
            map_b.entry(n.clone()).or_insert_with(|| v.clone());
        }
        let mut all_names: Vec<String> = Vec::new();
        for (n, _) in &va.fields {
            if !all_names.contains(n) {
                all_names.push(n.clone());
            }
        }
        for (n, _) in &vb.fields {
            if !all_names.contains(n) {
                all_names.push(n.clone());
            }
        }

        for n in &all_names {
            if let (Some(x), Some(y)) = (map_a.get(n), map_b.get(n)) {
                self.unify(x, y)?;
            }
        }

        let only_a: Vec<String> = all_names.iter().filter(|n| !map_b.contains_key(*n)).cloned().collect();
        let only_b: Vec<String> = all_names.iter().filter(|n| !map_a.contains_key(*n)).cloned().collect();

        let mut merged_fields: Vec<(String, Vid)> = Vec::new();
        for n in &all_names {
            let vid = map_a.get(n).or_else(|| map_b.get(n)).cloned().unwrap();
            merged_fields.push((n.clone(), vid));
        }

        match (va.closed, vb.closed) {
            (true, true) => {
                if !only_a.is_empty() || !only_b.is_empty() {
                    let (side, fields, row_origin, expected_origin) = if !only_b.is_empty() {
                        (RowSide::Left, only_b, Some(oa), Some(ob))
                    } else {
                        (RowSide::Right, only_a, Some(ob), Some(oa))
                    };
                    return Err(self.row_field_err(ra, rb, side, fields, row_origin, expected_origin));
                }
                let tail = self.find(&va.tail);
                if is_record {
                    self.put_record(rb, merged_fields, tail, ob);
                } else {
                    self.put_variant(rb, merged_fields, tail, ob);
                }
                self.union(ra.clone(), rb.clone());
                Ok(())
            }
            (true, false) => {
                // A closed, B open: B-only fields are required by B, so the
                // closed A row is missing them. A-only fields are harmless
                // extras absorbed into B's open tail.
                if !only_b.is_empty() {
                    return Err(self.row_field_err(ra, rb, RowSide::Left, only_b, Some(oa), Some(ob)));
                }
                let tail = self.find(&vb.tail);
                if is_record {
                    self.put_record(ra, merged_fields, tail, oa);
                } else {
                    self.put_variant(ra, merged_fields, tail, oa);
                }
                self.union(rb.clone(), ra.clone());
                Ok(())
            }
            (false, true) => {
                if !only_a.is_empty() {
                    return Err(self.row_field_err(rb, ra, RowSide::Right, only_a, Some(ob), Some(oa)));
                }
                let tail = self.find(&va.tail);
                if is_record {
                    self.put_record(rb, merged_fields, tail, ob);
                } else {
                    self.put_variant(rb, merged_fields, tail, ob);
                }
                self.union(ra.clone(), rb.clone());
                Ok(())
            }
            (false, false) => {
                self.unify(&va.tail, &vb.tail)?;
                let tail = self.find(&va.tail);
                if is_record {
                    self.put_record(ra, merged_fields, tail, oa);
                } else {
                    self.put_variant(ra, merged_fields, tail, oa);
                }
                self.union(rb.clone(), ra.clone());
                Ok(())
            }
        }
    }

    fn row_field_err(
        &mut self,
        lacking: &Vid,
        other: &Vid,
        side: RowSide,
        fields: Vec<String>,
        row_origin: Option<PartOrigin>,
        expected_origin: Option<PartOrigin>,
    ) -> UErr {
        let row = self.resolve_rty(lacking);
        let expected = self.resolve_rty(other);
        UErr::MissingFields {
            side,
            fields,
            row: Box::new(row),
            expected: Box::new(expected),
            row_origin,
            expected_origin,
        }
    }
}

fn descriptor_origin(n: &Node) -> Option<PartOrigin> {
    match n {
        Node::Con(_, o) | Node::Arrow(_, _, o) | Node::Record(_, o) | Node::Variant(_, o) => {
            Some(o.clone())
        }
        _ => None,
    }
}
