use crate::span::Span;
use crate::types::{SField, SRow, SType};
use std::collections::{HashMap, HashSet};

pub type UfId = u32;

#[derive(Debug, Clone)]
pub enum Ty {
    Con(String),
    Fun(Box<Ty>, Box<Ty>),
    Record(HashMap<String, UfId>, UfId),
    Variant(HashMap<String, UfId>, UfId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RowFlavor { Record, Variant }

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub key: String,
    pub span: Span,
    pub row_flavor: Option<RowFlavor>,
}

#[derive(Debug, Clone)]
pub struct UnifyError {
    pub kind: ErrorKind,
    pub span: Span,
    pub left: SType,
    pub right: SType,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Mismatch { detail: String },
    Infinite { var_key: String, through: SType },
    RowShape {
        flavor: RowFlavor,
        /// fields the actual row lacks (expected side required them)
        missing: Vec<FieldType>,
        /// fields present on the actual row that a closed expected row forbids
        extra: Vec<FieldType>,
        /// common labels whose payload types disagree
        conflicts: Vec<(String, SType, SType)>,
        /// same label already present deeper in an open tail (extension clash)
        duplicate: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub struct FieldType { pub label: String, pub ty: SType }

pub struct Uf {
    parent: Vec<UfId>,
    rank: Vec<u32>,
    desc: Vec<Option<Ty>>,
    vars: Vec<VarInfo>,
}

#[derive(Clone, Copy)]
enum Chase { Open(UfId), Closed }

impl Uf {
    pub fn new() -> Self {
        Uf { parent: Vec::new(), rank: Vec::new(), desc: Vec::new(), vars: Vec::new() }
    }

    pub fn fresh(&mut self, key: String, span: Span, row_flavor: Option<RowFlavor>) -> UfId {
        let id = self.parent.len() as UfId;
        self.parent.push(id);
        self.rank.push(0);
        self.desc.push(None);
        self.vars.push(VarInfo { key, span, row_flavor });
        id
    }

    pub fn key_of(&self, id: UfId) -> String {
        self.vars[self.find(id) as usize].key.clone()
    }

    pub fn find(&self, mut x: UfId) -> UfId {
        while self.parent[x as usize] != x { x = self.parent[x as usize]; }
        x
    }
    pub fn find_mut(&mut self, mut x: UfId) -> UfId {
        let root = {
            let mut y = x;
            while self.parent[y as usize] != y { y = self.parent[y as usize]; }
            y
        };
        while self.parent[x as usize] != x {
            let nxt = self.parent[x as usize];
            self.parent[x as usize] = root;
            x = nxt;
        }
        root
    }

    // Bind an *unbound* variable directly onto a (possibly unbound) root.
    fn bind_var(&mut self, var: UfId, to: UfId) {
        let v = self.find_mut(var);
        let t = self.find_mut(to);
        debug_assert!(self.desc[v as usize].is_none());
        if v == t { return; }
        self.parent[v as usize] = t;
    }

    // Bind two unbound variables using ranks.
    fn bind_two_vars(&mut self, a: UfId, b: UfId) {
        let a = self.find_mut(a);
        let b = self.find_mut(b);
        if a == b { return; }
        if self.rank[a as usize] < self.rank[b as usize] {
            self.parent[a as usize] = b;
        } else if self.rank[a as usize] > self.rank[b as usize] {
            self.parent[b as usize] = a;
        } else {
            self.parent[b as usize] = a;
            self.rank[a as usize] += 1;
        }
    }

    pub fn inst_s(&mut self, st: &SType, memo: &mut HashMap<String, UfId>, span: Span) -> UfId {
        match st {
            SType::V(k) => *memo.entry(k.clone()).or_insert_with(|| self.fresh(k.clone(), span, None)),
            SType::Con(n) => {
                let id = self.fresh(format!("#con:{}", n), span, None);
                self.desc[id as usize] = Some(Ty::Con(n.clone()));
                id
            }
            SType::Fun(a, b) => {
                let x = self.inst_s(a, memo, span);
                let y = self.inst_s(b, memo, span);
                let id = self.fresh("#fun".into(), span, None);
                self.desc[id as usize] = Some(Ty::Fun(Box::new(x), Box::new(y)));
                id
            }
            SType::Record(fs, row) | SType::Variant(fs, row) => {
                let flavor = if matches!(st, SType::Record(..)) { RowFlavor::Record } else { RowFlavor::Variant };
                let tail = match row {
                    SRow::Closed => self.empty_row(flavor),
                    SRow::Open(k) => *memo.entry(k.clone()).or_insert_with(|| self.fresh(k.clone(), span, Some(flavor))),
                };
                let mut map = HashMap::new();
                for f in fs {
                    let v = self.inst_s(&f.ty, memo, span);
                    map.insert(f.label.clone(), v);
                }
                let id = self.fresh("#row".into(), span, None);
                self.desc[id as usize] = Some(match flavor {
                    RowFlavor::Record => Ty::Record(map, tail),
                    RowFlavor::Variant => Ty::Variant(map, tail),
                });
                id
            }
        }
    }

    pub fn empty_row(&mut self, flavor: RowFlavor) -> UfId {
        let t = self.fresh("#closed".into(), Span::DUMMY, Some(flavor));
        self.desc[t as usize] = Some(match flavor {
            RowFlavor::Record => Ty::Record(HashMap::new(), t),
            RowFlavor::Variant => Ty::Variant(HashMap::new(), t),
        });
        t
    }

    /// Follow tail links, accumulating labels; ends at an open variable or a
    /// closed empty row. Labels encountered along the way are returned.
    fn chase(&mut self, tail: UfId) -> (Chase, HashSet<String>) {
        let mut labels = HashSet::new();
        let mut cur = self.find_mut(tail);
        let mut guard = 0;
        loop {
            let d = self.desc[cur as usize].clone();
            match d {
                Some(Ty::Record(m, next)) | Some(Ty::Variant(m, next)) => {
                    labels.extend(m.keys().cloned());
                    let n = self.find_mut(next);
                    if n == cur { return (Chase::Closed, labels); }
                    cur = n;
                }
                None => return (Chase::Open(cur), labels),
            }
            guard += 1;
            if guard > 1024 { return (Chase::Open(cur), labels); }
        }
    }
}

impl Uf {
    pub fn resolve(&mut self, id: UfId) -> SType {
        let root = self.find_mut(id);
        match self.desc[root as usize].clone() {
            None => SType::V(self.vars[root as usize].key.clone()),
            Some(Ty::Con(n)) => SType::Con(n),
            Some(Ty::Fun(a, b)) => SType::fun(self.resolve(a), self.resolve(b)),
            Some(Ty::Record(m, tail)) => SType::Record(self.resolve_fields(&m), self.resolve_row(tail)),
            Some(Ty::Variant(m, tail)) => SType::Variant(self.resolve_fields(&m), self.resolve_row(tail)),
        }
    }
    fn resolve_fields(&mut self, m: &HashMap<String, UfId>) -> Vec<SField> {
        let mut fs: Vec<SField> = m.iter().map(|(label, id)| SField {
            label: label.clone(), ty: self.resolve(*id),
        }).collect();
        fs.sort_by(|a, b| a.label.cmp(&b.label));
        fs
    }
    fn resolve_row(&mut self, tail: UfId) -> SRow {
        match self.chase(tail).0 {
            Chase::Closed => SRow::Closed,
            Chase::Open(v) => SRow::Open(self.vars[v as usize].key.clone()),
        }
    }

    fn occurs(&self, v: UfId, t: UfId, seen: &mut HashSet<UfId>) -> bool {
        let v = self.find(v);
        let t = self.find(t);
        if t == v { return true; }
        if !seen.insert(t) { return false; }
        match &self.desc[t as usize] {
            None => false,
            Some(Ty::Con(_)) => false,
            Some(Ty::Fun(a, b)) => self.occurs(v, *a, seen) || self.occurs(v, *b, seen),
            Some(Ty::Record(m, tail)) | Some(Ty::Variant(m, tail)) => {
                m.values().any(|x| self.occurs(v, *x, seen)) || self.occurs(v, *tail, seen)
            }
        }
    }

    pub fn unify(&mut self, expected: UfId, actual: UfId, span: Span) -> Result<(), UnifyError> {
        let a = self.find_mut(expected);
        let b = self.find_mut(actual);
        if a == b { return Ok(()); }
        let da = self.desc[a as usize].clone();
        let db = self.desc[b as usize].clone();
        match (da, db) {
            (None, Some(_)) => {
                if self.occurs(a, b, &mut HashSet::new()) {
                    return Err(self.infinite(a, b, span));
                }
                self.bind_var(a, b);
                Ok(())
            }
            (Some(_), None) => {
                if self.occurs(b, a, &mut HashSet::new()) {
                    return Err(self.infinite(b, a, span));
                }
                self.bind_var(b, a);
                Ok(())
            }
            (None, None) => { self.bind_two_vars(a, b); Ok(()) }
            (Some(Ty::Con(x)), Some(Ty::Con(y))) => {
                if x == y { Ok(()) } else {
                    Err(self.mismatch(span, SType::Con(x), SType::Con(y),
                        format!("基础类型 {} 与 {} 不一致", x, y)))
                }
            }
            (Some(Ty::Fun(ax, ar)), Some(Ty::Fun(bx, br))) => {
                self.unify(ax, bx, span)?;
                self.unify(ar, br, span)
            }
            (Some(Ty::Record(fa, ta)), Some(Ty::Record(fb, tb))) => {
                self.unify_rows(RowFlavor::Record, fa, ta, fb, tb, span)
            }
            (Some(Ty::Variant(fa, ta)), Some(Ty::Variant(fb, tb))) => {
                self.unify_rows(RowFlavor::Variant, fa, ta, fb, tb, span)
            }
            (Some(x), Some(y)) => {
                let sx = self.static_snapshot(&x);
                let sy = self.static_snapshot(&y);
                Err(self.mismatch(span, sx, sy, "类型形状不同（函数 / 记录 / 变体 / 基础类型无法匹配）".into()))
            }
        }
    }

    fn static_snapshot(&mut self, t: &Ty) -> SType {
        match t {
            Ty::Con(n) => SType::Con(n.clone()),
            Ty::Fun(a, b) => SType::fun(self.resolve(*a), self.resolve(*b)),
            Ty::Record(m, tail) => SType::Record(self.resolve_fields(m), self.resolve_row(*tail)),
            Ty::Variant(m, tail) => SType::Variant(self.resolve_fields(m), self.resolve_row(*tail)),
        }
    }

    fn infinite(&self, v: UfId, t: UfId, span: Span) -> UnifyError {
        // resolve through a temporary copy-free path: v key + best-effort t
        let key = self.vars[self.find(v) as usize].key.clone();
        UnifyError {
            kind: ErrorKind::Infinite { var_key: key, through: SType::V("…".into()) },
            span,
            left: SType::V(self.vars[self.find(v) as usize].key.clone()),
            right: SType::V(self.vars[self.find(t) as usize].key.clone()),
        }
    }
    fn mismatch(&self, span: Span, l: SType, r: SType, detail: String) -> UnifyError {
        UnifyError { kind: ErrorKind::Mismatch { detail }, span, left: l, right: r }
    }
}

impl Uf {
    #[allow(clippy::too_many_arguments)]
    fn unify_rows(
        &mut self,
        flavor: RowFlavor,
        fa: HashMap<String, UfId>, ta: UfId,
        fb: HashMap<String, UfId>, tb: UfId,
        span: Span,
    ) -> Result<(), UnifyError> {
        // pre-unification snapshots for diagnostics
        let snap_a = self.row_snapshot(flavor, &fa, ta);
        let snap_b = self.row_snapshot(flavor, &fb, tb);

        let mut labels: Vec<String> = fa.keys().chain(fb.keys()).cloned().collect::<HashSet<_>>()
            .into_iter().collect();
        labels.sort();

        // common fields: unify payload types, remembering the first clash
        let mut conflicts = Vec::new();
        for l in &labels {
            if let (Some(x), Some(y)) = (fa.get(l), fb.get(l)) {
                let sx = self.resolve(*x);
                let sy = self.resolve(*y);
                if self.unify(*x, *y, span).is_err() {
                    conflicts.push((l.clone(), sx, sy));
                }
            }
        }

        let (ca, seen_a) = self.chase(ta);
        let (cb, seen_b) = self.chase(tb);

        let mut missing = Vec::new(); // required by expected, absent in actual
        let mut extra = Vec::new();   // present in actual, forbidden by closed expected
        let mut duplicate: Option<String> = None;

        let mut give_a: Vec<(String, UfId)> = Vec::new(); // actual-only -> expected open tail
        let mut give_b: Vec<(String, UfId)> = Vec::new(); // expected-only -> actual open tail

        for l in &labels {
            let in_a = fa.contains_key(l);
            let in_b = fb.contains_key(l);
            if in_a && !in_b {
                let ty = fa[l];
                match cb {
                    Chase::Closed => missing.push(FieldType { label: l.clone(), ty: self.resolve(ty) }),
                    Chase::Open(_) => {
                        if seen_b.contains(l) && duplicate.is_none() { duplicate = Some(l.clone()); }
                        give_b.push((l.clone(), ty));
                    }
                }
            } else if in_b && !in_a {
                let ty = fb[l];
                match ca {
                    Chase::Closed => extra.push(FieldType { label: l.clone(), ty: self.resolve(ty) }),
                    Chase::Open(_) => {
                        if seen_a.contains(l) && duplicate.is_none() { duplicate = Some(l.clone()); }
                        give_a.push((l.clone(), ty));
                    }
                }
            }
        }

        if !conflicts.is_empty() || !missing.is_empty() || !extra.is_empty() || duplicate.is_some() {
            return Err(UnifyError {
                kind: ErrorKind::RowShape { flavor, missing, extra, conflicts, duplicate },
                span, left: snap_a, right: snap_b,
            });
        }

        // commit absorptions: prepend new field layer onto each open tail
        if let Chase::Open(va) = ca {
            if !give_a.is_empty() {
                let m: HashMap<String, UfId> = give_a.into_iter().collect();
                let layer = match flavor {
                    RowFlavor::Record => { let id = self.fresh("#row".into(), Span::DUMMY, None);
                        self.desc[id as usize] = Some(Ty::Record(m, va)); id }
                    RowFlavor::Variant => { let id = self.fresh("#row".into(), Span::DUMMY, None);
                        self.desc[id as usize] = Some(Ty::Variant(m, va)); id }
                };
                // redirect the open variable (currently unbound) onto the new layer
                self.bind_var(va, layer);
            }
        }
        if let Chase::Open(vb) = cb {
            if !give_b.is_empty() {
                let m: HashMap<String, UfId> = give_b.into_iter().collect();
                let layer = match flavor {
                    RowFlavor::Record => { let id = self.fresh("#row".into(), Span::DUMMY, None);
                        self.desc[id as usize] = Some(Ty::Record(m, vb)); id }
                    RowFlavor::Variant => { let id = self.fresh("#row".into(), Span::DUMMY, None);
                        self.desc[id as usize] = Some(Ty::Variant(m, vb)); id }
                };
                self.bind_var(vb, layer);
            }
        }

        // join tails
        match (ca, cb) {
            (Chase::Closed, Chase::Closed) => Ok(()),
            (Chase::Closed, Chase::Open(vb)) => {
                let empty = self.empty_row(flavor);
                self.unify(vb, empty, span)
            }
            (Chase::Open(va), Chase::Closed) => {
                let empty = self.empty_row(flavor);
                self.unify(va, empty, span)
            }
            (Chase::Open(va), Chase::Open(vb)) => self.unify(va, vb, span),
        }
    }

    fn row_snapshot(&mut self, flavor: RowFlavor, head: &HashMap<String, UfId>, tail: UfId) -> SType {
        let mut all: HashMap<String, SType> = HashMap::new();
        for (l, id) in head { all.insert(l.clone(), self.resolve(*id)); }
        let (end, seen) = self.chase(tail);
        // chase labels need resolving too; walk again resolving descriptors
        let mut cur = self.find_mut(tail);
        let mut guard = 0;
        loop {
            let d = self.desc[cur as usize].clone();
            match d {
                Some(Ty::Record(m, next)) | Some(Ty::Variant(m, next)) => {
                    for (l, id) in m { all.entry(l).or_insert_with(|| self.resolve(id)); }
                    let n = self.find_mut(next);
                    if n == cur { break; }
                    cur = n;
                }
                None => break,
            }
            guard += 1;
            if guard > 1024 { break; }
        }
        let _ = seen;
        let mut fs: Vec<SField> = all.into_iter().map(|(label, ty)| SField { label, ty }).collect();
        fs.sort_by(|a, b| a.label.cmp(&b.label));
        let row = match end { Chase::Closed => SRow::Closed, Chase::Open(v) => SRow::Open(self.vars[v as usize].key.clone()) };
        match flavor {
            RowFlavor::Record => SType::Record(fs, row),
            RowFlavor::Variant => SType::Variant(fs, row),
        }
    }
}
