//! Type representation: union-find variables, levels for rank-based
//! generalization, and row types (records/variants) with tails.

use std::cell::Cell;

use crate::syntax::{NId, Span};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TId(pub usize);

#[derive(Clone, Debug)]
pub struct Provenance {
    /// Expression node that produced this type part.
    pub origin_node: Option<NId>,
    /// Constraint that last contributed structure here.
    pub via_constraint: Option<usize>,
    pub span: Option<Span>,
    /// Human-readable label of the provenance role.
    pub note: String,
}

impl Provenance {
    pub fn none() -> Self {
        Provenance {
            origin_node: None,
            via_constraint: None,
            span: None,
            note: String::new(),
        }
    }
    pub fn node(n: NId, note: &str) -> Self {
        Provenance {
            origin_node: Some(n),
            via_constraint: None,
            span: None,
            note: note.to_string(),
        }
    }
    pub fn merge(&mut self, other: &Provenance) {
        if self.origin_node.is_none() {
            *self = other.clone();
        }
    }
}

#[derive(Clone, Debug)]
pub struct TyNode {
    pub kind: TyKind,
    pub prov: Provenance,
}

#[derive(Clone, Debug)]
pub enum TyKind {
    Var(VarData),
    Con {
        name: String,
        args: Vec<TId>,
    },
    Fun {
        from: TId,
        to: TId,
    },
    Row {
        is_record: bool,
        fields: Vec<RowField>,
        tail: Option<TId>,
    },
}

#[derive(Clone, Debug)]
pub struct RowField {
    pub label: String,
    pub value: TId,
    pub span: Option<Span>,
}

#[derive(Clone, Debug)]
pub struct VarData {
    pub rank: u32,
    pub is_row: bool,
    pub link: Option<TId>,
}

pub struct TypeStore {
    pub nodes: Vec<TyNode>,
    pub(crate) syms: Vec<String>,
    fresh_counter: Cell<usize>,
}

impl Default for TypeStore {
    fn default() -> Self {
        TypeStore {
            nodes: Vec::new(),
            syms: Vec::new(),
            fresh_counter: Cell::new(0),
        }
    }
}

impl TypeStore {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn fresh(&mut self, rank: u32, is_row: bool, sym: impl Into<String>, prov: Provenance) -> TId {
        let id = self.nodes.len();
        self.nodes.push(TyNode {
            kind: TyKind::Var(VarData { rank, is_row, link: None }),
            prov,
        });
        self.syms.push(sym.into());
        let _ = self.fresh_counter.get();
        TId(id)
    }

    pub fn mk(&mut self, kind: TyKind, prov: Provenance) -> TId {
        let id = self.nodes.len();
        self.nodes.push(TyNode { kind, prov });
        self.syms.push(String::new());
        TId(id)
    }

    pub fn sym(&self, t: TId) -> &str {
        &self.syms[t.0]
    }

    pub fn set_sym(&mut self, t: TId, s: impl Into<String>) {
        self.syms[t.0] = s.into();
    }

    pub fn root_of(&mut self, t: TId) -> TId {
        let mut cur = t;
        let mut path: Vec<TId> = Vec::new();
        loop {
            let link = match &self.nodes[cur.0].kind {
                TyKind::Var(v) => v.link,
                _ => {
                    for p in path {
                        if let TyKind::Var(v) = &mut self.nodes[p.0].kind {
                            v.link = Some(cur);
                        }
                    }
                    return cur;
                }
            };
            match link {
                Some(next) => {
                    path.push(cur);
                    cur = next;
                }
                None => {
                    for p in path {
                        if let TyKind::Var(v) = &mut self.nodes[p.0].kind {
                            v.link = Some(cur);
                        }
                    }
                    return cur;
                }
            }
        }
    }

    pub fn resolve(&mut self, t: TId) -> TId {
        self.root_of(t)
    }

    pub fn get(&self, t: TId) -> &TyNode {
        &self.nodes[t.0]
    }

    pub fn rank_of(&mut self, t: TId) -> u32 {
        let r = self.resolve(t);
        match &self.nodes[r.0].kind {
            TyKind::Var(v) => v.rank,
            _ => u32::MAX,
        }
    }
}
