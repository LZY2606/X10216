use serde::Serialize;
use std::collections::BTreeMap;
use crate::lang::NodeId;

pub type CId = u32;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub struct TyId(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub struct RowId(pub u32);

#[derive(Clone, Debug)]
pub enum TyNode {
    Pending { level: u32 },
    Con { name: String, args: Vec<TyId>, from: Option<CId> },
    Arrow { from: Option<CId>, dom: TyId, cod: TyId },
    Record { from: Option<CId>, row: RowId },
    Variant { from: Option<CId>, row: RowId },
    Redirect(TyId),
}

#[derive(Clone, Debug)]
pub struct FieldEntry {
    pub ty: TyId,
    pub from: Option<CId>,
}

#[derive(Clone, Debug)]
pub enum RowTail {
    Open(RowId),
    Closed,
}

#[derive(Clone, Debug)]
pub enum RowNode {
    Pending { level: u32 },
    Fields { fields: BTreeMap<String, FieldEntry>, tail: RowTail, row_from: Option<CId> },
    Redirect(RowId),
}

#[derive(Default, Debug)]
pub struct Arena {
    pub tys: Vec<TyNode>,
    pub rows: Vec<RowNode>,
    pub ty_origins: Vec<Vec<NodeId>>,
    pub row_origins: Vec<Vec<NodeId>>,
    pub ty_names: Vec<Option<String>>,
}

impl Arena {
    pub fn new() -> Self {
        Arena::default()
    }

    pub fn fresh_ty(&mut self, level: u32, origins: Vec<NodeId>, name: Option<String>) -> TyId {
        let id = TyId(self.tys.len() as u32);
        self.tys.push(TyNode::Pending { level });
        self.ty_origins.push(origins);
        self.ty_names.push(name);
        id
    }

    pub fn fresh_row(&mut self, level: u32, origins: Vec<NodeId>) -> RowId {
        let id = RowId(self.rows.len() as u32);
        self.rows.push(RowNode::Pending { level });
        self.row_origins.push(origins);
        id
    }

    /// An open empty row: {} | ρ
    pub fn open_row(&mut self, level: u32, origins: Vec<NodeId>, from: Option<CId>) -> RowId {
        let tail = self.fresh_row(level, origins.clone());
        let id = RowId(self.rows.len() as u32);
        self.rows.push(RowNode::Fields {
            fields: BTreeMap::new(),
            tail: RowTail::Open(tail),
            row_from: from,
        });
        self.row_origins.push(origins);
        id
    }

    pub fn closed_row(&mut self, fields: BTreeMap<String, FieldEntry>, origins: Vec<NodeId>, from: Option<CId>) -> RowId {
        let id = RowId(self.rows.len() as u32);
        self.rows.push(RowNode::Fields { fields, tail: RowTail::Closed, row_from: from });
        self.row_origins.push(origins);
        id
    }

    pub fn rep(&mut self, t: TyId) -> TyId {
        match &self.tys[t.0 as usize] {
            TyNode::Redirect(r) => {
                let r = *r;
                let root = self.rep(r);
                self.tys[t.0 as usize] = TyNode::Redirect(root);
                root
            }
            _ => t,
        }
    }

    pub fn rep_row(&mut self, r: RowId) -> RowId {
        match &self.rows[r.0 as usize] {
            RowNode::Redirect(q) => {
                let q = *q;
                let root = self.rep_row(q);
                self.rows[r.0 as usize] = RowNode::Redirect(root);
                root
            }
            _ => r,
        }
    }

    pub fn desc(&self, t: TyId) -> &TyNode {
        &self.tys[t.0 as usize]
    }
    pub fn row_desc(&self, r: RowId) -> &RowNode {
        &self.rows[r.0 as usize]
    }

    /// Bind pending variable `v` to descriptor `d` (already constructed), merging provenance.
    pub fn bind_ty(&mut self, v: TyId, d: TyNode, extra_origins: Vec<NodeId>) {
        for o in extra_origins {
            if !self.ty_origins[v.0 as usize].contains(&o) {
                self.ty_origins[v.0 as usize].push(o);
            }
        }
        self.tys[v.0 as usize] = d;
    }

    pub fn bind_row(&mut self, v: RowId, d: RowNode, extra_origins: Vec<NodeId>) {
        for o in extra_origins {
            if !self.row_origins[v.0 as usize].contains(&o) {
                self.row_origins[v.0 as usize].push(o);
            }
        }
        self.rows[v.0 as usize] = d;
    }

    pub fn redirect_ty(&mut self, v: TyId, root: TyId) {
        for o in std::mem::take(&mut self.ty_origins[v.0 as usize]) {
            if !self.ty_origins[root.0 as usize].contains(&o) {
                self.ty_origins[root.0 as usize].push(o);
            }
        }
        if self.ty_names[v.0 as usize].is_some() && self.ty_names[root.0 as usize].is_none() {
            self.ty_names[root.0 as usize] = self.ty_names[v.0 as usize].take();
        }
        self.tys[v.0 as usize] = TyNode::Redirect(root);
    }

    pub fn redirect_row(&mut self, v: RowId, root: RowId) {
        for o in std::mem::take(&mut self.row_origins[v.0 as usize]) {
            if !self.row_origins[root.0 as usize].contains(&o) {
                self.row_origins[root.0 as usize].push(o);
            }
        }
        self.rows[v.0 as usize] = RowNode::Redirect(root);
    }

    /// Occurs check for type variables inside a type descriptor rooted at `t`.
    pub fn occurs_ty(&self, v: TyId, t: TyId) -> bool {
        match &self.tys[t.0 as usize] {
            TyNode::Pending { .. } => t == v,
            TyNode::Redirect(r) => self.occurs_ty(v, *r),
            TyNode::Con { args, .. } => args.iter().any(|a| self.occurs_ty(v, *a)),
            TyNode::Arrow { dom, cod, .. } => self.occurs_ty(v, *dom) || self.occurs_ty(v, *cod),
            TyNode::Record { row, .. } | TyNode::Variant { row, .. } => self.occurs_row(v, *row),
        }
    }

    pub fn occurs_row(&self, v: TyId, r: RowId) -> bool {
        match &self.rows[r.0 as usize] {
            RowNode::Pending { .. } => false,
            RowNode::Redirect(q) => self.occurs_row(v, *q),
            RowNode::Fields { fields, tail, .. } => {
                fields.values().any(|f| self.occurs_ty(v, f.ty))
                    || match tail {
                        RowTail::Open(q) => self.occurs_row(v, *q),
                        RowTail::Closed => false,
                    }
            }
        }
    }

    /// CIds found on the structural path from `root` to variable `v` (for occurs-core seeds).
    pub fn occurs_path_cids(&self, v: TyId, root: TyId) -> Vec<CId> {
        let mut out = Vec::new();
        self.collect_occurs_path(v, root, &mut out);
        out
    }

    fn collect_occurs_path(&self, v: TyId, t: TyId, out: &mut Vec<CId>) -> bool {
        match &self.tys[t.0 as usize] {
            TyNode::Pending { .. } => return t == v,
            TyNode::Redirect(r) => return self.collect_occurs_path(v, *r, out),
            TyNode::Con { args, from, .. } => {
                if let Some(c) = from {
                    out.push(*c);
                }
                for a in args {
                    if self.collect_occurs_path(v, *a, out) {
                        return true;
                    }
                }
                false
            }
            TyNode::Arrow { from, dom, cod } => {
                if let Some(c) = from {
                    out.push(*c);
                }
                self.collect_occurs_path(v, *dom, out) || self.collect_occurs_path(v, *cod, out)
            }
            TyNode::Record { from, row } | TyNode::Variant { from, row } => {
                if let Some(c) = from {
                    out.push(*c);
                }
                self.collect_row_path(v, *row, out)
            }
        }
    }

    fn collect_row_path(&self, v: TyId, r: RowId, out: &mut Vec<CId>) -> bool {
        match &self.rows[r.0 as usize] {
            RowNode::Pending { .. } => false,
            RowNode::Redirect(q) => self.collect_row_path(v, *q, out),
            RowNode::Fields { fields, tail, .. } => {
                for f in fields.values() {
                    if self.collect_occurs_path(v, f.ty, out) {
                        if let Some(c) = f.from {
                            out.push(c);
                        }
                        return true;
                    }
                }
                match tail {
                    RowTail::Open(q) => self.collect_row_path(v, *q, out),
                    RowTail::Closed => false,
                }
            }
        }
    }

    pub fn ty_origins(&self, t: TyId) -> &[NodeId] {
        &self.ty_origins[t.0 as usize]
    }
    pub fn row_origins(&self, r: RowId) -> &[NodeId] {
        &self.row_origins[r.0 as usize]
    }
}
