use crate::syntax::Span;
use serde::Serialize;
use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;
use std::rc::Rc;

pub type Nid = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum D {
    Var,
    Con(Rc<str>),
    Arr(Nid, Nid),
    Row(RowExt, Option<Nid>),
    Rec(Nid),
    Variant(Nid),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize)]
pub enum RowKind { Record, Variant }

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RowExt {
    pub kind: RowKind,
    pub label: Rc<str>,
    pub value: Nid,
    pub tail: Nid,
}

#[derive(Clone, Debug)]
pub struct Node {
    pub d: D,
    pub rank: Cell<u32>,
    pub name: Rc<str>,
}

#[derive(Clone, Debug, Serialize)]
pub struct TypeJson {
    pub id: u32,
    pub name: String,
    pub node: String,
    #[serde(skip_serializing_if = "Option::is_none")] pub dom: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")] pub cod: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")] pub row: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")] pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")] pub value: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")] pub tail: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")] pub tail_var: Option<u32>,
}

pub struct TypeTable {
    pub nodes: Vec<Node>,
    up: Vec<Cell<u32>>,
    var_counter: u32,
    row_counter: u32,
}

impl TypeTable {
    pub fn new() -> Self { Self { nodes: Vec::new(), up: Vec::new(), var_counter: 0, row_counter: 0 } }

    pub fn make(&mut self, d: D, rank: u32) -> Nid {
        let id = self.nodes.len() as Nid;
        let name = match &d {
            D::Var => { let n = self.var_counter; self.var_counter += 1; Rc::from(format!("t{n}")) }
            D::Row(_, None) => { let n = self.row_counter; self.row_counter += 1; Rc::from(format!("ρ{n}")) }
            _ => Rc::from(""),
        };
        self.nodes.push(Node { d, rank: Cell::new(rank), name });
        self.up.push(Cell::new(id));
        id
    }
    pub fn var(&mut self, rank: u32) -> Nid { self.make(D::Var, rank) }
    pub fn con(&mut self, name: &str) -> Nid { self.make(D::Con(Rc::from(name)), 0) }
    pub fn arr(&mut self, dom: Nid, cod: Nid) -> Nid { self.make(D::Arr(dom, cod), 0) }
    pub fn rec(&mut self, row: Nid) -> Nid { self.make(D::Rec(row), 0) }
    pub fn variant(&mut self, row: Nid) -> Nid { self.make(D::Variant(row), 0) }
    pub fn empty_row(&mut self, kind: RowKind) -> Nid { self.make(D::Row(RowExt { kind, label: Rc::from(""), value: 0, tail: 0 }, None), 0) }

    pub fn closed_row(&mut self, kind: RowKind, fields: BTreeMap<Rc<str>, Nid>) -> Nid {
        let mut tail = self.empty_row(kind);
        for (label, value) in fields.into_iter().rev() {
            let old = tail;
            tail = self.make(D::Row(RowExt { kind, label, value, tail: old }, None), 0);
        }
        tail
    }

    pub fn extend_row(&mut self, kind: RowKind, label: Rc<str>, value: Nid, tail: Nid) -> Nid {
        self.make(D::Row(RowExt { kind, label, value, tail }, None), 0)
    }

    pub fn open_row_var(&mut self, _kind: RowKind, rank: u32) -> Nid { self.make(D::Var, rank) }

    pub fn find(&self, mut x: Nid) -> Nid {
        let mut root = x;
        while self.up[root as usize].get() != root { root = self.up[root as usize].get(); }
        while self.up[x as usize].get() != x { let next = self.up[x as usize].get(); self.up[x as usize].set(root); x = next; }
        root
    }

    pub fn d(&self, x: Nid) -> &D { &self.nodes[self.find(x) as usize].d }
    pub fn rank(&self, x: Nid) -> u32 { self.nodes[self.find(x) as usize].rank.get() }
    pub fn set_rank(&self, x: Nid, rank: u32) { self.nodes[self.find(x) as usize].rank.set(rank); }
    pub fn name(&self, x: Nid) -> Rc<str> {
        let r = self.find(x);
        if let D::Var = self.nodes[r as usize].d {
            let n = r;
            return Rc::from(format!("t{n}"));
        }
        self.nodes[r as usize].name.clone()
    }

    pub fn union(&self, a: Nid, b: Nid) {
        let a = self.find(a); let b = self.find(b);
        if a == b { return; }
        let (child, parent) = if self.rank_node(a) <= self.rank_node(b) { (a,b) } else { (b,a) };
        self.up[child as usize].set(parent);
        self.nodes[parent as usize].rank.set(self.rank(a).min(self.rank(b)));
    }
    fn rank_node(&self, x: Nid) -> u32 { self.nodes[x as usize].rank.get() }

    pub fn link(&self, child: Nid, parent: Nid) {
        let child = self.find(child);
        let parent = self.find(parent);
        if child != parent { self.up[child as usize].set(parent); }
    }


    pub fn row_view(&self, x: Nid) -> Result<RowView, RowError> {
        match self.d(x) {
            D::Var => Ok(RowView::Open(self.find(x))),
            D::Row(ext, tail_var) => Ok(RowView::Field(ext.clone(), *tail_var)),
            _ => Err(RowError::NotRow),
        }
    }

    pub fn normalize_row(&self, x: Nid) -> (Vec<(Rc<str>, Nid)>, Option<Nid>) {
        let mut fields = Vec::new();
        let mut cur = self.find(x);
        let mut seen = HashSet::new();
        loop {
            match &self.nodes[cur as usize].d {
                D::Row(ext, tv) if !ext.label.is_empty() => {
                    if !seen.insert(cur) { return (fields, None); }
                    fields.push((ext.label.clone(), self.find(ext.value)));
                    cur = self.find(ext.tail);
                    if let Some(tv) = tv { return (fields, Some(self.find(*tv))); }
                }
                D::Row(_, Some(tv)) => return (fields, Some(self.find(*tv))),
                D::Row(_, None) => return (fields, None),
                D::Var => return (fields, Some(cur)),
                _ => return (fields, None),
            }
        }
    }

    pub fn free_vars(&self, roots: &[Nid]) -> HashSet<Nid> {
        let mut out = HashSet::new();
        let mut stack = roots.to_vec();
        while let Some(x) = stack.pop() {
            let x = self.find(x);
            if !out.insert(x) { continue; }
            match &self.nodes[x as usize].d {
                D::Var | D::Con(_) => {}
                D::Arr(a,b) => { stack.push(*a); stack.push(*b); }
                D::Rec(r) | D::Variant(r) => stack.push(*r),
                D::Row(ext, tv) => { stack.push(ext.value); stack.push(ext.tail); if let Some(tv)=*tv {stack.push(tv);} }
            }
        }
        out
    }

    pub fn generalize(&self, root: Nid, ambient: u32) -> Vec<Nid> {
        let mut quantified = Vec::new();
        let mut seen = HashSet::new();
        let mut stack = vec![self.find(root)];
        while let Some(x) = stack.pop() {
            let x = self.find(x);
            if !seen.insert(x) { continue; }
            match &self.nodes[x as usize].d {
                D::Var if self.rank(x) > ambient => { quantified.push(x); }
                D::Var | D::Con(_) => {}
                D::Arr(a,b) => { stack.push(*a); stack.push(*b); }
                D::Rec(r) | D::Variant(r) => stack.push(*r),
                D::Row(ext, tv) => { stack.push(ext.value); stack.push(ext.tail); if let Some(tv)=tv { stack.push(*tv); } }
            }
        }
        quantified
    }

    pub fn snapshot(&self, roots: &[Nid]) -> (Vec<TypeJson>, HashMap<u32,u32>) {
        let mut remap: HashMap<Nid,u32> = HashMap::new();
        let mut raw: Vec<(Nid, TypeJson, Vec<Option<Nid>>)> = Vec::new();
        let mut stack: Vec<Nid> = roots.iter().map(|x| self.find(*x)).collect();
        while let Some(x) = stack.pop() {
            let root = self.find(x);
            if remap.contains_key(&root) { continue; }
            let nid = remap.len() as u32;
            remap.insert(root, nid);
            let d = &self.nodes[root as usize].d;
            let mut j = TypeJson { id: nid, name: self.public_name(root), node: "var".to_string(),
                dom: None, cod: None, row: None, label: None, value: None, tail: None, tail_var: None };
            let mut links = vec![None; 6];
            match d {
                D::Var => {}
                D::Con(n) => { j.node = "con".into(); j.label = Some(n.to_string()); }
                D::Arr(a,b) => { j.node = "arr".into(); links[0]=Some(*a); links[1]=Some(*b); stack.push(*a); stack.push(*b); }
                D::Rec(r) => { j.node = "record".into(); links[2]=Some(*r); stack.push(*r); }
                D::Variant(r) => { j.node = "variant".into(); links[2]=Some(*r); stack.push(*r); }
                D::Row(ext, tv) => {
                    if ext.label.is_empty() { j.node = "empty_row".into(); }
                    else {
                        j.node = "row_field".into(); j.label = Some(ext.label.to_string());
                        links[3]=Some(ext.value); links[4]=Some(ext.tail); stack.push(ext.value); stack.push(ext.tail);
                    }
                    if let Some(tv)=tv { links[5]=Some(*tv); stack.push(*tv); }
                }
            }
            raw.push((root,j,links));
        }
        raw.sort_by_key(|(_,j,_)| j.id);
        let nodes = raw.into_iter().map(|(root, mut j, links)| {
            let map = |x: Option<Nid>| x.map(|x| remap[&self.find(x)]);
            j.dom=map(links[0]); j.cod=map(links[1]); j.row=map(links[2]); j.value=map(links[3]); j.tail=map(links[4]); j.tail_var=map(links[5]);
            j.name=self.public_name(self.find(root)); j
        }).collect();
        (nodes, remap)
    }

    fn public_name(&self, root: Nid) -> String {
        match &self.nodes[root as usize].d {
            D::Var => format!("t{root}"),
            D::Row(_, Some(tv)) => format!("ρ{root}~t{}", self.find(*tv)),
            D::Row(_, None) if self.is_empty_row(root) => String::new(),
            _ => String::new(),
        }
    }

    fn is_empty_row(&self, root: Nid) -> bool {
        matches!(&self.nodes[root as usize].d, D::Row(ext, None) if ext.label.is_empty())
    }

    pub fn render(&self, x: Nid) -> String {
        let mut r = Renderer { table: self, seen: HashSet::new(), var_names: HashMap::new(), next: 0 };
        r.typ(x, true)
    }
}

#[derive(Clone, Debug)]
pub enum RowView { Open(Nid), Field(RowExt, Option<Nid>) }

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RowError { NotRow, Occurs, Mismatch(RowMismatch) }

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct RowMismatch {
    pub kind: RowKind,
    pub missing: Vec<(Rc<str>, Nid)>,
    pub extra: Vec<(Rc<str>, Nid)>,
    pub common_conflicts: Vec<(Rc<str>, Nid, Nid)>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Scheme {
    pub quantified: Vec<u32>,
    pub root: u32,
}

struct Renderer<'a> { table: &'a TypeTable, seen: HashSet<Nid>, var_names: HashMap<Nid, char>, next: u8 }
impl<'a> Renderer<'a> {
    fn name_var(&mut self, x: Nid) -> char {
        if let Some(c) = self.var_names.get(&x) { return *c; }
        let c = (b'a' + self.next.min(25)) as char; self.next += 1; self.var_names.insert(x,c); c
    }
    fn typ(&mut self, x: Nid, top: bool) -> String {
        let x = self.table.find(x);
        match &self.table.nodes[x as usize].d {
            D::Var => self.name_var(x).to_string(),
            D::Con(s) => match s.as_ref() { "Int" => "Int".into(), "Bool" => "Bool".into(), "String" => "String".into(), other => other.to_string() },
            D::Arr(a,b) => { let s = format!("{} -> {}", self.typ(*a,false), self.typ(*b,true)); if top {s} else {format!("({s})")} }
            D::Rec(r) => format!("{{ {} }}", self.row(*r)),
            D::Variant(r) => format!("[ {} ]", self.row(*r)),
            D::Row(_,_) => self.row(x),
        }
    }
    fn row(&mut self, x: Nid) -> String {
        let (fields, tail) = self.table.normalize_row(x);
        let mut parts: Vec<String> = fields.iter().map(|(l,v)| format!("{l}: {}", self.typ(*v,true))).collect();
        if let Some(t) = tail { parts.push(format!("...{}", self.name_var(t))); }
        parts.join(", ")
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct ExprType { pub anchor: String, pub r#type: String, pub node: u32 }

#[allow(dead_code)]
pub fn cmp_span(_a: Span, _b: Span) -> Ordering { Ordering::Equal }
impl fmt::Display for RowKind { fn fmt(&self,f:&mut fmt::Formatter<'_>)->fmt::Result { write!(f, "{}", if matches!(self,RowKind::Record) {"record"} else {"variant"}) } }
