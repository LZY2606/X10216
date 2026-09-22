use serde::Serialize;
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Var(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum Ty {
    Var(Var),
    Con(String, Vec<Ty>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Row {
    pub fields: BTreeMap<String, Ty>,
    pub ext: Option<Var>,
}

impl Row {
    pub fn open(fields: BTreeMap<String, Ty>, ext: Var) -> Row {
        Row { fields, ext: Some(ext) }
    }
    pub fn closed(fields: BTreeMap<String, Ty>) -> Row {
        Row { fields, ext: None }
    }
    pub fn empty_open(ext: Var) -> Row {
        Row { fields: BTreeMap::new(), ext: Some(ext) }
    }
}

pub fn t_int() -> Ty {
    Ty::Con("Int".into(), vec![])
}
pub fn t_bool() -> Ty {
    Ty::Con("Bool".into(), vec![])
}
pub fn t_str() -> Ty {
    Ty::Con("Str".into(), vec![])
}
pub fn t_fun(a: Ty, b: Ty) -> Ty {
    Ty::Con("->".into(), vec![a, b])
}

pub fn free_vars(ty: &Ty, acc: &mut BTreeSet<Var>) {
    match ty {
        Ty::Var(v) => {
            acc.insert(v.clone());
        }
        Ty::Con(_, args) => {
            for a in args {
                free_vars(a, acc);
            }
        }
        Ty::Record(r) | Ty::Variant(r) => {
            for f in r.fields.values() {
                free_vars(f, acc);
            }
            if let Some(v) = &r.ext {
                acc.insert(v.clone());
            }
        }
    }
}

/// Union-find node carrying a lazily assigned pretty name.
#[derive(Clone, Debug)]
pub enum Node {
    Link(Var),
    Root(RootData),
}

#[derive(Clone, Debug, Default)]
pub struct RootData {
    pub rank: u32,
    pub desc: Option<Ty>,
}

#[derive(Default)]
pub struct Uf {
    pub nodes: RefCell<Vec<Node>>,
}

impl Uf {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn fresh(&self) -> Var {
        let mut n = self.nodes.borrow_mut();
        let id = n.len() as u32;
        n.push(Node::Root(RootData::default()));
        Var(id)
    }
    pub fn find(&self, v: &Var) -> Var {
        {
            let n = self.nodes.borrow();
            if let Node::Link(p) = &n[v.0 as usize] {
                if p == v {
                    return *v;
                }
                let p = *p;
                drop(n);
                let r = self.find(&p);
                self.nodes.borrow_mut()[v.0 as usize] = Node::Link(r.clone());
                return r;
            }
        }
        v.clone()
    }
    pub fn set_desc(&self, v: &Var, d: Ty) {
        let r = self.find(v);
        let mut n = self.nodes.borrow_mut();
        if let Node::Root(rd) = &mut n[r.0 as usize] {
            rd.desc = Some(d);
        }
    }
    pub fn desc(&self, v: &Var) -> Option<Ty> {
        let r = self.find(v);
        let n = self.nodes.borrow();
        match &n[r.0 as usize] {
            Node::Root(rd) => rd.desc.clone(),
            _ => None,
        }
    }
    pub fn union(&self, a: &Var, b: &Var) -> Var {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return ra;
        }
        {
            let n = self.nodes.borrow();
            let rank_a = if let Node::Root(rd) = &n[ra.0 as usize] { rd.rank } else { 0 };
            let rank_b = if let Node::Root(rd) = &n[rb.0 as usize] { rd.rank } else { 0 };
            let (child, parent) = if rank_a > rank_b {
                (rb, ra)
            } else {
                (ra, rb)
            };
            drop(n);
            let mut n = self.nodes.borrow_mut();
            let child_desc = if let Node::Root(crd) = &n[child.0 as usize] {
                Some((crd.rank, crd.desc.clone()))
            } else {
                None
            };
            if let Some((crank, cdesc)) = child_desc {
                if let Node::Root(prd) = &mut n[parent.0 as usize] {
                    prd.rank = prd.rank.max(crank);
                    if prd.desc.is_none() {
                        prd.desc = cdesc;
                    }
                }
            }
            n[child.0 as usize] = Node::Link(parent);
            parent
        }
    }
}

/// Resolve a type to its representative form with all variables rooted.
pub fn resolve(uf: &Uf, ty: &Ty) -> Ty {
    match ty {
        Ty::Var(v) => match uf.desc(v) {
            Some(d) => resolve(uf, &d),
            None => Ty::Var(uf.find(v)),
        },
        Ty::Con(name, args) => Ty::Con(name.clone(), args.iter().map(|a| resolve(uf, a)).collect()),
        Ty::Record(r) => Ty::Record(resolve_row(uf, r)),
        Ty::Variant(r) => Ty::Variant(resolve_row(uf, r)),
    }
}

pub fn resolve_row(uf: &Uf, r: &Row) -> Row {
    fn walk(uf: &Uf, v: &Var, fields: &mut BTreeMap<String, Ty>, seen: &mut BTreeSet<Var>) -> Option<Var> {
        let root = uf.find(v);
        if !seen.insert(root) {
            return None;
        }
        match uf.desc(&root) {
            Some(Ty::Var(w)) => {
                let w = uf.find(&w);
                if seen.contains(&w) {
                    None
                } else {
                    walk(uf, &w, fields, seen)
                }
            }
            Some(Ty::Record(r2)) | Some(Ty::Variant(r2)) => {
                for (k, val) in &r2.fields {
                    fields.insert(k.clone(), resolve(uf, val));
                }
                match &r2.ext {
                    Some(e) => walk(uf, e, fields, seen),
                    None => None,
                }
            }
            Some(_) => Some(root),
            None => Some(root),
        }
    }
    let mut fields = BTreeMap::new();
    for (k, v) in &r.fields {
        fields.insert(k.clone(), resolve(uf, v));
    }
    let mut seen: BTreeSet<Var> = fields
        .iter()
        .flat_map(|(_, t)| {
            let mut s = BTreeSet::new();
            free_vars(t, &mut s);
            s
        })
        .collect();
    let ext = match &r.ext {
        Some(e) => walk(uf, e, &mut fields, &mut seen),
        None => None,
    };
    Row { fields, ext }
}

pub fn occurs(uf: &Uf, v: &Var, ty: &Ty) -> bool {
    let mut s = BTreeSet::new();
    free_vars(&resolve(uf, ty), &mut s);
    s.contains(&uf.find(v))
}

pub struct Renamer {
    names: BTreeMap<Var, String>,
    idx: u32,
}

impl Renamer {
    pub fn new() -> Self {
        Self { names: BTreeMap::new(), idx: 0 }
    }
    fn glyph(mut n: u32) -> String {
        let mut out = String::new();
        loop {
            out.push((b'a' + (n % 26) as u8) as char);
            n /= 26;
            if n == 0 {
                break;
            }
            n -= 1;
        }
        out.chars().rev().collect()
    }
    pub fn name(&mut self, v: &Var) -> String {
        if let Some(n) = self.names.get(v) {
            return n.clone();
        }
        let n = format!("'{}", Self::glyph(self.idx));
        self.idx += 1;
        self.names.insert(v.clone(), n.clone());
        n
    }
    pub fn show(&mut self, ty: &Ty) -> String {
        match ty {
            Ty::Var(v) => self.name(v),
            Ty::Con(n, args) if n == "->" => {
                format!("({} -> {})", self.show(&args[0]), self.show(&args[1]))
            }
            Ty::Con(n, args) if args.is_empty() => n.clone(),
            Ty::Con(n, args) => {
                format!("{}<{}>", n, args.iter().map(|a| self.show(a)).collect::<Vec<_>>().join(", "))
            }
            Ty::Record(r) => {
                let mut parts: Vec<String> =
                    r.fields.iter().map(|(k, v)| format!("{k}: {}", self.show(v))).collect();
                if let Some(e) = &r.ext {
                    parts.push(format!(".{}", self.name(e)));
                }
                format!("{{{}}}", parts.join(", "))
            }
            Ty::Variant(r) => {
                let mut parts: Vec<String> = r
                    .fields
                    .iter()
                    .map(|(k, v)| match v {
                        Ty::Con(u, args) if u == "()" && args.is_empty() => format!("#{k}"),
                        _ => format!("#{}({})", k, self.show(v)),
                    })
                    .collect();
                if let Some(e) = &r.ext {
                    parts.push(format!(".{}", self.name(e)));
                }
                format!("[{}]", parts.join(", "))
            }
        }
    }
}

pub fn show_ty(ty: &Ty) -> String {
    Renamer::new().show(ty)
}
