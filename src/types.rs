//! Type representation, union-find, row unification, generalization/instantiation.

use crate::syntax::Span;
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Vid(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Pid(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Star,
    Row,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Prov {
    pub cids: Vec<u64>,
    pub span: Option<Span>,
}

impl Prov {
    pub fn cid(c: u64) -> Self {
        Prov { cids: vec![c], span: None }
    }
    pub fn span(s: Span) -> Self {
        Prov { cids: vec![], span: Some(s) }
    }
    pub fn merge(&mut self, other: &Prov) {
        for c in &other.cids {
            if !self.cids.contains(c) {
                self.cids.push(*c);
            }
        }
        if self.span.is_none() {
            self.span = other.span;
        }
        self.cids.sort_unstable();
    }
}

#[derive(Clone, Debug)]
pub enum Tail {
    Open(Vid),
    Closed,
}

#[derive(Clone, Debug)]
pub struct Rows {
    pub fields: BTreeMap<String, (Term, Prov)>,
    pub tail: Tail,
    pub prov: Prov,
}

#[derive(Clone, Debug)]
pub enum Term {
    TInt(Prov),
    TBool(Prov),
    TStr(Prov),
    TUnit(Prov),
    TFun(Box<(Term, Prov)>, Box<(Term, Prov)>, Prov),
    TRecord(Box<Rows>, Prov),
    TVariant(Box<Rows>, Prov),
}

#[derive(Clone, Debug)]
struct Node {
    parent: Option<Vid>,
    kind: Kind,
    level: u32,
    term: Option<Term>,
    prov: Prov,
}

#[derive(Default)]
pub struct UF {
    nodes: Vec<Node>,
    var_prov: Vec<Prov>,
    var_kind: Vec<Kind>,
}

impl UF {
    pub fn new() -> Self {
        UF::default()
    }

    pub fn fresh(&mut self, kind: Kind, prov: Prov) -> Vid {
        let v = Vid(self.nodes.len() as u32);
        self.nodes.push(Node { parent: None, kind, level: 0, term: None, prov: prov.clone() });
        self.var_prov.push(prov);
        self.var_kind.push(kind);
        v
    }

    pub fn fresh_at(&mut self, kind: Kind, level: u32, prov: Prov) -> Vid {
        let v = self.fresh(kind, prov);
        self.nodes[v.0 as usize].level = level;
        v
    }

    pub fn kind_of(&self, v: Vid) -> Kind {
        self.nodes[v.0 as usize].kind
    }

    pub fn level_of(&mut self, v: Vid) -> u32 {
        let r = self.find(v);
        self.nodes[r.0 as usize].level
    }

    pub fn set_level(&mut self, v: Vid, level: u32) {
        let r = self.find(v);
        self.nodes[r.0 as usize].level = level;
    }

    pub fn current_kind(&self, v: Vid) -> Kind {
        let n = &self.nodes[v.0 as usize];
        if let Some(p) = n.parent {
            self.current_kind(p)
        } else {
            n.kind
        }
    }

    fn find(&mut self, v: Vid) -> Vid {
        let p = match self.nodes[v.0 as usize].parent {
            Some(p) => p,
            None => return v,
        };
        let r = self.find(p);
        self.nodes[v.0 as usize].parent = Some(r);
        r
    }

    pub fn prov_of(&mut self, v: Vid) -> Prov {
        let r = self.find(v);
        let mut p = self.nodes[r.0 as usize].prov.clone();
        if let Some(t) = &self.nodes[r.0 as usize].term {
            p.merge(&term_prov(t));
        }
        p
    }

    pub fn set_var_prov(&mut self, v: Vid, prov: Prov) {
        let r = self.find(v);
        self.nodes[r.0 as usize].prov.merge(&prov);
    }

    pub fn term_of(&mut self, v: Vid) -> Option<Term> {
        let r = self.find(v);
        self.nodes[r.0 as usize].term.clone()
    }

    pub fn resolve(&mut self, v: Vid) -> Term {
        let r = self.find(v);
        match &self.nodes[r.0 as usize].term {
            Some(t) => t.clone(),
            None => match self.nodes[r.0 as usize].kind {
                Kind::Star => panic!("unresolved star variable {}", r.0),
                Kind::Row => empty_open_row(r, self.nodes[r.0 as usize].prov.clone()),
            },
        }
    }

    pub fn describe(&mut self, v: Vid) -> Term {
        let r = self.find(v);
        match &self.nodes[r.0 as usize].term {
            Some(t) => t.clone(),
            None => match self.nodes[r.0 as usize].kind {
                Kind::Star => free_var_term(r),
                Kind::Row => empty_open_row(r, self.nodes[r.0 as usize].prov.clone()),
            },
        }
    }

    /// Force v to point to the same root/term as r (used when instantiating a
    /// row tail variable inside a reconstructed term).
    pub fn alias(&mut self, v: Vid, target: Vid) {
        let rv = self.find(v);
        let rt = self.find(target);
        if rv != rt {
            let n = self.nodes[rv.0 as usize].clone();
            self.nodes[rv.0 as usize].parent = Some(rt);
            self.nodes[rt.0 as usize].prov.merge(&n.prov);
        }
    }

    fn bind(&mut self, v: Vid, term: Term) {
        let r = self.find(v);
        let node = &mut self.nodes[r.0 as usize];
        node.term = Some(term);
        node.prov.merge(&term_prov(node.term.as_ref().unwrap()));
    }
}

pub fn term_prov(t: &Term) -> Prov {
    match t {
        Term::TInt(p) | Term::TBool(p) | Term::TStr(p) | Term::TUnit(p) | Term::TRecord(_, p) | Term::TVariant(_, p) => {
            p.clone()
        }
        Term::TFun(a, b, p) => {
            let mut m = p.clone();
            m.merge(&a.1);
            m.merge(&b.1);
            m
        }
    }
}

fn free_var_term(v: Vid) -> Term {
    let name = var_name(v.0);
    let p = Prov::default();
    let _ = name;
    Term::TVarFree(v, p)
}

/// Variable placeholder used only while snapshotting / pretty-printing.
#[derive(Clone, Debug)]
pub enum DTerm {
    DInt,
    DBool,
    DStr,
    DUnit,
    DVar(u32),
    DParam(Pid),
    DFun(Box<DTerm>, Box<DTerm>),
    DRecord(Vec<(String, DTerm)>, Option<u32>),
    DVariant(Vec<(String, DTerm)>, Option<u32>),
}

/// Name generator for free variables (deterministic within a run).
pub fn var_name(n: u32) -> String {
    let mut n = n;
    let mut s = String::new();
    loop {
        s.insert(0, (b'a' + (n % 26) as u8) as char);
        n /= 26;
        if n == 0 {
            break;
        }
        n -= 1;
    }
    s
}

pub fn param_name(n: u32) -> String {
    format!("'p{}", n)
}

fn empty_open_row(v: Vid, p: Prov) -> Term {
    let kind = Kind::Row;
    let _ = kind;
    Term::TRecord(
        Box::new(Rows { fields: BTreeMap::new(), tail: Tail::Open(v), prov: p.clone() }),
        p,
    )
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub params: Vec<(Pid, Kind)>,
    pub body: DTerm,
    pub prov: Prov,
}
