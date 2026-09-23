use std::collections::HashMap;
use std::rc::Rc;

pub type Ty = Rc<TyNode>;
pub type RowRef = Rc<Row>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RowOrigin {
    Literal,
    Projection,
    Unknown,
}

#[derive(Debug, Clone, Default)]
pub struct Prov {
    pub cons: Vec<String>,
    pub exprs: Vec<String>,
}

impl Prov {
    pub fn expr(id: &str) -> Prov {
        Prov { cons: Vec::new(), exprs: vec![id.to_string()] }
    }
    pub fn merge(&self, other: &Prov) -> Prov {
        let mut out = self.clone();
        for c in &other.cons {
            if !out.cons.contains(c) {
                out.cons.push(c.clone());
            }
        }
        for e in &other.exprs {
            if !out.exprs.contains(e) {
                out.exprs.push(e.clone());
            }
        }
        out
    }
    pub fn with_con(mut self, key: &str) -> Prov {
        if !self.cons.iter().any(|c| c == key) {
            self.cons.push(key.to_string());
        }
        self
    }
}

#[derive(Debug)]
pub struct TyNode {
    pub kind: Kind,
    pub prov: Prov,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Var(u32),
    Int,
    Bool,
    Str,
    Unit,
    Fun(Ty, Ty),
    Record(RowRef),
    Variant(RowRef),
}

#[derive(Debug, Clone)]
pub enum Row {
    Var(u32),
    Empty,
    Extend {
        label: Rc<str>,
        ty: Ty,
        rest: RowRef,
        origin: RowOrigin,
        expr: Option<String>,
    },
}

pub fn ty(kind: Kind, prov: Prov) -> Ty {
    Rc::new(TyNode { kind, prov })
}

#[derive(Debug, Clone)]
pub struct Scheme {
    pub tvars: Vec<u32>,
    pub rvars: Vec<u32>,
    pub ty: Ty,
}

impl Scheme {
    pub fn mono(t: Ty) -> Scheme {
        Scheme { tvars: Vec::new(), rvars: Vec::new(), ty: t }
    }
}

pub fn ftv_ty(t: &Ty, tset: &mut Vec<u32>, rset: &mut Vec<u32>) {
    match &t.kind {
        Kind::Var(v) => {
            if !tset.contains(v) {
                tset.push(*v);
            }
        }
        Kind::Fun(a, b) => {
            ftv_ty(a, tset, rset);
            ftv_ty(b, tset, rset);
        }
        Kind::Record(r) | Kind::Variant(r) => ftv_row(r, tset, rset),
        _ => {}
    }
}

pub fn ftv_row(r: &Row, tset: &mut Vec<u32>, rset: &mut Vec<u32>) {
    match r {
        Row::Var(v) => {
            if !rset.contains(v) {
                rset.push(*v);
            }
        }
        Row::Empty => {}
        Row::Extend { ty, rest, .. } => {
            ftv_ty(ty, tset, rset);
            ftv_row(rest, tset, rset);
        }
    }
}

pub fn tyvar_name(i: usize) -> String {
    let c = (b'a' + (i % 26) as u8) as char;
    if i < 26 {
        format!("'{c}")
    } else {
        format!("'{c}{}", i / 26)
    }
}

pub struct Printer {
    tnames: HashMap<u32, usize>,
    rnames: HashMap<u32, usize>,
}

impl Printer {
    pub fn new() -> Printer {
        Printer { tnames: HashMap::new(), rnames: HashMap::new() }
    }
    pub fn tname(&mut self, v: u32) -> String {
        let n = self.tnames.len();
        tyvar_name(*self.tnames.entry(v).or_insert(n))
    }
    pub fn rname(&mut self, v: u32) -> String {
        let n = self.rnames.len();
        let i = *self.rnames.entry(v).or_insert(n);
        if i == 0 {
            "ρ".to_string()
        } else {
            format!("ρ{i}")
        }
    }
    pub fn print_ty(&mut self, t: &Ty) -> String {
        match &t.kind {
            Kind::Var(v) => self.tname(*v),
            Kind::Int => "Int".into(),
            Kind::Bool => "Bool".into(),
            Kind::Str => "String".into(),
            Kind::Unit => "Unit".into(),
            Kind::Fun(a, b) => format!("{} -> {}", self.print_atom(a), self.print_ty(b)),
            Kind::Record(r) => format!("{{{}}}", self.print_row(r)),
            Kind::Variant(r) => format!("<{}>", self.print_row(r)),
        }
    }
    fn print_atom(&mut self, t: &Ty) -> String {
        match &t.kind {
            Kind::Fun(..) => format!("({})", self.print_ty(t)),
            _ => self.print_ty(t),
        }
    }
    pub fn print_row(&mut self, r: &Row) -> String {
        let mut fields = Vec::new();
        let mut cur = r;
        let tail = loop {
            match cur {
                Row::Var(v) => break Some(self.rname(*v)),
                Row::Empty => break None,
                Row::Extend { label, ty, rest, .. } => {
                    let t = self.print_ty(ty);
                    fields.push(format!("{label}: {t}"));
                    cur = rest;
                }
            }
        };
        match tail {
            Some(t) if fields.is_empty() => format!("| {t}"),
            Some(t) => format!("{} | {t}", fields.join(", ")),
            None => fields.join(", "),
        }
    }
}

pub fn print_scheme(sch: &Scheme) -> String {
    let mut p = Printer::new();
    let mut qs = Vec::new();
    for v in &sch.tvars {
        qs.push(p.tname(*v));
    }
    for v in &sch.rvars {
        qs.push(p.rname(*v));
    }
    let body = p.print_ty(&sch.ty);
    if qs.is_empty() {
        body
    } else {
        format!("∀ {}. {}", qs.join(" "), body)
    }
}
