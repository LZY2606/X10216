use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub struct SVar(pub u32);

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub enum Ty {
    Var(SVar),
    Con(String),
    Arrow(Box<Ty>, Box<Ty>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub enum RowTail {
    Open(SVar),
    Closed,
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub struct Row {
    pub fields: BTreeMap<String, Ty>,
    pub tail: RowTail,
}

impl Row {
    pub fn closed(fields: BTreeMap<String, Ty>) -> Self { Row { fields, tail: RowTail::Closed } }
    pub fn open(fields: BTreeMap<String, Ty>, v: SVar) -> Self { Row { fields, tail: RowTail::Open(v) } }
    pub fn empty_closed() -> Self { Row { fields: BTreeMap::new(), tail: RowTail::Closed } }
}

impl Ty {
    pub fn int() -> Self { Ty::Con("Int".into()) }
    pub fn bool() -> Self { Ty::Con("Bool".into()) }
    pub fn unit() -> Self { Ty::Con("Unit".into()) }
    pub fn arrow(a: Ty, b: Ty) -> Self { Ty::Arrow(Box::new(a), Box::new(b)) }

    pub fn vars(&self, out: &mut Vec<SVar>) {
        match self {
            Ty::Var(v) => { if !out.contains(v) { out.push(v.clone()); } }
            Ty::Con(_) => {}
            Ty::Arrow(a, b) => { a.vars(out); b.vars(out); }
            Ty::Record(r) | Ty::Variant(r) => {
                for t in r.fields.values() { t.vars(out); }
                if let RowTail::Open(v) = &r.tail {
                    if !out.contains(v) { out.push(v.clone()); }
                }
            }
        }
    }

    pub fn pretty(&self) -> String {
        let mut names: BTreeMap<u32, String> = BTreeMap::new();
        let mut letters: Vec<char> = ('a'..='z').collect();
        let mut idx = 0usize;
        let mut all = Vec::new();
        self.vars(&mut all);
        for v in &all {
            names.entry(v.0).or_insert_with(|| {
                let n = if idx < letters.len() {
                    let c = letters[idx]; idx += 1; c.to_string()
                } else {
                    let n = format!("t{}", idx); idx += 1; n
                };
                n
            });
        }
        fn go(t: &Ty, names: &BTreeMap<u32, String>) -> String {
            match t {
                Ty::Var(SVar(i)) => names.get(i).cloned().unwrap_or_else(|| format!("?{}", i)),
                Ty::Con(s) => s.clone(),
                Ty::Arrow(a, b) => {
                    let pa = match a.as_ref() { Ty::Arrow(_, _) => format!("({})", go(a, names)), _ => go(a, names) };
                    format!("{} -> {}", pa, go(b, names))
                }
                Ty::Record(r) => {
                    let inner = render_row(r, names, false);
                    format!("{{{}}}", inner)
                }
                Ty::Variant(r) => {
                    let inner = render_row(r, names, true);
                    format!("[{}]", inner)
                }
            }
        }
        fn render_row(r: &Row, names: &BTreeMap<u32, String>, variant: bool) -> String {
            let mut parts: Vec<String> = r.fields.iter()
                .map(|(k, v)| if variant { format!("`{} {}", k, go(v, names)) } else { format!("{} = {}", k, go(v, names)) })
                .collect();
            match &r.tail {
                RowTail::Open(SVar(i)) => parts.push(format!("| {}", names.get(i).cloned().unwrap_or_else(|| format!("?{}", i)))),
                RowTail::Closed => {}
            }
            parts.join(", ")
        }
        go(self, &names)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, Debug)]
pub struct Scheme {
    pub quant: Vec<SVar>,
    pub body: Ty,
}

impl Scheme {
    pub fn mono(t: Ty) -> Self { Scheme { quant: vec![], body: t } }
    pub fn pretty(&self) -> String {
        if self.quant.is_empty() { self.body.pretty() }
        else {
            let q: Vec<String> = self.quant.iter().map(|v| self.body.pretty()).take(0).collect();
            let _ = q;
            format!("∀{}. {}", self.quant.iter().map(|SVar(i)| format!("t{}", i)).collect::<Vec<_>>().join(" "), self.body.pretty())
        }
    }
}
