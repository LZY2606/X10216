//! Internal type representation: union-find metas, structural terms and
//! quantified schemes, plus the provenance-bearing `TypeView` used by the UI.

use crate::syntax::NodeId;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};

pub type MetaId = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TCon {
    Int,
    Bool,
    String,
    Unit,
}

impl TCon {
    pub fn name(&self) -> &'static str {
        match self {
            TCon::Int => "int",
            TCon::Bool => "bool",
            TCon::String => "string",
            TCon::Unit => "unit",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Term {
    /// Unresolved unification variable (id into the solver meta table).
    Var(MetaId),
    /// Quantified variable inside a frozen scheme (De Bruijn-ish index).
    QVar(u32),
    Con(TCon),
    Fun(Box<Term>, Box<Term>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Row {
    /// Always sorted by label; each label appears once in a well-formed row.
    pub fields: Vec<(String, Term)>,
    pub end: RowEnd,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum RowEnd {
    Closed,
    Open(Box<Term>),
}

impl Row {
    pub fn new(mut fields: Vec<(String, Term)>, end: RowEnd) -> Self {
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        Row { fields, end }
    }

    pub fn closed(fields: Vec<(String, Term)>) -> Self {
        Row::new(fields, RowEnd::Closed)
    }

    pub fn open(fields: Vec<(String, Term)>, tail: Term) -> Self {
        Row::new(fields, RowEnd::Open(Box::new(tail)))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum TVarKind {
    Star,
    Row,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Meta {
    pub id: MetaId,
    pub level: u32,
    pub kind: TVarKind,
    pub origin: NodeId,
    /// Human readable role, e.g. "参数 x 的类型", "let rhs 结果".
    pub label: String,
}

// ---------------------------------------------------------------------------
// Term traversal helpers
// ---------------------------------------------------------------------------

pub fn free_vars(t: &Term, out: &mut Vec<MetaId>) {
    match t {
        Term::Var(v) => out.push(*v),
        Term::QVar(_) | Term::Con(_) => {}
        Term::Fun(a, b) => {
            free_vars(a, out);
            free_vars(b, out);
        }
        Term::Record(r) | Term::Variant(r) => {
            for (_, f) in &r.fields {
                free_vars(f, out);
            }
            if let RowEnd::Open(tail) = &r.end {
                free_vars(tail, out);
            }
        }
    }
}

pub fn free_var_set(t: &Term) -> BTreeSet<MetaId> {
    let mut v = Vec::new();
    free_vars(t, &mut v);
    v.into_iter().collect()
}

pub fn qvars(t: &Term) -> BTreeSet<u32> {
    let mut out = BTreeSet::new();
    fn go(t: &Term, out: &mut BTreeSet<u32>) {
        match t {
            Term::QVar(i) => {
                out.insert(*i);
            }
            Term::Var(_) | Term::Con(_) => {}
            Term::Fun(a, b) => {
                go(a, out);
                go(b, out);
            }
            Term::Record(r) | Term::Variant(r) => {
                for (_, f) in &r.fields {
                    go(f, out);
                }
                if let RowEnd::Open(tail) = &r.end {
                    go(tail, out);
                }
            }
        }
    }
    go(t, &mut out);
    out
}

/// Deep-copy a scheme body, renaming QVars with fresh indices.
pub fn instantiate_quantified(body: &Term, qs: &BTreeSet<u32>, next_q: &mut u32) -> Term {
    let mut map: HashMap<u32, u32> = HashMap::new();
    for q in qs {
        map.insert(*q, *next_q);
        *next_q += 1;
    }
    rename_qvars(body, &map)
}

pub fn rename_qvars(t: &Term, map: &HashMap<u32, u32>) -> Term {
    match t {
        Term::QVar(i) => Term::QVar(*map.get(i).unwrap_or(i)),
        Term::Var(_) | Term::Con(_) => t.clone(),
        Term::Fun(a, b) => Term::Fun(
            Box::new(rename_qvars(a, map)),
            Box::new(rename_qvars(b, map)),
        ),
        Term::Record(r) => Term::Record(rename_row(r, map)),
        Term::Variant(r) => Term::Variant(rename_row(r, map)),
    }
}

fn rename_row(r: &Row, map: &HashMap<u32, u32>) -> Row {
    Row {
        fields: r.fields.iter().map(|(l, t)| (l.clone(), rename_qvars(t, map))).collect(),
        end: match &r.end {
            RowEnd::Closed => RowEnd::Closed,
            RowEnd::Open(t) => RowEnd::Open(Box::new(rename_qvars(t, map))),
        },
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Scheme {
    /// Quantified variable indices appearing in `body`.
    pub quantifiers: Vec<u32>,
    pub body: Term,
}

// ---------------------------------------------------------------------------
// Pretty printing
// ---------------------------------------------------------------------------

pub fn qname(i: u32) -> String {
    let mut i = i;
    let mut s = String::from("'");
    loop {
        s.push((b'a' + (i % 26) as u8) as char);
        i /= 26;
        if i == 0 {
            break;
        }
        i -= 1;
    }
    s
}

pub fn render(t: &Term) -> String {
    match t {
        Term::Var(v) => format!("?{v}"),
        Term::QVar(i) => qname(*i),
        Term::Con(c) => c.name().to_string(),
        Term::Fun(a, b) => format!("{} -> {}", render_paren(a), render(b)),
        Term::Record(r) => {
            let mut s = String::from("{ ");
            let mut parts: Vec<String> =
                r.fields.iter().map(|(l, t)| format!("{l}: {}", render(t))).collect();
            if let RowEnd::Open(tail) = &r.end {
                parts.push(format!("| {}", render(tail)));
            }
            s.push_str(&parts.join(", "));
            s.push_str(" }");
            s
        }
        Term::Variant(r) => {
            let mut s = String::from("[ ");
            let mut parts: Vec<String> =
                r.fields.iter().map(|(l, t)| format!("`{l} {}", render_paren(t))).collect();
            if let RowEnd::Open(tail) = &r.end {
                parts.push(format!("| {}", render(tail)));
            }
            s.push_str(&parts.join(", "));
            s.push_str(" ]");
            s
        }
    }
}

fn render_paren(t: &Term) -> String {
    match t {
        Term::Fun(_, _) => format!("({})", render(t)),
        _ => render(t),
    }
}

pub fn render_scheme(s: &Scheme) -> String {
    if s.quantifiers.is_empty() {
        render(&s.body)
    } else {
        let names: Vec<String> = s.quantifiers.iter().map(|q| qname(*q)).collect();
        format!("forall {}. {}", names.join(" "), render(&s.body))
    }
}

// ---------------------------------------------------------------------------
// Provenance bearing type tree served to the UI
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TypeView {
    pub origins: BTreeSet<u64>,
    #[serde(flatten)]
    pub node: ViewNode,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "node")]
pub enum ViewNode {
    Var { id: MetaId, level: u32, kind: TVarKind, label: String },
    QVar { index: u32 },
    Con { name: String },
    Fun { param: Box<TypeView>, result: Box<TypeView> },
    Record { fields: Vec<(String, TypeView)>, tail: Option<Box<TypeView>> },
    Variant { fields: Vec<(String, TypeView)>, tail: Option<Box<TypeView>> },
}
