//! Type terms, rows, schemes and display.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt::Write as _;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Ty {
    Int,
    Bool,
    Fun(Box<Ty>, Box<Ty>),
    Record(Row),
    Variant(Row),
    Var(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Row {
    pub fields: BTreeMap<String, Ty>,
    pub tail: Option<u32>,
}

impl Row {
    pub fn closed(fields: BTreeMap<String, Ty>) -> Row { Row { fields, tail: None } }
    pub fn open(tail: u32) -> Row { Row { fields: BTreeMap::new(), tail: Some(tail) } }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<u32>,
    pub ty: Ty,
}

pub fn show_ty(t: &Ty, subst: &dyn Fn(u32) -> Option<Ty>) -> String {
    let mut names: BTreeMap<u32, String> = BTreeMap::new();
    let mut s = String::new();
    show_ty_into(t, subst, &mut names, &mut s, false);
    s
}

fn name_of(names: &mut BTreeMap<u32, String>, v: u32) -> String {
    if let Some(n) = names.get(&v) { return n.clone(); }
    let n = format!("'{}", (b'a' + (names.len() as u8) % 26) as char);
    names.insert(v, n.clone());
    n
}

fn show_ty_into(
    t: &Ty,
    subst: &dyn Fn(u32) -> Option<Ty>,
    names: &mut BTreeMap<u32, String>,
    out: &mut String,
    paren_fun: bool,
) {
    match t {
        Ty::Int => out.push_str("Int"),
        Ty::Bool => out.push_str("Bool"),
        Ty::Var(v) => {
            if let Some(r) = subst(*v) {
                show_ty_into(&r, subst, names, out, paren_fun);
            } else {
                let n = name_of(names, *v);
                out.push_str(&n);
            }
        }
        Ty::Fun(a, b) => {
            if paren_fun { out.push('('); }
            show_ty_into(a, subst, names, out, true);
            out.push_str(" -> ");
            show_ty_into(b, subst, names, out, false);
            if paren_fun { out.push(')'); }
        }
        Ty::Record(r) => {
            out.push('{');
            show_row(r, subst, names, out);
            out.push('}');
        }
        Ty::Variant(r) => {
            out.push_str("<");
            show_row(r, subst, names, out);
            out.push('>');
        }
    }
}

fn show_row(r: &Row, subst: &dyn Fn(u32) -> Option<Ty>, names: &mut BTreeMap<u32, String>, out: &mut String) {
    let mut first = true;
    for (l, t) in &r.fields {
        if !first { out.push_str(", "); }
        first = false;
        let _ = write!(out, "{}: ", l);
        show_ty_into(t, subst, names, out, false);
    }
    match &r.tail {
        Some(v) => {
            if let Some(Ty::Record(rr)) = subst(*v) {
                for (l, t) in &rr.fields {
                    if !first { out.push_str(", "); }
                    first = false;
                    let _ = write!(out, "{}: ", l);
                    show_ty_into(t, subst, names, out, false);
                }
                if let Some(tv) = rr.tail {
                    if !first { out.push_str(", "); }
                    let _ = write!(out, "..{}", name_of(names, tv));
                }
            } else {
                if !first { out.push_str(", "); }
                let _ = write!(out, "..{}", name_of(names, *v));
            }
        }
        None => {}
    }
}
