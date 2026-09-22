//! 类型、行类型、方案(scheme)与替换。

use std::collections::{HashMap, HashSet};
use std::fmt;

pub type TVar = u32;
pub type RVar = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var(TVar),
    Int,
    Bool,
    Str,
    Fun(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Row {
    Empty,
    Var(RVar),
    Extend { label: String, ty: Box<Type>, rest: Box<Row> },
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub tvars: Vec<TVar>,
    pub rvars: Vec<RVar>,
    pub ty: Type,
}

#[derive(Default, Clone, Debug)]
pub struct Subst {
    pub tys: HashMap<TVar, Type>,
    pub rows: HashMap<RVar, Row>,
}

impl Type {
    pub fn fun(a: Type, b: Type) -> Type {
        Type::Fun(Box::new(a), Box::new(b))
    }
    pub fn record_closed(fields: Vec<(String, Type)>) -> Type {
        let mut row = Row::Empty;
        for (l, t) in fields.into_iter().rev() {
            row = Row::Extend { label: l, ty: Box::new(t), rest: Box::new(row) };
        }
        Type::Record(row)
    }
}

impl Row {
    pub fn extend(label: &str, ty: Type, rest: Row) -> Row {
        Row::Extend { label: label.to_string(), ty: Box::new(ty), rest: Box::new(rest) }
    }
}

impl Subst {
    pub fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(v) => match self.tys.get(v) {
                Some(t) => self.resolve(t),
                None => ty.clone(),
            },
            Type::Int | Type::Bool | Type::Str => ty.clone(),
            Type::Fun(a, b) => Type::fun(self.resolve(a), self.resolve(b)),
            Type::Record(r) => Type::Record(self.resolve_row(r)),
            Type::Variant(r) => Type::Variant(self.resolve_row(r)),
        }
    }

    pub fn resolve_row(&self, row: &Row) -> Row {
        match row {
            Row::Empty => Row::Empty,
            Row::Var(v) => match self.rows.get(v) {
                Some(r) => self.resolve_row(r),
                None => row.clone(),
            },
            Row::Extend { label, ty, rest } => Row::Extend {
                label: label.clone(),
                ty: Box::new(self.resolve(ty)),
                rest: Box::new(self.resolve_row(rest)),
            },
        }
    }
}

/// 把行重写为 (有序字段表, 行尾)。
pub fn rewrite_row(row: &Row) -> (Vec<(String, Type)>, Row) {
    let mut fields: Vec<(String, Type)> = Vec::new();
    let mut cur = row.clone();
    loop {
        match cur {
            Row::Extend { label, ty, rest } => {
                fields.push((label, *ty));
                cur = *rest;
            }
            tail => break,
        }
    }
    fields.sort_by(|a, b| a.0.cmp(&b.0));
    (fields, cur)
}

pub fn ftv_type(ty: &Type, tvs: &mut HashSet<TVar>, rvs: &mut HashSet<RVar>) {
    match ty {
        Type::Var(v) => {
            tvs.insert(*v);
        }
        Type::Int | Type::Bool | Type::Str => {}
        Type::Fun(a, b) => {
            ftv_type(a, tvs, rvs);
            ftv_type(b, tvs, rvs);
        }
        Type::Record(r) | Type::Variant(r) => ftv_row(r, tvs, rvs),
    }
}

pub fn ftv_row(row: &Row, tvs: &mut HashSet<TVar>, rvs: &mut HashSet<RVar>) {
    match row {
        Row::Empty => {}
        Row::Var(v) => {
            rvs.insert(*v);
        }
        Row::Extend { ty, rest, .. } => {
            ftv_type(ty, tvs, rvs);
            ftv_row(rest, tvs, rvs);
        }
    }
}

pub fn fmt_type(ty: &Type) -> String {
    // 给变量起可读名字: t0->'a, t1->'b ...
    let mut names: HashMap<TVar, String> = HashMap::new();
    let mut rnames: HashMap<RVar, String> = HashMap::new();
    let mut counter = 0u32;
    fn name(counter: &mut u32) -> String {
        let n = *counter;
        *counter += 1;
        let c = (b'a' + (n % 26) as u8) as char;
        if n < 26 {
            format!("'{c}")
        } else {
            format!("'{c}{}", n / 26)
        }
    }
    fn go(ty: &Type, names: &mut HashMap<TVar, String>, rnames: &mut HashMap<RVar, String>, counter: &mut u32, out: &mut String, prec: u8) {
        match ty {
            Type::Var(v) => {
                let n = names.entry(*v).or_insert_with(|| name(counter)).clone();
                out.push_str(&n);
            }
            Type::Int => out.push_str("int"),
            Type::Bool => out.push_str("bool"),
            Type::Str => out.push_str("string"),
            Type::Fun(a, b) => {
                if prec > 0 {
                    out.push('(');
                }
                go(a, names, rnames, counter, out, 1);
                out.push_str(" -> ");
                go(b, names, rnames, counter, out, 0);
                if prec > 0 {
                    out.push(')');
                }
            }
            Type::Record(r) => {
                out.push('{');
                fmt_row(r, names, rnames, counter, out);
                out.push('}');
            }
            Type::Variant(r) => {
                out.push('<');
                fmt_row(r, names, rnames, counter, out);
                out.push('>');
            }
        }
    }
    fn fmt_row(row: &Row, names: &mut HashMap<TVar, String>, rnames: &mut HashMap<RVar, String>, counter: &mut u32, out: &mut String) {
        let (fields, tail) = rewrite_row(row);
        let mut first = true;
        for (l, t) in &fields {
            if !first {
                out.push_str(", ");
            }
            first = false;
            out.push_str(l);
            out.push_str(": ");
            go(t, names, rnames, counter, out, 0);
        }
        match tail {
            Row::Var(v) => {
                if !first {
                    out.push_str(" | ");
                }
                let n = rnames.entry(v).or_insert_with(|| format!("ρ{}", counter_helper(counter))).clone();
                out.push_str(&n);
            }
            Row::Empty => {}
            Row::Extend { .. } => unreachable!(),
        }
    }
    fn counter_helper(c: &mut u32) -> u32 {
        *c += 1;
        *c
    }
    let mut s = String::new();
    go(ty, &mut names, &mut rnames, &mut counter, &mut s, 0);
    s
}

pub fn fmt_scheme(sc: &Scheme) -> String {
    if sc.tvars.is_empty() && sc.rvars.is_empty() {
        return fmt_type(&sc.ty);
    }
    let qs: Vec<String> = sc
        .tvars
        .iter()
        .map(|v| format!("t{v}"))
        .chain(sc.rvars.iter().map(|v| format!("r{v}")))
        .collect();
    format!("∀{}. {}", qs.join(" "), fmt_type(&sc.ty))
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_type(self))
    }
}
