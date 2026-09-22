use crate::syntax::ExprId;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};

pub type VarId = u32;
pub type ConId = u64;

/// 来源:类型结构的每一部分记录产生它的表达式与约束。
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Prov {
    pub expr: Option<ExprId>,
    pub con: Option<ConId>,
}

impl Prov {
    pub fn of(expr: ExprId, con: ConId) -> Self {
        Prov {
            expr: Some(expr),
            con: Some(con),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Ty {
    Var(VarId, Prov),
    Int(Prov),
    Bool(Prov),
    Fun(Box<Ty>, Box<Ty>, Prov),
    Record(Row, Prov),
    Variant(Row, Prov),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Row {
    pub fields: BTreeMap<String, Ty>,
    pub tail: Option<VarId>,
    pub prov: Prov,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<VarId>,
    pub ty: Ty,
}

impl Ty {
    pub fn prov(&self) -> Prov {
        match self {
            Ty::Var(_, p) | Ty::Int(p) | Ty::Bool(p) | Ty::Fun(_, _, p) | Ty::Record(_, p)
            | Ty::Variant(_, p) => *p,
        }
    }
}

/// 类型变量替换与行变量替换分离。
#[derive(Clone, Debug, Default)]
pub struct Subst {
    pub tys: HashMap<VarId, Ty>,
    pub rows: HashMap<VarId, Row>,
}

impl Subst {
    pub fn size(&self) -> usize {
        self.tys.len() + self.rows.len()
    }

    pub fn resolve(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(v, p) => {
                if let Some(t) = self.tys.get(v) {
                    self.resolve(t)
                } else {
                    Ty::Var(*v, *p)
                }
            }
            Ty::Int(p) => Ty::Int(*p),
            Ty::Bool(p) => Ty::Bool(*p),
            Ty::Fun(a, b, p) => Ty::Fun(Box::new(self.resolve(a)), Box::new(self.resolve(b)), *p),
            Ty::Record(r, p) => Ty::Record(self.resolve_row(r), *p),
            Ty::Variant(r, p) => Ty::Variant(self.resolve_row(r), *p),
        }
    }

    pub fn resolve_row(&self, row: &Row) -> Row {
        let mut fields: BTreeMap<String, Ty> = row
            .fields
            .iter()
            .map(|(k, v)| (k.clone(), self.resolve(v)))
            .collect();
        let mut tail = row.tail;
        let mut prov = row.prov;
        while let Some(v) = tail {
            if let Some(r) = self.rows.get(&v) {
                for (k, val) in &r.fields {
                    fields.entry(k.clone()).or_insert_with(|| self.resolve(val));
                }
                prov = r.prov;
                tail = r.tail;
            } else {
                break;
            }
        }
        Row { fields, tail, prov }
    }
}

pub fn fv_ty(ty: &Ty, out: &mut BTreeSet<VarId>) {
    match ty {
        Ty::Var(v, _) => {
            out.insert(*v);
        }
        Ty::Int(_) | Ty::Bool(_) => {}
        Ty::Fun(a, b, _) => {
            fv_ty(a, out);
            fv_ty(b, out);
        }
        Ty::Record(r, _) | Ty::Variant(r, _) => fv_row(r, out),
    }
}

pub fn fv_row(row: &Row, out: &mut BTreeSet<VarId>) {
    for t in row.fields.values() {
        fv_ty(t, out);
    }
    if let Some(v) = row.tail {
        out.insert(v);
    }
}

pub fn fv_scheme(s: &Scheme) -> BTreeSet<VarId> {
    let mut out = BTreeSet::new();
    fv_ty(&s.ty, &mut out);
    for v in &s.vars {
        out.remove(v);
    }
    out
}

/// occurs check:变量(类型或行)是否出现在类型中。
pub fn occurs(v: VarId, ty: &Ty, subst: &Subst) -> bool {
    let t = subst.resolve(ty);
    let mut set = BTreeSet::new();
    fv_ty_resolved(&t, &mut set, subst);
    set.contains(&v)
}

fn fv_ty_resolved(ty: &Ty, out: &mut BTreeSet<VarId>, subst: &Subst) {
    match ty {
        Ty::Var(v, _) => {
            out.insert(*v);
        }
        Ty::Int(_) | Ty::Bool(_) => {}
        Ty::Fun(a, b, _) => {
            fv_ty_resolved(a, out, subst);
            fv_ty_resolved(b, out, subst);
        }
        Ty::Record(r, _) | Ty::Variant(r, _) => {
            for t in r.fields.values() {
                fv_ty_resolved(t, out, subst);
            }
            if let Some(v) = r.tail {
                out.insert(v);
            }
        }
    }
}

/// 收集类型结构中的所有来源(表达式,约束),用于错误原因链。
pub fn prov_chain(ty: &Ty) -> Vec<Prov> {
    let mut out: Vec<Prov> = Vec::new();
    fn go(t: &Ty, out: &mut Vec<Prov>) {
        let p = t.prov();
        if (p.expr.is_some() || p.con.is_some()) && !out.contains(&p) {
            out.push(p);
        }
        match t {
            Ty::Fun(a, b, _) => {
                go(a, out);
                go(b, out);
            }
            Ty::Record(r, _) | Ty::Variant(r, _) => {
                if (r.prov.expr.is_some() || r.prov.con.is_some()) && !out.contains(&r.prov) {
                    out.push(r.prov);
                }
                for v in r.fields.values() {
                    go(v, out);
                }
            }
            _ => {}
        }
    }
    go(ty, &mut out);
    out
}

/// 稳定命名:按出现顺序命名为 a, b, ..., 便于增量/全量结果对比。
pub fn display_ty(ty: &Ty) -> String {
    let mut names: HashMap<VarId, String> = HashMap::new();
    let mut next = 0u32;
    let mut s = String::new();
    write_ty(ty, &mut names, &mut next, &mut s);
    s
}

fn var_name(names: &mut HashMap<VarId, String>, next: &mut u32, v: VarId) -> String {
    if let Some(n) = names.get(&v) {
        return n.clone();
    }
    let idx = *next;
    *next += 1;
    let base = (b'a' + (idx % 26) as u8) as char;
    let name = if idx < 26 {
        format!("'{}", base)
    } else {
        format!("'{}{}", base, idx / 26)
    };
    names.insert(v, name.clone());
    name
}

fn write_ty(ty: &Ty, names: &mut HashMap<VarId, String>, next: &mut u32, out: &mut String) {
    match ty {
        Ty::Var(v, _) => out.push_str(&var_name(names, next, *v)),
        Ty::Int(_) => out.push_str("Int"),
        Ty::Bool(_) => out.push_str("Bool"),
        Ty::Fun(a, b, _) => {
            out.push('(');
            write_ty(a, names, next, out);
            out.push_str(" -> ");
            write_ty(b, names, next, out);
            out.push(')');
        }
        Ty::Record(r, _) => write_row(r, "{", "}", names, next, out),
        Ty::Variant(r, _) => write_row(r, "[", "]", names, next, out),
    }
}

fn write_row(
    r: &Row,
    open: &str,
    close: &str,
    names: &mut HashMap<VarId, String>,
    next: &mut u32,
    out: &mut String,
) {
    out.push_str(open);
    let mut first = true;
    for (k, v) in &r.fields {
        if !first {
            out.push_str(", ");
        }
        first = false;
        out.push_str(k);
        out.push_str(": ");
        write_ty(v, names, next, out);
    }
    if let Some(t) = r.tail {
        if !first {
            out.push_str(" ");
        }
        out.push('|');
        out.push_str(&var_name(names, next, t));
    }
    out.push_str(close);
}

pub fn display_scheme(s: &Scheme) -> String {
    let mut names: HashMap<VarId, String> = HashMap::new();
    let mut next = 0u32;
    for v in &s.vars {
        var_name(&mut names, &mut next, *v);
    }
    let mut out = String::new();
    if !s.vars.is_empty() {
        out.push_str("forall ");
        for v in &s.vars {
            out.push_str(&names[v]);
            out.push(' ');
        }
        out.push_str(". ");
    }
    write_ty(&s.ty, &mut names, &mut next, &mut out);
    out
}
