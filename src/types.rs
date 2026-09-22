use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};

/// 类型变量身份:由产生它的语法节点路径加序号构成(如 "0.1#0"),
/// 与编辑位置无关,因此增量编辑时身份稳定。
pub type VarId = String;

pub type Subst = HashMap<VarId, Type>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Type {
    Var(VarId),
    Con(String),
    Fun(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Row {
    pub fields: BTreeMap<String, Type>,
    pub tail: Option<VarId>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Scheme {
    pub vars: Vec<VarId>,
    pub ty: Type,
}

impl Type {
    pub fn con(name: &str) -> Type {
        Type::Con(name.to_string())
    }
    pub fn fun(a: Type, b: Type) -> Type {
        Type::Fun(Box::new(a), Box::new(b))
    }
    pub fn var(id: VarId) -> Type {
        Type::Var(id)
    }
}

impl Scheme {
    pub fn mono(ty: Type) -> Scheme {
        Scheme { vars: vec![], ty }
    }
}

/// 行变量生成器(统一过程中需要新鲜的行尾变量)。
pub struct VarGen(pub String, pub usize);

impl VarGen {
    pub fn fresh(&mut self) -> VarId {
        self.1 += 1;
        format!("{}{}", self.0, self.1)
    }
}

pub fn free_vars(t: &Type) -> BTreeSet<VarId> {
    let mut out = BTreeSet::new();
    fv(t, &mut out);
    out
}

fn fv(t: &Type, out: &mut BTreeSet<VarId>) {
    match t {
        Type::Var(v) => {
            out.insert(v.clone());
        }
        Type::Con(_) => {}
        Type::Fun(a, b) => {
            fv(a, out);
            fv(b, out);
        }
        Type::Record(r) | Type::Variant(r) => {
            for v in r.fields.values() {
                fv(v, out);
            }
            if let Some(tail) = &r.tail {
                out.insert(tail.clone());
            }
        }
    }
}

pub fn apply(subst: &Subst, t: &Type) -> Type {
    match t {
        Type::Var(v) => match subst.get(v) {
            Some(t2) => apply(subst, t2),
            None => t.clone(),
        },
        Type::Con(_) => t.clone(),
        Type::Fun(a, b) => Type::fun(apply(subst, a), apply(subst, b)),
        Type::Record(r) => Type::Record(apply_row(subst, r)),
        Type::Variant(r) => Type::Variant(apply_row(subst, r)),
    }
}

pub fn apply_row(subst: &Subst, row: &Row) -> Row {
    let mut fields: BTreeMap<String, Type> = row
        .fields
        .iter()
        .map(|(k, v)| (k.clone(), apply(subst, v)))
        .collect();
    let mut tail = row.tail.clone();
    loop {
        match &tail {
            Some(v) => match subst.get(v) {
                Some(Type::Var(v2)) => {
                    let v2 = v2.clone();
                    tail = Some(v2);
                }
                Some(Type::Record(r2)) => {
                    for (k, v) in &r2.fields {
                        fields.entry(k.clone()).or_insert_with(|| apply(subst, v));
                    }
                    tail = r2.tail.clone();
                }
                Some(_) => {
                    tail = None;
                }
                None => break,
            },
            None => break,
        }
    }
    Row { fields, tail }
}

pub fn occurs(var: &VarId, t: &Type) -> bool {
    free_vars(t).contains(var)
}

/// 实例化:把量化变量替换为给定映射(通常为新鲜变量)。
pub fn subst_vars(t: &Type, map: &HashMap<VarId, Type>) -> Type {
    match t {
        Type::Var(v) => map.get(v).cloned().unwrap_or_else(|| t.clone()),
        Type::Con(_) => t.clone(),
        Type::Fun(a, b) => Type::fun(subst_vars(a, map), subst_vars(b, map)),
        Type::Record(r) => Type::Record(subst_vars_row(r, map)),
        Type::Variant(r) => Type::Variant(subst_vars_row(r, map)),
    }
}

fn subst_vars_row(r: &Row, map: &HashMap<VarId, Type>) -> Row {
    Row {
        fields: r.fields.iter().map(|(k, v)| (k.clone(), subst_vars(v, map))).collect(),
        tail: r.tail.as_ref().map(|v| match map.get(v) {
            Some(Type::Var(v2)) => v2.clone(),
            _ => v.clone(),
        }),
    }
}

#[derive(Debug, Clone)]
pub enum UnifyError {
    Mismatch(Type, Type),
    Infinite(VarId, Type),
    /// 封闭记录/变体缺少字段:字段列表 + 缺字段的那个类型。
    MissingField(Vec<String>, Type),
    /// 封闭记录/变体包含多余字段:字段列表 + 含多余字段的那个类型。
    ExtraField(Vec<String>, Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RowKind {
    Record,
    Variant,
}

fn row_type(kind: RowKind, row: Row) -> Type {
    match kind {
        RowKind::Record => Type::Record(row),
        RowKind::Variant => Type::Variant(row),
    }
}

pub fn unify(
    subst: &mut Subst,
    a: &Type,
    b: &Type,
    gen: &mut VarGen,
) -> Result<(), UnifyError> {
    let a = apply(subst, a);
    let b = apply(subst, b);
    match (&a, &b) {
        (Type::Var(x), Type::Var(y)) if x == y => Ok(()),
        (Type::Var(x), _) => bind(subst, x, &b),
        (_, Type::Var(y)) => bind(subst, y, &a),
        (Type::Con(x), Type::Con(y)) if x == y => Ok(()),
        (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
            unify(subst, a1, a2, gen)?;
            unify(subst, b1, b2, gen)
        }
        (Type::Record(r1), Type::Record(r2)) => unify_rows(subst, r1, r2, RowKind::Record, gen),
        (Type::Variant(r1), Type::Variant(r2)) => unify_rows(subst, r1, r2, RowKind::Variant, gen),
        _ => Err(UnifyError::Mismatch(a, b)),
    }
}

fn bind(subst: &mut Subst, var: &VarId, t: &Type) -> Result<(), UnifyError> {
    if occurs(var, t) {
        return Err(UnifyError::Infinite(var.clone(), t.clone()));
    }
    subst.insert(var.clone(), t.clone());
    Ok(())
}

fn unify_rows(
    subst: &mut Subst,
    r1: &Row,
    r2: &Row,
    kind: RowKind,
    gen: &mut VarGen,
) -> Result<(), UnifyError> {
    let r1 = apply_row(subst, r1);
    let r2 = apply_row(subst, r2);
    let mut only1 = Vec::new();
    let mut only2 = Vec::new();
    for (k, v1) in &r1.fields {
        match r2.fields.get(k) {
            Some(v2) => unify(subst, v1, v2, gen)?,
            None => only1.push(k.clone()),
        }
    }
    for k in r2.fields.keys() {
        if !r1.fields.contains_key(k) {
            only2.push(k.clone());
        }
    }
    let pick = |r: &Row, keys: &[String]| -> BTreeMap<String, Type> {
        keys.iter().map(|k| (k.clone(), r.fields[k].clone())).collect()
    };
    match (&r1.tail, &r2.tail) {
        (None, None) => {
            if !only1.is_empty() {
                return Err(UnifyError::ExtraField(only1, row_type(kind, r1)));
            }
            if !only2.is_empty() {
                return Err(UnifyError::MissingField(only2, row_type(kind, r1)));
            }
            Ok(())
        }
        (Some(v1), None) => {
            if !only1.is_empty() {
                // 封闭一侧缺少开放行要求的字段。
                return Err(UnifyError::MissingField(only1, row_type(kind, r2)));
            }
            bind(
                subst,
                v1,
                &Type::Record(Row {
                    fields: pick(&r2, &only2),
                    tail: None,
                }),
            )
        }
        (None, Some(v2)) => {
            if !only2.is_empty() {
                return Err(UnifyError::MissingField(only2, row_type(kind, r1)));
            }
            bind(
                subst,
                v2,
                &Type::Record(Row {
                    fields: pick(&r1, &only1),
                    tail: None,
                }),
            )
        }
        (Some(v1), Some(v2)) => {
            if v1 == v2 {
                if !only1.is_empty() {
                    return Err(UnifyError::ExtraField(only1, row_type(kind, r1)));
                }
                if !only2.is_empty() {
                    return Err(UnifyError::MissingField(only2, row_type(kind, r1)));
                }
                return Ok(());
            }
            let fresh = gen.fresh();
            bind(
                subst,
                v1,
                &Type::Record(Row {
                    fields: pick(&r2, &only2),
                    tail: Some(fresh.clone()),
                }),
            )?;
            bind(
                subst,
                v2,
                &Type::Record(Row {
                    fields: pick(&r1, &only1),
                    tail: Some(fresh),
                }),
            )
        }
    }
}

/// 稳定的类型变量命名器:按出现顺序命名为 'a, 'b, ...
pub struct Namer {
    map: HashMap<VarId, String>,
    next: usize,
}

impl Namer {
    pub fn new() -> Self {
        Namer {
            map: HashMap::new(),
            next: 0,
        }
    }
    pub fn name(&mut self, v: &VarId) -> String {
        if let Some(s) = self.map.get(v) {
            return s.clone();
        }
        let n = self.next;
        self.next += 1;
        let s = if n < 26 {
            ((b'a' + n as u8) as char).to_string()
        } else {
            format!("a{}", n - 26)
        };
        self.map.insert(v.clone(), s.clone());
        s
    }
}

impl Default for Namer {
    fn default() -> Self {
        Self::new()
    }
}

pub fn render(t: &Type, n: &mut Namer) -> String {
    match t {
        Type::Var(v) => format!("'{}", n.name(v)),
        Type::Con(c) => c.clone(),
        Type::Fun(a, b) => {
            let left = match a.as_ref() {
                Type::Fun(..) => format!("({})", render(a, n)),
                _ => render(a, n),
            };
            format!("{} -> {}", left, render(b, n))
        }
        Type::Record(r) => format!("{{{}}}", render_row(r, n)),
        Type::Variant(r) => format!("<{}>", render_row(r, n)),
    }
}

fn render_row(r: &Row, n: &mut Namer) -> String {
    let mut parts: Vec<String> = r
        .fields
        .iter()
        .map(|(k, v)| format!("{k}: {}", render(v, n)))
        .collect();
    if let Some(tail) = &r.tail {
        parts.push(format!("| '{}", n.name(tail)));
    }
    parts.join(", ")
}

pub fn render_scheme(s: &Scheme, n: &mut Namer) -> String {
    if s.vars.is_empty() {
        render(&s.ty, n)
    } else {
        let names: Vec<String> = s.vars.iter().map(|v| format!("'{}", n.name(v))).collect();
        format!("∀ {}. {}", names.join(" "), render(&s.ty, n))
    }
}

/// 方案的结构化指纹:量化变量按出现顺序归一化为 %0, %1, ...,
/// 自由(单态)变量保留其稳定身份。与变量名无关,因此改名不改变指纹。
pub fn scheme_fingerprint(s: &Scheme) -> String {
    let mut out = String::new();
    norm(&s.ty, &s.vars, &mut out);
    out
}

fn norm(t: &Type, qvars: &[VarId], out: &mut String) {
    match t {
        Type::Var(v) => {
            if let Some(i) = qvars.iter().position(|q| q == v) {
                out.push_str(&format!("%{i}"));
            } else {
                out.push_str(&format!("${v}"));
            }
        }
        Type::Con(c) => out.push_str(c),
        Type::Fun(a, b) => {
            out.push_str("(->");
            norm(a, qvars, out);
            norm(b, qvars, out);
            out.push(')');
        }
        Type::Record(r) | Type::Variant(r) => {
            out.push('{');
            for (k, v) in &r.fields {
                out.push_str(k);
                out.push(':');
                norm(v, qvars, out);
                out.push(';');
            }
            match &r.tail {
                Some(tv) => {
                    out.push('|');
                    if let Some(i) = qvars.iter().position(|q| q == tv) {
                        out.push_str(&format!("%{i}"));
                    } else {
                        out.push_str(&format!("${tv}"));
                    }
                }
                None => out.push('.'),
            }
            out.push('}');
        }
    }
}
