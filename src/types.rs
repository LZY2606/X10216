use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// 来源信息：每个类型构造子记录它是由哪段源码产生的。
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProvTag {
    /// 由字面量 / 表达式自身产生（如记录字面量）
    Literal,
    /// 由上下文需求产生（如字段访问、if 条件、运算符）
    Demand,
    Other,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Prov {
    pub span: Option<(usize, usize)>,
    pub note: String,
    pub tag: ProvTag,
}

impl Prov {
    pub fn none() -> Self {
        Prov { span: None, note: String::new(), tag: ProvTag::Other }
    }
    pub fn lit(span: (usize, usize), note: impl Into<String>) -> Self {
        Prov { span: Some(span), note: note.into(), tag: ProvTag::Literal }
    }
    pub fn demand(span: (usize, usize), note: impl Into<String>) -> Self {
        Prov { span: Some(span), note: note.into(), tag: ProvTag::Demand }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowKind {
    Unknown,
    Record,
    Variant,
}

#[derive(Clone, Debug)]
pub enum Type {
    Var(TVar),
    Int(Prov),
    Bool(Prov),
    Unit(Prov),
    Fun(Box<Type>, Box<Type>, Prov),
    Record(Row, Prov),
    Variant(Row, Prov),
}

#[derive(Clone, Debug)]
pub struct TVar(pub Rc<RefCell<TVarInner>>);

#[derive(Debug)]
pub struct TVarInner {
    pub name: String,
    pub level: u32,
    pub link: Option<Type>,
}

#[derive(Clone, Debug)]
pub enum Row {
    Empty,
    Extend { label: String, ty: Box<Type>, rest: Box<Row> },
    Var(RVar),
}

#[derive(Clone, Debug)]
pub struct RVar(pub Rc<RefCell<RVarInner>>);

#[derive(Debug)]
pub struct RVarInner {
    pub name: String,
    pub level: u32,
    pub kind: RowKind,
    pub link: Option<Row>,
}

impl TVar {
    pub fn new(name: String, level: u32) -> TVar {
        TVar(Rc::new(RefCell::new(TVarInner { name, level, link: None })))
    }
    pub fn name(&self) -> String {
        self.0.borrow().name.clone()
    }
    pub fn level(&self) -> u32 {
        self.0.borrow().level
    }
}

impl RVar {
    pub fn new(name: String, level: u32) -> RVar {
        RVar(Rc::new(RefCell::new(RVarInner { name, level, kind: RowKind::Unknown, link: None })))
    }
    pub fn name(&self) -> String {
        self.0.borrow().name.clone()
    }
    pub fn level(&self) -> u32 {
        self.0.borrow().level
    }
}

pub fn norm(t: &Type) -> Type {
    match t {
        Type::Var(v) => {
            let link = v.0.borrow().link.clone();
            match link {
                Some(inner) => {
                    let n = norm(&inner);
                    v.0.borrow_mut().link = Some(n.clone());
                    n
                }
                None => t.clone(),
            }
        }
        _ => t.clone(),
    }
}

pub fn norm_row(r: &Row) -> Row {
    match r {
        Row::Var(v) => {
            let link = v.0.borrow().link.clone();
            match link {
                Some(inner) => {
                    let n = norm_row(&inner);
                    v.0.borrow_mut().link = Some(n.clone());
                    n
                }
                None => r.clone(),
            }
        }
        _ => r.clone(),
    }
}

pub fn prov_of(t: &Type) -> Prov {
    match norm(t) {
        Type::Var(_) => Prov::none(),
        Type::Int(p) | Type::Bool(p) | Type::Unit(p) | Type::Fun(_, _, p)
        | Type::Record(_, p) | Type::Variant(_, p) => p,
    }
}

pub fn show(t: &Type) -> String {
    match norm(t) {
        Type::Var(v) => v.0.borrow().name.clone(),
        Type::Int(_) => "int".into(),
        Type::Bool(_) => "bool".into(),
        Type::Unit(_) => "unit".into(),
        Type::Fun(a, b, _) => format!("{} -> {}", show_atom(&a), show(&b)),
        Type::Record(r, _) => format!("{{{}}}", show_row(&r)),
        Type::Variant(r, _) => format!("[{}]", show_row(&r)),
    }
}

fn show_atom(t: &Type) -> String {
    match norm(t) {
        Type::Fun(..) => format!("({})", show(t)),
        other => show(&other),
    }
}

pub fn show_row(r: &Row) -> String {
    let mut fields = Vec::new();
    let mut cur = norm_row(r);
    loop {
        match cur {
            Row::Empty => break,
            Row::Extend { label, ty, rest } => {
                fields.push(format!("{}: {}", label, show(&ty)));
                cur = norm_row(&rest);
            }
            Row::Var(v) => {
                let name = v.0.borrow().name.clone();
                if fields.is_empty() {
                    return name;
                }
                return format!("{} | {}", fields.join(", "), name);
            }
        }
    }
    fields.join(", ")
}

/// 可序列化的纯类型描述，用于缓存与持久化。
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum SType {
    Var(String),
    Int,
    Bool,
    Unit,
    Fun(Box<SType>, Box<SType>),
    Record(SRow),
    Variant(SRow),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum SRow {
    Empty,
    Extend(String, SType, Box<SRow>),
    Var(String),
}

pub fn to_stype(t: &Type) -> SType {
    match norm(t) {
        Type::Var(v) => SType::Var(v.0.borrow().name.clone()),
        Type::Int(_) => SType::Int,
        Type::Bool(_) => SType::Bool,
        Type::Unit(_) => SType::Unit,
        Type::Fun(a, b, _) => SType::Fun(Box::new(to_stype(&a)), Box::new(to_stype(&b))),
        Type::Record(r, _) => SType::Record(to_srow(&r)),
        Type::Variant(r, _) => SType::Variant(to_srow(&r)),
    }
}

pub fn to_srow(r: &Row) -> SRow {
    match norm_row(r) {
        Row::Empty => SRow::Empty,
        Row::Extend { label, ty, rest } => {
            SRow::Extend(label, to_stype(&ty), Box::new(to_srow(&rest)))
        }
        Row::Var(v) => SRow::Var(v.0.borrow().name.clone()),
    }
}

/// 结构指纹：变量按出现顺序重命名，与具体变量名无关，
/// 因此变量改名 / 运行间变量编号漂移不会影响比较结果。
pub fn fingerprint(t: &Type) -> String {
    let st = to_stype(t);
    let mut tmap: HashMap<String, usize> = HashMap::new();
    let mut rmap: HashMap<String, usize> = HashMap::new();
    fp_type(&st, &mut tmap, &mut rmap)
}

fn fp_type(st: &SType, tmap: &mut HashMap<String, usize>, rmap: &mut HashMap<String, usize>) -> String {
    match st {
        SType::Var(n) => {
            let k = tmap.len();
            let id = *tmap.entry(n.clone()).or_insert(k);
            format!("'t{}", id)
        }
        SType::Int => "int".into(),
        SType::Bool => "bool".into(),
        SType::Unit => "unit".into(),
        SType::Fun(a, b) => format!("({} -> {})", fp_type(a, tmap, rmap), fp_type(b, tmap, rmap)),
        SType::Record(r) => format!("{{{}}}", fp_row(r, tmap, rmap)),
        SType::Variant(r) => format!("[{}]", fp_row(r, tmap, rmap)),
    }
}

fn fp_row(sr: &SRow, tmap: &mut HashMap<String, usize>, rmap: &mut HashMap<String, usize>) -> String {
    match sr {
        SRow::Empty => "".into(),
        SRow::Extend(l, t, rest) => {
            format!("{}: {}, {}", l, fp_type(t, tmap, rmap), fp_row(rest, tmap, rmap))
        }
        SRow::Var(n) => {
            let k = rmap.len();
            let id = *rmap.entry(n.clone()).or_insert(k);
            format!("'r{}", id)
        }
    }
}
