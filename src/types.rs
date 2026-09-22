//! 类型、行（row）与来源（provenance）表示，以及类型渲染。

use std::collections::BTreeSet;
use std::collections::HashMap;

pub type TyVarId = u32;

/// 来源信息：每个类型构造节点记录“由哪个表达式产生 / 涉及哪些约束”。
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Prov {
    pub node: Option<usize>,
    pub cons: BTreeSet<usize>,
}

impl Prov {
    pub fn at(node: usize) -> Self {
        Prov { node: Some(node), cons: BTreeSet::new() }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Var(TyVarId),
    Int(Prov),
    Bool(Prov),
    Fun(Prov, Box<Type>, Box<Type>),
    Record(Prov, Row),
    Variant(Prov, Row),
    /// 错误恢复用的占位类型，与任何类型都可统一。
    Error,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Row {
    Empty(Prov),
    Var(TyVarId),
    Extend(Prov, String, Box<Type>, Box<Row>),
}

#[derive(Clone, Debug)]
pub struct Scheme {
    pub ty_vars: Vec<TyVarId>,
    pub row_vars: Vec<TyVarId>,
    pub ty: Type,
}

impl Type {
    pub fn prov(&self) -> Option<&Prov> {
        match self {
            Type::Var(_) | Type::Error => None,
            Type::Int(p) | Type::Bool(p) | Type::Fun(p, _, _) | Type::Record(p, _) | Type::Variant(p, _) => Some(p),
        }
    }
}

impl Row {
    pub fn prov(&self) -> Option<&Prov> {
        match self {
            Row::Var(_) => None,
            Row::Empty(p) | Row::Extend(p, _, _, _) => Some(p),
        }
    }
}

/// 类型变量命名器：类型变量用希腊字母（跳过 ρ），行变量用 ρ 系列。
pub struct Namer {
    ty: HashMap<u32, String>,
    row: HashMap<u32, String>,
}

const GREEK: [&str; 23] = [
    "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο", "π", "σ", "τ",
    "υ", "φ", "χ", "ψ", "ω",
];

impl Namer {
    pub fn new() -> Self {
        Namer { ty: HashMap::new(), row: HashMap::new() }
    }
    pub fn ty_name(&mut self, v: u32) -> String {
        let n = self.ty.len();
        self.ty.entry(v).or_insert_with(|| {
            if n < GREEK.len() { GREEK[n].to_string() } else { format!("α{}", n - GREEK.len() + 1) }
        }).clone()
    }
    pub fn row_name(&mut self, v: u32) -> String {
        let n = self.row.len();
        self.row.entry(v).or_insert_with(|| if n == 0 { "ρ".to_string() } else { format!("ρ{}", n) }).clone()
    }
}

fn prov_str(p: &Prov, show: bool) -> String {
    if show && !p.cons.is_empty() {
        let ids: Vec<String> = p.cons.iter().map(|c| format!("#{}", c)).collect();
        format!("⟦{}⟧", ids.join(","))
    } else {
        String::new()
    }
}

pub fn render_type(t: &Type, n: &mut Namer, show_prov: bool) -> String {
    match t {
        Type::Var(v) => n.ty_name(*v),
        Type::Error => "⟨错误⟩".to_string(),
        Type::Int(p) => format!("Int{}", prov_str(p, show_prov)),
        Type::Bool(p) => format!("Bool{}", prov_str(p, show_prov)),
        Type::Fun(p, a, b) => {
            let sa = match **a {
                Type::Fun(..) => format!("({})", render_type(a, n, show_prov)),
                _ => render_type(a, n, show_prov),
            };
            format!("{} → {}{}", sa, render_type(b, n, show_prov), prov_str(p, show_prov))
        }
        Type::Record(p, r) => format!("{{{}}}{}", render_row_fields(r, n, show_prov), prov_str(p, show_prov)),
        Type::Variant(p, r) => format!("⟨{}⟩{}", render_row_fields(r, n, show_prov), prov_str(p, show_prov)),
    }
}

fn render_row_fields(r: &Row, n: &mut Namer, show_prov: bool) -> String {
    let mut fields = Vec::new();
    let mut cur = r;
    loop {
        match cur {
            Row::Extend(_, l, t, rest) => {
                fields.push(format!("{}: {}", l, render_type(t, n, show_prov)));
                cur = rest;
            }
            Row::Var(v) => {
                let name = n.row_name(*v);
                if fields.is_empty() {
                    return format!("… | {}", name);
                }
                return format!("{} | {}", fields.join(", "), name);
            }
            Row::Empty(_) => return fields.join(", "),
        }
    }
}

pub fn render_row(r: &Row, n: &mut Namer, show_prov: bool) -> String {
    format!("{{{}}}", render_row_fields(r, n, show_prov))
}

pub fn render_scheme(s: &Scheme, n: &mut Namer) -> String {
    if s.ty_vars.is_empty() && s.row_vars.is_empty() {
        render_type(&s.ty, n, false)
    } else {
        let mut qs = Vec::new();
        for v in &s.ty_vars { qs.push(n.ty_name(*v)); }
        for v in &s.row_vars { qs.push(n.row_name(*v)); }
        format!("∀{}. {}", qs.join(" "), render_type(&s.ty, n, false))
    }
}
