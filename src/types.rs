// 核心类型表示：HM 类型 + 行（record / variant），以及稳定身份工具。

use std::collections::BTreeMap;

// ---------- 稳定标识 ----------

pub const FNV_OFFSET: u64 = 0xcbf29ce484222325;
pub const FNV_PRIME: u64 = 0x00000100000001b3;

pub fn fnv1a(seed: u64, bytes: &[u8]) -> u64 {
    let mut h = seed;
    for &b in bytes {
        h ^= b as u64;
        h = h.wrapping_mul(FNV_PRIME);
    }
    h
}

pub fn fnv_str(seed: u64, s: &str) -> u64 {
    fnv1a(seed, s.as_bytes())
}

pub fn fnv_mix(seed: u64, parts: &[&str]) -> u64 {
    let mut h = seed;
    for p in parts {
        h = fnv_str(h, p);
        h = h.wrapping_add(0x9e3779b97f4a7c15);
    }
    h
}

// ---------- 变量 ----------

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct Var(pub u32);
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub struct RowVar(pub u32);

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Row {
    RVar(RowVar),
    /// 封闭行：字段按标签排序
    Closed(Vec<(String, Type)>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Var(Var),
    Unit,
    Bool,
    Arrow(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
    /// 仅供增量缓存回放使用，推断过程不会产生
    Skolem(String),
}

impl Type {
    pub fn arrow(a: Type, b: Type) -> Type {
        Type::Arrow(Box::new(a), Box::new(b))
    }

    /// 深度遍历，f 收到 (uid, is_row)，返回替换后的 uid；用于实例化复制。
    pub fn map_vars(&self, f: &mut impl FnMut(u32, bool) -> u32) -> Type {
        match self {
            Type::Var(Var(u)) => Type::Var(Var(f(*u, false))),
            Type::Unit | Type::Bool | Type::Skolem(_) => self.clone(),
            Type::Arrow(a, b) => Type::arrow(a.map_vars(f), b.map_vars(f)),
            Type::Record(r) => Type::Record(map_row(r, f)),
            Type::Variant(r) => Type::Variant(map_row(r, f)),
        }
    }

    /// 收集自由变量（未解析的根由 solver 在外部处理，这里只做结构遍历）。
    pub fn walk_vars(&self, out: &mut Vec<(u32, bool)>) {
        match self {
            Type::Var(Var(u)) => out.push((*u, false)),
            Type::Unit | Type::Bool | Type::Skolem(_) => {}
            Type::Arrow(a, b) => {
                a.walk_vars(out);
                b.walk_vars(out);
            }
            Type::Record(r) | Type::Variant(r) => collect_row(r, out),
        }
    }

    pub fn constructor(&self) -> &'static str {
        match self {
            Type::Var(_) => "var",
            Type::Unit => "unit",
            Type::Bool => "bool",
            Type::Arrow(_, _) => "arrow",
            Type::Record(_) => "record",
            Type::Variant(_) => "variant",
            Type::Skolem(_) => "skolem",
        }
    }
}

fn map_row(r: &Row, f: &mut impl FnMut(u32, bool) -> u32) -> Row {
    match r {
        Row::RVar(RowVar(u)) => Row::RVar(RowVar(f(*u, true))),
        Row::Closed(fs) => Row::Closed(
            fs.iter()
                .map(|(l, t)| (l.clone(), t.map_vars(f)))
                .collect(),
        ),
    }
}

fn collect_row(r: &Row, out: &mut Vec<(u32, bool)>) {
    match r {
        Row::RVar(RowVar(u)) => out.push((*u, true)),
        Row::Closed(fs) => {
            for (_, t) in fs {
                t.walk_vars(out);
            }
        }
    }
}

// ---------- 打印（带自动变量命名） ----------

fn greek(n: usize, base: char) -> String {
    // α β γ δ ε ... 简单 26 循环 + 下标
    let seq = ['α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ',
        'ν', 'ξ', 'ο', 'π', 'ρ', 'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω'];
    let (idx, sub) = (n % seq.len(), n / seq.len());
    let mut s = seq[idx].to_string();
    if sub > 0 {
        s.push_str(&sub.to_string());
    }
    let _ = base;
    s
}

pub struct Renamer {
    tv: BTreeMap<u32, String>,
    rv: BTreeMap<u32, String>,
}

impl Renamer {
    pub fn new() -> Self {
        Renamer {
            tv: BTreeMap::new(),
            rv: BTreeMap::new(),
        }
    }

    pub fn name_tv(&mut self, u: u32) -> String {
        if let Some(s) = self.tv.get(&u) {
            return s.clone();
        }
        let s = greek(self.tv.len(), 'α');
        self.tv.insert(u, s.clone());
        s
    }
    pub fn name_rv(&mut self, u: u32) -> String {
        if let Some(s) = self.rv.get(&u) {
            return s.clone();
        }
        let n = self.rv.len();
        let s = if n < 18 {
            ['ρ', 'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω', 'ρ', 'σ'][n.min(9)].to_string()
                + if n >= 8 { &(n - 8).to_string() } else { "" }
        } else {
            format!("ρ{}", n)
        };
        self.rv.insert(u, s.clone());
        s
    }

    pub fn render_type(&mut self, t: &Type) -> String {
        match t {
            Type::Var(Var(u)) => self.name_tv(*u),
            Type::Unit => "unit".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Skolem(s) => s.clone(),
            Type::Arrow(a, b) => {
                let a = self.render_type(a);
                let b = self.render_type(b);
                let a = if matches!(**a, Type::Arrow(_, _)) {
                    format!("({a})")
                } else {
                    a
                };
                format!("{a} → {b}")
            }
            Type::Record(r) => {
                let (fields, tail) = flatten_for_render(r);
                let mut parts: Vec<String> = fields
                    .iter()
                    .map(|(l, t)| format!("{l}: {}", self.render_type(t)))
                    .collect();
                if let Some(u) = tail {
                    parts.push(self.name_rv(u));
                }
                format!("{{ {} }}", parts.join(", "))
            }
            Type::Variant(r) => {
                let (fields, tail) = flatten_for_render(r);
                let mut parts: Vec<String> = fields
                    .iter()
                    .map(|(l, t)| match t {
                        Type::Unit => l.clone(),
                        _ => format!("{l}: {}", self.render_type(t)),
                    })
                    .collect();
                if let Some(u) = tail {
                    parts.push(self.name_rv(u));
                }
                format!("[ {} ]", parts.join(", "))
            }
        }
    }
}

/// 仅用于纯渲染：Closed 直接返回字段；RVar 交给 solver 解析后再调用。
fn flatten_for_render(r: &Row) -> (Vec<(String, Type)>, Option<u32>) {
    match r {
        Row::Closed(fs) => (fs.clone(), None),
        Row::RVar(RowVar(u)) => (vec![], Some(*u)),
    }
}

/// 规范化打印：变量按结构遍历顺序命名，便于跨运行比较等价性。
pub fn canonical_type_string(t: &Type) -> String {
    let mut rn = Renamer::new();
    rn.render_type(t)
}
