use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, PartialOrd, Ord)]
pub struct TV(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, PartialOrd, Ord)]
pub struct RV(pub u32);

/// 推断期类型：record/variant 的行一律用行 id 索引（描述符在求解器中）。
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum Ty {
    Var(TV),
    Int,
    Str,
    Bool,
    Unit,
    Con(String, Box<Ty>),
    Rec(RV),
    Variant(RV),
    Arrow(Box<Ty>, Box<Ty>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum QKind {
    T,
    R,
}

/// 泛化后的只读类型。QVar 为绑定器编号；FreeT/FreeR 为外层共享的未泛化变量。
#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum STy {
    QVar(u32),
    FreeT(u32),
    Int,
    Str,
    Bool,
    Unit,
    Con(String, Box<STy>),
    Rec(SRow),
    Variant(SRow),
    Arrow(Box<STy>, Box<STy>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize)]
pub enum SRow {
    RQVar(u32),
    FreeR(u32),
    Closed,
    Row(Vec<(String, STy)>, Box<SRow>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize)]
pub struct Scheme {
    pub quants: Vec<QKind>,
    pub body: STy,
    pub blocked_vr: bool,
}

fn qname(kind: QKind, kind_idx: u32) -> String {
    let base = match kind {
        QKind::T => 'a',
        QKind::R => 'r',
    };
    if kind_idx < 26 {
        format!("{}{}", base, (b'a' + kind_idx as u8) as char)
    } else {
        format!("{}{}'{}", base, (b'a' + (kind_idx % 26) as u8) as char, kind_idx / 26)
    }
}

pub fn render_scheme(sch: &Scheme) -> String {
    let mut t_count = 0u32;
    let mut r_count = 0u32;
    let mut names: Vec<String> = Vec::with_capacity(sch.quants.len());
    for k in &sch.quants {
        let n = match k {
            QKind::T => {
                let n = qname(*k, t_count);
                t_count += 1;
                n
            }
            QKind::R => {
                let n = qname(*k, r_count);
                r_count += 1;
                n
            }
        };
        names.push(n);
    }
    let body = render_sty(&sch.body, &names);
    if names.is_empty() {
        body
    } else {
        format!("∀{}. {}", names.join(" "), body)
    }
}

fn render_sty(t: &STy, names: &[String]) -> String {
    match t {
        STy::QVar(i) => names[*i as usize].clone(),
        STy::FreeT(i) => format!("?t{}", i),
        STy::Int => "Int".into(),
        STy::Str => "String".into(),
        STy::Bool => "Bool".into(),
        STy::Unit => "Unit".into(),
        STy::Con(tag, inner) => format!("{} {}", tag, render_atom_s(inner, names)),
        STy::Arrow(a, b) => format!("{} -> {}", render_sty_left(a, names), render_sty(b, names)),
        STy::Rec(r) => format!("{{ {}}}", render_srow(r, names, false)),
        STy::Variant(r) => format!("[ {}]", render_srow(r, names, true)),
    }
}

fn render_sty_left(t: &STy, names: &[String]) -> String {
    match t {
        STy::Arrow(_, _) => format!("({})", render_sty(t, names)),
        _ => render_sty(t, names),
    }
}

fn render_atom_s(t: &STy, names: &[String]) -> String {
    match t {
        STy::Arrow(_, _) | STy::Con(_, _) => format!("({})", render_sty(t, names)),
        _ => render_sty(t, names),
    }
}

fn render_srow(r: &SRow, names: &[String], variant: bool) -> String {
    let mut parts: Vec<String> = Vec::new();
    let mut cur = r;
    loop {
        match cur {
            SRow::Closed => return parts.join(", "),
            SRow::RQVar(i) => {
                parts.push(names[*i as usize].clone());
                return parts.join(", ");
            }
            SRow::FreeR(i) => {
                parts.push(format!("?r{}", i));
                return parts.join(", ");
            }
            SRow::Row(fields, tail) => {
                for (l, ty) in fields {
                    if variant {
                        parts.push(format!("{} {}", l, render_atom_s(ty, names)));
                    } else {
                        parts.push(format!("{}: {}", l, render_sty(ty, names)));
                    }
                }
                cur = tail;
            }
        }
    }
}
