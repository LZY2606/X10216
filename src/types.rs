use std::collections::HashMap;

use serde::Serialize;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "t", content = "c", rename_all = "snake_case")]
pub enum Type {
    Var(u32),
    Int,
    Bool,
    Unit,
    Fun(Box<Type>, Box<Type>),
    Record(Vec<RowField>, RowTail),
    Variant(Vec<RowField>, RowTail),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct RowField {
    pub label: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RowTail {
    Closed,
    Open(u32),
}

impl Type {
    pub fn max_var(&self, acc: u32) -> u32 {
        match self {
            Type::Var(v) => acc.max(*v),
            Type::Int | Type::Bool | Type::Unit => acc,
            Type::Fun(a, b) => b.max_var(a.max_var(acc)),
            Type::Record(fs, tail) | Type::Variant(fs, tail) => {
                let mut m = acc;
                for f in fs {
                    m = f.ty.max_var(m);
                }
                if let RowTail::Open(v) = tail {
                    m = m.max(*v);
                }
                m
            }
        }
    }
    pub fn vars(&self, out: &mut Vec<u32>) {
        match self {
            Type::Var(v) => out.push(*v),
            Type::Int | Type::Bool | Type::Unit => {}
            Type::Fun(a, b) => {
                a.vars(out);
                b.vars(out);
            }
            Type::Record(fs, tail) | Type::Variant(fs, tail) => {
                for f in fs {
                    f.ty.vars(out);
                }
                if let RowTail::Open(v) = tail {
                    out.push(*v);
                }
            }
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Origin {
    pub label: String,
    pub node: Option<String>,
    pub constraint: String,
    pub kind: String,
}

pub struct UFEntry {
    pub rank: u32,
    pub root: Option<Type>,
    pub origins: Vec<Origin>,
}

pub struct UnionFind {
    pub entries: Vec<UFEntry>,
}

impl UnionFind {
    pub fn new() -> Self {
        UnionFind {
            entries: Vec::new(),
        }
    }

    pub fn fresh(&mut self, rank: u32, origin: Origin) -> Type {
        let id = self.entries.len() as u32;
        self.entries.push(UFEntry {
            rank,
            root: None,
            origins: vec![origin],
        });
        Type::Var(id)
    }

    pub fn find(&mut self, t: &Type) -> Type {
        match t {
            Type::Var(v) => match &self.entries[*v as usize].root {
                None => t.clone(),
                Some(parent) => {
                    let root = self.find(parent);
                    self.entries[*v as usize].root = Some(root.clone());
                    root
                }
            },
            _ => t.clone(),
        }
    }

    pub fn origins_of(&mut self, t: &Type) -> Vec<Origin> {
        match self.find(t) {
            Type::Var(v) => self.entries[v as usize].origins.clone(),
            _ => Vec::new(),
        }
    }

    /// Bind variable `v` (already a root) to non-variable/other root `to`.
    /// Performs the occurs check for bound structure inside `to`.
    pub fn bind(&mut self, v: u32, to: &Type, cid: &str) -> Result<(), u32> {
        let root = self.find(to);
        let mut acc = Vec::new();
        root.vars(&mut acc);
        if acc.contains(&v) {
            return Err(v);
        }
        let r1 = self.entries[v as usize].rank;
        let mut origins = std::mem::take(&mut self.entries[v as usize].origins);
        let r2 = match &root {
            Type::Var(w) => {
                let mut o2 = std::mem::take(&mut self.entries[*w as usize].origins);
                origins.append(&mut o2);
                self.entries[*w as usize].rank
            }
            _ => {
                origins.push(Origin {
                    label: format!("unified by {}", cid),
                    node: None,
                    constraint: cid.to_string(),
                    kind: "unify".into(),
                });
                r1
            }
        };
        self.entries[v as usize].rank = r1.min(r2);
        self.entries[v as usize].root = Some(root.clone());
        self.entries[v as usize].origins = origins;
        Ok(())
    }
}

// ---------- rendered type view with provenance ----------

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "t", rename_all = "snake_case")]
pub enum TypeView {
    Var {
        name: String,
        prov: Vec<String>,
        src: Vec<String>,
    },
    Int {
        prov: Vec<String>,
        src: Vec<String>,
    },
    Bool {
        prov: Vec<String>,
        src: Vec<String>,
    },
    Unit {
        prov: Vec<String>,
        src: Vec<String>,
    },
    Fun {
        arg: Box<TypeView>,
        result: Box<TypeView>,
        prov: Vec<String>,
        src: Vec<String>,
    },
    Record {
        fields: Vec<FieldView>,
        tail: String,
        prov: Vec<String>,
        src: Vec<String>,
    },
    Variant {
        fields: Vec<FieldView>,
        tail: String,
        prov: Vec<String>,
        src: Vec<String>,
    },
}

#[derive(Clone, Debug, Serialize)]
pub struct FieldView {
    pub label: String,
    pub ty: TypeView,
    pub prov: Vec<String>,
    pub src: Vec<String>,
}

fn dedup(v: Vec<String>) -> Vec<String> {
    let mut seen = std::collections::HashSet::new();
    v.into_iter().filter(|x| seen.insert(x.clone())).collect()
}

impl UnionFind {
    fn prov_of(&mut self, t: &Type) -> (Vec<String>, Vec<String>) {
        let os = self.origins_of(t);
        let prov = dedup(os.iter().map(|o| o.constraint.clone()).collect());
        let src = dedup(
            os.iter()
                .filter_map(|o| o.node.clone())
                .collect(),
        );
        (prov, src)
    }

    pub fn view(&mut self, t: &Type, names: &NameMap) -> TypeView {
        let r = self.find(t);
        let (prov, src) = self.prov_of(&r);
        match r {
            Type::Var(v) => TypeView::Var {
                name: names.name_of(v),
                prov,
                src,
            },
            Type::Int => TypeView::Int { prov, src },
            Type::Bool => TypeView::Bool { prov, src },
            Type::Unit => TypeView::Unit { prov, src },
            Type::Fun(a, b) => TypeView::Fun {
                arg: Box::new(self.view(&a, names)),
                result: Box::new(self.view(&b, names)),
                prov,
                src,
            },
            Type::Record(fs, tail) => {
                let fields = fs
                    .into_iter()
                    .map(|f| {
                        let (p, s) = self.prov_of(&f.ty);
                        FieldView {
                            label: f.label,
                            ty: self.view(&f.ty, names),
                            prov: p,
                            src: s,
                        }
                    })
                    .collect();
                TypeView::Record {
                    fields,
                    tail: tail_name(&tail, names),
                    prov,
                    src,
                }
            }
            Type::Variant(fs, tail) => {
                let fields = fs
                    .into_iter()
                    .map(|f| {
                        let (p, s) = self.prov_of(&f.ty);
                        FieldView {
                            label: f.label,
                            ty: self.view(&f.ty, names),
                            prov: p,
                            src: s,
                        }
                    })
                    .collect();
                TypeView::Variant {
                    fields,
                    tail: tail_name(&tail, names),
                    prov,
                    src,
                }
            }
        }
    }

    pub fn render(&mut self, t: &Type, names: &NameMap) -> String {
        render_view(&self.view(t, names))
    }
}

fn tail_name(tail: &RowTail, names: &NameMap) -> String {
    match tail {
        RowTail::Closed => String::new(),
        RowTail::Open(v) => names.name_of(*v),
    }
}

pub fn render_view(v: &TypeView) -> String {
    match v {
        TypeView::Var { name, .. } => name.clone(),
        TypeView::Int { .. } => "int".into(),
        TypeView::Bool { .. } => "bool".into(),
        TypeView::Unit { .. } => "unit".into(),
        TypeView::Fun { arg, result, .. } => {
            let a = render_view(arg);
            let r = render_view(result);
            let a = if matches!(**arg, TypeView::Fun { .. }) {
                format!("({})", a)
            } else {
                a
            };
            format!("{} -> {}", a, r)
        }
        TypeView::Record { fields, tail, .. } => {
            let mut parts: Vec<String> = fields.iter().map(|f| format!("{}: {}", f.label, render_view(&f.ty))).collect();
            if !tail.is_empty() {
                parts.push(format!("..{}", tail));
            }
            format!("{{{}}}", parts.join(", "))
        }
        TypeView::Variant { fields, tail, .. } => {
            let mut parts: Vec<String> = fields
                .iter()
                .map(|f| format!("{}({})", f.label, render_view(&f.ty)))
                .collect();
            if !tail.is_empty() {
                parts.push(format!("..{}", tail));
            }
            format!("[{}]", parts.join(", "))
        }
    }
}

/// Stable variable naming: origin-declared label, else generated a, b, ...
pub struct NameMap {
    named: HashMap<u32, String>,
    taken: std::collections::HashSet<String>,
}

impl NameMap {
    pub fn new() -> Self {
        NameMap {
            named: HashMap::new(),
            taken: std::collections::HashSet::new(),
        }
    }

    pub fn name_var(&mut self, v: u32, label: &str) {
        let label = sanitize(label);
        let mut name = label.clone();
        let mut k = 1;
        while self.taken.contains(&name) {
            name = format!("{}{}", label, k);
            k += 1;
        }
        self.taken.insert(name.clone());
        self.named.insert(v, name);
    }

    pub fn all_named(&self) -> Vec<(u32, String)> {
        self.named.iter().map(|(k, v)| (*k, v.clone())).collect()
    }

    pub fn name_of(&self, v: u32) -> String {
        if let Some(n) = self.named.get(&v) {
            return n.clone();
        }
        generic_name(v as usize)
    }
}

fn sanitize(s: &str) -> String {
    let mut out = String::new();
    for c in s.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            out.push(c);
        }
    }
    if out.is_empty() {
        out.push('t');
    }
    if out.chars().next().unwrap().is_ascii_digit() {
        out.insert(0, 't');
    }
    out
}

fn generic_name(i: usize) -> String {
    let n = i;
    let letter = (b'a' + (n % 26) as u8) as char;
    let cycle = n / 26;
    if cycle == 0 {
        letter.to_string()
    } else {
        format!("{}{}", letter, cycle)
    }
}
