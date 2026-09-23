//! 类型表示（union-find 节点 arena 中的内容）、类型方案与 pretty-printer。

use serde::Serialize;

/// arena 索引。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct TyId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Kind {
    /// 普通类型（可成为 int / bool / arrow / record / variant）。
    Type,
    /// 行（只能与行统一）。
    Row,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Content {
    /// 未解析的灵活变量；level 为词法层级，generic 标记泛化槽位。
    Flex { level: u32, generic: Option<u32> },
    Int,
    Bool,
    Arrow {
        params: Vec<TyId>,
        ret: TyId,
    },
    Row {
        kind: RowKind,
        fields: Vec<(String, TyId)>,
        /// 开放行的尾巴变量；闭合行为 None。
        tail: Option<TyId>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum RowKind {
    Record,
    Variant,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct TyNode {
    pub content: Content,
    pub kind: Kind,
}

impl Content {
    pub fn head_name(&self) -> &'static str {
        match self {
            Content::Flex { .. } => "flex",
            Content::Int => "int",
            Content::Bool => "bool",
            Content::Arrow { .. } => "->",
            Content::Row {
                kind: RowKind::Record,
                ..
            } => "record",
            Content::Row {
                kind: RowKind::Variant,
                ..
            } => "variant",
        }
    }
}

/// 泛化后的类型方案：root 与 scheme 节点都留在 arena 中，
/// `generics` 记录泛化槽位（槽位编号 -> 通用变量 TyId）。
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Scheme {
    pub root: TyId,
    pub generics: Vec<TyId>,
}

/// 可读的类型树（用于 JSON / 显示 / 等价性指纹）。
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "t", content = "d")]
pub enum TyView {
    Var { id: u32, generic: Option<u32> },
    Int,
    Bool,
    Arrow(Vec<TyView>, Box<TyView>),
    Record {
        fields: Vec<(String, TyView)>,
        tail: Option<Box<TyView>>,
    },
    Variant {
        options: Vec<(String, Option<TyView>)>,
        tail: Option<Box<TyView>>,
    },
}

impl TyView {
    pub fn render(&self) -> String {
        let mut s = String::new();
        self.write(&mut s, true);
        s
    }
    fn write(&self, s: &mut String, top: bool) {
        match self {
            TyView::Int => s.push_str("int"),
            TyView::Bool => s.push_str("bool"),
            TyView::Var { id, generic } => match generic {
                Some(g) => {
                    s.push('\'');
                    s.push_str(&gen_name(*g));
                    let _ = id;
                }
                None => s.push_str(&var_name(*id)),
            },
            TyView::Arrow(ps, r) => {
                if !top {
                    s.push('(');
                }
                for p in ps {
                    p.write(s, false);
                    s.push(' ');
                }
                s.push_str("-> ");
                r.write(s, top);
                if !top {
                    s.push(')');
                }
            }
            TyView::Record { fields, tail } => {
                s.push_str("{ ");
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push('`');
                    s.push_str(k);
                    s.push_str(": ");
                    v.write(s, true);
                }
                if let Some(t) = tail {
                    if !fields.is_empty() {
                        s.push_str(", ");
                    }
                    s.push_str("..");
                    t.write(s, true);
                }
                s.push_str(" }");
            }
            TyView::Variant { options, tail } => {
                s.push('[');
                for (i, (k, v)) in options.iter().enumerate() {
                    if i > 0 {
                        s.push_str(" | ");
                    }
                    s.push('`');
                    s.push_str(k);
                    if let Some(v) = v {
                        s.push('(');
                        v.write(s, true);
                        s.push(')');
                    }
                }
                if let Some(t) = tail {
                    if !options.is_empty() {
                        s.push_str(" | ");
                    }
                    s.push_str("..");
                    t.write(s, true);
                }
                s.push(']');
            }
        }
    }

    /// 忽略变量身份、只保留结构（未解析变量按同一通配符）的指纹。
    pub fn structural(&self) -> String {
        let mut s = String::new();
        self.struct_rec(&mut s, &mut 0u32, &mut std::collections::HashMap::new());
        s
    }
    fn struct_rec(
        &self,
        s: &mut String,
        fresh: &mut u32,
        canon: &mut std::collections::HashMap<u32, u32>,
    ) {
        match self {
            TyView::Int => s.push('I'),
            TyView::Bool => s.push('B'),
            TyView::Var { id, generic: _ } => {
                let k = canon.len() as u32;
                let c = *canon.entry(*id).or_insert(k);
                s.push('V');
                s.push_str(&c.to_string());
                s.push(';');
                let _ = fresh;
            }
            TyView::Arrow(ps, r) => {
                s.push_str("A(");
                for p in ps {
                    p.struct_rec(s, fresh, canon);
                }
                r.struct_rec(s, fresh, canon);
                s.push(')');
            }
            TyView::Record { fields, tail } => {
                s.push_str("R{");
                for (k, v) in fields {
                    s.push_str(k);
                    s.push('=');
                    v.struct_rec(s, fresh, canon);
                }
                if let Some(t) = tail {
                    s.push_str("+");
                    t.struct_rec(s, fresh, canon);
                }
                s.push('}');
            }
            TyView::Variant { options, tail } => {
                s.push('[');
                for (k, v) in options {
                    s.push_str(k);
                    if let Some(v) = v {
                        s.push('=');
                        v.struct_rec(s, fresh, canon);
                    }
                }
                if let Some(t) = tail {
                    s.push_str("+");
                    t.struct_rec(s, fresh, canon);
                }
                s.push(']');
            }
        }
    }
}

/// 通用变量按槽位命名 a..z, a1..。
pub fn gen_name(g: u32) -> String {
    let bytes = b"abcdefghijklmnopqrstuvwxyz";
    let i = (g as usize) % 26;
    let n = g / 26;
    if n == 0 {
        (bytes[i] as char).to_string()
    } else {
        format!("{}{}", bytes[i] as char, n)
    }
}

/// 未解析灵活变量命名 t0, t1...（仅显示用）。
pub fn var_name(id: u32) -> String {
    format!("t{}", id)
}
