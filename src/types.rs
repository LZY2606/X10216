//! 类型的结构表示（求解前的 term），以及求解后的 resolved 类型。

use crate::constraint::Cid;
use serde::Serialize;
use std::collections::BTreeSet;

/// 参与一次类型冲突的约束集合（proof set）。
pub type Proof = BTreeSet<Cid>;

/// 行尾部：开放（含一个行变量）或封闭。
#[derive(Clone, PartialEq, Eq, Debug, Serialize)]
pub enum RowTail {
    Open(u32),
    Closed,
}

/// 行 = 若干字段（名字 → 类型，生成时无序）+ 尾部。
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Row {
    pub fields: Vec<(String, TypeTerm)>,
    pub tail: RowTail,
}

impl Row {
    pub fn closed(fields: Vec<(String, TypeTerm)>) -> Row {
        Row {
            fields,
            tail: RowTail::Closed,
        }
    }
    pub fn open(fields: Vec<(String, TypeTerm)>, tail: u32) -> Row {
        Row {
            fields,
            tail: RowTail::Open(tail),
        }
    }
    pub fn empty_closed() -> Row {
        Row {
            fields: vec![],
            tail: RowTail::Closed,
        }
    }
    pub fn is_open(&self) -> bool {
        matches!(self.tail, RowTail::Open(_))
    }
}

/// 求解器使用的类型项；变量是 union-find 的索引（生成阶段不使用）。
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypeTerm {
    TVar(u32),
    TCon(String), // int / bool / unit
    TFun(Box<TypeTerm>, Box<TypeTerm>),
    TRecord(Row),
    TVariant(Row),
}

impl TypeTerm {
    pub fn fun(p: TypeTerm, r: TypeTerm) -> TypeTerm {
        TypeTerm::TFun(Box::new(p), Box::new(r))
    }
    pub fn record(row: Row) -> TypeTerm {
        TypeTerm::TRecord(row)
    }
    pub fn variant(row: Row) -> TypeTerm {
        TypeTerm::TVariant(row)
    }
    pub fn walk_vars(&self, out: &mut Vec<u32>) {
        match self {
            TypeTerm::TVar(v) => out.push(*v),
            TypeTerm::TCon(_) => {}
            TypeTerm::TFun(a, b) => {
                a.walk_vars(out);
                b.walk_vars(out);
            }
            TypeTerm::TRecord(r) | TypeTerm::TVariant(r) => {
                for (_, t) in &r.fields {
                    t.walk_vars(out);
                }
                if let RowTail::Open(v) = r.tail {
                    out.push(v);
                }
            }
        }
    }
}

/// 泛化得到的类型模式：量化的普通变量与行变量分别编号。
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Scheme {
    /// 普通泛化变量（按首次出现顺序）。
    pub vars: Vec<u32>,
    /// 行尾泛化变量。
    pub row_vars: Vec<u32>,
    pub body: TypeTerm,
    /// 因 value restriction 无法泛化的变量（弱类型变量），仅用于展示。
    pub weak: Vec<u32>,
}

/// 求解后、给前端/错误使用的已解析类型。
#[derive(Clone, PartialEq, Eq, Debug, Serialize)]
#[serde(tag = "k", content = "v")]
pub enum RTy {
    Var(String),
    RowVar(String),
    Weak(String),
    Con(String),
    Fun(Box<RTy>, Box<RTy>),
    Record(Vec<(String, RTy)>, Option<String>),
    Variant(Vec<(String, RTy)>, Option<String>),
}

/// 类型到字符串的渲染（带行变量名）。
pub fn render(ty: &RTy) -> String {
    match ty {
        RTy::Var(n) | RTy::Weak(n) | RTy::RowVar(n) => n.clone(),
        RTy::Con(s) => s.clone(),
        RTy::Fun(a, b) => {
            let pa = if matches!(a.as_ref(), RTy::Fun(_, _)) {
                format!("({})", render(a))
            } else {
                render(a)
            };
            format!("{} -> {}", pa, render(b))
        }
        RTy::Record(fs, tail) => {
            let mut s = String::from("{ ");
            for (i, (f, t)) in fs.iter().enumerate() {
                if i > 0 {
                    s.push_str("; ");
                }
                s.push_str(f);
                s.push_str(" = ");
                s.push_str(&render(t));
            }
            if let Some(r) = tail {
                if !fs.is_empty() {
                    s.push_str("; ");
                }
                s.push_str("...");
                s.push_str(r);
            }
            s.push_str(" }");
            s
        }
        RTy::Variant(fs, tail) => {
            let mut s = String::from("[ ");
            for (i, (f, t)) in fs.iter().enumerate() {
                if i > 0 {
                    s.push_str(" | ");
                }
                s.push_str(f);
                s.push(' ');
                s.push_str(&render(t));
            }
            if let Some(r) = tail {
                if !fs.is_empty() {
                    s.push_str(" |");
                }
                s.push_str(" ...");
                s.push_str(r);
            }
            s.push_str(" ]");
            s
        }
    }
}

/// 一条约束的人类可读来源类型（用于步骤说明）。
#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
pub enum ConstraintKind {
    Con,
    Unify,
    Inst,
    Gen,
    Error,
}

pub fn kind_name(k: ConstraintKind) -> &'static str {
    match k {
        ConstraintKind::Con => "构造",
        ConstraintKind::Unify => "统一",
        ConstraintKind::Inst => "实例化",
        ConstraintKind::Gen => "泛化",
        ConstraintKind::Error => "错误",
    }
}

/// 占位：proof 合并工具。
pub fn merge_proof(a: &Proof, b: &Proof) -> Proof {
    let mut p = a.clone();
    p.extend(b.iter().cloned());
    p
}

/// 单条 proof。
pub fn one_proof(c: Cid) -> Proof {
    let mut p = BTreeSet::new();
    p.insert(c);
    p
}
