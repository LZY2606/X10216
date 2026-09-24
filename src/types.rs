//! 内部类型与类型表（union-find）。
//!
//! 类型变量可变绑定（None 表示未绑定）。行是字段表 + 行尾：
//! `tail = None` 表示闭合行（构造点），`Some(r)` 表示开放行，可被补全。

use std::cell::Cell;
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Var(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type_ {
    TVar(Var),
    TInt,
    TBool,
    TStr,
    TFun(u32, u32),
    /// 字段按名排序；tail 为行尾变量（无即闭合）
    Record(Vec<(String, u32)>, Option<Var>),
    Variant(Vec<(String, Option<u32>)>, Option<Var>),
}

#[derive(Clone, Debug)]
pub struct TypeNode {
    pub value: Type_,
    pub origin: VarOrigin,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VarOrigin {
    /// 由某表达式节点产生（推断变量）
    Node { node_id_id: u32 },
    /// 泛化量词
    Generalized(u32),
    /// 行补全时构造出的行变量
    RowTail(u32),
}

pub struct TypeStore {
    pub nodes: Vec<TypeNode>,
    pub rank: Vec<u32>,
    pub bind: Vec<Option<u32>>,
    gen_counter: Cell<u32>,
}

impl Default for TypeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeStore {
    pub fn new() -> Self {
        TypeStore { nodes: Vec::new(), rank: Vec::new(), bind: Vec::new(), gen_counter: Cell::new(0) }
    }

    pub fn fresh(&mut self, origin: VarOrigin) -> u32 {
        let id = self.nodes.len() as u32;
        self.nodes.push(TypeNode { value: Type_::TVar(Var(id)), origin });
        self.rank.push(0);
        self.bind.push(None);
        id
    }

    pub fn fresh_gen(&mut self) -> u32 {
        let n = self.gen_counter.get();
        self.gen_counter.set(n + 1);
        self.fresh(VarOrigin::Generalized(n))
    }

    pub fn make(&mut self, value: Type_, origin: VarOrigin) -> u32 {
        let id = self.nodes.len() as u32;
        self.nodes.push(TypeNode { value, origin });
        self.rank.push(0);
        self.bind.push(None);
        id
    }

    pub fn fresh_row_tail(&mut self) -> (Option<Var>, u32) {
        let n = self.gen_counter.get();
        self.gen_counter.set(n + 1);
        let v = self.fresh(VarOrigin::RowTail(n));
        (Some(Var(v)), v)
    }

    /// 沿绑定链找到代表。
    pub fn find(&mut self, t: u32) -> u32 {
        let mut cur = t;
        loop {
            match self.bind[cur as usize] {
                Some(p) => {
                    let root = self.find(p);
                    self.bind[cur as usize] = Some(root);
                    cur = root;
                }
                None => return cur,
            }
        }
    }

    pub fn rep_value(&mut self, t: u32) -> Type_ {
        let r = self.find(t);
        self.nodes[r as usize].value.clone()
    }

    pub fn origin(&self, t: u32) -> VarOrigin {
        self.nodes[t as usize].origin
    }

    /// 判定 v 是否出现在 t 中（occurs check），返回出现处描述。
    pub fn occurs(&mut self, v: u32, t: u32) -> bool {
        let r = self.find(t);
        if r == v {
            return true;
        }
        match self.nodes[r as usize].value.clone() {
            Type_::TVar(_) => false,
            Type_::TInt | Type_::TBool | Type_::TStr => false,
            Type_::TFun(a, b) => self.occurs(v, a) || self.occurs(v, b),
            Type_::Record(fields, tail) => {
                fields.iter().any(|(_, ft)| self.occurs(v, *ft))
                    || tail.map(|Var(rv)| self.occurs(v, rv)).unwrap_or(false)
            }
            Type_::Variant(tags, tail) => {
                tags.iter().any(|(_, pt)| pt.map(|pt| self.occurs(v, pt)).unwrap_or(false))
                    || tail.map(|Var(rv)| self.occurs(v, rv)).unwrap_or(false)
            }
        }
    }

    pub fn record_fields_sorted(fields: Vec<(String, u32)>) -> Vec<(String, u32)> {
        let mut m: BTreeMap<String, u32> = BTreeMap::new();
        for (k, v) in fields {
            m.insert(k, v);
        }
        m.into_iter().collect()
    }
}

/// 解包后的类型视图（已 follow）。
#[derive(Clone, Debug)]
pub enum View {
    Var(u32),
    Int,
    Bool,
    Str,
    Fun(u32, u32),
    Record(Vec<(String, u32)>, Option<Var>),
    Variant(Vec<(String, Option<u32>)>, Option<Var>),
}

impl TypeStore {
    pub fn view(&mut self, t: u32) -> View {
        let r = self.find(t);
        match self.nodes[r as usize].value.clone() {
            Type_::TVar(_) => View::Var(r),
            Type_::TInt => View::Int,
            Type_::TBool => View::Bool,
            Type_::TStr => View::Str,
            Type_::TFun(a, b) => View::Fun(a, b),
            Type_::Record(f, tail) => View::Record(f, tail),
            Type_::Variant(tags, tail) => View::Variant(tags, tail),
        }
    }
}
