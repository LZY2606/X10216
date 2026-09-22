//! 类型表示与 union-find 统一器：含 occurs-check、行多态的缺字段/多余字段区分。

use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt::Write;

/// 类型变量（按统一器内部编号）。
pub type Var = u32;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum T {
    Var(Var),
    Con(String, Vec<T>),
    Rec(Row),
    Variant(Row),
}

/// 行：字段表 + 可选尾变量。`closed=true` 表示行尾为空（记录封闭）。
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Row {
    pub fields: BTreeMap<String, T>,
    pub tail: Option<Var>,
}

impl Row {
    pub fn open(fields: BTreeMap<String, T>, tail: Var) -> Self { Row { fields, tail: Some(tail) } }
    pub fn closed(fields: BTreeMap<String, T>) -> Self { Row { fields, tail: None } }
    pub fn empty_closed() -> Self { Row { fields: BTreeMap::new(), tail: None } }
}

impl T {
    pub fn int() -> T { T::Con("Int".into(), vec![]) }
    pub fn bool() -> T { T::Con("Bool".into(), vec![]) }
    pub fn unit() -> T { T::Con("Unit".into(), vec![]) }
    pub fn arrow(a: T, b: T) -> T { T::Con("->".into(), vec![a, b]) }
    pub fn is_int(&self) -> bool { matches!(self, T::Con(n, v) if n == "Int" && v.is_empty()) }
}

pub fn var_name(v: Var) -> String {
    // t0, t1 ...；用字母混合以提高可读性
    let letter = (b'a' + (v % 26) as u8) as char;
    let cycle = v / 26;
    if cycle == 0 { format!("{letter}") } else { format!("{letter}{cycle}") }
}

/// 类型漂亮打印（默认传入已 resolve 的类型）。
pub fn pretty(t: &T) -> String {
    let mut s = String::new();
    pretty_into(t, &mut s, true);
    s
}

fn pretty_into(t: &T, s: &mut String, top: bool) {
    match t {
        T::Var(v) => write!(s, "{}", var_name(*v)).unwrap(),
        T::Con(n, args) if n == "->" && args.len() == 2 => {
            if !top { s.push('('); }
            pretty_into(&args[0], s, false);
            s.push_str(" -> ");
            pretty_into(&args[1], s, true);
            if !top { s.push(')'); }
        }
        T::Con(n, args) => {
            write!(s, "{n}").unwrap();
            if !args.is_empty() {
                s.push('(');
                for (i, a) in args.iter().enumerate() {
                    if i > 0 { s.push_str(", "); }
                    pretty_into(a, s, true);
                }
                s.push(')');
            }
        }
        T::Rec(row) => {
            s.push('{');
            for (i, (k, v)) in row.fields.iter().enumerate() {
                if i > 0 { s.push_str(", "); }
                write!(s, "{k}: ").unwrap();
                pretty_into(v, s, true);
            }
            match row.tail {
                Some(tv) => {
                    if !row.fields.is_empty() { s.push_str(", "); }
                    write!(s, "| {}", var_name(tv)).unwrap();
                }
                None => {}
            }
            s.push('}');
        }
        T::Variant(row) => {
            s.push('[');
            for (i, (k, v)) in row.fields.iter().enumerate() {
                if i > 0 { s.push_str(" | "); }
                match v {
                    T::Con(n, _) if n == "Unit" => write!(s, "{k}").unwrap(),
                    other => { write!(s, "{k} ").unwrap(); pretty_into(other, s, true); }
                }
            }
            if let Some(tv) = row.tail {
                if !row.fields.is_empty() { s.push_str(" | "); }
                write!(s, "{}", var_name(tv)).unwrap();
            }
            s.push(']');
        }
    }
}

// ---------------- Union-Find ----------------

#[derive(Clone, Debug)]
enum Node {
    Root(RootData),
    Link(Var),
}

#[derive(Clone, Debug)]
struct RootData {
    rank: u32,
    t: Option<T>, // 根绑定到的结构（不含未 resolve 的 T::Var 指向其他节点）
    rank_level: u32, // 泛化层级（创建时的 level）
}

pub struct Unifier {
    nodes: RefCell<Vec<Node>>,
}

impl Default for Unifier { fn default() -> Self { Unifier { nodes: RefCell::new(Vec::new()) } } }

impl Unifier {
    pub fn new() -> Self { Self::default() }

    pub fn fresh(&self, level: u32) -> T {
        let mut n = self.nodes.borrow_mut();
        let v = n.len() as Var;
        n.push(Node::Root(RootData { rank: 0, t: None, rank_level: level }));
        T::Var(v)
    }

    pub fn level(&self, v: Var) -> u32 {
        match self.find_data(v) { Some(d) => d.rank_level, None => 0 }
    }

    fn find_data(&self, mut v: Var) -> Option<RootData> {
        loop {
            let node = self.nodes.borrow().get(v as usize).cloned();
            match node {
                Some(Node::Root(d)) => return Some(d),
                Some(Node::Link(p)) => v = p,
                None => return None,
            }
        }
    }

    pub fn resolve(&self, t: &T) -> T {
        match t {
            T::Var(v) => match self.find_root(*v) {
                Some(root) => match &root.t {
                    Some(bound) => self.resolve(bound),
                    None => T::Var(root_v(&self.nodes.borrow(), *v)),
                },
                None => T::Var(*v),
            },
            T::Con(n, args) => T::Con(n.clone(), args.iter().map(|a| self.resolve(a)).collect()),
            T::Rec(r) => T::Rec(Row { fields: r.fields.iter().map(|(k, v)| (k.clone(), self.resolve(v))).collect(), tail: self.resolve_tail(r.tail) }),
            T::Variant(r) => T::Variant(Row { fields: r.fields.iter().map(|(k, v)| (k.clone(), self.resolve(v))).collect(), tail: self.resolve_tail(r.tail) }),
        }
    }

    fn resolve_tail(&self, tail: Option<Var>) -> Option<Var> {
        tail.and_then(|v| {
            let root = self.find_root(v)?;
            match root.t {
                Some(_) => None, // 理论上行变量只与行变量合一，结构绑定不会发生
                None => Some(root_v(&self.nodes.borrow(), v)),
            }
        })
    }

    /// 查找根，并做路径压缩。返回根上的数据。
    fn find_root(&self, v: Var) -> Option<RootData> {
        let root = {
            let n = self.nodes.borrow();
            let mut cur = v;
            loop {
                match n.get(cur as usize) {
                    Some(Node::Root(_)) => break cur,
                    Some(Node::Link(p)) => cur = *p,
                    None => return None,
                }
            }
        };
        // 路径压缩
        let mut n = self.nodes.borrow_mut();
        let mut cur = v;
        while let Some(Node::Link(p)) = n.get(cur as usize).cloned() {
            if let Node::Link(slot) = n.get_mut(cur as usize).unwrap() {
                *slot = root;
            }
            cur = p;
            if cur == root { break; }
        }
        if let Some(Node::Root(d)) = n.get(root as usize) { Some(d.clone()) } else { None }
    }

    /// 两个类型变量是否当前为同一等价类。
    pub fn same(&self, a: Var, b: Var) -> bool {
        let ra = self.find_root(a).map(|_| root_of(self, a));
        let rb = self.find_root(b).map(|_| root_of(self, b));
        ra.is_some() && ra == rb
    }

    pub fn unify(&self, a: &T, b: &T) -> Result<(), UnifyError> {
        let a = self.resolve(a);
        let b = self.resolve(b);
        match (&a, &b) {
            (T::Var(x), T::Var(y)) if x == y => Ok(()),
            (T::Var(x), other) | (other, T::Var(x)) => {
                if occurs(&self, *x, other) {
                    return Err(UnifyError::InfiniteType(T::Var(*x), other.clone()));
                }
                self.bind_var(*x, other.clone());
                Ok(())
            }
            (T::Con(n1, a1), T::Con(n2, a2)) if n1 == n2 && a1.len() == a2.len() => {
                for (x, y) in a1.iter().zip(a2.iter()) { self.unify(x, y)?; }
                Ok(())
            }
            (T::Rec(r1), T::Rec(r2)) => self.unify_rows(r1, r2, RowKind::Record),
            (T::Variant(r1), T::Variant(r2)) => self.unify_rows(r1, r2, RowKind::Variant),
            _ => Err(mismatch(&a, &b)),
        }
    }

    fn bind_var(&self, v: Var, t: T) {
        // v 是根；把它指向 t。简单做法：根节点写入结构，并让 t 中的变量保持。
        let mut n = self.nodes.borrow_mut();
        if let Some(Node::Root(d)) = n.get_mut(v as usize) {
            d.t = Some(t);
            d.rank = d.rank.max(1);
        }
    }

    fn unify_rows(&self, r1: &Row, r2: &Row, kind: RowKind) -> Result<(), UnifyError> {
        // 行合一（A. Pottier 风格）：公共字段递归合一；只在一侧的字段尝试用另一侧的尾变量吸收；
        // 若两侧都封闭（尾已尽）仍有剩余 => 缺字段 / 多余字段错误（按方向区分）。
        let mut left_only: Vec<(String, T)> = Vec::new();
        let mut right_only: Vec<(String, T)> = Vec::new();
        for (k, v) in &r1.fields {
            match r2.fields.get(k) {
                Some(v2) => self.unify(v, v2)?,
                None => left_only.push((k.clone(), v.clone())),
            }
        }
        for (k, v) in &r2.fields {
            if !r1.fields.contains_key(k) { right_only.push((k.clone(), v.clone())); }
        }
        let ltail = r1.tail;
        let rtail = r2.tail;
        match (ltail, rtail) {
            (None, None) => {
                if left_only.is_empty() && right_only.is_empty() { return Ok(()); }
                Err(row_mismatch(kind, &left_only, &right_only, true))
            }
            (Some(lv), Some(rv)) => {
                // 两个开放行：差异字段必须能被对侧尾吸收——因为尾是多态变量，
                // 经典 ML 行多态下不能让带公共缺字段的两行直接统一，除非把差异
                // 吸收到新尾变量。我们生成三个新的“残差”行变量：
                //   left  = left_only ++ common_tail
                //   right = right_only ++ common_tail
                // 即 lv <| left_only..m ; rv <| right_only..m （m 为共享新尾）
                // 用“尾实例化”表达：将 lv 绑定为行 (left_only, tail=m)，
                // rv 绑定为 (right_only, tail=m)。
                // 但 lv/rv 是普通类型变量（可能被别处引用），因此这是标准的
                // 行变量吸收；occurs 检查依然成立。
                let m: Var = self.fresh(self.level(lv).max(self.level(rv))).as_var();
                self.absorb(lv, left_only, m)?;
                self.absorb(rv, right_only, m)?;
                Ok(())
            }
            (Some(lv), None) => {
                // 右侧封闭：左尾必须恰好吸收 right_only？方向：
                // 左行是“实际”，右行是“期望”（调用方向由约束侧给出，但行合一对称）。
                // 右封闭意味着右行没有更多字段；左行多出的 left_only 无处可去 => 多余字段。
                // 右行独有的 right_only 必须由左尾吸收 => 左尾补全（可行）。
                if !left_only.is_empty() {
                    return Err(row_mismatch(kind, &left_only, &right_only, false));
                }
                self.absorb(lv, right_only, closed_tail())?;
                Ok(())
            }
            (None, Some(rv)) => {
                if !right_only.is_empty() {
                    return Err(row_mismatch(kind, &left_only, &right_only, false));
                }
                self.absorb(rv, left_only, closed_tail())?;
                Ok(())
            }
        }
    }

    /// 把行尾变量 `v` 实例化为 `fields ++ tail`。
    /// v 必须为无根结构的变量；吸收要求 fields 不包含 v（occurs）。
    fn absorb(&self, v: Var, fields: Vec<(String, T)>, tail: Tail) -> Result<(), UnifyError> {
        let map: BTreeMap<String, T> = fields.into_iter().collect();
        for (_, ft) in &map {
            if occurs(self, v, ft) {
                return Err(UnifyError::InfiniteType(T::Var(v), ft.clone()));
            }
        }
        let row = Row { fields: map, tail: tail.0 };
        // 按该尾变量是记录尾还是变体尾，由调用处决定不可知：两种结构都可绑定，
        // 但后续 resolve 会通过外层 T::Rec/Variant 访问；单独绑定 T::Rec 即可，
        // 变体侧由其对称调用保证。为区分，记录与变体使用各自的 absorb 调用点。
        self.bind_var(v, T::Rec(row));
        Ok(())
    }

    pub fn absorb_record(&self, v: Var, fields: Vec<(String, T)>, tail: Tail) -> Result<(), UnifyError> {
        self.absorb(v, fields, tail)
    }
    pub fn absorb_variant(&self, v: Var, fields: Vec<(String, T)>, tail: Tail) -> Result<(), UnifyError> {
        // 与 absorb 相同，但绑定为 Variant 行
        let map: BTreeMap<String, T> = fields.into_iter().collect();
        for (_, ft) in &map {
            if occurs(self, v, ft) { return Err(UnifyError::InfiniteType(T::Var(v), ft.clone())); }
        }
        self.bind_var(v, T::Variant(Row { fields: map, tail: tail.0 }));
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct Tail(pub Option<Var>);
pub fn closed_tail() -> Tail { Tail(None) }
pub fn open_tail(v: Var) -> Tail { Tail(Some(v)) }
impl T { pub fn as_var(&self) -> Var { if let T::Var(v) = self { *v } else { panic!("not var") } } }

fn root_of(u: &Unifier, v: Var) -> Var {
    let n = u.nodes.borrow();
    let mut cur = v;
    loop {
        match n.get(cur as usize) {
            Some(Node::Root(_)) => return cur,
            Some(Node::Link(p)) => cur = *p,
            None => return cur,
        }
    }
}

fn root_v(_n: &std::cell::Ref<Vec<Node>>, v: Var) -> Var { v }

fn occurs(u: &Unifier, v: Var, t: &T) -> bool {
    match t {
        T::Var(x) => u.same(v, *x),
        T::Con(_, args) => args.iter().any(|a| occurs(u, v, &u.resolve(a))),
        T::Rec(r) | T::Variant(r) => {
            r.fields.values().any(|f| occurs(u, v, &u.resolve(f)))
                || r.tail.map(|t| u.same(v, t)).unwrap_or(false)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowKind { Record, Variant }

#[derive(Clone, Debug)]
pub enum UnifyError {
    Mismatch { expected: T, got: T },
    InfiniteType(T, T),
    RowField {
        kind: RowKind,
        /// 期望侧（约束右方）缺失的字段名（实际侧有）
        extra: Vec<String>,
        /// 实际侧缺失的字段名（期望侧有）
        missing: Vec<String>,
        /// 两侧均封闭却有差异
        both_closed: bool,
    },
}

fn mismatch(a: &T, b: &T) -> UnifyError {
    UnifyError::Mismatch { expected: a.clone(), got: b.clone() }
}
fn row_mismatch(kind: RowKind, left_only: &[(String, T)], right_only: &[(String, T)], both_closed: bool) -> UnifyError {
    // 以“左为实际、右为期望”的约定：left_only = 多出(extra)，right_only = 缺失(missing)
    UnifyError::RowField {
        kind,
        extra: left_only.iter().map(|(k, _)| k.clone()).collect(),
        missing: right_only.iter().map(|(k, _)| k.clone()).collect(),
        both_closed,
    }
}

// ---------------- 泛化 / 实例化（层级） ----------------

/// 返回类型中 level > current 的未链接变量（泛化变量），按出现去重排序。
pub fn free_vars_above(u: &Unifier, t: &T, current: u32) -> Vec<Var> {
    let mut seen = std::collections::BTreeSet::new();
    collect(u, t, current, &mut seen);
    seen.into_iter().collect()
}

fn collect(u: &Unifier, t: &T, current: u32, out: &mut std::collections::BTreeSet<Var>) {
    match u.resolve(t) {
        T::Var(v) => {
            let lvl = u.level(v);
            if lvl > current { out.insert(v); }
        }
        T::Con(_, args) => for a in args { collect(u, &a, current, out); },
        T::Rec(r) | T::Variant(r) => {
            for (_, f) in &r.fields { collect(u, f, current, out); }
            if let Some(tv) = r.tail {
                if u.level(tv) > current { out.insert(tv); }
            }
        }
    }
}
