//! 类型表示、替换、occurs-check、行 (row) 统一与类型打印。

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub type VarId = u32;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(VarId),
    Int,
    Bool,
    Str,
    Unit,
    Fun(Rc<Type>, Rc<Type>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Row {
    Empty,
    Var(VarId),
    Extend(Rc<str>, Rc<Type>, Rc<Row>),
}

#[derive(Clone, Debug)]
pub enum UErr {
    Mismatch(Type, Type),
    OccursT(VarId, Type),
    OccursR(VarId, Row),
    /// missing: 右侧要求而左侧缺失的字段；extra: 左侧多出而右侧没有的字段
    Row { missing: Vec<String>, extra: Vec<String> },
}

#[derive(Clone, Debug, Default)]
pub struct Solver {
    tsub: Vec<Option<Type>>,
    rsub: Vec<Option<Row>>,
}

impl Solver {
    pub fn new() -> Self { Self::default() }

    pub fn with_capacity(t: usize, r: usize) -> Self {
        Solver { tsub: vec![None; t], rsub: vec![None; r] }
    }

    pub fn sizes(&self) -> (usize, usize) { (self.tsub.len(), self.rsub.len()) }

    pub fn set_sizes(&mut self, t: usize, r: usize) {
        self.tsub.resize(t, None);
        self.rsub.resize(r, None);
    }

    pub fn new_tvar(&mut self) -> Type {
        self.tsub.push(None);
        Type::Var((self.tsub.len() - 1) as VarId)
    }

    pub fn new_rvar(&mut self) -> Row {
        self.rsub.push(None);
        Row::Var((self.rsub.len() - 1) as VarId)
    }

    pub fn zonk(&self, t: &Type) -> Type {
        match t {
            Type::Var(v) => match self.tsub.get(*v as usize).and_then(|x| x.as_ref()) {
                Some(t2) => self.zonk(t2),
                None => t.clone(),
            },
            Type::Fun(a, b) => Type::Fun(Rc::new(self.zonk(a)), Rc::new(self.zonk(b))),
            Type::Record(r) => Type::Record(self.zonk_row(r)),
            Type::Variant(r) => Type::Variant(self.zonk_row(r)),
            _ => t.clone(),
        }
    }

    pub fn zonk_row(&self, r: &Row) -> Row {
        match r {
            Row::Var(v) => match self.rsub.get(*v as usize).and_then(|x| x.as_ref()) {
                Some(r2) => self.zonk_row(r2),
                None => r.clone(),
            },
            Row::Extend(l, t, rest) => Row::Extend(l.clone(), Rc::new(self.zonk(t)), Rc::new(self.zonk_row(rest))),
            Row::Empty => Row::Empty,
        }
    }

    fn occurs_t(&self, v: VarId, t: &Type) -> bool {
        match t {
            Type::Var(x) => *x == v,
            Type::Fun(a, b) => self.occurs_t(v, a) || self.occurs_t(v, b),
            Type::Record(r) | Type::Variant(r) => self.occurs_t_row(v, r),
            _ => false,
        }
    }

    fn occurs_t_row(&self, v: VarId, r: &Row) -> bool {
        match r {
            Row::Extend(_, t, rest) => self.occurs_t(v, t) || self.occurs_t_row(v, rest),
            _ => false,
        }
    }

    fn occurs_r(&self, v: VarId, r: &Row) -> bool {
        match r {
            Row::Var(x) => *x == v,
            Row::Extend(_, t, rest) => self.occurs_r_type(v, t) || self.occurs_r(v, rest),
            Row::Empty => false,
        }
    }

    fn occurs_r_type(&self, v: VarId, t: &Type) -> bool {
        match t {
            Type::Fun(a, b) => self.occurs_r_type(v, a) || self.occurs_r_type(v, b),
            Type::Record(r) | Type::Variant(r) => self.occurs_r(v, r),
            _ => false,
        }
    }

    fn bind_tvar(&mut self, v: VarId, t: &Type) -> Result<(), UErr> {
        if *t == Type::Var(v) { return Ok(()); }
        if self.occurs_t(v, t) { return Err(UErr::OccursT(v, t.clone())); }
        self.tsub[v as usize] = Some(t.clone());
        Ok(())
    }

    fn bind_rvar(&mut self, v: VarId, r: &Row) -> Result<(), UErr> {
        if *r == Row::Var(v) { return Ok(()); }
        if self.occurs_r(v, r) { return Err(UErr::OccursR(v, r.clone())); }
        self.rsub[v as usize] = Some(r.clone());
        Ok(())
    }

    pub fn unify(&mut self, a: &Type, b: &Type) -> Result<(), UErr> {
        let a = self.zonk(a);
        let b = self.zonk(b);
        match (&a, &b) {
            (Type::Var(x), _) => self.bind_tvar(*x, &b),
            (_, Type::Var(y)) => self.bind_tvar(*y, &a),
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool)
            | (Type::Str, Type::Str) | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Fun(a1, b1), Type::Fun(a2, b2)) => {
                self.unify(a1, a2)?;
                self.unify(b1, b2)
            }
            (Type::Record(r1), Type::Record(r2)) => self.unify_row(r1, r2),
            (Type::Variant(r1), Type::Variant(r2)) => self.unify_row(r1, r2),
            _ => Err(UErr::Mismatch(a, b)),
        }
    }

    fn row_labels(&self, r: &Row) -> (Vec<String>, bool) {
        let mut labels = Vec::new();
        let mut cur = self.zonk_row(r);
        loop {
            match cur {
                Row::Extend(l, _, rest) => { labels.push(l.to_string()); cur = self.zonk_row(&rest); }
                Row::Empty => return (labels, true),
                Row::Var(_) => return (labels, false),
            }
        }
    }

    fn row_err(&self, a: &Row, b: &Row) -> UErr {
        let (la, _) = self.row_labels(a);
        let (lb, _) = self.row_labels(b);
        let sa: HashSet<&String> = la.iter().collect();
        let sb: HashSet<&String> = lb.iter().collect();
        let missing = lb.iter().filter(|l| !sa.contains(l)).cloned().collect();
        let extra = la.iter().filter(|l| !sb.contains(l)).cloned().collect();
        UErr::Row { missing, extra }
    }

    /// 在行 r 中查找标签 label，必要时延伸行变量；返回 (字段类型, 剩余行)
    fn rewrite(&mut self, r: &Row, label: &str) -> Option<(Type, Row)> {
        match self.zonk_row(r) {
            Row::Empty => None,
            Row::Var(v) => {
                let t = self.new_tvar();
                let rest = self.new_rvar();
                self.rsub[v as usize] = Some(Row::Extend(Rc::from(label), Rc::new(t.clone()), Rc::new(rest.clone())));
                Some((t, rest))
            }
            Row::Extend(l, t, rest) => {
                if &*l == label { Some(((*t).clone(), (*rest).clone())) } else {
                    let (t2, rest2) = self.rewrite(&rest, label)?;
                    Some((t2, Row::Extend(l, t, Rc::new(rest2))))
                }
            }
        }
    }

    pub fn unify_row(&mut self, a: &Row, b: &Row) -> Result<(), UErr> {
        let a = self.zonk_row(a);
        let b = self.zonk_row(&b.clone());
        match (&a, &b) {
            (Row::Empty, Row::Empty) => Ok(()),
            (Row::Var(x), r) => self.bind_rvar(*x, r),
            (r, Row::Var(y)) => self.bind_rvar(*y, r),
            (Row::Extend(l, t, rest), other) => {
                match self.rewrite(other, l) {
                    Some((t2, rest2)) => {
                        self.unify(t, &t2)?;
                        self.unify_row(rest, &rest2)
                    }
                    None => Err(self.row_err(&a, &b)),
                }
            }
            (Row::Empty, Row::Extend(..)) => Err(self.row_err(&a, &b)),
        }
    }
}

// ---------------- 自由变量 ----------------

pub fn ftv_type(t: &Type, tvs: &mut HashSet<VarId>, rvs: &mut HashSet<VarId>) {
    match t {
        Type::Var(v) => { tvs.insert(*v); }
        Type::Fun(a, b) => { ftv_type(a, tvs, rvs); ftv_type(b, tvs, rvs); }
        Type::Record(r) | Type::Variant(r) => ftv_row(r, tvs, rvs),
        _ => {}
    }
}

pub fn ftv_row(r: &Row, tvs: &mut HashSet<VarId>, rvs: &mut HashSet<VarId>) {
    match r {
        Row::Var(v) => { rvs.insert(*v); }
        Row::Extend(_, t, rest) => { ftv_type(t, tvs, rvs); ftv_row(rest, tvs, rvs); }
        Row::Empty => {}
    }
}

// ---------------- 打印 ----------------

pub struct Printer {
    tnames: HashMap<VarId, String>,
    rnames: HashMap<VarId, String>,
}

impl Printer {
    pub fn new() -> Self { Printer { tnames: HashMap::new(), rnames: HashMap::new() } }

    fn tname(&mut self, v: VarId) -> String {
        let n = self.tnames.len();
        self.tnames.entry(v).or_insert_with(|| {
            let mut s = String::new();
            let mut k = n;
            loop { s.push((b'a' + (k % 26) as u8) as char); k = k / 26; if k == 0 { break; } k -= 1; }
            s
        }).clone()
    }

    fn rname(&mut self, v: VarId) -> String {
        let n = self.rnames.len();
        self.rnames.entry(v).or_insert_with(|| format!("ρ{}", n)).clone()
    }

    pub fn print(&mut self, t: &Type) -> String {
        match t {
            Type::Var(v) => self.tname(*v),
            Type::Int => "Int".into(),
            Type::Bool => "Bool".into(),
            Type::Str => "String".into(),
            Type::Unit => "Unit".into(),
            Type::Fun(a, b) => {
                let l = match &**a {
                    Type::Fun(..) => format!("({})", self.print(a)),
                    _ => self.print(a),
                };
                format!("{} -> {}", l, self.print(b))
            }
            Type::Record(r) => format!("{{{}}}", self.print_row(r)),
            Type::Variant(r) => format!("[{}]", self.print_row(r)),
        }
    }

    fn print_row(&mut self, r: &Row) -> String {
        let mut parts = Vec::new();
        let mut cur = r.clone();
        loop {
            match cur {
                Row::Extend(l, t, rest) => {
                    parts.push(format!("{}: {}", l, self.print(&t)));
                    cur = (*rest).clone();
                }
                Row::Empty => break,
                Row::Var(v) => { parts.push(format!("| {}", self.rname(v))); break; }
            }
        }
        parts.join(", ")
    }
}

pub fn print_type(t: &Type) -> String { Printer::new().print(t) }
