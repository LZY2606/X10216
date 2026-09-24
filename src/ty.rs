//! 类型表示、并查集、统一与 occurs-check。
//!
//! 行（record/variant）以有序字段表 + 尾行表示；尾行可以是开放的行变量，
//! 这是实现行多态的关键。统一时把“缺字段”和“多余字段”分开报告。

use serde::Serialize;
use std::collections::BTreeSet;
use std::fmt::Write;

/// 类型变量 id（一次推断内唯一）。
pub type VarId = u32;

/// 一个约束/操作的稳定身份，会进入 support，用于定位最小错误核心。
pub type OpKey = String;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum TypeF {
    Int,
    Bool,
    /// 无载荷变体标签的载荷类型。
    Unit,
    Arrow(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Type {
    pub var: Option<VarId>,
    pub term: Option<TypeF>,
    /// 参与塑造该类型项的约束身份集合（最小核心定位用）。
    pub support: BTreeSet<OpKey>,
}

impl Type {
    pub fn var(v: VarId) -> Self {
        Type { var: Some(v), term: None, support: BTreeSet::new() }
    }
    pub fn term(term: TypeF) -> Self {
        Type { var: None, term: Some(term), support: BTreeSet::new() }
    }
    pub fn with_support(mut self, s: impl IntoIterator<Item = OpKey>) -> Self {
        self.support.extend(s);
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Field {
    pub label: String,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Row {
    /// 字段按 label 排序。
    pub fields: Vec<Field>,
    /// `Some(v)` 表示开放行（可能还有其它字段）；`None` 表示封闭行。
    pub tail: Option<VarId>,
    /// 行整体的 support（主要用于缺/余字段定位）。
    pub support: BTreeSet<OpKey>,
}

impl Row {
    pub fn empty_closed() -> Self {
        Row { fields: Vec::new(), tail: None, support: BTreeSet::new() }
    }
    pub fn get(&self, label: &str) -> Option<&Type> {
        self.fields.iter().find(|f| f.label == label).map(|f| &f.ty)
    }
}

// ---------------------------------------------------------------------------
// Union-find
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
enum NodeData {
    Root(Option<TypeF>, BTreeSet<OpKey>),
    Link(VarId),
}

pub struct UnionFind {
    nodes: Vec<NodeData>,
    /// 非泛化（刚性）变量集合：lambda 参数、递归自引用等。
    rigid: BTreeSet<VarId>,
}

impl UnionFind {
    pub fn new() -> Self {
        UnionFind { nodes: Vec::new(), rigid: BTreeSet::new() }
    }

    pub fn fresh(&mut self) -> VarId {
        let id = self.nodes.len() as VarId;
        self.nodes.push(NodeData::Root(None, BTreeSet::new()));
        id
    }

    pub fn mark_rigid(&mut self, v: VarId) {
        self.rigid.insert(v);
    }
    pub fn unmark_rigid(&mut self, v: VarId) {
        self.rigid.remove(&v);
    }
    pub fn is_rigid(&self, v: VarId) -> bool {
        let r = self.find(v);
        self.rigid.contains(&r)
    }

    pub fn find(&self, mut v: VarId) -> VarId {
        let mut root = v;
        loop {
            match &self.nodes[root as usize] {
                NodeData::Link(p) => root = *p,
                NodeData::Root(..) => break,
            }
        }
        // 路径压缩（就地）
        while v != root {
            if let NodeData::Link(p) = &self.nodes[v as usize] {
                let next = *p;
                self.nodes[v as usize] = NodeData::Link(root);
                v = next;
            } else {
                break;
            }
        }
        root
    }

    fn root(&self, v: VarId) -> (&Option<TypeF>, &BTreeSet<OpKey>) {
        let r = self.find(v);
        if let NodeData::Root(t, s) = &self.nodes[r as usize] {
            (t, s)
        } else {
            unreachable!()
        }
    }

    fn root_mut(&mut self, v: VarId) -> (VarId, &mut Option<TypeF>, &mut BTreeSet<OpKey>) {
        let r = self.find(v);
        if let NodeData::Root(t, s) = &mut self.nodes[r as usize] {
            (r, t, s)
        } else {
            unreachable!()
        }
    }

    pub fn support_of(&self, v: VarId) -> BTreeSet<OpKey> {
        self.root(v).1.clone()
    }

    pub fn add_support(&mut self, v: VarId, keys: &BTreeSet<OpKey>) {
        let (_, _, s) = self.root_mut(v);
        s.extend(keys.iter().cloned());
    }

    /// 读取变量当前解析出的类型（递归解析项中的变量）。
    pub fn resolve(&self, ty: &Type) -> Type {
        let mut out = ty.clone();
        out.support = self.collect_support(ty);
        if let Some(v) = ty.var {
            let r = self.find(v);
            if let NodeData::Root(Some(term), sup) = &self.nodes[r as usize] {
                out.var = Some(r);
                out.term = Some(self.resolve_term(term));
                out.support.extend(sup.iter().cloned());
            } else {
                out.var = Some(r);
                out.term = None;
            }
        } else if let Some(term) = &ty.term {
            out.term = Some(self.resolve_term(term));
        }
        out
    }

    fn resolve_term(&self, term: &TypeF) -> TypeF {
        match term {
            TypeF::Int | TypeF::Bool | TypeF::Unit => term.clone(),
            TypeF::Arrow(a, b) => TypeF::Arrow(
                Box::new(self.resolve(a)),
                Box::new(self.resolve(b)),
            ),
            TypeF::Record(row) => TypeF::Record(self.resolve_row(row)),
            TypeF::Variant(row) => TypeF::Variant(self.resolve_row(row)),
        }
    }

    fn resolve_row(&self, row: &Row) -> Row {
        let mut fields = Vec::new();
        for f in &row.fields {
            fields.push(Field { label: f.label.clone(), ty: self.resolve(&f.ty) });
        }
        // 若尾变量已绑定，则并入其字段。
        let (fields, tail, mut sup) = self.flatten_tail(fields, row.tail);
        sup.extend(row.support.iter().cloned());
        Row { fields, tail, support: sup }
    }

    /// 把可能已绑定的尾行展开，返回规范的（排序、去重交给统一保证）行。
    fn flatten_tail(
        &self,
        mut acc: Vec<Field>,
        tail: Option<VarId>,
    ) -> (Vec<Field>, Option<VarId>, BTreeSet<OpKey>) {
        let mut support = BTreeSet::new();
        let mut cur = tail;
        while let Some(v) = cur {
            let r = self.find(v);
            match &self.nodes[r as usize] {
                NodeData::Root(Some(TypeF::Record(rr)) | Some(TypeF::Variant(rr)), s) => {
                    for f in &rr.fields {
                        acc.push(Field {
                            label: f.label.clone(),
                            ty: self.resolve(&f.ty),
                        });
                    }
                    support.extend(s.iter().cloned());
                    cur = rr.tail;
                }
                NodeData::Root(..) => {
                    return (acc, Some(r), support);
                }
                NodeData::Link(..) => unreachable!(),
            }
        }
        (acc, None, support)
    }

    pub fn collect_support(&self, ty: &Type) -> BTreeSet<OpKey> {
        let mut s = ty.support.clone();
        match (&ty.var, &ty.term) {
            (Some(v), _) => {
                let r = self.find(*v);
                if let NodeData::Root(term, sup) = &self.nodes[r as usize] {
                    s.extend(sup.iter().cloned());
                    if let Some(t) = term {
                        s.extend(self.support_term(t));
                    }
                }
            }
            (None, Some(t)) => s.extend(self.support_term(t)),
            (None, None) => {}
        }
        s
    }

    fn support_term(&self, t: &TypeF) -> BTreeSet<OpKey> {
        let mut s = BTreeSet::new();
        match t {
            TypeF::Int | TypeF::Bool | TypeF::Unit => {}
            TypeF::Arrow(a, b) => {
                s.extend(self.collect_support(a));
                s.extend(self.collect_support(b));
            }
            TypeF::Record(r) | TypeF::Variant(r) => {
                s.extend(r.support.iter().cloned());
                for f in &r.fields {
                    s.extend(self.collect_support(&f.ty));
                }
            }
        }
        s
    }
}

// ---------------------------------------------------------------------------
// Occurs check 与统一
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum RowKind {
    Record,
    Variant,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct FieldError {
    pub label: String,
    /// 触发该字段存在性约束的操作。
    pub supports: Vec<OpKey>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum UnifyError {
    Occurs {
        var: VarId,
        printed: String,
        supports: Vec<OpKey>,
    },
    Clash {
        left: String,
        right: String,
        supports: Vec<OpKey>,
    },
    RowMismatch {
        kind: RowKind,
        /// 期望有、实际缺的字段。
        missing: Vec<FieldError>,
        /// 实际有、期望封闭而多余的字段。
        extra: Vec<FieldError>,
        supports: Vec<OpKey>,
    },
    LabelClash {
        kind: RowKind,
        label: String,
        supports: Vec<OpKey>,
    },
}

impl UnionFind {
    /// occurs-check：变量 v 是否出现在类型 ty 中。
    pub fn occurs(&self, v: VarId, ty: &Type) -> bool {
        let resolved = self.resolve(ty);
        self.occurs_resolved(v, &resolved)
    }

    fn occurs_resolved(&self, v: VarId, ty: &Type) -> bool {
        if let Some(tv) = ty.var {
            if tv == v {
                return true;
            }
        }
        match &ty.term {
            None => false,
            Some(TypeF::Arrow(a, b)) => self.occurs_resolved(v, a) || self.occurs_resolved(v, b),
            Some(TypeF::Record(r)) | Some(TypeF::Variant(r)) => {
                r.fields.iter().any(|f| self.occurs_resolved(v, &f.ty))
                    || r.tail.map(|t| t == v).unwrap_or(false)
            }
            Some(TypeF::Int) | Some(TypeF::Bool) => false,
        }
    }

    /// 统一两个类型。`op` 是触发本次统一的约束身份。
    pub fn unify(&mut self, actual: &Type, expected: &Type, op: &OpKey) -> Result<(), UnifyError> {
        let a = self.resolve(actual);
        let e = self.resolve(expected);
        self.unify_resolved(a, e, op)
    }

    fn unify_resolved(
        &mut self,
        a: Type,
        e: Type,
        op: &OpKey,
    ) -> Result<(), UnifyError> {
        // 两边都是变量
        if a.term.is_none() && e.term.is_none() {
            let av = a.var.unwrap();
            let ev = e.var.unwrap();
            if av != ev {
                self.link(av, ev, op);
            }
            return Ok(());
        }
        // 一边是变量：做 occurs-check 后绑定
        if a.term.is_none() || e.term.is_none() {
            let (var_side, term_side) = if a.term.is_none() {
                (a.clone(), e.clone())
            } else {
                (e.clone(), a.clone())
            };
            let v = var_side.var.unwrap();
            let term = term_side.term.clone().unwrap();
            if self.occurs_in_term(v, &term) {
                let printed = print_type(&self.resolve(&Type::term(term.clone())));
                return Err(UnifyError::Occurs {
                    var: v,
                    printed,
                    supports: self.supports_for_occurs(&term_side, op),
                });
            }
            let term_resolved = self.resolve_term(&term);
            let mut sup = term_side.support.clone();
            sup.extend(var_side.support.iter().cloned());
            self.bind_term(v, term_resolved, sup, op);
            return Ok(());
        }

        // 两边都是具体项
        let at = a.term.clone().unwrap();
        let et = e.term.clone().unwrap();
        match (at, et) {
            (TypeF::Int, TypeF::Int)
            | (TypeF::Bool, TypeF::Bool)
            | (TypeF::Unit, TypeF::Unit) => Ok(()),
            (TypeF::Arrow(a1, a2), TypeF::Arrow(e1, e2)) => {
                let op1 = op.clone();
                self.unify(&a1, &e1, &op1)?;
                self.unify(&a2, &e2, op)?;
                Ok(())
            }
            (TypeF::Record(ra), TypeF::Record(re)) => {
                self.unify_rows(RowKind::Record, ra, re, op)
            }
            (TypeF::Variant(ra), TypeF::Variant(re)) => {
                self.unify_rows(RowKind::Variant, ra, re, op)
            }
            (x, y) => {
                let lx = print_term(&x);
                let ly = print_term(&y);
                let mut sup = BTreeSet::new();
                sup.extend(self.collect_support(&Type::term(x.clone())));
                sup.extend(self.collect_support(&Type::term(y.clone())));
                sup.insert(op.clone());
                Err(UnifyError::Clash {
                    left: lx,
                    right: ly,
                    supports: sup.into_iter().collect(),
                })
            }
        }
    }

    fn supports_for_occurs(&self, term_side: &Type, op: &OpKey) -> Vec<OpKey> {
        let mut s = self.collect_support(term_side);
        s.insert(op.clone());
        s.into_iter().collect()
    }

    fn occurs_in_term(&self, v: VarId, t: &TypeF) -> bool {
        match t {
            TypeF::Int | TypeF::Bool | TypeF::Unit => false,
            TypeF::Arrow(a, b) => self.occurs(v, a) || self.occurs(v, b),
            TypeF::Record(r) | TypeF::Variant(r) => {
                r.fields.iter().any(|f| self.occurs(v, &f.ty))
                    || r.tail.map(|t| self.occurs_var(v, t)).unwrap_or(false)
            }
        }
    }

    fn occurs_var(&self, v: VarId, in_v: VarId) -> bool {
        let r = self.find(in_v);
        if r == v {
            return true;
        }
        if let NodeData::Root(Some(t), _) = &self.nodes[r as usize] {
            return self.occurs_in_term(v, t);
        }
        false
    }

    fn link(&mut self, av: VarId, ev: VarId, op: &OpKey) {
        let ar = self.find(av);
        let er = self.find(ev);
        if ar == er {
            return;
        }
        // 保持刚性标记：任一为根刚性则结果刚性
        let rigid = self.rigid.contains(&ar) || self.rigid.contains(&er);
        // 若一边已绑定 term，则绑定到另一边
        let a_term = if let NodeData::Root(t, _) = &self.nodes[ar as usize] {
            t.clone()
        } else {
            None
        };
        let e_term = if let NodeData::Root(t, _) = &self.nodes[er as usize] {
            t.clone()
        } else {
            None
        };
        let mut sup = BTreeSet::new();
        sup.extend(self.support_of(ar));
        sup.extend(self.support_of(er));
        sup.insert(op.clone());
        // er 作为根
        self.nodes[ar as usize] = NodeData::Link(er);
        if let NodeData::Root(t, s) = &mut self.nodes[er as usize] {
            if t.is_none() {
                if let Some(tt) = a_term.or(e_term) {
                    *t = Some(self.resolve_term(&tt));
                }
            }
            s.extend(sup);
        }
        if rigid {
            self.rigid.insert(er);
        }
    }

    fn bind_term(&mut self, v: VarId, term: TypeF, sup: BTreeSet<OpKey>, op: &OpKey) {
        let r = self.find(v);
        if let NodeData::Root(t, s) = &mut self.nodes[r as usize] {
            *t = Some(term);
            s.extend(sup);
            s.insert(op.to_string());
        }
    }
}

impl UnionFind {
    /// 行统一。约定 `actual` 是实际值行，`expected` 是被期待行；
    /// 缺字段 = 期望有而实际没有；多余字段 = 实际有而期望（封闭）没有。
    pub fn unify_rows(
        &mut self,
        kind: RowKind,
        actual: Row,
        expected: Row,
        op: &OpKey,
    ) -> Result<(), UnifyError> {
        let a = self.canon_row(actual);
        let e = self.canon_row(expected);

        let mut missing: Vec<FieldError> = Vec::new();
        let mut extra: Vec<FieldError> = Vec::new();

        // 统一共同字段
        for ef in &e.fields {
            match a.fields.iter().find(|f| f.label == ef.label) {
                Some(af) => {
                    self.unify(&af.ty, &ef.ty, op)?;
                }
                None => {
                    if e.tail.is_none() || a.tail.is_some() {
                        // 期望封闭，或实际开放：可由开放尾吸收/提供，不算缺失；
                        // 只有当实际封闭（无法补齐）而期望要求该字段时才是缺失。
                        if a.tail.is_none() {
                            missing.push(FieldError {
                                label: ef.label.clone(),
                                supports: ef
                                    .ty
                                    .support
                                    .iter()
                                    .chain(e.support.iter())
                                    .cloned()
                                    .collect::<BTreeSet<_>>()
                                    .into_iter()
                                    .collect(),
                            });
                        }
                    }
                }
            }
        }

        // 多余字段：实际有、期望封闭则报错；期望开放则吸收进期望尾变量。
        for af in &a.fields {
            if !e.fields.iter().any(|f| f.label == af.label) {
                if e.tail.is_none() {
                    extra.push(FieldError {
                        label: af.label.clone(),
                        supports: af
                            .ty
                            .support
                            .iter()
                            .chain(a.support.iter())
                            .cloned()
                            .collect::<BTreeSet<_>>()
                            .into_iter()
                            .collect(),
                    });
                }
            }
        }

        if !missing.is_empty() || !extra.is_empty() {
            let mut sup = BTreeSet::new();
            sup.insert(op.to_string());
            sup.extend(a.support.iter().cloned());
            sup.extend(e.support.iter().cloned());
            return Err(UnifyError::RowMismatch {
                kind,
                missing,
                extra,
                supports: sup.into_iter().collect(),
            });
        }

        // 处理尾行
        match (a.tail, e.tail) {
            (None, None) => Ok(()),
            (Some(at), None) => {
                // 实际开放、期望封闭：实际尾必须为空行
                let empty = match kind {
                    RowKind::Record => TypeF::Record(Row::empty_closed()),
                    RowKind::Variant => TypeF::Variant(Row::empty_closed()),
                };
                self.unify(
                    &Type::var(at),
                    &Type::term(empty),
                    op,
                )
            }
            (None, Some(et)) => {
                // 实际封闭、期望开放：把期望尾绑定为空（多态调用点实例化为封闭）
                let empty = match kind {
                    RowKind::Record => TypeF::Record(Row::empty_closed()),
                    RowKind::Variant => TypeF::Variant(Row::empty_closed()),
                };
                self.unify(
                    &Type::term(empty),
                    &Type::var(et),
                    op,
                )
            }
            (Some(at), Some(et)) => {
                if self.find(at) != self.find(et) {
                    self.link(at, et, op);
                }
                Ok(())
            }
        }
    }

    /// 规范化一行：展开已绑定尾、字段排序去重（重复由 LabelClash 报告交给上层）。
    pub fn canon_row(&self, row: Row) -> Row {
        let resolved = self.resolve_row(&row);
        let mut fields = resolved.fields;
        fields.sort_by(|x, y| x.label.cmp(&y.label));
        let mut support = resolved.support;
        for f in &fields {
            support.extend(f.ty.support.iter().cloned());
        }
        Row { fields, tail: resolved.tail, support }
    }
}

// ---------------------------------------------------------------------------
// 打印
// ---------------------------------------------------------------------------

pub fn print_type(ty: &Type) -> String {
    let mut s = String::new();
    write_type(ty, &mut s, &mut 0);
    s
}

fn write_type(ty: &Type, s: &mut String, depth: &mut u32) {
    if let Some(t) = &ty.term {
        write_term(t, s, depth);
    } else if let Some(v) = ty.var {
        write!(s, "t{}", v).ok();
    } else {
        s.push('?');
    }
}

pub fn print_term(t: &TypeF) -> String {
    let mut s = String::new();
    write_term(t, &mut s, &mut 0);
    s
}

fn write_term(t: &TypeF, s: &mut String, depth: &mut u32) {
    match t {
        TypeF::Int => s.push_str("Int"),
        TypeF::Bool => s.push_str("Bool"),
        TypeF::Unit => s.push_str("()"),
        TypeF::Arrow(a, b) => {
            s.push('(');
            write_type(a, s, depth);
            s.push_str(" -> ");
            write_type(b, s, depth);
            s.push(')');
        }
        TypeF::Record(row) => {
            s.push_str("{ ");
            write_row(row, s, depth);
            s.push_str(" }");
        }
        TypeF::Variant(row) => {
            s.push_str("[ ");
            write_row(row, s, depth);
            s.push_str(" ]");
        }
    }
}

fn write_row(row: &Row, s: &mut String, depth: &mut u32) {
    let mut first = true;
    for f in &row.fields {
        if !first {
            s.push_str(", ");
        }
        first = false;
        write!(s, "{}: ", f.label).ok();
        write_type(&f.ty, s, depth);
    }
    if let Some(v) = row.tail {
        if !first {
            s.push_str(", ");
        }
        write!(s, "<t{}>", v).ok();
    }
}
