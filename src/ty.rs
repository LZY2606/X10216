//! 生成阶段的类型与行类型（携带稳定身份，供约束图追踪）。

use crate::span::Span;
use serde::{Deserialize, Serialize};

/// 类型变量标识（同一次生成内稳定；跨版本由 UID 匹配决定是否复用）。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TVid(pub u32);

impl std::fmt::Display for TVid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Ty {
    Var { id: TVid, span: Span },
    Con { name: String, span: Span },
    App { ctor: String, args: Vec<Ty>, span: Span },
    Fn { arg: Box<Ty>, ret: Box<Ty>, span: Span },
    Rec { row: RowTy, span: Span },
    Variant { row: RowTy, span: Span },
}

impl Ty {
    pub fn span(&self) -> Span {
        match self {
            Ty::Var { span, .. }
            | Ty::Con { span, .. }
            | Ty::App { span, .. }
            | Ty::Fn { span, .. }
            | Ty::Rec { span, .. }
            | Ty::Variant { span, .. } => *span,
        }
    }

    pub fn var(id: TVid, span: Span) -> Ty {
        Ty::Var { id, span }
    }

    pub fn con(name: &str, span: Span) -> Ty {
        Ty::Con {
            name: name.to_string(),
            span,
        }
    }

    pub fn fun(arg: Ty, ret: Ty, span: Span) -> Ty {
        Ty::Fn {
            arg: Box::new(arg),
            ret: Box::new(ret),
            span,
        }
    }

    pub fn rec(row: RowTy, span: Span) -> Ty {
        Ty::Rec { row, span }
    }

    pub fn variant(row: RowTy, span: Span) -> Ty {
        Ty::Variant { row, span }
    }

    pub fn vars(&self, out: &mut Vec<TVid>) {
        match self {
            Ty::Var { id, .. } => out.push(*id),
            Ty::Con { .. } => {}
            Ty::App { args, .. } => {
                for a in args {
                    a.vars(out);
                }
            }
            Ty::Fn { arg, ret, .. } => {
                arg.vars(out);
                ret.vars(out);
            }
            Ty::Rec { row, .. } | Ty::Variant { row, .. } => row.vars(out),
        }
    }

    /// 把变量替换为另一个类型（映射后仍为变量的情况），用于行收尾的 occurs 结构构造。
    pub fn map_vars(&self, f: &mut impl FnMut(TVid) -> Ty) -> Ty {
        match self {
            Ty::Var { id, span } => f(*id).with_span_fallback(*span),
            Ty::Con { .. } => self.clone(),
            Ty::App { ctor, args, span } => Ty::App {
                ctor: ctor.clone(),
                args: args.iter().map(|a| a.map_vars(f)).collect(),
                span: *span,
            },
            Ty::Fn { arg, ret, span } => Ty::Fn {
                arg: Box::new(arg.map_vars(f)),
                ret: Box::new(ret.map_vars(f)),
                span: *span,
            },
            Ty::Rec { row, span } => Ty::Rec {
                row: row.map_vars(f),
                span: *span,
            },
            Ty::Variant { row, span } => Ty::Variant {
                row: row.map_vars(f),
                span: *span,
            },
        }
    }

    fn with_span_fallback(self, _span: Span) -> Ty {
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RowTail {
    /// 封闭行：{ x: T } 无 `| ..`。
    Closed,
    /// 开放行：{ x: T | r }。
    Open(TVid),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RowField {
    pub label: String,
    pub label_span: Span,
    pub ty: Ty,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RowTy {
    pub fields: Vec<RowField>,
    pub tail: RowTail,
    /// 该结构构造所在源码位置。
    pub span: Span,
}

impl RowTy {
    pub fn vars(&self, out: &mut Vec<TVid>) {
        for f in &self.fields {
            f.ty.vars(out);
        }
        if let RowTail::Open(v) = self.tail {
            out.push(v);
        }
    }

    pub fn map_vars(&self, f: &mut impl FnMut(TVid) -> Ty) -> RowTy {
        RowTy {
            fields: self
                .fields
                .iter()
                .map(|fld| RowField {
                    label: fld.label.clone(),
                    label_span: fld.label_span,
                    ty: fld.ty.map_vars(f),
                })
                .collect(),
            tail: match self.tail {
                RowTail::Closed => RowTail::Closed,
                RowTail::Open(v) => match f(v) {
                    Ty::Var { id, .. } => RowTail::Open(id),
                    _ => RowTail::Open(v),
                },
            },
            span: self.span,
        }
    }

    pub fn lookup(&self, label: &str) -> Option<&Ty> {
        self.fields
            .iter()
            .find(|f| f.label == label)
            .map(|f| &f.ty)
    }
}

/// 生成器 / 求解器共享的变量供给器。
#[derive(Debug, Default)]
pub struct Supply {
    next: u32,
}

impl Supply {
    pub fn new(start: u32) -> Supply {
        Supply { next: start }
    }

    pub fn fresh(&mut self) -> TVid {
        let id = TVid(self.next);
        self.next += 1;
        id;
        id
    }

    pub fn peek(&self) -> u32 {
        self.next
    }

    pub fn note(&mut self, id: TVid) {
        if id.0 >= self.next {
            self.next = id.0 + 1;
        }
    }
}
