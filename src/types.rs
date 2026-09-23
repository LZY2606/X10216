//! 冻结类型（无并查集的纯数据），用于方案、结果视图与 JSON。
use serde::Serialize;
use std::collections::BTreeMap;

/// 类型变量标识（求解器内编号）。
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TV(pub u32);
/// 行变量标识。
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RV(pub u32);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Row {
    /// 开放行：字段 + 一个行变量。
    Open(BTreeMap<String, RType>, RV),
    /// 封闭行：恰好这些字段。
    Closed(BTreeMap<String, RType>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RType {
    Var(TV),
    Int,
    Bool,
    Unit,
    Arrow(Box<RType>, Box<RType>),
    Record(Row),
    Variant(Row),
}

impl RType {
    pub fn is_int(&self) -> bool { matches!(self, RType::Int) }
}

/// 泛化方案 `forall bvars. body`。bvars 为类型变量，`rbvars` 为行变量。
#[derive(Clone, Debug)]
pub struct Scheme {
    pub bvars: Vec<TV>,
    pub rbvars: Vec<RV>,
    pub body: RType,
}
