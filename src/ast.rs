use crate::span::Span;
use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, PartialOrd, Ord)]
pub struct Pid(pub u64);

impl Pid {
    pub const ROOT: Pid = Pid(0x511CE5511CE551CE);
}

/// 由父身份、子序号与角色派生稳定身份；不依赖变量名。
pub fn pid(parent: Pid, idx: u32, role: u32) -> Pid {
    let mut x = parent.0
        .wrapping_mul(0x9E3779B97F4A7C15)
        .wrapping_add((idx as u64) << 8 | role as u64);
    x ^= x >> 30;
    x = x.wrapping_mul(0xBF58476D1CE4E5B9);
    x ^= x >> 27;
    x = x.wrapping_mul(0x94D049BB133111EB);
    x ^= x >> 31;
    Pid(x)
}

#[derive(Clone, Debug, Serialize, PartialEq)]
#[serde(tag = "kind")]
pub enum ExprE {
    Lit(Lit),
    Var { name: String },
    Fun { param: String, body: Pid },
    App { fun: Pid, arg: Pid },
    Let { name: String, value: Pid, body: Pid, rec_pid: Option<Pid> },
    Record { fields: Vec<(String, Pid)> },
    Field { obj: Pid, label: String },
    Tag { tag: String, payload: Option<Pid> },
    Match { scrut: Pid, arms: Vec<Arm> },
    Bin { op: BinOp, lhs: Pid, rhs: Pid },
}

#[derive(Clone, Copy, Debug, Serialize, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Eq,
}

#[derive(Clone, Copy, Debug, Serialize, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Str,
    Bool(bool),
    Unit,
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Arm {
    pub tag: String,
    pub binding: Option<String>,
    pub expr: Pid,
}

#[derive(Clone, Debug, Serialize)]
pub struct Node {
    pub pid: Pid,
    pub span: Span,
    pub expr: ExprE,
}
