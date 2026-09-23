//! 共享基础类型：源码范围与节点标识。

use serde::Serialize;

/// 半开字符区间 [start, end)，基于 Unicode 标量值之前的字节偏移。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span { start: a.start.min(b.start), end: a.end.max(b.end) }
    }
}

/// 解析节点在树中的稳定标识：从根开始的子节点序号路径，点分隔。
/// 例如 "0.2.1"。与变量命名无关，故改名不会造成身份漂移。
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodePath(pub Vec<u32>);

impl NodePath {
    pub fn root() -> Self {
        NodePath(Vec::new())
    }
    pub fn child(&self, idx: u32) -> Self {
        let mut p = self.0.clone();
        p.push(idx);
        NodePath(p)
    }
    pub fn to_string_id(&self) -> String {
        self.0
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(".")
    }
}
