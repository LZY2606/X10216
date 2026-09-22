use serde::{Deserialize, Serialize};

/// 源码范围（字节偏移，半开区间）
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start: start as u32, end: end as u32 }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span { start: a.start.min(b.start), end: a.end.max(b.end) }
    }
    pub fn len(&self) -> u32 {
        self.end.saturating_sub(self.start)
    }
}
