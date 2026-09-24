//! 源码区间（半开区间，字节偏移）。

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const EMPTY: Span = Span { start: 0, end: 0 };

    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn combine<I: IntoIterator<Item = Span>>(spans: I) -> Span {
        let mut iter = spans.into_iter();
        let first = iter.next().unwrap_or(Span::EMPTY);
        iter.fold(first, Span::merge)
    }

    pub fn snippet<'a>(&self, src: &'a str) -> &'a str {
        &src[self.start..self.end.min(src.len())]
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::EMPTY
    }
}
