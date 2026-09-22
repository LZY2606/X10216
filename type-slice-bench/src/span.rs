//! 源码范围（字节偏移）与坐标换算。

use serde::{Deserialize, Serialize};

pub type Gid = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
    pub fn join(self, other: Span) -> Span {
        Span { lo: self.lo.min(other.lo), hi: self.hi.max(other.hi) }
    }
}

/// 把字节偏移换算成 (行,列)，从 1 开始计数。
pub fn line_col(src: &str, off: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, ch) in src.char_indices() {
        if i >= off {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

pub fn line_cols(src: &str, spans: &[Span]) -> Vec<(usize, usize, usize, usize)> {
    spans
        .iter()
        .map(|s| {
            let (l1, c1) = line_col(src, s.lo);
            let (l2, c2) = line_col(src, s.hi);
            (l1, c1, l2, c2)
        })
        .collect()
}
