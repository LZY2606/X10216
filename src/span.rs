//! Byte ranges into the source string with cheap conversion to line/column.

use serde::{Deserialize, Serialize};

pub type NodeId = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
    pub fn merge(self, other: Span) -> Span {
        Span::new(self.lo.min(other.lo) as usize, self.hi.max(other.hi) as usize)
    }
    pub fn loc(self, src: &str) -> Loc {
        let mut line = 1u32;
        let mut col = 1u32;
        for (i, b) in src.bytes().enumerate() {
            if i == self.lo as usize {
                break;
            }
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        Loc { line, col }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Loc {
    pub line: u32,
    pub col: u32,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}
