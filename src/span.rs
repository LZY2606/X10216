//! Source locations and byte ranges.

use serde::Serialize;

/// A half-open byte range `[start, end)` inside one source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
    /// 1-based line/col of `offset` (col counts UTF-8 bytes within the line).
    pub fn line_col(&self, src: &str, offset: usize) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;
        for (i, b) in src.bytes().enumerate() {
            if i == offset {
                break;
            }
            if b == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        (line, col)
    }
    pub fn snippet<'a>(&self, src: &'a str) -> &'a str {
        &src[self.start.min(src.len())..self.end.min(src.len())]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SpannedError {
    pub message: String,
    pub span: Span,
}
