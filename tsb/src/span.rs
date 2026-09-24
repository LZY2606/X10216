//! Byte-accurate source ranges, shared by the lexer, parser and diagnostic layer.

use serde::Serialize;

pub type BytePos = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
}

impl Span {
    pub const fn new(lo: BytePos, hi: BytePos) -> Self {
        Self { lo, hi }
    }
    pub fn union(self, other: Span) -> Span {
        Span {
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
        }
    }
    pub fn as_str<'a>(&self, src: &'a str) -> &'a str {
        &src[self.lo..self.hi]
    }
}

/// Translate a byte span to 1-based line/column coordinates for the UI.
#[derive(Clone, Debug)]
pub struct LineIndex {
    line_starts: Vec<BytePos>,
}

impl LineIndex {
    pub fn new(src: &str) -> Self {
        let mut line_starts = vec![0];
        for (i, b) in src.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// Returns (line, col_start, col_end), all 1-based, in characters.
    pub fn locate(&self, src: &str, span: Span) -> (usize, usize, usize) {
        let line = match self.line_starts.binary_search(&span.lo) {
            Ok(i) => i,
            Err(i) => i - 1,
        };
        let start = self.line_starts[line];
        let col_start = src[start..span.lo].chars().count() + 1;
        let col_end = col_start + src[span.lo..span.hi].chars().count();
        (line + 1, col_start, col_end)
    }
}
