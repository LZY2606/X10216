//! Source spans: byte offsets + line/column + the raw text snippet.

use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub const DUMMY: Span = Span { lo: 0, hi: 0 };
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
    pub fn merge(self, other: Span) -> Span {
        Span { lo: self.lo.min(other.lo), hi: self.hi.max(other.hi) }
    }
    pub fn range(&self) -> std::ops::Range<usize> {
        (self.lo as usize)..(self.hi as usize)
    }
    pub fn snippet<'a>(&self, src: &'a str) -> &'a str {
        &src[self.range()]
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.lo, self.hi)
    }
}

/// A stable, parse-independent identity for a syntax node.
///
/// - `anchor`: byte offset of the node's start in the source it was parsed from
/// - `role`:   the node's grammar role (expr kind, binding, type-var role...)
/// - `tag`:    content-derived hash so that moving/retyping changes identity
///
/// Renaming a bound variable does not change the node it *refers to*:
/// reference nodes carry the anchor of their binding.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct NodeId {
    pub anchor: u32,
    pub role: u16,
    pub tag: u32,
}

impl NodeId {
    pub fn new(anchor: usize, role: u16, tag: u32) -> Self {
        NodeId { anchor: anchor as u32, role, tag }
    }
    pub fn key(&self) -> String {
        format!("n{}-r{}-t{:08x}", self.anchor, self.role, self.tag)
    }
}

impl fmt::Debug for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.key())
    }
}

pub fn line_col(src: &str, offset: usize) -> (usize, usize) {
    let mut line = 1usize;
    let mut col = 1usize;
    for (i, ch) in src.char_indices() {
        if i >= offset {
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
