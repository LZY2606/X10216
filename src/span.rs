use serde::Serialize;

/// Half-open byte range [start, end) into the source string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const DUMMY: Span = Span { start: 0, end: 0 };
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn union(a: Span, b: Span) -> Span {
        Span { start: a.start.min(b.start), end: a.end.max(b.end) }
    }
    /// 1-based line, 1-based column, and length in bytes.
    pub fn line_col(&self, src: &str) -> (usize, usize, usize, usize) {
        let (mut sl, mut sc) = (1usize, 1usize);
        for (i, ch) in src.char_indices() {
            if i >= self.start { break; }
            if ch == '\n' { sl += 1; sc = 1; } else { sc += ch.len_utf8(); }
        }
        let (mut el, mut ec) = (sl, sc);
        for (i, ch) in src.char_indices() {
            if i >= self.end { break; }
            if i >= self.start && ch == '\n' { el += 1; ec = 1; }
            else if i >= self.start { ec += ch.len_utf8(); }
        }
        (sl, sc, el, ec)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct Label(pub String);

impl Label {
    pub fn new(s: impl Into<String>) -> Self { Label(s.into()) }
    pub fn as_str(&self) -> &str { &self.0 }
}
