//! Source ranges and pretty diagnostics.

use serde::ser::{Serialize, SerializeStruct, Serializer};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl Serialize for Span {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let mut st = s.serialize_struct("Span", 2)?;
        st.serialize_field("start", &self.start)?;
        st.serialize_field("end", &self.end)?;
        st.end()
    }
}

/// Line/column based expansion of a [`Span`], plus the source line for squiggles.
#[derive(Clone, Debug)]
pub struct SpanView {
    pub line: usize,
    pub col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub text: String,
    pub line_start: usize,
}

pub fn view_span(source: &str, span: Span) -> SpanView {
    let mut line = 1usize;
    let mut col = 1usize;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if i == span.start {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
            line_start = i + 1;
        } else {
            col += 1;
        }
    }
    let mut end_line = line;
    let mut end_col = col;
    for (i, ch) in source.char_indices() {
        if i < span.start || i >= span.end {
            continue;
        }
        end_col = col;
        end_line = line;
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    let line_end = source[line_start..]
        .find('\n')
        .map(|i| line_start + i)
        .unwrap_or(source.len());
    SpanView {
        line,
        col,
        end_line,
        end_col,
        text: source[line_start..line_end].to_string(),
        line_start,
    }
}
