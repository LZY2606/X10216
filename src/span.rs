use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start as u32, end: end as u32 }
    }
    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Pos {
    pub line: u32,
    pub col: u32,
}

pub fn line_col(src: &str, offset: u32) -> Pos {
    let mut line = 1u32;
    let mut col = 1u32;
    for (i, ch) in src.char_indices() {
        if i as u32 == offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    Pos { line, col }
}
