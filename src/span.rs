//! 源码来源范围（UTF-8 字节偏移）。

use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }
    pub fn is_empty(&self) -> bool {
        self.end <= self.start
    }
    /// 返回 (起始行, 起始列, 结束行, 结束列)，行列均从 1 开始。
    pub fn line_cols(&self, src: &str) -> (usize, usize, usize, usize) {
        let lc = |off: usize| {
            let mut line = 1;
            let mut col = 1;
            for (i, c) in src.char_indices() {
                if i >= off {
                    break;
                }
                if c == '\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
            }
            (line, col)
        };
        let (sl, sc) = lc(self.start);
        let (el, ec) = lc(self.end);
        (sl, sc, el, ec)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn line_cols() {
        let s = "let x = 1\nin x";
        let sp = Span::new(10, 11);
        assert_eq!(sp.line_cols(s), (2, 1, 2, 2));
    }
}
