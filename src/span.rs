//! 源码范围（半开区间，按 UTF-8 字节偏移）。

use serde::Serialize;

/// 半开字节区间 [start, end)。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// 从源码按行/列（0 基）提取展示片段所需的行信息。
#[derive(Clone, Debug, Serialize)]
pub struct LineInfo {
    pub line: usize,
    pub col_start: usize,
    pub col_end: usize,
    pub text: String,
}

pub fn line_info(src: &str, span: Span) -> LineInfo {
    let mut line = 0usize;
    let mut line_start = 0usize;
    for (i, b) in src.bytes().enumerate() {
        if i == span.start {
            break;
        }
        if b == b'\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    let mut line_end = src[line_start..]
        .find('\n')
        .map(|p| line_start + p)
        .unwrap_or(src.len());
    if line_end < span.end {
        // 跨度跨行时，文本仍只展示起始行；结束列钳制在行尾
        line_end = line_end.max(span.end);
    }
    let text = src[line_start..line_end.min(src.len())].to_string();
    LineInfo {
        line,
        col_start: span.start - line_start,
        col_end: (span.end - line_start).min(text.len()),
        text,
    }
}
