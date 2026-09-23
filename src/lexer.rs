//! 手写词法分析器。

use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Tok {
    pub kind: TokKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokKind {
    Let,
    Rec,
    In,
    Fn,
    If,
    Then,
    Else,
    Match,
    With,
    True,
    False,
    Wild,
    DotDot,
    Dot,
    Eq,
    Arrow,
    Bar,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Int(i64),
    Ident(String),
    Label(String),
    Eof,
}

pub fn lex(src: &str) -> Result<Vec<Tok>, (String, Span)> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mut push = |kind: TokKind, start: usize, end: usize| {
        out.push(Tok {
            kind,
            span: Span::new(start, end),
        });
    };
    while i < b.len() {
        let c = b[i];
        match c {
            b' ' | b'\t' | b'\n' | b'\r' => i += 1,
            b'(' => {
                push(TokKind::LParen, i, i + 1);
                i += 1;
            }
            b')' => {
                push(TokKind::RParen, i, i + 1);
                i += 1;
            }
            b'{' => {
                push(TokKind::LBrace, i, i + 1);
                i += 1;
            }
            b'}' => {
                push(TokKind::RBrace, i, i + 1);
                i += 1;
            }
            b',' => {
                push(TokKind::Comma, i, i + 1);
                i += 1;
            }
            b'|' => {
                push(TokKind::Bar, i, i + 1);
                i += 1;
            }
            b'.' => {
                if b.get(i + 1) == Some(&b'.') {
                    push(TokKind::DotDot, i, i + 2);
                    i += 2;
                } else {
                    push(TokKind::Dot, i, i + 1);
                    i += 1;
                }
            }
            b'=' => {
                push(TokKind::Eq, i, i + 1);
                i += 1;
            }
            b'-' if b.get(i + 1) == Some(&b'>') => {
                push(TokKind::Arrow, i, i + 2);
                i += 2;
            }
            b'0'..=b'9' => {
                let start = i;
                let mut n = 0i64;
                while i < b.len() && b[i].is_ascii_digit() {
                    n = n
                        .saturating_mul(10)
                        .saturating_add((b[i] - b'0') as i64);
                    i += 1;
                }
                push(TokKind::Int(n), start, i);
            }
            b'_' => {
                let start = i;
                i += 1;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') {
                    i += 1;
                }
                let s = &src[start..i];
                if s == "_" {
                    push(TokKind::Wild, start, i);
                } else {
                    push(TokKind::Ident(s.to_string()), start, i);
                }
            }
            b'a'..=b'z' if b.get(i + 1) != Some(&b'`') => {
                let start = i;
                i += 1;
                while i < b.len()
                    && (b[i].is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'\'')
                {
                    i += 1;
                }
                let s = &src[start..i];
                let kind = match s {
                    "let" => TokKind::Let,
                    "rec" => TokKind::Rec,
                    "in" => TokKind::In,
                    "fn" => TokKind::Fn,
                    "if" => TokKind::If,
                    "then" => TokKind::Then,
                    "else" => TokKind::Else,
                    "match" => TokKind::Match,
                    "with" => TokKind::With,
                    "true" => TokKind::True,
                    "false" => TokKind::False,
                    _ => TokKind::Ident(s.to_string()),
                };
                push(kind, start, i);
            }
            b'`' => {
                // `label
                let start = i;
                i += 1;
                let ls = i;
                if i >= b.len() || !(b[i].is_ascii_lowercase() || b[i] == b'_') {
                    return Err((
                        "标签必须以小写字母开头（形如 `ok）".into(),
                        Span::new(start, i.max(start + 1)),
                    ));
                }
                while i < b.len()
                    && (b[i].is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'\'')
                {
                    i += 1;
                }
                push(TokKind::Label(src[ls..i].to_string()), start, i);
            }
            other => {
                return Err((
                    format!("无法识别的字符 {:?}", other as char),
                    Span::new(i, i + 1),
                ))
            }
        }
    }
    push(TokKind::Eof, b.len(), b.len());
    Ok(out)
}
