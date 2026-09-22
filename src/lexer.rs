//! Tokenizer for the HM demo language.

use crate::span::{Span, SpannedError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tok {
    Int(i64),
    Float(String),
    Str(String),
    Ident(String),
    // keywords
    Let, Rec, In, Fun, If, Then, Else, True, False, Match, With,
    // punctuation
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Eq, Colon, Pipe, Arrow, FatArrow, Minus, Star, Slash, Plus, Lt, Gt, Le, Ge,
    EqEq, NotEq, And, Or, Not,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, SpannedError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i];
        if c == b' ' || c == b'\t' || c == b'\r' || c == b'\n' {
            i += 1;
            continue;
        }
        let start = i;
        // comments: (* ... *) and // ...
        if c == b'(' && b.get(i + 1) == Some(&b'*') {
            i += 2;
            let mut closed = false;
            while i < b.len() {
                if b[i] == b'*' && b.get(i + 1) == Some(&b')') {
                    i += 2;
                    closed = true;
                    break;
                }
                i += 1;
            }
            if !closed {
                return Err(SpannedError {
                    message: "unterminated comment (* ... *)".into(),
                    span: Span::new(start, src.len()),
                });
            }
            continue;
        }
        if c == b'/' && b.get(i + 1) == Some(&b'/') {
            i += 2;
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        // numbers
        if c.is_ascii_digit() {
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            let mut is_float = false;
            if b[i] == b'.' && b.get(i + 1).map_or(false, |d| d.is_ascii_digit()) {
                is_float = true;
                i += 1;
                while i < b.len() && b[i].is_ascii_digit() {
                    i += 1;
                }
            }
            let text = &src[start..i];
            let tok = if is_float {
                Tok::Float(text.to_string())
            } else {
                Tok::Int(text.parse().map_err(|_| SpannedError {
                    message: format!("integer literal out of range: {text}"),
                    span: Span::new(start, i),
                })?)
            };
            out.push(Token { tok, span: Span::new(start, i) });
            continue;
        }
        // strings
        if c == b'"' {
            i += 1;
            let mut s = String::new();
            loop {
                if i >= b.len() {
                    return Err(SpannedError {
                        message: "unterminated string literal".into(),
                        span: Span::new(start, i),
                    });
                }
                let ch = b[i];
                if ch == b'"' {
                    i += 1;
                    break;
                }
                if ch == b'\\' {
                    i += 1;
                    if i >= b.len() {
                        return Err(SpannedError {
                            message: "unterminated escape".into(),
                            span: Span::new(start, i),
                        });
                    }
                    match b[i] {
                        b'n' => s.push('\n'),
                        b't' => s.push('\t'),
                        b'r' => s.push('\r'),
                        b'\\' => s.push('\\'),
                        b'"' => s.push('"'),
                        b'0' => s.push('\0'),
                        other => s.push(other as char),
                    }
                    i += 1;
                } else {
                    // copy a full utf-8 char
                    let width = if ch < 0x80 { 1 } else if ch >> 5 == 0b110 { 2 }
                        else if ch >> 4 == 0b1110 { 3 } else { 4 };
                    s.push_str(src.get(i..i + width).unwrap_or("?"));
                    i += width;
                }
            }
            out.push(Token { tok: Tok::Str(s), span: Span::new(start, i) });
            continue;
        }
        // identifiers / keywords
        if c.is_ascii_alphabetic() || c == b'_' {
            while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') {
                i += 1;
            }
            let word = &src[start..i];
            let tok = match word {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fun" => Tok::Fun,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "true" => Tok::True,
                "false" => Tok::False,
                "match" => Tok::Match,
                "with" => Tok::With,
                _ => Tok::Ident(word.to_string()),
            };
            out.push(Token { tok, span: Span::new(start, i) });
            continue;
        }
        // variant tags: Capitalized identifiers become variants only in data position;
        // we lex capitalized words as Ident and let the parser decide via is_tag.
        // punctuation
        macro_rules! push { ($t:expr, $n:expr) => {{
            out.push(Token { tok: $t, span: Span::new(start, start + $n) });
            i += $n;
            continue;
        }}; }
        match c {
            b'(' => push!(Tok::LParen, 1),
            b')' => push!(Tok::RParen, 1),
            b'{' => push!(Tok::LBrace, 1),
            b'}' => push!(Tok::RBrace, 1),
            b'[' => push!(Tok::LBracket, 1),
            b']' => push!(Tok::RBracket, 1),
            b',' => push!(Tok::Comma, 1),
            b'.' => push!(Tok::Dot, 1),
            b'|' => {
                if b.get(i + 1) == Some(&b'|') {
                    push!(Tok::Or, 2)
                }
                push!(Tok::Pipe, 1)
            }
            b':' => push!(Tok::Colon, 1),
            b'#' => {
                // '#' used as record-rest binding marker: #r
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') {
                    i += 1;
                }
                push!(Tok::Ident(src[start..i].to_string()), 0)
            }
            b'-' => {
                if b.get(i + 1) == Some(&b'>') {
                    push!(Tok::Arrow, 2)
                }
                push!(Tok::Minus, 1)
            }
            b'*' => push!(Tok::Star, 1),
            b'/' => push!(Tok::Slash, 1),
            b'+' => push!(Tok::Plus, 1),
            b'<' => {
                match b.get(i + 1) {
                    Some(b'=') => push!(Tok::Le, 2),
                    Some(b'>') => push!(Tok::NotEq, 2),
                    _ => push!(Tok::Lt, 1),
                }
            }
            b'>' => {
                if b.get(i + 1) == Some(&b'=') {
                    push!(Tok::Ge, 2)
                }
                push!(Tok::Gt, 1)
            }
            b'=' => {
                if b.get(i + 1) == Some(&b'=') {
                    push!(Tok::EqEq, 2)
                } else if b.get(i + 1) == Some(&b'>') {
                    push!(Tok::FatArrow, 2)
                }
                push!(Tok::Eq, 1)
            }
            b'!' => {
                if b.get(i + 1) == Some(&b'=') {
                    push!(Tok::NotEq, 2)
                }
                push!(Tok::Not, 1)
            }
            b'&' if b.get(i + 1) == Some(&b'&') => push!(Tok::And, 2),
            other => {
                return Err(SpannedError {
                    message: format!("unexpected character {:?}", other as char),
                    span: Span::new(start, start + 1),
                })
            }
        }
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}
