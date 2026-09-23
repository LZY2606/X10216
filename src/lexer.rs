//! Hand-written lexer.

use serde::{Deserialize, Serialize};

use crate::syntax::Span;

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Tok {
    Int(i64),
    Str(String),
    Ident(String),
    Op(String),
    Let,
    Rec,
    In,
    Fn,
    If,
    Then,
    Else,
    True,
    False,
    Match,
    With,
    Underscore,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LAngle,
    RAngle,
    Eq,
    DoubleColon,
    Colon,
    Comma,
    Dot,
    Arrow,
    FatArrow,
    Pipe,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, (usize, String)> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    loop {
        while i < b.len() {
            match b[i] {
                b' ' | b'\t' | b'\n' | b'\r' => i += 1,
                b'-' if i + 1 < b.len() && b[i + 1] == b'-' => {
                    i += 2;
                    while i < b.len() && b[i] != b'\n' {
                        i += 1;
                    }
                }
                b'/' if i + 1 < b.len() && b[i + 1] == b'*' => {
                    let start = i;
                    i += 2;
                    let mut depth = 1;
                    while i < b.len() && depth > 0 {
                        if i + 1 < b.len() && b[i] == b'/' && b[i + 1] == b'*' {
                            depth += 1;
                            i += 2;
                        } else if i + 1 < b.len() && b[i] == b'*' && b[i + 1] == b'/' {
                            depth -= 1;
                            i += 2;
                        } else {
                            i += 1;
                        }
                    }
                    if depth > 0 {
                        return Err((start, "unterminated block comment".into()));
                    }
                }
                _ => break,
            }
        }
        if i >= b.len() {
            out.push(Token {
                tok: Tok::Eof,
                span: Span::new(src.len(), src.len()),
            });
            return Ok(out);
        }
        let start = i;
        let c = b[i];
        if c.is_ascii_digit() {
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[start..i].parse().unwrap();
            out.push(Token { tok: Tok::Int(n), span: Span::new(start, i) });
        } else if c.is_ascii_alphabetic() || c == b'_' {
            while i < b.len()
                && (b[i].is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'\'')
            {
                i += 1;
            }
            let word = &src[start..i];
            let tok = if word == "_" && i - start == 1 {
                Tok::Underscore
            } else {
                match word {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "fn" => Tok::Fn,
                    "if" => Tok::If,
                    "then" => Tok::Then,
                    "else" => Tok::Else,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "match" => Tok::Match,
                    "with" => Tok::With,
                    _ => Tok::Ident(word.to_string()),
                }
            };
            out.push(Token { tok, span: Span::new(start, i) });
        } else if c == b'"' {
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1];
                    s.push(match e {
                        b'n' => '\n',
                        b't' => '\t',
                        b'"' => '"',
                        b'\\' => '\\',
                        _ => e as char,
                    });
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err((start, "unterminated string".into()));
            }
            i += 1;
            out.push(Token { tok: Tok::Str(s), span: Span::new(start, i) });
        } else {
            let push = |out: &mut Vec<Token>, tok: Tok, end: usize| {
                out.push(Token { tok, span: Span::new(start, end) });
            };
            match c {
                b'(' => { push(&mut out, Tok::LParen, i + 1); i += 1; }
                b')' => { push(&mut out, Tok::RParen, i + 1); i += 1; }
                b'{' => { push(&mut out, Tok::LBrace, i + 1); i += 1; }
                b'}' => { push(&mut out, Tok::RBrace, i + 1); i += 1; }
                b'<' => { push(&mut out, Tok::LAngle, i + 1); i += 1; }
                b'>' => { push(&mut out, Tok::RAngle, i + 1); i += 1; }
                b',' => { push(&mut out, Tok::Comma, i + 1); i += 1; }
                b'.' => { push(&mut out, Tok::Dot, i + 1); i += 1; }
                b'|' => { push(&mut out, Tok::Pipe, i + 1); i += 1; }
                b':' if i + 1 < b.len() && b[i + 1] == b':' => {
                    push(&mut out, Tok::DoubleColon, i + 2); i += 2;
                }
                b':' => { push(&mut out, Tok::Colon, i + 1); i += 1; }
                b'=' if i + 1 < b.len() && b[i + 1] == b'=' => {
                    push(&mut out, Tok::Op("==".into()), i + 2); i += 2;
                }
                b'=' if i + 1 < b.len() && b[i + 1] == b'>' => {
                    push(&mut out, Tok::FatArrow, i + 2); i += 2;
                }
                b'=' => { push(&mut out, Tok::Eq, i + 1); i += 1; }
                b'&' if i + 1 < b.len() && b[i + 1] == b'&' => {
                    push(&mut out, Tok::Op("&&".into()), i + 2); i += 2;
                }
                b'-' if i + 1 < b.len() && b[i + 1] == b'>' => {
                    push(&mut out, Tok::Arrow, i + 2); i += 2;
                }
                b'+' | b'-' | b'*' => {
                    push(&mut out, Tok::Op((c as char).to_string()), i + 1); i += 1;
                }
                _ => return Err((start, format!("unexpected character {:?}", c as char))),
            }
        }
    }
}
