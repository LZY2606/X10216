//! Hand-written lexer for the slice language.

use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    // literals
    Int(i64),
    Bool(bool),
    Str(String),
    // identifiers
    Lower(String),
    Upper(String),
    // keywords
    Let, Rec, In, Fn, If, Then, Else, Match, With, True, False,
    // punctuation
    LParen, RParen, LBrace, RBrace, Comma, Dot, Colon, Eq, Arrow, FatArrow,
    Pipe, Backslash, Semicolon, Plus, Minus, Star, Slash, Lt, Gt, Le, Ge, Neq,
    And, Or,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub msg: String,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let bytes = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < bytes.len() {
        let b = bytes[i];
        match b {
            b' ' | b'\t' | b'\r' | b'\n' => i += 1,
            b'(' => { out.push(one(Tok::LParen, i, 1)); i += 1; }
            b')' => { out.push(one(Tok::RParen, i, 1)); i += 1; }
            b'{' => { out.push(one(Tok::LBrace, i, 1)); i += 1; }
            b'}' => { out.push(one(Tok::RBrace, i, 1)); i += 1; }
            b',' => { out.push(one(Tok::Comma, i, 1)); i += 1; }
            b'.' => { out.push(one(Tok::Dot, i, 1)); i += 1; }
            b';' => { out.push(one(Tok::Semicolon, i, 1)); i += 1; }
            b'+' => { out.push(one(Tok::Plus, i, 1)); i += 1; }
            b'-' if bytes.get(i + 1) == Some(&b'>') => {
                out.push(one(Tok::Arrow, i, 2)); i += 2;
            }
            b'-' => { out.push(one(Tok::Minus, i, 1)); i += 1; }
            b'*' => { out.push(one(Tok::Star, i, 1)); i += 1; }
            b'/' if bytes.get(i + 1) == Some(&b'/') => {
                while i < bytes.len() && bytes[i] != b'\n' { i += 1; }
            }
            b'/' => { out.push(one(Tok::Slash, i, 1)); i += 1; }
            b'\\' => { out.push(one(Tok::Backslash, i, 1)); i += 1; }
            b'|' if bytes.get(i + 1) == Some(&b'|') => {
                out.push(one(Tok::Or, i, 2)); i += 2;
            }
            b'|' => { out.push(one(Tok::Pipe, i, 1)); i += 1; }
            b'=' if bytes.get(i + 1) == Some(&b'>') => {
                out.push(one(Tok::FatArrow, i, 2)); i += 2;
            }
            b'=' if bytes.get(i + 1) == Some(&b'=') => {
                out.push(one(Tok::Eq, i, 2)); i += 2;
            }
            b'=' => { out.push(one(Tok::Eq, i, 1)); i += 1; }
            b'!' if bytes.get(i + 1) == Some(&b'=') => {
                out.push(one(Tok::Neq, i, 2)); i += 2;
            }
            b'<' if bytes.get(i + 1) == Some(&b'=') => {
                out.push(one(Tok::Le, i, 2)); i += 2;
            }
            b'<' if bytes.get(i + 1) == Some(&b'&') && bytes.get(i + 2) == Some(&b'-') => {
                // backwards-friendly noop, not part of language
                return Err(LexError { msg: "unexpected character '<'".into(), span: Span::new(i, i + 1) });
            }
            b'<' => { out.push(one(Tok::Lt, i, 1)); i += 1; }
            b'>' if bytes.get(i + 1) == Some(&b'=') => {
                out.push(one(Tok::Ge, i, 2)); i += 2;
            }
            b'>' => { out.push(one(Tok::Gt, i, 1)); i += 1; }
            b'&' if bytes.get(i + 1) == Some(&b'&') => {
                out.push(one(Tok::And, i, 2)); i += 2;
            }
            b':' => { out.push(one(Tok::Colon, i, 1)); i += 1; }
            b'"' => {
                let start = i;
                i += 1;
                let mut s = String::new();
                while i < bytes.len() && bytes[i] != b'"' {
                    if bytes[i] == b'\\' {
                        i += 1;
                        if i >= bytes.len() { break; }
                        match bytes[i] {
                            b'n' => s.push('\n'),
                            b't' => s.push('\t'),
                            b'\\' => s.push('\\'),
                            b'"' => s.push('"'),
                            c => s.push(c as char),
                        }
                        i += 1;
                    } else {
                        // collect a full utf-8 char
                        let ch_start = i;
                        i += 1;
                        while i < bytes.len() && (bytes[i] & 0b1100_0000) == 0b1000_0000 {
                            i += 1;
                        }
                        s.push_str(&src[ch_start..i]);
                    }
                }
                if i >= bytes.len() {
                    return Err(LexError { msg: "unterminated string".into(), span: Span::new(start, bytes.len()) });
                }
                i += 1;
                out.push(Token { tok: Tok::Str(s), span: Span::new(start, i) });
            }
            b'0'..=b'9' => {
                let start = i;
                while i < bytes.len() && bytes[i].is_ascii_digit() { i += 1; }
                let n: i64 = src[start..i].parse().map_err(|_| LexError {
                    msg: "invalid integer literal".into(),
                    span: Span::new(start, i),
                })?;
                out.push(Token { tok: Tok::Int(n), span: Span::new(start, i) });
            }
            c if c.is_ascii_alphabetic() || c == b'_' => {
                let start = i;
                while i < bytes.len()
                    && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_')
                {
                    i += 1;
                }
                let word = &src[start..i];
                let tok = match word {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "fn" => Tok::Fn,
                    "if" => Tok::If,
                    "then" => Tok::Then,
                    "else" => Tok::Else,
                    "match" => Tok::Match,
                    "with" => Tok::With,
                    "true" => Tok::Bool(true),
                    "false" => Tok::Bool(false),
                    _ if (c as char).is_ascii_uppercase() => Tok::Upper(word.to_string()),
                    _ => Tok::Lower(word.to_string()),
                };
                out.push(Token { tok, span: Span::new(start, i) });
            }
            _ => {
                let ch_start = i;
                i += 1;
                while i < bytes.len() && (bytes[i] & 0b1100_0000) == 0b1000_0000 {
                    i += 1;
                }
                return Err(LexError {
                    msg: format!("unexpected character {:?}", &src[ch_start..i]),
                    span: Span::new(ch_start, i),
                });
            }
        }
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(bytes.len(), bytes.len()) });
    Ok(out)
}

fn one(tok: Tok, start: usize, len: usize) -> Token {
    Token { tok, span: Span::new(start, start + len) }
}
