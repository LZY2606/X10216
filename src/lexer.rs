//! Hand-written lexer producing tokens with byte spans.

use crate::types::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Str(String),
    Ident(String),
    // keywords
    Let, Rec, In, Fun, True, False, If, Then, Else, Match, With,
    // punctuation
    LParen, RParen, LBrace, RBrace, LBracket, RBracket, Bar, Arrow, FatArrow,
    Eq, Colon, Comma, Semicolon, Backtick, Dot, Star, Plus, Minus, Slash, Lt,
    And, Or, Not, Backslash, Underscore, Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub msg: String,
    pub span: Span,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0 }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }
    fn peek2(&self) -> Option<u8> {
        self.src.get(self.pos + 1).copied()
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                b' ' | b'\t' | b'\r' | b'\n' => self.pos += 1,
                b'/' if self.peek2() == Some(b'/') => {
                    while let Some(c) = self.peek() {
                        self.pos += 1;
                        if c == b'\n' { break; }
                    }
                }
                _ => break,
            }
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            let start = self.pos;
            let Some(c) = self.peek() else {
                out.push(Token { tok: Tok::Eof, span: Span::new(start, start) });
                return Ok(out);
            };
            let tok = match c {
                b'0'..=b'9' => {
                    let mut n = 0i64;
                    while let Some(d) = self.peek()
                        && d.is_ascii_digit()
                    {
                        n = n.saturating_mul(10).saturating_add((d - b'0') as i64);
                        self.pos += 1;
                    }
                    Tok::Int(n)
                }
                b'"' => {
                    self.pos += 1;
                    let mut s = String::new();
                    loop {
                        match self.peek() {
                            None => return Err(LexError {
                                msg: "unterminated string".into(),
                                span: Span::new(start, self.pos),
                            }),
                            Some(b'"') => { self.pos += 1; break; }
                            Some(b'\\') => {
                                self.pos += 1;
                                let e = self.peek().ok_or(LexError {
                                    msg: "bad escape".into(),
                                    span: Span::new(start, self.pos),
                                })?;
                                let mapped = match e {
                                    b'n' => '\n', b't' => '\t', b'r' => '\r',
                                    b'"' => '"', b'\\' => '\\',
                                    _ => e as char,
                                };
                                s.push(mapped);
                                self.pos += 1;
                            }
                            Some(c2) => { s.push(c2 as char); self.pos += 1; }
                        }
                    }
                    Tok::Str(s)
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    while let Some(c2) = self.peek()
                        && (c2.is_ascii_alphanumeric() || c2 == b'_')
                    {
                        self.pos += 1;
                    }
                    let word = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
                    match word {
                        "let" => Tok::Let,
                        "rec" => Tok::Rec,
                        "in" => Tok::In,
                        "fun" => Tok::Fun,
                        "true" => Tok::True,
                        "false" => Tok::False,
                        "if" => Tok::If,
                        "then" => Tok::Then,
                        "else" => Tok::Else,
                        "match" => Tok::Match,
                        "with" => Tok::With,
                        _ => Tok::Ident(word.to_string()),
                    }
                }
                b'`' => { self.pos += 1; Tok::Backtick }
                b'(' => { self.pos += 1; Tok::LParen }
                b')' => { self.pos += 1; Tok::RParen }
                b'{' => { self.pos += 1; Tok::LBrace }
                b'}' => { self.pos += 1; Tok::RBrace }
                b'[' => { self.pos += 1; Tok::LBracket }
                b']' => { self.pos += 1; Tok::RBracket }
                b'|' => {
                    self.pos += 1;
                    if self.peek() == Some(b']') { self.pos += 1; Tok::RBracket }
                    else if self.peek() == Some(b'|') { self.pos += 1; Tok::Or }
                    else { Tok::Bar }
                }
                b'-' => {
                    self.pos += 1;
                    if self.peek() == Some(b'>') { self.pos += 1; Tok::Arrow }
                    else { Tok::Minus }
                }
                b'=' => {
                    self.pos += 1;
                    if self.peek() == Some(b'>') { self.pos += 1; Tok::FatArrow }
                    else { Tok::Eq }
                }
                b':' => { self.pos += 1; Tok::Colon }
                b',' => { self.pos += 1; Tok::Comma }
                b';' => { self.pos += 1; Tok::Semicolon }
                b'.' => { self.pos += 1; Tok::Dot }
                b'*' => { self.pos += 1; Tok::Star }
                b'+' => { self.pos += 1; Tok::Plus }
                b'/' => { self.pos += 1; Tok::Slash }
                b'<' => { self.pos += 1; Tok::Lt }
                b'&' if self.peek2() == Some(b'&') => { self.pos += 2; Tok::And }
                b'!' => { self.pos += 1; Tok::Not }
                b'\\' => { self.pos += 1; Tok::Backslash }
                b'_' => {
                    self.pos += 1;
                    while let Some(c2) = self.peek()
                        && (c2.is_ascii_alphanumeric() || c2 == b'_')
                    {
                        self.pos += 1;
                    }
                    if self.pos == start + 1 { Tok::Underscore }
                    else {
                        let word = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
                        Tok::Ident(word.to_string())
                    }
                }
                _ => return Err(LexError {
                    msg: format!("unexpected character {:?}", c as char),
                    span: Span::new(start, start + 1),
                }),
            };
            out.push(Token { tok, span: Span::new(start, self.pos) });
        }
    }
}
