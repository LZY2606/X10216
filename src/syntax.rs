// 词法与语法分析：产生带来源范围与稳定身份路径的 AST。
use serde::Serialize;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Serialize, PartialEq, Eq)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Span {
        Span { lo, hi }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span { lo: a.lo.min(b.lo), hi: a.hi.max(b.hi) }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Ident(String),
    // 关键字 / 标点以字符串形式存放
    Sym(String),
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}
fn is_ident_cont(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0 }
    }

    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).map(|b| *b as char)
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, ParseError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            if self.pos >= self.src.len() {
                break;
            }
            let lo = self.pos;
            let c = self.peek().unwrap();
            if c.is_ascii_digit() {
                let mut n = 0i64;
                while let Some(d) = self.peek() {
                    if let Some(v) = d.to_digit(10) {
                        n = n.wrapping_mul(10).wrapping_add(v as i64);
                        self.pos += d.len_utf8();
                    } else {
                        break;
                    }
                }
                out.push(Token { tok: Tok::Int(n), span: Span::new(lo, self.pos) });
            } else if is_ident_start(c) {
                while let Some(d) = self.peek() {
                    if is_ident_cont(d) {
                        self.pos += d.len_utf8();
                    } else {
                        break;
                    }
                }
                let s = std::str::from_utf8(&self.src[lo..self.pos]).unwrap().to_string();
                out.push(Token { tok: Tok::Ident(s), span: Span::new(lo, self.pos) });
            } else if c == '-' && self.src.get(self.pos + 1) == Some(&b'-') {
                while let Some(d) = self.peek() {
                    self.pos += d.len_utf8();
                    if d == '\n' {
                        break;
                    }
                }
            } else {
                let multi = ["->", "==", "|{", ".."];
                let mut matched: Option<&str> = None;
                for m in multi.iter() {
                    if self.src[self.pos..].starts_with(m.as_bytes()) {
                        matched = Some(m);
                        break;
                    }
                }
                if let Some(m) = matched {
                    self.pos += m.len();
                    out.push(Token { tok: Tok::Sym(m.to_string()), span: Span::new(lo, self.pos) });
                } else {
                    self.pos += c.len_utf8();
                    out.push(Token { tok: Tok::Sym(c.to_string()), span: Span::new(lo, self.pos) });
                }
            }
        }
        Ok(out)
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.pos += c.len_utf8();
            } else {
                break;
            }
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

