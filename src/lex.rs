//! 词法分析：产生带来源范围的 token 流。

use crate::ast::Span;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Tok {
    Int(i64),
    Str(String),
    Ident(String),
    Tag(String),
    // 关键字
    Let,
    Rec,
    In,
    Fun,
    True,
    False,
    If,
    Then,
    Else,
    Match,
    With,
    // 标点
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Bar,
    Dot,
    Colon,
    Eq,
    Arrow,
    Semi,
    SemiSemi,
    DotDot,
    LBracket,
    RBracket,
    Eof,
}

#[derive(Clone)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

#[derive(Debug)]
pub struct LexError {
    pub span: Span,
    pub msg: String,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            src: src.as_bytes(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }
    fn peek2(&self) -> Option<u8> {
        self.src.get(self.pos + 1).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn skip_trivia(&mut self) {
        loop {
            match self.peek() {
                Some(b) if b.is_ascii_whitespace() => {
                    self.pos += 1;
                }
                Some(b'(') if self.peek2() == Some(b'*') => {
                    let start = self.pos;
                    self.pos += 2;
                    let mut depth = 1usize;
                    while depth > 0 {
                        match (self.peek(), self.peek2()) {
                            (Some(b'('), Some(b'*')) => {
                                depth += 1;
                                self.pos += 2;
                            }
                            (Some(b'*'), Some(b')')) => {
                                depth -= 1;
                                self.pos += 2;
                            }
                            (Some(_), _) => {
                                self.pos += 1;
                            }
                            (None, _) => {
                                self.pos = start;
                                // 未闭合注释：保留 (* 给后续报错
                                break;
                            }
                        }
                    }
                }
                _ => break,
            }
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            self.skip_trivia();
            let lo = self.pos;
            let Some(c) = self.peek() else {
                out.push(Token {
                    tok: Tok::Eof,
                    span: Span::new(lo, lo),
                });
                return Ok(out);
            };
            let tok = if c.is_ascii_digit() {
                let mut n: i64 = 0;
                while let Some(d) = self.peek()
                    && d.is_ascii_digit()
                {
                    n = n
                        .saturating_mul(10)
                        .saturating_add((d - b'0') as i64);
                    self.bump();
                }
                Tok::Int(n)
            } else if c == b'_' || c.is_ascii_alphabetic() {
                while let Some(a) = self.peek()
                    && (a == b'_' || a.is_ascii_alphanumeric())
                {
                    self.bump();
                }
                let s = std::str::from_utf8(&self.src[lo..self.pos]).unwrap().to_string();
                match s.as_str() {
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
                    _ => Tok::Ident(s),
                }
            } else if c == b'"' {
                self.bump();
                let mut s = String::new();
                loop {
                    match self.bump() {
                        None => {
                            return Err(LexError {
                                span: Span::new(lo, self.pos),
                                msg: "字符串未闭合".into(),
                            });
                        }
                        Some(b'"') => break,
                        Some(b'\\') => match self.bump() {
                            Some(b'n') => s.push('\n'),
                            Some(b't') => s.push('\t'),
                            Some(b'"') => s.push('"'),
                            Some(b'\\') => s.push('\\'),
                            Some(other) => s.push(other as char),
                            None => {
                                return Err(LexError {
                                    span: Span::new(lo, self.pos),
                                    msg: "字符串转义未结束".into(),
                                })
                            }
                        },
                        Some(b) => {
                            // 收集完整 UTF-8 序列
                            let width = if b < 0x80 {
                                1
                            } else if b >> 5 == 0b110 {
                                2
                            } else if b >> 4 == 0b1110 {
                                3
                            } else {
                                4
                            };
                            let start = self.pos - 1;
                            for _ in 1..width {
                                self.bump();
                            }
                            s.push_str(
                                std::str::from_utf8(&self.src[start..self.pos]).unwrap_or(""),
                            );
                        }
                    }
                }
                Tok::Str(s)
            } else if c == b'#' {
                self.bump();
                let start = self.pos;
                while let Some(a) = self.peek()
                    && (a == b'_' || a.is_ascii_alphanumeric())
                {
                    self.bump();
                }
                if start == self.pos {
                    return Err(LexError {
                        span: Span::new(lo, self.pos),
                        msg: "# 后需要变体标签名".into(),
                    });
                }
                Tok::Tag(
                    std::str::from_utf8(&self.src[start..self.pos])
                        .unwrap()
                        .to_string(),
                )
            } else {
                self.bump();
                match c {
                    b'(' => Tok::LParen,
                    b')' => Tok::RParen,
                    b'{' => Tok::LBrace,
                    b'}' => Tok::RBrace,
                    b'[' => Tok::LBracket,
                    b']' => Tok::RBracket,
                    b',' => Tok::Comma,
                    b'|' => Tok::Bar,
                    b';' => {
                        if self.peek() == Some(b';') {
                            self.bump();
                            Tok::SemiSemi
                        } else {
                            Tok::Semi
                        }
                    }
                    b'.' => {
                        if self.peek() == Some(b'.') {
                            self.bump();
                            Tok::DotDot
                        } else {
                            Tok::Dot
                        }
                    }
                    b':' => Tok::Colon,
                    b'=' => Tok::Eq,
                    b'-' if self.peek() == Some(b'>') => {
                        self.bump();
                        Tok::Arrow
                    }
                    other => {
                        return Err(LexError {
                            span: Span::new(lo, self.pos),
                            msg: format!("无法识别的字符 {:?}", other as char),
                        });
                    }
                }
            };
            let hi = self.pos;
            out.push(Token {
                tok,
                span: Span::new(lo, hi),
            });
        }
    }
}
