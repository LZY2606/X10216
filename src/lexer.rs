//! 手工词法分析器，产出带 Span 的 Token。

use crate::common::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Str,
    Ident(String),
    Upper(String),
    // 关键字
    Let, Rec, In, Fun, True, False, If, Then, Else, Case, Of, End,
    // 标点
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Eq, Arrow, Colon, Colons, Pipe, Semicolon, Backslash,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
    /// 字符串字面量的解码后内容（其它 token 为空）。
    pub text: String,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

fn is_ident_start(b: u8) -> bool {
    b == b'_' || (b'a'..=b'z').contains(&b)
}
fn is_ident_cont(b: u8) -> bool {
    is_ident_start(b) || (b'0'..=b'9').contains(&b)
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0 }
    }

    fn bump(&mut self) -> Option<u8> {
        let b = self.src.get(self.pos).copied()?;
        self.pos += 1;
        Some(b)
    }

    fn skip_ws(&mut self) {
        loop {
            match self.src.get(self.pos).copied() {
                Some(b) if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' => {
                    self.pos += 1;
                }
                Some(b'/') if self.src.get(self.pos + 1) == Some(&b'/') => {
                    while let Some(b) = self.src.get(self.pos).copied() {
                        self.pos += 1;
                        if b == b'\n' {
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, (String, Span)> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            let start = self.pos;
            let Some(b) = self.src.get(self.pos).copied() else {
                out.push(Token { tok: Tok::Eof, span: Span::new(start, start), text: String::new() });
                break;
            };
            if (b'0'..=b'9').contains(&b) {
                let mut n: i64 = 0;
                while let Some(c) = self.src.get(self.pos).copied() {
                    if (b'0'..=b'9').contains(&c) {
                        self.pos += 1;
                        n = n.saturating_mul(10).saturating_add((c - b'0') as i64);
                    } else {
                        break;
                    }
                }
                out.push(Token { tok: Tok::Int(n), span: Span::new(start, self.pos), text: String::new() });
            } else if is_ident_start(b) {
                self.pos += 1;
                while let Some(c) = self.src.get(self.pos).copied() {
                    if is_ident_cont(c) {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let s = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
                let tok = match s.as_str() {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "fun" => Tok::Fun,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "if" => Tok::If,
                    "then" => Tok::Then,
                    "else" => Tok::Else,
                    "case" => Tok::Case,
                    "of" => Tok::Of,
                    "end" => Tok::End,
                    _ => Tok::Ident(s),
                };
                out.push(Token { tok, span: Span::new(start, self.pos), text: String::new() });
            } else if (b'A'..=b'Z').contains(&b) {
                self.pos += 1;
                while let Some(c) = self.src.get(self.pos).copied() {
                    if is_ident_cont(c) || (b'A'..=b'Z').contains(&c) {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let s = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
                out.push(Token { tok: Tok::Upper(s), span: Span::new(start, self.pos), text: String::new() });
            } else if b == b'"' {
                self.pos += 1;
                let mut text = String::new();
                loop {
                    match self.bump() {
                        None => return Err(("未闭合的字符串".into(), Span::new(start, self.pos))),
                        Some(b'"') => break,
                        Some(b'\\') => match self.bump() {
                            Some(b'n') => text.push('\n'),
                            Some(b't') => text.push('\t'),
                            Some(b'"') => text.push('"'),
                            Some(b'\\') => text.push('\\'),
                            Some(c) => text.push(c as char),
                            None => return Err(("未闭合的字符串".into(), Span::new(start, self.pos))),
                        },
                        Some(c) => {
                            // UTF-8 安全回退
                            if c < 0x80 {
                                text.push(c as char);
                            } else {
                                self.pos -= 1;
                                let ch_start = self.pos;
                                self.pos += 1;
                                while self.src.get(self.pos).map_or(false, |x| x & 0xC0 == 0x80) {
                                    self.pos += 1;
                                }
                                if let Ok(s) = std::str::from_utf8(&self.src[ch_start..self.pos]) {
                                    text.push_str(s);
                                }
                            }
                        }
                    }
                }
                out.push(Token { tok: Tok::Str, span: Span::new(start, self.pos), text });
            } else {
                self.pos += 1;
                let (tok, len) = match b {
                    b'(' => (Tok::LParen, 1),
                    b')' => (Tok::RParen, 1),
                    b'{' => (Tok::LBrace, 1),
                    b'}' => (Tok::RBrace, 1),
                    b'[' => (Tok::LBracket, 1),
                    b']' => (Tok::RBracket, 1),
                    b',' => (Tok::Comma, 1),
                    b'.' => (Tok::Dot, 1),
                    b'|' => (Tok::Pipe, 1),
                    b';' => (Tok::Semicolon, 1),
                    b'\\' => (Tok::Backslash, 1),
                    b'=' => (Tok::Eq, 1),
                    b':' if self.src.get(self.pos) == Some(&b':') => {
                        self.pos += 1;
                        (Tok::Colons, 2)
                    }
                    b':' => (Tok::Colon, 1),
                    b'-' if self.src.get(self.pos) == Some(&b'>') => {
                        self.pos += 1;
                        (Tok::Arrow, 2)
                    }
                    _ => {
                        return Err((format!("无法识别的字符 {:?}", b as char), Span::new(start, start + 1)));
                    }
                };
                let _ = len;
                out.push(Token { tok, span: Span::new(start, self.pos), text: String::new() });
            }
        }
        Ok(out)
    }
}
