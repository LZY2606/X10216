use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Str(String),
    Ident(String),
    Upper(String),
    // punctuation / keywords
    Let, Rec, In, Fn, If, Then, Else, Match, With, Underscore,
    Arrow, Eq, FatArrow, Backslash, Dot, Comma, Semicolon,
    LParen, RParen, LBrace, RBrace,
    Backtick(String),
    Eof,
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
    pub span: Span,
    pub msg: String,
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
    fn bump(&mut self) -> Option<u8> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn skip_ws(&mut self) {
        loop {
            match self.peek() {
                Some(b) if b.is_ascii_whitespace() => {
                    self.pos += 1;
                }
                Some(b'/') if self.peek2() == Some(b'/') => {
                    while let Some(c) = self.peek() {
                        if c == b'\n' {
                            break;
                        }
                        self.pos += 1;
                    }
                }
                Some(b'/') if self.peek2() == Some(b'*') => {
                    self.pos += 2;
                    let mut depth = 1;
                    while depth > 0 {
                        match (self.peek(), self.peek2()) {
                            (Some(b'/'), Some(b'*')) => {
                                depth += 1;
                                self.pos += 2;
                            }
                            (Some(b'*'), Some(b'/')) => {
                                depth -= 1;
                                self.pos += 2;
                            }
                            (Some(_), _) => self.pos += 1,
                            _ => break,
                        }
                    }
                }
                _ => break,
            }
        }
    }

    fn is_ident_start(c: u8) -> bool {
        c == b'_' || c.is_ascii_lowercase()
    }
    fn is_ident_cont(c: u8) -> bool {
        c == b'_' || c.is_ascii_alphanumeric() || c == b'\''
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            let lo = self.pos;
            let Some(c) = self.peek() else {
                out.push(Token { tok: Tok::Eof, span: Span::new(lo, lo) });
                break;
            };
            if c.is_ascii_digit() {
                let mut n: i64 = 0;
                while let Some(d) = self.peek() {
                    if d.is_ascii_digit() {
                        n = n.saturating_mul(10).saturating_add((d - b'0') as i64);
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                out.push(Token { tok: Tok::Int(n), span: Span::new(lo, self.pos) });
                continue;
            }
            if c == b'"' {
                self.pos += 1;
                let mut s = String::new();
                loop {
                    match self.bump() {
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
                                    msg: "字符串未闭合".to_string(),
                                })
                            }
                        },
                        Some(ch) => s.push(ch as char),
                        None => {
                            return Err(LexError {
                                span: Span::new(lo, self.pos),
                                msg: "字符串未闭合".to_string(),
                            })
                        }
                    }
                }
                out.push(Token { tok: Tok::Str(s), span: Span::new(lo, self.pos) });
                continue;
            }
            if c == b'`' {
                self.pos += 1;
                let start = self.pos;
                while let Some(ch) = self.peek() {
                    if ch == b'`' {
                        break;
                    }
                    self.pos += 1;
                }
                let name = std::str::from_utf8(&self.src[start..self.pos]).unwrap_or("").to_string();
                if self.peek() != Some(b'`') {
                    return Err(LexError { span: Span::new(lo, self.pos), msg: "反引号标识符未闭合".to_string() });
                }
                self.pos += 1;
                out.push(Token { tok: Tok::Backtick(name), span: Span::new(lo, self.pos) });
                continue;
            }
            if Self::is_ident_start(c) {
                let start = self.pos;
                while let Some(ch) = self.peek() {
                    if Self::is_ident_cont(ch) {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let word = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
                let tok = match word.as_str() {
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
                    "_" => Tok::Underscore,
                    _ => Tok::Ident(word),
                };
                out.push(Token { tok, span: Span::new(lo, self.pos) });
                continue;
            }
            if c.is_ascii_uppercase() {
                let start = self.pos;
                while let Some(ch) = self.peek() {
                    if Self::is_ident_cont(ch) {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let word = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
                out.push(Token { tok: Tok::Upper(word), span: Span::new(lo, self.pos) });
                continue;
            }
            macro_rules! punct {
                ($t:expr) => {{
                    self.pos += 1;
                    out.push(Token { tok: $t, span: Span::new(lo, self.pos) });
                    continue;
                }};
            }
            match c {
                b'-' if self.peek2() == Some(b'>') => {
                    self.pos += 2;
                    out.push(Token { tok: Tok::Arrow, span: Span::new(lo, self.pos) });
                    continue;
                }
                b'=' if self.peek2() == Some(b'>') => {
                    self.pos += 2;
                    out.push(Token { tok: Tok::FatArrow, span: Span::new(lo, self.pos) });
                    continue;
                }
                b'=' => punct!(Tok::Eq),
                b'\\' => punct!(Tok::Backslash),
                b'.' => punct!(Tok::Dot),
                b',' => punct!(Tok::Comma),
                b';' => punct!(Tok::Semicolon),
                b'(' => punct!(Tok::LParen),
                b')' => punct!(Tok::RParen),
                b'{' => punct!(Tok::LBrace),
                b'}' => punct!(Tok::RBrace),
                _ => {
                    return Err(LexError {
                        span: Span::new(lo, lo + 1),
                        format!("无法识别的字符 {:?}", c as char),
                    })
                }
            }
        }
        Ok(out)
    }
}
