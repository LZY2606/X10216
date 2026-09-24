use crate::ast::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Str(String),
    Lower(String),
    Upper(String),
    // keywords
    Let, Rec, Fn, Match, With, True, False,
    // punctuation
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Equals, Arrow, FatArrow, Pipe, Comma, Dot, Colon, Underscore, Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    pub errors: Vec<(Span, String)>,
}

fn is_ident_start(b: u8) -> bool {
    b == b'_' || b.is_ascii_alphabetic()
}
fn is_ident(b: u8) -> bool {
    b == b'_' || b.is_ascii_alphanumeric() || b == b'\''
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0, errors: Vec::new() }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn skip_ws(&mut self) {
        loop {
            while let Some(b) = self.peek() {
                if b.is_ascii_whitespace() {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if self.src.get(self.pos..self.pos + 2) == Some(b"--") {
                while let Some(b) = self.peek() {
                    self.pos += 1;
                    if b == b'\n' {
                        break;
                    }
                }
                continue;
            }
            break;
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_ws();
        let start = self.pos;
        let Some(b) = self.peek() else {
            return Token { tok: Tok::Eof, span: Span::new(start, start) };
        };
        if b.is_ascii_digit() {
            self.pos += 1;
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            let text = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
            let value: i64 = text.parse().unwrap_or(0);
            return Token { tok: Tok::Int(value), span: Span::new(start, self.pos) };
        }
        if is_ident_start(b) {
            self.pos += 1;
            while let Some(c) = self.peek() {
                if is_ident(c) {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            let text = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
            let tok = match text.as_str() {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "fn" => Tok::Fn,
                "match" => Tok::Match,
                "with" => Tok::With,
                "true" => Tok::True,
                "false" => Tok::False,
                "_" => Tok::Underscore,
                _ => {
                    if b.is_ascii_uppercase() {
                        Tok::Upper(text)
                    } else {
                        Tok::Lower(text)
                    }
                }
            };
            return Token { tok, span: Span::new(start, self.pos) };
        }
        if b == b'"' {
            self.pos += 1;
            let mut value = String::new();
            let mut closed = false;
            while let Some(c) = self.peek() {
                self.pos += 1;
                match c {
                    b'"' => {
                        closed = true;
                        break;
                    }
                    b'\\' => match self.peek() {
                        Some(b'n') => {
                            self.pos += 1;
                            value.push('\n');
                        }
                        Some(b't') => {
                            self.pos += 1;
                            value.push('\t');
                        }
                        Some(b'\\') => {
                            self.pos += 1;
                            value.push('\\');
                        }
                        Some(b'"') => {
                            self.pos += 1;
                            value.push('"');
                        }
                        Some(o) => {
                            self.pos += 1;
                            value.push(o as char);
                        }
                        None => break,
                    },
                    _ => value.push(c as char),
                }
            }
            if !closed {
                self.errors
                    .push((Span::new(start, self.pos), "unterminated string".to_string()));
            }
            return Token { tok: Tok::Str(value), span: Span::new(start, self.pos) };
        }
        let two = self.src.get(self.pos..self.pos + 2).copied();
        let mut take2 = |t: Tok| {
            self.pos += 2;
            Token { tok: t, span: Span::new(start, self.pos) }
        };
        let mut take1 = |t: Tok| {
            self.pos += 1;
            Token { tok: t, span: Span::new(start, self.pos) }
        };
        match two {
            Some(b"->") => return take2(Tok::Arrow),
            Some(b"=>") => return take2(Tok::FatArrow),
            _ => {}
        }
        match b {
            b'(' => take1(Tok::LParen),
            b')' => take1(Tok::RParen),
            b'{' => take1(Tok::LBrace),
            b'}' => take1(Tok::RBrace),
            b'[' => take1(Tok::LBracket),
            b']' => take1(Tok::RBracket),
            b'=' => take1(Tok::Equals),
            b'|' => take1(Tok::Pipe),
            b',' => take1(Tok::Comma),
            b'.' => take1(Tok::Dot),
            b':' => take1(Tok::Colon),
            _ => {
                self.pos += 1;
                self.errors
                    .push((Span::new(start, self.pos), format!("unexpected character {:?}", b as char)));
                self.next_token()
            }
        }
    }

    pub fn tokenize(mut self) -> (Vec<Token>, Vec<(Span, String)>) {
        let mut toks = Vec::new();
        loop {
            let t = self.next_token();
            let eof = t.tok == Tok::Eof;
            toks.push(t);
            if eof {
                break;
            }
        }
        (toks, self.errors)
    }
}
