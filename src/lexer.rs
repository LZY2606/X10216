use crate::span::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Lower(String),
    Upper(String),
    Label(String),
    Let,
    Rec,
    In,
    Fun,
    True,
    False,
    If,
    Then,
    Else,
    Record,
    Variant,
    Match,
    With,
    Unit,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Eq,
    Colon,
    Dot,
    DArrow,
    Pipe,
    Star,
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

#[derive(Debug)]
pub struct LexError {
    pub msg: String,
    pub pos: usize,
}

fn is_ident_start(b: u8) -> bool {
    b == b'_' || (b >= b'a' && b <= b'z') || (b >= b'A' && b <= b'Z')
}
fn is_ident_cont(b: u8) -> bool {
    b == b'_' || b == b'\'' || (b >= b'a' && b <= b'z') || (b >= b'A' && b <= b'Z')
        || (b >= b'0' && b <= b'9')
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            src: src.as_bytes(),
            pos: 0,
        }
    }

    fn bump(&mut self) -> Option<u8> {
        let b = self.src.get(self.pos).copied();
        if b.is_some() {
            self.pos += 1;
        }
        b
    }

    fn skip_ws(&mut self) {
        loop {
            while let Some(&b) = self.src.get(self.pos) {
                if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if self.src.get(self.pos..self.pos + 2) == Some(b"--") {
                while let Some(b) = self.src.get(self.pos) {
                    self.pos += 1;
                    if *b == b'\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            let start = self.pos;
            let Some(b) = self.bump() else {
                out.push(Token {
                    tok: Tok::Eof,
                    span: Span::new(start, start),
                });
                return Ok(out);
            };
            let make = |tok: Tok, end: usize| Token {
                tok,
                span: Span::new(start, end),
            };
            match b {
                b'(' => out.push(make(Tok::LParen, self.pos)),
                b')' => out.push(make(Tok::RParen, self.pos)),
                b'{' => out.push(make(Tok::LBrace, self.pos)),
                b'}' => out.push(make(Tok::RBrace, self.pos)),
                b'[' => out.push(make(Tok::LBracket, self.pos)),
                b']' => out.push(make(Tok::RBracket, self.pos)),
                b',' => out.push(make(Tok::Comma, self.pos)),
                b'=' => out.push(make(Tok::Eq, self.pos)),
                b':' => out.push(make(Tok::Colon, self.pos)),
                b'.' => out.push(make(Tok::Dot, self.pos)),
                b'|' => out.push(make(Tok::Pipe, self.pos)),
                b'*' => out.push(make(Tok::Star, self.pos)),
                b'-' if self.src.get(self.pos) == Some(&b'>') => {
                    self.pos += 1;
                    out.push(make(Tok::DArrow, self.pos));
                }
                b'0'..=b'9' => {
                    while let Some(&c) = self.src.get(self.pos) {
                        if c.is_ascii_digit() {
                            self.pos += 1;
                        } else {
                            break;
                        }
                    }
                    let text = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
                    let n: i64 = text.parse().map_err(|_| LexError {
                        msg: "integer literal out of range".into(),
                        pos: start,
                    })?;
                    out.push(make(Tok::Int(n), self.pos));
                }
                b'#' => {
                    while let Some(&c) = self.src.get(self.pos) {
                        if is_ident_cont(c) {
                            self.pos += 1;
                        } else {
                            break;
                        }
                    }
                    let label = std::str::from_utf8(&self.src[start + 1..self.pos])
                        .unwrap()
                        .to_string();
                    if label.is_empty() {
                        return Err(LexError {
                            msg: "empty label".into(),
                            pos: start,
                        });
                    }
                    out.push(make(Tok::Label(label), self.pos));
                }
                b if is_ident_start(b) => {
                    while let Some(&c) = self.src.get(self.pos) {
                        if is_ident_cont(c) {
                            self.pos += 1;
                        } else {
                            break;
                        }
                    }
                    let text = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
                    let tok = match text {
                        "let" => Tok::Let,
                        "rec" => Tok::Rec,
                        "in" => Tok::In,
                        "fun" => Tok::Fun,
                        "true" => Tok::True,
                        "false" => Tok::False,
                        "if" => Tok::If,
                        "then" => Tok::Then,
                        "else" => Tok::Else,
                        "record" => Tok::Record,
                        "variant" => Tok::Variant,
                        "match" => Tok::Match,
                        "with" => Tok::With,
                        "unit" => Tok::Unit,
                        _ if b.is_ascii_uppercase() => Tok::Upper(text.to_string()),
                        _ => Tok::Lower(text.to_string()),
                    };
                    out.push(make(tok, self.pos));
                }
                _ => {
                    return Err(LexError {
                        msg: format!("unexpected character {:?}", b as char),
                        pos: start,
                    });
                }
            }
        }
    }
}
