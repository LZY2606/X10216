use crate::ast::{Span, SrcError};

#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    Ident(String),
    Int(i64),
    True,
    False,
    Let,
    Rec,
    In,
    Fun,
    If,
    Then,
    Else,
    Match,
    With,
    Arrow,
    Bar,
    Hash,
    LBrace,
    RBrace,
    Eq,
    Comma,
    Dot,
    LParen,
    RParen,
    Plus,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

fn keyword(word: &str) -> Tok {
    match word {
        "let" => Tok::Let,
        "rec" => Tok::Rec,
        "in" => Tok::In,
        "fun" => Tok::Fun,
        "if" => Tok::If,
        "then" => Tok::Then,
        "else" => Tok::Else,
        "match" => Tok::Match,
        "with" => Tok::With,
        "true" => Tok::True,
        "false" => Tok::False,
        _ => Tok::Ident(word.to_string()),
    }
}

pub fn lex(src: &str) -> Result<Vec<Token>, SrcError> {
    let bytes = src.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i] as char;
        let start = i;
        let tok = match c {
            c if c.is_whitespace() => {
                i += 1;
                continue;
            }
            '/' if bytes.get(i + 1) == Some(&b'/') => {
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                }
                continue;
            }
            '0'..='9' => {
                while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                    i += 1;
                }
                let n: i64 = src[start..i].parse().map_err(|_| SrcError {
                    message: "整数过大".into(),
                    span: Span { start, end: i },
                })?;
                Tok::Int(n)
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                while i < bytes.len() {
                    let c = bytes[i] as char;
                    if c.is_ascii_alphanumeric() || c == '_' || c == '\'' {
                        i += 1;
                    } else {
                        break;
                    }
                }
                keyword(&src[start..i])
            }
            '-' if bytes.get(i + 1) == Some(&b'>') => {
                i += 2;
                Tok::Arrow
            }
            '|' => {
                i += 1;
                Tok::Bar
            }
            '#' => {
                i += 1;
                Tok::Hash
            }
            '{' => {
                i += 1;
                Tok::LBrace
            }
            '}' => {
                i += 1;
                Tok::RBrace
            }
            '=' => {
                i += 1;
                Tok::Eq
            }
            ',' => {
                i += 1;
                Tok::Comma
            }
            '.' => {
                i += 1;
                Tok::Dot
            }
            '(' => {
                i += 1;
                Tok::LParen
            }
            ')' => {
                i += 1;
                Tok::RParen
            }
            '+' => {
                i += 1;
                Tok::Plus
            }
            _ => {
                return Err(SrcError {
                    message: format!("无法识别的字符 {c:?}"),
                    span: Span { start, end: start + 1 },
                })
            }
        };
        tokens.push(Token {
            tok,
            span: Span { start, end: i },
        });
    }
    tokens.push(Token {
        tok: Tok::Eof,
        span: Span {
            start: src.len(),
            end: src.len(),
        },
    });
    Ok(tokens)
}
