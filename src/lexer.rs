//! 词法分析。

use crate::span::Span;
use serde::Serialize;

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Tok {
    // 字面量 / 名字
    Int(i64),
    Str(String),
    LIdent(String),
    UIdent(String),
    // 关键字
    Let, Rec, In, Fun, True, False, Match, With, Unit,
    // 标点
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Eq, Colon, Arrow, Pipe, Underscore, Semicolon,
    // 运算符
    Plus, Minus, Star, Slash, Lt, Gt, Leq, Geq, EqEq, NotEq, AndAnd, OrOr,
    // 行标注
    DotDot,
    Eof,
}

#[derive(Clone, Debug, Serialize)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub struct LexError {
    pub msg: String,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let n = b.len();
    macro_rules! sp {
        ($s:expr) => {
            Span::new($s, i)
        };
    }
    while i < n {
        let s = i;
        let c = b[i] as char;
        match c {
            ' ' | '\t' | '\r' | '\n' => {
                i += c.len_utf8();
            }
            '(' => {
                i += 1;
                out.push(Token { tok: Tok::LParen, span: sp!(s) });
            }
            ')' => {
                i += 1;
                out.push(Token { tok: Tok::RParen, span: sp!(s) });
            }
            '{' => {
                i += 1;
                out.push(Token { tok: Tok::LBrace, span: sp!(s) });
            }
            '}' => {
                i += 1;
                out.push(Token { tok: Tok::RBrace, span: sp!(s) });
            }
            '[' => {
                i += 1;
                out.push(Token { tok: Tok::LBracket, span: sp!(s) });
            }
            ']' => {
                i += 1;
                out.push(Token { tok: Tok::RBracket, span: sp!(s) });
            }
            ',' => {
                i += 1;
                out.push(Token { tok: Tok::Comma, span: sp!(s) });
            }
            ';' => {
                i += 1;
                out.push(Token { tok: Tok::Semicolon, span: sp!(s) });
            }
            '|' => {
                i += 1;
                if i < n && b[i] == b'|' {
                    i += 1;
                    out.push(Token { tok: Tok::OrOr, span: sp!(s) });
                } else {
                    out.push(Token { tok: Tok::Pipe, span: sp!(s) });
                }
            }
            '&' if i + 1 < n && b[i + 1] == b'&' => {
                i += 2;
                out.push(Token { tok: Tok::AndAnd, span: sp!(s) });
            }
            '.' => {
                i += 1;
                if i < n && b[i] == b'.' {
                    i += 1;
                    out.push(Token { tok: Tok::DotDot, span: sp!(s) });
                } else {
                    out.push(Token { tok: Tok::Dot, span: sp!(s) });
                }
            }
            '=' => {
                i += 1;
                if i < n && b[i] == b'=' {
                    i += 1;
                    out.push(Token { tok: Tok::EqEq, span: sp!(s) });
                } else {
                    out.push(Token { tok: Tok::Eq, span: sp!(s) });
                }
            }
            '!' if i + 1 < n && b[i + 1] == b'=' => {
                i += 2;
                out.push(Token { tok: Tok::NotEq, span: sp!(s) });
            }
            '<' => {
                i += 1;
                if i < n && b[i] == b'=' {
                    i += 1;
                    out.push(Token { tok: Tok::Leq, span: sp!(s) });
                } else {
                    out.push(Token { tok: Tok::Lt, span: sp!(s) });
                }
            }
            '>' => {
                i += 1;
                if i < n && b[i] == b'=' {
                    i += 1;
                    out.push(Token { tok: Tok::Geq, span: sp!(s) });
                } else {
                    out.push(Token { tok: Tok::Gt, span: sp!(s) });
                }
            }
            ':' => {
                i += 1;
                out.push(Token { tok: Tok::Colon, span: sp!(s) });
            }
            '-' if i + 1 < n && b[i + 1] == b'>' => {
                i += 2;
                out.push(Token { tok: Tok::Arrow, span: sp!(s) });
            }
            '+' => {
                i += 1;
                out.push(Token { tok: Tok::Plus, span: sp!(s) });
            }
            '-' => {
                i += 1;
                out.push(Token { tok: Tok::Minus, span: sp!(s) });
            }
            '*' => {
                i += 1;
                out.push(Token { tok: Tok::Star, span: sp!(s) });
            }
            '/' if i + 1 < n && b[i + 1] == b'/' => {
                while i < n && b[i] != b'\n' {
                    i += 1;
                }
            }
            '/' => {
                i += 1;
                out.push(Token { tok: Tok::Slash, span: sp!(s) });
            }
            '_' => {
                i += 1;
                out.push(Token { tok: Tok::Underscore, span: sp!(s) });
            }
            '"' => {
                i += 1;
                let mut val = String::new();
                loop {
                    if i >= n {
                        return Err(LexError { msg: "字符串未闭合".into(), span: Span::new(s, n) });
                    }
                    let ch = b[i] as char;
                    if ch == '"' {
                        i += 1;
                        break;
                    } else if ch == '\\' {
                        i += 1;
                        if i >= n {
                            return Err(LexError { msg: "字符串转义未完成".into(), span: Span::new(s, i) });
                        }
                        let e = b[i] as char;
                        match e {
                            'n' => val.push('\n'),
                            't' => val.push('\t'),
                            'r' => val.push('\r'),
                            '\\' => val.push('\\'),
                            '"' => val.push('"'),
                            _ => val.push(e),
                        }
                        i += 1;
                    } else {
                        val.push(ch);
                        i += ch.len_utf8();
                    }
                }
                out.push(Token { tok: Tok::Str(val), span: sp!(s) });
            }
            '0'..='9' => {
                let start = i;
                while i < n && b[i].is_ascii_digit() {
                    i += 1;
                }
                let txt = &src[start..i];
                let v: i64 = txt.parse().map_err(|_| LexError {
                    msg: "整数字面量越界".into(),
                    span: Span::new(start, i),
                })?;
                out.push(Token { tok: Tok::Int(v), span: sp!(s) });
            }
            c if c.is_alphabetic() || c == '_' => {
                let start = i;
                while i < n {
                    let ch = b[i] as char;
                    if ch.is_alphanumeric() || ch == '_' {
                        i += ch.len_utf8();
                    } else {
                        break;
                    }
                }
                let word = &src[start..i];
                let tok = match word {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "fun" => Tok::Fun,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "match" => Tok::Match,
                    "with" => Tok::With,
                    "unit" => Tok::Unit,
                    _ if c.is_uppercase() => Tok::UIdent(word.to_string()),
                    _ => Tok::LIdent(word.to_string()),
                };
                out.push(Token { tok, span: sp!(s) });
            }
            other => {
                return Err(LexError {
                    msg: format!("无法识别的字符 {other:?}"),
                    span: Span::new(s, s + c.len_utf8()),
                });
            }
        }
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(n, n) });
    Ok(out)
}
