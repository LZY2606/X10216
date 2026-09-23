use crate::span::Span;
use serde::Serialize;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum TokK {
    Int(i64),
    Str(String),
    Lower(String),
    Upper(String),
    Let,
    Rec,
    In,
    Fun,
    With,
    True,
    False,
    Unit,
    Arrow,
    EqEq,
    Plus,
    Eq,
    Dot,
    Comma,
    Colon,
    Semi,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    Eof,
}

#[derive(Clone, Debug, Serialize)]
pub struct Tok {
    pub k: TokK,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub struct LexError {
    pub span: Span,
    pub msg: String,
}

fn keyword(s: &str) -> Option<TokK> {
    Some(match s {
        "let" => TokK::Let,
        "rec" => TokK::Rec,
        "in" => TokK::In,
        "fun" => TokK::Fun,
        "with" => TokK::With,
        "true" => TokK::True,
        "false" => TokK::False,
        "unit" => TokK::Unit,
        _ => return None,
    })
}

pub fn lex(src: &str) -> Result<Vec<Tok>, LexError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        let start = i;
        if c.is_ascii_digit() {
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[start..i].parse().map_err(|_| LexError {
                span: Span::new(start, i),
                msg: "整数字面量越界".into(),
            })?;
            out.push(Tok { k: TokK::Int(n), span: Span::new(start, i) });
            continue;
        }
        if c == '"' {
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    s.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(LexError {
                    span: Span::new(start, b.len()),
                    msg: "字符串缺少结尾引号".into(),
                });
            }
            i += 1;
            out.push(Tok { k: TokK::Str(s), span: Span::new(start, i) });
            continue;
        }
        if c == '_' || c.is_alphabetic() {
            while i < b.len() {
                let ch = b[i] as char;
                if ch == '_' || ch.is_alphanumeric() {
                    i += 1;
                } else {
                    break;
                }
            }
            let word = &src[start..i];
            let k = keyword(word).unwrap_or_else(|| {
                if c.is_ascii_uppercase() {
                    TokK::Upper(word.to_string())
                } else {
                    TokK::Lower(word.to_string())
                }
            });
            out.push(Tok { k, span: Span::new(start, i) });
            continue;
        }
        macro_rules! one {
            ($k:expr) => {{
                i += 1;
                out.push(Tok { k: $k, span: Span::new(start, i) });
                continue;
            }};
        }
        match c {
            '-' if i + 1 < b.len() && b[i + 1] == b'>' => {
                i += 2;
                out.push(Tok { k: TokK::Arrow, span: Span::new(start, i) });
                continue;
            }
            '=' if i + 1 < b.len() && b[i + 1] == b'=' => {
                i += 2;
                out.push(Tok { k: TokK::EqEq, span: Span::new(start, i) });
                continue;
            }
            '+' => one!(TokK::Plus),
            '=' => one!(TokK::Eq),
            '.' => one!(TokK::Dot),
            ',' => one!(TokK::Comma),
            ':' => one!(TokK::Colon),
            ';' => one!(TokK::Semi),
            '(' => one!(TokK::LParen),
            ')' => one!(TokK::RParen),
            '{' => one!(TokK::LBrace),
            '}' => one!(TokK::RBrace),
            '[' => one!(TokK::LBrack),
            ']' => one!(TokK::RBrack),
            _ => {
                return Err(LexError {
                    span: Span::new(start, start + 1),
                    msg: format!("无法识别的字符 {:?}", c),
                });
            }
        }
    }
    out.push(Tok { k: TokK::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}
