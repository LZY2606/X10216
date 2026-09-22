// Hand-written lexer producing spans for every token.

use crate::syntax::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tok {
    pub kind: TokKind,
    pub span: Span,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokKind {
    Int(i64),
    Bool(bool),
    Str(String),
    Ident,
    Backtick,
    Fn,
    Let,
    Rec,
    In,
    Match,
    With,
    Pipe,
    Arrow,
    Eq,
    Dot,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub span: Span,
    pub message: String,
}

pub fn lex(src: &str) -> Result<Vec<Tok>, LexError> {
    let b = src.as_bytes();
    let mut i = 0usize;
    let mut out = Vec::new();
    let bump = |i: &mut usize, n: usize| {
        let s = *i;
        *i += n;
        s
    };
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '-' && i + 1 < b.len() && b[i + 1] == b'-' {
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        let start = i;
        if c.is_ascii_digit() {
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let txt = &src[start..i];
            let n: i64 = txt.parse().map_err(|_| LexError {
                span: Span(start as u32, i as u32),
                message: "数字超出范围".to_string(),
            })?;
            out.push(Tok {
                kind: TokKind::Int(n),
                span: Span(start as u32, i as u32),
                text: txt.to_string(),
            });
            continue;
        }
        if c == '"' {
            i += 1;
            let mut val = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    val.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        other => other,
                    });
                    i += 2;
                } else {
                    val.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(LexError {
                    span: Span(start as u32, i as u32),
                    message: "字符串缺少结束引号".to_string(),
                });
            }
            i += 1;
            out.push(Tok {
                kind: TokKind::Str(val),
                span: Span(start as u32, i as u32),
                text: src[start..i].to_string(),
            });
            continue;
        }
        if c == '`' {
            let s = bump(&mut i, 1);
            out.push(Tok {
                kind: TokKind::Backtick,
                span: Span(s as u32, i as u32),
                text: "`".to_string(),
            });
            continue;
        }
        if c.is_alphabetic() || c == '_' {
            while i < b.len() && {
                let ch = b[i] as char;
                ch.is_alphanumeric() || ch == '_'
            } {
                i += 1;
            }
            let txt = src[start..i].to_string();
            let kind = match txt.as_str() {
                "fn" => TokKind::Fn,
                "let" => TokKind::Let,
                "rec" => TokKind::Rec,
                "in" => TokKind::In,
                "match" => TokKind::Match,
                "with" => TokKind::With,
                "true" => TokKind::Bool(true),
                "false" => TokKind::Bool(false),
                _ => TokKind::Ident,
            };
            out.push(Tok {
                kind,
                span: Span(start as u32, i as u32),
                text: txt,
            });
            continue;
        }
        macro_rules! punct {
            ($k:expr, $n:expr) => {{
                let s = bump(&mut i, $n);
                out.push(Tok {
                    kind: $k,
                    span: Span(s as u32, i as u32),
                    text: src[s..i].to_string(),
                });
                continue;
            }};
        }
        if c == '-' && i + 1 < b.len() && b[i + 1] == b'>' {
            punct!(TokKind::Arrow, 2);
        }
        match c {
            '|' => punct!(TokKind::Pipe, 1),
            '=' => punct!(TokKind::Eq, 1),
            '.' => punct!(TokKind::Dot, 1),
            ',' => punct!(TokKind::Comma, 1),
            '(' => punct!(TokKind::LParen, 1),
            ')' => punct!(TokKind::RParen, 1),
            '{' => punct!(TokKind::LBrace, 1),
            '}' => punct!(TokKind::RBrace, 1),
            _ => {
                return Err(LexError {
                    span: Span(i as u32, (i + 1) as u32),
                    message: format!("无法识别的字符 {c:?}"),
                })
            }
        }
    }
    out.push(Tok {
        kind: TokKind::Eof,
        span: Span(b.len() as u32, b.len() as u32),
        text: String::new(),
    });
    Ok(out)
}
