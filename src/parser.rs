use crate::span::Span;
use crate::syntax::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Ident(String),
    Int(i64),
    Str(String),
    // punctuation
    LParen, RParen, LBrace, RBrace, Arrow, Eq, Dot, Comma, Backslash,
    // keywords
    Let, Rec, In, Fun, True, False, Match, With, Extend,
    Eof,
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
    c.is_alphanumeric() || c == '_' || c == '\''
}

pub fn lex(src: &str) -> Result<Vec<Token>, (String, Span)> {
    let bytes = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let bump = |i: &mut usize, n: usize| {
        let old = *i;
        for _ in 0..n {
            *i += src[*i..].chars().next().map(char::len_utf8).unwrap_or(1);
        }
        old
    };
    while i < bytes.len() {
        let c = bytes[i] as char;
        if c.is_whitespace() {
            i += c.len_utf8();
            continue;
        }
        let lo = i;
        if c.is_ascii_digit() {
            let start = i;
            while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                i += 1;
            }
            let text = &src[start..i];
            let n: i64 = text.parse().map_err(|_| {
                (format!("invalid integer literal `{text}`"), Span::new(start, i))
            })?;
            out.push(Token { tok: Tok::Int(n), span: Span::new(lo, i) });
            continue;
        }
        if c == '"' {
            i += 1;
            let start = i;
            let mut s = String::new();
            loop {
                if i >= bytes.len() {
                    return Err(("unterminated string".into(), Span::new(lo, i)));
                }
                let ch = bytes[i] as char;
                if ch == '"' {
                    i += 1;
                    break;
                }
                if ch == '\\' {
                    i += 1;
                    if i >= bytes.len() {
                        return Err(("bad escape".into(), Span::new(lo, i)));
                    }
                    let e = bytes[i] as char;
                    s.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        _ => e,
                    });
                    i += 1;
                } else {
                    s.push(ch);
                    i += ch.len_utf8();
                }
            }
            let _ = start;
            out.push(Token { tok: Tok::Str(s), span: Span::new(lo, i) });
            continue;
        }
        if is_ident_start(c) {
            let start = i;
            while i < bytes.len() && is_ident_cont(bytes[i] as char) {
                i += (bytes[i] as char).len_utf8();
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
                "extend" => Tok::Extend,
                _ => Tok::Ident(word.to_string()),
            };
            out.push(Token { tok, span: Span::new(start, i) });
            continue;
        }
        let two: String = src[i..].chars().take(2).collect();
        let (tok, n) = match two.as_str() {
            "->" => (Tok::Arrow, 2),
            _ => match c {
                '(' => (Tok::LParen, 1),
                ')' => (Tok::RParen, 1),
                '{' => (Tok::LBrace, 1),
                '}' => (Tok::RBrace, 1),
                '=' => (Tok::Eq, 1),
                '.' => (Tok::Dot, 1),
                ',' => (Tok::Comma, 1),
                '\\' | 'λ' => (Tok::Backslash, if c == 'λ' { 1 } else { 1 }),
                _ => return Err((format!("unexpected character `{c}`"), Span::new(lo, lo + c.len_utf8()))),
            },
        };
        let _ = bump;
        i += n;
        out.push(Token { tok, span: Span::new(lo, i) });
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(src.len(), src.len()) });
    Ok(out)
}
