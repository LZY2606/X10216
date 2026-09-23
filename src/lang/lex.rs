//! Hand-written lexer.

use super::ast::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Let, Rec, In, Fn, If, Then, Else, Match, With, True, False,
    Ident(String), Int(i64), Str(String),
    Arrow, Fat, Eq, Plus, Minus, Eq2, Lt,
    Lp, Rp, Lb, Rb, Comma, Semi, Dot, Dot2, Pipe, Hash, Under, Eof,
}

#[derive(Clone, Debug)]
pub struct Token { pub tok: Tok, pub span: Span }

#[derive(Clone, Debug)]
pub struct LexError { pub msg: String, pub span: Span }

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mk = |tok: Tok, lo: usize, hi: usize| Token { tok, span: Span(lo as u32, hi as u32) };
    while i < b.len() {
        let c = b[i];
        if c == b' ' || c == b'\t' || c == b'\n' || c == b'\r' { i += 1; continue; }
        if c == b'/' && i + 1 < b.len() && b[i + 1] == b'/' {
            while i < b.len() && b[i] != b'\n' { i += 1; }
            continue;
        }
        if b'(' == c && i + 1 < b.len() && b[i + 1] == b'*' {
            let lo = i;
            i += 2;
            let mut depth = 1;
            while i < b.len() && depth > 0 {
                if i + 1 < b.len() && b[i] == b'(' && b[i + 1] == b'*' { depth += 1; i += 2; }
                else if i + 1 < b.len() && b[i] == b'*' && b[i + 1] == b')' { depth -= 1; i += 2; }
                else { i += 1; }
            }
            if depth > 0 { return Err(LexError { msg: "注释未闭合".into(), span: Span(lo as u32, lo as u32 + 2) }); }
            continue;
        }
        let lo = i;
        if c.is_ascii_digit() {
            while i < b.len() && b[i].is_ascii_digit() { i += 1; }
            let v: i64 = src[lo..i].parse().map_err(|_| LexError { msg: "整数越界".into(), span: Span(lo as u32, i as u32) })?;
            out.push(mk(Tok::Int(v), lo, i));
            continue;
        }
        if c == b'"' {
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1];
                    s.push(match e { b'n' => '\n', b't' => '\t', b'"' => '"', b'\\' => '\\', _ => e as char });
                    i += 2;
                } else {
                    let ch_start = i;
                    if b[i] < 0x80 { s.push(b[i] as char); i += 1; }
                    else {
                        let end = (i + 1..=b.len()).find(|&e| std::str::from_utf8(&b[i..e]).is_ok() && e > i).unwrap_or(i + 1);
                        s.push_str(&src[ch_start..end]);
                        i = end;
                    }
                }
            }
            if i >= b.len() { return Err(LexError { msg: "字符串未闭合".into(), span: Span(lo as u32, i as u32) }); }
            i += 1;
            out.push(mk(Tok::Str(s), lo, i));
            continue;
        }
        if c.is_ascii_alphabetic() || c == b'_' {
            while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') { i += 1; }
            let w = &src[lo..i];
            let tok = match w {
                "let" => Tok::Let, "rec" => Tok::Rec, "in" => Tok::In, "fn" => Tok::Fn,
                "if" => Tok::If, "then" => Tok::Then, "else" => Tok::Else,
                "match" => Tok::Match, "with" => Tok::With,
                "true" => Tok::True, "false" => Tok::False,
                "_" => Tok::Under,
                other => Tok::Ident(other.to_string()),
            };
            out.push(mk(tok, lo, i));
            continue;
        }
        let two = if i + 1 < b.len() { &src[i..i + 2] } else { "" };
        let (tok, n) = match two {
            "->" => (Tok::Arrow, 2), "=>" => (Tok::Fat, 2), "==" => (Tok::Eq2, 2), ".." => (Tok::Dot2, 2),
            _ => (match c {
                b'=' => Tok::Eq, b'+' => Tok::Plus, b'-' => Tok::Minus, b'<' => Tok::Lt,
                b'(' => Tok::Lp, b')' => Tok::Rp, b'{' => Tok::Lb, b'}' => Tok::Rb,
                b',' => Tok::Comma, b';' => Tok::Semi, b'.' => Tok::Dot, b'|' => Tok::Pipe,
                b'#' => Tok::Hash,
                _ => return Err(LexError { msg: format!("无法识别的字符 {:?}", c as char), span: Span(i as u32, i as u32 + 1) }),
            }, 1),
        };
        out.push(mk(tok, lo, i + n));
        i += n;
    }
    let end = b.len() as u32;
    out.push(Token { tok: Tok::Eof, span: Span(end, end) });
    Ok(out)
}
