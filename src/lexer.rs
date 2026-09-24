use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Ident(String),
    Tag(String),
    Int(i64),
    Str(String),
    Let,
    Rec,
    In,
    Fun,
    Match,
    With,
    True,
    False,
    Underscore,
    // punctuation
    Eq,
    EqEq,
    Plus,
    Minus,
    Star,
    Arrow,
    Bar,
    Dot,
    Comma,
    Semi,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Backtick,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, (Span, String)> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i];
        match c {
            b' ' | b'\t' | b'\r' | b'\n' => i += 1,
            b'(' => { out.push(p(Tok::LParen, i, i + 1)); i += 1; }
            b')' => { out.push(p(Tok::RParen, i, i + 1)); i += 1; }
            b'{' => { out.push(p(Tok::LBrace, i, i + 1)); i += 1; }
            b'}' => { out.push(p(Tok::RBrace, i, i + 1)); i += 1; }
            b'|' => {
                if b.get(i + 1) == Some(&b'|') { return Err((Span::new(i, i + 2), "意外的 `||`，本语言暂不支持".into())); }
                out.push(p(Tok::Bar, i, i + 1)); i += 1;
            }
            b'.' => { out.push(p(Tok::Dot, i, i + 1)); i += 1; }
            b',' => { out.push(p(Tok::Comma, i, i + 1)); i += 1; }
            b';' => { out.push(p(Tok::Semi, i, i + 1)); i += 1; }
            b'+' => { out.push(p(Tok::Plus, i, i + 1)); i += 1; }
            b'*' => { out.push(p(Tok::Star, i, i + 1)); i += 1; }
            b'-' => {
                if b.get(i + 1) == Some(&b'>') { out.push(p(Tok::Arrow, i, i + 2)); i += 2; }
                else { out.push(p(Tok::Minus, i, i + 1)); i += 1; }
            }
            b'=' => {
                if b.get(i + 1) == Some(&b'=') { out.push(p(Tok::EqEq, i, i + 2)); i += 2; }
                else { out.push(p(Tok::Eq, i, i + 1)); i += 1; }
            }
            b'`' => {
                let lo = i;
                i += 1;
                let s = i;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'\'') {
                    i += 1;
                }
                if s == i { return Err((Span::new(lo, i), "反引号后缺少变体标签".into())); }
                let name = src[s..i].to_string();
                out.push(Token { tok: Tok::Tag(name), span: Span::new(lo, i) });
            }
            b'0'..=b'9' => {
                let s = i;
                while i < b.len() && b[i].is_ascii_digit() { i += 1; }
                let n: i64 = src[s..i].parse().map_err(|_| (Span::new(s, i), "整数字面量越界".to_string()))?;
                out.push(Token { tok: Tok::Int(n), span: Span::new(s, i) });
            }
            b'"' => {
                let lo = i;
                i += 1;
                let mut s = String::new();
                while i < b.len() && b[i] != b'"' {
                    if b[i] == b'\\' {
                        i += 1;
                        match b.get(i) {
                            Some(b'n') => s.push('\n'),
                            Some(b't') => s.push('\t'),
                            Some(b'"') => s.push('"'),
                            Some(b'\\') => s.push('\\'),
                            _ => return Err((Span::new(lo, i), "非法转义".into())),
                        }
                        i += 1;
                    } else {
                        // collect up to non-ascii safely
                        let ch_start = i;
                        if b[i] < 0x80 {
                            s.push(b[i] as char);
                            i += 1;
                        } else {
                            while i < b.len() && b[i] >= 0x80 { i += 1; }
                            s.push_str(&src[ch_start..i]);
                        }
                    }
                }
                if i >= b.len() { return Err((Span::new(lo, i), "字符串缺少右引号".into())); }
                i += 1;
                out.push(Token { tok: Tok::Str(s), span: Span::new(lo, i) });
            }
            c if c.is_ascii_alphabetic() || c == b'_' => {
                let s = i;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'\'') {
                    i += 1;
                }
                let word = &src[s..i];
                let tok = match word {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "fun" => Tok::Fun,
                    "match" => Tok::Match,
                    "with" => Tok::With,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "_" => Tok::Underscore,
                    other => Tok::Ident(other.to_string()),
                };
                out.push(Token { tok, span: Span::new(s, i) });
            }
            _ => return Err((Span::new(i, i + 1), format!("无法识别的字符 {:?}", c as char))),
        }
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}

fn p(t: Tok, lo: usize, hi: usize) -> Token {
    Token { tok: t, span: Span::new(lo, hi) }
}
