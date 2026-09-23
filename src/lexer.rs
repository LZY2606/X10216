use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Lower(String),
    Upper(String),
    Label(String),
    // punctuation
    LParen, RParen, LBrace, RBrace, Arrow, Eq, EqEq, Lt, Gt, Plus, Minus,
    Backslash, Dot, Comma, Semicolon, Bar, Under, Colon, Star,
    // keywords
    Let, Rec, Fn, If, Then, Else, Match, With, True, False, In, Record, Variant,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token { pub tok: Tok, pub span: Span }

#[derive(Debug, Clone)]
pub struct LexError { pub msg: String, pub span: Span }

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mk = |t: Tok, s, e| Token { tok: t, span: Span::new(s, e) };
    while i < b.len() {
        let c = b[i] as char;
        match c {
            ' ' | '\t' | '\n' | '\r' => { i += 1; }
            '/' if b.get(i + 1) == Some(&b'/') => {
                while i < b.len() && b[i] != b'\n' { i += 1; }
            }
            '(' if b.get(i + 1) == Some(&b'*') => {
                let s = i;
                i += 2;
                let mut depth = 1usize;
                while i < b.len() && depth > 0 {
                    if b[i] == b'(' && b.get(i + 1) == Some(&b'*') { depth += 1; i += 2; }
                    else if b[i] == b'*' && b.get(i + 1) == Some(&b')') { depth -= 1; i += 2; }
                    else { i += 1; }
                }
                if depth > 0 { return Err(LexError { msg: "未闭合的 (* 注释".into(), span: Span::new(s, b.len()) }); }
            }
            '-' if b.get(i + 1) == Some(&b'>') => { out.push(mk(Tok::Arrow, i, i + 2)); i += 2; }
            '(' => { out.push(mk(Tok::LParen, i, i + 1)); i += 1; }
            ')' => { out.push(mk(Tok::RParen, i, i + 1)); i += 1; }
            '{' => { out.push(mk(Tok::LBrace, i, i + 1)); i += 1; }
            '}' => { out.push(mk(Tok::RBrace, i, i + 1)); i += 1; }
            '=' if b.get(i + 1) == Some(&b'=') => { out.push(mk(Tok::EqEq, i, i + 2)); i += 2; }
            '=' => { out.push(mk(Tok::Eq, i, i + 1)); i += 1; }
            '<' => { out.push(mk(Tok::Lt, i, i + 1)); i += 1; }
            '>' => { out.push(mk(Tok::Gt, i, i + 1)); i += 1; }
            '+' => { out.push(mk(Tok::Plus, i, i + 1)); i += 1; }
            '-' => { out.push(mk(Tok::Minus, i, i + 1)); i += 1; }
            '*' => { out.push(mk(Tok::Star, i, i + 1)); i += 1; }
            '\\' => { out.push(mk(Tok::Backslash, i, i + 1)); i += 1; }
            '.' => { out.push(mk(Tok::Dot, i, i + 1)); i += 1; }
            ',' => { out.push(mk(Tok::Comma, i, i + 1)); i += 1; }
            ';' => { out.push(mk(Tok::Semicolon, i, i + 1)); i += 1; }
            '|' => { out.push(mk(Tok::Bar, i, i + 1)); i += 1; }
            '_' => {
                let s = i;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') { i += 1; }
                if i > s + 1 {
                    out.push(mk(Tok::Lower(src[s..i].to_string()), s, i));
                } else {
                    out.push(mk(Tok::Under, s, i));
                }
            }
            ':' => { out.push(mk(Tok::Colon, i, i + 1)); i += 1; }
            '0'..='9' => {
                let s = i;
                while i < b.len() && b[i].is_ascii_digit() { i += 1; }
                let n: i64 = src[s..i].parse().map_err(|_| LexError {
                    msg: "整数字面量超出范围".into(), span: Span::new(s, i) })?;
                out.push(mk(Tok::Int(n), s, i));
            }
            'a'..='z' => {
                let s = i;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') { i += 1; }
                let word = &src[s..i];
                let t = match word {
                    "let" => Tok::Let, "rec" => Tok::Rec, "fn" => Tok::Fn,
                    "if" => Tok::If, "then" => Tok::Then, "else" => Tok::Else,
                    "match" => Tok::Match, "with" => Tok::With,
                    "true" => Tok::True, "false" => Tok::False, "in" => Tok::In,
                    "record" => Tok::Record, "variant" => Tok::Variant,
                    other => Tok::Lower(other.to_string()),
                };
                out.push(mk(t, s, i));
            }
            'A'..='Z' => {
                let s = i;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') { i += 1; }
                out.push(mk(Tok::Upper(src[s..i].to_string()), s, i));
            }
            '~' if b.get(i + 1) == Some(&b'"') => {
                // ~"label" quoted field label
                let s = i;
                i += 2;
                let ls = i;
                while i < b.len() && b[i] != b'"' { i += 1; }
                if i >= b.len() { return Err(LexError { msg: "缺少闭合的 \"".into(), span: Span::new(s, i) }); }
                let label = src[ls..i].to_string();
                i += 1;
                out.push(mk(Tok::Label(label), s, i));
            }
            _ => return Err(LexError {
                msg: format!("无法识别的字符 {:?}", c), span: Span::new(i, i + c.len_utf8()) }),
        }
    }
    out.push(mk(Tok::Eof, b.len(), b.len()));
    Ok(out)
}
