#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Str(String),
    Lower(String),
    Upper(String),
    // operators / punctuation
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Colon, Dot, Equals, Backslash, Pipe, Underscore,
    Arrow, FatArrow, Plus, Minus, Star, Slash, EqEq, Lt, Gt, AmpAmp, PipePipe,
    Let, In, Rec, True, False, Case, Of,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub start: u32,
    pub end: u32,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub msg: String,
    pub at: u32,
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let b = src.as_bytes();
    let mut i = 0usize;
    let mut out = Vec::new();
    let mut push = |t: Tok, s: usize, e: usize| {
        out.push(Token { tok: t, start: s as u32, end: e as u32 });
    };
    while i < b.len() {
        let c = b[i];
        match c {
            b' ' | b'\t' | b'\n' | b'\r' => i += 1,
            b'(' => { push(Tok::LParen, i, i + 1); i += 1; }
            b')' => { push(Tok::RParen, i, i + 1); i += 1; }
            b'{' => { push(Tok::LBrace, i, i + 1); i += 1; }
            b'}' => { push(Tok::RBrace, i, i + 1); i += 1; }
            b'[' => { push(Tok::LBracket, i, i + 1); i += 1; }
            b']' => { push(Tok::RBracket, i, i + 1); i += 1; }
            b',' => { push(Tok::Comma, i, i + 1); i += 1; }
            b':' => { push(Tok::Colon, i, i + 1); i += 1; }
            b'.' => { push(Tok::Dot, i, i + 1); i += 1; }
            b'=' => {
                if b.get(i + 1) == Some(&b'=') { push(Tok::EqEq, i, i + 2); i += 2; }
                else if b.get(i + 1) == Some(&b'>') { push(Tok::FatArrow, i, i + 2); i += 2; }
                else { push(Tok::Equals, i, i + 1); i += 1; }
            }
            b'\\' => { push(Tok::Backslash, i, i + 1); i += 1; }
            b'|' => {
                if b.get(i + 1) == Some(&b'|') { push(Tok::PipePipe, i, i + 2); i += 2; }
                else { push(Tok::Pipe, i, i + 1); i += 1; }
            }
            b'_' => {
                if b.get(i + 1).map_or(false, |n| n.is_ascii_alphanumeric() || *n == b'_') {
                    let s = i;
                    i += 1;
                    while b.get(i).map_or(false, |n| n.is_ascii_alphanumeric() || *n == b'_') { i += 1; }
                    let w = &src[s..i];
                    push(Tok::Lower(w.to_string()), s, i);
                } else { push(Tok::Underscore, i, i + 1); i += 1; }
            }
            b'-' => {
                if b.get(i + 1) == Some(&b'>') { push(Tok::Arrow, i, i + 2); i += 2; }
                else { push(Tok::Minus, i, i + 1); i += 1; }
            }
            b'+' => { push(Tok::Plus, i, i + 1); i += 1; }
            b'*' => { push(Tok::Star, i, i + 1); i += 1; }
            b'/' => {
                if b.get(i + 1) == Some(&b'/') {
                    while i < b.len() && b[i] != b'\n' { i += 1; }
                } else if b.get(i + 1) == Some(&b'*') {
                    i += 2;
                    let mut depth = 1;
                    while i < b.len() && depth > 0 {
                        if b[i] == b'/' && b.get(i + 1) == Some(&b'*') { depth += 1; i += 2; }
                        else if b[i] == b'*' && b.get(i + 1) == Some(&b'/') { depth -= 1; i += 2; }
                        else { i += 1; }
                    }
                } else { push(Tok::Slash, i, i + 1); i += 1; }
            }
            b'<' => { push(Tok::Lt, i, i + 1); i += 1; }
            b'>' => { push(Tok::Gt, i, i + 1); i += 1; }
            b'&' if b.get(i + 1) == Some(&b'&') => { push(Tok::AmpAmp, i, i + 2); i += 2; }
            b'"' => {
                let s = i;
                i += 1;
                let mut val = String::new();
                while i < b.len() && b[i] != b'"' {
                    if b[i] == b'\\' {
                        i += 1;
                        if i >= b.len() { break; }
                        match b[i] {
                            b'n' => val.push('\n'),
                            b't' => val.push('\t'),
                            b'"' => val.push('"'),
                            b'\\' => val.push('\\'),
                            o => val.push(o as char),
                        }
                        i += 1;
                    } else {
                        let ch_start = i;
                        // advance one utf-8 char
                        i += if b[i] < 0x80 { 1 } else {
                            let mut n = 1;
                            while b.get(i + n).map_or(false, |x| (x & 0xC0) == 0x80) { n += 1; }
                            n
                        };
                        val.push_str(&src[ch_start..i]);
                    }
                }
                if i >= b.len() {
                    return Err(LexError { msg: "unterminated string".into(), at: s as u32 });
                }
                i += 1;
                push(Tok::Str(val), s, i);
            }
            d if d.is_ascii_digit() => {
                let s = i;
                while i < b.len() && b[i].is_ascii_digit() { i += 1; }
                let v: i64 = src[s..i].parse().map_err(|_| LexError {
                    msg: "bad integer".into(), at: s as u32,
                })?;
                push(Tok::Int(v), s, i);
            }
            a if a.is_ascii_alphabetic() => {
                let s = i;
                while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') { i += 1; }
                let w = &src[s..i];
                let t = match w {
                    "let" => Tok::Let,
                    "in" => Tok::In,
                    "rec" => Tok::Rec,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "case" => Tok::Case,
                    "of" => Tok::Of,
                    _ if c.is_ascii_uppercase() => Tok::Upper(w.to_string()),
                    _ => Tok::Lower(w.to_string()),
                };
                push(t, s, i);
            }
            _ => return Err(LexError { msg: format!("unexpected character {:?}", c as char), at: i as u32 }),
        }
    }
    Ok(out)
}
