use super::ast::Span;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Tok {
    Int(i64),
    Ident(String),
    Let, Rec, In, Fun, If, Then, Else, True, False, Match, With, Underscore,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Eq, EqEq, Arrow, Dot, Comma, Colon, Pipe,
    Plus, Minus, Star, Semicolon, Lt, Gt,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub lo: u32,
    pub hi: u32,
}

impl Token {
    pub fn span(&self) -> Span {
        Span::new(self.lo, self.hi)
    }
}

pub fn lex(src: &str) -> Result<Vec<Token>, (String, u32, u32)> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let kw = |s: &str| -> Tok {
        match s {
            "let" => Tok::Let,
            "rec" => Tok::Rec,
            "in" => Tok::In,
            "fun" => Tok::Fun,
            "if" => Tok::If,
            "then" => Tok::Then,
            "else" => Tok::Else,
            "true" => Tok::True,
            "false" => Tok::False,
            "match" => Tok::Match,
            "with" => Tok::With,
            "_" => Tok::Underscore,
            _ => Tok::Ident(s.to_string()),
        }
    };
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        let lo = i as u32;
        if c.is_ascii_digit() {
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let v: i64 = src[lo as usize..i].parse().map_err(|_| ("bad integer".into(), lo, i as u32))?;
            out.push(Token { tok: Tok::Int(v), lo, hi: i as u32 });
            continue;
        }
        if c.is_ascii_alphabetic() || c == '_' {
            i += 1;
            while i < b.len() {
                let ch = b[i] as char;
                if ch.is_alphanumeric() || ch == '_' {
                    i += 1;
                } else {
                    break;
                }
            }
            out.push(Token { tok: kw(&src[lo as usize..i]), lo, hi: i as u32 });
            continue;
        }
        let two = |t: Tok, hi: usize, out: &mut Vec<Token>| out.push(Token { tok: t, lo, hi: hi as u32 });
        let one = |t: Tok, out: &mut Vec<Token>| out.push(Token { tok: t, lo, hi: (i + 1) as u32 });
        match c {
            '#' => {
                while i < b.len() && b[i] != b'\n' {
                    i += 1;
                }
                continue;
            }
            '-' if i + 1 < b.len() && b[i + 1] == b'>' => two(Tok::Arrow, i + 2, &mut out),
            '=' if i + 1 < b.len() && b[i + 1] == b'=' => two(Tok::EqEq, i + 2, &mut out),
            '(' => one(Tok::LParen, &mut out),
            ')' => one(Tok::RParen, &mut out),
            '{' => one(Tok::LBrace, &mut out),
            '}' => one(Tok::RBrace, &mut out),
            '[' => one(Tok::LBracket, &mut out),
            ']' => one(Tok::RBracket, &mut out),
            '=' => one(Tok::Eq, &mut out),
            '.' => one(Tok::Dot, &mut out),
            ',' => one(Tok::Comma, &mut out),
            ':' => one(Tok::Colon, &mut out),
            '|' => one(Tok::Pipe, &mut out),
            '+' => one(Tok::Plus, &mut out),
            '-' => one(Tok::Minus, &mut out),
            '*' => one(Tok::Star, &mut out),
            ';' => one(Tok::Semicolon, &mut out),
            '<' => one(Tok::Lt, &mut out),
            '>' => one(Tok::Gt, &mut out),
            _ => return Err((format!("unexpected character {:?}", c), lo, lo + 1)),
        }
        i = out.last().unwrap().hi as usize;
    }
    out.push(Token { tok: Tok::Eof, lo: src.len() as u32, hi: src.len() as u32 });
    Ok(out)
}
