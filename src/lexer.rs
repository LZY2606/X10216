use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Name(String),
    Tag(String), // starts with backtick
    Arrow,     // ->
    FatArrow,  // =>
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Eq,
    Colon,
    Comma,
    Semi,
    Dot,
    Pipe,
    Underscore,
    // keywords
    Let,
    Rec,
    In,
    Fun,
    Match,
    With,
    True,
    False,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub span: Span,
    pub msg: String,
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}
fn is_ident_cont(c: char) -> bool {
    c == '_' || c == '\'' || c.is_alphanumeric()
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let chars: Vec<char> = src.chars().collect();
    let mut i = 0usize;
    let mut tokens = Vec::new();
    let byte_of = |ci: usize| {
        let mut byte = 0usize;
        for ch in &chars[..ci] {
            byte += ch.len_utf8();
        }
        byte
    };
    while i < chars.len() {
        let c = chars[i];
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        // comments: -- line, (* block, nested *)
        if c == '-' && i + 1 < chars.len() && chars[i + 1] == '-' {
            i += 2;
            while i < chars.len() && chars[i] != '\n' {
                i += 1;
            }
            continue;
        }
        if c == '(' && i + 1 < chars.len() && chars[i + 1] == '*' {
            let start = byte_of(i);
            let mut depth = 1;
            i += 2;
            while i < chars.len() && depth > 0 {
                if i + 1 < chars.len() && chars[i] == '(' && chars[i + 1] == '*' {
                    depth += 1;
                    i += 2;
                } else if i + 1 < chars.len() && chars[i] == '*' && chars[i + 1] == ')' {
                    depth -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if depth != 0 {
                return Err(LexError {
                    span: Span::new(start, src.len()),
                    msg: "unterminated block comment".into(),
                });
            }
            continue;
        }
        let start = byte_of(i);
        // integer
        if c.is_ascii_digit() {
            let mut n = 0i64;
            while i < chars.len() && chars[i].is_ascii_digit() {
                n = n.saturating_mul(10).saturating_add(chars[i].to_digit(10).unwrap() as i64);
                i += 1;
            }
            let end = byte_of(i);
            tokens.push(Token { tok: Tok::Int(n), span: Span::new(start, end) });
            continue;
        }
        // backtick tag
        if c == '`' {
            i += 1;
            let s0 = i;
            while i < chars.len() && is_ident_cont(chars[i]) {
                i += 1;
            }
            if i == s0 {
                let end = byte_of(i.min(chars.len()));
                return Err(LexError { span: Span::new(start, end), msg: "expected tag name after `".into() });
            }
            let name: String = chars[s0..i].iter().collect();
            let end = byte_of(i);
            tokens.push(Token { tok: Tok::Tag(name), span: Span::new(start, end) });
            continue;
        }
        if is_ident_start(c) {
            let s0 = i;
            while i < chars.len() && is_ident_cont(chars[i]) {
                i += 1;
            }
            let name: String = chars[s0..i].iter().collect();
            let end = byte_of(i);
            let tok = match name.as_str() {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fun" => Tok::Fun,
                "match" => Tok::Match,
                "with" => Tok::With,
                "true" => Tok::True,
                "false" => Tok::False,
                "_" => Tok::Underscore,
                _ => Tok::Name(name),
            };
            tokens.push(Token { tok, span: Span::new(start, end) });
            continue;
        }
        // multi-char
        if c == '-' && i + 1 < chars.len() && chars[i + 1] == '>' {
            let end = byte_of(i + 2);
            tokens.push(Token { tok: Tok::Arrow, span: Span::new(start, end) });
            i += 2;
            continue;
        }
        if c == '=' && i + 1 < chars.len() && chars[i + 1] == '>' {
            let end = byte_of(i + 2);
            tokens.push(Token { tok: Tok::FatArrow, span: Span::new(start, end) });
            i += 2;
            continue;
        }
        let (tok, adv) = match c {
            '{' => (Tok::LBrace, 1),
            '}' => (Tok::RBrace, 1),
            '[' => (Tok::LBracket, 1),
            ']' => (Tok::RBracket, 1),
            '(' => (Tok::LParen, 1),
            ')' => (Tok::RParen, 1),
            '=' => (Tok::Eq, 1),
            ':' => (Tok::Colon, 1),
            ',' => (Tok::Comma, 1),
            ';' => (Tok::Semi, 1),
            '.' => (Tok::Dot, 1),
            '|' => (Tok::Pipe, 1),
            _ => {
                let end = byte_of(i + 1);
                return Err(LexError { span: Span::new(start, end), msg: format!("unexpected character {:?}", c) });
            }
        };
        let end = byte_of(i + adv);
        tokens.push(Token { tok, span: Span::new(start, end) });
        i += adv;
    }
    tokens.push(Token { tok: Tok::Eof, span: Span::new(src.len(), src.len()) });
    Ok(tokens)
}
