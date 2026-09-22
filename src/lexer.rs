#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub col: u32,
}

impl Span {
    pub fn new(start: usize, end: usize, source: &str) -> Self {
        let prefix = &source[..start.min(source.len())];
        let line = prefix.as_bytes().iter().filter(|b| **b == b'\n').count() as u32 + 1;
        let col = prefix.rsplit('\n').next().unwrap_or(prefix).chars().count() as u32 + 1;
        Self { start, end, line, col }
    }

    pub fn join(self, other: Span) -> Span {
        if self.start <= other.start {
            Span { start: self.start, end: other.end.max(self.end), line: self.line, col: self.col }
        } else {
            other.join(self)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
#[serde(tag = "kind", content = "text", rename_all = "snake_case")]
pub enum TokenKind {
    Int(i64),
    Str(String),
    Ident(String),
    Upper(String),
    Let, Rec, In, Fn, Fun, If, Then, Else, Match, With, True, False, Record, Extend, Variant,
    Arrow, FatArrow, Eq, EqEq, Neq, Lt, Le, Gt, Ge, Plus, Minus, Star, Slash, AndAnd, OrOr,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket, Comma, Dot, Colon, Semicolon, Pipe,
    Underscore, Eof,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    c == '_' || c.is_alphanumeric() || c == '\''
}

fn keyword(text: &str) -> Option<TokenKind> {
    Some(match text {
        "let" => TokenKind::Let,
        "rec" => TokenKind::Rec,
        "in" => TokenKind::In,
        "fn" | "fun" => TokenKind::Fn,
        "if" => TokenKind::If,
        "then" => TokenKind::Then,
        "else" => TokenKind::Else,
        "match" => TokenKind::Match,
        "with" => TokenKind::With,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "record" => TokenKind::Record,
        "extend" => TokenKind::Extend,
        "variant" => TokenKind::Variant,
        _ => return None,
    })
}

pub fn lex(source: &str) -> Result<Vec<Token>, String> {
    let chars: Vec<char> = source.chars().collect();
    let mut index = 0usize;
    let mut tokens = Vec::new();
    while index < chars.len() {
        let c = chars[index];
        if c.is_whitespace() {
            index += 1;
            continue;
        }
        let start_bytes: usize = chars[..index].iter().map(|c| c.len_utf8()).sum();
        if c == '-' && index + 1 < chars.len() && chars[index + 1] == '-' {
            while index < chars.len() && chars[index] != '\n' {
                index += 1;
            }
            continue;
        }
        let start = index;
        let mut two = |a: char, b: char| index + 1 < chars.len() && chars[index] == a && chars[index + 1] == b;
        if c.is_ascii_digit() {
            while index < chars.len() && chars[index].is_ascii_digit() {
                index += 1;
            }
            let text: String = chars[start..index].iter().collect();
            let value = text.parse().map_err(|_| format!("invalid integer at {start_bytes}"))?;
            push(&mut tokens, TokenKind::Int(value), start_bytes, byte_end(&chars, index), source);
            continue;
        }
        if is_ident_start(c) {
            while index < chars.len() && is_ident_continue(chars[index]) {
                index += 1;
            }
            let text: String = chars[start..index].iter().collect();
            let kind = keyword(&text).unwrap_or_else(|| {
                if text == "_" {
                    TokenKind::Underscore
                } else if text.chars().next().is_some_and(|c| c.is_uppercase()) {
                    TokenKind::Upper(text)
                } else {
                    TokenKind::Ident(text)
                }
            });
            push(&mut tokens, kind, start_bytes, byte_end(&chars, index), source);
            continue;
        }
        if c == '"' {
            index += 1;
            let mut value = String::new();
            while index < chars.len() && chars[index] != '"' {
                if chars[index] == '\\' && index + 1 < chars.len() {
                    let escaped = chars[index + 1];
                    value.push(match escaped {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        other => other,
                    });
                    index += 2;
                } else {
                    value.push(chars[index]);
                    index += 1;
                }
            }
            if index >= chars.len() {
                return Err(format!("unterminated string at {start_bytes}"));
            }
            index += 1;
            push(&mut tokens, TokenKind::Str(value), start_bytes, byte_end(&chars, index), source);
            continue;
        }
        let kind = if two('-', '>') {
            index += 2; TokenKind::Arrow
        } else if two('=', '>') {
            index += 2; TokenKind::FatArrow
        } else if two('=', '=') {
            index += 2; TokenKind::EqEq
        } else if two('!', '=') {
            index += 2; TokenKind::Neq
        } else if two('<', '=') {
            index += 2; TokenKind::Le
        } else if two('>', '=') {
            index += 2; TokenKind::Ge
        } else if two('&', '&') {
            index += 2; TokenKind::AndAnd
        } else if two('|', '|') {
            index += 2; TokenKind::OrOr
        } else {
            let one = match c {
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                ',' => TokenKind::Comma,
                '.' => TokenKind::Dot,
                ':' => TokenKind::Colon,
                ';' => TokenKind::Semicolon,
                '|' => TokenKind::Pipe,
                '=' => TokenKind::Eq,
                '<' => TokenKind::Lt,
                '>' => TokenKind::Gt,
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                _ => return Err(format!("unexpected character {c:?} at {start_bytes}")),
            };
            index += 1;
            one
        };
        let end = byte_end(&chars, index);
        push(&mut tokens, kind, start_bytes, end, source);
    }
    let end = source.len();
    tokens.push(Token { kind: TokenKind::Eof, span: Span::new(end, end, source) });
    Ok(tokens)
}

fn byte_end(chars: &[char], index: usize) -> usize {
    chars[..index].iter().map(|c| c.len_utf8()).sum()
}

fn push(tokens: &mut Vec<Token>, kind: TokenKind, start: usize, end: usize, source: &str) {
    tokens.push(Token { kind, span: Span::new(start, end, source) });
}
