// Lexer, parser and AST for the slicer language.
use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub lo: usize, // char offset, inclusive
    pub hi: usize, // char offset, exclusive
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span { lo: a.lo.min(b.lo), hi: a.hi.max(b.hi) }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tok {
    Int, Float, Str, Char, Bool,
    Ident, Upper,
    Let, Rec, In, Fun, If, Then, Else, True, False, Match, With,
    Arrow, FatArrow, Eq, EqEq, LtGt, Pipe, Dot, Comma, Colon,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Plus, Minus, Star, Slash, Lt, Gt, Le, Ge,
    Semicolon, Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub text: String,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a str,
    chars: Vec<(usize, char)>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src, chars: src.char_indices().collect(), pos: 0 }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).map(|c| c.1)
    }
    fn peek2(&self) -> Option<char> {
        self.chars.get(self.pos + 1).map(|c| c.1)
    }
    fn bump(&mut self) -> Option<(usize, char)> {
        let c = self.chars.get(self.pos).copied();
        if c.is_some() { self.pos += 1; }
        c
    }

    fn skip_ws(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => { self.bump(); }
                Some('/') if self.peek2() == Some('/') => {
                    while let Some(c) = self.peek() {
                        if c == '\n' { break; }
                        self.bump();
                    }
                }
                Some('/') if self.peek2() == Some('*') => {
                    self.bump(); self.bump();
                    let mut depth = 1usize;
                    while depth > 0 {
                        match self.bump() {
                            Some((_, '/')) if self.peek() == Some('*') => { self.bump(); depth += 1; }
                            Some((_, '*')) if self.peek() == Some('/') => { self.bump(); depth -= 1; }
                            None => break,
                            _ => {}
                        }
                    }
                }
                _ => break,
            }
        }
    }

    fn keyword(t: &str) -> Option<Tok> {
        Some(match t {
            "let" => Tok::Let, "rec" => Tok::Rec, "in" => Tok::In,
            "fun" => Tok::Fun, "if" => Tok::If, "then" => Tok::Then,
            "else" => Tok::Else, "true" => Tok::True, "false" => Tok::False,
            "match" => Tok::Match, "with" => Tok::With,
            _ => return None,
        })
    }

    pub fn next_token(&mut self) -> Result<Token, (Span, String)> {
        self.skip_ws();
        let (start_byte, c) = match self.bump() {
            Some(v) => v,
            None => return Ok(Token { tok: Tok::Eof, text: String::new(), span: Span::new(self.pos, self.pos) }),
        };
        let start = self.pos - 1;
        let single = |tok: Tok, text: String, slen: usize| {
            Ok(Token { tok, text, span: Span::new(start, start + slen) })
        };
        match c {
            '(' => single(Tok::LParen, "(".into(), 1),
            ')' => single(Tok::RParen, ")".into(), 1),
            '{' => single(Tok::LBrace, "{".into(), 1),
            '}' => single(Tok::RBrace, "}".into(), 1),
            '[' => single(Tok::LBracket, "[".into(), 1),
            ']' => single(Tok::RBracket, "]".into(), 1),
            ',' => single(Tok::Comma, ",".into(), 1),
            ';' => single(Tok::Semicolon, ";".into(), 1),
            '.' => single(Tok::Dot, ".".into(), 1),
            '|' => single(Tok::Pipe, "|".into(), 1),
            '+' => single(Tok::Plus, "+".into(), 1),
            '-' if self.peek() == Some('>') => { self.bump(); single(Tok::Arrow, "->".into(), 2) }
            '-' => single(Tok::Minus, "-".into(), 1),
            '*' => single(Tok::Star, "*".into(), 1),
            '/' => single(Tok::Slash, "/".into(), 1),
            '<' if self.peek() == Some('=') => { self.bump(); single(Tok::Le, "<=".into(), 2) }
            '<' if self.peek() == Some('>') => { self.bump(); single(Tok::LtGt, "<>".into(), 2) }
            '<' => single(Tok::Lt, "<".into(), 1),
            '>' if self.peek() == Some('=') => { self.bump(); single(Tok::Ge, ">=".into(), 2) }
            '>' => single(Tok::Gt, ">".into(), 1),
            '=' if self.peek() == Some('=') => { self.bump(); single(Tok::EqEq, "==".into(), 2) }
            '=' if self.peek() == Some('>') => { self.bump(); single(Tok::FatArrow, "=>".into(), 2) }
            '=' => single(Tok::Eq, "=".into(), 1),
            ':' => single(Tok::Colon, ":".into(), 1),
            '"' => self.lex_string(start),
            '\'' if self.chars.get(self.pos).map(|c| c.1.is_alphanumeric()).unwrap_or(false) => self.lex_char(start),
            d if d.is_ascii_digit() => self.lex_number(start, d),
            c if c.is_alphabetic() || c == '_' => {
                let end_byte = self.lex_ident_rest(start_byte);
                let lo = start;
                let hi = self.pos;
                let text = self.src[start_byte..end_byte].to_string();
                let first_upper = text.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                let tok = Self::keyword(&text).unwrap_or(if first_upper { Tok::Upper } else { Tok::Ident });
                Ok(Token { tok, text, span: Span::new(lo, hi) })
            }
            other => Err((Span::new(start, start + 1), format!("非法字符 {:?}", other))),
        }
    }

    fn lex_ident_rest(&mut self, start_byte: usize) -> usize {
        let mut end = start_byte;
        while let Some(&(b, ch)) = self.chars.get(self.pos) {
            if ch.is_alphanumeric() || ch == '_' || ch == '\'' {
                end = b + ch.len_utf8();
                self.pos += 1;
            } else { break; }
        }
        end
    }

    fn lex_number(&mut self, start: usize, first: char) -> Result<Token, (Span, String)> {
        let mut text = String::new();
        text.push(first);
        let mut is_float = false;
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() { text.push(ch); self.bump(); }
            else if ch == '.' && self.peek2().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                is_float = true;
                text.push(ch); self.bump();
            } else { break; }
        }
        Ok(Token {
            tok: if is_float { Tok::Float } else { Tok::Int },
            text,
            span: Span::new(start, self.pos),
        })
    }

    fn lex_char(&mut self, start: usize) -> Result<Token, (Span, String)> {
        let mut text = String::new();
        let mut closed = false;
        while let Some(ch) = self.peek() {
            self.bump();
            if ch == '\'' { closed = true; break; }
            text.push(ch);
            if text.chars().count() > 4 { break; }
        }
        if !closed || text.is_empty() {
            return Err((Span::new(start, self.pos), "字符字面量缺少结尾 '".into()));
        }
        Ok(Token { tok: Tok::Char, text, span: Span::new(start, self.pos) })
    }

    fn lex_string(&mut self, start: usize) -> Result<Token, (Span, String)> {
        let mut text = String::new();
        let mut closed = false;
        while let Some(ch) = self.peek() {
            self.bump();
            if ch == '"' { closed = true; break; }
            if ch == '\\' {
                if let Some(e) = self.peek() { self.bump(); text.push(escape(e)); }
            } else {
                text.push(ch);
            }
        }
        if !closed {
            return Err((Span::new(start, self.pos), "字符串缺少结尾 \"".into()));
        }
        Ok(Token { tok: Tok::Str, text, span: Span::new(start, self.pos) })
    }
}

fn escape(c: char) -> char {
    match c {
        'n' => '\n', 't' => '\t', 'r' => '\r', '\\' => '\\', '"' => '"', '\'' => '\'',
        '0' => '\0', other => other,
    }
}

pub fn lex(src: &str) -> Result<Vec<Token>, (Span, String)> {
    let mut lx = Lexer::new(src);
    let mut out = Vec::new();
    loop {
        let t = lx.next_token()?;
        let eof = t.tok == Tok::Eof;
        out.push(t);
        if eof { break; }
    }
    Ok(out)
}
