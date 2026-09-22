//! Lexer + recursive-descent parser. Produces an AST whose node
//! fingerprints depend on structural position (kind, child index, literal
//! value), never on bound names, so renaming a variable keeps identities.

use crate::span::{NodeId, ParseError, Span};
use crate::syntax::{AstNode, BinderId, DefaultArm, Expr, Program};

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Int(i64),
    Str(String),
    Ident(String),
    Upper(String),
    Keyword(String),
    Punct(char),
    Backtick(String),
    Eof,
}

#[derive(Clone, Debug)]
struct Token {
    tok: Tok,
    span: Span,
}

const KEYWORDS: &[&str] = &["let", "in", "rec", "fn", "if", "then", "else", "true", "false", "match", "with", "default"];

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    toks: Vec<Token>,
    errors: Vec<ParseError>,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0, toks: Vec::new(), errors: Vec::new() }
    }
    fn peek(&self) -> Option<u8> { self.src.get(self.pos).copied() }
    fn bump(&mut self) -> Option<u8> {
        let b = self.peek();
        if b.is_some() { self.pos += 1; }
        b
    }
    fn lex(mut self) -> (Vec<Token>, Vec<ParseError>) {
        while let Some(b) = self.peek() {
            match b {
                b' ' | b'\t' | b'\r' | b'\n' => { self.pos += 1; }
                b'(' | b')' | b'{' | b'}' | b'.' | b',' | b'=' | b'|' | b';' | b':' | b'\\' | b'+' | b'-' | b'*' | b'[' | b']' => {
                    let lo = self.pos;
                    self.pos += 1;
                    let c = b as char;
                    self.toks.push(Token { tok: Tok::Punct(c), span: Span::new(lo, self.pos) });
                }
                b'0'..=b'9' => {
                    let lo = self.pos;
                    let mut v: i64 = 0;
                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() {
                            v = v.saturating_mul(10).saturating_add((c - b'0') as i64);
                            self.pos += 1;
                        } else { break; }
                    }
                    self.toks.push(Token { tok: Tok::Int(v), span: Span::new(lo, self.pos) });
                }
                b'"' => self.lex_string(),
                b'`' => {
                    let lo = self.pos;
                    self.pos += 1;
                    let start = self.pos;
                    while let Some(c) = self.peek() {
                        if c.is_ascii_alphanumeric() || c == b'_' { self.pos += 1; } else { break; }
                    }
                    let name = std::str::from_utf8(&self.src[start..self.pos]).unwrap_or("").to_string();
                    if name.is_empty() {
                        self.errors.push(ParseError { span: Span::new(lo, self.pos), message: "空的变体标签".into() });
                    }
                    self.toks.push(Token { tok: Tok::Backtick(name), span: Span::new(lo, self.pos) });
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    let lo = self.pos;
                    while let Some(c) = self.peek() {
                        if c.is_ascii_alphanumeric() || c == b'_' { self.pos += 1; } else { break; }
                    }
                    let word = std::str::from_utf8(&self.src[lo..self.pos]).unwrap_or("").to_string();
                    if word.chars().next().unwrap().is_ascii_uppercase() {
                        self.toks.push(Token { tok: Tok::Upper(word), span: Span::new(lo, self.pos) });
                    } else if KEYWORDS.contains(&word.as_str()) {
                        self.toks.push(Token { tok: Tok::Keyword(word), span: Span::new(lo, self.pos) });
                    } else {
                        self.toks.push(Token { tok: Tok::Ident(word), span: Span::new(lo, self.pos) });
                    }
                }
                _ => {
                    let lo = self.pos;
                    self.pos += 1;
                    self.errors.push(ParseError { span: Span::new(lo, self.pos), message: format!("无法识别的字符 {:?}", b as char) });
                }
            }
        }
        self.toks.push(Token { tok: Tok::Eof, span: Span::new(self.pos, self.pos) });
        (self.toks, self.errors)
    }
    fn lex_string(&mut self) {
        let lo = self.pos;
        self.pos += 1;
        let mut out = String::new();
        loop {
            match self.bump() {
                None => {
                    self.errors.push(ParseError { span: Span::new(lo, self.pos), message: "字符串未闭合".into() });
                    break;
                }
                Some(b'"') => break,
                Some(b'\\') => match self.bump() {
                    Some(b'n') => out.push('\n'),
                    Some(b't') => out.push('\t'),
                    Some(b'"') => out.push('"'),
                    Some(b'\\') => out.push('\\'),
                    Some(c) => out.push(c as char),
                    None => {}
                },
                Some(c) => out.push(c as char),
            }
        }
        self.toks.push(Token { tok: Tok::Str(out), span: Span::new(lo, self.pos) });
    }
}

/// Pre-identity expression: node numbers and fingerprints filled in by Builder.
#[derive(Clone, Debug)]
enum PExpr {
    Int(i64, Span),
    Str(String, Span),
    Bool(bool, Span),
    Var(String, Span),
    Abs(String, Span, Box<PExpr>),
    App(Box<PExpr>, Box<PExpr>, Span),
    Let(String, Span, bool, Box<PExpr>, Box<PExpr>),
    If(Box<PExpr>, Box<PExpr>, Box<PExpr>, Span),
    Record(Vec<(String, Span, PExpr)>, Span),
    Proj(Box<PExpr>, String, Span),
    Extend(Option<Box<PExpr>>, Vec<(String, Span, PExpr)>, Span),
    Inject(String, Span, Option<Box<PExpr>>, Span),
    Match(Box<PExpr>, Vec<PArm>, Option<PDefault>, Span),
}

#[derive(Clone, Debug)]
struct PArm { tag: String, tag_span: Span, payload: Option<(String, Span)>, body: PExpr }

#[derive(Clone, Debug)]
struct PDefault { binder: Option<(String, Span)>, body: PExpr }

impl PExpr {
    fn span(&self) -> Span {
        match self {
            PExpr::Int(_, s) | PExpr::Str(_, s) | PExpr::Bool(_, s) | PExpr::Var(_, s) => *s,
            PExpr::Abs(_, s, b) => s.merge(b.span()),
            PExpr::App(_, _, s) | PExpr::Let(_, s, _, _, _) => *s,
            PExpr::If(_, _, _, s) | PExpr::Record(_, s) | PExpr::Proj(_, _, s) |
            PExpr::Extend(_, _, s) | PExpr::Match(_, _, _, s) => *s,
            PExpr::Inject(_, _, _, s) => *s,
        }
    }
}

struct Parser {
    toks: Vec<Token>,
    idx: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    fn peek(&self) -> &Tok { &self.toks[self.idx].tok }
    fn peek_span(&self) -> Span { self.toks[self.idx].span }
    fn at_eof(&self) -> bool { matches!(self.peek(), Tok::Eof) }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.idx].clone();
        if !matches!(t.tok, Tok::Eof) { self.idx += 1; }
        t
    }
    fn eat_punct(&mut self, c: char) -> bool {
        if matches!(self.peek(), Tok::Punct(p) if *p == c) { self.bump(); true } else { false }
    }
    fn expect_punct(&mut self, c: char) -> bool {
        if self.eat_punct(c) { return true; }
        let s = self.peek_span();
        self.errors.push(ParseError { span: s, message: format!("期望 '{}'", c) });
        false
    }
    fn eat_kw(&mut self, k: &str) -> bool {
        if matches!(self.peek(), Tok::Keyword(w) if w == k) { self.bump(); true } else { false }
    }
    fn expect_kw(&mut self, k: &str) -> bool {
        if self.eat_kw(k) { return true; }
        let s = self.peek_span();
        self.errors.push(ParseError { span: s, message: format!("期望关键字 {}", k) });
        false
    }

    fn parse_program(&mut self) -> Option<PExpr> {
        let e = self.parse_expr();
        if !self.at_eof() {
            let s = self.peek_span();
            self.errors.push(ParseError { span: s, message: "表达式后有多余标记".into() });
        }
        e
    }

    fn parse_expr(&mut self) -> Option<PExpr> {
        match self.peek() {
            Tok::Keyword(k) if k == "let" => self.parse_let(),
            Tok::Keyword(k) if k == "fn" || k == "\\" => self.parse_abs(),
            Tok::Keyword(k) if k == "if" => self.parse_if(),
            Tok::Keyword(k) if k == "match" => self.parse_match(),
            _ => self.parse_app(),
        }
    }

    fn parse_let(&mut self) -> Option<PExpr> {
        let kw = self.bump();
        let rec = self.eat_kw("rec");
        let (name, ns) = self.parse_name()?;
        self.expect_punct('=');
        let rhs = self.parse_expr()?;
        self.eat_punct(';');
        let body = if self.at_eof() {
            // allow a top-level `let x = e` without `in`
            PExpr::Var(name.clone(), ns)
        } else {
            self.expect_kw("in");
            self.parse_expr()?
        };
        Some(PExpr::Let(name, kw.span, rec, Box::new(rhs), Box::new(body)))
    }

    fn parse_abs(&mut self) -> Option<PExpr> {
        let kw = self.bump();
        let (name, ns) = self.parse_name()?;
        self.expect_punct(':'); // separator for clarity; also accept '.'
        if !self.eat_punct('.') { /* ':' already consumed */ }
        let body = self.parse_expr()?;
        Some(PExpr::Abs(name, kw.span, Box::new(body)))
    }

    fn parse_if(&mut self) -> Option<PExpr> {
        let kw = self.bump();
        let cond = self.parse_expr()?;
        self.expect_kw("then");
        let then_e = self.parse_expr()?;
        self.expect_kw("else");
        let else_e = self.parse_expr()?;
        Some(PExpr::If(Box::new(cond), Box::new(then_e), Box::new(else_e), kw.span))
    }

    fn parse_match(&mut self) -> Option<PExpr> {
        let kw = self.bump();
        let scrut = self.parse_app()?;
        self.expect_kw("with");
        let mut arms = Vec::new();
        let mut default = None;
        loop {
            if self.eat_punct('|') { /* optional leading/separator bar */ }
            if self.eat_kw("default") {
                let binder = if let Tok::Ident(_) = self.peek() {
                    let t = self.bump();
                    if let Tok::Ident(n) = t.tok { Some((n, t.span)) } else { None }
                } else { None };
                self.expect_punct(':');
                let body = self.parse_expr()?;
                default = Some(PDefault { binder, body });
                break;
            }
            let tag_span = self.peek_span();
            let tag = match self.bump().tok {
                Tok::Backtick(t) => t,
                Tok::Eof => break,
                _ => {
                    self.errors.push(ParseError { span: tag_span, message: "match 分支需要 `标签".into() });
                    break;
                }
            };
            let payload = if let Tok::Ident(_) = self.peek() {
                let t = self.bump();
                if let Tok::Ident(n) = t.tok { Some((n, t.span)) } else { None }
            } else { None };
            self.expect_punct(':');
            let body = self.parse_expr()?;
            arms.push(PArm { tag, tag_span, payload, body });
            if matches!(self.peek(), Tok::Punct('|')) { continue; }
            break;
        }
        Some(PExpr::Match(Box::new(scrut), arms, default, kw.span))
    }

    fn parse_name(&mut self) -> Option<(String, Span)> {
        let t = self.bump();
        match t.tok {
            Tok::Ident(n) => Some((n, t.span)),
            Tok::Keyword(k) if k == "default" => Some((k, t.span)),
            _ => {
                self.errors.push(ParseError { span: t.span, message: "期望标识符".into() });
                None
            }
        }
    }

    fn parse_app(&mut self) -> Option<PExpr> {
        let mut head = self.parse_postfix()?;
        loop {
            match self.peek() {
                Tok::Int(_) | Tok::Str(_) | Tok::Ident(_) | Tok::Keyword(k)
                    if matches!(k.as_str(), "true" | "false" | "fn" | "if" | "match")
                        || k == "\\" =>
                {
                    let arg = self.parse_postfix()?;
                    let s = head.span().merge(arg.span());
                    head = PExpr::App(Box::new(head), Box::new(arg), s);
                }
                Tok::Punct(c) if *c == '(' || *c == '{' || *c == '[' => {
                    let arg = self.parse_postfix()?;
                    let s = head.span().merge(arg.span());
                    head = PExpr::App(Box::new(head), Box::new(arg), s);
                }
                Tok::Backtick(_) => {
                    let arg = self.parse_postfix()?;
                    let s = head.span().merge(arg.span());
                    head = PExpr::App(Box::new(head), Box::new(arg), s);
                }
                _ => break,
            }
        }
        Some(head)
    }

    fn parse_postfix(&mut self) -> Option<PExpr> {
        let mut e = self.parse_atom()?;
        while self.eat_punct('.') {
            let t = self.bump();
            match t.tok {
                Tok::Ident(n) => {
                    let s = e.span().merge(t.span);
                    e = PExpr::Proj(Box::new(e), n, s);
                }
                _ => self.errors.push(ParseError { span: t.span, message: "'.' 后需要字段名".into() }),
            }
        }
        Some(e)
    }

    fn parse_atom(&mut self) -> Option<PExpr> {
        let t = self.toks[self.idx].clone();
        match &t.tok {
            Tok::Int(v) => { self.bump(); Some(PExpr::Int(*v, t.span)) }
            Tok::Str(s) => { self.bump(); Some(PExpr::Str(s.clone(), t.span)) }
            Tok::Keyword(k) if *k == "true" || *k == "false" => {
                self.bump();
                Some(PExpr::Bool(*k == "true", t.span))
            }
            Tok::Ident(n) => { self.bump(); Some(PExpr::Var(n.clone(), t.span)) }
            Tok::Punct('(') => {
                self.bump();
                let e = self.parse_expr()?;
                self.expect_punct(')');
                Some(e)
            }
            Tok::Punct('{') => self.parse_record(),
            Tok::Backtick(tag) => {
                self.bump();
                // optional payload: `Tag atom
                let payload = match self.peek() {
                    Tok::Int(_) | Tok::Str(_) | Tok::Ident(_) | Tok::Backtick(_) | Tok::Punct('(')
                    | Tok::Punct('{') | Tok::Keyword(k) if *k == "true" || *k == "false" => {
                        let p = self.parse_postfix()?;
                        let end = p.span();
                        Some((Box::new(p), end))
                    }
                    _ => None,
                };
                let (payload, pend) = match payload {
                    Some((p, s)) => (Some(p), s),
                    None => (None, t.span),
                };
                Some(PExpr::Inject(tag.clone(), t.span, payload, t.span.merge(pend)))
            }
            _ => {
                if !matches!(t.tok, Tok::Eof) { self.bump(); }
                self.errors.push(ParseError { span: t.span, format_placeholder: () }.span_note());
                None
            }
        }
    }

    fn parse_record(&mut self) -> Option<PExpr> {
        let lb = self.bump();
        let mut base = None;
        // { expr | field = ... } extension form
        if let Tok::Ident(_) | Tok::Punct('(') = self.peek() {
            // lookahead: identifier followed by '|' means base; otherwise a field.
            if matches!(self.peek(), Tok::Ident(_)) {
                let save = self.idx;
                let name = if let Tok::Ident(n) = self.bump().tok { n } else { String::new() };
                if self.eat_punct('|') {
                    let b = PExpr::Var(name, self.toks[save].span);
                    base = Some(Box::new(b));
                } else {
                    self.idx = save;
                }
            }
        }
        let mut fields = Vec::new();
        loop {
            if matches!(self.peek(), Tok::Punct('}')) { break; }
            let ft = self.bump();
            let (fname, fspan) = match ft.tok {
                Tok::Ident(n) => (n, ft.span),
                Tok::Eof => break,
                _ => {
                    self.errors.push(ParseError { span: ft.span, message: "记录中需要字段名".into() });
                    break;
                }
            };
            self.expect_punct('=');
            let val = self.parse_expr()?;
            fields.push((fname, fspan, val));
            if !self.eat_punct(',') { break; }
        }
        self.expect_punct('}');
        let end = self.peek_span();
        let span = lb.span.merge(end);
        if base.is_some() {
            Some(PExpr::Extend(base, fields, span))
        } else {
            Some(PExpr::Record(fields, span))
        }
    }
}
