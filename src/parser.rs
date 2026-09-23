use crate::ast::*;
use crate::span::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok {
    Ident(String),
    Int(i64),
    Let, Rec, In, Fun, Arrow, DArrow, Eq, If, Then, Else,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Colon, Dot, Bar, Backtick(String), True, False, Unit,
    Match, With, End, Eof,
}

#[derive(Clone, Debug)]
pub struct Token { pub tok: Tok, pub lo: usize, pub hi: usize }

pub struct Lexer<'a> { src: &'a [u8], pos: usize }

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self { Lexer { src: src.as_bytes(), pos: 0 } }

    fn skip_ws(&mut self) {
        while self.pos < self.src.len() {
            let c = self.src[self.pos];
            if c.is_ascii_whitespace() {
                self.pos += 1;
            } else if c == b'/' && self.pos + 1 < self.src.len() && self.src[self.pos + 1] == b'/' {
                while self.pos < self.src.len() && self.src[self.pos] != b'\n' { self.pos += 1; }
            } else if c == b'(' && self.pos + 1 < self.src.len() && self.src[self.pos + 1] == b'*' {
                self.pos += 2;
                while self.pos + 1 < self.src.len() && !(self.src[self.pos] == b'*' && self.src[self.pos + 1] == b')') {
                    self.pos += 1;
                }
                self.pos = (self.pos + 2).min(self.src.len());
            } else { break; }
        }
    }

    pub fn next_tok(&mut self) -> Result<Token, (String, Span)> {
        self.skip_ws();
        let lo = self.pos;
        if self.pos >= self.src.len() {
            return Ok(Token { tok: Tok::Eof, lo, hi: lo });
        }
        let c = self.src[self.pos];
        let single = |t: Tok, n: usize, slf: &mut Self| -> Token {
            slf.pos += n;
            Token { tok: t, lo, hi: slf.pos }
        };
        match c {
            b'(' => Ok(single(Tok::LParen, 1, self)),
            b')' => Ok(single(Tok::RParen, 1, self)),
            b'{' => Ok(single(Tok::LBrace, 1, self)),
            b'}' => Ok(single(Tok::RBrace, 1, self)),
            b'[' => Ok(single(Tok::LBracket, 1, self)),
            b']' => Ok(single(Tok::RBracket, 1, self)),
            b',' => Ok(single(Tok::Comma, 1, self)),
            b'.' => Ok(single(Tok::Dot, 1, self)),
            b'|' => Ok(single(Tok::Bar, 1, self)),
            b':' => {
                self.pos += 1;
                Ok(Token { tok: Tok::Colon, lo, hi: self.pos })
            }
            b'-' => {
                if self.pos + 1 < self.src.len() && self.src[self.pos + 1] == b'>' {
                    self.pos += 2;
                    Ok(Token { tok: Tok::Arrow, lo, hi: self.pos })
                } else { Err((format!("unexpected character '-'"), Span::new(lo, lo + 1))) }
            }
            b'=' => {
                if self.pos + 1 < self.src.len() && self.src[self.pos + 1] == b'>' {
                    self.pos += 2;
                    Ok(Token { tok: Tok::DArrow, lo, hi: self.pos })
                } else { Ok(Token { tok: Tok::Eq, lo, hi: self.pos }) }
            }
            b'`' => {
                self.pos += 1;
                let s = self.pos;
                while self.pos < self.src.len() && !self.src[self.pos].is_ascii_whitespace()
                    && !matches!(self.src[self.pos], b'`' | b'}' | b')' | b']' | b',' | b';') {
                    self.pos += 1;
                }
                let name = std::str::from_utf8(&self.src[s..self.pos]).unwrap_or("").to_string();
                if self.pos < self.src.len() && self.src[self.pos] == b'`' { self.pos += 1; }
                Ok(Token { tok: Tok::Backtick(name), lo, hi: self.pos })
            }
            b'0'..=b'9' => {
                while self.pos < self.src.len() && self.src[self.pos].is_ascii_digit() { self.pos += 1; }
                let text = std::str::from_utf8(&self.src[lo..self.pos]).unwrap_or("0");
                let v: i64 = text.parse().unwrap_or(0);
                Ok(Token { tok: Tok::Int(v), lo, hi: self.pos })
            }
            _ if c.is_ascii_alphabetic() || c == b'_' => {
                while self.pos < self.src.len()
                    && (self.src[self.pos].is_ascii_alphanumeric() || self.src[self.pos] == b'_') {
                    self.pos += 1;
                }
                let word = std::str::from_utf8(&self.src[lo..self.pos]).unwrap_or("");
                let t = match word {
                    "let" => Tok::Let, "rec" => Tok::Rec, "in" => Tok::In,
                    "fun" => Tok::Fun, "if" => Tok::If, "then" => Tok::Then,
                    "else" => Tok::Else, "true" => Tok::True, "false" => Tok::False,
                    "unit" => Tok::Unit, "match" => Tok::Match, "with" => Tok::With,
                    "end" => Tok::End,
                    _ => Tok::Ident(word.to_string()),
                };
                Ok(Token { tok: t, lo, hi: self.pos })
            }
            _ => Err((format!("unexpected character {:?}", c as char), Span::new(lo, lo + 1))),
        }
    }
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct ParseError { pub message: String, pub span: Span }

pub struct Parser {
    toks: Vec<Token>,
    pos: usize,
    ast: Ast,
    binders: Vec<String>,
    pub errors: Vec<ParseError>,
}

type R = NodeId;

impl Parser {
    pub fn parse(src: &str) -> Result<Ast, Vec<ParseError>> {
        let mut lex = Lexer::new(src);
        let mut toks = Vec::new();
        let mut errors = Vec::new();
        loop {
            match lex.next_tok() {
                Ok(t) => {
                    let eof = matches!(t.tok, Tok::Eof);
                    toks.push(t);
                    if eof { break; }
                }
                Err((m, sp)) => {
                    errors.push(ParseError { message: m, span: sp });
                    return Err(errors);
                }
            }
        }
        let mut p = Parser { toks, pos: 0, ast: Ast { nodes: Vec::new(), root: 0 }, binders: Vec::new(), errors };
        let root = p.atom_pos();
        p.expect(Tok::Eof, "expected end of input");
        p.ast.root = root;
        p.ast.compute_hashes();
        if p.errors.is_empty() { Ok(p.ast) } else { Err(p.errors) }
    }

    fn peek(&self) -> &Tok { &self.toks[self.pos].tok }
    fn span(&self) -> Span { Span::new(self.toks[self.pos].lo, self.toks[self.pos].hi) }
    fn last_span(&self) -> Span {
        let t = &self.toks[self.pos.saturating_sub(1).max(0)];
        Span::new(t.lo, t.hi)
    }
    fn bump(&mut self) -> Tok {
        let t = self.toks[self.pos].tok.clone();
        if !matches!(t, Tok::Eof) { self.pos += 1; }
        t
    }
    fn eat(&mut self, t: Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            self.bump();
            true
        } else { false }
    }
    fn expect(&mut self, t: Tok, msg: &str) {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            self.bump();
        } else {
            self.error(msg);
        }
    }
    fn error(&mut self, msg: &str) {
        self.errors.push(ParseError { message: msg.to_string(), span: self.span() });
    }

    fn atom_pos(&mut self) -> usize { self.toks[self.pos].lo }

    fn push_binder(&mut self, name: &str) {
        self.binders.push(name.to_string());
    }
    fn pop_binder(&mut self) { self.binders.pop(); }
    fn resolve_var(&self, name: &str) -> Option<u32> {
        self.binders.iter().rev().position(|b| b == name).map(|d| d as u32)
    }

    /// expression ::= fun_expr | let_expr | if_expr | match_expr | app_expr
    pub fn expr(&mut self) -> R {
        match self.peek() {
            Tok::Fun => self.fun_expr(),
            Tok::Let => self.let_expr(),
            Tok::If => self.if_expr(),
            Tok::Match => self.match_expr(),
            _ => self.app_expr(),
        }
    }

    fn fun_expr(&mut self) -> R {
        let lo = self.atom_pos();
        self.bump(); // fun
        let mut names: Vec<(String, Span)> = Vec::new();
        loop {
            match self.peek().clone() {
                Tok::Ident(n) => { let sp = self.span(); self.bump(); names.push((n, sp)); }
                _ => { self.error("expected parameter name after fun"); break; }
            }
            if matches!(self.peek(), Tok::Arrow) { self.bump(); break; }
        }
        for (n, _) in &names { self.push_binder(n); }
        let body = self.expr();
        let mut result = body;
        for (n, sp) in names.iter().rev() {
            let var = self.var_from_binder(n, sp);
            let hi = self.ast.node(result).span.hi;
            result = self.ast.add(NodeKind::Lam, Span::new(lo, hi as usize), None, vec![var, result]);
            self.pop_binder();
        }
        result
    }

    fn var_from_binder(&mut self, name: &str, sp: &Span) -> R {
        match self.resolve_var(name) {
            Some(depth) => self.ast.add(NodeKind::Var { depth }, *sp, Some(name.to_string()), vec![]),
            None => {
                self.errors.push(ParseError { message: format!("internal: binder {} missing", name), span: *sp });
                self.ast.add(NodeKind::Var { depth: u32::MAX }, *sp, Some(name.to_string()), vec![])
            }
        }
    }

    fn let_expr(&mut self) -> R {
        let lo = self.atom_pos();
        self.bump();
        let rec = self.eat(Tok::Rec);
        let name = match self.peek().clone() {
            Tok::Ident(n) => { self.bump(); n }
            _ => { self.error("expected binding name after let"); "_".to_string() }
        };
        // curried parameter sugar: let f x y = body ; let rec loop = ...
        let mut params: Vec<(String, Span)> = Vec::new();
        while let Tok::Ident(n) = self.peek().clone() {
            if matches!(self.toks.get(self.pos + 1).map(|t| &t.tok), Some(Tok::Eq)) { break; }
            let sp = self.span();
            self.bump();
            params.push((n, sp));
        }
        self.expect(Tok::Eq, "expected '=' in let");
        if rec { self.push_binder(&name); }
        for (p, _) in &params { self.push_binder(p); }
        let _value_start = self.atom_pos();
        let mut value = self.expr();
        // Wrap value in lambdas *outside* the parameters' scopes; record the span
        // before unwrapping by stashing on the binder var below is unnecessary, so
        // recompute from the first parameter token.
        let value_lo = params.first().map(|(_, sp)| sp.lo).unwrap_or_else(|| self.ast.node(value).span.lo);
        for (p, sp) in params.iter().rev() {
            let var = self.var_from_binder(p, sp);
            let hi = self.ast.node(value).span.hi;
            value = self.ast.add(NodeKind::Lam, Span::new(value_lo as usize, hi as usize), None, vec![var, value]);
            self.pop_binder();
        }
        // Binder reference node: points at the let name; depth is relative to the
        // environment where the value is inferred (recursive name already pushed).
        let bind_hi = lo + 3 + if rec { 4 } else { 0 } + name.len();
        let bind_depth = if rec {
            self.resolve_var(&name).unwrap_or(0)
        } else {
            self.binders.len() as u32
        };
        let bind = self.ast.add(NodeKind::Var { depth: bind_depth },
            Span::new(lo + if rec { 7 } else { 4 }, bind_hi), Some(name.clone()), vec![]);
        self.expect(Tok::In, "expected 'in' in let");
        if !rec { self.push_binder(&name); }
        let body = self.expr();
        if rec { self.pop_binder(); } else { self.pop_binder(); }
        let hi = self.ast.node(body).span.hi;
        self.ast.add(NodeKind::Let { rec }, Span::new(lo, hi as usize), Some(name), vec![bind, value, body])
    }

    fn if_expr(&mut self) -> R {
        let lo = self.atom_pos();
        self.bump();
        let cond = self.expr();
        self.expect(Tok::Then, "expected 'then'");
        let then_e = self.expr();
        self.expect(Tok::Else, "expected 'else'");
        let else_e = self.expr();
        let hi = self.ast.node(else_e).span.hi;
        self.ast.add(NodeKind::If, Span::new(lo, hi as usize), None, vec![cond, then_e, else_e])
    }

    fn match_expr(&mut self) -> R {
        let lo = self.atom_pos();
        self.bump();
        let scrut = self.expr();
        self.expect(Tok::With, "expected 'with' after match scrutinee");
        let mut arms = Vec::new();
        loop {
            if matches!(self.peek(), Tok::End) { break; }
            self.eat(Tok::Bar);
            let label = match self.peek().clone() {
                Tok::Backtick(s) => { self.bump(); s }
                _ => { self.error("expected variant tag like `Tag in match arm"); self.recover_arm(); break; }
            };
            let (has_binder, binder) = match self.peek().clone() {
                Tok::Ident(n) => { self.bump(); (true, Some(n)) }
                _ => (false, None),
            };
            self.expect(Tok::DArrow, "expected '=>' in match arm");
            let arm_lo = self.atom_pos();
            if let Some(b) = &binder { self.push_binder(b); }
            let body = self.expr();
            let hi = self.ast.node(body).span.hi;
            // Arm binder node is a Binder slot; body uses normal de Bruijn Var nodes.
            let mut children = Vec::new();
            if let Some(b) = &binder {
                let bnode = self.ast.add(NodeKind::Binder,
                    Span::new(arm_lo, arm_lo + b.len()), Some(b.clone()), vec![]);
                children.push(bnode);
            }
            children.push(body);
            if let Some(b) = &binder { self.pop_binder(); }
            let arm = self.ast.add(NodeKind::Arm { has_binder }, Span::new(arm_lo, hi as usize),
                Some(label), children);
            arms.push(arm);
            if !matches!(self.peek(), Tok::Bar) { break; }
        }
        self.expect(Tok::End, "expected 'end' to close match");
        let hi = self.toks.iter().rev().find(|t| !matches!(t.tok, Tok::Eof)).map(|t| t.hi).unwrap_or(lo);
        self.ast.add(NodeKind::Switch, Span::new(lo, hi), None, {
            let mut ch = vec![scrut];
            ch.extend(arms);
            ch
        })
    }

    fn recover_arm(&mut self) {
        while !matches!(self.peek(), Tok::Bar | Tok::End | Tok::Eof) { self.bump(); }
    }

    fn app_expr(&mut self) -> R {
        let mut func = self.postfix();
        loop {
            if self.atom_starts() {
                let arg = self.postfix();
                let hi = self.ast.node(arg).span.hi;
                let lo = self.ast.node(func).span.lo;
                func = self.ast.add(NodeKind::App, Span::new(lo as usize, hi as usize), None, vec![func, arg]);
            } else { break; }
        }
        func
    }

    fn atom_starts(&self) -> bool {
        matches!(self.peek(),
            Tok::Ident(_) | Tok::Int(_) | Tok::True | Tok::False | Tok::Unit
            | Tok::LParen | Tok::LBrace | Tok::Backtick(_))
    }

    fn postfix(&mut self) -> R {
        let mut e = self.atom();
        while matches!(self.peek(), Tok::Dot) {
            self.bump();
            let label = match self.peek().clone() {
                Tok::Ident(n) => { self.bump(); n }
                _ => { self.error("expected field name after '.'"); "_".to_string() }
            };
            let hi = self.last_span().hi;
            let lo = self.ast.node(e).span.lo;
            e = self.ast.add(NodeKind::Proj, Span::new(lo as usize, hi as usize), Some(label), vec![e]);
        }
        e
    }

    fn atom(&mut self) -> R {
        let lo = self.atom_pos();
        match self.peek().clone() {
            Tok::Int(v) => { self.bump(); self.lit(NodeKind::Lit(Lit::Int(v)), lo, self.last_span().hi) }
            Tok::True => { self.bump(); self.lit(NodeKind::Lit(Lit::Bool(true)), lo, self.last_span().hi) }
            Tok::False => { self.bump(); self.lit(NodeKind::Lit(Lit::Bool(false)), lo, self.last_span().hi) }
            Tok::Unit => { self.bump(); self.lit(NodeKind::Lit(Lit::Unit), lo, self.last_span().hi) }
            Tok::Ident(n) => {
                self.bump();
                match self.resolve_var(&n) {
                    Some(depth) => self.ast.add(NodeKind::Var { depth }, Span::new(lo, self.last_span().hi), Some(n), vec![]),
                    None => {
                        self.errors.push(ParseError {
                            message: format!("unbound variable '{}'", n),
                            span: Span::new(lo, self.last_span().hi),
                        });
                        self.ast.add(NodeKind::Var { depth: u32::MAX }, Span::new(lo, self.last_span().hi), Some(n), vec![])
                    }
                }
            }
            Tok::LParen => {
                self.bump();
                if matches!(self.peek(), Tok::RParen) {
                    self.bump();
                    return self.lit(NodeKind::Lit(Lit::Unit), lo, self.last_span().hi);
                }
                let e = self.expr();
                self.expect(Tok::RParen, "expected ')'");
                e
            }
            Tok::Backtick(label) => {
                self.bump();
                // `Tag payload  (atomic application of the injection constructor)
                let tag_hi = self.last_span().hi;
                let tag = self.ast.add(NodeKind::Variant, Span::new(lo, tag_hi), Some(label), vec![]);
                if self.atom_starts() {
                    let arg = self.postfix();
                    let hi = self.ast.node(arg).span.hi;
                    self.ast.add(NodeKind::App, Span::new(lo, hi as usize), None, vec![tag, arg])
                } else {
                    tag
                }
            }
            Tok::LBrace => self.record(lo),
            _ => {
                self.error("expected an expression");
                let sp = self.span();
                self.ast.add(NodeKind::Lit(Lit::Unit), sp, None, vec![])
            }
        }
    }

    fn lit(&mut self, k: NodeKind, lo: usize, hi: usize) -> R {
        self.ast.add(k, Span::new(lo, hi), None, vec![])
    }

    fn record(&mut self, lo: usize) -> R {
        self.bump();
        let mut fields = Vec::new();
        if !matches!(self.peek(), Tok::RBrace) {
            loop {
                let label = match self.peek().clone() {
                    Tok::Ident(n) => { self.bump(); n }
                    _ => { self.error("expected field name in record"); self.recover_record(); break; }
                };
                self.expect(Tok::Eq, "expected '=' after field name");
                let val = self.expr();
                let hi = self.ast.node(val).span.hi;
                let f = self.ast.add(NodeKind::RecordField, Span::new(lo, hi as usize), Some(label), vec![val]);
                fields.push(f);
                if self.eat(Tok::Comma) {
                    if matches!(self.peek(), Tok::RBrace) { break; }
                    continue;
                }
                break;
            }
        }
        self.expect(Tok::RBrace, "expected '}' to close record");
        let hi = self.last_span().hi;
        self.ast.add(NodeKind::Record, Span::new(lo, hi), None, fields)
    }

    fn recover_record(&mut self) {
        while !matches!(self.peek(), Tok::Comma | Tok::RBrace | Tok::Eof) { self.bump(); }
    }
}

pub fn parse(src: &str) -> Result<Ast, Vec<ParseError>> { Parser::parse(src) }
