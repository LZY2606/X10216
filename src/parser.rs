use crate::syntax::*;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
enum TokKind { Ident(Rc<str>), Tag(Rc<str>), Int(i64), Str(Rc<str>), Punct(char), Op(Rc<str>) }

#[derive(Clone, Debug)]
struct Tok { kind: TokKind, span: Span }

struct Lexer<'a> { src: &'a [u8], pos: usize, out: Vec<Tok> }

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self { Self { src: src.as_bytes(), pos: 0, out: Vec::new() } }

    fn lex(mut self) -> Result<Vec<Tok>, ParseError> {
        while self.pos < self.src.len() {
            let c = self.src[self.pos];
            if c.is_ascii_whitespace() { self.pos += 1; continue; }
            if c == b'/' && self.peek(1) == Some(b'/') { while self.pos < self.src.len() && self.src[self.pos] != b'\n' { self.pos += 1; } continue; }
            let start = self.pos;
            match c {
                b'0'..=b'9' => {
                    let mut v = 0i64;
                    while let Some(d) = self.cur().filter(|c| c.is_ascii_digit()) { v = v.wrapping_mul(10).wrapping_add((d - b'0') as i64); self.pos += 1; }
                    self.push(TokKind::Int(v), start);
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    while let Some(_c) = self.cur().filter(|c| c.is_ascii_alphanumeric() || *c == b'_') { self.pos += 1; }
                    let s = Rc::from(std::str::from_utf8(&self.src[start..self.pos]).unwrap_or(""));
                    self.push(TokKind::Ident(s), start);
                }
                b'"' => {
                    self.pos += 1;
                    let mut text = String::new();
                    loop {
                        match self.cur() {
                            None => return Err(self.err("unterminated string", start)),
                            Some(b'"') => { self.pos += 1; break; }
                            Some(b'\\') => {
                                self.pos += 1;
                                match self.cur() { Some(b'n') => text.push('\n'), Some(b't') => text.push('\t'), Some(b'"') => text.push('"'), Some(b'\\') => text.push('\\'), _ => return Err(self.err("bad escape", self.pos.saturating_sub(1))) }
                                self.pos += 1;
                            }
                            Some(_) => { text.push(self.src[self.pos] as char); self.pos += 1; }
                        }
                    }
                    self.push(TokKind::Str(Rc::from(text.as_str())), start);
                }
                b'#' => {
                    self.pos += 1;
                    while let Some(_c) = self.cur().filter(|c| c.is_ascii_alphanumeric() || *c == b'_') { self.pos += 1; }
                    if self.pos == start + 1 { return Err(self.err("expected variant tag after '#'", start)); }
                    let tag = Rc::from(std::str::from_utf8(&self.src[start + 1..self.pos]).unwrap_or(""));
                    self.push(TokKind::Tag(tag), start);
                }
                b'{' | b'}' | b'(' | b')' | b'.' | b',' | b';' | b'=' | b'>' => {
                    if c == b'>' { self.pos += 1; } else if c == b'=' && self.peek(1) == Some(b'>') { self.pos += 2; } else { self.pos += 1; }
                    if start + 2 == self.pos && &self.src[start..self.pos] == b"=>" { self.push(TokKind::Op(Rc::from("=>")), start); }
                    else { self.push(TokKind::Punct(c as char), start); }
                }
                b'+' | b'-' | b'*' | b'<' => {
                    self.pos += 1;
                    if c == b'=' && self.cur() == Some(b'=') { self.pos += 1; }
                    let s = Rc::from(std::str::from_utf8(&self.src[start..self.pos]).unwrap_or(""));
                    self.push(TokKind::Op(s), start);
                }
                _ => return Err(self.err("unexpected character", start)),
            }
        }
        Ok(self.out)
    }
    fn cur(&self) -> Option<u8> { self.src.get(self.pos).copied() }
    fn peek(&self, n: usize) -> Option<u8> { self.src.get(self.pos + n).copied() }
    fn push(&mut self, kind: TokKind, start: usize) { let span = Span::new(start, self.pos); self.out.push(Tok { kind, span }); }
    fn err(&self, msg: &str, start: usize) -> ParseError { ParseError { message: msg.to_string(), span: Span::new(start, (start + 1).min(self.src.len())) } }
}

pub struct Parser { toks: Vec<Tok>, pos: usize }

impl Parser {
    pub fn parse(src: &str) -> Result<PExpr, ParseError> {
        let toks = Lexer::new(src).lex()?;
        let mut p = Parser { toks, pos: 0 };
        let e = p.expr(0)?;
        if p.pos < p.toks.len() { return Err(p.err_at("unexpected token", p.toks[p.pos].span)); }
        Ok(e)
    }

    fn expr(&mut self, min_bp: u8) -> Result<PExpr, ParseError> {
        let mut left = self.unary()?;
        loop {
            if self.check_punct('.') {
                self.pos += 1;
                let (name, name_span) = self.ident()?;
                let span = left.span().join(name_span);
                left = rc(Expr::Field { record: left, name, name_span, span });
                continue;
            }
            if self.check_punct('(') {
                let arg = self.paren_expr()?;
                let span = left.span().join(arg.span());
                left = rc(Expr::App { fun: left, arg, span });
                continue;
            }
            let Some(op) = self.bin_op() else { break; };
            let (lbp, rbp) = op_bp(op);
            if lbp < min_bp { break; }
            self.pos += 1;
            let right = self.expr(rbp)?;
            let span = left.span().join(right.span());
            left = rc(Expr::Bin { op, left, right, span });
        }
        Ok(left)
    }

    fn unary(&mut self) -> Result<PExpr, ParseError> {
        let t = self.peek().ok_or_else(|| self.err_eof())?;
        match &t.kind {
            TokKind::Int(v) => { let v = *v; let span = t.span; self.pos += 1; Ok(rc(Expr::Int { value: v, span })) }
            TokKind::Str(s) => { let s = s.clone(); let span = t.span; self.pos += 1; Ok(rc(Expr::Str { value: s, span })) }
            TokKind::Tag(s) => {
                let tag = s.clone(); let tag_span = t.span; self.pos += 1;
                self.punct('(')?;
                let payload = self.expr(0)?;
                let close = self.punct(')')?;
                Ok(rc(Expr::Variant { tag, payload, span: tag_span.join(close) }))
            }
            TokKind::Ident(s) => {
                let s = s.clone(); let span = t.span; self.pos += 1;
                match s.as_ref() {
                    "true" => Ok(rc(Expr::Bool { value: true, span })),
                    "false" => Ok(rc(Expr::Bool { value: false, span })),
                    "fn" => self.lambda_after(span),
                    "if" => self.if_after(span),
                    "let" => self.let_after(span),
                    "match" => self.match_after(span),
                    _ => Ok(rc(Expr::Var { name: s, span })),
                }
            }
            TokKind::Punct('(') => self.paren_expr(),
            TokKind::Punct('{') => self.record_after(t.span),
            _ => Err(self.err_at("expected expression", t.span)),
        }
    }

    fn lambda_after(&mut self, kw: Span) -> Result<PExpr, ParseError> {
        let (param, param_span) = self.ident()?;
        self.op("=>")?;
        let body = self.expr(0)?;
        let span = kw.join(body.span());
        Ok(rc(Expr::Lam { param, param_span, body, span }))
    }

    fn if_after(&mut self, kw: Span) -> Result<PExpr, ParseError> {
        let cond = self.expr(0)?;
        self.keyword("then")?;
        let then_branch = self.expr(0)?;
        self.keyword("else")?;
        let else_branch = self.expr(0)?;
        let span = kw.join(else_branch.span());
        Ok(rc(Expr::If { cond, then_branch, else_branch, span }))
    }

    fn let_after(&mut self, kw: Span) -> Result<PExpr, ParseError> {
        let recursive = if self.check_keyword("rec") { self.pos += 1; true } else { false };
        let (name, name_span) = self.ident()?;
        self.punct('=')?;
        let value = self.expr(0)?;
        self.keyword("in")?;
        let body = self.expr(0)?;
        let span = kw.join(body.span());
        Ok(rc(Expr::Let { recursive, name, name_span, value, body, span }))
    }

    fn record_after(&mut self, open: Span) -> Result<PExpr, ParseError> {
        self.pos += 1;
        let mut fields = Vec::new();
        if !self.check_punct('}') {
            loop {
                let (name, _) = self.ident()?;
                self.punct('=')?;
                let value = self.expr(0)?;
                fields.push((name, value));
                if self.check_punct(',') { self.pos += 1; } else { break; }
            }
        }
        let close = self.punct('}')?;
        Ok(rc(Expr::Record { fields, span: open.join(close) }))
    }

    fn match_after(&mut self, kw: Span) -> Result<PExpr, ParseError> {
        let scrutinee = self.unary()?;
        let _open = self.punct('{')?;
        let mut arms = Vec::new();
        let mut fallback = None;
        while !self.check_punct('}') {
            if self.check_punct('_') {
                self.pos += 1;
                self.op("=>")?;
                fallback = Some(self.expr(0)?);
            } else {
                let t = self.cur().ok_or_else(|| self.err_eof())?.clone();
                let TokKind::Tag(tag) = t.kind else { return Err(self.err_at("expected #tag in match arm", t.span)); };
                let tag_span = t.span;
                self.pos += 1;
                let (binding, binding_span) = self.ident()?;
                self.op("=>")?;
                let body = self.expr(0)?;
                arms.push(MatchArm { tag, tag_span, binding, binding_span, body });
            }
            if self.check_punct(';') || self.check_punct(',') { self.pos += 1; } else { break; }
        }
        let close = self.punct('}')?;
        Ok(rc(Expr::Match { scrutinee, arms, fallback, span: kw.join(close) }))
    }

    fn paren_expr(&mut self) -> Result<PExpr, ParseError> {
        let _open = self.punct('(')?;
        let e = self.expr(0)?;
        let _close = self.punct(')')?;
        Ok(e)
    }

    fn bin_op(&self) -> Option<BinOp> {
        let t = self.toks.get(self.pos)?;
        match &t.kind {
            TokKind::Op(s) => match s.as_ref() { "+" => Some(BinOp::Add), "-" => Some(BinOp::Sub), "*" => Some(BinOp::Mul), "==" => Some(BinOp::Eq), "<" => Some(BinOp::Lt), _ => None },
            _ => None,
        }
    }
    fn check_punct(&self, c: char) -> bool { matches!(self.toks.get(self.pos).map(|t| &t.kind), Some(TokKind::Punct(x)) if *x == c) }
    fn check_keyword(&self, s: &str) -> bool { matches!(self.toks.get(self.pos).map(|t| &t.kind), Some(TokKind::Ident(x)) if x.as_ref() == s) }
    fn punct(&mut self, c: char) -> Result<Span, ParseError> {
        if !self.check_punct(c) {
            let span = self.toks.get(self.pos).map(|t| t.span).unwrap_or_else(|| Span::new(0, 0));
            return Err(self.err_at(&format!("expected '{c}'"), span));
        }
        let s = self.toks[self.pos].span; self.pos += 1; Ok(s)
    }
    fn op(&mut self, s: &str) -> Result<(), ParseError> {
        let ok = matches!(self.toks.get(self.pos).map(|t| &t.kind), Some(TokKind::Op(x)) if x.as_ref() == s);
        if !ok { let span = self.toks.get(self.pos).map(|t| t.span).unwrap_or_else(|| Span::new(0,0)); return Err(self.err_at(&format!("expected '{s}'"), span)); }
        self.pos += 1; Ok(())
    }
    fn keyword(&mut self, s: &str) -> Result<(), ParseError> {
        if !self.check_keyword(s) { let span = self.toks.get(self.pos).map(|t| t.span).unwrap_or_else(|| Span::new(0,0)); return Err(self.err_at(&format!("expected '{s}'"), span)); }
        self.pos += 1; Ok(())
    }
    fn ident(&mut self) -> Result<(Rc<str>, Span), ParseError> {
        let t = self.toks.get(self.pos).ok_or_else(|| self.err_eof())?;
        if let TokKind::Ident(s) = &t.kind { let s = s.clone(); let span = t.span; self.pos += 1; return Ok((s, span)); }
        Err(self.err_at("expected identifier", t.span))
    }
    fn pop_span(&mut self) -> Span { let s = self.toks[self.pos].span; s }
    fn cur(&self) -> Option<&Tok> { self.toks.get(self.pos) }
    fn peek(&self) -> Option<&Tok> { self.toks.get(self.pos) }
    fn err_eof(&self) -> ParseError { ParseError { message: "unexpected end of input".to_string(), span: Span::new(self.src_len().saturating_sub(1), self.src_len()) } }
    fn src_len(&self) -> usize { self.toks.last().map(|t| t.span.end as usize).unwrap_or(0) }
    fn err_at(&self, msg: &str, span: Span) -> ParseError { ParseError { message: msg.to_string(), span } }
}

fn op_bp(op: BinOp) -> (u8, u8) {
    match op {
        BinOp::Eq | BinOp::Lt => (2, 3),
        BinOp::Add | BinOp::Sub => (4, 5),
        BinOp::Mul => (6, 7),
    }
}

fn rc(e: Expr) -> PExpr { Rc::new(e) }
