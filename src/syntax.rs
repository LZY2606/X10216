//! 词法与语法：span、token、AST、递归下降解析器。

use serde::{Deserialize, Serialize};

/// 源码区间，按字节偏移。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
    pub fn merge(&self, other: Span) -> Span {
        Span { lo: self.lo.min(other.lo), hi: self.hi.max(other.hi) }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tok {
    // 字面量与名字
    Int, Bool, Ident, Upper,
    // 关键字
    Let, In, Rec, Fun, If, Then, Else, True, False, Match, With, Unit,
    // 标点
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Dot, Colon, Eq, Arrow, FatArrow, Pipe, Backslash, Star, Semicolon,
    // 运算符
    Plus, Minus, Lt,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub text: String,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0 }
    }

    fn bump(&mut self) -> Option<u8> {
        let b = self.src.get(self.pos).copied();
        if b.is_some() { self.pos += 1; }
        b
    }
    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }
    fn peek2(&self) -> Option<u8> {
        self.src.get(self.pos + 1).copied()
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, (String, Span)> {
        let mut out = Vec::new();
        loop {
            while let Some(b) = self.peek() {
                if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' { self.pos += 1; } else { break; }
            }
            let lo = self.pos;
            let Some(b) = self.peek() else {
                out.push(Token { tok: Tok::Eof, text: String::new(), span: Span::new(lo, lo) });
                return Ok(out);
            };
            // 注释
            if b == b'/' && self.peek2() == Some(b'/') {
                while let Some(c) = self.peek() {
                    self.pos += 1;
                    if c == b'\n' { break; }
                }
                continue;
            }
            if b == b'/' && self.peek2() == Some(b'*') {
                self.pos += 2;
                let mut depth = 1;
                while depth > 0 {
                    match self.bump() {
                        Some(b'/') if self.peek() == Some(b'*') => { self.pos += 1; depth += 1; }
                        Some(b'*') if self.peek() == Some(b'/') => { self.pos += 1; depth -= 1; }
                        Some(_) => {}
                        None => return Err(("未闭合的块注释".into(), Span::new(lo, self.pos))),
                    }
                }
                continue;
            }
            if b.is_ascii_digit() {
                self.pos += 1;
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() { self.pos += 1; } else { break; }
                }
                out.push(Token { tok: Tok::Int, text: self.text(lo), span: Span::new(lo, self.pos) });
                continue;
            }
            if b == b'_' || b.is_ascii_alphabetic() {
                self.pos += 1;
                while let Some(c) = self.peek() {
                    if c == b'_' || c.is_ascii_alphanumeric() { self.pos += 1; } else { break; }
                }
                let text = self.text(lo);
                let tok = match text.as_str() {
                    "let" => Tok::Let, "in" => Tok::In, "rec" => Tok::Rec,
                    "fun" => Tok::Fun, "if" => Tok::If, "then" => Tok::Then,
                    "else" => Tok::Else, "true" => Tok::True, "false" => Tok::False,
                    "match" => Tok::Match, "with" => Tok::With, "unit" => Tok::Unit,
                    _ => {
                        let first = text.bytes().next().unwrap();
                        if first.is_ascii_uppercase() { Tok::Upper } else { Tok::Ident }
                    }
                };
                out.push(Token { tok, text, span: Span::new(lo, self.pos) });
                continue;
            }
            // 多字符标点
            let two: &[u8] = if self.pos + 2 <= self.src.len() { &self.src[self.pos..self.pos + 2] } else { b"" };
            if two == b"->" { self.pos += 2; out.push(Token { tok: Tok::Arrow, text: "->".into(), span: Span::new(lo, self.pos) }); continue; }
            if two == b"=>" { self.pos += 2; out.push(Token { tok: Tok::FatArrow, text: "=>".into(), span: Span::new(lo, self.pos) }); continue; }
            let (tok, len) = match b {
                b'(' => (Tok::LParen, 1), b')' => (Tok::RParen, 1),
                b'{' => (Tok::LBrace, 1), b'}' => (Tok::RBrace, 1),
                b'[' => (Tok::LBracket, 1), b']' => (Tok::RBracket, 1),
                b',' => (Tok::Comma, 1), b'.' => (Tok::Dot, 1),
                b':' => (Tok::Colon, 1), b'=' => (Tok::Eq, 1),
                b'|' => (Tok::Pipe, 1), b'\\' => (Tok::Backslash, 1),
                b'*' => (Tok::Star, 1), b';' => (Tok::Semicolon, 1),
                b'+' => (Tok::Plus, 1), b'-' => (Tok::Minus, 1),
                b'<' => (Tok::Lt, 1),
                _ => return Err((format!("无法识别的字符 {:?}", b as char), Span::new(lo, lo + 1))),
            };
            self.pos += len;
            out.push(Token { tok, text: self.text(lo), span: Span::new(lo, self.pos) });
        }
    }

    fn text(&self, lo: usize) -> String {
        String::from_utf8_lossy(&self.src[lo..self.pos]).into_owned()
    }
}

// ---------------- AST ----------------

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Expr {
    Int(i64, Span),
    Bool(bool, Span),
    Unit(Span),
    Var(String, Span),
    Tag(String, Option<Box<Expr>>, Span),
    Rec(Vec<Field>, Option<(String, Span)>, Span),
    Sel(Box<Expr>, String, Span),
    Lam(String, Span, Box<Expr>, Span),
    App(Box<Expr>, Box<Expr>, Span),
    Let(bool, Vec<Binding>, Box<Expr>, Span),
    Bin(BinOp, Box<Expr>, Box<Expr>, Span),
    If(Box<Expr>, Box<Expr>, Box<Expr>, Span),
    Match(Box<Expr>, Vec<Arm>, Span),
    Paren(Box<Expr>, Span),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp { Add, Sub, Mul, Lt }

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub expr: Expr,
    pub name_span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Binding {
    pub name: String,
    pub name_span: Span,
    pub args: Vec<(String, Span)>,
    pub body: Expr,
    pub whole_span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Arm {
    pub tag: String,
    pub tag_span: Span,
    pub binder: Option<(String, Span)>,
    pub body: Expr,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, s) | Expr::Bool(_, s) | Expr::Unit(s) | Expr::Var(_, s)
            | Expr::Tag(_, _, s) | Expr::Rec(_, _, s) | Expr::Sel(_, _, s)
            | Expr::Lam(_, _, _, s) | Expr::App(_, _, s) | Expr::Let(_, _, _, s)
            | Expr::Bin(_, _, _, s) | Expr::If(_, _, _, s) | Expr::Match(_, _, s)
            | Expr::Paren(_, s) => *s,
        }
    }
}

/// 解析后的顶层包：一组顶层 let 绑定（按顺序进入作用域）+ 可选最终表达式。
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Program {
    pub top: Vec<Binding>,
    pub final_expr: Option<Expr>,
    pub span: Span,
}

// ---------------- Parser ----------------

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
}

pub fn parse(src: &str) -> Result<Program, (String, Span)> {
    let toks = Lexer::new(src).tokenize()?;
    let mut p = Parser { toks, idx: 0 };
    let mut top = Vec::new();
    let mut last_span = None;
    while p.peek().tok != Tok::Eof {
        let lo = p.peek().span.lo;
        if p.peek().tok == Tok::Let {
            let b = p.parse_binding()?;
            // 顶层允许可选分号
            if p.peek().tok == Tok::Semicolon { p.idx += 1; }
            last_span = Some(Span::new(lo, b.whole_span.hi));
            top.push(b);
        } else {
            break;
        }
    }
    let final_expr = if p.peek().tok != Tok::Eof {
        Some(p.parse_expr()?)
    } else {
        None
    };
    if p.peek().tok == Tok::Semicolon { p.idx += 1; }
    if p.peek().tok != Tok::Eof {
        return Err(("顶层只能出现 let 绑定与一个最终表达式".into(), p.peek().span));
    }
    let hi = final_expr.as_ref().map(|e| e.span().hi)
        .or_else(|| last_span.map(|s| s.hi))
        .unwrap_or(0);
    Ok(Program { top, final_expr, span: Span::new(0, hi) })
}

impl Parser {
    fn peek(&self) -> &Token { &self.toks[self.idx] }
    fn peek2(&self) -> &Token { self.toks.get(self.idx + 1).unwrap_or_else(|| self.toks.last().unwrap()) }
    fn bump(&mut self) -> Token { let t = self.toks[self.idx].clone(); if self.idx + 1 < self.toks.len() { self.idx += 1; } t }
    fn eat(&mut self, t: Tok) -> bool { if self.peek().tok == t { self.idx += 1; true } else { false } }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, (String, Span)> {
        if self.peek().tok == t { Ok(self.bump()) }
        else { Err((format!("期望 {what}，但遇到 {:?}", self.peek().text), self.peek().span)) }
    }

    fn parse_binding(&mut self) -> Result<Binding, (String, Span)> {
        let lo = self.expect(Tok::Let, "'let'")?.span.lo;
        let rec = self.eat(Tok::Rec);
        let _ = rec;
        let nt = self.expect(Tok::Ident, "绑定名")?;
        let mut args = Vec::new();
        while self.peek().tok == Tok::Ident {
            let t = self.bump();
            args.push((t.text, t.span));
        }
        self.expect(Tok::Eq, "'='")?;
        let body = self.parse_expr()?;
        let hi = body.span().hi;
        Ok(Binding { name: nt.text, name_span: nt.span, args, body, whole_span: Span::new(lo, hi) })
    }

    fn parse_expr(&mut self) -> Result<Expr, (String, Span)> {
        // 比较层（最低）
        let mut left = self.parse_add()?;
        if self.peek().tok == Tok::Lt {
            let s = self.bump().span;
            let right = self.parse_add()?;
            let span = left.span().merge(right.span());
            left = Expr::Bin(BinOp::Lt, Box::new(left), Box::new(right), s.merge(span));
        }
        Ok(left)
    }

    fn parse_add(&mut self) -> Result<Expr, (String, Span)> {
        let mut left = self.parse_mul()?;
        loop {
            let (op, s) = match self.peek().tok {
                Tok::Plus => (BinOp::Add, self.bump().span),
                Tok::Minus => (BinOp::Sub, self.bump().span),
                _ => break,
            };
            let right = self.parse_mul()?;
            let span = left.span().merge(right.span());
            left = Expr::Bin(op, Box::new(left), Box::new(right), s.merge(span));
        }
        Ok(left)
    }

    fn parse_mul(&mut self) -> Result<Expr, (String, Span)> {
        let mut left = self.parse_app()?;
        while self.peek().tok == Tok::Star {
            let s = self.bump().span;
            let right = self.parse_app()?;
            let span = left.span().merge(right.span());
            left = Expr::Bin(BinOp::Mul, Box::new(left), Box::new(right), s.merge(span));
        }
        Ok(left)
    }

    fn parse_app(&mut self) -> Result<Expr, (String, Span)> {
        let mut f = self.parse_postfix()?;
        loop {
            if atom_start(self.peek().tok) || self.peek().tok == Tok::LBrace || self.peek().tok == Tok::LBracket {
                let arg = self.parse_postfix()?;
                let span = f.span().merge(arg.span());
                f = Expr::App(Box::new(f), Box::new(arg), span);
            } else { break; }
        }
        Ok(f)
    }

    fn parse_postfix(&mut self) -> Result<Expr, (String, Span)> {
        let mut e = self.parse_atom()?;
        while self.peek().tok == Tok::Dot {
            let ds = self.bump().span;
            let t = self.expect(Tok::Ident, "记录字段名")?;
            let span = e.span().merge(t.span).merge(ds);
            e = Expr::Sel(Box::new(e), t.text, span);
        }
        Ok(e)
    }

    fn parse_atom(&mut self) -> Result<Expr, (String, Span)> {
        let t = self.peek().clone();
        match t.tok {
            Tok::Int => { self.bump(); let v: i64 = t.text.parse().unwrap(); Ok(Expr::Int(v, t.span)) }
            Tok::True => { self.bump(); Ok(Expr::Bool(true, t.span)) }
            Tok::False => { self.bump(); Ok(Expr::Bool(false, t.span)) }
            Tok::Unit => { self.bump(); Ok(Expr::Unit(t.span)) }
            Tok::Ident => { self.bump(); Ok(Expr::Var(t.text, t.span)) }
            Tok::LParen => {
                self.bump();
                let e = self.parse_expr()?;
                let r = self.expect(Tok::RParen, "')'")?;
                Ok(Expr::Paren(Box::new(e), t.span.merge(r.span)))
            }
            Tok::Backslash | Tok::Fun => self.parse_lam(),
            Tok::Let => self.parse_let(),
            Tok::If => self.parse_if(),
            Tok::Match => self.parse_match(),
            Tok::LBrace => self.parse_rec(),
            Tok::Upper => self.parse_tag(),
            Tok::Minus => {
                self.bump();
                let n = self.parse_postfix()?;
                let span = t.span.merge(n.span());
                Ok(Expr::Bin(BinOp::Sub, Box::new(Expr::Int(0, t.span)), Box::new(n), span))
            }
            _ => Err((format!("期望表达式，但遇到 {:?}", t.text), t.span)),
        }
    }

    fn parse_lam(&mut self) -> Result<Expr, (String, Span)> {
        let lo = self.bump().span.lo;
        let mut params = Vec::new();
        loop {
            match self.peek().tok {
                Tok::Ident => { let t = self.bump(); params.push((t.text, t.span)); }
                _ => break,
            }
        }
        if params.is_empty() {
            return Err(("函数需要至少一个参数名".into(), self.peek().span));
        }
        // 允许 fun x y -> e 或 \x y -> e
        if self.peek().tok == Tok::Arrow { self.bump(); }
        let body = self.parse_expr()?;
        let hi = body.span().hi;
        let mut e = body;
        while let Some((name, ns)) = params.pop() {
            let s = Span::new(lo, e.span().hi);
            e = Expr::Lam(name, ns, Box::new(e), s);
        }
        Ok(e)
    }

    fn parse_let(&mut self) -> Result<Expr, (String, Span)> {
        let lo = self.bump().span.lo;
        let rec = self.eat(Tok::Rec);
        let mut bindings = Vec::new();
        loop {
            let nt = self.expect(Tok::Ident, "绑定名")?;
            let mut args = Vec::new();
            while self.peek().tok == Tok::Ident {
                let t = self.bump();
                args.push((t.text, t.span));
            }
            self.expect(Tok::Eq, "'='")?;
            let body = self.parse_expr()?;
            let whole = Span::new(lo, body.span().hi);
            bindings.push(Binding { name: nt.text, name_span: nt.span, args, body, whole_span: whole });
            if self.eat(Tok::Semicolon) || self.peek().tok == Tok::Ident && self.peek2().tok == Tok::Eq {
                continue;
            }
            break;
        }
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr()?;
        let span = Span::new(lo, body.span().hi);
        Ok(Expr::Let(rec, bindings, Box::new(body), span))
    }

    fn parse_if(&mut self) -> Result<Expr, (String, Span)> {
        let lo = self.bump().span.lo;
        let c = self.parse_expr()?;
        self.expect(Tok::Then, "'then'")?;
        let t = self.parse_expr()?;
        self.expect(Tok::Else, "'else'")?;
        let e = self.parse_expr()?;
        let span = Span::new(lo, e.span().hi);
        Ok(Expr::If(Box::new(c), Box::new(t), Box::new(e), span))
    }

    fn parse_match(&mut self) -> Result<Expr, (String, Span)> {
        let lo = self.bump().span.lo;
        let scrut = self.parse_expr()?;
        self.expect(Tok::With, "'with'")?;
        let mut arms = Vec::new();
        loop {
            if self.peek().tok == Tok::Pipe { self.bump(); }
            let tagt = self.expect(Tok::Upper, "变体构造子（大写开头）")?;
            let binder = if self.peek().tok == Tok::Ident {
                let t = self.bump();
                Some((t.text, t.span))
            } else { None };
            self.expect(Tok::FatArrow, "'=>'")?;
            let body = self.parse_expr()?;
            arms.push(Arm { tag: tagt.text, tag_span: tagt.span, binder, body });
            if self.peek().tok == Tok::Pipe { continue; }
            break;
        }
        let span = Span::new(lo, arms.last().map(|a| a.body.span().hi).unwrap_or(lo));
        Ok(Expr::Match(Box::new(scrut), arms, span))
    }

    fn parse_rec(&mut self) -> Result<Expr, (String, Span)> {
        let lo = self.bump().span.lo;
        let mut fields = Vec::new();
        let mut tail = None;
        if self.peek().tok != Tok::RBrace {
            loop {
                if self.peek().tok == Tok::Pipe {
                    self.bump();
                    let t = self.expect(Tok::Ident, "行多态变量名")?;
                    tail = Some((t.text, t.span));
                    break;
                }
                let nt = self.expect(Tok::Ident, "字段名")?;
                self.expect(Tok::Eq, "'='")?;
                let e = self.parse_expr()?;
                fields.push(Field { name: nt.text, name_span: nt.span, expr: e });
                if self.eat(Tok::Comma) || self.eat(Tok::Semicolon) { continue; }
                break;
            }
        }
        let r = self.expect(Tok::RBrace, "'}'")?;
        Ok(Expr::Rec(fields, tail, Span::new(lo, r.span.hi)))
    }

    fn parse_tag(&mut self) -> Result<Expr, (String, Span)> {
        let t = self.bump();
        if atom_start(self.peek().tok) || self.peek().tok == Tok::LBrace {
            let payload = self.parse_postfix()?;
            let span = t.span.merge(payload.span());
            Ok(Expr::Tag(t.text, Some(Box::new(payload)), span))
        } else {
            Ok(Expr::Tag(t.text, None, t.span))
        }
    }
}

fn atom_start(t: Tok) -> bool {
    matches!(t, Tok::Int | Tok::True | Tok::False | Tok::Unit | Tok::Ident
        | Tok::Upper | Tok::LParen | Tok::Backslash | Tok::Fun | Tok::Let
        | Tok::If | Tok::Match)
}
