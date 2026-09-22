//! Lexer, parser, scope resolution and structural pairing for the slice language.
use serde::Serialize;
use std::collections::HashMap;

pub type NodeId = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start as u32, end: end as u32 }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Tok {
    Int(u64),
    Bool(bool),
    Str,
    Upper(String),
    Lower(String),
    // punctuation
    LParen, RParen, LBrace, RBrace, LBrack, RBrack, Dot, Comma, Colon, Eq, Arrow, FatArrow,
    Backslash, Pipe, Underscore, Semicolon, Let, Rec, In, Fun, If, Then, Else,
    Match, With, Unit, Star, Plus, Minus, Slash, Lt, Gt, Le, Ge, EqEq, Ne, And, Or,
    Eof,
}

#[derive(Clone, Debug)]
struct LexTok {
    tok: Tok,
    span: Span,
    text: String,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub span: Span,
    pub msg: String,
}

fn lex(src: &str) -> Result<Vec<LexTok>, LexError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mkle = |i: usize, msg: &str| LexError { span: Span::new(i, i + 1), msg: msg.to_string() };
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() { i += 1; continue; }
        let start = i;
        if c.is_ascii_digit() {
            let mut n = 0u64;
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                n = n.wrapping_mul(10).wrapping_add((b[i] - b'0') as u64);
                i += 1;
            }
            out.push(LexTok { tok: Tok::Int(n), span: Span::new(start, i), text: src[start..i].to_string() });
            continue;
        }
        if c == '"' {
            i += 1;
            while i < b.len() && b[i] as char != '"' {
                if b[i] as char == '\\' && i + 1 < b.len() { i += 2; } else { i += 1; }
            }
            if i >= b.len() { return Err(mkle(start, "unterminated string")); }
            i += 1;
            out.push(LexTok { tok: Tok::Str, span: Span::new(start, i), text: src[start..i].to_string() });
            continue;
        }
        if c == '_' && (i + 1 >= b.len() || !(b[i + 1] as char).is_alphanumeric()) {
            out.push(LexTok { tok: Tok::Underscore, span: Span::new(i, i + 1), text: "_".into() });
            i += 1; continue;
        }
        if c.is_alphabetic() || c == '_' {
            while i < b.len() && ((b[i] as char).is_alphanumeric() || b[i] as char == '_') { i += 1; }
            let s = &src[start..i];
            let tok = match s {
                "let" => Tok::Let, "rec" => Tok::Rec, "in" => Tok::In, "fun" => Tok::Fun,
                "if" => Tok::If, "then" => Tok::Then, "else" => Tok::Else,
                "match" => Tok::Match, "with" => Tok::With, "true" => Tok::Bool(true),
                "false" => Tok::Bool(false), "unit" => Tok::Unit,
                _ if c.is_uppercase() => Tok::Upper(s.to_string()),
                _ => Tok::Lower(s.to_string()),
            };
            out.push(LexTok { tok, span: Span::new(start, i), text: s.to_string() });
            continue;
        }
        let two: &[u8] = if i + 1 < b.len() { &b[i..i + 2] } else { &b[i..i] };
        let (tok, n) = match two {
            b"->" => (Tok::Arrow, 2), b"=>" => (Tok::FatArrow, 2),
            b"==" => (Tok::EqEq, 2), b"<=" => (Tok::Le, 2), b">=" => (Tok::Ge, 2),
            b"!=" => (Tok::Ne, 2), b"&&" => (Tok::And, 2), b"||" => (Tok::Or, 2),
            _ => (match c {
                '(' => Tok::LParen, ')' => Tok::RParen, '{' => Tok::LBrace, '}' => Tok::RBrace,
                '[' => Tok::LBrack, ']' => Tok::RBrack,
                '.' => Tok::Dot, ',' => Tok::Comma, ':' => Tok::Colon, '=' => Tok::Eq,
                '\\' => Tok::Backslash, '|' => Tok::Pipe, ';' => Tok::Semicolon,
                '*' => Tok::Star, '+' => Tok::Plus, '-' => Tok::Minus, '/' => Tok::Slash,
                '<' => Tok::Lt, '>' => Tok::Gt,
                _ => return Err(mkle(i, "unexpected character")),
            }, 1),
        };
        out.push(LexTok { tok, span: Span::new(start, i + n), text: src[start..i + n].to_string() });
        i += n;
    }
    out.push(LexTok { tok: Tok::Eof, span: Span::new(b.len(), b.len()), text: String::new() });
    Ok(out)
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum Ty {
    TCon(String),
    TVar(String),
    TFun(Box<Ty>, Box<Ty>),
    TRecord(Vec<(String, Ty)>, bool),
    TVariant(Vec<(String, Option<Ty>)>, bool),
    TUnit,
}

#[derive(Clone, Debug, Serialize)]
pub enum NodeKind {
    Int(i64),
    Bool(bool),
    Str(String),
    Unit,
    Var(String),
    Binop(String),
    App,
    Lam { param: String, ann: Option<Ty> },
    Let { name: String, rec: bool, ann: Option<Ty> },
    If,
    Record,
    Field(String),
    Variant { ctor: String },
    Match,
    Arm,
    PatWild,
    PatBind(String),
    PatCon { ctor: String },
    Anno(Ty),
}

#[derive(Clone, Debug, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub span: Span,
    pub children: Vec<NodeId>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseErrorInfo {
    pub span: Span,
    pub msg: String,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct Program {
    pub nodes: Vec<Node>,
    pub root: Option<NodeId>,
    pub parse_errors: Vec<ParseErrorInfo>,
    /// variable node id -> binding node id (after resolution)
    pub binds: HashMap<NodeId, NodeId>,
}

impl Program {
    pub fn node(&self, id: NodeId) -> &Node { &self.nodes[id as usize] }
    pub fn kind_name(&self, id: NodeId) -> &'static str {
        use NodeKind::*;
        match &self.nodes[id as usize].kind {
            Int(_) => "Int", Bool(_) => "Bool", Str(_) => "Str", Unit => "Unit",
            Var(_) => "Var", Binop(_) => "Binop", App => "App",
            Lam { .. } => "Lam", Let { .. } => "Let", If => "If", Record => "Record",
            Field(_) => "Field", Variant { .. } => "Variant", Match => "Match", Arm => "Arm",
            PatWild => "PatWild", PatBind(_) => "PatBind", PatCon { .. } => "PatCon",
            Anno(_) => "Anno",
        }
    }
    pub fn label(&self, id: NodeId) -> String {
        use NodeKind::*;
        match &self.nodes[id as usize].kind {
            Int(n) => format!("{n}"), Bool(b) => format!("{b}"), Str(s) => format!("{s:?}"),
            Unit => "()".into(), Var(n) => n.clone(), Binop(op) => op.clone(), App => "@".into(),
            Lam { param, .. } => format!("fun {param}"),
            Let { name, rec, .. } => format!("{}let {name}", if *rec { "rec " } else { "" }),
            If => "if".into(), Record => "{}".into(), Field(f) => format!(".{f}"),
            Variant { ctor } => ctor.clone(), Match => "match".into(), Arm => "arm".into(),
            PatWild => "_".into(), PatBind(n) => n.clone(), PatCon { ctor } => ctor.clone(),
            Anno(t) => format!(": {t:?}"),
        }
    }
}

struct Parser<'a> {
    src: &'a str,
    toks: Vec<LexTok>,
    pos: usize,
    nodes: Vec<Node>,
    errs: Vec<ParseErrorInfo>,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Tok { &self.toks[self.pos].tok }
    fn peek_at(&self, k: usize) -> &Tok {
        let i = (self.pos + k).min(self.toks.len() - 1);
        &self.toks[i].tok
    }
    fn span_at(&self, k: usize) -> Span {
        let i = (self.pos + k).min(self.toks.len() - 1);
        self.toks[i].span
    }
    fn bump(&mut self) -> LexTok { let t = self.toks[self.pos].clone(); if self.pos < self.toks.len() - 1 { self.pos += 1; } t }
    fn mk(&mut self, kind: NodeKind, span: Span, children: Vec<NodeId>) -> NodeId {
        let id = self.nodes.len() as NodeId;
        self.nodes.push(Node { id, kind, span, children });
        id
    }
    fn err(&mut self, span: Span, msg: impl Into<String>) {
        self.errs.push(ParseErrorInfo { span, msg: msg.into() });
    }
    fn eat(&mut self, t: Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) { self.bump(); true } else { false }
    }
    fn expect(&mut self, t: Tok, what: &str) -> bool {
        if self.eat(t.clone()) { true } else { self.err(self.toks[self.pos].span, format!("expected {what}")); false }
    }

    fn parse_program(&mut self) -> Option<NodeId> {
        let e = self.parse_expr();
        if !matches!(self.peek(), Tok::Eof) {
            self.err(self.toks[self.pos].span, "unexpected trailing tokens");
        }
        e
    }

    fn parse_expr(&mut self) -> Option<NodeId> { self.parse_let() }

    fn parse_let(&mut self) -> Option<NodeId> {
        if !matches!(self.peek(), Tok::Let) { return self.parse_fun(); }
        let start = self.bump().span.start;
        let rec = self.eat(Tok::Rec);
        let name = match self.bump().tok {
            Tok::Lower(s) => s,
            other => { self.err(self.toks[self.pos.saturating_sub(1)].span, "expected binding name"); return None; }
        };
        let ann = if matches!(self.peek(), Tok::Colon) { self.bump(); Some(self.parse_ty()?) } else { None };
        self.expect(Tok::Eq, "=");
        let value = self.parse_expr()?;
        self.expect(Tok::In, "in");
        let body = self.parse_expr()?;
        let end = self.nodes[body as usize].span.end;
        Some(self.mk(NodeKind::Let { name, rec, ann }, Span::new(start as usize, end as usize), vec![value, body]))
    }

    fn parse_fun(&mut self) -> Option<NodeId> {
        if !matches!(self.peek(), Tok::Backslash) { return self.parse_if(); }
        let start = self.bump().span.start;
        let param = match self.bump().tok { Tok::Lower(s) => s, _ => { self.err(self.toks[self.pos.saturating_sub(1)].span, "expected parameter"); return None; } };
        let ann = if matches!(self.peek(), Tok::Colon) { self.bump(); Some(self.parse_ty()?) } else { None };
        self.expect(Tok::Arrow, "->");
        let body = self.parse_expr()?;
        let end = self.nodes[body as usize].span.end;
        Some(self.mk(NodeKind::Lam { param, ann }, Span::new(start as usize, end as usize), vec![body]))
    }

    fn parse_if(&mut self) -> Option<NodeId> {
        if !matches!(self.peek(), Tok::If) { return self.parse_match(); }
        let start = self.bump().span.start;
        let cond = self.parse_expr()?;
        self.expect(Tok::Then, "then");
        let then = self.parse_expr()?;
        self.expect(Tok::Else, "else");
        let els = self.parse_expr()?;
        let end = self.nodes[els as usize].span.end;
        Some(self.mk(NodeKind::If, Span::new(start as usize, end as usize), vec![cond, then, els]))
    }

    fn parse_match(&mut self) -> Option<NodeId> {
        if !matches!(self.peek(), Tok::Match) { return self.parse_binary(0) }
        let start = self.bump().span.start;
        let scrut = self.parse_expr()?;
        self.expect(Tok::With, "with");
        let mut arms = vec![scrut];
        loop {
            self.eat(Tok::Pipe);
            let pat = self.parse_pat()?;
            self.expect(Tok::FatArrow, "=>");
            let body = self.parse_expr()?;
            let end = self.nodes[body as usize].span.end;
            let arm = self.mk(NodeKind::Arm, Span::new(self.nodes[pat as usize].span.start as usize, end as usize), vec![pat, body]);
            arms.push(arm);
            if matches!(self.peek(), Tok::Pipe) { continue; }
            break;
        }
        let end = self.nodes[*arms.last().unwrap() as usize].span.end;
        Some(self.mk(NodeKind::Match, Span::new(start as usize, end as usize), arms))
    }

    fn parse_pat(&mut self) -> Option<NodeId> {
        let t = self.bump();
        let (start, end) = (t.span.start, t.span.end);
        let n = match t.tok {
            Tok::Underscore => self.mk(NodeKind::PatWild, t.span, vec![]),
            Tok::Lower(s) => self.mk(NodeKind::PatBind(s), t.span, vec![]),
            Tok::Upper(ctor) => {
                let mut children = vec![];
                if matches!(self.peek(), Tok::Lower(_)) {
                    let v = self.bump();
                    if let Tok::Lower(s) = v.tok { children.push(self.mk(NodeKind::PatBind(s), v.span, vec![])); }
                }
                let end = children.last().map(|c| self.nodes[*c as usize].span.end).unwrap_or(end);
                self.mk(NodeKind::PatCon { ctor }, Span::new(start as usize, end as usize), children)
            }
            _ => { self.err(t.span, "expected pattern"); return None; }
        };
        Some(n)
    }

    fn binop_tok(&self) -> Option<(String, u8)> {
        let (op, prec) = match self.peek() {
            Tok::Or => ("||", 1),
            Tok::And => ("&&", 2),
            Tok::EqEq => ("==", 3), Tok::Ne => ("!=", 3),
            Tok::Lt => ("<", 4), Tok::Gt => (">", 4), Tok::Le => ("<=", 4), Tok::Ge => (">=", 4),
            Tok::Plus => ("+", 5), Tok::Minus => ("-", 5),
            Tok::Star => ("*", 6), Tok::Slash => ("/", 6),
            _ => return None,
        };
        Some((op.to_string(), prec))
    }

    fn parse_binary(&mut self, min_prec: u8) -> Option<NodeId> {
        let mut left = self.parse_postfix()?;
        while let Some((op, prec)) = self.binop_tok() {
            if prec < min_prec { break; }
            let t = self.bump();
            let right = self.parse_binary(prec + 1)?;
            let opn = self.mk(NodeKind::Binop(op), t.span, vec![]);
            let end = self.nodes[right as usize].span.end;
            left = self.mk(NodeKind::App, Span::new(self.nodes[left as usize].span.start as usize, end as usize), vec![opn, left, right]);
        }
        Some(left)
    }

    fn parse_postfix(&mut self) -> Option<NodeId> {
        let mut e = self.parse_atom()?;
        while matches!(self.peek(), Tok::Dot) {
            let dot = self.bump().span.start;
            let t = self.bump();
            match t.tok {
                Tok::Lower(f) => {
                    let end = t.span.end;
                    e = self.mk(NodeKind::Field(f), Span::new(dot as usize, end as usize), vec![e]);
                }
                _ => { self.err(t.span, "expected field name"); return None; }
            }
        }
        Some(e)
    }

    fn parse_atom(&mut self) -> Option<NodeId> {
        let t = self.toks[self.pos].clone();
        let (start, mut n) = match t.tok {
            Tok::Int(v) => { self.bump(); (t.span.start as usize, self.mk(NodeKind::Int(v as i64), t.span, vec![])) }
            Tok::Bool(v) => { self.bump(); (t.span.start as usize, self.mk(NodeKind::Bool(v), t.span, vec![])) }
            Tok::Str => { self.bump(); let s = t.text.clone(); (t.span.start as usize, self.mk(NodeKind::Str(s), t.span, vec![])) }
            Tok::Unit => { self.bump(); (t.span.start as usize, self.mk(NodeKind::Unit, t.span, vec![])) }
            Tok::Lower(s) => { self.bump(); (t.span.start as usize, self.mk(NodeKind::Var(s), t.span, vec![])) }
            Tok::Upper(ctor) => {
                self.bump();
                let mut children = vec![];
                if self.atom_starts_here() {
                    children.push(self.parse_postfix()?);
                }
                let end = children.last().map(|c| self.nodes[*c as usize].span.end).unwrap_or(t.span.end);
                (start as usize, self.mk(NodeKind::Variant { ctor }, Span::new(start as usize, end as usize), children))
            }
            Tok::LParen => { self.bump(); let e = self.parse_expr()?; self.expect(Tok::RParen, ")"); (self.nodes[e as usize].span.start as usize, e) }
            Tok::LBrace => self.parse_record()?,
            _ => { self.err(t.span, "expected expression"); return None; }
        };
        // application: several juxtaposed atoms
        while self.atom_starts_here() {
            let arg = self.parse_postfix()?;
            let end = self.nodes[arg as usize].span.end;
            n = self.mk(NodeKind::App, Span::new(start, end as usize), vec![n, arg]);
        }
        Some(n)
    }

    fn atom_starts_here(&self) -> bool {
        matches!(self.peek(),
            Tok::Int(_) | Tok::Bool(_) | Tok::Str | Tok::Unit | Tok::Lower(_) |
            Tok::Upper(_) | Tok::LParen | Tok::LBrace)
    }

    fn parse_record(&mut self) -> Option<(usize, NodeId)> {
        let lb = self.bump().span;
        let mut fields = vec![];
        let mut last_end = lb.end;
        loop {
            if matches!(self.peek(), Tok::RBrace) { break; }
            let name = match self.bump().tok { Tok::Lower(s) => s, _ => { self.err(self.toks[self.pos.saturating_sub(1)].span, "expected field name"); return None; } };
            self.expect(Tok::Eq, "=");
            let val = self.parse_expr()?;
            last_end = self.nodes[val as usize].span.end;
            fields.push((name, val));
            if self.eat(Tok::Comma) || self.eat(Tok::Semicolon) { continue; }
            break;
        }
        let rb = self.bump().span;
        if !matches!(rb.span, _) {}
        let mut children = vec![];
        // keep field label + value pairs as flat (label marker node, value)
        for (name, val) in fields {
            let marker = self.mk(NodeKind::Field(name), Span::new(0, 0), vec![]);
            children.push(marker);
            children.push(val);
        }
        let n = self.mk(NodeKind::Record, Span::new(lb.start as usize, rb.end as usize), children);
        let _ = last_end;
        Some((lb.start as usize, n))
    }

    fn parse_ty(&mut self) -> Option<Ty> {
        let mut from = self.parse_ty_atom()?;
        if self.eat(Tok::Arrow) {
            let to = self.parse_ty()?;
            from = Ty::TFun(Box::new(from), Box::new(to));
        }
        Some(from)
    }

    fn parse_ty_atom(&mut self) -> Option<Ty> {
        let t = self.toks[self.pos].clone();
        match t.tok {
            Tok::LParen => { self.bump(); let ty = self.parse_ty()?; self.expect(Tok::RParen, ")"); Some(ty) }
            Tok::Unit => { self.bump(); Some(Ty::TUnit) }
            Tok::LBrace => {
                self.bump();
                let mut fields = vec![];
                let closed;
                loop {
                    if matches!(self.peek(), Tok::RBrace) { closed = true; break; }
                    if matches!(self.peek(), Tok::Pipe) { self.bump(); closed = false; break; }
                    let name = match self.bump().tok { Tok::Lower(s) => s, _ => { self.err(t.span, "expected field"); return None; } };
                    self.expect(Tok::Colon, ":");
                    let ty = self.parse_ty()?;
                    fields.push((name, ty));
                    if self.eat(Tok::Comma) { continue; }
                    if matches!(self.peek(), Tok::RBrace) { closed = true; break; }
                    if matches!(self.peek(), Tok::Pipe) { self.bump(); closed = false; break; }
                    self.err(self.toks[self.pos].span, "expected , or |"); return None;
                }
                self.expect(Tok::RBrace, "}");
                Some(Ty::TRecord(fields, closed))
            }
            Tok::LBrack => {
                self.bump();
                let mut ctors = vec![];
                let closed;
                loop {
                    let name = match self.bump().tok { Tok::Upper(s) => s, _ => { self.err(t.span, "expected constructor"); return None; } };
                    let payload = if matches!(self.peek(), Tok::LParen) {
                        self.bump();
                        let ty = self.parse_ty()?;
                        self.expect(Tok::RParen, ")");
                        Some(ty)
                    } else { None };
                    ctors.push((name, payload));
                    if self.eat(Tok::Pipe) { continue; }
                    if matches!(self.peek(), Tok::RBrack) { closed = true; break; }
                    self.err(self.toks[self.pos].span, "expected | or ]"); return None;
                }
                self.expect(Tok::RBrack, "]");
                Some(Ty::TVariant(ctors, closed))
            }
            Tok::Lower(s) => { self.bump(); Some(Ty::TVar(s)) }
            Tok::Upper(s) => { self.bump(); Some(Ty::TCon(s)) }
            _ => { self.err(t.span, "expected type"); None }
        }
    }
}
