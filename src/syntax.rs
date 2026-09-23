//! Lexer, AST and recursive-descent parser for the mini HM language.
//!
//! Grammar (semicolons separate top-level bindings):
//!
//! prog   := binding (";" binding)*
//! bind   := "let" ident "=" expr
//!         | "let rec" ident params "=" expr
//! params := ident*
//! expr   := "\" ident "->" expr
//!         | "let" ident "=" expr "in" expr
//!         | "let rec" ident params "=" expr "in" expr
//!         | "if" expr "then" expr "else" expr
//!         | "match" expr "with" arm ("|" arm)*
//! arm    := tag ("(" ident ")")? "->" expr
//!         | label "(" expr ")" "=>" expr      (record-field view arm)
//! expr2  := expr2 ("with" label "=" expr)
//! expr3  := expr3 "." label | expr3 atom
//! atom   := int | bool | ident | tag ("(" expr ")")?
//!         | "{" (label "=" expr ("," label "=" expr)*)? "}"
//!         | "(" expr ")"

use crate::span::{line_col, NodeId, Span};
use serde::{Deserialize, Serialize};

fn fnv1a(bytes: &[u8]) -> u32 {
    let mut h: u32 = 0x811c9dc5;
    for &b in bytes {
        h ^= b as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

// Grammar roles used inside [`NodeId`].
pub mod role {
    pub const BINDING: u16 = 1;
    pub const REC_BINDING: u16 = 2;
    pub const PARAM: u16 = 3;
    pub const LIT_INT: u16 = 10;
    pub const LIT_BOOL: u16 = 11;
    pub const VAR: u16 = 12;
    pub const LAM: u16 = 13;
    pub const APP: u16 = 14;
    pub const LET: u16 = 15;
    pub const IF: u16 = 16;
    pub const RECORD: u16 = 17;
    pub const FIELD: u16 = 18;
    pub const WITH: u16 = 19;
    pub const TAG: u16 = 20;
    pub const MATCH: u16 = 21;
    pub const ARM: u16 = 22;
    pub const VIEW: u16 = 23;
    pub const PARAM_TY: u16 = 30;
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Binding {
    pub id: NodeId,
    pub name: String,
    pub rec: bool,
    pub params: Vec<(String, NodeId, Span)>,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Expr {
    LitInt(i64, NodeId, Span),
    LitBool(bool, NodeId, Span),
    Var(String, NodeId, Span),
    Lam {
        param: String,
        param_id: NodeId,
        param_span: Span,
        body: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    Let {
        name: String,
        binding_id: NodeId,
        rhs: Box<Expr>,
        body: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    If {
        cond: Box<Expr>,
        then_e: Box<Expr>,
        else_e: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    Record {
        fields: Vec<(String, Expr)>,
        id: NodeId,
        span: Span,
    },
    Field {
        record: Box<Expr>,
        label: String,
        id: NodeId,
        span: Span,
    },
    With {
        record: Box<Expr>,
        label: String,
        value: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    Tag {
        tag: String,
        payload: Option<Box<Expr>>,
        id: NodeId,
        span: Span,
    },
    Match {
        scrut: Box<Expr>,
        tag_arms: Vec<TagArm>,
        field_arms: Vec<FieldArm>,
        id: NodeId,
        span: Span,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TagArm {
    pub tag: String,
    pub payload: Option<(String, NodeId, Span)>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FieldArm {
    pub label: String,
    pub bound: String,
    pub bound_id: NodeId,
    pub bound_span: Span,
    pub body: Expr,
    pub span: Span,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::LitInt(_, _, s)
            | Expr::LitBool(_, _, s)
            | Expr::Var(_, _, s) => *s,
            Expr::Lam { span, .. }
            | Expr::App { span, .. }
            | Expr::Let { span, .. }
            | Expr::If { span, .. }
            | Expr::Record { span, .. }
            | Expr::Field { span, .. }
            | Expr::With { span, .. }
            | Expr::Tag { span, .. }
            | Expr::Match { span, .. } => *span,
        }
    }
    pub fn id(&self) -> &NodeId {
        match self {
            Expr::LitInt(_, i, _)
            | Expr::LitBool(_, i, _)
            | Expr::Var(_, i, _) => i,
            Expr::Lam { id, .. }
            | Expr::App { id, .. }
            | Expr::Let { id, .. }
            | Expr::If { id, .. }
            | Expr::Record { id, .. }
            | Expr::Field { id, .. }
            | Expr::With { id, .. }
            | Expr::Tag { id, .. }
            | Expr::Match { id, .. } => id,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParseError {
    pub message: String,
    pub lo: usize,
    pub hi: usize,
}

impl ParseError {
    pub fn render(&self, src: &str) -> String {
        let (line, col) = line_col(src, self.lo);
        format!("{}:{}: {}", line, col, self.message)
    }
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Int(i64),
    Bool(bool),
    Ident(String),
    Upper(String), // variant tag
    Label(String), // lowercase identifier that may be a record label
    KwLet,
    KwRec,
    KwIn,
    KwIf,
    KwThen,
    KwElse,
    KwMatch,
    KwWith,
    Arrow,   // ->
    FatArrow, // =>
    Pipe,    // |
    Eq,
    Dot,
    Comma,
    Semi,
    Backslash,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Eof,
}

#[derive(Clone)]
struct LexTok {
    tok: Tok,
    lo: usize,
    hi: usize,
}

fn lex(src: &str) -> Result<Vec<LexTok>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mk = |tok: Tok, lo: usize, hi: usize| LexTok { tok, lo, hi };
    while i < b.len() {
        let c = b[i] as char;
        match c {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '-' if i + 1 < b.len() && b[i + 1] == b'>' => {
                out.push(mk(Tok::Arrow, i, i + 2));
                i += 2;
            }
            '=' if i + 1 < b.len() && b[i + 1] == b'>' => {
                out.push(mk(Tok::FatArrow, i, i + 2));
                i += 2;
            }
            '=' => { out.push(mk(Tok::Eq, i, i + 1)); i += 1; }
            '.' => { out.push(mk(Tok::Dot, i, i + 1)); i += 1; }
            ',' => { out.push(mk(Tok::Comma, i, i + 1)); i += 1; }
            ';' => { out.push(mk(Tok::Semi, i, i + 1)); i += 1; }
            '|' => { out.push(mk(Tok::Pipe, i, i + 1)); i += 1; }
            '\\' => { out.push(mk(Tok::Backslash, i, i + 1)); i += 1; }
            '{' => { out.push(mk(Tok::LBrace, i, i + 1)); i += 1; }
            '}' => { out.push(mk(Tok::RBrace, i, i + 1)); i += 1; }
            '(' => { out.push(mk(Tok::LParen, i, i + 1)); i += 1; }
            ')' => { out.push(mk(Tok::RParen, i, i + 1)); i += 1; }
            '0'..='9' => {
                let lo = i;
                while i < b.len() && (b[i] as char).is_ascii_digit() {
                    i += 1;
                }
                let n: i64 = src[lo..i].parse().map_err(|_| ParseError {
                    message: format!("integer literal out of range: {}", &src[lo..i]),
                    lo, hi: i,
                })?;
                out.push(mk(Tok::Int(n), lo, i));
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let lo = i;
                while i < b.len()
                    && (matches!(b[i] as char, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '\''))
                {
                    i += 1;
                }
                let word = &src[lo..i];
                let tok = match word {
                    "let" => Tok::KwLet,
                    "rec" => Tok::KwRec,
                    "in" => Tok::KwIn,
                    "if" => Tok::KwIf,
                    "then" => Tok::KwThen,
                    "else" => Tok::KwElse,
                    "match" => Tok::KwMatch,
                    "with" => Tok::KwWith,
                    "true" => Tok::Bool(true),
                    "false" => Tok::Bool(false),
                    _ => {
                        if word.starts_with(|c: char| c.is_ascii_uppercase()) {
                            Tok::Upper(word.to_string())
                        } else {
                            // Labels share the lexical shape of identifiers;
                            // the parser decides which is which by context.
                            Tok::Label(word.to_string())
                        }
                    }
                };
                out.push(mk(tok, lo, i));
            }
            other => {
                return Err(ParseError {
                    message: format!("unexpected character {:?}", other),
                    lo: i,
                    hi: i + other.len_utf8(),
                });
            }
        }
    }
    out.push(mk(Tok::Eof, b.len(), b.len()));
    Ok(out)
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct Parser<'a> {
    src: &'a str,
    toks: Vec<LexTok>,
    pos: usize,
    scope: Vec<(String, (NodeId, Span))>,
    errors: Vec<ParseError>,
}

fn tag_of(role: u16, anchor: usize, extra: &str) -> u32 {
    let mut s = format!("{}:{}:", role, anchor);
    s.push_str(extra);
    fnv1a(s.as_bytes())
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Result<Self, ParseError> {
        Ok(Parser {
            src,
            toks: lex(src)?,
            pos: 0,
            scope: Vec::new(),
            errors: Vec::new(),
        })
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn peek_span(&self) -> Span {
        let t = &self.toks[self.pos];
        Span::new(t.lo, t.hi)
    }
    fn bump(&mut self) -> LexTok {
        let t = self.toks[self.pos].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, wanted: &Tok) -> Option<LexTok> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(wanted) {
            Some(self.bump())
        } else {
            None
        }
    }
    fn expect(&mut self, wanted: Tok, what: &str) -> Result<LexTok, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&wanted) {
            Ok(self.bump())
        } else {
            let sp = self.peek_span();
            Err(ParseError {
                message: format!("expected {}, found {}", what, self.tok_desc()),
                lo: sp.lo as usize,
                hi: sp.hi as usize,
            })
        }
    }
    fn tok_desc(&self) -> String {
        match self.peek() {
            Tok::Int(_) => "integer".into(),
            Tok::Bool(_) => "boolean".into(),
            Tok::Ident(s) | Tok::Upper(s) | Tok::Label(s) => format!("identifier `{}`", s),
            Tok::KwLet => "`let`".into(),
            Tok::KwRec => "`rec`".into(),
            Tok::KwIn => "`in`".into(),
            Tok::KwIf => "`if`".into(),
            Tok::KwThen => "`then`".into(),
            Tok::KwElse => "`else`".into(),
            Tok::KwMatch => "`match`".into(),
            Tok::KwWith => "`with`".into(),
            Tok::Arrow => "`->`".into(),
            Tok::FatArrow => "`=>`".into(),
            Tok::Pipe => "`|`".into(),
            Tok::Eq => "`=`".into(),
            Tok::Dot => "`.`".into(),
            Tok::Comma => "`,`".into(),
            Tok::Semi => "`;`".into(),
            Tok::Backslash => "`\\`".into(),
            Tok::LBrace => "`{`".into(),
            Tok::RBrace => "`}`".into(),
            Tok::LParen => "`(`".into(),
            Tok::RParen => "`)`".into(),
            Tok::Eof => "end of input".into(),
        }
    }

    fn lookup(&self, name: &str) -> Option<(NodeId, Span)> {
        for (n, v) in self.scope.iter().rev() {
            if n == name {
                return Some(v.clone());
            }
        }
        None
    }
    fn push(&mut self, name: String, id: NodeId, span: Span) {
        self.scope.push((name, (id, span)));
    }
    fn pop_to(&mut self, depth: usize) {
        self.scope.truncate(depth);
    }

    // -- top level -----------------------------------------------------------

    fn parse_program(&mut self) -> Result<Vec<Binding>, Vec<ParseError>> {
        let mut bindings = Vec::new();
        loop {
            if matches!(self.peek(), Tok::Eof) {
                break;
            }
            match self.parse_binding() {
                Ok(b) => bindings.push(b),
                Err(e) => {
                    self.errors.push(e);
                    self.recover();
                }
            }
            if self.eat(&Tok::Semi).is_none() {
                if matches!(self.peek(), Tok::Eof) {
                    break;
                }
                let sp = self.peek_span();
                self.errors.push(ParseError {
                    message: format!("expected `;` between bindings, found {}", self.tok_desc()),
                    lo: sp.lo as usize,
                    hi: sp.hi as usize,
                });
                self.recover();
            }
        }
        if self.errors.is_empty() {
            Ok(bindings)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn recover(&mut self) {
        let mut depth = 0i32;
        loop {
            match self.peek() {
                Tok::Eof => return,
                Tok::Semi if depth == 0 => return,
                Tok::LParen | Tok::LBrace => {
                    depth += 1;
                    self.bump();
                }
                Tok::RParen | Tok::RBrace => {
                    if depth == 0 {
                        return;
                    }
                    depth -= 1;
                    self.bump();
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn parse_binding(&mut self) -> Result<Binding, ParseError> {
        let let_tok = self.expect(Tok::KwLet, "`let`")?;
        let rec = self.eat(&Tok::KwRec).is_some();
        let (name, name_span) = self.parse_name()?;
        let mut params: Vec<(String, NodeId, Span)> = Vec::new();
        while let Tok::Label(_) = self.peek() {
            let t = self.bump();
            if let Tok::Label(p) = t.tok {
                let sp = Span::new(t.lo, t.hi);
                let id = NodeId::new(t.lo, role::PARAM, tag_of(role::PARAM, t.lo, &p));
                self.push(p.clone(), id.clone(), sp);
                params.push((p, id, sp));
            }
        }
        self.expect(Tok::Eq, "`=`")?;
        let depth0 = self.scope.len();
        // Recursive bindings are in scope while their body is parsed.
        let rec_id = if rec {
            let anchor = let_tok.lo + 4;
            let id = NodeId::new(
                anchor,
                role::REC_BINDING,
                tag_of(role::REC_BINDING, anchor, &name),
            );
            self.push(name.clone(), id.clone(), name_span);
            Some(id)
        } else {
            None
        };
        let expr = self.parse_expr();
        let expr = match expr {
            Ok(e) => e,
            Err(e) => {
                self.pop_to(depth0);
                return Err(e);
            }
        };
        self.pop_to(depth0);
        let span = Span::new(let_tok.lo, expr.span().hi as usize);
        let id = if rec {
            rec_id.unwrap()
        } else {
            NodeId::new(let_tok.lo, role::BINDING, tag_of(role::BINDING, let_tok.lo, &name))
        };
        // Binding becomes visible to later top-level bindings.
        self.push(name.clone(), id.clone(), name_span);
        Ok(Binding { id, name, rec, params, expr, span })
    }

    fn parse_name(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().clone() {
            Tok::Label(s) => {
                let t = self.bump();
                Ok((s, Span::new(t.lo, t.hi)))
            }
            _ => {
                let sp = self.peek_span();
                Err(ParseError {
                    message: format!("expected an identifier, found {}", self.tok_desc()),
                    lo: sp.lo as usize,
                    hi: sp.hi as usize,
                })
            }
        }
    }

    // -- expressions ---------------------------------------------------------

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_let_or_if_lambda()
    }

    fn parse_let_or_if_lambda(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Tok::Backslash => self.parse_lambda(),
            Tok::KwLet => self.parse_let_expr(),
            Tok::KwIf => self.parse_if(),
            Tok::KwMatch => self.parse_match(),
            _ => self.parse_postfix(),
        }
    }

    fn parse_lambda(&mut self) -> Result<Expr, ParseError> {
        let bs = self.bump(); // \
        let (param, pspan) = self.parse_name()?;
        self.expect(Tok::Arrow, "`->`")?;
        let param_id = NodeId::new(
            pspan.lo as usize,
            role::PARAM,
            tag_of(role::PARAM, pspan.lo as usize, &param),
        );
        let depth = self.scope.len();
        self.push(param.clone(), param_id.clone(), pspan);
        let body = self.parse_expr()?;
        self.pop_to(depth);
        let span = Span::new(bs.lo, body.span().hi as usize);
        Ok(Expr::Lam {
            param,
            param_id,
            param_span: pspan,
            body: Box::new(body),
            id: NodeId::new(bs.lo, role::LAM, tag_of(role::LAM, bs.lo, "")),
            span,
        })
    }

    fn parse_let_expr(&mut self) -> Result<Expr, ParseError> {
        let let_tok = self.bump();
        let rec = self.eat(&Tok::KwRec).is_some();
        let (name, nspan) = self.parse_name()?;
        let mut params: Vec<(String, NodeId, Span)> = Vec::new();
        while let Tok::Label(_) = self.peek() {
            let t = self.bump();
            if let Tok::Label(p) = t.tok {
                let sp = Span::new(t.lo, t.hi);
                let id = NodeId::new(t.lo, role::PARAM, tag_of(role::PARAM, t.lo, &p));
                self.push(p.clone(), id.clone(), sp);
                params.push((p, id, sp));
            }
        }
        self.expect(Tok::Eq, "`=`")?;
        let depth0 = self.scope.len();
        let binding_id = if rec {
            let anchor = let_tok.lo + 4;
            let id = NodeId::new(
                anchor,
                role::REC_BINDING,
                tag_of(role::REC_BINDING, anchor, &name),
            );
            self.push(name.clone(), id.clone(), nspan);
            id
        } else {
            NodeId::new(let_tok.lo, role::BINDING, tag_of(role::BINDING, let_tok.lo, &name))
        };
        let rhs_anchor = self.peek_span().lo as usize;
        let rhs = self.parse_expr()?;
        self.pop_to(depth0);
        let rhs = desugar_params(rhs_anchor, &params, binding_id.clone(), rhs, self);
        self.expect(Tok::KwIn, "`in`")?;
        self.push(name.clone(), binding_id.clone(), nspan);
        let body = self.parse_expr()?;
        self.pop_to(depth0);
        let span = Span::new(let_tok.lo, body.span().hi as usize);
        Ok(Expr::Let {
            name,
            binding_id,
            rhs: Box::new(rhs),
            body: Box::new(body),
            id: NodeId::new(let_tok.lo, role::LET, tag_of(role::LET, let_tok.lo, "")),
            span,
        })
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let kw = self.bump();
        let cond = self.parse_expr()?;
        self.expect(Tok::KwThen, "`then`")?;
        let then_e = self.parse_expr()?;
        self.expect(Tok::KwElse, "`else`")?;
        let else_e = self.parse_expr()?;
        let span = Span::new(kw.lo, else_e.span().hi as usize);
        Ok(Expr::If {
            cond: Box::new(cond),
            then_e: Box::new(then_e),
            else_e: Box::new(else_e),
            id: NodeId::new(kw.lo, role::IF, tag_of(role::IF, kw.lo, "")),
            span,
        })
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_apply()?;
        loop {
            match self.peek() {
                Tok::KwWith => {
                    let w = self.bump();
                    let (label, _) = self.parse_name()?;
                    self.expect(Tok::Eq, "`=`")?;
                    let value = self.parse_let_or_if_lambda()?;
                    let span = Span::new(w.lo, value.span().hi as usize);
                    e = Expr::With {
                        record: Box::new(e),
                        label,
                        value: Box::new(value),
                        id: NodeId::new(w.lo, role::WITH, tag_of(role::WITH, w.lo, "")),
                        span,
                    };
                }
                Tok::Dot => {
                    let d = self.bump();
                    let (label, lspan) = self.parse_name()?;
                    let span = Span::new(e.span().lo as usize, lspan.hi as usize);
                    e = Expr::Field {
                        record: Box::new(e),
                        label,
                        id: NodeId::new(
                            d.lo,
                            role::FIELD,
                            tag_of(role::FIELD, d.lo, &label),
                        ),
                        span,
                    };
                }
                _ => break,
            }
        }
        Ok(e)
    }

    fn parse_apply(&mut self) -> Result<Expr, ParseError> {
        let mut fun = self.parse_atom()?;
        loop {
            // Application continues while an atom-start token follows with no
            // separator; we stop on tokens that clearly end an expression.
            if !self.starts_atom() {
                break;
            }
            let arg = self.parse_atom()?;
            let span = Span::new(fun.span().lo as usize, arg.span().hi as usize);
            fun = Expr::App {
                fun: Box::new(fun),
                arg: Box::new(arg),
                id: NodeId::new(
                    fun.span().lo as usize,
                    role::APP,
                    tag_of(role::APP, fun.span().lo as usize, ""),
                ),
                span,
            };
        }
        Ok(fun)
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_)
                | Tok::Bool(_)
                | Tok::Label(_)
                | Tok::Upper(_)
                | Tok::LBrace
                | Tok::LParen
                | Tok::Backslash
        )
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let lt = self.peek().clone();
        match lt {
            Tok::Int(n) => {
                let t = self.bump();
                let sp = Span::new(t.lo, t.hi);
                Ok(Expr::LitInt(
                    n,
                    NodeId::new(t.lo, role::LIT_INT, tag_of(role::LIT_INT, t.lo, &n.to_string())),
                    sp,
                ))
            }
            Tok::Bool(b) => {
                let t = self.bump();
                let sp = Span::new(t.lo, t.hi);
                Ok(Expr::LitBool(
                    b,
                    NodeId::new(t.lo, role::LIT_BOOL, tag_of(role::LIT_BOOL, t.lo, &b.to_string())),
                    sp,
                ))
            }
            Tok::Label(name) => {
                let t = self.bump();
                let sp = Span::new(t.lo, t.hi);
                // Unbound names are tolerated here and reported as a type error
                // (`unbound variable`) so that constraint generation stays total.
                let binding = self.lookup(&name).unwrap_or((
                    NodeId::new(u32::MAX as usize, role::VAR, fnv1a(name.as_bytes())),
                    Span::DUMMY,
                ));
                // The tag embeds the *binding* identity, not the name, so
                // renaming a binding keeps every reference identical.
                let tag = if binding.1 == Span::DUMMY {
                    fnv1a(format!("unbound:{}", name).as_bytes())
                } else {
                    fnv1a(format!("var@{}", binding.0.key()).as_bytes())
                };
                Ok(Expr::Var(
                    name,
                    NodeId::new(t.lo, role::VAR, tag),
                    sp,
                ))
            }
            Tok::Upper(tag) => {
                let t = self.bump();
                let payload = if let Tok::LParen = self.peek() {
                    self.bump();
                    let e = self.parse_expr()?;
                    self.expect(Tok::RParen, "`)`")?;
                    Some(Box::new(e))
                } else {
                    None
                };
                let hi = payload.as_ref().map(|p| p.span().hi).unwrap_or(t.hi);
                let span = Span::new(t.lo, hi as usize);
                Ok(Expr::Tag {
                    tag,
                    payload,
                    id: NodeId::new(t.lo, role::TAG, tag_of(role::TAG, t.lo, "")),
                    span,
                })
            }
            Tok::LBrace => self.parse_record(),
            Tok::LParen => {
                self.bump();
                let e = self.parse_expr()?;
                self.expect(Tok::RParen, "`)`")?;
                Ok(e)
            }
            Tok::Backslash => self.parse_lambda(),
            _ => {
                let sp = self.peek_span();
                Err(ParseError {
                    message: format!("expected an expression, found {}", self.tok_desc()),
                    lo: sp.lo as usize,
                    hi: sp.hi as usize,
                })
            }
        }
    }

    fn parse_record(&mut self) -> Result<Expr, ParseError> {
        let lb = self.bump(); // {
        let mut fields = Vec::new();
        if !matches!(self.peek(), Tok::RBrace) {
            loop {
                let (label, _) = self.parse_name()?;
                self.expect(Tok::Eq, "`=`")?;
                let e = self.parse_let_or_if_lambda()?;
                fields.push((label, e));
                if self.eat(&Tok::Comma).is_none() {
                    break;
                }
            }
        }
        let rb = self.expect(Tok::RBrace, "`}`")?;
        let span = Span::new(lb.lo, rb.hi);
        Ok(Expr::Record {
            fields,
            id: NodeId::new(lb.lo, role::RECORD, tag_of(role::RECORD, lb.lo, "")),
            span,
        })
    }

    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        let kw = self.bump(); // match
        let scrut = self.parse_postfix()?;
        self.expect(Tok::KwWith, "`with`")?;
        let mut tag_arms = Vec::new();
        let mut field_arms = Vec::new();
        if self.eat(&Tok::Pipe).is_none() {
            // allow zero-pipe single arm
        }
        loop {
            // Decide arm kind from the head token.
            match self.peek().clone() {
                Tok::Upper(tag) => {
                    let t = self.bump();
                    let arm_start = t.lo;
                    let payload = if let Tok::LParen = self.peek() {
                        self.bump();
                        let (pname, pspan) = self.parse_name()?;
                        self.expect(Tok::RParen, "`)`")?;
                        let pid = NodeId::new(
                            pspan.lo as usize,
                            role::PARAM,
                            tag_of(role::PARAM, pspan.lo as usize, &pname),
                        );
                        Some((pname, pid, pspan))
                    } else {
                        None
                    };
                    self.expect(Tok::Arrow, "`->`")?;
                    let depth = self.scope.len();
                    if let Some((p, id, sp)) = &payload {
                        self.push(p.clone(), id.clone(), *sp);
                    }
                    let body = self.parse_expr()?;
                    self.pop_to(depth);
                    let span = Span::new(arm_start, body.span().hi as usize);
                    tag_arms.push(TagArm { tag, payload, body, span });
                }
                Tok::Label(label) => {
                    let t = self.bump();
                    let arm_start = t.lo;
                    self.expect(Tok::LParen, "`(` for field view arm `label(x) => ...`")?;
                    let (bound, bspan) = self.parse_name()?;
                    self.expect(Tok::RParen, "`)`")?;
                    self.expect(Tok::FatArrow, "`=>`")?;
                    let bid = NodeId::new(
                        bspan.lo as usize,
                        role::PARAM,
                        tag_of(role::PARAM, bspan.lo as usize, &bound),
                    );
                    let depth = self.scope.len();
                    self.push(bound.clone(), bid.clone(), bspan);
                    let body = self.parse_expr()?;
                    self.pop_to(depth);
                    let span = Span::new(arm_start, body.span().hi as usize);
                    field_arms.push(FieldArm {
                        label,
                        bound,
                        bound_id: bid,
                        bound_span: bspan,
                        body,
                        span,
                    });
                }
                _ => {
                    let sp = self.peek_span();
                    return Err(ParseError {
                        message: format!(
                            "expected a match arm (variant tag or record label), found {}",
                            self.tok_desc()
                        ),
                        lo: sp.lo as usize,
                        hi: sp.hi as usize,
                    });
                }
            }
            if self.eat(&Tok::Pipe).is_none() {
                break;
            }
        }
        if tag_arms.is_empty() && field_arms.is_empty() {
            return Err(ParseError {
                message: "`match` needs at least one arm".into(),
                lo: kw.lo,
                hi: kw.hi,
            });
        }
        let last_hi = tag_arms
            .last()
            .map(|a| a.span.hi)
            .or_else(|| field_arms.last().map(|a| a.span.hi))
            .unwrap_or(kw.hi);
        let span = Span::new(kw.lo, last_hi as usize);
        Ok(Expr::Match {
            scrut: Box::new(scrut),
            tag_arms,
            field_arms,
            id: NodeId::new(kw.lo, role::MATCH, tag_of(role::MATCH, kw.lo, "")),
            span,
        })
    }
}

/// `let rec f x y = e` stores params on the binding; for expression-level
/// `let f x = e in b` we wrap the RHS in lambdas here so the core typechecker
/// only needs plain lambda.
fn desugar_params(
    anchor: usize,
    params: &[(String, NodeId, Span)],
    _binding: NodeId,
    body: Expr,
    p: &mut Parser,
) -> Expr {
    let mut e = body;
    for (name, _id, sp) in params.iter().rev() {
        let span = Span::new(anchor, e.span().hi as usize);
        let param_id = NodeId::new(
            sp.lo as usize,
            role::PARAM,
            tag_of(role::PARAM, sp.lo as usize, name),
        );
        e = Expr::Lam {
            param: name.clone(),
            param_id,
            param_span: *sp,
            body: Box::new(e),
            id: NodeId::new(anchor, role::LAM, tag_of(role::LAM, anchor, name)),
            span,
        };
    }
    let _ = p;
    e
}

pub fn parse(src: &str) -> Result<Vec<Binding>, Vec<ParseError>> {
    let mut parser = Parser::new(src)?;
    parser.parse_program()
}
