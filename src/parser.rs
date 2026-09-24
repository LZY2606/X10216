//! Pratt-style parser. After parsing, a scope-resolution pass replaces
//! [`Expr::Var`] nodes with [`Expr::ResolvedVar`] pointing at binder nodes.

use std::rc::Rc;

use crate::ast::{Ast, Expr, NodeData, NodeId, Pat};
use crate::lexer::{Lexer, Tok, Token};
use crate::types::{RcStr, Span};

#[derive(Clone, Debug)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

struct Parser<'a> {
    toks: Vec<Token>,
    pos: usize,
    ast: Ast,
    _src: &'a str,
}

impl<'a> Parser<'a> {
    fn cur(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn cur_span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.cur() == t {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.cur() == &t {
            Ok(self.bump())
        } else {
            Err(ParseError {
                msg: format!("expected {what}, found {}", tok_name(self.cur())),
                span: self.cur_span(),
            })
        }
    }

    fn mk_e(&mut self, e: Expr, span: Span) -> NodeId {
        self.ast.add(NodeData::E(e), span)
    }

    // ---- entry -----------------------------------------------------------
    fn parse_program(&mut self) -> Result<NodeId, ParseError> {
        let start = self.cur_span().start as usize;
        let mut exprs = Vec::new();
        loop {
            exprs.push(self.parse_toplevel_let()?);
            if !self.eat(&Tok::Semicolon) {
                break;
            }
        }
        self.expect(Tok::Eof, "end of input")?;
        let end = self.toks[self.pos.saturating_sub(1)].span.end as usize;
        let span = Span::new(start, end);
        Ok(if exprs.len() == 1 {
            exprs.remove(0)
        } else {
            self.mk_e(Expr::Seq(exprs), span)
        })
    }

    /// A top-level `let x = e` without `in` becomes `let x = e in <rest>`.
    fn parse_toplevel_let(&mut self) -> Result<NodeId, ParseError> {
        if matches!(self.cur(), Tok::Let) {
            let start = self.toks[self.pos].span.start as usize;
            self.bump();
            let rec_kw = self.eat(&Tok::Rec);
            let pat = self.parse_simple_pat()?;
            self.expect(Tok::Eq, "'='")?;
            let value = self.parse_expr()?;
            let body = if matches!(self.cur(), Tok::Semicolon) {
                self.bump();
                self.parse_toplevel_let()?
            } else {
                self.expect(Tok::In, "'in'")?;
                self.parse_expr()?
            };
            let span = Span::new(start, self.ast.span_of(body).end as usize);
            Ok(self.mk_e(Expr::Let { pat, value, body, rec_kw }, span))
        } else {
            self.parse_expr()
        }
    }

    // ---- precedence: let < fun < if/match < binop < app < atom ----------
    fn parse_expr(&mut self) -> Result<NodeId, ParseError> {
        match self.cur() {
            Tok::Let => self.parse_let(),
            Tok::Fun | Tok::Backslash => self.parse_fun(),
            Tok::If => self.parse_if(),
            Tok::Match => self.parse_match(),
            _ => self.parse_seq_arrow(),
        }
    }

    fn parse_let(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span.start as usize;
        let rec_kw = self.eat(&Tok::Rec);
        let pat = self.parse_simple_pat()?;
        self.expect(Tok::Eq, "'='")?;
        let value = self.parse_expr()?;
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr()?;
        let span = Span::new(start, self.ast.span_of(body).end as usize);
        Ok(self.mk_e(Expr::Let { pat, value, body, rec_kw }, span))
    }

    fn parse_fun(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span.start as usize;
        let param = self.parse_simple_pat()?;
        self.expect(Tok::Arrow, "'->'")?;
        let body = self.parse_expr()?;
        let span = Span::new(start, self.ast.span_of(body).end as usize);
        Ok(self.mk_e(Expr::Lam { param, body }, span))
    }

    fn parse_if(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span.start as usize;
        let cond = self.parse_expr()?;
        self.expect(Tok::Then, "'then'")?;
        let then_b = self.parse_expr()?;
        self.expect(Tok::Else, "'else'")?;
        let else_b = self.parse_expr()?;
        let span = Span::new(start, self.ast.span_of(else_b).end as usize);
        Ok(self.mk_e(Expr::If { cond, then_b, else_b }, span))
    }

    fn parse_match(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span.start as usize;
        let scrut = self.parse_expr()?;
        self.expect(Tok::With, "'with'")?;
        let mut arms = Vec::new();
        loop {
            self.eat(&Tok::Bar);
            let p = self.parse_pat()?;
            self.expect(Tok::FatArrow, "'=>'")?;
            let b = self.parse_expr()?;
            arms.push((p, b));
            if !matches!(self.cur(), Tok::Bar) {
                break;
            }
        }
        let end = arms.last().map(|(_, b)| self.ast.span_of(*b).end as usize)
            .unwrap_or(start);
        Ok(self.mk_e(Expr::Match { scrut, arms }, Span::new(start, end)))
    }

    /// `->` is right associative and only allowed in a few places; we parse
    /// binary comparisons first, then allow an explicit top-level arrow.
    fn parse_seq_arrow(&mut self) -> Result<NodeId, ParseError> {
        let left = self.parse_binop(0)?;
        if matches!(self.cur(), Tok::Arrow) {
            let start = self.ast.span_of(left).start as usize;
            self.bump();
            let body = self.parse_expr()?;
            // Wrap the lhs as a synthetic lambda parameter pattern when it is a
            // plain variable; arrows outside fun-literals are otherwise
            // rejected, keeping the grammar small.
            return Err(ParseError {
                msg: "unexpected '->'; use 'fun x -> ...'".into(),
                span: Span::new(start, self.ast.span_of(body).end as usize),
            });
        }
        Ok(left)
    }

    fn binop_tok(&self) -> Option<(Tok, u8, u8)> {
        Some(match self.cur().clone() {
            Tok::Or => (Tok::Or, 1, 2),
            Tok::And => (Tok::And, 3, 4),
            Tok::Lt => (Tok::Lt, 5, 6),
            Tok::Plus | Tok::Minus => (Tok::Plus, 7, 8),
            Tok::Star | Tok::Slash => (Tok::Star, 9, 10),
            _ => return None,
        })
    }

    fn parse_binop(&mut self, min_prec: u8) -> Result<NodeId, ParseError> {
        let mut left = self.parse_unary()?;
        while let Some((op_tok, lp, rp)) = self.binop_tok() {
            if lp < min_prec {
                break;
            }
            let op_span = self.cur_span();
            self.bump();
            let right = self.parse_binop(rp)?;
            let name = match op_tok {
                Tok::Or => "||", Tok::And => "&&", Tok::Lt => "<",
                Tok::Plus => "+", Tok::Minus => "-",
                Tok::Star => "*", Tok::Slash => "/",
                _ => unreachable!(),
            };
            let span = Span::new(
                self.ast.span_of(left).start as usize,
                self.ast.span_of(right).end as usize,
            );
            let opv = self.mk_e(Expr::Var(Rc::from(name)), op_span);
            let app1 = self.mk_e(Expr::App { fun: opv, arg: left }, span);
            left = self.mk_e(Expr::App { fun: app1, arg: right }, span);
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<NodeId, ParseError> {
        if matches!(self.cur(), Tok::Not) {
            let t = self.bump();
            let e = self.parse_unary()?;
            let span = Span::new(t.span.start as usize,
                self.ast.span_of(e).end as usize);
            let op = self.mk_e(Expr::Var(Rc::from("!")), t.span);
            return Ok(self.mk_e(Expr::App { fun: op, arg: e }, span));
        }
        if matches!(self.cur(), Tok::Minus) {
            let t = self.bump();
            let e = self.parse_unary()?;
            let span = Span::new(t.span.start as usize,
                self.ast.span_of(e).end as usize);
            let zero = self.mk_e(Expr::Int(0), t.span);
            let op = self.mk_e(Expr::Var(Rc::from("-")), t.span);
            let app1 = self.mk_e(Expr::App { fun: op, arg: zero }, span);
            return Ok(self.mk_e(Expr::App { fun: app1, arg: e }, span));
        }
        self.parse_app()
    }

    fn parse_app(&mut self) -> Result<NodeId, ParseError> {
        let mut head = self.parse_postfix(self.parse_atom()?)?;
        while self.starts_atom() {
            let arg = self.parse_postfix(self.parse_atom()?)?;
            let span = Span::new(
                self.ast.span_of(head).start as usize,
                self.ast.span_of(arg).end as usize,
            );
            head = self.mk_e(Expr::App { fun: head, arg }, span);
        }
        Ok(head)
    }

    fn parse_postfix(&mut self, e: NodeId) -> Result<NodeId, ParseError> {
        let mut cur = e;
        while matches!(self.cur(), Tok::Dot) {
            self.bump();
            let name = self.expect_ident("field name")?;
            let end_span = self.toks[self.pos.saturating_sub(1)].span;
            let span = Span::new(self.ast.span_of(cur).start as usize,
                end_span.end as usize);
            cur = self.mk_e(Expr::Field { record: cur, name: Rc::from(name) }, span);
        }
        Ok(cur)
    }

    fn expect_ident(&mut self, what: &str) -> Result<String, ParseError> {
        if let Tok::Ident(s) = self.cur().clone() {
            self.bump();
            Ok(s)
        } else {
            Err(ParseError {
                msg: format!("expected {what}, found {}", tok_name(self.cur())),
                span: self.cur_span(),
            })
        }
    }

    fn starts_atom(&self) -> bool {
        matches!(self.cur(),
            Tok::Int(_) | Tok::Str(_) | Tok::Ident(_) | Tok::True | Tok::False
            | Tok::LParen | Tok::LBrace | Tok::Backtick | Tok::Underscore)
    }

    fn parse_atom(&mut self) -> Result<NodeId, ParseError> {
        let t = self.bump();
        let span = t.span;
        match t.tok {
            Tok::Int(n) => Ok(self.mk_e(Expr::Int(n), span)),
            Tok::Str(s) => Ok(self.mk_e(Expr::Str(Rc::from(s.as_str())), span)),
            Tok::True => Ok(self.mk_e(Expr::Bool(true), span)),
            Tok::False => Ok(self.mk_e(Expr::Bool(false), span)),
            Tok::Ident(name) => {
                // parenthesized unit: () handled via LParen below
                Ok(self.mk_e(Expr::Var(Rc::from(name.as_str())), span))
            }
            Tok::Underscore => Ok(self.mk_e(Expr::Var(Rc::from("_")), span)),
            Tok::LParen => {
                if self.eat(&Tok::RParen) {
                    return Ok(self.mk_e(Expr::Unit, span));
                }
                let inner = self.parse_expr()?;
                self.expect(Tok::RParen, "')'")?;
                Ok(inner)
            }
            Tok::LBrace => self.parse_record_body(span.start as usize),
            Tok::Backtick => self.parse_variant(span.start as usize),
            other => Err(ParseError {
                msg: format!("unexpected {}", tok_name(&other)),
                span,
            }),
        }
    }

    fn parse_record_body(&mut self, start: usize) -> Result<NodeId, ParseError> {
        // `{ }`            : empty closed record
        // `{ a = e, .. b }`: extension; without `..` it's a closed literal
        let mut fields: Vec<(RcStr, NodeId)> = Vec::new();
        let mut base: Option<NodeId> = None;
        let mut closed_lit = true;
        if !self.eat(&Tok::RBrace) {
            loop {
                if self.eat(&Tok::Dot) {
                    self.expect(Tok::Dot, "'..' for row extension")?;
                    base = Some(self.parse_atom()?);
                    closed_lit = false;
                } else {
                    let name = self.expect_ident("field name")?;
                    self.expect(Tok::Eq, "'='")?;
                    let val = self.parse_expr()?;
                    fields.push((Rc::from(name.as_str()), val));
                }
                if !self.eat(&Tok::Comma) {
                    break;
                }
            }
            self.expect(Tok::RBrace, "'}'")?;
        }
        let end_tok = self.toks[self.pos.saturating_sub(1)].span;
        let span = Span::new(start, end_tok.end as usize);
        let expr = if base.is_some() {
            Expr::Extend { base, fields }
        } else if closed_lit {
            Expr::RecordLit(fields)
        } else {
            Expr::Record(fields)
        };
        Ok(self.mk_e(expr, span))
    }

    fn parse_variant(&mut self, start: usize) -> Result<NodeId, ParseError> {
        // `` `label expr ``  (payload optional -> unit)
        let label = self.expect_ident("variant label after `")?;
        let payload = if self.starts_atom() {
            self.parse_postfix(self.parse_atom()?)?
        } else {
            self.mk_e(Expr::Unit, Span::new(start, start + 1))
        };
        let span = Span::new(start, self.ast.span_of(payload).end as usize);
        Ok(self.mk_e(Expr::Variant {
            label: Rc::from(label.as_str()), payload,
        }, span))
    }

    // ---- patterns --------------------------------------------------------
    fn parse_simple_pat(&mut self) -> Result<NodeId, ParseError> {
        match self.cur() {
            Tok::Ident(_) | Tok::Underscore => {
                let t = self.bump();
                let pat = if let Tok::Ident(s) = &t.tok {
                    Pat::PVar(Rc::from(s.as_str()))
                } else { Pat::PWild };
                Ok(self.ast.add(NodeData::P(pat), t.span))
            }
            other => Err(ParseError {
                msg: format!("expected a binding name, found {}", tok_name(other)),
                span: self.cur_span(),
            }),
        }
    }

    fn parse_pat(&mut self) -> Result<NodeId, ParseError> {
        match self.cur().clone() {
            Tok::Underscore => {
                let t = self.bump();
                Ok(self.ast.add(NodeData::P(Pat::PWild), t.span))
            }
            Tok::Ident(s) => {
                let t = self.bump();
                Ok(self.ast.add(
                    NodeData::P(Pat::PVar(Rc::from(s.as_str()))), t.span))
            }
            Tok::Backtick => {
                let bt = self.bump();
                let label = self.expect_ident("variant label")?;
                // payload is either a parenthesized pattern or a bare name
                let payload = if matches!(self.cur(), Tok::Ident(_) | Tok::Underscore) {
                    self.parse_simple_pat()?
                } else {
                    self.expect(Tok::LParen, "'(' or payload name")?;
                    let inner = self.parse_pat()?;
                    self.expect(Tok::RParen, "')'")?;
                    inner
                };
                let span = Span::new(bt.span.start as usize,
                    self.ast.span_of(payload).end as usize);
                Ok(self.ast.add(NodeData::P(Pat::PVariant {
                    label: Rc::from(label.as_str()), payload,
                }), span))
            }
            Tok::LBrace => {
                let lb = self.bump();
                let mut fields = Vec::new();
                if !self.eat(&Tok::RBrace) {
                    loop {
                        let name = self.expect_ident("field name")?;
                        self.expect(Tok::Colon, "':'")?;
                        let p = self.parse_pat()?;
                        fields.push((Rc::from(name.as_str()), p));
                        if !self.eat(&Tok::Comma) { break; }
                    }
                    self.expect(Tok::RBrace, "'}'")?;
                }
                let span = Span::new(lb.span.start as usize,
                    self.toks[self.pos.saturating_sub(1)].span.end as usize);
                Ok(self.ast.add(NodeData::P(Pat::PRecord(fields)), span))
            }
            other => Err(ParseError {
                msg: format!("expected a pattern, found {}", tok_name(&other)),
                span: self.cur_span(),
            }),
        }
    }
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Eof => "end of input".to_string(),
        Tok::Int(_) => "integer".to_string(),
        Tok::Str(_) => "string".to_string(),
        Tok::Ident(s) => format!("identifier '{s}'"),
        _ => format!("{t:?}"),
    }
}

/// Parse and run scope resolution.
pub fn parse(src: &str) -> Result<Ast, ParseError> {
    let toks = Lexer::new(src).tokenize()?;
    let mut p = Parser { toks, pos: 0, ast: Ast::new(), _src: src };
    let root = p.parse_program()?;
    p.ast.root = root;
    let ast = p.ast;
    resolve_scopes(ast)
}

/// Lexical scoping. Builtins are pre-populated so they resolve like ordinary
/// names (with a marker binder of `u32::MAX`).
pub fn resolve_scopes(mut ast: Ast) -> Result<Ast, ParseError> {
    let root = ast.root;
    let mut scopes: Vec<Vec<(RcStr, Option<NodeId>)>> = vec![Vec::new()];
    for b in BUILTINS {
        scopes[0].push((Rc::from(*b), None));
    }
    resolve_node(&mut ast, root, &mut scopes)?;
    Ok(ast)
}

pub const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "<", "&&", "||", "!",
    "print_int", "print_str", "int_to_str", "string_concat",
];

fn lookup(scopes: &[Vec<(RcStr, Option<NodeId>)>], name: &str) -> Option<Option<NodeId>> {
    for s in scopes.iter().rev() {
        for (n, b) in s.iter().rev() {
            if n.as_ref() == name {
                return Some(*b);
            }
        }
    }
    None
}

fn bind_pattern(ast: &Ast, pat: NodeId, scopes: &mut Vec<Vec<(RcStr, Option<NodeId>)>>) {
    match &ast.nodes[pat.0 as usize].data {
        NodeData::P(Pat::PVar(n)) if n.as_ref() != "_" => {
            scopes.last_mut().unwrap().push((n.clone(), Some(pat)))
        }
        NodeData::P(Pat::PRecord(fs)) => {
            for (_, p) in fs {
                bind_pattern(ast, *p, scopes);
            }
        }
        NodeData::P(Pat::PVariant { payload, .. }) => {
            bind_pattern(ast, *payload, scopes)
        }
        _ => {}
    }
}

fn resolve_node(ast: &mut Ast, id: NodeId,
        scopes: &mut Vec<Vec<(RcStr, Option<NodeId>)>>) -> Result<(), ParseError> {
    let data = ast.nodes[id.0 as usize].data.clone();
    match data {
        NodeData::E(Expr::Var(name)) => {
            match lookup(scopes, &name) {
                Some(binder) => {
                    ast.nodes[id.0 as usize].data =
                        NodeData::E(Expr::ResolvedVar { name, binder });
                }
                None => return Err(ParseError {
                    msg: format!("unbound variable '{}'", name),
                    span: ast.nodes[id.0 as usize].span,
                }),
            }
        }
        NodeData::E(Expr::Lam { param, body }) => {
            resolve_node(ast, param, scopes)?;
            scopes.push(Vec::new());
            bind_pattern(ast, param, scopes);
            resolve_node(ast, body, scopes)?;
            scopes.pop();
        }
        NodeData::E(Expr::Let { pat, value, body, rec_kw }) => {
            scopes.push(Vec::new());
            if rec_kw {
                bind_pattern(ast, pat, scopes);
                resolve_node(ast, value, scopes)?;
            } else {
                resolve_node(ast, value, scopes)?;
            }
            resolve_node(ast, pat, scopes)?;
            bind_pattern(ast, pat, scopes);
            resolve_node(ast, body, scopes)?;
            scopes.pop();
        }
        NodeData::E(Expr::Match { scrut, arms }) => {
            resolve_node(ast, scrut, scopes)?;
            for (p, b) in arms {
                resolve_node(ast, p, scopes)?;
                scopes.push(Vec::new());
                bind_pattern(ast, p, scopes);
                resolve_node(ast, b, scopes)?;
                scopes.pop();
            }
        }
        NodeData::E(e) => {
            let kids = child_exprs(&e);
            for k in kids {
                resolve_node(ast, k, scopes)?;
            }
        }
        NodeData::P(Pat::PVariant { payload, .. }) => {
            resolve_node(ast, payload, scopes)?;
        }
        NodeData::P(Pat::PRecord(fs)) => {
            for (_, p) in fs { resolve_node(ast, p, scopes)?; }
        }
        _ => {}
    }
    Ok(())
}

fn child_exprs(e: &Expr) -> Vec<NodeId> {
    match e {
        Expr::App { fun, arg } => vec![*fun, *arg],
        Expr::If { cond, then_b, else_b } => vec![*cond, *then_b, *else_b],
        Expr::Field { record, .. } => vec![*record],
        Expr::Record(xs) | Expr::RecordLit(xs) => xs.iter().map(|(_, n)| *n).collect(),
        Expr::Extend { base, fields } => {
            let mut v: Vec<NodeId> = base.into_iter().copied().collect();
            v.extend(fields.iter().map(|(_, n)| *n));
            v
        }
        Expr::Variant { payload, .. } => vec![*payload],
        Expr::Seq(xs) => xs.clone(),
        _ => Vec::new(),
    }
}
