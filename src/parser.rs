//! Hand-written Pratt parser; lowers to a stable-ID `Program`.

use std::collections::HashMap;

use crate::lexer::{lex, Tok, Token};
use crate::syntax::{Binder, DefKind, Kind, NId, Node, Op, ParseError, Program, Span};

#[derive(Clone)]
pub struct RExpr {
    kind: RKind,
    span: Span,
    label: Option<String>,
    op: Option<String>,
    rec: bool,
    closed: bool,
    children: Vec<RExpr>,
    ann: Option<RType>,
}

#[derive(Clone, PartialEq)]
enum RKind {
    Program,
    Int,
    Str,
    Bool,
    Var,
    Fn,
    Param,
    App,
    Let,
    Binder,
    If,
    BinOp,
    Record,
    RecordField,
    Proj,
    With,
    Variant,
    Match,
    Arm,
    TagBinder,
    Wild,
    Ann,
}

#[derive(Clone)]
pub struct RType {
    kind: RTKind,
    span: Span,
    label: Option<String>,
    closed: bool,
    children: Vec<RType>,
}

#[derive(Clone, PartialEq)]
enum RTKind {
    Con,
    Arrow,
    Record,
    Variant,
    Field,
    Tag,
}

impl RExpr {
    fn mk(kind: RKind, span: Span) -> Self {
        RExpr {
            kind,
            span,
            label: None,
            op: None,
            rec: false,
            closed: false,
            children: Vec::new(),
            ann: None,
        }
    }
}

struct Parser<'a> {
    toks: &'a [Token],
    pos: usize,
    errors: Vec<ParseError>,
}

fn same_disc(a: &Tok, b: &Tok) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn peek_tok(&self) -> &Token {
        &self.toks[self.pos]
    }
    fn at(&self, t: &Tok) -> bool {
        same_disc(self.peek(), t)
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos < self.toks.len() - 1 {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.at(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Option<Token> {
        if same_disc(self.peek(), &t) {
            Some(self.bump())
        } else {
            let sp = self.peek_tok().span;
            self.errors.push(ParseError {
                span: sp,
                message: format!("expected {what}"),
            });
            None
        }
    }
    fn err(&mut self, sp: Span, msg: impl Into<String>) {
        self.errors.push(ParseError {
            span: sp,
            message: msg.into(),
        });
    }
}

// ---------------- type parsing ----------------

impl<'a> Parser<'a> {
    fn parse_type(&mut self) -> RType {
        self.parse_arrow_type()
    }

    fn parse_arrow_type(&mut self) -> RType {
        let lhs = self.parse_atom_type();
        if self.at(&Tok::Arrow) {
            let start = lhs.span;
            self.bump();
            let rhs = self.parse_arrow_type();
            RType {
                kind: RTKind::Arrow,
                span: start.merge(rhs.span),
                label: None,
                closed: false,
                children: vec![lhs, rhs],
            }
        } else {
            lhs
        }
    }

    fn parse_row_type(&mut self, is_record: bool) -> RType {
        let open_tok = self.bump();
        let mut rows: Vec<RType> = Vec::new();
        let mut closed = true;
        loop {
            let closer = if is_record { Tok::RBrace } else { Tok::RAngle };
            if self.at(&closer) {
                break;
            }
            if self.at(&Tok::Pipe) {
                self.bump();
                closed = false;
                break;
            }
            let label_tok = self.bump();
            let label = match label_tok.tok {
                Tok::Ident(s) if is_record => s,
                other => {
                    if is_record {
                        self.err(label_tok.span, "expected field label");
                        String::new()
                    } else if let Tok::Ident(s) = other {
                        s
                    } else {
                        self.err(label_tok.span, "expected tag name");
                        String::new()
                    }
                }
            };
            let (ty, row_span) = if is_record {
                self.expect(Tok::Colon, "':'");
                let ty = self.parse_type();
                (ty, label_tok.span.merge(ty.span))
            } else {
                let unit = RType {
                    kind: RTKind::Con,
                    span: label_tok.span,
                    label: Some("Unit".into()),
                    closed: false,
                    children: Vec::new(),
                };
                (unit, label_tok.span)
            };
            rows.push(RType {
                kind: if is_record { RTKind::Field } else { RTKind::Tag },
                span: row_span,
                label: Some(label),
                closed: false,
                children: vec![ty],
            });
            if !self.eat(&Tok::Comma) {
                break;
            }
        }
        let closer = if is_record { Tok::RBrace } else { Tok::RAngle };
        let end = self
            .expect(closer, if is_record { "'}'" } else { "'>'" })
            .map(|t| t.span.end)
            .unwrap_or(open_tok.span.end);
        RType {
            kind: if is_record { RTKind::Record } else { RTKind::Variant },
            span: Span::new(open_tok.span.start, end),
            label: None,
            closed,
            children: rows,
        }
    }

    fn parse_atom_type(&mut self) -> RType {
        match self.peek().clone() {
            Tok::LBrace => self.parse_row_type(true),
            Tok::LAngle => self.parse_row_type(false),
            Tok::LParen => {
                let lp = self.bump();
                let mut inner = self.parse_type();
                let end = self
                    .expect(Tok::RParen, "')'")
                    .map(|t| t.span.end)
                    .unwrap_or(lp.span.end);
                inner.span = Span::new(lp.span.start, end);
                inner
            }
            Tok::Ident(name) => {
                let t = self.bump();
                RType {
                    kind: RTKind::Con,
                    span: t.span,
                    label: Some(name),
                    closed: false,
                    children: Vec::new(),
                }
            }
            _ => {
                let t = self.bump();
                self.err(t.span, "expected a type");
                RType {
                    kind: RTKind::Con,
                    span: t.span,
                    label: Some("Int".into()),
                    closed: false,
                    children: Vec::new(),
                }
            }
        }
    }
}

// ---------------- expression parsing ----------------

impl<'a> Parser<'a> {
    fn bin_bp(op: &str) -> Option<(u8, u8)> {
        Some(match op {
            "&&" => (1, 2),
            "==" | "<" => (3, 4),
            "+" | "-" => (5, 6),
            "*" => (7, 8),
            _ => return None,
        })
    }

    fn parse_expr(&mut self, min_bp: u8) -> RExpr {
        let mut lhs = self.parse_prefix();
        loop {
            // postfix: application and projection
            match self.peek() {
                Tok::Dot => {
                    self.bump();
                    let lab = self.bump();
                    let label = if let Tok::Ident(s) = lab.tok {
                        s
                    } else {
                        self.err(lab.span, "expected field name after '.'");
                        String::new()
                    };
                    let span = lhs.span.merge(lab.span);
                    let mut node = RExpr::mk(RKind::Proj, span);
                    node.label = Some(label);
                    node.children.push(lhs);
                    lhs = node;
                    continue;
                }
                Tok::Op(s) => {
                    let op_name = s.clone();
                    let (lbp, rbp) = match Self::bin_bp(&op_name) {
                        Some(v) => v,
                        None => break,
                    };
                    if lbp < min_bp {
                        break;
                    }
                    self.bump();
                    let rhs = self.parse_expr(rbp);
                    let span = lhs.span.merge(rhs.span);
                    let mut node = RExpr::mk(RKind::BinOp, span);
                    node.op = Some(op_name);
                    node.children.push(lhs);
                    node.children.push(rhs);
                    lhs = node;
                    continue;
                }
                Tok::Colon => {
                    if min_bp > 0 {
                        break;
                    }
                    self.bump();
                    let ty = self.parse_type();
                    let span = lhs.span.merge(ty.span);
                    let mut node = RExpr::mk(RKind::Ann, span);
                    node.children.push(lhs);
                    node.ann = Some(ty);
                    lhs = node;
                    continue;
                }
                _ => {}
            }
            if self.postfix_arg_start() {
                let arg = self.parse_prefix();
                let span = lhs.span.merge(arg.span);
                let mut node = RExpr::mk(RKind::App, span);
                node.children.push(lhs);
                node.children.push(arg);
                lhs = node;
                continue;
            }
            break;
        }
        lhs
    }

    fn postfix_arg_start(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_)
                | Tok::Str(_)
                | Tok::True
                | Tok::False
                | Tok::Ident(_)
                | Tok::LParen
                | Tok::LBrace
                | Tok::Fn
                | Tok::If
                | Tok::Match
        )
    }

    fn is_tag(s: &str) -> bool {
        s.chars().next().is_some_and(|c| c.is_ascii_uppercase())
    }

    fn parse_prefix(&mut self) -> RExpr {
        let t = self.peek_tok().clone();
        match &t.tok {
            Tok::Int(n) => {
                self.bump();
                let mut e = RExpr::mk(RKind::Int, t.span);
                e.label = Some(n.to_string());
                e
            }
            Tok::Str(s) => {
                self.bump();
                let mut e = RExpr::mk(RKind::Str, t.span);
                e.label = Some(s.clone());
                e
            }
            Tok::True | Tok::False => {
                self.bump();
                let mut e = RExpr::mk(RKind::Bool, t.span);
                e.label = Some(if matches!(t.tok, Tok::True) { "true".into() } else { "false".into() });
                e
            }
            Tok::LParen => {
                self.bump();
                let e = self.parse_expr(0);
                self.expect(Tok::RParen, "')'");
                let mut e = e;
                e.span = Span::new(t.span.start, self.toks[self.pos.saturating_sub(1)].span.end);
                e
            }
            Tok::Ident(_) => {
                self.bump();
                let name = if let Tok::Ident(s) = t.tok { s } else { unreachable!() };
                let mut e = RExpr::mk(RKind::Var, t.span);
                e.label = Some(name);
                e
            }
            Tok::Fn => self.parse_fn(),
            Tok::Let => self.parse_let(false),
            Tok::If => self.parse_if(),
            Tok::Match => self.parse_match(),
            Tok::LBrace => self.parse_record(),
            Tok::Ident(s) if Self::is_tag(s) => {
                self.bump();
                // bare tag unit value
                let mut e = RExpr::mk(RKind::Variant, t.span);
                e.label = Some(s.clone());
                e
            }
            other => {
                self.bump();
                self.err(t.span, format!("unexpected token {other:?}"));
                RExpr::mk(RKind::Int, t.span)
            }
        }
    }

    fn parse_match(&mut self) -> RExpr {
        let mt = self.bump(); // match
        // tag with payload is written `Tag(e)` as an application:
        // handled because uppercase ident prefix returns Variant, then app.
        let scrut = self.parse_expr(0);
        self.expect(Tok::With, "'with'");
        let mut arms = Vec::new();
        loop {
            if self.at(&Tok::Eof) || arms_break(self.peek()) {
                break;
            }
            self.expect(Tok::Pipe, "'|'");
            let tag_tok = self.bump();
            let (label, has_binder, arm_start) = match tag_tok.tok {
                Tok::Ident(s) => {
                    let tag = if Self::is_tag(&s) {
                        s
                    } else {
                        self.err(tag_tok.span, "match arm tag must start uppercase");
                        s
                    };
                    (tag, false, tag_tok.span.start)
                }
                _ => {
                    self.err(tag_tok.span, "expected tag in match arm");
                    (String::new(), false, tag_tok.span.start)
                }
            };
            // optional payload binder
            let has_binder = if self.at(&Tok::LParen) {
                self.bump();
                let b = self.bump();
                if let Tok::Ident(bn) = b.tok {
                    let mut binder = RExpr::mk(RKind::TagBinder, b.span);
                    binder.label = Some(bn);
                    arms_payload_holder(&mut arms, binder);
                } else {
                    self.err(b.span, "expected payload binder");
                }
                self.expect(Tok::RParen, "')'");
                true
            } else {
                false
            };
            self.expect(Tok::FatArrow, "'=>'");
            let body = self.parse_expr(0);
            let span = Span::new(arm_start, body.span.end);
            let mut arm = RExpr::mk(RKind::Arm, span);
            arm.label = Some(label);
            arm.children.push(body);
            if let Some(b) = arms.drain(..).last() {
                let mut arm2 = RExpr::mk(RKind::Arm, span);
                arm2.label = arm.label.clone();
                arm2.children.push(b);
                arm2.children.push(body);
                arms.push(arm2);
            } else {
                arms.push(arm);
            }
            let _ = has_binder;
        }
        let end = arms.last().map(|a| a.span.end).unwrap_or(mt.span.end);
        let mut n = RExpr::mk(RKind::Match, Span::new(mt.span.start, end));
        n.children.push(scrut);
        n.children.extend(arms);
        n
    }

    fn parse_param(&mut self) -> RExpr {
        if self.at(&Tok::LParen) {
            self.bump();
            let id = self.bump();
            let name = match id.tok {
                Tok::Ident(s) => s,
                _ => {
                    self.err(id.span, "expected parameter name");
                    "_".into()
                }
            };
            self.expect(Tok::Colon, "':'");
            let ann = self.parse_type();
            let end = self
                .expect(Tok::RParen, "')'")
                .map(|x| x.span.end)
                .unwrap_or(id.span.end);
            let mut p = RExpr::mk(RKind::Param, Span::new(id.span.start, end));
            p.label = Some(name);
            p.ann = Some(ann);
            p
        } else {
            let id = self.bump();
            match id.tok {
                Tok::Ident(s) => {
                    let mut p = RExpr::mk(RKind::Param, id.span);
                    p.label = Some(s);
                    p
                }
                _ => {
                    self.err(id.span, "expected parameter name");
                    RExpr::mk(RKind::Param, id.span)
                }
            }
        }
    }

    fn parse_fn(&mut self) -> RExpr {
        let start = self.bump().span.start; // fn
        let mut params = Vec::new();
        loop {
            if self.at(&Tok::FatArrow) {
                break;
            }
            params.push(self.parse_param());
        }
        self.expect(Tok::FatArrow, "'=>'");
        let body = self.parse_expr(0);
        let mut node = RExpr::mk(RKind::Fn, Span::new(start, body.span.end));
        // nest: [p1, [p2, body]] represented flat as params + body
        node.children = params;
        node.children.push(body);
        node
    }

    fn parse_let(&mut self, top: bool) -> RExpr {
        let lt = self.bump(); // let
        let rec = self.eat(&Tok::Rec);
        let id = self.bump();
        let name = match id.tok {
            Tok::Ident(s) => s,
            _ => {
                self.err(id.span, "expected binding name");
                "_".into()
            }
        };
        // curried params
        let mut params = Vec::new();
        while matches!(self.peek(), Tok::Ident(_)) {
            params.push(self.parse_param());
        }
        let ann = if self.at(&Tok::DoubleColon) {
            self.bump();
            Some(self.parse_type())
        } else {
            None
        };
        self.expect(Tok::Eq, "'='");
        let mut value = self.parse_expr(0);
        for param in params.into_iter().rev() {
            let fnspan = Span::new(param.span.start, value.span.end);
            let mut f = RExpr::mk(RKind::Fn, fnspan);
            f.children.push(param);
            f.children.push(value);
            value = f;
        }
        let mut binder = RExpr::mk(RKind::Binder, id.span);
        binder.label = Some(name);
        binder.rec = rec;
        binder.ann = ann;
        let end = if top {
            if self.at(&Tok::In) {
                self.bump();
            }
            value.span.end
        } else {
            self.expect(Tok::In, "'in'");
            let body = self.parse_expr(0);
            let be = body.span.end;
            let mut n = RExpr::mk(RKind::Let, Span::new(lt.span.start, be));
            n.rec = rec;
            n.children.push(binder);
            n.children.push(value);
            n.children.push(body);
            return n;
        };
        let mut n = RExpr::mk(RKind::Let, Span::new(lt.span.start, end));
        n.rec = rec;
        n.children.push(binder);
        n.children.push(value);
        n
    }

    fn parse_if(&mut self) -> RExpr {
        let it = self.bump(); // if
        let cond = self.parse_expr(0);
        self.expect(Tok::Then, "'then'");
        let then = self.parse_expr(0);
        self.expect(Tok::Else, "'else'");
        let els = self.parse_expr(0);
        let mut n = RExpr::mk(RKind::If, Span::new(it.span.start, els.span.end));
        n.children = vec![cond, then, els];
        n
    }

    fn parse_record(&mut self) -> RExpr {
        let lb = self.bump(); // {
        // extension form: { base with f = e, g = e }
        if self.at(&Tok::LParen)
            || matches!(self.peek(), Tok::Ident(_) | Tok::Int(_) | Tok::Str(_))
                && false
        {
            // placeholder, real decision below
        }
        // detect extension: if first is an Ident that resolves as expression
        // followed by `with`, parse it.
        let saved = self.pos;
        let is_extension = matches!(self.peek(), Tok::Ident(_))
            && {
                // lookahead two tokens: Ident 'with'
                self.toks.get(self.pos + 1).map(|t| matches!(t.tok, Tok::With)).unwrap_or(false)
            };
        if is_extension {
            let base_tok = self.bump();
            let mut base = RExpr::mk(RKind::Var, base_tok.span);
            base.label = if let Tok::Ident(s) = base_tok.tok { Some(s) } else { None };
            self.expect(Tok::With, "'with'");
            return self.finish_record(lb.span, base, true);
        }
        let _ = saved;

        let mut fields: Vec<RExpr> = Vec::new();
        loop {
            if self.at(&Tok::RBrace) {
                break;
            }
            let lab_tok = self.bump();
            let label = match lab_tok.tok {
                Tok::Ident(s) => s,
                _ => {
                    self.err(lab_tok.span, "expected field label");
                    String::new()
                }
            };
            self.expect(Tok::Eq, "'='");
            let val = self.parse_expr(0);
            let mut f = RExpr::mk(RKind::RecordField, lab_tok.span.merge(val.span));
            f.label = Some(label);
            f.children.push(val);
            fields.push(f);
            if !self.eat(&Tok::Comma) {
                break;
            }
        }
        self.finish_record_fields(lb.span, fields)
    }

    fn finish_record_fields(&mut self, start: usize, fields: Vec<RExpr>) -> RExpr {
        let end = self
            .expect(Tok::RBrace, "'}'")
            .map(|t| t.span.end)
            .unwrap_or(start);
        let mut r = RExpr::mk(RKind::Record, Span::new(start, end));
        r.children = fields;
        r
    }

    fn finish_record(&mut self, start: Span, base: RExpr, _ext: bool) -> RExpr {
        let mut updates = Vec::new();
        loop {
            if self.at(&Tok::RBrace) {
                break;
            }
            let lab = self.bump();
            let label = if let Tok::Ident(s) = lab.tok {
                s
            } else {
                self.err(lab.span, "expected field label");
                String::new()
            };
            self.expect(Tok::Eq, "'='");
            let val = self.parse_expr(0);
            let mut w = RExpr::mk(RKind::With, lab.span.merge(val.span));
            w.label = Some(label);
            w.children.push(val);
            updates.push(w);
            if !self.eat(&Tok::Comma) {
                break;
            }
        }
        let end = self
            .expect(Tok::RBrace, "'}'")
            .map(|t| t.span.end)
            .unwrap_or(start.end);
        let mut node = base;
        for u in updates {
            let span = Span::new(start.start, u.span.end);
            let mut wrap = RExpr::mk(RKind::With, span);
            wrap.label = u.label.clone();
            wrap.children.push(node);
            wrap.children.push(u.children.into_iter().next().unwrap());
            node = wrap;
        }
        node.span = Span::new(start.start, end);
        node
    }
