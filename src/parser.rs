// Lexer and Pratt/recursive-descent parser. Produces rich parse errors with spans.

use crate::syntax::*;

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Str(String),
    Ident(String),
    Upper(String),
    // keywords
    Let, Rec, In, Fun, True, False, If, Then, Else, Match, With,
    // punctuation
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Eq, Arrow, FatArrow, Dot, Comma, Colon, Pipe, Hash,
    // operators
    Plus, Minus, Star, Slash, EqEq, Lt, Gt, Le, Ge, Semi, Backslash,
    Eof,
}

#[derive(Clone, Debug)]
struct Token {
    tok: Tok,
    span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    loop {
        while i < b.len() && (b[i] as char).is_whitespace() {
            i += 1;
        }
        if i >= b.len() {
            break;
        }
        let start = i;
        let c = b[i] as char;
        if c.is_ascii_digit() {
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[start..i].parse().map_err(|_| ParseError {
                msg: "integer literal out of range".into(),
                span: Span::new(start, i),
            })?;
            out.push(Token { tok: Tok::Int(n), span: Span::new(start, i) });
            continue;
        }
        if c == '"' {
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    s.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        other => other,
                    });
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(ParseError {
                    msg: "unterminated string literal".into(),
                    span: Span::new(start, i),
                });
            }
            i += 1;
            out.push(Token { tok: Tok::Str(s), span: Span::new(start, i) });
            continue;
        }
        if c.is_alphabetic() || c == '_' {
            while i < b.len() {
                let ch = b[i] as char;
                if ch.is_alphanumeric() || ch == '_' || ch == '\'' {
                    i += 1;
                } else {
                    break;
                }
            }
            let word = &src[start..i];
            let tok = match word {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fun" => Tok::Fun,
                "true" => Tok::True,
                "false" => Tok::False,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "match" => Tok::Match,
                "with" => Tok::With,
                _ => {
                    if word.chars().next().unwrap().is_ascii_uppercase() {
                        Tok::Upper(word.to_string())
                    } else {
                        Tok::Ident(word.to_string())
                    }
                }
            };
            out.push(Token { tok, span: Span::new(start, i) });
            continue;
        }
        let two: Option<Tok> = if i + 1 < b.len() {
            match &src[i..i + 2] {
                "->" => Some(Tok::Arrow),
                "=>" => Some(Tok::FatArrow),
                "==" => Some(Tok::EqEq),
                "<=" => Some(Tok::Le),
                ">=" => Some(Tok::Ge),
                _ => None,
            }
        } else {
            None
        };
        if let Some(t) = two {
            out.push(Token { tok: t, span: Span::new(i, i + 2) });
            i += 2;
            continue;
        }
        let one = match c {
            '(' => Tok::LParen,
            ')' => Tok::RParen,
            '{' => Tok::LBrace,
            '}' => Tok::RBrace,
            '[' => Tok::LBracket,
            ']' => Tok::RBracket,
            '=' => Tok::Eq,
            '.' => Tok::Dot,
            ',' => Tok::Comma,
            ':' => Tok::Colon,
            '|' => Tok::Pipe,
            '#' => Tok::Hash,
            '+' => Tok::Plus,
            '-' => Tok::Minus,
            '*' => Tok::Star,
            '/' => Tok::Slash,
            '<' => Tok::Lt,
            '>' => Tok::Gt,
            ';' => Tok::Semi,
            '\\' => Tok::Backslash,
            _ => {
                return Err(ParseError {
                    msg: format!("unexpected character {:?}", c),
                    span: Span::new(i, i + 1),
                })
            }
        };
        out.push(Token { tok: one, span: Span::new(i, i + 1) });
        i += 1;
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}

pub struct Parser<'a> {
    toks: Vec<Token>,
    pos: usize,
    _src: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Result<Parser<'a>, ParseError> {
        Ok(Parser { toks: lex(src)?, pos: 0, _src: src })
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn span(&self) -> Span {
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
        if self.peek() == t {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.peek() == &t {
            Ok(self.bump())
        } else {
            Err(ParseError {
                msg: format!("expected {}, found {}", what, tok_name(self.peek())),
                span: self.span(),
            })
        }
    }

    pub fn parse_program(&mut self) -> Result<Expr, ParseError> {
        let e = self.parse_expr()?;
        if !matches!(self.peek(), Tok::Eof) {
            let s = self.span();
            return Err(ParseError {
                msg: format!("unexpected token {}", tok_name(self.peek())),
                span: s,
            });
        }
        Ok(e)
    }

    fn parse_pat(&mut self) -> Result<Pat, ParseError> {
        let t = self.bump();
        match t.tok {
            Tok::Ident(n) => Ok(Pat::Var(n, t.span)),
            Tok::Hash => {
                let w = self.expect(Tok::Star, "`*` in wildcard pattern `#*`")?;
                Ok(Pat::Wild(Span::merge(t.span, w.span)))
            }
            _ => Err(ParseError {
                msg: format!("expected pattern, found {}", tok_name(&t.tok)),
                span: t.span,
            }),
        }
    }

    // Expr precedence (tight to loose):
    // atom, field/app projection; unary -; * /; + -; comparison;
    // annotated `e : t`; fun/let/if/match.
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_ann()
    }

    fn parse_ann(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_binop()?;
        if self.eat(&Tok::Colon) {
            let start = e.span().start;
            let ty = self.parse_ty()?;
            let span = Span::new(start, ty.span().end);
            e = Expr::Ann { expr: Box::new(e), ty, span };
        }
        Ok(e)
    }

    fn parse_binop(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_add()?;
        loop {
            let op = match self.peek() {
                Tok::EqEq => "==",
                Tok::Lt => "<",
                Tok::Gt => ">",
                Tok::Le => "<=",
                Tok::Ge => ">=",
                _ => break,
            };
            let op_span = self.span();
            self.bump();
            let rhs = self.parse_add()?;
            let span = Span::merge(e.span(), rhs.span());
            e = primitive_app(op, e, rhs, span, op_span);
        }
        Ok(e)
    }

    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_mul()?;
        loop {
            let op = match self.peek() {
                Tok::Plus => "+",
                Tok::Minus => "-",
                _ => break,
            };
            let op_span = self.span();
            self.bump();
            let rhs = self.parse_mul()?;
            let span = Span::merge(e.span(), rhs.span());
            e = primitive_app(op, e, rhs, span, op_span);
        }
        Ok(e)
    }

    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_unary()?;
        loop {
            let op = match self.peek() {
                Tok::Star => "*",
                Tok::Slash => "/",
                _ => break,
            };
            let op_span = self.span();
            self.bump();
            let rhs = self.parse_unary()?;
            let span = Span::merge(e.span(), rhs.span());
            e = primitive_app(op, e, rhs, span, op_span);
        }
        Ok(e)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if matches!(self.peek(), Tok::Minus) {
            let t = self.bump();
            let e = self.parse_unary()?;
            let span = Span::merge(t.span, e.span());
            let zero = Expr::Lit(Lit::Int(0), t.span);
            return Ok(primitive_app("-", zero, e, span, t.span));
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_atom()?;
        loop {
            if self.eat(&Tok::Dot) {
                let nt = self.bump();
                let (name, name_span) = match nt.tok {
                    Tok::Ident(n) => (n, nt.span),
                    other => {
                        return Err(ParseError {
                            msg: format!("expected field name, found {}", tok_name(&other)),
                            span: nt.span,
                        })
                    }
                };
                let span = Span::merge(e.span(), name_span);
                e = Expr::Field {
                    record: Box::new(e),
                    name,
                    name_span,
                    span,
                };
            } else if self.starts_atom() {
                let arg = self.parse_atom()?;
                let span = Span::merge(e.span(), arg.span());
                e = Expr::App { fun: Box::new(e), arg: Box::new(arg), span };
            } else {
                break;
            }
        }
        Ok(e)
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_)
                | Tok::Str(_)
                | Tok::Ident(_)
                | Tok::Upper(_)
                | Tok::True
                | Tok::False
                | Tok::LParen
                | Tok::LBrace
                | Tok::Hash
                | Tok::Fun
                | Tok::Backslash
        )
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let t = self.bump();
        let start = t.span.start;
        match t.tok {
            Tok::Int(n) => Ok(Expr::Lit(Lit::Int(n), t.span)),
            Tok::Str(s) => Ok(Expr::Lit(Lit::Str(s), t.span)),
            Tok::True => Ok(Expr::Lit(Lit::Bool(true), t.span)),
            Tok::False => Ok(Expr::Lit(Lit::Bool(false), t.span)),
            Tok::Ident(n) => Ok(Expr::Var(n, t.span)),
            Tok::Upper(n) => {
                // Constructor applied to parenthesized/wrapped payload, or nullary.
                if self.starts_atom() {
                    let payload = self.parse_atom()?;
                    let span = Span::merge(t.span, payload.span());
                    Ok(Expr::Variant {
                        label: n,
                        payload: Some(Box::new(payload)),
                        span,
                    })
                } else {
                    Ok(Expr::Variant { label: n, payload: None, span: t.span })
                }
            }
            Tok::LParen => {
                let e = self.parse_expr()?;
                self.expect(Tok::RParen, "`)`")?;
                Ok(e)
            }
            Tok::Hash => self.parse_variant_hash(start),
            Tok::LBrace => self.parse_record_rest(start),
            Tok::Fun | Tok::Backslash => self.parse_fun_rest(t.span),
            Tok::Let => self.parse_let_rest(t.span, false),
            Tok::Rec => Err(ParseError {
                msg: "`rec` is only valid as `let rec`".into(),
                span: t.span,
            }),
            Tok::If => self.parse_if_rest(t.span),
            Tok::Match => self.parse_match_rest(t.span),
            other => Err(ParseError {
                msg: format!("unexpected token {}", tok_name(&other)),
                span: t.span,
            }),
        }
    }

    fn parse_variant_hash(&mut self, start: usize) -> Result<Expr, ParseError> {
        let nt = self.bump();
            let _ = matches!(nt.tok, Tok::Upper(_));
            if !matches!(nt.tok, Tok::Upper(_)) {
                return Err(ParseError {
                    msg: format!("expected uppercase constructor after `#`, found {}", tok_name(&nt.tok)),
                    span: nt.span,
                });
            }
        if self.starts_atom() {
            let payload = self.parse_atom()?;
            let span = Span::new(start, payload.span().end);
            Ok(Expr::Variant {
                label: match nt.tok { Tok::Upper(s) => s, _ => unreachable!() },
                payload: Some(Box::new(payload)),
                span,
            })
        } else {
            Ok(Expr::Variant {
                label: match nt.tok { Tok::Upper(s) => s, _ => unreachable!() },
                payload: None,
                span: Span::new(start, nt.span.end),
            })
        }
    }

    fn parse_fun_rest(&mut self, start_span: Span) -> Result<Expr, ParseError> {
        let param = self.parse_pat()?;
        self.expect(Tok::Arrow, "`->`")?;
        let body = self.parse_expr()?;
        let span = Span::merge(start_span, body.span());
        Ok(Expr::Lam { param, body: Box::new(body), span })
    }

    fn parse_let_rest(&mut self, start_span: Span, mut rec: bool) -> Result<Expr, ParseError> {
        if !rec {
            rec = self.eat(&Tok::Rec);
        } else if !self.eat(&Tok::Rec) {
            return Err(ParseError {
                msg: "expected `rec` after stray `rec` token".into(),
                span: start_span,
            });
        }
        let pat = self.parse_pat()?;
        self.expect(Tok::Eq, "`=`")?;
        let rhs = self.parse_expr()?;
        let in_tok = self.expect(Tok::In, "`in`")?;
        let _ = in_tok;
        let body = self.parse_expr()?;
        let span = Span::merge(start_span, body.span());
        Ok(Expr::Let { rec, pat, rhs: Box::new(rhs), body: Box::new(body), span })
    }

    fn parse_if_rest(&mut self, start_span: Span) -> Result<Expr, ParseError> {
        let cond = self.parse_expr()?;
        self.expect(Tok::Then, "`then`")?;
        let then_e = self.parse_expr()?;
        self.expect(Tok::Else, "`else`")?;
        let else_e = self.parse_expr()?;
        let span = Span::merge(start_span, else_e.span());
        Ok(Expr::If {
            cond: Box::new(cond),
            then_e: Box::new(then_e),
            else_e: Box::new(else_e),
            span,
        })
    }

    fn parse_match_rest(&mut self, start_span: Span) -> Result<Expr, ParseError> {
        let scrut = self.parse_expr()?;
        self.expect(Tok::With, "`with`")?;
        let mut cases = Vec::new();
        loop {
            self.eat(&Tok::Pipe);
            let hash = self.eat(&Tok::Hash);
            let lt = self.bump();
            let label = match lt.tok {
                Tok::Upper(s) => s,
                other => {
                    return Err(ParseError {
                        msg: format!("expected constructor in match case, found {}", tok_name(&other)),
                        span: lt.span,
                    })
                }
            };
            let case_start = if hash { lt.span.start - 1 } else { lt.span.start };
            let pat = if self.starts_pat() {
                self.parse_pat()?
            } else {
                Pat::Wild(Span::new(case_start, case_start))
            };
            self.expect(Tok::FatArrow, "`=>`")?;
            let body = self.parse_expr()?;
            let case_span = Span::new(case_start, body.span().end);
            cases.push((label, pat, body, case_span));
            if self.eat(&Tok::Pipe) {
                continue;
            }
            break;
        }
        let span = Span::merge(start_span, cases.last().unwrap().3);
        Ok(Expr::Match { scrut: Box::new(scrut), cases, span })
    }

    fn starts_pat(&self) -> bool {
        matches!(self.peek(), Tok::Ident(_) | Tok::Hash)
    }

    fn parse_record_rest(&mut self, start: usize) -> Result<Expr, ParseError> {
        // Forms:
        //   { a = e, b = e }            closed record literal
        //   { base with a = e, ... }?   row extension; base may be a path atom.
        //   { .. base with? fields }    also extension; detect `..`.
        let mut extends = false;
        if self.eat(&Tok::LBracket) {
            // `{[` weird; instead we use `..` for extension.
            return Err(ParseError {
                msg: "unexpected `[` inside record".into(),
                span: Span::new(start, start + 1),
            });
        }
        // Distinguish by trying: first token could be `..`?
        // We don't have `..` token; parse first field-or-base heuristic.
        // Simpler: support `{ e | f = .. }` with `|` as separator? We choose
        // explicit syntax: `{ .. e with f = .. }` would need dots; reuse
        // `{ base with ... }` where base is a lower ident / parenthesized.
        if matches!(self.peek(), Tok::Ident(_) | Tok::LParen) {
            // Could be `base with ...` or a field name.
            // Save position and try lookahead through base atom only for `with`.
            let save = self.pos;
            if self.try_parse_extend_head()? {
                extends = true;
            } else {
                self.pos = save;
            }
        }
        let _ = extends;
        if extends {
            let base = self.parse_extend_base()?;
            self.expect(Tok::With, "`with` in record extension")?;
            let fields = self.parse_field_binds()?;
            self.expect(Tok::RBrace, "`}`")?;
            let span = Span::new(start, self.toks[self.pos - 1].span.end);
            Ok(Expr::Extend { base: Box::new(base), fields, span })
        } else {
            let fields = self.parse_field_binds()?;
            self.expect(Tok::RBrace, "`}`")?;
            let span = Span::new(start, self.toks[self.pos - 1].span.end);
            Ok(Expr::Record { fields, span })
        }
    }

    fn try_parse_extend_head(&mut self) -> Result<bool, ParseError> {
        // Consume a base atom (variable or parenthesized expr), then look for `with`.
        let save = self.pos;
        match self.peek().clone() {
            Tok::Ident(_) => {
                self.bump();
            }
            Tok::LParen => {
                self.bump();
                let mut depth = 1;
                while depth > 0 {
                    match self.peek() {
                        Tok::Eof => {
                            self.pos = save;
                            return Ok(false);
                        }
                        Tok::LParen => {
                            depth += 1;
                            self.bump();
                        }
                        Tok::RParen => {
                            depth -= 1;
                            self.bump();
                        }
                        _ => {
                            self.bump();
                        }
                    }
                }
            }
            _ => {
                self.pos = save;
                return Ok(false);
            }
        }
        if matches!(self.peek(), Tok::With) {
            self.pos = save;
            Ok(true)
        } else {
            self.pos = save;
            Ok(false)
        }
    }

    fn parse_extend_base(&mut self) -> Result<Expr, ParseError> {
        let mut e = match self.bump().tok {
            Tok::Ident(n) => Expr::Var(n, self.toks[self.pos.saturating_sub(1)].span),
            Tok::LParen => {
                let inner = self.parse_expr()?;
                self.expect(Tok::RParen, "`)`")?;
                inner
            }
            other => unreachable!("{:?}", other),
        };
        while self.eat(&Tok::Dot) {
            let nt = self.bump();
            if let Tok::Ident(n) = nt.tok {
                let span = Span::merge(e.span(), nt.span);
                e = Expr::Field {
                    record: Box::new(e),
                    name: n,
                    name_span: nt.span,
                    span,
                };
            }
        }
        Ok(e)
    }

    fn parse_field_binds(&mut self) -> Result<Vec<(String, Expr, Span)>, ParseError> {
        let mut fields = Vec::new();
        if matches!(self.peek(), Tok::RBrace) {
            return Ok(fields);
        }
        loop {
            let nt = self.bump();
            let (name, name_span) = match nt.tok {
                Tok::Ident(n) => (n, nt.span),
                other => {
                    return Err(ParseError {
                        msg: format!("expected field name, found {}", tok_name(&other)),
                        span: nt.span,
                    })
                }
            };
            self.expect(Tok::Eq, "`=` after field name")?;
            let val = self.parse_expr()?;
            let fspan = Span::merge(name_span, val.span());
            fields.push((name, val, fspan));
            if self.eat(&Tok::Comma) {
                if matches!(self.peek(), Tok::RBrace) {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(fields)
    }

    // Type syntax (only used by annotations):
    //   int | bool | string | a | t -> t | {f: t, ...} | [A: t, ...]
    fn parse_ty(&mut self) -> Result<TyAst, ParseError> {
        let l = self.parse_ty_atom()?;
        if self.eat(&Tok::Arrow) {
            let r = self.parse_ty()?;
            let sp = Span::merge(l.span(), r.span());
            return Ok(TyAst::Arrow(Box::new(l), Box::new(r), sp));
        }
        Ok(l)
    }

    fn parse_ty_atom(&mut self) -> Result<TyAst, ParseError> {
        let t = self.bump();
        match t.tok {
            Tok::Ident(n) => Ok(TyAst::Var(n, t.span)),
            Tok::Upper(n) => Ok(TyAst::Con(n, t.span)),
            Tok::LParen => {
                let ty = self.parse_ty()?;
                self.expect(Tok::RParen, "`)`")?;
                Ok(ty)
            }
            Tok::LBrace => {
                let mut fields = Vec::new();
                let mut open = false;
                if !matches!(self.peek(), Tok::RBrace) {
                    loop {
                        let nt = self.bump();
                        let name = match nt.tok {
                            Tok::Ident(s) => s,
                            other => {
                                return Err(ParseError {
                                    msg: format!("expected field name in type, found {}", tok_name(&other)),
                                    span: nt.span,
                                })
                            }
                        };
                        self.expect(Tok::Colon, "`:` in record type")?;
                        let ty = self.parse_ty_atom()?;
                        fields.push((name, ty));
                        if self.eat(&Tok::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                if self.eat(&Tok::Comma) {
                    open = true;
                }
                self.expect(Tok::RBrace, "`}`")?;
                Ok(TyAst::Record(fields, open, t.span))
            }
            Tok::LBracket => {
                let mut fields = Vec::new();
                let mut open = false;
                if !matches!(self.peek(), Tok::RBracket) {
                    loop {
                        self.eat(&Tok::Pipe);
                        let nt = self.bump();
                        let name = match nt.tok {
                            Tok::Upper(s) => s,
                            other => {
                                return Err(ParseError {
                                    msg: format!("expected constructor in variant type, found {}", tok_name(&other)),
                                    span: nt.span,
                                })
                            }
                        };
                        let ty = if self.eat(&Tok::Colon) {
                            self.parse_ty_atom()?
                        } else {
                            TyAst::Con("Unit".into(), nt.span)
                        };
                        fields.push((name, ty));
                        if self.eat(&Tok::Comma) {
                            continue;
                        }
                        break;
                    }
                }
                if self.eat(&Tok::Comma) {
                    open = true;
                }
                self.expect(Tok::RBracket, "`]`")?;
                Ok(TyAst::Variant(fields, open, t.span))
            }
            other => Err(ParseError {
                msg: format!("expected type, found {}", tok_name(&other)),
                span: t.span,
            }),
        }
    }
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Int(_) => "integer".into(),
        Tok::Str(_) => "string".into(),
        Tok::Ident(s) => format!("identifier `{}`", s),
        Tok::Upper(s) => format!("constructor `{}`", s),
        Tok::Let => "`let`".into(),
        Tok::Rec => "`rec`".into(),
        Tok::In => "`in`".into(),
        Tok::Fun => "`fun`".into(),
        Tok::True => "`true`".into(),
        Tok::False => "`false`".into(),
        Tok::If => "`if`".into(),
        Tok::Then => "`then`".into(),
        Tok::Else => "`else`".into(),
        Tok::Match => "`match`".into(),
        Tok::With => "`with`".into(),
        Tok::LParen => "`(`".into(),
        Tok::RParen => "`)`".into(),
        Tok::LBrace => "`{`".into(),
        Tok::RBrace => "`}`".into(),
        Tok::LBracket => "`[`".into(),
        Tok::RBracket => "`]`".into(),
        Tok::Eq => "`=`".into(),
        Tok::Arrow => "`->`".into(),
        Tok::FatArrow => "`=>`".into(),
        Tok::Dot => "`.`".into(),
        Tok::Comma => "`,`".into(),
        Tok::Colon => "`:`".into(),
        Tok::Pipe => "`|`".into(),
        Tok::Hash => "`#`".into(),
        Tok::Plus => "`+`".into(),
        Tok::Minus => "`-`".into(),
        Tok::Star => "`*`".into(),
        Tok::Slash => "`/`".into(),
        Tok::EqEq => "`==`".into(),
        Tok::Lt => "`<`".into(),
        Tok::Gt => "`>`".into(),
        Tok::Le => "`<=`".into(),
        Tok::Ge => "`>=`".into(),
        Tok::Semi => "`;`".into(),
        Tok::Backslash => "`\\`".into(),
        Tok::Eof => "end of input".into(),
    }
}

// Build applications of a built-in binary operator used to desugar operators.
fn primitive_app(op: &str, lhs: Expr, rhs: Expr, span: Span, _op_span: Span) -> Expr {
    let v = Expr::Var(format!("__builtin_{}", op_name(op)), Span::new(span.start, span.start));
    let a1 = Expr::App { fun: Box::new(v), arg: Box::new(lhs), span };
    Expr::App { fun: Box::new(a1), arg: Box::new(rhs), span }
}

fn op_name(op: &str) -> &'static str {
    match op {
        "+" => "add",
        "-" => "sub",
        "*" => "mul",
        "/" => "div",
        "==" => "eq",
        "<" => "lt",
        ">" => "gt",
        "<=" => "le",
        ">=" => "ge",
        _ => "op",
    }
}

pub fn parse(src: &str) -> Result<Expr, ParseError> {
    Parser::new(src)?.parse_program()
}
