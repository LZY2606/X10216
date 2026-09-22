use crate::ast::*;
use crate::incremental::prepare::{prepare_program, RawProgram};
use crate::lexer::{lex, Span, Token, TokenKind};

pub fn parse(source: &str) -> Result<Program, String> {
    let tokens = lex(source)?;
    let mut parser = Parser { tokens, position: 0, source };
    let raw = parser.program()?;
    Ok(prepare_program(raw, source))
}

struct Parser<'a> { tokens: Vec<Token>, position: usize, source: &'a str }

impl<'a> Parser<'a> {
    fn span(&self) -> Span { self.tokens[self.position].span }
    fn peek(&self) -> &TokenKind { &self.tokens[self.position].kind }
    fn bump(&mut self) -> Token { let token = self.tokens[self.position].clone(); if self.position + 1 < self.tokens.len() { self.position += 1; } token }
    fn eat(&mut self, expected: &TokenKind) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(expected) { self.bump(); true } else { false }
    }
    fn expect(&mut self, expected: TokenKind, what: &str) -> Result<Token, String> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&expected) { Ok(self.bump()) }
        else { Err(format!("expected {what} at byte {}", self.span().start)) }
    }
    fn ident(&mut self) -> Result<(String, Span), String> {
        match self.peek().clone() {
            TokenKind::Ident(name) => { let token = self.bump(); Ok((name, token.span)) }
            other => Err(format!("expected identifier, found {other:?} at byte {}", self.span().start)),
        }
    }

    fn program(&mut self) -> Result<RawProgram, String> {
        let mut statements = Vec::new();
        while !matches!(self.peek(), TokenKind::Eof) {
            statements.push(self.statement()?);
            while self.eat(&TokenKind::Semicolon) {}
        }
        Ok(RawProgram { statements })
    }

    fn statement(&mut self) -> Result<Stmt, String> {
        let span = self.span();
        let kind = if matches!(self.peek(), TokenKind::Let) {
            StmtKind::Let(self.let_bind(true)?)
        } else {
            StmtKind::Expr(self.expr(0)?)
        };
        let end = self.previous_end();
        Ok(Stmt { kind, span: span.join(Span::new(end, end, self.source)), node_id: 0 })
    }

    fn previous_end(&self) -> usize { self.tokens[self.position.saturating_sub(1)].span.end }

    fn let_bind(&mut self, top_level: bool) -> Result<LetBind, String> {
        let start = self.expect(TokenKind::Let, "let")?.span;
        let rec = if self.eat(&TokenKind::Rec) { Some(self.tokens[self.position.saturating_sub(1)].span) } else { None };
        let (name, _) = self.ident()?;
        self.expect(TokenKind::Eq, "=")?;
        let value = self.expr(0)?;
        let body = if !top_level && self.eat(&TokenKind::In) { Some(self.expr(0)?) } else { None };
        let is_value = is_syntactic_value(&value);
        let end = body.as_ref().map(|e| e.span.end).unwrap_or(value.span.end);
        Ok(LetBind { name, rec, value, body, is_value, span: start.join(Span::new(end, end, self.source)) })
    }

    fn expr(&mut self, min_bp: u8) -> Result<Expr, String> {
        let mut left = self.unary()?;
        loop {
            let (op, lbp, rbp) = match self.peek() {
                TokenKind::OrOr => ("||", 1, 2),
                TokenKind::AndAnd => ("&&", 3, 4),
                TokenKind::EqEq => ("==", 5, 6),
                TokenKind::Neq => ("!=", 5, 6),
                TokenKind::Lt => ("<", 7, 8),
                TokenKind::Le => ("<=", 7, 8),
                TokenKind::Gt => (">", 7, 8),
                TokenKind::Ge => (">=", 7, 8),
                TokenKind::Plus => ("+", 9, 10),
                TokenKind::Minus => ("-", 9, 10),
                TokenKind::Star => ("*", 11, 12),
                TokenKind::Slash => ("/", 11, 12),
                _ => break,
            };
            if lbp < min_bp { break; }
            let op_span = self.span();
            self.bump();
            let right = self.expr(rbp)?;
            let span = left.span.join(right.span);
            left = raw_expr(ExprKind::Binary { op: op.to_string(), left: Box::new(left), right: Box::new(right) }, span.join(op_span));
        }
        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if matches!(self.peek(), TokenKind::Minus) {
            let span = self.bump().span;
            let inner = self.unary()?;
            let zero = raw_expr(ExprKind::Int(0), span);
            let inner_span = inner.span;
            return Ok(raw_expr(ExprKind::Binary { op: "-".into(), left: Box::new(zero), right: Box::new(inner) }, span.join(inner_span)));
        }
        self.postfix()
    }

    fn postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.atom()?;
        loop {
            if self.eat(&TokenKind::Dot) {
                let (name, span) = self.label()?;
                let span_all = expr.span.join(span);
                expr = raw_expr(ExprKind::Field { record: Box::new(expr), name }, span_all);
            } else if is_atom_start(self.peek()) {
                let arg = self.atom()?;
                let span = expr.span.join(arg.span);
                expr = raw_expr(ExprKind::App { fun: Box::new(expr), arg: Box::new(arg) }, span);
            } else { break; }
        }
        Ok(expr)
    }

    fn atom(&mut self) -> Result<Expr, String> {
        let token = self.bump();
        let span = token.span;
        let kind = match token.kind {
            TokenKind::Int(value) => ExprKind::Int(value),
            TokenKind::Str(value) => ExprKind::Str(value),
            TokenKind::True => ExprKind::Bool(true),
            TokenKind::False => ExprKind::Bool(false),
            TokenKind::Ident(name) => ExprKind::Var(name),
            TokenKind::Upper(label) => {
                if matches!(self.peek(), TokenKind::LParen) {
                    self.bump();
                    let payload = self.expr(0)?;
                    let end = self.expect(TokenKind::RParen, ")")?.span;
                    ExprKind::Variant { label, payload: Some(Box::new(payload)), closed: Some(end) }
                } else {
                    ExprKind::Variant { label, payload: None, closed: None }
                }
            }
            TokenKind::Variant => {
                let (label, label_span) = self.label()?;
                let payload = if matches!(self.peek(), TokenKind::LParen) {
                    self.bump();
                    let value = self.expr(0)?;
                    self.expect(TokenKind::RParen, ")")?;
                    Some(Box::new(value))
                } else { None };
                let closed = if self.eat(&TokenKind::LBracket) { Some(self.expect(TokenKind::RBracket, "]")?.span) } else { None };
                let _ = label_span;
                ExprKind::Variant { label, payload, closed }
            }
            TokenKind::LParen => {
                if self.eat(&TokenKind::RParen) {
                    return Ok(raw_expr(ExprKind::Record { fields: vec![], closed: true }, span.join(self.previous_span())));
                }
                let expr = self.expr(0)?;
                let end = self.expect(TokenKind::RParen, ")")?.span;
                return Ok(raw_expr(expr.kind, span.join(end)));
            }
            TokenKind::Fn | TokenKind::Fun => {
                let (param, _) = self.ident()?;
                self.expect(TokenKind::FatArrow, "=>")?;
                let body = self.expr(0)?;
                ExprKind::Lam { param, body: Box::new(body) }
            }
            TokenKind::If => {
                let cond = self.expr(0)?;
                self.expect(TokenKind::Then, "then")?;
                let then_branch = self.expr(0)?;
                self.expect(TokenKind::Else, "else")?;
                let else_branch = self.expr(0)?;
                ExprKind::If { cond: Box::new(cond), then_branch: Box::new(then_branch), else_branch: Box::new(else_branch) }
            }
            TokenKind::Match => {
                let scrutinee = self.expr(0)?;
                self.expect(TokenKind::With, "with")?;
                let mut arms = Vec::new();
                loop {
                    self.eat(&TokenKind::Pipe);
                    if matches!(self.peek(), TokenKind::Semicolon | TokenKind::Eof) { break; }
                    let pattern = self.pattern()?;
                    self.expect(TokenKind::FatArrow, "=>")?;
                    let body = self.expr(0)?;
                    let pattern_span = match &pattern {
                        Pattern::Wildcard(span) => *span,
                        Pattern::Variant { span, .. } => *span,
                    };
                    let arm_span = pattern_span.join(body.span);
                    arms.push(MatchArm { pattern, body, span: arm_span });
                    if !matches!(self.peek(), TokenKind::Pipe) { break; }
                }
                ExprKind::Match { scrutinee: Box::new(scrutinee), arms }
            }
            TokenKind::Let => {
                self.position -= 1;
                let bind = self.let_bind(false)?;
                let body = bind.body.clone().ok_or_else(|| "let expression needs in".to_string())?;
                ExprKind::Let { bind: Box::new(bind), body: Box::new(body) }
            }
            TokenKind::Record => return self.record(span),
            TokenKind::LBrace => return self.record(span),
            other => return Err(format!("unexpected token {other:?} at byte {}", span.start)),
        };
        Ok(raw_expr(kind, span))
    }

    fn previous_span(&self) -> Span { self.tokens[self.position.saturating_sub(1)].span }

    fn record(&mut self, start: Span) -> Result<Expr, String> {
        let existing = if self.source[start.start..start.end] == *"record" && !matches!(self.peek(), TokenKind::LBrace) {
            Some(self.expr(0)?)
        } else {
            None
        };
        self.expect(TokenKind::LBrace, "{")?;
        self.bump();
        let mut fields = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace) {
            let (name, _) = self.label()?;
            self.expect(TokenKind::Eq, "=")?;
            let value = self.expr(0)?;
            fields.push((name, value));
            if !self.eat(&TokenKind::Comma) { break; }
        }
        let end = self.expect(TokenKind::RBrace, "}")?.span;
        let closed = if self.eat(&TokenKind::LBracket) { self.expect(TokenKind::RBracket, "]")?; true } else { false };
        if let Some(record) = existing {
            Ok(raw_expr(ExprKind::RecordFields { record: Box::new(record), fields, closed }, start.join(end)))
        } else {
            Ok(raw_expr(ExprKind::Record { fields, closed }, start.join(end)))
        }
    }

    fn label(&mut self) -> Result<(String, Span), String> {
        match self.peek().clone() {
            TokenKind::Ident(name) => { let token = self.bump(); Ok((name, token.span)) }
            TokenKind::Upper(name) => { let token = self.bump(); Ok((name, token.span)) }
            other => Err(format!("expected field label, found {other:?} at byte {}", self.span().start)),
        }
    }

    fn pattern(&mut self) -> Result<Pattern, String> {
        let span = self.span();
        if self.eat(&TokenKind::Underscore) {
            return Ok(Pattern::Wildcard(self.previous_span()));
        }
        let (label, label_span) = self.label()?;
        let payload = if matches!(self.peek(), TokenKind::Ident(_)) && !matches!(self.peek(), TokenKind::FatArrow) {
            let (name, _) = self.ident()?;
            Some(name)
        } else { None };
        Ok(Pattern::Variant { label, payload, label_span, span: span.join(self.previous_span()) })
    }
}

fn raw_expr(kind: ExprKind, span: Span) -> Expr {
    Expr { kind, span, node_id: 0, binder_id: None, binder_key: None, ref_binder: None, scope_hash: None }
}

fn is_atom_start(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::Int(_) | TokenKind::Str(_) | TokenKind::Ident(_) | TokenKind::Upper(_) |
        TokenKind::True | TokenKind::False | TokenKind::LParen | TokenKind::LBrace | TokenKind::Fn |
        TokenKind::Fun | TokenKind::If | TokenKind::Match | TokenKind::Variant | TokenKind::Record)
}

fn is_syntactic_value(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Int(_) | ExprKind::Str(_) | ExprKind::Bool(_) | ExprKind::Lam { .. } | ExprKind::Var(_) => true,
        ExprKind::Record { fields, .. } => fields.iter().all(|(_, value)| is_syntactic_value(value)),
        ExprKind::Variant { payload, .. } => payload.as_ref().is_none_or(|value| is_syntactic_value(value)),
        ExprKind::Let { bind, body } => bind.is_value && is_syntactic_value(body),
        _ => false,
    }
}
