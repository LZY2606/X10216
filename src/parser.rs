use crate::syntax::*;

#[derive(Debug, Clone, PartialEq, Eq)]
#[derive(serde::Serialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

pub fn lex(source: &str) -> Result<Vec<Token>, ParseError> {
    let bytes = source.as_bytes();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let start = i;
        let c = bytes[i] as char;
        if c.is_whitespace() { i += 1; continue; }
        if c == '/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
            while i < bytes.len() && bytes[i] != b'\n' { i += 1; }
            continue;
        }
        if c.is_ascii_digit() {
            while i < bytes.len() && (bytes[i] as char).is_ascii_digit() { i += 1; }
            let text = source[start..i].to_string();
            tokens.push(Token { kind: TokenKind::Int(text), span: Span::new(start, i) });
            continue;
        }
        if c == '_' || c.is_ascii_alphabetic() {
            while i < bytes.len() {
                let ch = bytes[i] as char;
                if ch == '_' || ch.is_ascii_alphanumeric() { i += 1; } else { break; }
            }
            let text = &source[start..i];
            let kind = match text {
                "let" => TokenKind::Let, "rec" => TokenKind::Rec, "in" => TokenKind::In,
                "fun" => TokenKind::Fun, "if" => TokenKind::If, "then" => TokenKind::Then,
                "else" => TokenKind::Else, "match" => TokenKind::Match, "with" => TokenKind::With,
                "true" => TokenKind::True, "false" => TokenKind::False,
                "_" => TokenKind::Underscore,
                other => TokenKind::Ident(other.to_string()),
            };
            tokens.push(Token { kind, span: Span::new(start, i) });
            continue;
        }
        if c == '"' {
            i += 1;
            let mut value = String::new();
            while i < bytes.len() && bytes[i] != b'"' {
                let ch = bytes[i] as char;
                if ch == '\\' && i + 1 < bytes.len() {
                    let e = bytes[i + 1] as char;
                    value.push(match e { 'n'=>'\n', 't'=>'\t', 'r'=>'\r', '"'=>'"', '\\'=>'\\', other=>other });
                    i += 2;
                } else { value.push(ch); i += 1; }
            }
            if i >= bytes.len() {
                return Err(ParseError { message: "字符串缺少结束引号".into(), span: Span::new(start, bytes.len()) });
            }
            i += 1;
            tokens.push(Token { kind: TokenKind::String(value), span: Span::new(start, i) });
            continue;
        }
        let (kind, len) = symbol(source, i).ok_or_else(|| {
            ParseError { message: format!("无法识别字符 {c:?}"), span: Span::new(i, i + 1) }
        })?;
        i += len;
        tokens.push(Token { kind, span: Span::new(start, i) });
    }
    tokens.push(Token { kind: TokenKind::Semicolon, span: Span::new(source.len(), source.len()) });
    Ok(tokens)
}

fn symbol(source: &str, i: usize) -> Option<(TokenKind, usize)> {
    let b = source.as_bytes();
    let two = if i + 1 < b.len() { &source[i..i + 2] } else { "" };
    Some(match two {
        "->" => (TokenKind::Arrow, 2), "=>" => (TokenKind::FatArrow, 2),
        "==" => (TokenKind::EqEq, 2), "!=" => (TokenKind::NotEq, 2),
        "<=" => (TokenKind::Le, 2), ">=" => (TokenKind::Ge, 2),
        "::" => (TokenKind::ColonColon, 2),
        _ => match b[i] as char {
            '.' => (TokenKind::Dot, 1), ',' => (TokenKind::Comma, 1), ';' => (TokenKind::Semicolon, 1),
            '=' => (TokenKind::Equals, 1), '\\' => (TokenKind::Backslash, 1),
            '(' => (TokenKind::LParen, 1), ')' => (TokenKind::RParen, 1),
            '{' => (TokenKind::LBrace, 1), '}' => (TokenKind::RBrace, 1),
            '[' => (TokenKind::LBracket, 1), ']' => (TokenKind::RBracket, 1),
            '+' => (TokenKind::Plus, 1), '-' => (TokenKind::Minus, 1), '*' => (TokenKind::Star, 1),
            '/' => (TokenKind::Slash, 1), '<' => (TokenKind::Lt, 1), '>' => (TokenKind::Gt, 1),
            '|' => (TokenKind::Pipe, 1), ':' => (TokenKind::Colon, 1),
            _ => return None,
        }
    })
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

pub fn parse(source: &str) -> Result<Program, ParseError> {
    let tokens = lex(source)?;
    let mut parser = Parser { tokens, pos: 0 };
    let mut bindings = Vec::new();
    while !parser.at_end() {
        bindings.push(parser.binding()?);
        parser.consume_semicolons();
    }
    Ok(Program { bindings })
}

impl Parser {
    fn peek(&self) -> &Token { &self.tokens[self.pos] }
    fn at_end(&self) -> bool { matches!(self.peek().kind, TokenKind::Semicolon) && self.peek().span.start == self.peek().span.end }
    fn bump(&mut self) -> Token { let token = self.tokens[self.pos].clone(); self.pos += 1; token }

    fn err(&self, message: impl Into<String>) -> ParseError {
        ParseError { message: message.into(), span: self.peek().span }
    }

    fn consume_semicolons(&mut self) {
        while matches!(self.peek().kind, TokenKind::Semicolon) && !self.at_end() { self.pos += 1; }
    }

    fn expect(&mut self, wanted: &TokenKind, what: &str) -> Result<Token, ParseError> {
        if std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(wanted) {
            Ok(self.bump())
        } else {
            Err(self.err(format!("期望 {what}")))
        }
    }

    fn ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().kind.clone() {
            TokenKind::Ident(name) => Ok((name, self.bump().span)),
            _ => Err(self.err("期望标识符")),
        }
    }

    fn label(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().kind.clone() {
            TokenKind::LBracket => {
                let start = self.bump().span.start;
                let (name, name_span) = self.ident()?;
                let end = self.expect(&TokenKind::RBracket, "结束标签括号 ]")?.span.end;
                Ok((name, Span::new(start, end).join(name_span)))
            }
            TokenKind::Ident(name) => Ok((name, self.bump().span)),
            _ => Err(self.err("期望标签名")),
        }
    }

    fn binding(&mut self) -> Result<Binding, ParseError> {
        let start = self.peek().span.start;
        self.expect(&TokenKind::Let, "let")?;
        let rec = if matches!(self.peek().kind, TokenKind::Rec) { self.pos += 1; true } else { false };
        let (name, name_span) = self.ident()?;
        let mut params = Vec::new();
        while matches!(self.peek().kind, TokenKind::Ident(_) | TokenKind::Underscore)
            && !matches!(self.peek().kind, TokenKind::Equals)
        {
            if matches!(self.peek().kind, TokenKind::Underscore) {
                params.push(("_".to_string(), self.bump().span));
            } else {
                params.push(self.ident()?);
            }
        }
        self.expect(&TokenKind::Equals, "=")?;
        let mut value = self.expr()?;
        for (param, param_span) in params.into_iter().rev() {
            let span = param_span.join(value.span());
            value = Expr::Fun { param, param_span, body: Box::new(value), span };
        }
        let span = Span::new(start, value.span().end);
        Ok(Binding { rec, name, name_span, value, span })
    }

    fn expr(&mut self) -> Result<Expr, ParseError> { self.binary(0) }

    fn binary(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut left = self.unary()?;
        loop {
            let (op, lbp, rbp) = match self.peek().kind {
                TokenKind::Plus => ("__add", 5, 6),
                TokenKind::Minus => ("__sub", 5, 6),
                TokenKind::Star => ("__mul", 7, 8),
                TokenKind::Slash => ("__div", 7, 8),
                TokenKind::EqEq => ("__eq", 4, 3),
                TokenKind::NotEq => ("__ne", 4, 3),
                TokenKind::Lt => ("__lt", 4, 3),
                TokenKind::Gt => ("__gt", 4, 3),
                TokenKind::Le => ("__le", 4, 3),
                TokenKind::Ge => ("__ge", 4, 3),
                _ => break,
            };
            if lbp < min_bp { break; }
            let op_span = self.bump().span;
            let right = self.binary(rbp)?;
            let fun = Expr::Var { name: op.to_string(), span: op_span };
            let end = right.span();
            let app1 = Expr::App { fun: Box::new(fun), arg: Box::new(left), span: op_span.join(end) };
            left = Expr::App { fun: Box::new(app1), arg: Box::new(right), span: op_span.join(end) };
        }
        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if matches!(self.peek().kind, TokenKind::Minus) {
            let span = self.bump().span;
            let value = self.unary()?;
            let zero = Expr::Lit { value: Lit::Int(0), span };
            return Ok(Expr::App { fun: Box::new(Expr::Var { name: "__sub".into(), span }), arg: Box::new(Expr::App { fun: Box::new(zero), arg: Box::new(value), span }), span });
        }
        self.postfix()
    }

    fn postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.atomic()?;
        loop {
            match self.peek().kind {
                TokenKind::Dot => {
                    let dot = self.bump().span;
                    let (name, name_span) = self.label()?;
                    let span = expr.span().join(name_span);
                    expr = Expr::Field { record: Box::new(expr), name, name_span: dot.join(name_span), span };
                }
                TokenKind::LBrace if !is_block_start(&self.tokens, self.pos) => {
                    let start = self.bump().span.start;
                    let fields = self.fields()?;
                    let end = self.expect(&TokenKind::RBrace, "结束记录 }")?.span.end;
                    let span = Span::new(start, end);
                    expr = Expr::Extend { base: Box::new(expr), fields, span };
                }
                _ if self.starts_atom() => {
                    let arg = self.atomic()?;
                    let span = expr.span().join(arg.span());
                    expr = Expr::App { fun: Box::new(expr), arg: Box::new(arg), span };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn starts_atom(&self) -> bool {
        matches!(self.peek().kind,
            TokenKind::Int(_) | TokenKind::String(_) | TokenKind::True | TokenKind::False
            | TokenKind::Ident(_) | TokenKind::LParen | TokenKind::LBrace | TokenKind::LBracket
            | TokenKind::Backslash | TokenKind::Fun | TokenKind::Let | TokenKind::Match)
    }

    fn fields(&mut self) -> Result<Vec<Field>, ParseError> {
        let mut fields = Vec::new();
        if matches!(self.peek().kind, TokenKind::RBrace) { return Ok(fields); }
        loop {
            let (name, name_span) = self.label()?;
            self.expect(&TokenKind::Equals, "字段后的 =")?;
            let value = self.expr()?;
            fields.push(Field { name, name_span, value });
            if !matches!(self.peek().kind, TokenKind::Comma) { break; }
            self.pos += 1;
        }
        Ok(fields)
    }

    fn atomic(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().clone();
        let expr = match token.kind {
            TokenKind::Int(text) => {
                let span = self.bump().span;
                let value = text.parse::<i64>().map_err(|_| ParseError { message: "整数字面量越界".into(), span })?;
                Expr::Lit { value: Lit::Int(value), span }
            }
            TokenKind::String(value) => {
                let span = self.bump().span;
                Expr::Lit { value: Lit::String(value), span }
            }
            TokenKind::True => Expr::Lit { value: Lit::Bool(true), span: self.bump().span },
            TokenKind::False => Expr::Lit { value: Lit::Bool(false), span: self.bump().span },
            TokenKind::Ident(name) => {
                let span = self.bump().span;
                if matches!(self.peek().kind, TokenKind::LBracket) {
                    let start = span.start;
                    self.pos += 1;
                    let (label, label_span) = self.ident()?;
                    let end = self.expect(&TokenKind::RBracket, "结束标签 ]")?.span.end;
                    let payload = if self.starts_atom() { Some(Box::new(self.atomic()?)) } else { None };
                    Expr::Variant { label, payload, span: Span::new(start, end).join(label_span) }
                } else {
                    Expr::Var { name, span }
                }
            }
            TokenKind::LParen => {
                let start = self.bump().span.start;
                if matches!(self.peek().kind, TokenKind::RParen) {
                    let end = self.bump().span.end;
                    Expr::Lit { value: Lit::Unit, span: Span::new(start, end) }
                } else {
                    let inner = self.expr()?;
                    let end = self.expect(&TokenKind::RParen, "右括号 )")?.span.end;
                    match inner { Expr::Lit { value: Lit::Unit, .. } => inner, other => other }
                }
            }
            TokenKind::LBrace => {
                let start = self.bump().span.start;
                let fields = self.fields()?;
                let end = self.expect(&TokenKind::RBrace, "结束记录 }")?.span.end;
                Expr::Record { fields, span: Span::new(start, end) }
            }
            TokenKind::LBracket => {
                let start = self.bump().span.start;
                let (label, label_span) = self.ident()?;
                let end = self.expect(&TokenKind::RBracket, "结束标签 ]")?.span.end;
                let payload = if self.starts_atom() { Some(Box::new(self.atomic()?)) } else { None };
                Expr::Variant { label, payload, span: Span::new(start, end).join(label_span) }
            }
            TokenKind::Backslash | TokenKind::Fun => self.fun_expr()?,
            TokenKind::Let => self.let_expr()?,
            TokenKind::Match => self.match_expr()?,
            _ => return Err(self.err("期望表达式")),
        };
        Ok(expr)
    }

    fn fun_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let mut params = Vec::new();
        while matches!(self.peek().kind, TokenKind::Ident(_) | TokenKind::Underscore) {
            if matches!(self.peek().kind, TokenKind::Underscore) {
                params.push(("_".to_string(), self.bump().span));
            } else {
                params.push(self.ident()?);
            }
        }
        if params.is_empty() { return Err(self.err("函数至少需要一个参数")); }
        self.expect(&TokenKind::Arrow, "->")?;
        let mut body = self.expr()?;
        for (param, param_span) in params.into_iter().rev() {
            let span = Span::new(start, body.span().end);
            body = Expr::Fun { param, param_span, body: Box::new(body), span };
        }
        Ok(body)
    }

    fn let_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        self.expect(&TokenKind::Let, "let")?;
        let rec = if matches!(self.peek().kind, TokenKind::Rec) { self.pos += 1; true } else { false };
        let (name, name_span) = self.ident()?;
        let mut params = Vec::new();
        while matches!(self.peek().kind, TokenKind::Ident(_) | TokenKind::Underscore) && !matches!(self.peek().kind, TokenKind::Equals) {
            if matches!(self.peek().kind, TokenKind::Underscore) { params.push(("_".to_string(), self.bump().span)); }
            else { params.push(self.ident()?); }
        }
        self.expect(&TokenKind::Equals, "=")?;
        let mut value = self.expr()?;
        for (param, param_span) in params.into_iter().rev() {
            let value_span = value.span();
            value = Expr::Fun { param, param_span, body: Box::new(value), span: param_span.join(value_span) };
        }
        self.expect(&TokenKind::In, "in")?;
        let body = self.expr()?;
        let body_span = body.span();
        Ok(Expr::Let { rec, name, name_span, value: Box::new(value), body: Box::new(body), span: Span::new(start, body_span.end) })
    }

    fn pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek().kind.clone() {
            TokenKind::Underscore => Ok(Pattern::Wildcard(self.bump().span)),
            TokenKind::Ident(name) => {
                let span = self.bump().span;
                if matches!(self.peek().kind, TokenKind::LBracket) {
                    self.pos += 1;
                    let (label, _) = self.ident()?;
                    self.expect(&TokenKind::RBracket, "结束标签 ]")?;
                    let payload = if matches!(self.peek().kind, TokenKind::Ident(_) | TokenKind::Underscore) {
                        Some(Box::new(self.pattern()?))
                    } else { None };
                    Ok(Pattern::Variant { label, payload, span })
                } else { Ok(Pattern::Var { name, span }) }
            }
            TokenKind::LBracket => {
                let span = self.bump().span;
                let (label, _) = self.ident()?;
                let end = self.expect(&TokenKind::RBracket, "结束标签 ]")?.span;
                let payload = if matches!(self.peek().kind, TokenKind::Ident(_) | TokenKind::Underscore) {
                    Some(Box::new(self.pattern()?))
                } else { None };
                Ok(Pattern::Variant { label, payload, span: span.join(end) })
            }
            _ => Err(self.err("期望变量、通配符或变体模式")),
        }
    }

    fn match_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        self.expect(&TokenKind::Match, "match")?;
        let scrutinee = self.expr()?;
        self.expect(&TokenKind::With, "with")?;
        let mut arms = Vec::new();
        while matches!(self.peek().kind, TokenKind::Pipe) {
            let arm_start = self.bump().span.start;
            let pattern = self.pattern()?;
            self.expect(&TokenKind::FatArrow, "=>")?;
            let body = self.expr()?;
            let span = Span::new(arm_start, body.span().end);
            arms.push(MatchArm { pattern, body, span });
        }
        if arms.is_empty() { return Err(self.err("match 至少需要一个 | 分支")); }
        let end = arms.last().unwrap().span.end;
        Ok(Expr::Match { scrutinee: Box::new(scrutinee), arms, span: Span::new(start, end) })
    }
}

fn is_block_start(_tokens: &[Token], _pos: usize) -> bool {
    false
}
