use crate::ast::*;
use crate::lexer::{lex, Tok, Token};
use crate::span::{Label, Span};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

struct P {
    toks: Vec<Token>,
    pos: usize,
}

impl P {
    fn peek(&self) -> &Tok { &self.toks[self.pos].tok }
    fn span(&self) -> Span { self.toks[self.pos].span }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos < self.toks.len() - 1 { self.pos += 1; }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(t) {
            self.bump();
            true
        } else { false }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            Ok(self.bump())
        } else {
            Err(ParseError { msg: format!("期望 {}，但遇到 {}", what, tok_name(self.peek())), span: self.span() })
        }
    }
    fn lower_ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().clone() {
            Tok::Lower(n) => { let t = self.bump(); Ok((n, t.span)) }
            other => Err(ParseError { msg: format!("期望小写标识符，但遇到 {}", tok_name(&other)), span: self.span() }),
        }
    }
    fn label(&mut self) -> Result<(Label, Span), ParseError> {
        match self.peek().clone() {
            Tok::Lower(n) => { let t = self.bump(); Ok((Label::new(n), t.span)) }
            Tok::Label(n) => { let t = self.bump(); Ok((Label::new(n), t.span)) }
            other => Err(ParseError { msg: format!("期望字段名，但遇到 {}", tok_name(&other)), span: self.span() }),
        }
    }
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Int(n) => format!("整数 {}", n),
        Tok::Lower(n) => format!("标识符 {}", n),
        Tok::Upper(n) => format!("构造子 {}", n),
        Tok::Label(n) => format!("字段名 {}", n),
        Tok::Eof => "文件末尾".into(),
        other => format!("{:?}", other),
    }
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    let toks = lex(src).map_err(|e| ParseError { msg: e.msg, span: e.span })?;
    let mut p = P { toks, pos: 0 };
    let expr = p.parse_expr()?;
    p.expect(Tok::Eof, "文件末尾")?;
    Ok(Program { expr })
}

impl P {
    fn mk(&self, data: ExprData, span: Span) -> Expr {
        let mut e = Expr { id: 0, span, data };
        e.id = compute_id(&e);
        e
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Tok::Let => self.parse_let(false),
            Tok::If => self.parse_if(),
            Tok::Fn | Tok::Backslash => self.parse_lam(),
            Tok::Match => self.parse_match(),
            _ => self.parse_bin_compare(),
        }
    }

    fn parse_let(&mut self, rec: bool) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let (name, name_span) = self.lower_ident()?;
        let ann = if self.eat(&Tok::Colon) { Some(self.parse_type()?) } else { None };
        self.expect(Tok::Eq, "'='")?;
        let value = self.parse_expr()?;
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr()?;
        let end = body.span.end;
        Ok(self.mk(ExprData::Let(Box::new(LetBind {
            rec, name, name_span, ann, value, body,
            span: Span::new(start, end),
        })), Span::new(start, end)))
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let cond = self.parse_expr()?;
        self.expect(Tok::Then, "'then'")?;
        let then_ = self.parse_expr()?;
        self.expect(Tok::Else, "'else'")?;
        let else_ = self.parse_expr()?;
        let span = Span::new(start, else_.span.end);
        Ok(self.mk(ExprData::If { cond, then_, else_ }, span))
    }

    fn parse_lam(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let (param, param_span) = match self.peek().clone() {
            Tok::Lower(n) => { let t = self.bump(); (n, t.span) }
            Tok::Under => { let t = self.bump(); ("_".to_string(), t.span) }
            other => return Err(ParseError { msg: format!("函数需要参数名，但遇到 {}", tok_name(&other)), span: self.span() }),
        };
        self.expect(Tok::Arrow, "'->'")?;
        let body = self.parse_expr()?;
        let span = Span::new(start, body.span.end);
        Ok(self.mk(ExprData::Lam { param, param_span, body }, span))
    }

    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let scrut = self.parse_expr()?;
        self.expect(Tok::With, "'with'")?;
        let mut arms = Vec::new();
        loop {
            self.eat(&Tok::Bar);
            let pat = self.parse_pat()?;
            self.expect(Tok::Arrow, "'->'")?;
            let body = self.parse_arm_body()?;
            let span = Span::union(pat.span(), body.span);
            arms.push(Arm { pat, body, span });
            if !self.eat(&Tok::Semicolon) { break; }
            if matches!(self.peek(), Tok::Eof) { break; }
        }
        let end = arms.last().map(|a| a.span.end).unwrap_or(start);
        Ok(self.mk(ExprData::Match { scrut, arms }, Span::new(start, end)))
    }

    // arm bodies include let/if/lambda but a nested match needs parens
    fn parse_arm_body(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Tok::Let => self.parse_let(false),
            Tok::If => self.parse_if(),
            Tok::Fn | Tok::Backslash => self.parse_lam(),
            _ => self.parse_bin_compare(),
        }
    }

    fn parse_pat(&mut self) -> Result<Pat, ParseError> {
        match self.peek().clone() {
            Tok::Under => { let t = self.bump(); Ok(Pat::Wild(t.span)) }
            Tok::Lower(n) => { let t = self.bump(); Ok(Pat::Var(n, t.span)) }
            Tok::Upper(n) => {
                let t = self.bump();
                let inner = if matches!(self.peek(), Tok::Lower(_) | Tok::Under | Tok::Upper(_)) {
                    Some(Box::new(self.parse_pat()?))
                } else { None };
                Ok(Pat::Tag(n, inner, t.span))
            }
            other => Err(ParseError { msg: format!("期望模式，但遇到 {}", tok_name(&other)), span: self.span() }),
        }
    }
}

impl P {
    // comparison: =  <  (non-associative)
    fn parse_bin_compare(&mut self) -> Result<Expr, ParseError> {
        let lhs = self.parse_bin_add()?;
        if matches!(self.peek(), Tok::EqEq | Tok::Lt) {
            let op = if matches!(self.peek(), Tok::EqEq) { Bin::Eq } else { Bin::Lt };
            let op_tok = self.bump();
            let rhs = self.parse_bin_add()?;
            let span = Span::union(lhs.span, rhs.span);
            return Ok(self.mk(ExprData::Bin { op, lhs, rhs }, span).tap(op_tok));
        }
        Ok(lhs)
    }

    fn parse_bin_add(&mut self) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_app()?;
        while matches!(self.peek(), Tok::Plus | Tok::Minus) {
            let op = if matches!(self.peek(), Tok::Plus) { Bin::Add } else { Bin::Sub };
            let op_tok = self.bump();
            let rhs = self.parse_app()?;
            let span = Span::union(lhs.span, rhs.span);
            lhs = self.mk(ExprData::Bin { op, lhs, rhs }, span).tap(op_tok);
        }
        Ok(lhs)
    }

    // application and projection/postfix forms are atomic
    fn parse_app(&mut self) -> Result<Expr, ParseError> {
        let mut func = self.parse_atom()?;
        loop {
            match self.peek() {
                // projection only when '.' is followed by a label
                Tok::Dot => {
                    let dot = self.toks[self.pos].span;
                    if let Tok::Lower(_) | Tok::Label(_) = self.toks[self.pos + 1].tok {
                        self.bump();
                        let (label, label_span) = self.label()?;
                        let span = Span::new(func.span.start, label_span.end);
                        func = self.mk(ExprData::Proj { record: func, label, label_span }, span);
                        let _ = dot;
                        continue;
                    }
                    break;
                }
                Tok::Int(_) | Tok::True | Tok::False | Tok::Lower(_) | Tok::Upper(_)
                | Tok::LParen | Tok::LBrace => {
                    let arg = self.parse_atom()?;
                    let span = Span::union(func.span, arg.span);
                    func = self.mk(ExprData::App { fun: func, arg }, span);
                }
                _ => break,
            }
        }
        Ok(func)
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let tok = self.peek().clone();
        match tok {
            Tok::Int(n) => {
                let t = self.bump();
                Ok(self.mk(ExprData::Lit(n, Lit::Int), t.span))
            }
            Tok::True => {
                let t = self.bump();
                Ok(self.mk(ExprData::Lit(1, Lit::Bool), t.span))
            }
            Tok::False => {
                let t = self.bump();
                Ok(self.mk(ExprData::Lit(0, Lit::Bool), t.span))
            }
            Tok::Lower(n) => {
                let t = self.bump();
                Ok(self.mk(ExprData::Var(n), t.span))
            }
            Tok::Upper(name) => {
                let t = self.bump();
                // variant constructor applied to one argument
                let payload = if matches!(self.peek(),
                    Tok::Int(_) | Tok::True | Tok::False | Tok::Lower(_) | Tok::Upper(_)
                    | Tok::LParen | Tok::LBrace)
                {
                    Some(self.parse_atom()?)
                } else { None };
                let span = match &payload {
                    Some(p) => Span::new(t.span.start, p.span.end),
                    None => t.span,
                };
                Ok(self.mk(ExprData::Variant { tag: Label::new(name), tag_span: t.span, payload }, span))
            }
            Tok::LParen => {
                self.bump();
                // record in parens (closed or open)
                if matches!(self.peek(), Tok::LBrace) {
                    let mut e = self.parse_record_expr()?;
                    self.expect(Tok::RParen, "')'")?;
                    e.span = Span::new(e.span.start, self.toks[self.pos - 1].span.end);
                    return Ok(e);
                }
                let e = self.parse_expr()?;
                self.expect(Tok::RParen, "')'")?;
                Ok(e)
            }
            Tok::LBrace => self.parse_record_expr(),
            other => Err(ParseError {
                msg: format!("期望表达式，但遇到 {}", tok_name(&other)),
                span: self.span(),
            }),
        }
    }

    fn parse_record_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect(Tok::LBrace, "'{'")?.span.start;
        let mut fields = Vec::new();
        let mut rest: Option<Expr> = None;
        loop {
            if matches!(self.peek(), Tok::RBrace) { break; }
            if matches!(self.peek(), Tok::Bar) {
                self.bump();
                rest = Some(self.parse_expr()?);
                break;
            }
            let (label, label_span) = self.label()?;
            self.expect(Tok::Eq, "'='")?;
            let value = self.parse_expr()?;
            fields.push(Field { label, label_span, value });
            if !self.eat(&Tok::Comma) { break; }
        }
        self.expect(Tok::RBrace, "'}'")?;
        let end = self.toks[self.pos - 1].span.end;
        let span = Span::new(start, end);
        match rest {
            None => Ok(self.mk(ExprData::Record { fields, row: None }, span)),
            Some(rest_expr) => {
                // fold fields into extension in source order: {a=1,b=2 | r}
                let mut e = rest_expr;
                for f in fields.into_iter().rev() {
                    let fs = Span::new(start, end);
                    e = self.mk(ExprData::Extend {
                        label: f.label, label_span: f.label_span, value: f.value, rest: e,
                    }, fs);
                }
                Ok(e)
            }
        }
    }
}

trait Tap { fn tap(self, _t: Token) -> Self; }
impl Tap for Expr { fn tap(self, _t: Token) -> Self { self } }

impl P {
    // types: arrow right-assoc; atoms: int, bool, unit, lower type vars,
    // { ... } records, < ... > variants
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let lhs = self.parse_type_atom()?;
        if self.eat(&Tok::Arrow) {
            let rhs = self.parse_type()?;
            let span = Span::new(lhs.span().start, rhs.span().end);
            Ok(Type::TFun(Box::new(lhs), Box::new(rhs), span))
        } else {
            Ok(lhs)
        }
    }

    fn parse_type_atom(&mut self) -> Result<Type, ParseError> {
        match self.peek().clone() {
            Tok::Lower(n) => {
                let t = self.bump();
                match n.as_str() {
                    "int" => Ok(Type::TCon("int".into(), t.span)),
                    "bool" => Ok(Type::TCon("bool".into(), t.span)),
                    "unit" => Ok(Type::TCon("unit".into(), t.span)),
                    other => Ok(Type::TVar(other.to_string(), t.span)),
                }
            }
            Tok::LBrace => {
                let start = self.bump().span.start;
                let mut fields = Vec::new();
                let mut row: Option<String> = None;
                loop {
                    if matches!(self.peek(), Tok::RBrace) { break; }
                    if self.eat(&Tok::Bar) {
                        match self.peek().clone() {
                            Tok::Lower(n) if n != "int" && n != "bool" && n != "unit" => {
                                self.bump();
                                row = Some(n);
                            }
                            Tok::Star => { self.bump(); row = Some("*".into()); }
                            other => return Err(ParseError {
                                msg: format!("开放记录需要行变量或 *，但遇到 {}", tok_name(&other)),
                                span: self.span(),
                            }),
                        }
                        break;
                    }
                    let (label, label_span) = self.label()?;
                    self.expect(Tok::Colon, "':'")?;
                    let ty = self.parse_type()?;
                    fields.push((label, label_span, ty));
                    if !self.eat(&Tok::Comma) { break; }
                }
                let close = self.expect(Tok::RBrace, "'}'")?;
                Ok(Type::TRecord(fields, row, Span::new(start, close.span.end)))
            }
            Tok::Variant => {
                let start = self.bump().span.start;
                self.expect(Tok::Lt, "'<'")?;
                let mut cases = Vec::new();
                let mut row: Option<String> = None;
                loop {
                    if matches!(self.peek(), Tok::Gt) { break; }
                    if self.eat(&Tok::Bar) {
                        match self.peek().clone() {
                            Tok::Lower(n) => { self.bump(); row = Some(n); }
                            Tok::Star => { self.bump(); row = Some("*".into()); }
                            _ => { row = Some("*".into()); }
                        }
                        break;
                    }
                    let (label, label_span) = self.label()?;
                    let payload = if self.eat(&Tok::Colon) { Some(self.parse_type()?) } else { None };
                    cases.push((label, label_span, payload));
                    if !self.eat(&Tok::Comma) { break; }
                }
                let close = self.expect(Tok::Gt, "'>'")?;
                Ok(Type::TVariant(cases, row, Span::new(start, close.span.end)))
            }
            Tok::LParen => {
                self.bump();
                let t = self.parse_type()?;
                self.expect(Tok::RParen, "')'")?;
                Ok(t)
            }
            other => Err(ParseError {
                msg: format!("期望类型，但遇到 {}", tok_name(&other)),
                span: self.span(),
            }),
        }
    }
}
