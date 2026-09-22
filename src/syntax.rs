use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn join(self, other: Self) -> Self {
        Self { start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Item {
    Let(LetBind),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LetBind {
    pub span: Span,
    pub rec: bool,
    pub name: String,
    pub name_span: Span,
    pub params: Vec<Param>,
    pub annot: Option<TypeExpr>,
    pub value: Expr,
    pub body: Box<Item>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Param {
    pub name: String,
    pub span: Span,
    pub annot: Option<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExprKind {
    Int(i64),
    Str(String),
    Bool(bool),
    Var(String),
    Lam(Vec<Param>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    Let(Box<LetBind>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Record(Vec<FieldExpr>, Option<Box<Expr>>),
    Field(Box<Expr>, String, Span),
    Extend(Box<Expr>, Vec<FieldExpr>),
    Variant(String, Box<Expr>),
    Match(Box<Expr>, Vec<MatchArm>),
    Ascribe(Box<Expr>, TypeExpr),
    Tuple(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldExpr {
    pub name: String,
    pub name_span: Span,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub label: String,
    pub label_span: Span,
    pub binding: String,
    pub binding_span: Span,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeExpr {
    pub span: Span,
    pub kind: TypeExprKind,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeExprKind {
    Con(String),
    Var(String),
    Arrow(Box<TypeExpr>, Box<TypeExpr>),
    Record(Vec<TypeField>, Option<String>),
    Variant(Vec<TypeField>, Option<String>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TypeField {
    pub name: String,
    pub name_span: Span,
    pub value: TypeExpr,
}

#[derive(Debug, Clone, Debug, PartialEq)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq)]
enum Tok {
    Int(i64, Span),
    Str(String, Span),
    Ident(String, Span),
    Symbol(&'static str, Span),
}

impl Tok {
    fn span(&self) -> Span {
        match self {
            Tok::Int(_, s) | Tok::Str(_, s) | Tok::Ident(_, s) | Tok::Symbol(_, s) => *s,
        }
    }
}

pub fn parse(source: &str) -> Result<Program, Vec<ParseError>> {
    let tokens = lex(source)?;
    Parser { source, tokens, pos: 0 }.program()
}

fn lex(source: &str) -> Result<Vec<Tok>, Vec<ParseError>> {
    let bytes = source.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mut errors = Vec::new();
    while i < bytes.len() {
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        if b == b'-' && bytes.get(i + 1) == Some(&b'-') {
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        let start = i;
        if b.is_ascii_digit() {
            i += 1;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            let n: i64 = source[start..i].parse().unwrap_or(0);
            out.push(Tok::Int(n, Span::new(start, i)));
            continue;
        }
        if b == b'"' {
            i += 1;
            while i < bytes.len() && bytes[i] != b'"' {
                if bytes[i] == b'\\' {
                    i += 1;
                }
                i += 1;
            }
            if i >= bytes.len() {
                errors.push(ParseError { span: Span::new(start, source.len()), message: "unterminated string".into() });
                break;
            }
            i += 1;
            out.push(Tok::Str(source[start + 1..i - 1].to_string(), Span::new(start, i)));
            continue;
        }
        if b.is_ascii_alphabetic() || b == b'_' {
            i += 1;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                i += 1;
            }
            out.push(Tok::Ident(source[start..i].to_string(), Span::new(start, i)));
            continue;
        }
        let symbols = ["->", "=>", "|", ".."];
        if let Some(sym) = symbols.iter().find(|s| source[i..].starts_with(**s)) {
            i += sym.len();
            out.push(Tok::Symbol(sym, Span::new(start, i)));
            continue;
        }
        if "()[]{},;:=`.<>".contains(b as char) {
            i += 1;
            out.push(Tok::Symbol(&source[start..i], Span::new(start, i)));
            continue;
        }
        errors.push(ParseError { span: Span::new(start, start + 1), message: format!("unexpected character {}", &source[start..start + 1]) });
        i += 1;
    }
    if errors.is_empty() { Ok(out) } else { Err(errors) }
}

struct Parser<'a> {
    source: &'a str,
    tokens: Vec<Tok>,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn program(&mut self) -> Result<Program, Vec<ParseError>> {
        let mut items = Vec::new();
        let mut errors = Vec::new();
        while self.pos < self.tokens.len() {
            match self.item() {
                Ok(item) => items.push(item),
                Err(e) => {
                    errors.push(e);
                    self.recover_item();
                }
            }
            self.consume_symbol(";");
        }
        if errors.is_empty() { Ok(Program { items }) } else { Err(errors) }
    }

    fn item(&mut self) -> Result<Item, ParseError> {
        if self.eat_keyword("let").is_some() {
            let rec = self.eat_keyword("rec").is_some();
            Ok(Item::Let(self.let_after_let(rec)?))
        } else {
            Ok(Item::Expr(self.expr()?))
        }
    }

    fn recover_item(&mut self) {
        while self.pos < self.tokens.len() {
            if let Tok::Symbol(";", _) = self.tokens[self.pos] {
                self.pos += 1;
                return;
            }
            if let Tok::Ident(w, _) = &self.tokens[self.pos] {
                if w == "let" {
                    return;
                }
            }
            self.pos += 1;
        }
    }

    fn let_after_let(&mut self, rec: bool) -> Result<LetBind, ParseError> {
        let let_span = if rec {
            self.keyword_span("rec")?.join(self.expect_keyword("in")?.0)
        } else {
            Span::new(0, 0)
        };
        let (name, name_span) = self.expect_ident()?;
        let mut params = Vec::new();
        while self.peek_is_ident() || self.peek_symbol("(") {
            params.push(self.param()?);
        }
        let annot = if self.eat_symbol(":") {
            Some(self.type_expr()?)
        } else { None };
        self.expect_symbol("=")?;
        let value = self.expr()?;
        let body = if self.eat_keyword("in").is_some() {
            self.item()?
        } else {
            self.consume_symbol(";");
            self.item()?
        };
        let body_span = match &body { Item::Let(x) => x.span, Item::Expr(e) => e.span };
        Ok(LetBind { span: name_span.join(value.span).join(body_span), rec, name, name_span, params, annot, value, body: Box::new(body) })
    }

    fn param(&mut self) -> Result<Param, ParseError> {
        if self.peek_symbol("(") {
            self.pos += 1;
            let (name, span) = self.expect_ident()?;
            let annot = if self.eat_symbol(":") { Some(self.type_expr()?) } else { None };
            self.expect_symbol(")")?;
            Ok(Param { name, span, annot })
        } else {
            let (name, span) = self.expect_ident()?;
            Ok(Param { name, span, annot: None })
        }
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        let mut head = self.app_expr()?;
        loop {
            if self.eat_symbol(".") {
                let (label, span) = self.expect_ident()?;
                head = Expr { span: head.span.join(span), kind: ExprKind::Field(Box::new(head), label, span) };
            } else if self.eat_symbol(":") {
                let ty = self.type_expr()?;
                head = Expr { span: head.span.join(ty.span), kind: ExprKind::Ascribe(Box::new(head), ty) };
            } else {
                break;
            }
        }
        Ok(head)
    }

    fn app_expr(&mut self) -> Result<Expr, ParseError> {
        let mut func = self.atomic_expr()?;
        loop {
            if self.peek_atom_start() {
                let arg = self.atomic_expr()?;
                let span = func.span.join(arg.span);
                let args = match func.kind {
                    ExprKind::App(f, mut args) => {
                        args.push(arg);
                        func = *f;
                        args
                    }
                    _ => vec![arg],
                };
                func = Expr { span, kind: ExprKind::App(Box::new(func), args) };
            } else {
                break;
            }
        }
        Ok(func)
    }
    }

    fn atomic_expr(&mut self) -> Result<Expr, ParseError> {
        if self.eat_keyword("fun").is_some() {
            let start = self.prev_span();
            let mut params = Vec::new();
            loop {
                params.push(self.param()?);
                if self.eat_symbol("=>") { break; }
            }
            let body = self.expr()?;
            return Ok(Expr { span: start.join(body.span), kind: ExprKind::Lam(params, Box::new(body)) });
        }
        if self.eat_keyword("let").is_some() {
            let start = self.prev_span();
            let rec = self.eat_keyword("rec").is_some();
            let bind = self.let_after_let(rec)?;
            return Ok(Expr { span: start.join(bind.span), kind: ExprKind::Let(Box::new(bind)) });
        }
        if self.eat_keyword("if").is_some() {
            let cond = self.expr()?;
            self.expect_keyword("then")?;
            let yes = self.expr()?;
            self.expect_keyword("else")?;
            let no = self.expr()?;
            return Ok(Expr { span: cond.span.join(no.span), kind: ExprKind::If(Box::new(cond), Box::new(yes), Box::new(no)) });
        }
        if self.eat_symbol("`") {
            let start = self.prev_span().start;
            let (label, _) = self.expect_ident()?;
            let payload = self.atomic_expr()?;
            return Ok(Expr { span: Span::new(start, payload.span.end), kind: ExprKind::Variant(label, Box::new(payload)) });
        }
        if self.eat_keyword("match").is_some() {
            let start = self.prev_span();
            let scrut = self.atomic_expr()?;
            self.expect_symbol("{")?;
            let mut arms = Vec::new();
            while !self.peek_symbol("}") {
                self.expect_symbol("`")?;
                let (label, label_span) = self.expect_ident()?;
                let (binding, binding_span) = self.expect_ident()?;
                self.expect_symbol("=>")?;
                let body = self.expr()?;
                arms.push(MatchArm { label, label_span, binding, binding_span, body });
                if !self.eat_symbol(",") { break; }
            }
            let end = self.expect_symbol("}")?;
            return Ok(Expr { span: start.join(end), kind: ExprKind::Match(Box::new(scrut), arms) });
        }
        if self.peek_symbol("(") {
            let start = self.tokens[self.pos].span();
            self.pos += 1;
            if self.eat_symbol(")") {
                return Ok(Expr { span: start, kind: ExprKind::Tuple(vec![]) });
            }
            let first = self.expr()?;
            if self.eat_symbol(",") {
                let mut values = vec![first];
                while !self.peek_symbol(")") {
                    values.push(self.expr()?);
                    if !self.eat_symbol(",") { break; }
                }
                let end = self.expect_symbol(")")?;
                return Ok(Expr { span: start.join(end), kind: ExprKind::Tuple(values) });
            }
            self.expect_symbol(")")?;
            return Ok(first);
        }
        if self.peek_record_start() {
            return self.record_expr();
        }
        let tok = self.tokens.get(self.pos).cloned().ok_or_else(|| ParseError { span: Span::new(self.source.len(), self.source.len()), message: "unexpected end of input".into() })?;
        self.pos += 1;
        match tok {
            Tok::Int(v, span) => Ok(Expr { span, kind: ExprKind::Int(v) }),
            Tok::Str(v, span) => Ok(Expr { span, kind: ExprKind::Str(v) }),
            Tok::Ident(w, span) if w == "true" => Ok(Expr { span, kind: ExprKind::Bool(true) }),
            Tok::Ident(w, span) if w == "false" => Ok(Expr { span, kind: ExprKind::Bool(false) }),
            Tok::Ident(w, span) => Ok(Expr { span, kind: ExprKind::Var(w) }),
            other => Err(ParseError { span: other.span(), message: "expected expression".into() }),
        }
    }

    fn record_expr(&mut self) -> Result<Expr, ParseError> {
        let start = self.tokens[self.pos].span();
        let variant = self.eat_symbol("{|");
        let mut fields = Vec::new();
        let mut extension = None;
        while !self.peek_symbol("}") && !self.peek_symbol("|}") {
            if self.eat_symbol("..") {
                extension = Some(Box::new(self.app_expr()?));
                self.eat_symbol(",");
                break;
            }
            let (name, name_span) = self.expect_ident()?;
            self.expect_symbol("=")?;
            let value = self.expr()?;
            fields.push(FieldExpr { name, name_span, value });
            if !self.eat_symbol(",") { break; }
        }
        let end = if variant { self.expect_symbol("|}")? } else { self.expect_symbol("}")? };
        let span = start.join(end);
        if let Some(base) = extension {
            Ok(Expr { span, kind: ExprKind::Extend(base, fields) })
        } else if variant {
            if fields.len() != 1 { return Err(ParseError { span, message: "variant literal is `Label expr or {| label = expr |}".into() }); }
            let f = fields.remove(0);
            Ok(Expr { span, kind: ExprKind::Variant(f.name, Box::new(f.value)) })
        } else {
            Ok(Expr { span, kind: ExprKind::Record(fields, None) })
        }
    }

    fn type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        let left = self.type_atom()?;
        if self.eat_symbol("->") {
            let right = self.type_expr()?;
            Ok(TypeExpr { span: left.span.join(right.span), kind: TypeExprKind::Arrow(Box::new(left), Box::new(right)) })
        } else { Ok(left) }
    }

    fn type_atom(&mut self) -> Result<TypeExpr, ParseError> {
        if self.peek_symbol("{") || self.peek_symbol("{|") {
            let record = self.eat_symbol("{");
            if !record { self.eat_symbol("{|"); }
            return self.row_type(self.prev_span(), record);
        }
        if self.peek_symbol("(") {
            let start = self.tokens[self.pos].span();
            self.pos += 1;
            let left = self.type_expr()?;
            self.expect_symbol(")")?;
            if self.eat_symbol("->") {
                let right = self.type_expr()?;
                return Ok(TypeExpr { span: start.join(right.span), kind: TypeExprKind::Arrow(Box::new(left), Box::new(right)) });
            }
            return Ok(left);
        }
        match self.tokens.get(self.pos).cloned() {
            Some(Tok::Ident(name, span)) => {
                self.pos += 1;
                if name.chars().next().is_some_and(char::is_ascii_lowercase) {
                    Ok(TypeExpr { span, kind: TypeExprKind::Var(name) })
                } else { Ok(TypeExpr { span, kind: TypeExprKind::Con(name) }) }
            }
            Some(other) => Err(ParseError { span: other.span(), message: "expected type".into() }),
            None => Err(ParseError { span: Span::new(self.source.len(), self.source.len()), message: "expected type".into() }),
        }
    }

    fn row_type(&mut self, start: Span, record: bool) -> Result<TypeExpr, ParseError> {
        let mut fields = Vec::new();
        let mut tail = None;
        let end_tok = if record { "}" } else { "|}" };
        while !self.peek_symbol(end_tok) {
            if self.peek_type_var_tail() {
                tail = Some(self.expect_ident()?.0);
                break;
            }
            let (name, name_span) = self.expect_ident()?;
            self.expect_symbol(":")?;
            let value = self.type_expr()?;
            fields.push(TypeField { name, name_span, value });
            if !self.eat_symbol(",") { break; }
        }
        let end = self.expect_symbol(end_tok)?;
        let kind = if record { TypeExprKind::Record(fields, tail) } else { TypeExprKind::Variant(fields, tail) };
        Ok(TypeExpr { span: start.join(end), kind })
    }

    fn peek_type_var_tail(&self) -> bool {
        matches!((self.tokens.get(self.pos), self.tokens.get(self.pos + 1)),
            (Some(Tok::Ident(name, _)), Some(Tok::Symbol(sym, _)))
            if name.chars().next().is_some_and(char::is_ascii_lowercase) && (*sym == "," || *sym == "}" || *sym == "|}"))
    }

    fn peek_atom_start(&self) -> bool {
        matches!(self.tokens.get(self.pos), Some(Tok::Int(..) | Tok::Str(..) | Tok::Ident(..) | Tok::Symbol("(" | "`" | "{" | "{|")))
            || self.peek_keyword("fun") || self.peek_keyword("if") || self.peek_keyword("match") || self.peek_keyword("let")
    }
    fn peek_record_start(&self) -> bool { self.peek_symbol("{") || self.peek_symbol("{|") }
    fn peek_symbol(&self, sym: &str) -> bool { matches!(self.tokens.get(self.pos), Some(Tok::Symbol(s, _)) if *s == sym) }
    fn eat_symbol(&mut self, sym: &str) -> bool { if self.peek_symbol(sym) { self.pos += 1; true } else { false } }
    fn expect_symbol(&mut self, sym: &str) -> Result<Span, ParseError> {
        if self.eat_symbol(sym) { Ok(self.prev_span()) } else {
            let span = self.tokens.get(self.pos).map_or_else(|| Span::new(self.source.len(), self.source.len()), Tok::span);
            Err(ParseError { span, message: format!("expected '{sym}'") })
        }
    }
    fn peek_keyword(&self, word: &str) -> bool { matches!(self.tokens.get(self.pos), Some(Tok::Ident(w, _)) if w == word) }
    fn eat_keyword(&mut self, word: &str) -> Option<Span> { if self.peek_keyword(word) { self.pos += 1; Some(self.prev_span()) } else { None } }
    fn expect_keyword(&mut self, word: &str) -> Result<(String, Span), ParseError> {
        if self.eat_keyword(word).is_some() { Ok((word.into(), self.prev_span())) } else {
            let span = self.tokens.get(self.pos).map_or_else(|| Span::new(self.source.len(), self.source.len()), Tok::span);
            Err(ParseError { span, message: format!("expected '{word}'") })
        }
    }
    fn expect_ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.tokens.get(self.pos).cloned() {
            Some(Tok::Ident(w, s)) => { self.pos += 1; Ok((w, s)) }
            _ => Err(ParseError { span: Span::new(self.source.len(), self.source.len()), message: "expected identifier".into() }),
        }
    }
    fn peek_is_ident(&self) -> bool {
        matches!(self.tokens.get(self.pos), Some(Tok::Ident(w, _)) if !["let","rec","in","fun","if","then","else","match","true","false"].contains(&w.as_str()))
    }
    fn keyword_span(&mut self, word: &str) -> Result<Span, ParseError> { Ok(self.expect_keyword(word)?.1) }
    fn prev_span(&self) -> Span { self.tokens[self.pos.saturating_sub(1)].span() }
}
