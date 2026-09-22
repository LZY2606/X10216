use crate::syntax::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    Int(i64),
    Ident(String),
    Upper(String),
    Symbol(String),
}

#[derive(Clone)]
struct Lexeme {
    tok: Tok,
    span: Span,
}

pub fn parse(input: &str) -> Result<ExprNode, ParseError> {
    let lexemes = lex(input)?;
    let mut p = Parser { input, lexemes, pos: 0, tmp: 0 };
    let expr = p.expr(0)?;
    p.expect(&Tok::Symbol(";".into()))?;
    if p.pos != p.lexemes.len() {
        return Err(p.fail("unexpected trailing input", p.cur_span()));
    }
    assign_ids(&expr)
}

fn lex(input: &str) -> Result<Vec<Lexeme>, ParseError> {
    let bytes = input.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let start = i;
        let b = bytes[i];
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        if b == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
            while i < bytes.len() && bytes[i] != b'\n' { i += 1; }
            continue;
        }
        if b.is_ascii_digit() {
            let s = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() { i += 1; }
            let n: i64 = input[s..i].parse().map_err(|_| ParseError { message: "invalid integer".into(), span: Span { start: s, end: i } })?;
            out.push(Lexeme { tok: Tok::Int(n), span: Span { start, end: i } });
        } else if b == b'.' && i + 3 <= bytes.len() && &input[i..i+3] == "..." {
            out.push(Lexeme { tok: Tok::Ident("...".into()), span: Span { start, end: start + 3 } });
            i += 3;
        } else if b == b'\'' || b == b'_' || b.is_ascii_alphabetic() {
            while i < bytes.len() && (bytes[i] == b'_' || bytes[i].is_ascii_alphanumeric() || bytes[i] == b'\'') { i += 1; }
            let name = &input[start..i];
            let tok = if b.is_ascii_uppercase() { Tok::Upper(name.to_string()) } else { Tok::Ident(name.to_string()) };
            out.push(Lexeme { tok, span: Span { start, end: i } });
        } else {
            let two: String = input[i..].chars().take(2).collect();
            if two == "->" || two == "|" || two == "::" {
                let len = if two == "->" || two == "::" { 2 } else { 1 };
                out.push(Lexeme { tok: Tok::Symbol(input[start..start + len].to_string()), span: Span { start, end: start + len } });
                i += len;
            } else if "(){}[],=.\\".contains(b as char) {
                out.push(Lexeme { tok: Tok::Symbol((b as char).to_string()), span: Span { start, end: start + 1 } });
                i += 1;
            } else {
                return Err(ParseError { message: format!("unexpected character '{}'", b as char), span: Span { start, end: start + 1 } });
            }
        }
    }
    Ok(out)
}

struct Parser<'a> { input: &'a str, lexemes: Vec<Lexeme>, pos: usize, tmp: usize }

impl<'a> Parser<'a> {
    fn tid(&mut self) -> String { let s = format!("t{}", self.tmp); self.tmp += 1; s }
    fn cur(&self) -> Option<&Lexeme> { self.lexemes.get(self.pos) }
    fn cur_span(&self) -> Span { self.cur().map(|x| x.span).unwrap_or(Span { start: self.input.len(), end: self.input.len() }) }
    fn fail<T>(&self, m: &str, span: Span) -> Result<T, ParseError> { Err(ParseError { message: m.into(), span }) }
    fn eat(&mut self, wanted: &Tok) -> bool { if self.cur().map(|x| &x.tok == wanted).unwrap_or(false) { self.pos += 1; true } else { false } }
    fn expect(&mut self, wanted: &Tok) -> Result<Lexeme, ParseError> {
        if self.cur().map(|x| &x.tok == wanted).unwrap_or(false) { let l = self.lexemes[self.pos].clone(); self.pos += 1; Ok(l) }
        else { self.fail(&format!("expected {wanted:?}"), self.cur_span()) }
    }
    fn ident(&mut self) -> Result<String, ParseError> { match self.cur().cloned() { Some(Lexeme { tok: Tok::Ident(s), .. }) => { self.pos += 1; Ok(s) }, _ => self.fail("expected identifier", self.cur_span()) } }
    fn upper(&mut self) -> Result<String, ParseError> { match self.cur().cloned() { Some(Lexeme { tok: Tok::Upper(s), .. }) => { self.pos += 1; Ok(s) }, _ => self.fail("expected capitalized label", self.cur_span()) } }
    fn label(&mut self) -> Result<Spanned<String>, ParseError> {
        let l = self.cur().cloned();
        match l { Some(Lexeme { tok: Tok::Ident(s), span }) | Some(Lexeme { tok: Tok::Upper(s), span }) => { self.pos += 1; Ok(Spanned::new(s, span, String::new())); }, _ => self.fail("expected label", self.cur_span()) }
    }
    fn expr(&mut self, min: u8) -> Result<ExprNode, ParseError> {
        let start = self.cur_span().start;
        let mut left = self.unary()?;
        loop {
            let sym = match self.cur() { Some(Lexeme { tok: Tok::Symbol(s), .. }) if s == "." => s.clone(), _ => break };
            let prec = 10u8;
            if prec < min { break; }
            self.pos += 1;
            if sym == "." {
                let label = self.label()?;
                let end = label.span.end;
                left = Spanned::new(Expr::Field { record: Box::new(left), label: label.node }, Span { start, end }, self.tid());
            }
        }
        while let Some(Lexeme { tok: Tok::Symbol(s), .. }) = self.cur() {
            if s != "|" || 1 < min { break; }
            self.pos += 1;
            let right = self.expr(2)?;
            let end = right.span.end;
            left = Spanned::new(Expr::App { fun: Box::new(left), arg: Box::new(right) }, Span { start, end }, self.tid());
        }
        Ok(left)
    }
    fn unary(&mut self) -> Result<ExprNode, ParseError> {
        match self.cur().cloned() {
            Some(Lexeme { tok: Tok::Ident(s), .. }) if s == "\\" => {
                let start = self.cur_span().start; self.pos += 1;
                let param = self.ident()?;
                self.expect(&Tok::Symbol("->".into()))?;
                let body = self.expr(0)?;
                let end = body.span.end;
                Ok(Spanned::new(Expr::Fun { param, body: Box::new(body) }, Span { start, end }, self.tid()))
            }
            Some(Lexeme { tok: Tok::Ident(s), .. }) if s == "let" => self.let_expr(false),
            Some(Lexeme { tok: Tok::Ident(s), .. }) if s == "letrec" => self.let_expr(true),
            Some(Lexeme { tok: Tok::Ident(s), .. }) if s == "true" || s == "false" => {
                let l = self.lexemes[self.pos].clone(); self.pos += 1;
                Ok(Spanned::new(Expr::Bool(s == "true"), l.span, self.tid()))
            }
            Some(Lexeme { tok: Tok::Ident(s), .. }) if s == "match" => self.match_expr(),
            _ => self.atom(),
        }
    }
    fn let_expr(&mut self, rec: bool) -> Result<ExprNode, ParseError> {
        let start = self.cur_span().start;
        self.pos += 1;
        let name = self.ident()?;
        self.expect(&Tok::Symbol("=".into()))?;
        let value = self.expr(0)?;
        self.expect(&Tok::Symbol(",".into()))?;
        let body = self.expr(0)?;
        let end = body.span.end;
        Ok(Spanned::new(Expr::Let { name, rec, value: Box::new(value), body: Box::new(body) }, Span { start, end }, self.tid()))
    }
    fn match_expr(&mut self) -> Result<ExprNode, ParseError> {
        let start = self.cur_span().start;
        self.pos += 1;
        let scrutinee = self.expr(2)?;
        self.expect(&Tok::Symbol("{".into()))?;
        let mut arms = Vec::new();
        loop {
            let pattern = self.pattern()?;
            self.expect(&Tok::Symbol("->".into()))?;
            let body = self.expr(0)?;
            arms.push(Arm { pattern, body });
            if !self.eat(&Tok::Symbol(",".into())) { break; }
        }
        self.expect(&Tok::Symbol("}".into()))?;
        let end = self.cur_span().start;
        Ok(Spanned::new(Expr::Match { scrutinee: Box::new(scrutinee), arms }, Span { start, end }, self.tid()))
    }
    fn pattern(&mut self) -> Result<PatternNode, ParseError> {
        let l = self.cur().cloned();
        match l {
            Some(Lexeme { tok: Tok::Ident(name), span }) => { self.pos += 1; Ok(Spanned::new(Pattern::Binding(name), span, self.tid())) }
            Some(Lexeme { tok: Tok::Upper(label), span }) => {
                self.pos += 1;
                let binding = if matches!(self.cur(), Some(Lexeme { tok: Tok::Ident(_), .. })) { Some(self.ident()?) } else { None };
                Ok(Spanned::new(Pattern::Variant { label, binding }, span, self.tid()))
            }
            _ => self.fail("expected pattern", self.cur_span()),
        }
    }
    fn atom(&mut self) -> Result<ExprNode, ParseError> {
        let l = self.cur().cloned();
        match l {
            Some(Lexeme { tok: Tok::Int(n), span }) => { self.pos += 1; Ok(Spanned::new(Expr::Int(n), span, self.tid())) }
            Some(Lexeme { tok: Tok::Ident(name), span }) if name != "true" && name != "false" => {
                self.pos += 1;
                let mut node = Spanned::new(Expr::Var { name, depth: 0 }, span, self.tid());
                while self.eat(&Tok::Symbol("::".into())) {
                    let ty = self.typ()?;
                    let end = ty.span.end;
                    node = Spanned::new(Expr::Annotation { expr: Box::new(node), ty }, Span { start: node.span.start, end }, self.tid());
                }
                Ok(node)
            }
            Some(Lexeme { tok: Tok::Upper(label), span }) => {
                self.pos += 1;
                let payload = if matches!(self.cur(), Some(Lexeme { tok: Tok::Symbol(s), .. }) if s == "(") {
                    self.pos += 1;
                    let e = self.expr(0)?;
                    self.expect(&Tok::Symbol(")".into()))?;
                    Some(Box::new(e))
                } else { None };
                let end = payload.as_ref().map_or(span.end, |x| x.span.end);
                Ok(Spanned::new(Expr::Variant { label, payload }, Span { start: span.start, end }, self.tid()))
            }
            Some(Lexeme { tok: Tok::Symbol(s), span }) if s == "(" => {
                self.pos += 1;
                let e = self.expr(0)?;
                self.expect(&Tok::Symbol(")".into()))?;
                Ok(e)
            }
            Some(Lexeme { tok: Tok::Symbol(s), span }) if s == "{" => self.record(span.start),
            _ => self.fail("expected expression", self.cur_span()),
        }
    }
    fn record(&mut self, start: usize) -> Result<ExprNode, ParseError> {
        self.pos += 1;
        let mut fields = Vec::new();
        if !self.eat(&Tok::Symbol("}".into())) {
            loop {
                let label = self.label()?;
                self.expect(&Tok::Symbol("=".into()))?;
                let value = self.expr(0)?;
                fields.push(Field { label: label.node, value });
                if !self.eat(&Tok::Symbol(",".into())) { break; }
            }
            self.expect(&Tok::Symbol("}".into()))?;
        }
        let end = self.cur_span().start;
        Ok(Spanned::new(Expr::Record { fields }, Span { start, end }, self.tid()))
    }
    fn typ(&mut self) -> Result<TypeNode, ParseError> {
        let left = self.typ_atom()?;
        if self.eat(&Tok::Symbol("->".into())) {
            let right = self.typ()?;
            let span = Span { start: left.span.start, end: right.span.end };
            return Ok(Spanned::new(Type::Fun(Box::new(left), Box::new(right)), span, self.tid()));
        }
        Ok(left)
    }
    fn typ_atom(&mut self) -> Result<TypeNode, ParseError> {
        let l = self.cur().cloned();
        match l {
            Some(Lexeme { tok: Tok::Ident(name), span }) if name == "int" || name == "bool" => {
                self.pos += 1;
                let node = if name == "int" { Type::Int } else { Type::Bool };
                Ok(Spanned::new(node, span, self.tid()))
            }
            Some(Lexeme { tok: Tok::Ident(name), span }) if name.starts_with('\'') => {
                self.pos += 1;
                Ok(Spanned::new(Type::Var(name), span, self.tid()))
            }
            Some(Lexeme { tok:Tok::Symbol(s), span }) if s == "{" => self.row_type(span.start, false),
            Some(Lexeme { tok:Tok::Symbol(s), span }) if s == "[" => self.row_type(span.start, true),
            Some(Lexeme { tok:Tok::Symbol(s), span }) if s == "(" => {
                self.pos += 1; let x = self.typ()?; self.expect(&Tok::Symbol(")".into()))?; Ok(x)
            }
            _ => self.fail("expected type", self.cur_span()),
        }
    }
    fn row_type(&mut self, start: usize, variant: bool) -> Result<TypeNode, ParseError> {
        let open_sym = if variant { "[" } else { "{" };
        let close_sym = if variant { "]" } else { "}" };
        self.expect(&Tok::Symbol(open_sym.into()))?;
        let mut fields = Vec::new();
        let mut extension = None;
        if !self.eat(&Tok::Symbol(close_sym.into())) {
            loop {
                if matches!(self.cur(), Some(Lexeme { tok: Tok::Ident(s), .. }) if s == "...") {
                    let l = self.lexemes[self.pos].clone();
                    self.pos += 1;
                    let rowvar = self.ident()?;
                    extension = Some(Spanned::new(rowvar, l.span, String::new()));
                } else {
                    let label = self.label()?;
                    self.expect(&Tok::Symbol("=".into()))?;
                    let value = self.typ()?;
                    fields.push(RowField { label, value });
                }
                if !self.eat(&Tok::Symbol(",".into())) { break; }
            }
            self.expect(&Tok::Symbol(close_sym.into()))?;
        }
        let end = self.cur_span().start;
        let row = Row { fields, extension };
        let ty = if variant { Type::Variant(row) } else { Type::Record(row) };
        Ok(Spanned::new(ty, Span { start, end }, self.tid()))
    }
}

fn assign_ids(root: &ExprNode) -> Result<ExprNode, ParseError> {
    let mut root = root.clone();
    resolve(&mut root, &mut Vec::new())?;
    let id = identify(&root, "root");
    if let Some(r) = id { root = r; }
    Ok(root)
}

fn resolve(e: &mut ExprNode, scopes: &mut Vec<String>) -> Result<(), ParseError> {
    match &mut e.node {
        Expr::Int(_) | Expr::Bool(_) => {}
        Expr::Var { name, depth } => {
            let found = scopes.iter().rev().position(|x| x == name);
            *depth = found.ok_or_else(|| ParseError { message: format!("unbound variable '{name}'"), span: e.span })?;
        }
        Expr::Fun { param, body } => {
            scopes.push(param.clone());
            resolve(body, scopes)?;
            scopes.pop();
        }
        Expr::App { fun, arg } => { resolve(fun, scopes)?; resolve(arg, scopes)?; }
        Expr::Let { name, rec, value, body } => {
            if *rec { scopes.push(name.clone()); }
            resolve(value, scopes)?;
            if *rec { scopes.pop(); }
            scopes.push(name.clone());
            resolve(body, scopes)?;
            scopes.pop();
        }
        Expr::Record { fields } => for f in fields { resolve(&mut f.value, scopes)?; },
        Expr::Field { record, .. } => resolve(record, scopes)?,
        Expr::Extend { value, rest, .. } => { resolve(value, scopes)?; if let Some(r) = rest { resolve(r, scopes)?; } }
        Expr::Variant { payload, .. } => if let Some(p) = payload { resolve(p, scopes)?; },
        Expr::Match { scrutinee, arms } => {
            resolve(scrutinee, scopes)?;
            for arm in arms {
                let bound = match &arm.pattern.node {
                    Pattern::Binding(name) => Some(name.clone()),
                    Pattern::Variant { binding, .. } => binding.clone(),
                };
                if let Some(name) = bound { scopes.push(name); }
                resolve(&mut arm.body, scopes)?;
                if let Some(name) = bound { let _ = name; scopes.pop(); }
            }
        }
        Expr::Annotation { expr, .. } => resolve(expr, scopes)?,
    }
    Ok(())
}

fn identify(e: &ExprNode, path: &str) -> Option<ExprNode> {
    let mut out = e.clone();
    out.id = path.to_string();
    let child = |x: &ExprNode, p: String| identify(x, &p);
    out.node = match out.node {
        Expr::Int(_) | Expr::Bool(_) | Expr::Var { .. } => out.node.clone(),
        Expr::Fun { param, body } => Expr::Fun { param, body: Box::new(child(&body, format!("{path}/body"))) },
        Expr::App { fun, arg } => Expr::App { fun: Box::new(child(&fun, format!("{path}/fun"))), arg: Box::new(child(&arg, format!("{path}/arg"))) },
        Expr::Let { name, rec, value, body } => Expr::Let {
            name, rec,
            value: Box::new(child(&value, format!("{path}/value"))),
            body: Box::new(child(&body, format!("{path}/body"))),
        },
        Expr::Record { fields } => Expr::Record {
            fields: fields.into_iter().enumerate().map(|(i, mut f)| {
                let p = format!("{path}/field:{}", f.label);
                f.value = identify(&f.value, &p);
                let _ = i;
                f
            }).collect()
        },
        Expr::Field { record, label } => Expr::Field { record: Box::new(child(&record, format!("{path}/record"))), label },
        Expr::Extend { label, value, rest } => Expr::Extend {
            label,
            value: Box::new(child(&value, format!("{path}/value"))),
            rest: rest.map(|r| Box::new(child(&r, format!("{path}/rest")))),
        },
        Expr::Variant { label, payload } => Expr::Variant {
            label,
            payload: payload.map(|p| Box::new(child(&p, format!("{path}/payload")))),
        },
        Expr::Match { scrutinee, arms } => Expr::Match {
            scrutinee: Box::new(child(&scrutinee, format!("{path}/scrutinee"))),
            arms: arms.into_iter().enumerate().map(|(i, mut arm)| {
                let p = format!("{path}/arm{i}");
                arm.pattern.id = format!("{p}/pat");
                arm.body = identify(&arm.body, &format!("{p}/body"));
                arm
            }).collect(),
        },
        Expr::Annotation { expr, ty } => Expr::Annotation { expr: Box::new(child(&expr, format!("{path}/expr"))), ty },
    };
    Some(out)
}
