use crate::ast::*;
use anyhow::{bail, Result};

#[derive(Debug, Clone, PartialEq, Eq)]
enum TokKind { Ident(String), Int(String), Bool(bool), Symbol(String), Keyword(String) }
#[derive(Debug, Clone)]
struct Token { kind: TokKind, span: Span }

pub struct Parser<'a> {
    source: &'a [u8],
    tokens: Vec<Token>,
    pos: usize,
    next_uid: usize,
}

#[derive(Clone)]
struct Scope<'a> { parent: Option<&'a Scope<'a>>, entries: Vec<(String, usize)> }

impl<'a> Scope<'a> {
    fn lookup(&self, name: &str) -> Option<usize> {
        self.entries.iter().rev().find(|(n, _)| n == name).map(|(_, id)| *id)
            .or_else(|| self.parent.and_then(|p| p.lookup(name)))
    }
}

impl<'a> Parser<'a> {
    pub fn parse(source: &'a str) -> Result<RawProgram> {
        let tokens = lex(source)?;
        let mut p = Parser { source: source.as_bytes(), tokens, pos: 0, next_uid: 0 };
        let mut program = RawProgram::default();
        while !p.eof() {
            let root = p.parse_top(&mut program)?;
            program.roots.push(root);
        }
        Ok(program)
    }

    fn uid(&mut self) -> usize { let v = self.next_uid; self.next_uid += 1; v }

    fn make(&mut self, program: &mut RawProgram, kind: Kind, span: Span, children: Vec<(String, usize)>) -> usize {
        let id = self.uid();
        program.nodes.insert(id, RawNode { uid: id, kind, span, children });
        id
    }

    fn make_binder(&mut self, program: &mut RawProgram, name: &str, id: usize, span: Span) {
        program.nodes.insert(id, RawNode { uid: id, kind: Kind::Var { name: name.to_string(), binder: Some(id) }, span, children: vec![] });
        if id >= self.next_uid { self.next_uid = id + 1; }
    }

    fn eof(&self) -> bool { self.pos >= self.tokens.len() }
    fn peek(&self) -> Option<&Token> { self.tokens.get(self.pos) }
    fn span_of(&self, idx: usize) -> Span { self.tokens[idx].span }

    fn eat_symbol(&mut self, s: &str) -> bool {
        if matches!(self.peek().map(|t| &t.kind), Some(TokKind::Symbol(x)) if x == s) { self.pos += 1; true } else { false }
    }
    fn eat_keyword(&mut self, s: &str) -> bool {
        if matches!(self.peek().map(|t| &t.kind), Some(TokKind::Keyword(x)) if x == s) { self.pos += 1; true } else { false }
    }
    fn expect_symbol(&mut self, s: &str) -> Result<Span> {
        if self.eat_symbol(s) { Ok(self.tokens[self.pos - 1].span) } else { bail!("expected {s} at token {}", self.pos) }
    }

    fn parse_top(&mut self, program: &mut RawProgram) -> Result<RawRoot> {
        let start = self.peek().map(|t| t.span.start).unwrap_or(self.source.len());
        if self.eat_keyword("let") {
            let rec = self.eat_keyword("rec");
            let name_tok = self.expect_ident()?;
            self.expect_symbol("=")?;
            let value = self.parse_expr(program, scope)?;
            let mut body = None;
            if self.eat_keyword("in") { body = Some(self.parse_expr(program, &Scope::default())?); }
            let end = body.and_then(|id| program.nodes.get(&id).map(|n| n.span.end)).or_else(|| program.nodes.get(&value).map(|n| n.span.end)).unwrap_or(start);
            let binder_id = p.uid();
            let bnode = p.make_binder(program, &name_tok.0, binder_id, name_tok.1);
            let _ = bnode;
            Ok(RawRoot { kind: RootKind::Let { rec, name: name_tok.0, binder: binder_id, value, body }, span: Span { start, end } })
        } else {
            let e = self.parse_expr(program, &Scope::default())?;
            let span = program.nodes[&e].span;
            Ok(RawRoot { kind: RootKind::Expr(e), span })
        }
    }

    fn expect_ident(&mut self) -> Result<(String, Span)> {
        match self.peek().cloned() {
            Some(Token { kind: TokKind::Ident(s), span }) => { self.pos += 1; Ok((s, span)) }
            _ => bail!("expected identifier"),
        }
    }

    fn parse_expr(&mut self, program: &mut RawProgram, scope: &Scope) -> Result<usize> {
        if self.eat_symbol("\\") || self.eat_keyword("fn") {
            let start = self.tokens[self.pos.saturating_sub(1)].span.start;
            let (param, _) = self.expect_ident()?;
            self.expect_symbol("->")?;
            let binder = self.uid();
            self.make_binder(program, &param, binder, Span { start, end: start + param.len() });
            let inner_scope = Scope { parent: Some(scope), entries: vec![(param.clone(), binder)] };
            let body = self.parse_expr(program, &inner_scope)?;
            let end = program.nodes[&body].span.end;
            return Ok(self.make(program, Kind::Abs { param }, Span { start, end }, vec![("binder", binder), ("body", body)]));
        }
        if self.eat_keyword("let") { return self.parse_let(program, scope); }
        self.parse_app(program, scope)
    }

    fn parse_let(&mut self, program: &mut RawProgram, scope: &Scope) -> Result<usize> {
        let start = self.tokens[self.pos - 1].span.start;
        let rec = self.eat_keyword("rec");
        let (name, name_span) = self.expect_ident()?;
        self.expect_symbol("=")?;
        let binder = self.uid();
        self.make_binder(program, &name, binder, name_span);
        let val_scope = if rec { Scope { parent: Some(scope), entries: vec![(name.clone(), binder)] } } else { scope.clone_lifetime() };
        let value = self.parse_expr(program, &val_scope)?;
        self.expect_keyword("in")?;
        let body_scope = Scope { parent: Some(scope), entries: vec![(name.clone(), binder)] };
        let body = self.parse_expr(program, &body_scope)?;
        let end = program.nodes[&body].span.end;
        Ok(self.make(program, Kind::Let { rec, name }, Span { start, end }, vec![("binder", binder), ("value", value), ("body", body)]))
    }

    fn parse_app(&mut self, program: &mut RawProgram, scope: &Scope) -> Result<usize> {
        let mut func = self.parse_atom(program, scope)?;
        let start = program.nodes[&func].span.start;
        while starts_atom(self.peek()) {
            let arg = self.parse_atom(program, scope)?;
            let end = program.nodes[&arg].span.end;
            func = self.make(program, Kind::App, Span { start, end }, vec![("func", func), ("arg", arg)]);
        }
        while self.eat_symbol(".") {
            let (name, span) = self.expect_ident()?;
            let end = span.end;
            func = self.make(program, Kind::Field { name }, Span { start, end }, vec![("record", func)]);
        }
        Ok(func)
    }

    fn parse_atom(&mut self, program: &mut RawProgram, scope: &Scope) -> Result<usize> {
        let tok = self.peek().cloned();
        match tok {
            Some(Token { kind: TokKind::Int(v), span }) => { self.pos += 1; Ok(self.make(program, Kind::Int { value: v }, span, vec![])) }
            Some(Token { kind: TokKind::Bool(v), span }) => { self.pos += 1; Ok(self.make(program, Kind::Bool { value: v }, span, vec![])) }
            Some(Token { kind: TokKind::Ident(name), span }) => {
                self.pos += 1;
                if self.eat_symbol("{") { return self.parse_record_tail(program, span.start, name); }
                Ok(self.make(program, Kind::Var { binder: scope.lookup(&name), name }, span, vec![]))
            }
            Some(Token { kind: TokKind::Symbol(s), span }) if s == "{" => { self.pos += 1; self.parse_record(program, span.start) }
            Some(Token { kind: TokKind::Symbol(s), span }) if s == "(" => { self.pos += 1; let e = self.parse_expr(program, scope)?; self.expect_symbol(")")?; Ok(e) }
            Some(Token { kind: TokKind::Keyword(k), span }) if k == "match" => { self.pos += 1; self.parse_match(program, scope, span.start) }
            _ => bail!("unexpected token near {}", self.source.len()),
        }
    }

    fn parse_record_tail(&mut self, program: &mut RawProgram, start: usize, tag: String) -> Result<usize> {
        let payload = self.parse_atom(program, scope)?;
        let close = self.expect_symbol("}")?;
        Ok(self.make(program, Kind::Variant { tag }, Span { start, end: close.end }, vec![("payload", payload)]))
    }

    fn parse_record(&mut self, program: &mut RawProgram, start: usize) -> Result<usize> {
        let mut children = vec![];
        if !self.eat_symbol("}") {
            loop {
                let (name, name_span) = self.expect_ident()?;
                self.expect_symbol("=")?;
                let value = self.parse_expr(program, scope)?;
                let end = program.nodes[&value].span.end;
                let field = self.make(program, Kind::RecordField { name }, Span { start: name_span.start, end }, vec![("value", value)]);
                children.push(("field", field));
                if !self.eat_symbol(",") { break; }
            }
            self.expect_symbol("}")?;
        }
        let end = self.tokens[self.pos - 1].span.end;
        Ok(self.make(program, Kind::Record, Span { start, end }, children))
    }

    fn parse_match(&mut self, program: &mut RawProgram, scope: &Scope, start: usize) -> Result<usize> {
        let scrut = self.parse_expr(program, scope)?;
        self.expect_symbol("{")?;
        let mut children = vec![("scrutinee", scrut)];
        while !self.eat_symbol("}") {
            let (tag, tag_span) = self.expect_ident()?;
            let (binding, bind_span) = self.expect_ident()?;
            self.expect_symbol("->")?;
            let binder = self.uid();
            self.make_binder(program, &binding, binder, bind_span);
            let arm_scope = Scope { parent: Some(scope), entries: vec![(binding.clone(), binder)] };
            let value = self.parse_expr(program, &arm_scope)?;
            let arm = self.make(program, Kind::MatchArm { tag, binding }, Span { start: tag_span.start, end: program.nodes[&value].span.end }, vec![("binder", binder), ("value", value)]);
            children.push(("arm", arm));
            if !self.eat_symbol(",") { self.expect_symbol("}")?; break; }
        }
        let end = self.tokens[self.pos - 1].span.end;
        Ok(self.make(program, Kind::Match, Span { start, end }, children))
    }
}

impl<'a> Scope<'a> {
    fn clone_lifetime(&self) -> Scope<'a> { Scope { parent: self.parent, entries: self.entries.clone() } }
}

fn starts_atom(t: Option<&Token>) -> bool {
    matches!(t, Some(Token { kind: TokKind::Int(_) | TokKind::Bool(_) | TokKind::Ident(_), .. })
        | Some(Token { kind: TokKind::Symbol(s), .. }) if s == "{" || s == "(")
}

fn lex(source: &str) -> Result<Vec<Token>> {
    let bytes = source.as_bytes();
    let mut i = 0;
    let mut out = vec![];
    let keywords = ["let", "rec", "in", "fn", "true", "false", "match"];
    while i < bytes.len() {
        if bytes[i].is_ascii_whitespace() { i += 1; continue; }
        let start = i;
        if bytes[i].is_ascii_alphabetic() || bytes[i] == b'_' {
            i += 1;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') { i += 1; }
            let text = &source[start..i];
            let kind = match text {
                "true" => TokKind::Bool(true),
                "false" => TokKind::Bool(false),
                x if keywords.contains(&x) => TokKind::Keyword(x.to_string()),
                x => TokKind::Ident(x.to_string()),
            };
            out.push(Token { kind, span: Span { start, end: i } });
        } else if bytes[i].is_ascii_digit() {
            while i < bytes.len() && bytes[i].is_ascii_digit() { i += 1; }
            out.push(Token { kind: TokKind::Int(source[start..i].to_string()), span: Span { start, end: i } });
        } else {
            let multi = ["->", "{", "}", "(", ")", ".", ",", "="]
                .iter().find(|s| source[i..].starts_with(**s));
            if let Some(s) = multi { i += s.len(); out.push(Token { kind: TokKind::Symbol((*s).to_string()), span: Span { start, end: i } }); }
            else { bail!("unexpected character {} at {i}", bytes[i] as char); }
        }
    }
    Ok(out)
}
