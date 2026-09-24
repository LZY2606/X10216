use crate::syntax::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    Ident(String), Tag(String), Int(String), Str(String),
    LParen, RParen, LBrace, RBrace, Comma, Dot, Equals, Backslash,
    Arrow, FatArrow, Pipe, With, Let, Rec, In, Match, Underscore, Semicolon, Eof,
}

#[derive(Clone)]
struct Token { tok: Tok, start: usize, end: usize }

pub fn parse(input: &str) -> Result<Program, Vec<ParseError>> {
    let toks = Lexer::new(input).lex();
    let mut p = Parser { input, toks, errors: Vec::new() };
    let mut decls = Vec::new();
    let mut result = None;
    while !p.at_end() {
        if p.peek_let() {
            decls.push(p.top_decl());
            p.eat(Tok::Semicolon);
        } else {
            result = Some(p.expr(false));
            if !p.at_end() { p.eat(Tok::Semicolon); }
        }
    }
    if p.errors.is_empty() {
        Ok(Program { decls, result })
    } else {
        Err(p.errors)
    }
}

struct Lexer<'a> { s: &'a [u8], i: usize }
impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self { Self { s: input.as_bytes(), i: 0 } }
    fn lex(mut self) -> Vec<Token> {
        let mut out = Vec::new();
        loop {
            while self.i < self.s.len() && self.s[self.i].is_ascii_whitespace() { self.i += 1; }
            let start = self.i;
            if self.i >= self.s.len() { out.push(Token{tok:Tok::Eof,start,end:self.i}); break; }
            let c = self.s[self.i] as char;
            let tok = if c.is_ascii_digit() {
                while self.i < self.s.len() && self.s[self.i].is_ascii_digit() { self.i += 1; }
                Tok::Int(String::from_utf8_lossy(&self.s[start..self.i]).to_string())
            } else if c == '"' { self.string() }
            else if c.is_ascii_alphabetic() || c == '_' { self.ident() }
            else { self.punct() };
            out.push(Token{tok,start,end:self.i});
        }
        out
    }
    fn string(&mut self) -> Tok {
        self.i += 1;
        let start = self.i;
        while self.i < self.s.len() && self.s[self.i] != b'"' { self.i += 1; }
        let v = String::from_utf8_lossy(&self.s[start..self.i]).to_string();
        if self.i < self.s.len() { self.i += 1; }
        Tok::Str(v)
    }
    fn ident(&mut self) -> Tok {
        let start = self.i;
        if self.s[self.i] == b'_' {
            self.i += 1;
            while self.i < self.s.len() && (self.s[self.i].is_ascii_alphanumeric() || self.s[self.i] == b'_') { self.i += 1; }
            if self.i == start + 1 { return Tok::Underscore; }
        }
        while self.i < self.s.len() && (self.s[self.i].is_ascii_alphanumeric() || self.s[self.i] == b'_') { self.i += 1; }
        let v = String::from_utf8_lossy(&self.s[start..self.i]).to_string();
        match v.as_str() {
            "let" => Tok::Let, "rec" => Tok::Rec, "in" => Tok::In, "match" => Tok::Match,
            "with" => Tok::With, _ if start < self.s.len() && self.s[start].is_ascii_uppercase() => Tok::Tag(v),
            _ => Tok::Ident(v),
        }
    }
    fn punct(&mut self) -> Tok {
        let two = if self.i + 1 < self.s.len() { std::str::from_utf8(&self.s[self.i..self.i+2]).unwrap_or("") } else { "" };
        if two == "->" { self.i += 2; return Tok::Arrow; }
        if two == "=>" { self.i += 2; return Tok::FatArrow; }
        let c = self.s[self.i] as char; self.i += 1;
        match c {
            '(' => Tok::LParen, ')' => Tok::RParen, '{' => Tok::LBrace, '}' => Tok::RBrace,
            ',' => Tok::Comma, '.' => Tok::Dot, '=' => Tok::Equals, '\\' => Tok::Backslash,
            '|' => Tok::Pipe, ';' => Tok::Semicolon, _ => Tok::Eof,
        }
    }
}

struct Parser<'a> {
    input: &'a str,
    toks: Vec<Token>,
    errors: Vec<ParseError>,
}
impl<'a> Parser<'a> {
    fn pos(&self) -> usize { self.toks.first().map(|t| t.start).unwrap_or(self.input.len()) }
    fn at_end(&self) -> bool { matches!(self.toks.first().map(|x| &x.tok), Some(Tok::Eof) | None) }
    fn peek_let(&self) -> bool {
        let a = self.toks.first().map(|x| &x.tok);
        let b = self.toks.get(1).map(|x| &x.tok);
        matches!(a, Some(Tok::Let)) && matches!(b, Some(Tok::Rec | Tok::Ident(_)))
    }
    fn err(&mut self, message: impl Into<String>, start: usize, end: usize) {
        self.errors.push(ParseError { message: message.into(), span: Span::new(start, end) });
    }
    fn bump(&mut self) -> Token {
        if self.toks.len() > 1 { self.toks.remove(0) } else { self.toks[0].clone() }
    }
    fn eat(&mut self, wanted: Tok) -> bool {
        if std::mem::discriminant(&self.toks[0].tok) == std::mem::discriminant(&wanted) { self.bump(); true } else { false }
    }
    fn expect(&mut self, wanted: Tok, what: &str) -> bool {
        if self.eat(wanted) { true } else {
            let t = self.toks[0].clone();
            self.err(format!("expected {what}"), t.start, t.end.max(t.start + 1));
            false
        }
    }
    fn ident(&mut self) -> (String, Span) {
        let t = self.bump();
        match t.tok {
            Tok::Ident(v) => (v, Span::new(t.start,t.end)),
            other => { self.err("expected identifier", t.start, t.end); (format!("__bad_{}", t.start), Span::new(t.start,t.end.max(t.start+1))) },
        }
    }
    fn top_decl(&mut self) -> Decl {
        let start = self.pos();
        self.bump();
        let rec = self.eat(Tok::Rec);
        let (name, ns) = self.ident();
        self.expect(Tok::Equals, "=");
        let rhs = self.expr(true);
        Decl { rec, name, name_span: ns, rhs, span: Span::new(start, self.pos()) }
    }
    fn expr(&mut self, stop_at_in: bool) -> Expr {
        if matches!(self.toks[0].tok, Tok::Let) {
            let start = self.pos(); self.bump();
            let rec = self.eat(Tok::Rec);
            let (name, name_span) = self.ident();
            self.expect(Tok::Equals, "=");
            let rhs = self.expr(true);
            self.expect(Tok::In, "in");
            let body = self.expr(stop_at_in);
            return Expr::Let { node: LetNode { rec, name, name_span, rhs, body, span: Span::new(start, self.pos()) } };
        }
        if matches!(self.toks[0].tok, Tok::Match) { return self.match_expr(); }
        let mut e = self.lam();
        loop {
            if matches!(self.toks[0].tok, Tok::In) && stop_at_in { break; }
            if matches!(self.toks[0].tok, Tok::RParen|Tok::RBrace|Tok::Comma|Tok::Semicolon|Tok::Pipe|Tok::FatArrow|Tok::Eof) { break; }
            if matches!(self.toks[0].tok, Tok::Let | Tok::Match) { break; }
            if matches!(self.toks[0].tok, Tok::Dot) {
                let t = self.bump();
                let (label, ls) = self.label();
                let end = ls.end;
                e = Expr::Field(FieldNode { record: e, label, label_span: ls, span: Span::new(e.span().start, end) });
            } else if matches!(self.toks[0].tok, Tok::LBrace) {
                let end_tok = self.bump();
                let fields = self.fields(Tok::RBrace);
                let end = self.pos();
                e = Expr::Extend(ExtendNode { base: e, fields, span: Span::new(e.span().start, end) });
            } else {
                let arg = self.atom();
                let span = Span::new(e.span().start, arg.span().end);
                e = Expr::App(AppNode { fun: e, arg, span });
            }
        }
        e
    }
    fn lam(&mut self) -> Expr {
        if !matches!(self.toks[0].tok, Tok::Backslash) { return self.atom(); }
        let start = self.pos(); self.bump();
        let (param, var_span) = self.ident();
        self.expect(Tok::Arrow, "->");
        let body = self.expr(false);
        Expr::Lam(LamNode { var_span, param, body, span: Span::new(start, self.pos()) })
    }
    fn fields(&mut self, end: Tok) -> Vec<RecordField> {
        let mut fields = Vec::new();
        if std::mem::discriminant(&self.toks[0].tok) == std::mem::discriminant(&end) { self.bump(); return fields; }
        loop {
            let (label, label_span) = self.label();
            self.expect(Tok::Equals, "=");
            let value = self.expr(true);
            fields.push(RecordField { label, label_span, value });
            if self.eat(Tok::Comma) { continue; }
            self.expect(if matches!(end, Tok::RBrace) { Tok::RBrace } else { Tok::Pipe }, "field list terminator");
            break;
        }
        fields
    }
    fn label(&mut self) -> (String, Span) {
        let t = self.bump();
        match t.tok {
            Tok::Ident(v) => (v, Span::new(t.start,t.end)),
            Tok::Tag(v) => (v, Span::new(t.start,t.end)),
            _ => { self.err("expected field label", t.start, t.end.max(t.start+1)); (String::from("__bad"), Span::new(t.start,t.end.max(t.start+1))) }
        }
    }
    fn match_expr(&mut self) -> Expr {
        let start = self.pos(); self.bump();
        let scrutinee = self.expr(false);
        self.eat(Tok::With);
        self.expect(Tok::LBrace, "{");
        let mut arms = Vec::new();
        self.eat(Tok::Pipe);
        loop {
            let ps = self.pos();
            let pattern = self.pattern(ps);
            self.expect(Tok::FatArrow, "=>");
            let body = self.expr(true);
            arms.push(MatchArm { pattern, body });
            if self.eat(Tok::Comma) { self.eat(Tok::Pipe); continue; }
            if self.eat(Tok::Pipe) { continue; }
            self.expect(Tok::RBrace, "}");
            break;
        }
        Expr::Match(MatchNode { scrutinee, arms, span: Span::new(start, self.pos()) })
    }
    fn pattern(&mut self, start: usize) -> PatternNode {
        let t = self.bump();
        match t.tok {
            Tok::Underscore => PatternNode { pattern: Pattern::Wildcard, span: Span::new(start, t.end) },
            Tok::Ident(v) => PatternNode { pattern: Pattern::Name(v), span: Span::new(start, t.end) },
            Tok::Tag(v) => {
                let mut inner = None;
                let end = if self.eat(Tok::LParen) {
                    let p = self.pattern(self.pos());
                    self.expect(Tok::RParen, ")");
                    let end = self.pos();
                    inner = Some(Box::new(p.pattern));
                    end
                } else { t.end };
                PatternNode { pattern: Pattern::Tag(v, inner), span: Span::new(start,end) }
            }
            _ => {
                self.err("expected match pattern", t.start, t.end.max(t.start+1));
                PatternNode { pattern: Pattern::Wildcard, span: Span::new(start,t.end.max(start+1)) }
            }
        }
    }
    fn atom(&mut self) -> Expr {
        let t = self.toks[0].clone();
        match t.tok {
            Tok::Int(v) => { self.bump(); Expr::Lit(LitNode { value: Lit::Int(v), span: Span::new(t.start,t.end) }) }
            Tok::Str(v) => { self.bump(); Expr::Lit(LitNode { value: Lit::Str(v), span: Span::new(t.start,t.end) }) }
            Tok::Ident(v) => { self.bump(); Expr::Var(VarNode { name: v, span: Span::new(t.start,t.end) }) }
            Tok::Backslash => self.lam(),
            Tok::Tag(v) => {
                self.bump();
                let mut payload = None;
                let mut end = t.end;
                if self.eat(Tok::LParen) {
                    payload = Some(self.expr(true));
                    self.expect(Tok::RParen, ")");
                    end = self.pos();
                }
                Expr::Tag(TagNode { label: v, label_span: Span::new(t.start,t.end), payload, span: Span::new(t.start,end) })
            }
            Tok::LParen => {
                self.bump();
                let e = self.expr(false);
                self.expect(Tok::RParen, ")");
                e
            }
            Tok::LBrace => {
                self.bump();
                let start = t.start;
                let fields = self.fields(Tok::RBrace);
                if let Some(dup) = fields.iter().enumerate().find_map(|(i,f)| fields[..i].iter().find(|x| x.label == f.label)) {
                    self.err(format!("duplicate record field {}", dup.label), dup.label_span.start, dup.label_span.end);
                }
                Expr::Record(RecordNode { fields, span: Span::new(start, self.pos()) })
            }
            _ => {
                self.bump();
                self.err("expected expression", t.start, t.end.max(t.start+1));
                Expr::Lit(LitNode { value: Lit::Int(String::from("0")), span: Span::new(t.start,t.end.max(t.start+1)) })
            }
        }
    }
}
