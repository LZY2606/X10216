//! Hand-written lexer and recursive-descent parser for the demo language.

use crate::ast::*;

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Ident(String),
    Upper(String),
    Int(i64),
    Str(String),
    Punct(String),
    Eof,
}

#[derive(Clone, Debug)]
struct Token {
    tok: Tok,
    span: Span,
}

const KEYWORDS: &[&str] = &[
    "let", "rec", "in", "fun", "if", "then", "else", "true", "false", "match", "with",
    "string", "int", "bool",
];

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_lowercase()
}
fn is_ident_cont(c: char) -> bool {
    c == '_' || c.is_alphanumeric()
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    toks: Vec<Token>,
    issues: Vec<ParseIssue>,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0, toks: Vec::new(), issues: Vec::new() }
    }

    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).map(|b| *b as char)
    }
    fn peek2(&self, off: usize) -> Option<char> {
        self.src.get(self.pos + off).map(|b| *b as char)
    }
    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        Some(c)
    }

    fn lex(mut self) -> (Vec<Token>, Vec<ParseIssue>) {
        while self.pos < self.src.len() {
            let c = self.peek().unwrap();
            if c.is_whitespace() {
                self.bump();
                continue;
            }
            if c == '/' && self.peek2(1) == Some('/') {
                while let Some(ch) = self.peek() {
                    if ch == '\n' {
                        break;
                    }
                    self.bump();
                }
                continue;
            }
            if c == '/' && self.peek2(1) == Some('*') {
                let start = self.pos;
                self.bump();
                self.bump();
                while self.pos < self.src.len() {
                    if self.peek() == Some('*') && self.peek2(1) == Some('/') {
                        self.bump();
                        self.bump();
                        break;
                    }
                    self.bump();
                }
                if self.pos >= self.src.len() {
                    self.issues.push(ParseIssue {
                        message: "unterminated block comment".into(),
                        span: Span::new(start, self.src.len()),
                    });
                }
                continue;
            }
            if c.is_ascii_digit() {
                self.lex_number();
                continue;
            }
            if is_ident_start(c) {
                self.lex_ident(false);
                continue;
            }
            if c.is_uppercase() {
                self.lex_ident(true);
                continue;
            }
            if c == '"' {
                self.lex_string();
                continue;
            }
            self.lex_punct();
        }
        self.toks.push(Token { tok: Tok::Eof, span: Span::new(self.pos, self.pos) });
        (self.toks, self.issues)
    }

    fn lex_number(&mut self) {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.bump();
            } else {
                break;
            }
        }
        let text = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
        let value: i64 = text.parse().unwrap_or(0);
        self.toks.push(Token { tok: Tok::Int(value), span: Span::new(start, self.pos) });
    }

    fn lex_ident(&mut self, upper: bool) {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if is_ident_cont(c) || c == '\'' {
                self.bump();
            } else {
                break;
            }
        }
        let text = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
        let tok = if upper { Tok::Upper(text) } else { Tok::Ident(text) };
        self.toks.push(Token { tok, span: Span::new(start, self.pos) });
    }

    fn lex_string(&mut self) {
        let start = self.pos;
        self.bump();
        let mut value = String::new();
        let mut terminated = false;
        while let Some(c) = self.peek() {
            match c {
                '"' => {
                    self.bump();
                    terminated = true;
                    break;
                }
                '\\' => {
                    self.bump();
                    if let Some(e) = self.peek() {
                        match e {
                            'n' => value.push('\n'),
                            't' => value.push('\t'),
                            '\\' => value.push('\\'),
                            '"' => value.push('"'),
                            other => value.push(other),
                        }
                        self.bump();
                    }
                }
                _ => {
                    value.push(c);
                    self.bump();
                }
            }
        }
        if !terminated {
            self.issues.push(ParseIssue {
                message: "unterminated string literal".into(),
                span: Span::new(start, self.pos),
            });
        }
        self.toks.push(Token { tok: Tok::Str(value), span: Span::new(start, self.pos) });
    }

    fn lex_punct(&mut self) {
        let start = self.pos;
        let two: String = self.src[self.pos..]
            .iter()
            .take(2)
            .map(|b| *b as char)
            .collect();
        let two_ops = ["->", "==", "!=", "<=", ">=", "&&", "||", "::", "{|"];
        if two_ops.contains(&two.as_str()) {
            self.pos += 2;
            self.toks.push(Token { tok: Tok::Punct(two), span: Span::new(start, self.pos) });
            return;
        }
        let c = self.bump().unwrap();
        self.toks.push(Token {
            tok: Tok::Punct(c.to_string()),
            span: Span::new(start, self.pos),
        });
    }
}

struct Parser {
    toks: Vec<Token>,
    idx: usize,
    issues: Vec<ParseIssue>,
}

impl Parser {
    fn cur(&self) -> &Token {
        &self.toks[self.idx]
    }
    fn span(&self) -> Span {
        self.cur().span
    }
    fn at(&self, s: &str) -> bool {
        matches!(&self.cur().tok, Tok::Ident(i) | Tok::Punct(i) if i == s)
    }
    fn at_kw(&self, k: &str) -> bool {
        matches!(&self.cur().tok, Tok::Ident(i) if i == k)
    }
    fn at_upper(&self) -> bool {
        matches!(self.cur().tok, Tok::Upper(_))
    }
    fn eat(&mut self, s: &str) -> bool {
        if self.at(s) {
            self.idx += 1;
            true
        } else {
            false
        }
    }
    fn expect(&mut self, s: &str) -> bool {
        if self.eat(s) {
            true
        } else {
            let sp = self.span();
            self.issues
                .push(ParseIssue { message: format!("expected '{s}'"), span: sp });
            false
        }
    }
    fn name(&mut self) -> Option<(String, Span)> {
        if let Tok::Ident(i) = &self.cur().tok {
            let n = i.clone();
            if KEYWORDS.contains(&n.as_str()) {
                self.issues
                    .push(ParseIssue { message: format!("'{n}' is a keyword"), span: self.span() });
                return None;
            }
            let sp = self.span();
            self.idx += 1;
            Some((n, sp))
        } else {
            self.issues
                .push(ParseIssue { message: "expected identifier".into(), span: self.span() });
            None
        }
    }

    fn parse_program(&mut self) -> Option<ExprNode> {
        if matches!(self.cur().tok, Tok::Eof) {
            return None;
        }
        let e = self.parse_expr(&[]);
        if !matches!(self.cur().tok, Tok::Eof) {
            let sp = self.span();
            self.issues
                .push(ParseIssue { message: "unexpected trailing tokens".into(), span: sp });
        }
        e
    }

    /// `stops` lists keywords/punctuators that terminate this expression
    /// (used for let bodies and arm bodies).
    fn parse_expr(&mut self, stops: &[&str]) -> Option<ExprNode> {
        self.parse_let(stops)
    }

    fn stopped(&self, stops: &[&str]) -> bool {
        match &self.cur().tok {
            Tok::Eof => true,
            Tok::Ident(i) => stops.contains(&i.as_str()),
            Tok::Punct(p) => stops.contains(&p.as_str()),
            _ => false,
        }
    }

    fn parse_let(&mut self, stops: &[&str]) -> Option<ExprNode> {
        if self.at_kw("let") {
            let start = self.span().start;
            self.idx += 1;
            let rec = self.eat("rec");
            let (name, _) = self.name()?;
            self.expect("=");
            let value = self.parse_expr(&[])?;
            self.expect("in");
            let inner_stops: Vec<String> =
                stops.iter().map(|s| (*s).to_string()).collect();
            let body = self.parse_expr(stops)?;
            let span = Span::new(start, body.span.end);
            let _ = inner_stops;
            return Some(node(Expr::Let {
                rec,
                name,
                value: Box::new(value),
                body: Box::new(body),
            }, span));
        }
        self.parse_fun(stops)
    }

    fn parse_fun(&mut self, stops: &[&str]) -> Option<ExprNode> {
        if self.at_kw("fun") {
            let start = self.span().start;
            self.idx += 1;
            let mut params = Vec::new();
            loop {
                if self.at("->") || self.stopped(stops) {
                    break;
                }
                if let Some((n, sp)) = self.name() {
                    params.push(Param { name: n, span: sp });
                } else {
                    break;
                }
            }
            if params.is_empty() {
                self.issues
                    .push(ParseIssue { message: "function needs at least one parameter".into(), span: self.span() });
            }
            self.expect("->");
            let body = self.parse_expr(stops)?;
            let span = Span::new(start, body.span.end);
            return Some(node(Expr::Fun { params, body: Box::new(body) }, span));
        }
        self.parse_if(stops)
    }

    fn parse_if(&mut self, stops: &[&str]) -> Option<ExprNode> {
        if self.at_kw("if") {
            let start = self.span().start;
            self.idx += 1;
            let cond = self.parse_expr(&[])?;
            self.expect("then");
            let then_e = self.parse_expr(&[])?;
            self.expect("else");
            let else_e = self.parse_expr(stops)?;
            let span = Span::new(start, else_e.span.end);
            return Some(node(
                Expr::If {
                    cond: Box::new(cond),
                    then_e: Box::new(then_e),
                    else_e: Box::new(else_e),
                },
                span,
            ));
        }
        self.parse_match(stops)
    }

    fn parse_match(&mut self, stops: &[&str]) -> Option<ExprNode> {
        if self.at_kw("match") {
            let start = self.span().start;
            self.idx += 1;
            let scrut = self.parse_expr(&[])?;
            self.expect("with");
            let mut arms = Vec::new();
            loop {
                if self.stopped(stops) || matches!(self.cur().tok, Tok::Eof) {
                    break;
                }
                self.eat("|");
                if self.stopped(stops) || matches!(self.cur().tok, Tok::Eof) {
                    break;
                }
                let arm = self.parse_arm(stops)?;
                arms.push(arm);
            }
            if arms.is_empty() {
                self.issues
                    .push(ParseIssue { message: "match needs at least one arm".into(), span: self.span() });
            }
            let end = arms.last().map(|a| a.span.end).unwrap_or(start);
            return Some(node(Expr::Match { scrut: Box::new(scrut), arms }, Span::new(start, end)));
        }
        self.parse_binop(stops, 0)
    }

    fn parse_arm(&mut self, stops: &[&str]) -> Option<Arm> {
        let start = self.span().start;
        let pat = self.parse_pattern()?;
        self.expect("->");
        let body = self.parse_expr(stops)?;
        let end = body.span.end;
        Some(Arm { pat, body, span: Span::new(start, end) })
    }

    fn parse_pattern(&mut self) -> Option<Pattern> {
        let sp = self.span();
        if self.eat("_") {
            return Some(Pattern::Wild);
        }
        if matches!(self.cur().tok, Tok::Upper(_)) {
            if let Tok::Upper(label) = self.cur().tok.clone() {
                let label_span = self.span();
                self.idx += 1;
                let binding = if let Some((n, _)) = self.name() { Some(n) } else { None };
                return Some(Pattern::Tag(label, binding, label_span));
            }
        }
        if let Some((n, _)) = self.name() {
            return Some(Pattern::Name(n));
        }
        let _ = sp;
        None
    }
}

fn prec(op: &str) -> Option<u8> {
    Some(match op {
        "||" => 1,
        "&&" => 2,
        "==" | "!=" => 3,
        "<" | ">" | "<=" | ">=" => 4,
        "+" | "-" => 5,
        "*" | "/" => 6,
        _ => return None,
    })
}

impl Parser {
    fn parse_binop(&mut self, stops: &[&str], min_prec: u8) -> Option<ExprNode> {
        let mut left = self.parse_postfix(stops)?;
        loop {
            if self.stopped(stops) {
                break;
            }
            let op = match &self.cur().tok {
                Tok::Punct(p) => p.clone(),
                _ => break,
            };
            let p = match prec(&op) {
                Some(p) if p >= min_prec => p,
                _ => break,
            };
            let op_span = self.span();
            self.idx += 1;
            let right = self.parse_binop(stops, p + 1)?;
            let span = Span::new(left.span.start, right.span.end);
            left = node(
                Expr::BinOp { op, left: Box::new(left), right: Box::new(right), op_span },
                span,
            );
        }
        Some(left)
    }

    fn parse_postfix(&mut self, stops: &[&str]) -> Option<ExprNode> {
        let mut e = self.parse_atom(stops)?;
        loop {
            if self.stopped(stops) {
                break;
            }
            if self.eat(".") {
                let label_span = self.span();
                if let Tok::Ident(l) = self.cur().tok.clone() {
                    self.idx += 1;
                    let span = Span::new(e.span.start, label_span.end);
                    e = node(Expr::Field { object: Box::new(e), label: l }, span);
                    continue;
                } else {
                    self.issues.push(ParseIssue {
                        message: "expected field label after '.'".into(),
                        span: label_span,
                    });
                    break;
                }
            }
            // application: atom followed by an atom-starter
            if self.atom_start() {
                let arg = self.parse_atom(stops)?;
                let span = Span::new(e.span.start, arg.span.end);
                e = node(Expr::App { fun: Box::new(e), arg: Box::new(arg) }, span);
                continue;
            }
            break;
        }
        Some(e)
    }

    fn atom_start(&self) -> bool {
        match &self.cur().tok {
            Tok::Int(_) | Tok::Str(_) | Tok::Upper(_) => true,
            Tok::Ident(i) => {
                !["let", "rec", "in", "fun", "if", "then", "else", "match", "with"].contains(&i.as_str())
            }
            Tok::Punct(p) => matches!(p.as_str(), "(" | "{" | "{|"),
            _ => false,
        }
    }

    fn parse_atom(&mut self, stops: &[&str]) -> Option<ExprNode> {
        let _ = stops;
        let sp = self.span();
        match self.cur().tok.clone() {
            Tok::Int(i) => {
                self.idx += 1;
                Some(node(Expr::Lit(Literal::Int(i)), sp))
            }
            Tok::Str(s) => {
                self.idx += 1;
                Some(node(Expr::Lit(Literal::Str(s)), sp))
            }
            Tok::Ident(i) if i == "true" || i == "false" => {
                self.idx += 1;
                Some(node(Expr::Lit(Literal::Bool(i == "true")), sp))
            }
            Tok::Ident(i) => {
                self.idx += 1;
                let mut node_e = node(Expr::Var(i.clone()), sp);
                node_e.local_name = Some(i);
                Some(node_e)
            }
            Tok::Upper(label) => {
                self.idx += 1;
                let payload = if self.eat("(") {
                    let p = self.parse_expr(&[])?;
                    self.expect(")");
                    Some(Box::new(p))
                } else {
                    None
                };
                let end = payload.as_ref().map(|p| p.span.end).unwrap_or(sp.end);
                Some(node(Expr::Tag { label, payload }, Span::new(sp.start, end)))
            }
            Tok::Punct(p) if p == "(" => {
                self.idx += 1;
                let e = self.parse_expr(&[])?;
                self.expect(")");
                Some(e)
            }
            Tok::Punct(p) if p == "{" || p == "{|" => {
                let open = p == "{|";
                self.idx += 1;
                let mut fields = Vec::new();
                if self.eat("}") {
                    return Some(node(
                        Expr::Record { fields, open },
                        Span::new(sp.start, self.toks[self.idx - 1].span.end),
                    ));
                }
                loop {
                    if let Tok::Ident(l) = self.cur().tok.clone() {
                        self.idx += 1;
                        self.expect("=");
                        let v = self.parse_expr(&[])?;
                        fields.push((l, v));
                    } else {
                        self.issues.push(ParseIssue {
                            message: "expected field label".into(),
                            span: self.span(),
                        });
                        break;
                    }
                    if self.eat(",") {
                        if self.at("}") {
                            break;
                        }
                        continue;
                    }
                    break;
                }
                let close_span = self.span();
                if self.eat("}") {
                    Some(node(Expr::Record { fields, open }, Span::new(sp.start, close_span.end)))
                } else {
                    self.issues
                        .push(ParseIssue { message: "expected '}'".into(), span: close_span });
                    Some(node(Expr::Record { fields, open }, Span::new(sp.start, close_span.start)))
                }
            }
            _ => {
                self.issues
                    .push(ParseIssue { message: "unexpected token".into(), span: sp });
                None
            }
        }
    }
}

fn node(expr: Expr, span: Span) -> ExprNode {
    ExprNode { id: NodeId(0), expr, span, local_name: None }
}

/// Parse source text into a program, assigning stable node ids.
pub fn parse(source: &str) -> Program {
    let (toks, mut issues) = Lexer::new(source).lex();
    let mut parser = Parser { toks, idx: 0, issues: Vec::new() };
    let root = parser.parse_program();
    issues.append(&mut parser.issues);
    let mut program = Program { root, source: source.to_string(), issues };
    assign_ids(&mut program);
    program
}
