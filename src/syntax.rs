//! Lexer, AST and parser for the small HM-style language.
//!
//! Every AST node carries a byte range (`Span`) and a stable path id
//! (e.g. `let.1.rhs.app.arg.0`) so that re-parsing an edited program reuses
//! unaffected nodes even when surrounding bindings were renamed.

use serde::Serialize;

pub type NodeId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

// ---------------------------------------------------------------------------
// Tokenizer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Str(String),
    Ident(String),
    Cap(String), // capitalized constructor name
    TVar(String), // 'a
    // keywords
    Let, Rec, Fn, In, If, Then, Else, True, False, Match, With,
    // punctuation / operators
    Eq, EqEq, Plus, Arrow, FatArrow, Backslash,
    LParen, RParen, LBrace, RBrace,
    Comma, Colon, ColonColon, Dot, Pipe, Underscore,
    DotDot, LVAngle, RVAngle,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, String> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i] as char;
        match c {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '-' if b.get(i + 1) == Some(&b'>') => {
                out.push(Token { tok: Tok::Arrow, span: Span::new(i, i + 2) });
                i += 2;
            }
            '=' if b.get(i + 1) == Some(&b'>') => {
                out.push(Token { tok: Tok::FatArrow, span: Span::new(i, i + 2) });
                i += 2;
            }
            '=' if b.get(i + 1) == Some(&b'=') => {
                out.push(Token { tok: Tok::EqEq, span: Span::new(i, i + 2) });
                i += 2;
            }
            '=' => { out.push(Token { tok: Tok::Eq, span: Span::new(i, i + 1) }); i += 1; }
            '+' => { out.push(Token { tok: Tok::Plus, span: Span::new(i, i + 1) }); i += 1; }
            '\\' => { out.push(Token { tok: Tok::Backslash, span: Span::new(i, i + 1) }); i += 1; }
            '(' => { out.push(Token { tok: Tok::LParen, span: Span::new(i, i + 1) }); i += 1; }
            ')' => { out.push(Token { tok: Tok::RParen, span: Span::new(i, i + 1) }); i += 1; }
            '{' => { out.push(Token { tok: Tok::LBrace, span: Span::new(i, i + 1) }); i += 1; }
            '}' => { out.push(Token { tok: Tok::RBrace, span: Span::new(i, i + 1) }); i += 1; }
            ',' => { out.push(Token { tok: Tok::Comma, span: Span::new(i, i + 1) }); i += 1; }
            ':' if b.get(i + 1) == Some(&b':') => {
                out.push(Token { tok: Tok::ColonColon, span: Span::new(i, i + 2) });
                i += 2;
            }
            '\'' if {
                let nb = b.get(i + 1).copied().unwrap_or(0);
                (nb as char).is_ascii_alphabetic() || nb == b'_'
            } => {
                let st = i;
                i += 1;
                let ns = i;
                while i < b.len() && ((b[i] as char).is_ascii_alphanumeric() || b[i] == b'_') { i += 1; }
                out.push(Token { tok: Tok::TVar(src[ns..i].to_string()), span: Span::new(st, i) });
            }
            ':' => { out.push(Token { tok: Tok::Colon, span: Span::new(i, i + 1) }); i += 1; }
            '.' => { out.push(Token { tok: Tok::Dot, span: Span::new(i, i + 1) }); i += 1; }
            '|' if b.get(i + 1) == Some(&b'>') => {
                out.push(Token { tok: Tok::RVAngle, span: Span::new(i, i + 2) });
                i += 2;
            }
            '|' => { out.push(Token { tok: Tok::Pipe, span: Span::new(i, i + 1) }); i += 1; }
            '<' if b.get(i + 1) == Some(&b'|') => {
                out.push(Token { tok: Tok::LVAngle, span: Span::new(i, i + 2) });
                i += 2;
            }
            '.' if b.get(i + 1) == Some(&b'.') => {
                out.push(Token { tok: Tok::DotDot, span: Span::new(i, i + 2) });
                i += 2;
            }
            '0'..='9' => {
                let s = i;
                while i < b.len() && b[i].is_ascii_digit() { i += 1; }
                let n: i64 = src[s..i].parse().map_err(|_| format!("bad int at {s}"))?;
                out.push(Token { tok: Tok::Int(n), span: Span::new(s, i) });
            }
            '"' => {
                let s = i;
                i += 1;
                let mut val = String::new();
                while i < b.len() && b[i] != b'"' {
                    if b[i] == b'\\' && i + 1 < b.len() {
                        let e = b[i + 1] as char;
                        val.push(match e { 'n' => '\n', 't' => '\t', '"' => '"', '\\' => '\\', _ => e });
                        i += 2;
                    } else {
                        val.push(b[i] as char);
                        i += 1;
                    }
                }
                if i >= b.len() { return Err(format!("unterminated string at {s}")); }
                i += 1;
                out.push(Token { tok: Tok::Str(val), span: Span::new(s, i) });
            }
            c if c.is_ascii_alphabetic() || c == '_' => {
                let s = i;
                while i < b.len()
                    && ((b[i] as char).is_ascii_alphanumeric() || b[i] == b'_')
                {
                    i += 1;
                }
                let w = &src[s..i];
                let tok = match w {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "fn" => Tok::Fn,
                    "in" => Tok::In,
                    "if" => Tok::If,
                    "then" => Tok::Then,
                    "else" => Tok::Else,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "match" => Tok::Match,
                    "with" => Tok::With,
                    "_" => Tok::Underscore,
                    _ => {
                        let first = w.chars().next().unwrap();
                        if first.is_ascii_uppercase() {
                            Tok::Cap(w.to_string())
                        } else {
                            Tok::Ident(w.to_string())
                        }
                    }
                };
                out.push(Token { tok, span: Span::new(s, i) });
            }
            _ => return Err(format!("unexpected character {c:?} at {i}")),
        }
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}

// ---------------------------------------------------------------------------
// AST
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize)]
pub struct Program {
    pub body: Expr,
}

#[derive(Debug, Clone, Serialize)]
pub struct Param {
    pub name: String,
    pub ann: Option<Type>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    Str(String),
    Unit,
}

#[derive(Debug, Clone, Serialize)]
pub struct FieldLit {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, Serialize)]
pub struct Arm {
    pub pat: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone, Serialize)]
pub enum Pattern {
    PVar(String, Option<Type>, Span),
    PWild(Span),
    PTag(String, Option<Box<Pattern>>, Span),
}

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    Lit(Lit, NodeId, Span),
    Var(String, NodeId, Span),
    Lambda {
        param: Param,
        body: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    App {
        fun: Box<Expr>,
        arg: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    Bin {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    Let {
        rec: bool,
        name: String,
        ann: Option<Type>,
        value: Box<Expr>,
        body: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    If {
        cond: Box<Expr>,
        then_e: Box<Expr>,
        else_e: Box<Expr>,
        id: NodeId,
        span: Span,
    },
    Record {
        fields: Vec<FieldLit>,
        id: NodeId,
        span: Span,
    },
    Field {
        record: Box<Expr>,
        name: String,
        id: NodeId,
        span: Span,
    },
    Tag {
        tag: String,
        payload: Option<Box<Expr>>,
        id: NodeId,
        span: Span,
    },
    Match {
        scrut: Box<Expr>,
        arms: Vec<Arm>,
        id: NodeId,
        span: Span,
    },
}

impl Expr {
    pub fn id(&self) -> &str {
        match self {
            Expr::Lit(_, i, _)
            | Expr::Var(_, i, _)
            | Expr::Lambda { id: i, .. }
            | Expr::App { id: i, .. }
            | Expr::Bin { id: i, .. }
            | Expr::Let { id: i, .. }
            | Expr::If { id: i, .. }
            | Expr::Record { id: i, .. }
            | Expr::Field { id: i, .. }
            | Expr::Tag { id: i, .. }
            | Expr::Match { id: i, .. } => i,
        }
    }
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit(_, _, s)
            | Expr::Var(_, _, s)
            | Expr::Lambda { span: s, .. }
            | Expr::App { span: s, .. }
            | Expr::Bin { span: s, .. }
            | Expr::Let { span: s, .. }
            | Expr::If { span: s, .. }
            | Expr::Record { span: s, .. }
            | Expr::Field { span: s, .. }
            | Expr::Tag { span: s, .. }
            | Expr::Match { span: s, .. } => *s,
        }
    }
    pub fn kind(&self) -> &'static str {
        match self {
            Expr::Lit(_, _, _) => "lit",
            Expr::Var(_, _, _) => "var",
            Expr::Lambda { .. } => "lambda",
            Expr::App { .. } => "app",
            Expr::Bin { .. } => "bin",
            Expr::Let { rec: false, .. } => "let",
            Expr::Let { rec: true, .. } => "let-rec",
            Expr::If { .. } => "if",
            Expr::Record { .. } => "record",
            Expr::Field { .. } => "field",
            Expr::Tag { .. } => "tag",
            Expr::Match { .. } => "match",
        }
    }
    pub fn label(&self) -> String {
        match self {
            Expr::Lit(l, _, _) => match l {
                Lit::Int(n) => format!("{n}"),
                Lit::Bool(b) => format!("{b}"),
                Lit::Str(s) => format!("\"{s}\""),
                Lit::Unit => "()".into(),
            },
            Expr::Var(n, _, _) => n.clone(),
            Expr::Lambda { param, .. } => format!("fn {}", param.name),
            Expr::App { .. } => "app".into(),
            Expr::Bin { op, .. } => format!("{op}"),
            Expr::Let { rec, name, .. } => {
                if *rec { format!("let rec {name}") } else { format!("let {name}") }
            }
            Expr::If { .. } => "if".into(),
            Expr::Record { fields, .. } => {
                format!("{{ {} }}", fields.iter().map(|f| f.name.as_str()).collect::<Vec<_>>().join(", "))
            }
            Expr::Field { name, .. } => format!(".{name}"),
            Expr::Tag { tag, .. } => format!(":{tag}"),
            Expr::Match { .. } => "match".into(),
        }
    }
}

// ---------------------------------------------------------------------------
// Type syntax (annotations)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct TypeRowField {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum Type {
    TCon(String, Span),
    TVar(String, Span),
    TArrow(Box<Type>, Box<Type>, Span),
    TRecord(Vec<TypeRowField>, bool /* open */, Span),
    TVariant(Vec<TypeRowField>, bool /* open */, Span),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::TCon(_, s)
            | Type::TVar(_, s)
            | Type::TArrow(_, _, s)
            | Type::TRecord(_, _, s)
            | Type::TVariant(_, _, s) => *s,
        }
    }
    pub fn render(&self) -> String {
        match self {
            Type::TCon(n, _) => n.clone(),
            Type::TVar(n, _) => format!("'{n}"),
            Type::TArrow(a, b, _) => format!("({} -> {})", a.render(), b.render()),
            Type::TRecord(fs, open, _) => {
                let mut s = String::from("{ ");
                for (i, f) in fs.iter().enumerate() {
                    if i > 0 { s.push_str(", "); }
                    s.push_str(&format!("{}: {}", f.name, f.ty.render()));
                }
                if *open {
                    if !fs.is_empty() { s.push_str(", "); }
                    s.push_str("..");
                }
                s.push_str(" }");
                s
            }
            Type::TVariant(fs, open, _) => {
                let mut s = String::from("[ ");
                for (i, f) in fs.iter().enumerate() {
                    if i > 0 { s.push_str(" | "); }
                    s.push_str(&format!(":{} {}", f.name, f.ty.render()));
                }
                if *open {
                    if !fs.is_empty() { s.push_str(" | "); }
                    s.push_str("..");
                }
                s.push_str(" ]");
                s
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct Parser {
    toks: Vec<Token>,
    pos: usize,
}

pub fn parse(src: &str) -> Result<Program, String> {
    let toks = lex(src)?;
    let mut p = Parser { toks, pos: 0 };
    let body = p.parse_expr("root")?;
    if !matches!(p.peek().tok, Tok::Eof) {
        let t = p.peek();
        return Err(format!("unexpected token at {}..{}", t.span.start, t.span.end));
    }
    Ok(Program { body })
}

impl Parser {
    fn peek(&self) -> &Token {
        &self.toks[self.pos]
    }
    fn peek2(&self) -> &Token {
        &self.toks[(self.pos + 1).min(self.toks.len() - 1)]
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos < self.toks.len() - 1 { self.pos += 1; }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, String> {
        if std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(&t) {
            Ok(self.bump())
        } else {
            let s = self.peek().span;
            Err(format!("expected {what} at {}..{}, found {:?}", s.start, s.end, self.peek().tok))
        }
    }
    fn at_ident(&self) -> Option<String> {
        if let Tok::Ident(n) = &self.peek().tok { Some(n.clone()) } else { None }
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        let lhs = self.parse_type_atom()?;
        if matches!(self.peek().tok, Tok::Arrow) {
            self.bump();
            let rhs = self.parse_type()?;
            let sp = Span::new(lhs.span().start, rhs.span().end);
            Ok(Type::TArrow(Box::new(lhs), Box::new(rhs), sp))
        } else {
            Ok(lhs)
        }
    }

    fn parse_type_atom(&mut self) -> Result<Type, String> {
        let t = self.peek().clone();
        match &t.tok {
            Tok::Ident(n) | Tok::Cap(n) => {
                let n = n.clone();
                self.bump();
                Ok(Type::TCon(n, t.span))
            }
            Tok::TVar(n) => {
                let n = n.clone();
                self.bump();
                Ok(Type::TVar(n, t.span))
            }
            Tok::LParen => {
                self.bump();
                let inner = self.parse_type()?;
                self.expect(Tok::RParen, "')'")?;
                Ok(inner)
            }
            Tok::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                let mut open = false;
                loop {
                    if matches!(self.peek().tok, Tok::RBrace) { break; }
                    if matches!(self.peek().tok, Tok::DotDot) {
                        self.bump();
                        open = true;
                        break;
                    }
                    let name = self.at_ident()
                        .ok_or_else(|| format!("expected field name at {}", self.peek().span.start))?;
                    self.bump();
                    self.expect(Tok::Colon, "':'")?;
                    let ty = self.parse_type()?;
                    fields.push(TypeRowField { name, ty });
                    if !self.eat(&Tok::Comma) { break; }
                }
                self.expect(Tok::RBrace, "'}'")?;
                Ok(Type::TRecord(fields, open, t.span))
            }
            Tok::LVAngle => {
                self.bump();
                let mut fields = Vec::new();
                let mut open = false;
                loop {
                    if matches!(self.peek().tok, Tok::RVAngle) { break; }
                    if matches!(self.peek().tok, Tok::Pipe) { self.bump(); }
                    if matches!(self.peek().tok, Tok::DotDot) {
                        self.bump();
                        open = true;
                        break;
                    }
                    self.expect(Tok::Colon, "':' before tag")?;
                    let tag = if let Tok::Cap(c) = &self.peek().tok {
                        let c = c.clone();
                        self.bump();
                        c
                    } else {
                        return Err(format!("expected capitalized tag at {}", self.peek().span.start));
                    };
                    let ty = self.parse_type_atom()?;
                    fields.push(TypeRowField { name: tag, ty });
                }
                self.expect(Tok::RVAngle, "'|>'")?;
                Ok(Type::TVariant(fields, open, t.span))
            }
            _ => Err(format!(
                "expected type at {}..{}, found {:?}",
                t.span.start, t.span.end, t.tok
            )),
        }
    }

    fn parse_param(&mut self) -> Result<Param, String> {
        let t = self.peek().clone();
        let name = if let Tok::Underscore = t.tok {
            self.bump();
            "_".to_string()
        } else if let Tok::Ident(n) = &t.tok {
            let n = n.clone();
            self.bump();
            n
        } else {
            return Err(format!("expected parameter name at {}", t.span.start));
        };
        let ann = if matches!(self.peek().tok, Tok::Colon) {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };
        Ok(Param { name, ann })
    }

    fn parse_expr(&mut self, path: &str) -> Result<Expr, String> {
        match self.peek().tok {
            Tok::Let => self.parse_let(path),
            Tok::Fn | Tok::Backslash => self.parse_lambda(path),
            Tok::If => self.parse_if(path),
            Tok::Match => self.parse_match(path),
            _ => self.parse_eq(path),
        }
    }

    fn parse_let(&mut self, path: &str) -> Result<Expr, String> {
        let start = self.expect(Tok::Let, "'let'")?.span.start;
        let rec = self.eat(&Tok::Rec);
        let name = self.at_ident()
            .ok_or_else(|| format!("expected binding name at {}", self.peek().span.start))?;
        self.bump();
        let ann = if matches!(self.peek().tok, Tok::Colon) {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(Tok::Eq, "'='")?;
        let value = self.parse_expr(&format!("{path}.value"))?;
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr(&format!("{path}.body"))?;
        let span = Span::new(start, body.span().end);
        Ok(Expr::Let {
            rec,
            name,
            ann,
            value: Box::new(value),
            body: Box::new(body),
            id: path.to_string(),
            span,
        })
    }

    fn parse_lambda(&mut self, path: &str) -> Result<Expr, String> {
        let start = self.bump().span.start;
        let param = self.parse_param()?;
        self.expect(Tok::Arrow, "'->'")?;
        let body = self.parse_expr(&format!("{path}.body"))?;
        let span = Span::new(start, body.span().end);
        Ok(Expr::Lambda {
            param,
            body: Box::new(body),
            id: path.to_string(),
            span,
        })
    }

    fn parse_if(&mut self, path: &str) -> Result<Expr, String> {
        let start = self.expect(Tok::If, "'if'")?.span.start;
        let cond = self.parse_expr(&format!("{path}.cond"))?;
        self.expect(Tok::Then, "'then'")?;
        let then_e = self.parse_expr(&format!("{path}.then"))?;
        self.expect(Tok::Else, "'else'")?;
        let else_e = self.parse_expr(&format!("{path}.else"))?;
        let span = Span::new(start, else_e.span().end);
        Ok(Expr::If {
            cond: Box::new(cond),
            then_e: Box::new(then_e),
            else_e: Box::new(else_e),
            id: path.to_string(),
            span,
        })
    }

    fn parse_match(&mut self, path: &str) -> Result<Expr, String> {
        let start = self.expect(Tok::Match, "'match'")?.span.start;
        let scrut = self.parse_expr(&format!("{path}.scrut"))?;
        self.expect(Tok::With, "'with'")?;
        let mut arms = Vec::new();
        let mut idx = 0;
        loop {
            if matches!(self.peek().tok, Tok::Pipe) { self.bump(); }
            let pat = self.parse_pattern()?;
            self.expect(Tok::FatArrow, "'=>'")?;
            let body = self.parse_expr(&format!("{path}.arm{idx}"))?;
            let pspan = match &pat {
                Pattern::PVar(_, _, s) | Pattern::PWild(s) | Pattern::PTag(_, _, s) => *s,
            };
            let _ = pspan;
            arms.push(Arm { pat, body });
            idx += 1;
            if matches!(self.peek().tok, Tok::Pipe) {
                continue;
            }
            break;
        }
        if arms.is_empty() {
            return Err(format!("match at {start} needs at least one arm"));
        }
        let end = arms.last().unwrap().body.span().end;
        Ok(Expr::Match {
            scrut: Box::new(scrut),
            arms,
            id: path.to_string(),
            span: Span::new(start, end),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        let t = self.peek().clone();
        match t.tok {
            Tok::Underscore => {
                self.bump();
                Ok(Pattern::PWild(t.span))
            }
            Tok::Ident(n) => {
                self.bump();
                let ann = if matches!(self.peek().tok, Tok::Colon) {
                    self.bump();
                    Some(self.parse_type()?)
                } else {
                    None
                };
                Ok(Pattern::PVar(n, ann, t.span))
            }
            Tok::Cap(c) => {
                self.bump();
                let payload = if matches!(self.peek().tok, Tok::Ident(_))
                    || matches!(self.peek().tok, Tok::Underscore)
                    || matches!(self.peek().tok, Tok::Cap(_))
                {
                    Some(Box::new(self.parse_pattern()?))
                } else {
                    None
                };
                Ok(Pattern::PTag(c, payload, t.span))
            }
            _ => Err(format!("expected pattern at {}..{}", t.span.start, t.span.end)),
        }
    }

    fn parse_eq(&mut self, path: &str) -> Result<Expr, String> {
        let mut lhs = self.parse_add(path)?;
        if matches!(self.peek().tok, Tok::EqEq) {
            let sp = self.peek().span;
            self.bump();
            let rhs = self.parse_add(&format!("{path}.rhs"))?;
            let end = rhs.span().end;
            lhs = Expr::Bin {
                op: "==".into(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                id: format!("{path}.eq"),
                span: Span::new(sp.start, end),
            };
        }
        Ok(lhs)
    }

    fn parse_add(&mut self, path: &str) -> Result<Expr, String> {
        let mut lhs = self.parse_app(&format!("{path}.lhs"))?;
        let mut i = 0;
        while matches!(self.peek().tok, Tok::Plus) {
            let sp = self.peek().span;
            self.bump();
            let rhs = self.parse_app(&format!("{path}.r{i}"))?;
            let end = rhs.span().end;
            lhs = Expr::Bin {
                op: "+".into(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                id: format!("{path}.add{i}"),
                span: Span::new(sp.start, end),
            };
            i += 1;
        }
        Ok(lhs)
    }

    fn parse_app(&mut self, path: &str) -> Result<Expr, String> {
        let mut fun = self.parse_atom(&format!("{path}.f0"))?;
        let mut i = 1;
        while self.starts_atom() {
            let arg = self.parse_atom(&format!("{path}.a{i}"))?;
            let span = Span::new(fun.span().start, arg.span().end);
            fun = Expr::App {
                fun: Box::new(fun),
                arg: Box::new(arg),
                id: format!("{path}.app{i}"),
                span,
            };
            i += 1;
        }
        Ok(fun)
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek().tok,
            Tok::Int(_)
                | Tok::True | Tok::False
                | Tok::Str(_)
                | Tok::Ident(_)
                | Tok::Cap(_)
                | Tok::LParen
                | Tok::LBrace
                | Tok::Colon
        )
    }

    fn parse_atom(&mut self, path: &str) -> Result<Expr, String> {
        let t = self.peek().clone();
        let mut e: Expr = match &t.tok {
            Tok::Int(n) => {
                self.bump();
                Expr::Lit(Lit::Int(*n), path.to_string(), t.span)
            }
            Tok::True | Tok::False => {
                self.bump();
                Expr::Lit(Lit::Bool(matches!(t.tok, Tok::True)), path.to_string(), t.span)
            }
            Tok::Str(s) => {
                let s = s.clone();
                self.bump();
                Expr::Lit(Lit::Str(s), path.to_string(), t.span)
            }
            Tok::Ident(n) => {
                let n = n.clone();
                self.bump();
                Expr::Var(n, path.to_string(), t.span)
            }
            Tok::LParen => {
                self.bump();
                if matches!(self.peek().tok, Tok::RParen) {
                    let end = self.bump().span.end;
                    Expr::Lit(Lit::Unit, path.to_string(), Span::new(t.span.start, end))
                } else {
                    let inner = self.parse_expr(path)?;
                    self.expect(Tok::RParen, "')'")?;
                    inner
                }
            }
            Tok::LBrace => self.parse_record(path, t.span.start)?,
            Tok::Colon => {
                self.bump();
                let tag = if let Tok::Cap(c) = &self.peek().tok {
                    let c = c.clone();
                    self.bump();
                    c
                } else {
                    return Err(format!("expected capitalized tag after ':' at {}", t.span.start));
                };
                let payload = if self.starts_atom() {
                    Some(Box::new(self.parse_atom(&format!("{path}.payload"))?))
                } else {
                    None
                };
                let end = payload.as_ref().map(|p| p.span().end).unwrap_or(t.span.end);
                Expr::Tag { tag, payload, id: path.to_string(), span: Span::new(t.span.start, end) }
            }
            _ => return Err(format!("expected expression at {}..{}, found {:?}", t.span.start, t.span.end, t.tok)),
        };
        // field access chain
        let mut fi = 0;
        while matches!(self.peek().tok, Tok::Dot) {
            let dot = self.bump().span;
            let name = self.at_ident()
                .ok_or_else(|| format!("expected field name after '.' at {}", self.peek().span.start))?;
            self.bump();
            let span = Span::new(e.span().start, self.toks.get(self.pos - 1).map(|x| x.span.end).unwrap_or(dot.end));
            e = Expr::Field {
                record: Box::new(e),
                name,
                id: format!("{path}.field{fi}"),
                span,
            };
            fi += 1;
        }
        Ok(e)
    }

    fn parse_record(&mut self, path: &str, start: usize) -> Result<Expr, String> {
        self.expect(Tok::LBrace, "'{'")?;
        let mut fields = Vec::new();
        let mut seen = std::collections::HashSet::new();
        let mut end = start + 1;
        loop {
            if matches!(self.peek().tok, Tok::RBrace) {
                end = self.bump().span.end;
                break;
            }
            let name = self.at_ident()
                .ok_or_else(|| format!("expected field name at {}", self.peek().span.start))?;
            self.bump();
            if !seen.insert(name.clone()) {
                return Err(format!("duplicate field '{name}' in record"));
            }
            self.expect(Tok::Eq, "'=' in record literal")?;
            let value = self.parse_expr(&format!("{path}.f{}", fields.len()))?;
            end = value.span().end;
            fields.push(FieldLit { name, value });
            if !self.eat(&Tok::Comma) {
                end = self.expect(Tok::RBrace, "'}'")?.span.end;
                break;
            }
        }
        Ok(Expr::Record {
            fields,
            id: path.to_string(),
            span: Span::new(start, end),
        })
    }

}
