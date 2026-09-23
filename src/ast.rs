use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
    pub id: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Binding {
    pub name: String,
    pub rec: bool,
    pub value: Box<Expr>,
    pub body: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Field {
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MatchArm {
    pub tag: Option<String>,
    pub binding: Option<String>,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ExprK {
    Int(i64),
    Bool(bool),
    Var(String),
    Abs {
        param: String,
        body: Box<Expr>,
    },
    App {
        callee: Box<Expr>,
        arg: Box<Expr>,
    },
    Let(Binding),
    Record(Vec<Field>),
    Extend {
        base: Box<Expr>,
        field: String,
        value: Box<Expr>,
    },
    Proj {
        record: Box<Expr>,
        field: String,
    },
    Variant {
        tag: String,
        payload: Option<Box<Expr>>,
    },
    Match {
        scrut: Box<Expr>,
        arms: Vec<MatchArm>,
    },
}

pub type Expr = Spanned<ExprK>;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Program {
    pub expr: Expr,
    pub bindings: HashMap<String, Span>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokK {
    Int,
    Name,
    KwLet,
    KwRec,
    KwIn,
    KwFn,
    KwMatch,
    KwWith,
    KwTrue,
    KwFalse,
    KwHash,
    Arrow,
    Equal,
    Backslash,
    Dot,
    Comma,
    Semicolon,
    Pipe,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Eof,
}

#[derive(Debug, Clone)]
struct Tok {
    kind: TokK,
    text: String,
    span: Span,
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn tokenize(mut self) -> Result<Vec<Tok>, ParseError> {
        let mut out = Vec::new();
        loop {
            while self.pos < self.src.len() && self.src[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }
            if self.pos >= self.src.len() {
                out.push(Tok { kind: TokK::Eof, text: String::new(), span: Span(self.pos, self.pos) });
                return Ok(out);
            }
            let start = self.pos;
            let c = self.src[self.pos] as char;
            let kind = if c.is_ascii_digit() {
                while self.pos < self.src.len() && self.src[self.pos].is_ascii_digit() {
                    self.pos += 1;
                }
                TokK::Int
            } else if c == '_' || c.is_ascii_alphabetic() {
                while self.pos < self.src.len() {
                    let x = self.src[self.pos] as char;
                    if x == '_' || x.is_ascii_alphanumeric() {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                match &self.src[start..self.pos] {
                    b"let" => TokK::KwLet,
                    b"rec" => TokK::KwRec,
                    b"in" => TokK::KwIn,
                    b"fn" => TokK::KwFn,
                    b"match" => TokK::KwMatch,
                    b"with" => TokK::KwWith,
                    b"true" => TokK::KwTrue,
                    b"false" => TokK::KwFalse,
                    _ => TokK::Name,
                }
            } else {
                self.pos += 1;
                match c {
                    '#' => TokK::KwHash,
                    '\\' => TokK::Backslash,
                    '.' => TokK::Dot,
                    ',' => TokK::Comma,
                    ';' => TokK::Semicolon,
                    '|' => TokK::Pipe,
                    '=' => TokK::Equal,
                    '(' => TokK::LParen,
                    ')' => TokK::RParen,
                    '{' => TokK::LBrace,
                    '}' => TokK::RBrace,
                    '[' => TokK::LBracket,
                    ']' => TokK::RBracket,
                    '-' if self.pos < self.src.len() && self.src[self.pos] == b'>' => {
                        self.pos += 1;
                        TokK::Arrow
                    }
                    _ => {
                        return Err(ParseError {
                            message: format!("unexpected character {c:?}"),
                            span: Span(start, self.pos),
                        })
                    }
                }
            };
            out.push(Tok {
                kind,
                text: String::from_utf8_lossy(&self.src[start..self.pos]).into_owned(),
                span: Span(start, self.pos),
            });
        }
    }
}

struct Parser {
    toks: Vec<Tok>,
    idx: usize,
}

impl Parser {
    fn peek(&self) -> TokK {
        self.toks[self.idx].kind
    }

    fn at2(&self, a: TokK, b: TokK) -> bool {
        self.peek() == a && self.idx + 1 < self.toks.len() && self.toks[self.idx + 1].kind == b
    }

    fn span(&self) -> Span {
        self.toks[self.idx].span
    }

    fn bump(&mut self) -> Tok {
        let t = self.toks[self.idx].clone();
        if self.idx + 1 < self.toks.len() {
            self.idx += 1;
        }
        t
    }

    fn eat(&mut self, kind: TokK) -> bool {
        if self.peek() == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokK, what: &str) -> Result<Tok, ParseError> {
        if self.peek() == kind {
            Ok(self.bump())
        } else {
            Err(ParseError { message: format!("expected {what}"), span: self.span() })
        }
    }

    fn make(&self, node: ExprK, span: Span, id: String) -> Expr {
        Spanned { node, span, id }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let expr = self.parse_expr("root")?;
        self.eat(TokK::Semicolon);
        self.expect(TokK::Eof, "end of input")?;
        Ok(Program { expr, bindings: HashMap::new() })
    }

    fn parse_expr(&mut self, path: &str) -> Result<Expr, ParseError> {
        let start = self.span().0;
        if self.peek() == TokK::KwLet {
            return self.parse_let(path, start);
        }
        if self.peek() == TokK::KwFn || self.peek() == TokK::Backslash {
            return self.parse_abs(path, start);
        }
        if self.peek() == TokK::KwMatch {
            return self.parse_match(path, start);
        }
        self.parse_app(path)
    }

    fn parse_let(&mut self, path: &str, start: usize) -> Result<Expr, ParseError> {
        self.bump();
        let rec = self.eat(TokK::KwRec);
        let name_tok = self.expect(TokK::Name, "binding name")?;
        self.expect(TokK::Equal, "'='")?;
        let value = self.parse_expr(&format!("{path}/binding:{}/value", name_tok.text))?;
        let body = if self.eat(TokK::KwIn) {
            Some(Box::new(self.parse_expr(&format!("{path}/binding:{}/body", name_tok.text))?))
        } else if self.eat(TokK::Semicolon) {
            Some(Box::new(self.parse_expr(&format!("{path}/binding:{}/body", name_tok.text))?))
        } else if self.peek() == TokK::Eof {
            None
        } else {
            return Err(ParseError { message: "expected 'in' or ';' after let value".into(), span: self.span() });
        };
        let end = body.as_ref().map_or_else(|| value.span.1, |b| b.span.1);
        Ok(self.make(
            ExprK::Let(Binding {
                name: name_tok.text,
                rec,
                value: Box::new(value),
                body,
                span: Span(start, end),
            }),
            Span(start, end),
            format!("{path}/let"),
        ))
    }

    fn parse_abs(&mut self, path: &str, start: usize) -> Result<Expr, ParseError> {
        self.bump();
        let param = self.expect(TokK::Name, "parameter name")?;
        self.expect(TokK::Arrow, "'->'")?;
        let body = self.parse_expr(&format!("{path}/param:{}/body", param.text))?;
        let span = Span(start, body.span.1);
        Ok(self.make(
            ExprK::Abs { param: param.text, body: Box::new(body) },
            span,
            format!("{path}/abs"),
        ))
    }

    fn parse_match(&mut self, path: &str, start: usize) -> Result<Expr, ParseError> {
        self.bump();
        let scrut = self.parse_app(&format!("{path}/scrutinee"))?;
        self.expect(TokK::KwWith, "'with'")?;
        self.eat(TokK::Pipe);
        let mut arms = Vec::new();
        loop {
            if self.peek() == TokK::Eof || self.peek() == TokK::RBrace || self.peek() == TokK::RParen {
                break;
            }
            let arm_start = self.span().0;
            let (tag, binding, key) = if self.peek() == TokK::Name && self.toks[self.idx].text == "_" {
                self.bump();
                (None, None, "*".to_string())
            } else {
                self.expect(TokK::KwHash, "'#' tag")?;
                let tag = self.expect(TokK::Name, "variant tag")?;
                let binding = if self.peek() == TokK::Name {
                    Some(self.bump().text)
                } else {
                    None
                };
                (Some(tag.text.clone()), binding, format!("tag:{}", tag.text))
            };
            self.expect(TokK::Arrow, "'->'")?;
            let body = self.parse_expr(&format!("{path}/arm:{key}/body"))?;
            let span = Span(arm_start, body.span.1);
            arms.push(MatchArm { tag, binding, body, span });
            if !self.eat(TokK::Pipe) {
                break;
            }
        }
        if arms.is_empty() {
            return Err(ParseError { message: "match needs at least one arm".into(), span: self.span() });
        }
        let end = arms.last().unwrap().span.1;
        Ok(self.make(
            ExprK::Match { scrut: Box::new(scrut), arms },
            Span(start, end),
            format!("{path}/match"),
        ))
    }

    fn atom_starts(&self) -> bool {
        matches!(
            self.peek(),
            TokK::Int
                | TokK::Name
                | TokK::KwTrue
                | TokK::KwFalse
                | TokK::KwFn
                | TokK::Backslash
                | TokK::KwMatch
                | TokK::KwHash
                | TokK::LParen
                | TokK::LBrace
        )
    }

    fn parse_app(&mut self, path: &str) -> Result<Expr, ParseError> {
        let mut head = self.parse_postfix(&format!("{path}/app/arg:0"))?;
        let mut n = 1;
        while self.atom_starts() {
            let arg = self.parse_postfix(&format!("{path}/app/arg:{n}"))?;
            let span = Span(head.span.0, arg.span.1);
            head = self.make(
                ExprK::App { callee: Box::new(head), arg: Box::new(arg) },
                span,
                format!("{path}/app"),
            );
            n += 1;
        }
        Ok(head)
    }

    fn parse_postfix(&mut self, path: &str) -> Result<Expr, ParseError> {
        let mut e = self.parse_atom(path)?;
        while self.peek() == TokK::Dot {
            let start = e.span.0;
            self.bump();
            let field = self.expect(TokK::Name, "field name")?;
            let span = Span(start, field.span.1);
            e = self.make(
                ExprK::Proj { record: Box::new(e), field: field.text.clone() },
                span,
                format!("root-project-{field}", field = field.text),
            );
        }
        Ok(e)
    }

    fn parse_atom(&mut self, path: &str) -> Result<Expr, ParseError> {
        let tok = self.toks[self.idx].clone();
        match tok.kind {
            TokK::Int => {
                self.bump();
                let n = tok.text.parse().map_err(|_| ParseError {
                    message: "invalid integer".into(),
                    span: tok.span,
                })?;
                Ok(self.make(ExprK::Int(n), tok.span, format!("{path}/int:{n}")))
            }
            TokK::KwTrue | TokK::KwFalse => {
                self.bump();
                let b = tok.kind == TokK::KwTrue;
                Ok(self.make(ExprK::Bool(b), tok.span, format!("{path}/bool:{b}")))
            }
            TokK::Name => {
                self.bump();
                Ok(self.make(ExprK::Var(tok.text), tok.span, format!("{path}/var")))
            }
            TokK::LParen => {
                let start = self.bump().span.0;
                let e = self.parse_expr(&format!("{path}/paren"))?;
                let end = self.expect(TokK::RParen, "')'")?.span.1;
                let mut e2 = e;
                e2.id = format!("{path}/paren");
                e2.span = Span(start, end);
                Ok(e2)
            }
            TokK::KwHash => self.parse_variant(path),
            TokK::LBrace => self.parse_record(path),
            TokK::KwFn | TokK::Backslash => self.parse_abs(path, tok.span.0),
            TokK::KwMatch => self.parse_match(path, tok.span.0),
            _ => Err(ParseError {
                message: format!("unexpected token '{}'", tok.text),
                span: tok.span,
            }),
        }
    }

    fn parse_variant(&mut self, path: &str) -> Result<Expr, ParseError> {
        let start = self.bump().span.0;
        let tag = self.expect(TokK::Name, "variant tag")?;
        let mut end = tag.span.1;
        let payload = if self.atom_starts() {
            let p = self.parse_atom(&format!("{path}/tag:{}/payload", tag.text))?;
            end = p.span.1;
            Some(Box::new(p))
        } else {
            None
        };
        Ok(self.make(
            ExprK::Variant { tag: tag.text, payload },
            Span(start, end),
            format!("{path}/tag:{}", tag.text),
        ))
    }

    fn parse_record(&mut self, path: &str) -> Result<Expr, ParseError> {
        let start = self.bump().span.0;
        if self.at2(TokK::Name, TokK::Equal) {
            let mut fields = Vec::new();
            loop {
                if self.peek() == TokK::RBrace {
                    break;
                }
                let name = self.expect(TokK::Name, "field name")?;
                self.expect(TokK::Equal, "'='")?;
                let value = self.parse_expr(&format!("{path}/field:{}/value", name.text))?;
                fields.push(Field {
                    name: name.text,
                    span: Span(name.span.0, value.span.1),
                    value,
                });
                if !self.eat(TokK::Comma) {
                    break;
                }
            }
            let end = self.expect(TokK::RBrace, "'}}'")?.span.1;
            return Ok(self.make(
                ExprK::Record(fields),
                Span(start, end),
                format!("{path}/record"),
            ));
        }
        let base = self.parse_app(&format!("{path}/extend/base"))?;
        self.expect(TokK::Pipe, "'|' in record extension")?;
        let field_tok = self.expect(TokK::Name, "field name")?;
        self.expect(TokK::Equal, "'='")?;
        let value = self.parse_expr(&format!("{path}/field:{}/value", field_tok.text))?;
        self.eat(TokK::Comma);
        let end = self.expect(TokK::RBrace, "'}}'")?.span.1;
        Ok(self.make(
            ExprK::Extend {
                base: Box::new(base),
                field: field_tok.text,
                value: Box::new(value),
            },
            Span(start, end),
            format!("{path}/extend:{}", field_tok.text),
        ))
    }
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    let toks = Lexer { src: src.as_bytes(), pos: 0 }.tokenize()?;
    Parser { toks, idx: 0 }.parse_program()
}

pub fn span_text(src: &str, span: Span) -> String {
    src.get(span.0..span.1).unwrap_or("").replace('\n', " ")
}
}
