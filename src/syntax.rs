//! Lexer, AST and parser. Node identities are path based so renaming a
//! variable never shifts identities of unrelated nodes.

use serde::Serialize;

pub type NodeId = u64;
pub type CId = u64;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span(pub u32, pub u32);

impl Span {
    pub fn join(self, other: Span) -> Span {
        Span(self.0, other.1)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub span: Span,
    pub expr: Expr,
    pub children: Vec<NodeId>,
    pub content_hash: u64,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Unit,
    Var(String),
    Lam { param: String, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Bin { op: BinOp, lhs: NodeId, rhs: NodeId },
    Let {
        name: String,
        rec: bool,
        value: NodeId,
        body: NodeId,
    },
    Record(Vec<(String, NodeId)>),
    Field { record: NodeId, label: String },
    Extend {
        record: NodeId,
        fields: Vec<(String, NodeId)>,
    },
    Tag { label: String, payload: Option<NodeId> },
    Match {
        scrut: NodeId,
        arms: Vec<(Pat, NodeId)>,
    },
    If { cond: NodeId, then: NodeId, else_: NodeId },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum Pat {
    Wild,
    Var(String),
    Tag { label: String, binding: Option<String> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
    And,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Program {
    pub root: NodeId,
    pub nodes: Vec<Node>,
    pub source: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokKind,
    pub span: Span,
    pub text: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokKind {
    Int(i64),
    Str(String),
    Ident,
    Tag,
    Keyword(&'static str),
    Op(&'static str),
    Punct(char),
}

pub fn lex(src: &str) -> Result<Vec<Token>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let start = i as u32;
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '/' && b.get(i + 1) == Some(&b'/') {
            i += 2;
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        if c == '(' && b.get(i + 1) == Some(&b'*') {
            i += 2;
            let mut depth = 1;
            while i < b.len() && depth > 0 {
                if b.get(i) == Some(&b'(') && b.get(i + 1) == Some(&b'*') {
                    depth += 1;
                    i += 2;
                } else if b.get(i) == Some(&b'*') && b.get(i + 1) == Some(&b')') {
                    depth -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if depth != 0 {
                return Err(ParseError {
                    span: Span(start, src.len() as u32),
                    message: "未闭合的块注释".to_string(),
                });
            }
            continue;
        }
        if c.is_ascii_digit() {
            let s = i;
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            let text: String = src[s..i].to_string();
            let n = text.parse().map_err(|_| ParseError {
                span: Span(start, i as u32),
                message: "整数字面量越界".to_string(),
            })?;
            out.push(Token {
                kind: TokKind::Int(n),
                span: Span(start, i as u32),
                text,
            });
            continue;
        }
        if c == '"' {
            i += 1;
            let s = i;
            let mut val = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    val.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        _ => e,
                    });
                    i += 2;
                } else {
                    val.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(ParseError {
                    span: Span(start, i as u32),
                    message: "未闭合的字符串".to_string(),
                });
            }
            i += 1;
            out.push(Token {
                kind: TokKind::Str(val),
                span: Span(start, i as u32),
                text: src[s..i - 1].to_string(),
            });
            continue;
        }
        if c == '#' {
            i += 1;
            let s = i;
            while i < b.len() && (b[i].is_ascii_alphanumeric() || b[i] == b'_') {
                i += 1;
            }
            if s == i {
                return Err(ParseError {
                    span: Span(start, i as u32),
                    message: "# 后缺少变体标签".to_string(),
                });
            }
            out.push(Token {
                kind: TokKind::Tag,
                span: Span(start, i as u32),
                text: src[s..i].to_string(),
            });
            continue;
        }
        if c.is_ascii_alphabetic() || c == '_' {
            let s = i;
            while i < b.len()
                && (b[i].is_ascii_alphanumeric() || b[i] == b'_')
            {
                i += 1;
            }
            let text = &src[s..i];
            let kw = match text {
                "let" => Some("let"),
                "rec" => Some("rec"),
                "in" => Some("in"),
                "fun" => Some("fun"),
                "true" => Some("true"),
                "false" => Some("false"),
                "if" => Some("if"),
                "then" => Some("then"),
                "else" => Some("else"),
                "match" => Some("match"),
                "with" => Some("with"),
                _ => None,
            };
            out.push(Token {
                kind: match kw {
                    Some(k) => TokKind::Keyword(k),
                    None => TokKind::Ident,
                },
                span: Span(start, i as u32),
                text: text.to_string(),
            });
            continue;
        }
        let punct = "(){}.,;=>|";
        if punct.contains(c) {
            i += 1;
            out.push(Token {
                kind: TokKind::Punct(c),
                span: Span(start, i as u32),
                text: c.to_string(),
            });
            continue;
        }
        let multi = ["->", "==", "<=", ">=", "&&", "+", "-", "*", "=", "<"];
        if let Some(op) = multi.iter().find(|op| src[i..].starts_with(**op)) {
            let n = op.len();
            i += n;
            out.push(Token {
                kind: TokKind::Op(op),
                span: Span(start, i as u32),
                text: op.to_string(),
            });
            continue;
        }
        return Err(ParseError {
            span: Span(start, start + 1),
            message: format!("无法识别的字符 {:?}", c),
        });
    }
    Ok(out)
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Debug)]
struct Raw {
    span: Span,
    kind: RawKind,
}

#[derive(Clone, Debug)]
enum RawKind {
    Int(i64),
    Bool(bool),
    Str(String),
    Unit,
    Var(String),
    Lam(String, Box<Raw>),
    App(Box<Raw>, Box<Raw>),
    Bin(BinOp, Box<Raw>, Box<Raw>),
    Let(String, bool, Box<Raw>, Box<Raw>),
    Record(Vec<(String, Raw)>),
    Field(Box<Raw>, String),
    Extend(Box<Raw>, Vec<(String, Raw)>),
    Tag(String, Option<Box<Raw>>),
    Match(Box<Raw>, Vec<(Pat, Raw)>),
    If(Box<Raw>, Box<Raw>, Box<Raw>),
}

struct Parser<'a> {
    toks: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<&Token> {
        self.toks.get(self.pos)
    }
    fn bump(&mut self) -> Option<&Token> {
        let t = self.toks.get(self.pos);
        if t.is_some() {
            self.pos += 1;
        }
        t
    }
    fn err<T>(&self, span: Span, msg: impl Into<String>) -> Result<T, ParseError> {
        Err(ParseError {
            span,
            message: msg.into(),
        })
    }
    fn eat_punct(&mut self, c: char) -> bool {
        if matches!(self.peek().map(|t| &t.kind), Some(TokKind::Punct(p)) if *p == c) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn expect_punct(&mut self, c: char) -> Result<Span, ParseError> {
        match self.peek() {
            Some(t) if matches!(t.kind, TokKind::Punct(p) if p == c) => {
                let s = t.span;
                self.pos += 1;
                Ok(s)
            }
            Some(t) => self.err(t.span, format!("期望 '{}'", c)),
            None => self.err(Span(self.toks.last().map_or(0, |t| t.span.1), 0), format!("期望 '{}'", c)),
        }
    }
    fn eat_kw(&mut self, kw: &str) -> bool {
        if matches!(self.peek().map(|t| &t.kind), Some(TokKind::Keyword(k)) if *k == kw) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn expect_kw(&mut self, kw: &str) -> Result<Span, ParseError> {
        match self.peek() {
            Some(t) if matches!(t.kind, TokKind::Keyword(k) if *k == kw) => {
                let s = t.span;
                self.pos += 1;
                Ok(s)
            }
            Some(t) => self.err(t.span, format!("期望关键字 {}", kw)),
            None => self.err(Span(0, 0), format!("期望关键字 {}", kw)),
        }
    }
    fn eat_op(&mut self, op: &str) -> bool {
        if matches!(self.peek().map(|t| &t.kind), Some(TokKind::Op(o)) if *o == op) {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn parse_program(&mut self) -> Result<Raw, ParseError> {
        let e = self.parse_expr()?;
        while self.eat_punct(';') {}
        if let Some(t) = self.peek() {
            return self.err(t.span, "顶层之后还有多余记号");
        }
        Ok(e)
    }

    fn parse_expr(&mut self) -> Result<Raw, ParseError> {
        if self.eat_kw("let") {
            return self.parse_let();
        }
        if self.eat_kw("if") {
            return self.parse_if();
        }
        if self.eat_kw("fun") {
            return self.parse_fun();
        }
        self.parse_and()
    }

    fn parse_let(&mut self) -> Result<Raw, ParseError> {
        let start = self.toks.get(self.pos.saturating_sub(1)).unwrap().span.0;
        let rec = self.eat_kw("rec");
        let name_tok = match self.bump() {
            Some(t) if matches!(t.kind, TokKind::Ident) => t.clone(),
            Some(t) => return self.err(t.span, "let 后期望变量名"),
            None => return self.err(Span(start, start), "let 后期望变量名"),
        };
        self.expect_punct('=')?;
        let value = self.parse_expr()?;
        self.expect_kw("in")?;
        let body = self.parse_expr()?;
        let end = body.span.1;
        Ok(Raw {
            span: Span(start, end),
            kind: RawKind::Let(name_tok.text, rec, Box::new(value), Box::new(body)),
        })
    }

    fn parse_if(&mut self) -> Result<Raw, ParseError> {
        let start = self.toks.get(self.pos.saturating_sub(1)).unwrap().span.0;
        let cond = self.parse_expr()?;
        self.expect_kw("then")?;
        let then = self.parse_expr()?;
        self.expect_kw("else")?;
        let else_ = self.parse_expr()?;
        Ok(Raw {
            span: Span(start, else_.span.1),
            kind: RawKind::If(Box::new(cond), Box::new(then), Box::new(else_)),
        })
    }

    fn parse_fun(&mut self) -> Result<Raw, ParseError> {
        let start = self.toks.get(self.pos.saturating_sub(1)).unwrap().span.0;
        let name = match self.bump() {
            Some(t) if matches!(t.kind, TokKind::Ident) => t.text.clone(),
            Some(t) => return self.err(t.span, "fun 后期望参数名"),
            None => return self.err(Span(start, start), "fun 后期望参数名"),
        };
        self.expect_op("->")?;
        let body = self.parse_expr()?;
        Ok(Raw {
            span: Span(start, body.span.1),
            kind: RawKind::Lam(name, Box::new(body)),
        })
    }

    fn binary(&mut self, ops: &[&str], next: fn(&mut Self) -> Result<Raw, ParseError>) -> Result<Raw, ParseError> {
        let mut left = next(self)?;
        loop {
            let op = match self.peek().map(|t| &t.kind) {
                Some(TokKind::Op(o)) if ops.contains(o) => *o,
                _ => break,
            };
            let op_span = self.bump().unwrap().span;
            let right = next(self)?;
            let bop = match op {
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                "*" => BinOp::Mul,
                "==" => BinOp::Eq,
                "<" => BinOp::Lt,
                "&&" => BinOp::And,
                _ => unreachable!(),
            };
            left = Raw {
                span: left.span.join(right.span),
                kind: RawKind::Bin(bop, Box::new(left), Box::new(right)),
            };
            let _ = op_span;
        }
        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Raw, ParseError> {
        self.binary(&["&&"], Self::parse_cmp)
    }
    fn parse_cmp(&mut self) -> Result<Raw, ParseError> {
        self.binary(&["==", "<"], Self::parse_add)
    }
    fn parse_add(&mut self) -> Result<Raw, ParseError> {
        self.binary(&["+", "-"], Self::parse_mul)
    }
    fn parse_mul(&mut self) -> Result<Raw, ParseError> {
        self.binary(&["*"], Self::parse_app)
    }
