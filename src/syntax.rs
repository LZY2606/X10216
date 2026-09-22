use serde::{Deserialize, Serialize};

pub type ExprId = u32;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Fun { param: String, body: Box<Node> },
    App(Box<Node>, Box<Node>),
    Let { rec_: bool, name: String, rhs: Box<Node>, body: Box<Node> },
    If(Box<Node>, Box<Node>, Box<Node>),
    BinOp(BinOp, Box<Node>, Box<Node>),
    Record(Vec<(String, Node)>),
    Field(Box<Node>, String),
    Inj(String, Box<Node>),
    Match(Box<Node>, Vec<(String, String, Node)>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Node {
    pub id: ExprId,
    pub span: Span,
    pub kind: Expr,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LetBinding {
    pub rec_: bool,
    pub name: String,
    pub name_span: Span,
    pub rhs: Node,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Program {
    pub lets: Vec<LetBinding>,
    pub tail: Option<Node>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    True,
    False,
    Ident(String),
    Fun,
    Let,
    Rec,
    In,
    If,
    Then,
    Else,
    Match,
    Inj,
    Arrow,
    Eq,
    EqEq,
    Lt,
    Plus,
    Minus,
    Star,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Bar,
    Semi2, // ;;
    Eof,
}

#[derive(Clone, Debug)]
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
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '-' && i + 1 < b.len() && b[i + 1] == b'-' {
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        let start = i;
        let tok = match c {
            '0'..='9' => {
                let mut n: i64 = 0;
                while i < b.len() && (b[i] as char).is_ascii_digit() {
                    n = n * 10 + (b[i] - b'0') as i64;
                    i += 1;
                }
                Tok::Int(n)
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let s0 = i;
                while i < b.len() {
                    let ch = b[i] as char;
                    if ch.is_alphanumeric() || ch == '_' || ch == '\'' {
                        i += 1;
                    } else {
                        break;
                    }
                }
                match &src[s0..i] {
                    "fun" => Tok::Fun,
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "if" => Tok::If,
                    "then" => Tok::Then,
                    "else" => Tok::Else,
                    "match" => Tok::Match,
                    "inj" => Tok::Inj,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    s => Tok::Ident(s.to_string()),
                }
            }
            '(' => {
                i += 1;
                Tok::LParen
            }
            ')' => {
                i += 1;
                Tok::RParen
            }
            '{' => {
                i += 1;
                Tok::LBrace
            }
            '}' => {
                i += 1;
                Tok::RBrace
            }
            ',' => {
                i += 1;
                Tok::Comma
            }
            '.' => {
                i += 1;
                Tok::Dot
            }
            '|' => {
                i += 1;
                Tok::Bar
            }
            '+' => {
                i += 1;
                Tok::Plus
            }
            '*' => {
                i += 1;
                Tok::Star
            }
            '<' => {
                i += 1;
                Tok::Lt
            }
            '-' => {
                if i + 1 < b.len() && b[i + 1] == b'>' {
                    i += 2;
                    Tok::Arrow
                } else {
                    i += 1;
                    Tok::Minus
                }
            }
            '=' => {
                if i + 1 < b.len() && b[i + 1] == b'=' {
                    i += 2;
                    Tok::EqEq
                } else {
                    i += 1;
                    Tok::Eq
                }
            }
            ';' => {
                if i + 1 < b.len() && b[i + 1] == b';' {
                    i += 2;
                    Tok::Semi2
                } else {
                    return Err(format!("unexpected ';' at {}", start));
                }
            }
            _ => return Err(format!("unexpected character '{}' at {}", c, start)),
        };
        out.push(Token {
            tok,
            span: Span { start, end: i },
        });
    }
    out.push(Token {
        tok: Tok::Eof,
        span: Span {
            start: src.len(),
            end: src.len(),
        },
    });
    Ok(out)
}

pub struct Parser {
    toks: Vec<Token>,
    pos: usize,
    next_id: ExprId,
}

impl Parser {
    pub fn new(src: &str) -> Result<Self, String> {
        Ok(Parser {
            toks: lex(src)?,
            pos: 0,
            next_id: 0,
        })
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }

    fn span(&self) -> Span {
        self.toks[self.pos].span
    }

    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }

    fn expect(&mut self, t: Tok) -> Result<Token, String> {
        if *self.peek() == t {
            Ok(self.bump())
        } else {
            Err(format!(
                "expected {:?}, found {:?} at {}",
                t,
                self.peek(),
                self.span().start
            ))
        }
    }

    fn node(&mut self, start: usize, kind: Expr) -> Node {
        let end = self.toks[self.pos.saturating_sub(1)].span.end;
        let id = self.next_id;
        self.next_id += 1;
        Node {
            id,
            span: Span { start, end },
            kind,
        }
    }

    fn ident(&mut self) -> Result<(String, Span), String> {
        match self.peek().clone() {
            Tok::Ident(s) => {
                let sp = self.bump().span;
                Ok((s, sp))
            }
            t => Err(format!("expected identifier, found {:?}", t)),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        //! program := ("let" ["rec"] ident "=" expr ";;")* expr?
        let mut lets = Vec::new();
        while *self.peek() == Tok::Let {
            let start = self.span().start;
            self.bump();
            let rec_ = if *self.peek() == Tok::Rec {
                self.bump();
                true
            } else {
                false
            };
            let (name, name_span) = self.ident()?;
            self.expect(Tok::Eq)?;
            let rhs = self.parse_expr()?;
            let end = self.expect(Tok::Semi2)?.span.end;
            lets.push(LetBinding {
                rec_,
                name,
                name_span,
                rhs,
                span: Span { start, end },
            });
        }
        let tail = if *self.peek() != Tok::Eof {
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(Tok::Eof)?;
        Ok(Program { lets, tail })
    }

    fn parse_expr(&mut self) -> Result<Node, String> {
        match self.peek() {
            Tok::Fun => {
                let start = self.bump().span.start;
                let (param, _) = self.ident()?;
                self.expect(Tok::Arrow)?;
                let body = self.parse_expr()?;
                Ok(self.node(
                    start,
                    Expr::Fun {
                        param,
                        body: Box::new(body),
                    },
                ))
            }
            Tok::Let => {
                let start = self.bump().span.start;
                let rec_ = if *self.peek() == Tok::Rec {
                    self.bump();
                    true
                } else {
                    false
                };
                let (name, _) = self.ident()?;
                self.expect(Tok::Eq)?;
                let rhs = self.parse_expr()?;
                self.expect(Tok::In)?;
                let body = self.parse_expr()?;
                Ok(self.node(
                    start,
                    Expr::Let {
                        rec_,
                        name,
                        rhs: Box::new(rhs),
                        body: Box::new(body),
                    },
                ))
            }
            Tok::If => {
                let start = self.bump().span.start;
                let c = self.parse_expr()?;
                self.expect(Tok::Then)?;
                let t = self.parse_expr()?;
                self.expect(Tok::Else)?;
                let e = self.parse_expr()?;
                Ok(self.node(
                    start,
                    Expr::If(Box::new(c), Box::new(t), Box::new(e)),
                ))
            }
            Tok::Match => {
                let start = self.bump().span.start;
                let scrut = self.parse_expr()?;
                self.expect(Tok::LBrace)?;
                let mut cases = Vec::new();
                while *self.peek() == Tok::Bar {
                    self.bump();
                    let (tag, _) = self.ident()?;
                    let (var, _) = self.ident()?;
                    self.expect(Tok::Arrow)?;
                    let body = self.parse_expr()?;
                    cases.push((tag, var, body));
                }
                if cases.is_empty() {
                    return Err("match needs at least one case".into());
                }
                self.expect(Tok::RBrace)?;
                Ok(self.node(start, Expr::Match(Box::new(scrut), cases)))
            }
            _ => self.parse_cmp(),
        }
    }

    fn parse_cmp(&mut self) -> Result<Node, String> {
        let lhs = self.parse_add()?;
        match self.peek() {
            Tok::EqEq => {
                let start = lhs.span.start;
                self.bump();
                let rhs = self.parse_add()?;
                Ok(self.node(start, Expr::BinOp(BinOp::Eq, Box::new(lhs), Box::new(rhs))))
            }
            Tok::Lt => {
                let start = lhs.span.start;
                self.bump();
                let rhs = self.parse_add()?;
                Ok(self.node(start, Expr::BinOp(BinOp::Lt, Box::new(lhs), Box::new(rhs))))
            }
            _ => Ok(lhs),
        }
    }

    fn parse_add(&mut self) -> Result<Node, String> {
        let mut lhs = self.parse_mul()?;
        loop {
            let op = match self.peek() {
                Tok::Plus => BinOp::Add,
                Tok::Minus => BinOp::Sub,
                _ => break,
            };
            let start = lhs.span.start;
            self.bump();
            let rhs = self.parse_mul()?;
            lhs = self.node(start, Expr::BinOp(op, Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn parse_mul(&mut self) -> Result<Node, String> {
        let mut lhs = self.parse_app()?;
        while *self.peek() == Tok::Star {
            let start = lhs.span.start;
            self.bump();
            let rhs = self.parse_app()?;
            lhs = self.node(start, Expr::BinOp(BinOp::Mul, Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_) | Tok::True | Tok::False | Tok::Ident(_) | Tok::LParen | Tok::LBrace | Tok::Inj
        )
    }

    fn parse_app(&mut self) -> Result<Node, String> {
        let mut lhs = self.parse_postfix()?;
        while self.starts_atom() {
            let start = lhs.span.start;
            let rhs = self.parse_postfix()?;
            lhs = self.node(start, Expr::App(Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn parse_postfix(&mut self) -> Result<Node, String> {
        let mut e = self.parse_atom()?;
        while *self.peek() == Tok::Dot {
            let start = e.span.start;
            self.bump();
            let (name, _) = self.ident()?;
            e = self.node(start, Expr::Field(Box::new(e), name));
        }
        Ok(e)
    }

    fn parse_atom(&mut self) -> Result<Node, String> {
        let start = self.span().start;
        match self.peek().clone() {
            Tok::Int(n) => {
                self.bump();
                Ok(self.node(start, Expr::Int(n)))
            }
            Tok::True => {
                self.bump();
                Ok(self.node(start, Expr::Bool(true)))
            }
            Tok::False => {
                self.bump();
                Ok(self.node(start, Expr::Bool(false)))
            }
            Tok::Ident(s) => {
                self.bump();
                Ok(self.node(start, Expr::Var(s)))
            }
            Tok::LParen => {
                self.bump();
                let e = self.parse_expr()?;
                self.expect(Tok::RParen)?;
                Ok(e)
            }
            Tok::Inj => {
                self.bump();
                let (tag, _) = self.ident()?;
                self.expect(Tok::Eq)?;
                let payload = self.parse_atom()?;
                Ok(self.node(start, Expr::Inj(tag, Box::new(payload))))
            }
            Tok::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                if *self.peek() != Tok::RBrace {
                    loop {
                        let (name, _) = self.ident()?;
                        self.expect(Tok::Eq)?;
                        let v = self.parse_expr()?;
                        fields.push((name, v));
                        if *self.peek() == Tok::Comma {
                            self.bump();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(Tok::RBrace)?;
                Ok(self.node(start, Expr::Record(fields)))
            }
            t => Err(format!("unexpected token {:?} at {}", t, start)),
        }
    }
}

pub fn parse(src: &str) -> Result<Program, String> {
    Parser::new(src)?.parse_program()
}
