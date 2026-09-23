//! Lexer, AST and parser for the tiny Hindley-Milner language.
//!
//! AST nodes carry byte ranges (`Span`) and a stable structural path
//! (`NodePath`) that survives renames and unchanged edits.

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start as u32, end: end as u32 }
    }
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }
    pub fn is_empty(&self) -> bool {
        self.end == self.start
    }
    pub fn range(&self) -> (u32, u32) {
        (self.start, self.end)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}..{}", self.message, self.span.start, self.span.end)
    }
}

/// One structural step in the AST path. Purely positional/structural so that
/// renaming a bound variable never changes the paths of unrelated nodes.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Seg {
    Root,
    Binder(usize),
    Param(usize),
    ParamTy,
    Arg(usize),
    Body,
    Rhs(usize),
    RecRhs(usize),
    Else,
    Then,
    Cond,
    Field(String),
    FieldTy(String),
    Tag(String),
    TagPayload(String),
    Branch(usize),
    BranchTag,
    BranchBody(usize),
    Op,
    Lhs,
    RhsOp,
    Expr,
    Decl(usize),
}

impl fmt::Display for Seg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Seg::Root => write!(f, "$"),
            Seg::Binder(i) => write!(f, "b{i}"),
            Seg::Param(i) => write!(f, "p{i}"),
            Seg::ParamTy => write!(f, "pty"),
            Seg::Arg(i) => write!(f, "a{i}"),
            Seg::Body => write!(f, "body"),
            Seg::Rhs(i) => write!(f, "rhs{i}"),
            Seg::RecRhs(i) => write!(f, "rec{i}"),
            Seg::Else => write!(f, "else"),
            Seg::Then => write!(f, "then"),
            Seg::Cond => write!(f, "cond"),
            Seg::Field(n) => write!(f, "f:{n}"),
            Seg::FieldTy(n) => write!(f, "ft:{n}"),
            Seg::Tag(n) => write!(f, "t:{n}"),
            Seg::TagPayload(n) => write!(f, "tp:{n}"),
            Seg::Branch(i) => write!(f, "br{i}"),
            Seg::BranchTag => write!(f, "brtag"),
            Seg::BranchBody(i) => write!(f, "brb{i}"),
            Seg::Op => write!(f, "op"),
            Seg::Lhs => write!(f, "lhs"),
            Seg::RhsOp => write!(f, "rhsop"),
            Seg::Expr => write!(f, "expr"),
            Seg::Decl(i) => write!(f, "d{i}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodePath(pub Vec<Seg>);

impl NodePath {
    pub fn root() -> Self {
        NodePath(vec![Seg::Root])
    }
    pub fn child(&self, seg: Seg) -> Self {
        let mut p = self.0.clone();
        p.push(seg);
        NodePath(p)
    }
    pub fn key(&self) -> String {
        self.0
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("/")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub name_span: Span,
    pub ann: Option<Box<AstNode<TypeKind>>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Branch {
    pub tag: String,
    pub tag_span: Span,
    pub binding: Option<(String, Span)>,
    pub body: Box<AstNode<ExprKind>>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Binding {
    pub name: String,
    pub name_span: Span,
    pub ann: Option<Box<AstNode<TypeKind>>>,
    pub rhs: Box<AstNode<ExprKind>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Int(i64),
    Bool(bool),
    Var(String),
    Lam {
        params: Vec<Param>,
        body: Box<AstNode<ExprKind>>,
    },
    App {
        fun: Box<AstNode<ExprKind>>,
        args: Vec<AstNode<ExprKind>>,
    },
    BinOp {
        op: Op,
        lhs: Box<AstNode<ExprKind>>,
        rhs: Box<AstNode<ExprKind>>,
    },
    Let {
        bindings: Vec<Binding>,
        rec: bool,
        body: Box<AstNode<ExprKind>>,
    },
    If {
        cond: Box<AstNode<ExprKind>>,
        then_e: Box<AstNode<ExprKind>>,
        else_e: Box<AstNode<ExprKind>>,
    },
    Record {
        fields: Vec<(String, Span, AstNode<ExprKind>)>,
    },
    Field {
        expr: Box<AstNode<ExprKind>>,
        name: String,
        name_span: Span,
    },
    Tag {
        tag: String,
        tag_span: Span,
        payload: Option<Box<AstNode<ExprKind>>>,
    },
    Match {
        scrut: Box<AstNode<ExprKind>>,
        branches: Vec<Branch>,
    },
    Paren(Box<AstNode<ExprKind>>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Eq,
}

impl Op {
    pub fn as_str(self) -> &'static str {
        match self {
            Op::Add => "+",
            Op::Eq => "==",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeKind {
    TCon(String),
    TFun(Vec<AstNode<TypeKind>>, Box<AstNode<TypeKind>>),
    TRecord(Vec<(String, Span, AstNode<TypeKind>)>, bool),
    TVariant(Vec<(String, Span, Option<AstNode<TypeKind>>)>, bool),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstNode<K> {
    pub kind: K,
    pub span: Span,
    pub path: NodePath,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Decl {
    pub name: String,
    pub name_span: Span,
    pub ann: Option<AstNode<TypeKind>>,
    pub rhs: AstNode<ExprKind>,
    pub rec: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub decls: Vec<Decl>,
    pub main: Option<AstNode<ExprKind>>,
    pub span: Span,
}

// ---------------- Lexer ----------------

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Ident(String),
    Let,
    Rec,
    In,
    If,
    Then,
    Else,
    True,
    False,
    Match,
    With,
    Fun,
    And,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Dot,
    Comma,
    Eq,
    Colon,
    Semicolon,
    Pipe,
    Arrow,
    Plus,
    EqEq,
    Backtick,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let kw = |s: &str| match s {
        "let" => Tok::Let,
        "rec" => Tok::Rec,
        "in" => Tok::In,
        "if" => Tok::If,
        "then" => Tok::Then,
        "else" => Tok::Else,
        "true" => Tok::True,
        "false" => Tok::False,
        "match" => Tok::Match,
        "with" => Tok::With,
        "fun" => Tok::Fun,
        "and" => Tok::And,
        _ => Tok::Ident(s.to_string()),
    };
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        let start = i;
        if c.is_ascii_digit() {
            let mut n: i64 = 0;
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                n = n.wrapping_mul(10).wrapping_add((b[i] - b'0') as i64);
                i += 1;
            }
            out.push(Token { tok: Tok::Int(n), span: Span::new(start, i) });
            continue;
        }
        if c.is_alphabetic() || c == '_' {
            while i < b.len() && {
                let ch = b[i] as char;
                ch.is_alphanumeric() || ch == '_'
            } {
                i += 1;
            }
            let s = &src[start..i];
            out.push(Token { tok: kw(s), span: Span::new(start, i) });
            continue;
        }
        let push1 = |t: Tok, out: &mut Vec<Token>, i: &mut usize| {
            out.push(Token { tok: t, span: Span::new(start, *i) });
        };
        match c {
            '{' => {
                i += 1;
                push1(Tok::LBrace, &mut out, &mut i);
            }
            '}' => {
                i += 1;
                push1(Tok::RBrace, &mut out, &mut i);
            }
            '(' => {
                i += 1;
                push1(Tok::LParen, &mut out, &mut i);
            }
            ')' => {
                i += 1;
                push1(Tok::RParen, &mut out, &mut i);
            }
            '.' => {
                i += 1;
                push1(Tok::Dot, &mut out, &mut i);
            }
            ',' => {
                i += 1;
                push1(Tok::Comma, &mut out, &mut i);
            }
            ':' => {
                i += 1;
                push1(Tok::Colon, &mut out, &mut i);
            }
            ';' => {
                i += 1;
                push1(Tok::Semicolon, &mut out, &mut i);
            }
            '|' => {
                i += 1;
                push1(Tok::Pipe, &mut out, &mut i);
            }
            '`' => {
                i += 1;
                push1(Tok::Backtick, &mut out, &mut i);
            }
            '+' => {
                i += 1;
                push1(Tok::Plus, &mut out, &mut i);
            }
            '=' => {
                i += 1;
                if i < b.len() && b[i] == b'=' {
                    i += 1;
                    out.push(Token { tok: Tok::EqEq, span: Span::new(start, i) });
                } else {
                    out.push(Token { tok: Tok::Eq, span: Span::new(start, i) });
                }
            }
            '-' => {
                i += 1;
                if i < b.len() && b[i] == b'>' {
                    i += 1;
                    out.push(Token { tok: Tok::Arrow, span: Span::new(start, i) });
                } else {
                    return Err(ParseError {
                        message: "unexpected '-' (did you mean '->'?)".into(),
                        span: Span::new(start, i),
                    });
                }
            }
            other => {
                return Err(ParseError {
                    message: format!("unexpected character {other:?}"),
                    span: Span::new(i, i + 1),
                });
            }
        }
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}

// ---------------- Parser ----------------

pub struct Parser<'a> {
    toks: Vec<Token>,
    pos: usize,
    src: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Result<Self, ParseError> {
        Ok(Parser { toks: lex(src)?, pos: 0, src })
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
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            Ok(self.bump())
        } else {
            Err(ParseError {
                message: format!("expected {what}, found {}", tok_name(self.peek())),
                span: self.span(),
            })
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let start = self.span().start;
        let mut decls = Vec::new();
        let mut idx = 0;
        loop {
            match self.peek() {
                Tok::Let => {
                    let path = NodePath::root().child(Seg::Decl(idx));
                    decls.push(self.parse_decl(&path)?);
                    idx += 1;
                    let _ = self.eat(&Tok::Semicolon);
                }
                Tok::Eof => break,
                _ => break,
            }
        }
        let main = match self.peek() {
            Tok::Eof => None,
            _ => Some(self.parse_expr(&NodePath::root().child(Seg::Body))?),
        };
        self.expect(Tok::Eof, "end of input")?;
        let end = self.toks[self.pos.saturating_sub(1)].span.end;
        Ok(Program { decls, main, span: Span::new(start as usize, end as usize) })
    }

    fn parse_decl(&mut self, path: &NodePath) -> Result<Decl, ParseError> {
        self.expect(Tok::Let, "'let'")?;
        let rec = self.eat(&Tok::Rec);
        let name_t = self.expect_ident()?;
        let mut params = Vec::new();
        let mut pi = 0;
        while let Some(p) = self.try_parse_param(path, pi)? {
            params.push(p);
            pi += 1;
        }
        let ann = if self.eat(&Tok::Colon) {
            Some(Box::new(self.parse_type(&path.child(Seg::Binder(0)))?))
        } else {
            None
        };
        self.expect(Tok::Eq, "'='")?;
        let body_path = path.child(Seg::Rhs(0));
        let rhs_expr = if !params.is_empty() {
            let body = self.parse_expr(&body_path.child(Seg::Body))?;
            let start = params.first().unwrap().span.start;
            let span = Span::new(start as usize, body.span.end as usize);
            AstNode {
                kind: ExprKind::Lam { params, body: Box::new(body) },
                span,
                path: body_path,
            }
        } else {
            self.parse_expr(&body_path)?
        };
        Ok(Decl {
            name: ident_str(&name_t),
            name_span: name_t.span,
            ann: ann.map(|b| *b),
            rhs: rhs_expr,
            rec,
        })
    }

    fn try_parse_param(
        &mut self,
        path: &NodePath,
        i: usize,
    ) -> Result<Option<Param>, ParseError> {
        if let Tok::Ident(_) = self.peek() {
            // Only a parameter when not followed by '=' and it is an identifier.
            let t = self.bump();
            let ann = if self.eat(&Tok::Colon) {
                Some(Box::new(self.parse_type(&path.child(Seg::Param(i)).child(Seg::ParamTy))?))
            } else {
                None
            };
            let end = ann.as_ref().map(|a| a.span.end).unwrap_or(t.span.end);
            return Ok(Some(Param {
                name: ident_str(&t),
                name_span: t.span,
                ann,
                span: Span::new(t.span.start as usize, end as usize),
            }));
        }
        Ok(None)
    }

    fn expect_ident(&mut self) -> Result<Token, ParseError> {
        if let Tok::Ident(_) = self.peek() {
            Ok(self.bump())
        } else {
            Err(ParseError {
                message: format!("expected an identifier, found {}", tok_name(self.peek())),
                span: self.span(),
            })
        }
    }

    fn parse_type(&mut self, path: &NodePath) -> Result<AstNode<TypeKind>, ParseError> {
        // Domain types may be parenthesized; record/variant annotations supported.
        let first = self.parse_type_atom(&path.child(Seg::Param(0)))?;
        if self.eat(&Tok::Arrow) {
            let ret = self.parse_type(&path.child(Seg::Body))?;
            let span = Span::new(first.span.start as usize, ret.span.end as usize);
            Ok(AstNode { kind: TypeKind::TFun(vec![first], Box::new(ret)), span, path: path.clone() })
        } else {
            Ok(first)
        }
    }

    fn parse_type_atom(&mut self, path: &NodePath) -> Result<AstNode<TypeKind>, ParseError> {
        match self.peek().clone() {
            Tok::LBrace => {
                let lb = self.bump().span;
                let mut fields = Vec::new();
                let mut closed = true;
                loop {
                    if let Tok::RBrace = self.peek() {
                        break;
                    }
                    if let Tok::Pipe = self.peek() {
                        self.bump();
                        closed = false;
                        break;
                    }
                    let n = self.expect_ident()?;
                    self.expect(Tok::Colon, "':'")?;
                    let ty = self.parse_type_atom(&path.child(Seg::FieldTy(ident_str(&n))))?;
                    fields.push((ident_str(&n), n.span, ty));
                    if !self.eat(&Tok::Comma) {
                        break;
                    }
                }
                let rb = self.expect(Tok::RBrace, "'}'")?.span;
                let span = Span::new(lb.start as usize, rb.end as usize);
                Ok(AstNode { kind: TypeKind::TRecord(fields, closed), span, path: path.clone() })
            }
            Tok::Ident(s) => {
                let t = self.bump();
                Ok(AstNode { kind: TypeKind::TCon(s), span: t.span, path: path.clone() })
            }
            _ => Err(ParseError {
                message: format!("expected a type, found {}", tok_name(self.peek())),
                span: self.span(),
            }),
        }
    }

    // Expression grammar (Pratt-ish):
    // expr   := let-expr | if-expr | fun-expr | match-expr | binop
    // binop  := app ('+'|'==') app
    // app    := atom+
    fn parse_expr(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        match self.peek() {
            Tok::Let => self.parse_let(path),
            Tok::If => self.parse_if(path),
            Tok::Fun => self.parse_fun(path),
            Tok::Match => self.parse_match(path),
            _ => self.parse_binop(path),
        }
    }

    fn parse_let(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        let lt = self.expect(Tok::Let, "'let'")?;
        let rec = self.eat(&Tok::Rec);
        let mut bindings = Vec::new();
        let mut bi = 0;
        loop {
            let name_t = self.expect_ident()?;
            let mut params = Vec::new();
            let mut pi = 0;
            while let Some(p) = self.try_parse_param(&path.child(Seg::Binder(bi)), pi)? {
                params.push(p);
                pi += 1;
            }
            let ann = if self.eat(&Tok::Colon) {
                Some(Box::new(self.parse_type(
                    &path.child(Seg::Binder(bi)).child(Seg::ParamTy),
                )?))
            } else {
                None
            };
            self.expect(Tok::Eq, "'='")?;
            let rhs0 = if !params.is_empty() {
                let body = self
                    .parse_expr(&path.child(Seg::Rhs(bi)).child(Seg::Body))?;
                let start = params.first().unwrap().span.start;
                let span = Span::new(start as usize, body.span.end as usize);
                AstNode {
                    kind: ExprKind::Lam { params, body: Box::new(body) },
                    span,
                    path: path.child(Seg::Rhs(bi)),
                }
            } else {
                self.parse_expr(&path.child(Seg::Rhs(bi)))?
            };
            bindings.push(Binding {
                name: ident_str(&name_t),
                name_span: name_t.span,
                ann,
                rhs: Box::new(rhs0),
            });
            bi += 1;
            if !self.eat(&Tok::And) {
                break;
            }
        }
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr(&path.child(Seg::Body))?;
        let span = Span::new(lt.span.start as usize, body.span.end as usize);
        Ok(AstNode { kind: ExprKind::Let { bindings, rec, body: Box::new(body) }, span, path: path.clone() })
    }

    fn parse_if(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        let it = self.expect(Tok::If, "'if'")?;
        let cond = self.parse_expr(&path.child(Seg::Cond))?;
        self.expect(Tok::Then, "'then'")?;
        let then_e = self.parse_expr(&path.child(Seg::Then))?;
        self.expect(Tok::Else, "'else'")?;
        let else_e = self.parse_expr(&path.child(Seg::Else))?;
        let span = Span::new(it.span.start as usize, else_e.span.end as usize);
        Ok(AstNode {
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_e: Box::new(then_e),
                else_e: Box::new(else_e),
            },
            span,
            path: path.clone(),
        })
    }

    fn parse_fun(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        let ft = self.expect(Tok::Fun, "'fun'")?;
        let mut params = Vec::new();
        let mut i = 0;
        while let Some(p) = self.try_parse_param(path, i)? {
            params.push(p);
            i += 1;
        }
        if params.is_empty() {
            return Err(ParseError {
                message: "'fun' needs at least one parameter".into(),
                span: ft.span,
            });
        }
        self.expect(Tok::Arrow, "'->'")?;
        let body = self.parse_expr(&path.child(Seg::Body))?;
        let span = Span::new(ft.span.start as usize, body.span.end as usize);
        Ok(AstNode { kind: ExprKind::Lam { params, body: Box::new(body) }, span, path: path.clone() })
    }

    fn parse_match(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        let mt = self.expect(Tok::Match, "'match'")?;
        let scrut = self.parse_expr(&path.child(Seg::Cond))?;
        self.expect(Tok::With, "'with'")?;
        let mut branches = Vec::new();
        let mut i = 0;
        loop {
            if !self.eat(&Tok::Pipe) {
                break;
            }
            let tag_t = self.expect_ident()?;
            let binding = if let Tok::Ident(_) = self.peek() {
                let t = self.bump();
                Some((ident_str(&t), t.span))
            } else {
                None
            };
            self.expect(Tok::Arrow, "'->'")?;
            let body = self.parse_expr(&path.child(Seg::Branch(i)).child(Seg::BranchBody(0)))?;
            let span = Span::new(tag_t.span.start as usize, body.span.end as usize);
            branches.push(Branch {
                tag: ident_str(&tag_t),
                tag_span: tag_t.span,
                binding,
                body: Box::new(body),
                span,
            });
            i += 1;
        }
        if branches.is_empty() {
            return Err(ParseError {
                message: "'match' needs at least one '| tag ->' branch".into(),
                span: mt.span,
            });
        }
        let end = branches.last().unwrap().span.end;
        let span = Span::new(mt.span.start as usize, end as usize);
        Ok(AstNode {
            kind: ExprKind::Match { scrut: Box::new(scrut), branches },
            span,
            path: path.clone(),
        })
    }

    fn parse_binop(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        let lhs = self.parse_app(&path.child(Seg::Lhs))?;
        let op = match self.peek() {
            Tok::Plus => {
                self.bump();
                Op::Add
            }
            Tok::EqEq => {
                self.bump();
                Op::Eq
            }
            _ => return Ok(lhs),
        };
        let rhs = self.parse_app(&path.child(Seg::RhsOp))?;
        let span = Span::new(lhs.span.start as usize, rhs.span.end as usize);
        Ok(AstNode {
            kind: ExprKind::BinOp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) },
            span,
            path: path.clone(),
        })
    }

    fn parse_app(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        let fun = self.parse_atom(&path.child(Seg::Expr))?;
        let mut args = Vec::new();
        let mut i = 0;
        while self.starts_atom() {
            args.push(self.parse_atom(&path.child(Seg::Arg(i)))?);
            i += 1;
        }
        if args.is_empty() {
            return Ok(fun);
        }
        let end = args.last().unwrap().span.end;
        let span = Span::new(fun.span.start as usize, end as usize);
        Ok(AstNode {
            kind: ExprKind::App { fun: Box::new(fun), args },
            span,
            path: path.clone(),
        })
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_)
                | Tok::Ident(_)
                | Tok::True
                | Tok::False
                | Tok::LParen
                | Tok::LBrace
                | Tok::Backtick
                | Tok::Fun
                | Tok::If
                | Tok::Match
                | Tok::Let
        )
    }

    fn parse_atom(&mut self, path: &NodePath) -> Result<AstNode<ExprKind>, ParseError> {
        let t = self.bump();
        let span0 = t.span;
        let node = match t.tok {
            Tok::Int(n) => AstNode { kind: ExprKind::Int(n), span: span0, path: path.clone() },
            Tok::True => AstNode { kind: ExprKind::Bool(true), span: span0, path: path.clone() },
            Tok::False => AstNode { kind: ExprKind::Bool(false), span: span0, path: path.clone() },
            Tok::Ident(name) => {
                let mut e = AstNode { kind: ExprKind::Var(name), span: span0, path: path.clone() };
                while self.eat(&Tok::Dot) {
                    let n = self.expect_ident()?;
                    let span = Span::new(e.span.start as usize, n.span.end as usize);
                    e = AstNode {
                        kind: ExprKind::Field {
                            expr: Box::new(e),
                            name: ident_str(&n),
                            name_span: n.span,
                        },
                        span,
                        path: path.clone(),
                    };
                }
                e
            }
            Tok::LParen => {
                let inner = self.parse_expr(&path.child(Seg::Expr))?;
                let rp = self.expect(Tok::RParen, "')'")?;
                let span = Span::new(span0.start as usize, rp.span.end as usize);
                AstNode { kind: ExprKind::Paren(Box::new(inner)), span, path: path.clone() }
            }
            Tok::LBrace => self.parse_record_tail(span0, path)?,
            Tok::Backtick => self.parse_tag_tail(span0, path)?,
            other => {
                return Err(ParseError {
                    message: format!("expected an expression, found {}", tok_name(&other)),
                    span: span0,
                });
            }
        };
        Ok(node)
    }

    fn parse_record_tail(
        &mut self,
        lb: Span,
        path: &NodePath,
    ) -> Result<AstNode<ExprKind>, ParseError> {
        let mut fields = Vec::new();
        loop {
            if let Tok::RBrace = self.peek() {
                break;
            }
            let n = self.expect_ident()?;
            self.expect(Tok::Colon, "':'")?;
            let e = self.parse_expr(&path.child(Seg::Field(ident_str(&n))))?;
            fields.push((ident_str(&n), n.span, e));
            if !self.eat(&Tok::Comma) {
                break;
            }
        }
        let rb = self.expect(Tok::RBrace, "'}'")?;
        let span = Span::new(lb.start as usize, rb.span.end as usize);
        Ok(AstNode { kind: ExprKind::Record { fields }, span, path: path.clone() })
    }

    fn parse_tag_tail(
        &mut self,
        bt: Span,
        path: &NodePath,
    ) -> Result<AstNode<ExprKind>, ParseError> {
        let n = self.expect_ident()?;
        let payload = if self.starts_atom() {
            Some(Box::new(self.parse_atom(&path.child(Seg::TagPayload(ident_str(&n))))?))
        } else {
            None
        };
        let end = payload.as_ref().map(|p| p.span.end).unwrap_or(n.span.end);
        let span = Span::new(bt.start as usize, end as usize);
        Ok(AstNode {
            kind: ExprKind::Tag { tag: ident_str(&n), tag_span: n.span, payload },
            span,
            path: path.clone(),
        })
    }
}

fn ident_str(t: &Token) -> String {
    match &t.tok {
        Tok::Ident(s) => s.clone(),
        _ => panic!("ident_str on non-ident token"),
    }
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Int(_) => "integer".into(),
        Tok::Ident(s) => format!("identifier '{s}'"),
        Tok::Let => "'let'".into(),
        Tok::Rec => "'rec'".into(),
        Tok::In => "'in'".into(),
        Tok::If => "'if'".into(),
        Tok::Then => "'then'".into(),
        Tok::Else => "'else'".into(),
        Tok::True => "'true'".into(),
        Tok::False => "'false'".into(),
        Tok::Match => "'match'".into(),
        Tok::With => "'with'".into(),
        Tok::Fun => "'fun'".into(),
        Tok::And => "'and'".into(),
        Tok::LBrace => "'{'".into(),
        Tok::RBrace => "'}'".into(),
        Tok::LParen => "'('".into(),
        Tok::RParen => "')'".into(),
        Tok::Dot => "'.'".into(),
        Tok::Comma => "','".into(),
        Tok::Eq => "'='".into(),
        Tok::Colon => "':'".into(),
        Tok::Semicolon => "';'".into(),
        Tok::Pipe => "'|'".into(),
        Tok::Arrow => "'->'".into(),
        Tok::Plus => "'+'".into(),
        Tok::EqEq => "'=='".into(),
        Tok::Backtick => "'`'".into(),
        Tok::Eof => "end of input".into(),
    }
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    Parser::new(src)?.parse_program()
}

