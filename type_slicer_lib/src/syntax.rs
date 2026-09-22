//! Lexer, parser and identity-annotated AST for the tiny HM language.
//!
//! Node identities are *path based*: they depend on structural slots, not on
//! identifier names, so renaming a variable does not make every identity drift.

use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start as u32, end: end as u32 }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NodeId(pub u64);

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Node<T> {
    pub id: NodeId,
    pub span: Span,
    pub inner: T,
}

pub type NExpr = Node<Expr>;
pub type NArm = Node<Arm>;
pub type NPat = Node<Pat>;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Eq,
    Lt,
}

impl BinOp {
    pub fn symbol(&self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Eq => "=",
            BinOp::Lt => "<",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Expr {
    Unit,
    Int(i64),
    Bool(bool),
    Str(String),
    Var(String),
    Lam { param: String, body: Box<NExpr> },
    App { fun: Box<NExpr>, arg: Box<NExpr> },
    Let { rec: bool, name: String, rhs: Box<NExpr>, body: Box<NExpr> },
    If { cond: Box<NExpr>, then_: Box<NExpr>, else_: Box<NExpr> },
    /// `{ a = e, b = f | rest }`; `rest` is the (optional) open row base.
    Record { fields: Vec<(String, NExpr)>, rest: Option<Box<NExpr>> },
    Proj { obj: Box<NExpr>, field: String },
    /// `` `Tag `` or `` `Tag(payload) ``.
    Tag { label: String, payload: Option<Box<NExpr>> },
    Match { scrut: Box<NExpr>, arms: Vec<NArm> },
    Bin { op: BinOp, left: Box<NExpr>, right: Box<NExpr> },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Arm {
    pub pat: NPat,
    pub body: NExpr,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Pat {
    Wild,
    Tag(String, Option<String>),
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Str(String),
    Ident(String),
    // keywords
    Let, Rec, In, Fun, If, Then, Else, True, False, Match, With, Underscore,
    // punctuation
    LParen, RParen, LBrace, RBrace, Comma, Dot, Eq, Arrow, FatArrow, Bar,
    Backtick, Plus, Minus, Star, Lt,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub start: usize,
    pub end: usize,
}

pub fn lex(src: &str) -> Result<Vec<Token>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i] as char;
        match c {
            ' ' | '\t' | '\r' | '\n' => i += 1,
            '-' if b.get(i + 1) == Some(&b'-') => {
                i += 2;
                while i < b.len() && b[i] != b'\n' {
                    i += 1;
                }
            }
            '-' if b.get(i + 1) == Some(&b'>') => {
                out.push(Token { tok: Tok::Arrow, start: i, end: i + 2 });
                i += 2;
            }
            '=' if b.get(i + 1) == Some(&b'>') => {
                out.push(Token { tok: Tok::FatArrow, start: i, end: i + 2 });
                i += 2;
            }
            '(' if b.get(i + 1) == Some(&b'*') => {
                i += 2;
                let mut depth = 1;
                while i < b.len() && depth > 0 {
                    if b[i] == b'(' && b.get(i + 1) == Some(&b'*') {
                        depth += 1;
                        i += 2;
                    } else if b[i] == b'*' && b.get(i + 1) == Some(&b')') {
                        depth -= 1;
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                if depth != 0 {
                    return Err(ParseError { span: Span::new(b.len(), b.len()), msg: "未闭合的块注释".into() });
                }
            }
            '0'..='9' => {
                let s = i;
                while i < b.len() && b[i].is_ascii_digit() {
                    i += 1;
                }
                let n: i64 = src[s..i].parse().map_err(|_| ParseError {
                    span: Span::new(s, i),
                    msg: "整数字面量超出范围".into(),
                })?;
                out.push(Token { tok: Tok::Int(n), start: s, end: i });
            }
            '"' => {
                let s = i;
                i += 1;
                let mut text = String::new();
                while i < b.len() && b[i] != b'"' {
                    if b[i] == b'\\' && i + 1 < b.len() {
                        let e = b[i + 1];
                        let ch = match e {
                            b'n' => '\n',
                            b't' => '\t',
                            b'\\' => '\\',
                            b'"' => '"',
                            _ => e as char,
                        };
                        text.push(ch);
                        i += 2;
                    } else {
                        text.push(b[i] as char);
                        i += 1;
                    }
                }
                if i >= b.len() {
                    return Err(ParseError { span: Span::new(s, b.len()), msg: "未闭合的字符串".into() });
                }
                i += 1;
                out.push(Token { tok: Tok::Str(text), start: s, end: i });
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let s = i;
                while i < b.len() {
                    let ch = b[i];
                    if ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'\'' {
                        i += 1;
                    } else {
                        break;
                    }
                }
                let word = &src[s..i];
                let tok = match word {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "fun" => Tok::Fun,
                    "if" => Tok::If,
                    "then" => Tok::Then,
                    "else" => Tok::Else,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "match" => Tok::Match,
                    "with" => Tok::With,
                    "_" => Tok::Underscore,
                    _ => Tok::Ident(word.to_string()),
                };
                out.push(Token { tok, start: s, end: i });
            }
            '(' => { out.push(Token { tok: Tok::LParen, start: i, end: i + 1 }); i += 1; }
            ')' => { out.push(Token { tok: Tok::RParen, start: i, end: i + 1 }); i += 1; }
            '{' => { out.push(Token { tok: Tok::LBrace, start: i, end: i + 1 }); i += 1; }
            '}' => { out.push(Token { tok: Tok::RBrace, start: i, end: i + 1 }); i += 1; }
            ',' => { out.push(Token { tok: Tok::Comma, start: i, end: i + 1 }); i += 1; }
            '.' => { out.push(Token { tok: Tok::Dot, start: i, end: i + 1 }); i += 1; }
            '=' => { out.push(Token { tok: Tok::Eq, start: i, end: i + 1 }); i += 1; }
            '|' => { out.push(Token { tok: Tok::Bar, start: i, end: i + 1 }); i += 1; }
            '+' => { out.push(Token { tok: Tok::Plus, start: i, end: i + 1 }); i += 1; }
            '-' => { out.push(Token { tok: Tok::Minus, start: i, end: i + 1 }); i += 1; }
            '*' => { out.push(Token { tok: Tok::Star, start: i, end: i + 1 }); i += 1; }
            '<' => { out.push(Token { tok: Tok::Lt, start: i, end: i + 1 }); i += 1; }
            '`' => { out.push(Token { tok: Tok::Backtick, start: i, end: i + 1 }); i += 1; }
            _ => {
                return Err(ParseError {
                    span: Span::new(i, i + 1),
                    msg: format!("无法识别的字符 {:?}", c),
                });
            }
        }
    }
    out.push(Token { tok: Tok::Eof, start: b.len(), end: b.len() });
    Ok(out)
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

// ---------------------------------------------------------------------------
// Parser (produces placeholder node ids; assign_ids fills path based keys)
// ---------------------------------------------------------------------------

pub struct Parser {
    toks: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(toks: Vec<Token>) -> Self {
        Parser { toks, pos: 0 }
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn peek_tok(&self) -> &Token {
        &self.toks[self.pos]
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.peek() == t {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.peek() == &t {
            Ok(self.bump())
        } else {
            let tk = self.peek_tok();
            Err(ParseError { span: Span::new(tk.start, tk.end), msg: format!("期望 {what}") })
        }
    }
    fn expect_ident(&mut self) -> Result<(String, Token), ParseError> {
        match self.peek().clone() {
            Tok::Ident(s) => {
                let t = self.bump();
                Ok((s, t))
            }
            _ => {
                let tk = self.peek_tok();
                Err(ParseError { span: Span::new(tk.start, tk.end), msg: "期望标识符".into() })
            }
        }
    }

    fn node(&self, inner: Expr, start: usize, end: usize) -> NExpr {
        Node { id: NodeId(0), span: Span::new(start, end), inner }
    }

    pub fn parse_program(mut self) -> Result<NExpr, ParseError> {
        let e = self.parse_expr()?;
        let tk = self.peek_tok();
        if !matches!(tk.tok, Tok::Eof) {
            return Err(ParseError { span: Span::new(tk.start, tk.end), msg: "表达式后有多余内容".into() });
        }
        Ok(e)
    }

    fn parse_expr(&mut self) -> Result<NExpr, ParseError> {
        match self.peek() {
            Tok::Let => self.parse_let(),
            Tok::Fun => self.parse_fun(),
            Tok::If => self.parse_if(),
            Tok::Match => self.parse_match(),
            _ => self.parse_bin(1),
        }
    }

    fn parse_let(&mut self) -> Result<NExpr, ParseError> {
        let kw = self.bump();
        let rec = self.eat(&Tok::Rec);
        let (name, _) = self.expect_ident()?;
        self.expect(Tok::Eq, "'='")?;
        let rhs = self.parse_expr()?;
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr()?;
        let end = body.span.end;
        Ok(self.node(Expr::Let { rec, name, rhs: Box::new(rhs), body: Box::new(body) }, kw.start as usize, end as usize))
    }

    fn parse_fun(&mut self) -> Result<NExpr, ParseError> {
        let kw = self.bump();
        let (param, _) = self.expect_ident()?;
        self.expect(Tok::Arrow, "'->'")?;
        let body = self.parse_expr()?;
        let end = body.span.end;
        Ok(self.node(Expr::Lam { param, body: Box::new(body) }, kw.start as usize, end as usize))
    }

    fn parse_if(&mut self) -> Result<NExpr, ParseError> {
        let kw = self.bump();
        let cond = self.parse_expr()?;
        self.expect(Tok::Then, "'then'")?;
        let then_ = self.parse_expr()?;
        self.expect(Tok::Else, "'else'")?;
        let else_ = self.parse_expr()?;
        let end = else_.span.end;
        Ok(self.node(
            Expr::If { cond: Box::new(cond), then_: Box::new(then_), else_: Box::new(else_) },
            kw.start as usize, end as usize,
        ))
    }

    fn parse_match(&mut self) -> Result<NExpr, ParseError> {
        let kw = self.bump();
        let scrut = self.parse_expr()?;
        self.expect(Tok::With, "'with'")?;
        self.eat(&Tok::Bar);
        let mut arms = Vec::new();
        loop {
            let arm = self.parse_arm()?;
            arms.push(arm);
            if self.eat(&Tok::Bar) {
                continue;
            }
            break;
        }
        let end = arms.last().map(|a| a.span.end).unwrap_or(kw.end as u32);
        Ok(self.node(Expr::Match { scrut: Box::new(scrut), arms }, kw.start as usize, end as usize))
    }

    fn parse_arm(&mut self) -> Result<NArm, ParseError> {
        let start = self.peek_tok().start as usize;
        let pat = self.parse_pat()?;
        self.expect(Tok::FatArrow, "'=>'")?;
        let body = self.parse_expr()?;
        Ok(Node { id: NodeId(0), span: Span::new(start, body.span.end as usize), inner: Arm { pat, body } })
    }

    fn parse_pat(&mut self) -> Result<NPat, ParseError> {
        let start = self.peek_tok().start as usize;
        if self.eat(&Tok::Underscore) {
            let t = self.toks.get(self.pos.saturating_sub(1)).cloned().unwrap();
            return Ok(Node { id: NodeId(0), span: Span::new(start, t.end as usize), inner: Pat::Wild });
        }
        self.expect(Tok::Backtick, "变体标签（以 ` 开头）")?;
        let (label, lt) = self.expect_ident()?;
        let mut end = lt.end as usize;
        let payload = if matches!(self.peek(), Tok::Ident(_) | Tok::Underscore) {
            if self.eat(&Tok::Underscore) {
                end = self.peek_tok().start.max(end) as usize;
                let last = self.toks.get(self.pos.saturating_sub(1)).cloned().unwrap();
                end = last.end as usize;
                Some(String::from("_"))
            } else {
                let (name, nt) = self.expect_ident()?;
                end = nt.end as usize;
                Some(name)
            }
        } else {
            None
        };
        Ok(Node { id: NodeId(0), span: Span::new(start, end), inner: Pat::Tag(label, payload) })
    }

    fn bin_info(t: &Tok) -> Option<(BinOp, u8)> {
        Some(match t {
            Tok::Lt => (BinOp::Lt, 1),
            Tok::Eq => (BinOp::Eq, 1),
            Tok::Plus => (BinOp::Add, 2),
            Tok::Minus => (BinOp::Sub, 2),
            Tok::Star => (BinOp::Mul, 3),
            _ => return None,
        })
    }

    fn parse_bin(&mut self, min_prec: u8) -> Result<NExpr, ParseError> {
        let mut left = self.parse_app()?;
        while let Some((op, prec)) = Self::bin_info(self.peek()) {
            if prec < min_prec {
                break;
            }
            self.bump();
            let right = self.parse_bin(prec + 1)?;
            let start = left.span.start;
            let end = right.span.end;
            left = self.node(
                Expr::Bin { op, left: Box::new(left), right: Box::new(right) },
                start as usize, end as usize,
            );
        }
        Ok(left)
    }

    fn atom_start(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_) | Tok::Str(_) | Tok::Ident(_) | Tok::True | Tok::False
                | Tok::LParen | Tok::LBrace | Tok::Backtick
        )
    }

    fn parse_app(&mut self) -> Result<NExpr, ParseError> {
        let mut fun = self.parse_postfix()?;
        while self.atom_start() {
            let arg = self.parse_postfix()?;
            let start = fun.span.start;
            let end = arg.span.end;
            fun = self.node(
                Expr::App { fun: Box::new(fun), arg: Box::new(arg) },
                start as usize, end as usize,
            );
        }
        Ok(fun)
    }

    fn parse_postfix(&mut self) -> Result<NExpr, ParseError> {
        let mut e = self.parse_atom()?;
        while self.eat(&Tok::Dot) {
            let (field, ft) = self.expect_ident()?;
            let start = e.span.start;
            let end = ft.end;
            e = self.node(Expr::Proj { obj: Box::new(e), field }, start as usize, end as usize);
        }
        Ok(e)
    }

    fn parse_atom(&mut self) -> Result<NExpr, ParseError> {
        let t = self.peek_tok().clone();
        match &t.tok {
            Tok::Int(n) => {
                let n = *n;
                self.bump();
                Ok(self.node(Expr::Int(n), t.start, t.end))
            }
            Tok::Str(s) => {
                let s = s.clone();
                self.bump();
                Ok(self.node(Expr::Str(s), t.start, t.end))
            }
            Tok::True => {
                self.bump();
                Ok(self.node(Expr::Bool(true), t.start, t.end))
            }
            Tok::False => {
                self.bump();
                Ok(self.node(Expr::Bool(false), t.start, t.end))
            }
            Tok::Ident(s) => {
                let s = s.clone();
                self.bump();
                Ok(self.node(Expr::Var(s), t.start, t.end))
            }
            Tok::LParen => {
                self.bump();
                if self.eat(&Tok::RParen) {
                    Ok(self.node(Expr::Unit, t.start, t.end + 1))
                } else {
                    let e = self.parse_expr()?;
                    self.expect(Tok::RParen, "')'")?;
                    Ok(e)
                }
            }
            Tok::LBrace => self.parse_record(t.start),
            Tok::Backtick => self.parse_tag(t.start),
            _ => Err(ParseError { span: Span::new(t.start, t.end), msg: "期望表达式".into() }),
        }
    }

    fn parse_record(&mut self, start: usize) -> Result<NExpr, ParseError> {
        self.expect(Tok::LBrace, "'{'")?;
        let mut fields = Vec::new();
        let mut rest = None;
        loop {
            if self.eat(&Tok::RBrace) {
                break;
            }
            if self.eat(&Tok::Bar) {
                rest = Some(Box::new(self.parse_expr()?));
                self.expect(Tok::RBrace, "'}'")?;
                break;
            }
            let (name, _) = self.expect_ident()?;
            self.expect(Tok::Eq, "'='")?;
            let val = self.parse_expr()?;
            fields.push((name, val));
            if self.eat(&Tok::Comma) {
                continue;
            }
            if self.eat(&Tok::Bar) {
                rest = Some(Box::new(self.parse_expr()?));
                self.expect(Tok::RBrace, "'}'")?;
                break;
            }
            self.expect(Tok::RBrace, "'}' 或 ','")?;
            break;
        }
        let end = self.toks.get(self.pos.saturating_sub(1)).map(|t| t.end).unwrap_or(start + 1);
        Ok(self.node(Expr::Record { fields, rest }, start, end))
    }

    fn parse_tag(&mut self, start: usize) -> Result<NExpr, ParseError> {
        self.expect(Tok::Backtick, "'`'")?;
        let (label, _) = self.expect_ident()?;
        let mut payload = None;
        let mut end = self.toks.get(self.pos.saturating_sub(1)).map(|t| t.end).unwrap_or(start + 1);
        if self.eat(&Tok::LParen) {
            payload = Some(Box::new(self.parse_expr()?));
            let cl = self.expect(Tok::RParen, "')'")?;
            end = cl.end;
        }
        Ok(self.node(Expr::Tag { label, payload }, start, end))
    }
}

// ---------------------------------------------------------------------------
// Path based stable identity
// ---------------------------------------------------------------------------

const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;
const ROOT_SALT: u64 = 0x511c_1d17_e5ab_1e;

fn mix(h: &mut u64, v: u64) {
    *h = (*h ^ v).wrapping_mul(FNV_PRIME);
}

fn path_key(path: &[u64]) -> NodeId {
    let mut h = FNV_OFFSET;
    mix(&mut h, ROOT_SALT);
    for p in path {
        mix(&mut h, *p);
    }
    NodeId(h)
}

fn walk_pat(pat: &mut NPat, path: &mut Vec<u64>) {
    pat.id = path_key(path);
    if let Pat::Tag(_, Some(var)) = &mut pat.inner {
        if var == "_" {
            *var = String::new();
        }
    }
}

fn walk_expr(expr: &mut NExpr, path: &mut Vec<u64>) {
    expr.id = path_key(path);
    match &mut expr.inner {
        Expr::Unit | Expr::Int(_) | Expr::Bool(_) | Expr::Str(_) | Expr::Var(_) => {}
        Expr::Lam { body, .. } => {
            path.push(1);
            walk_expr(body, path);
            path.pop();
        }
        Expr::App { fun, arg } => {
            path.push(1);
            walk_expr(fun, path);
            path.pop();
            path.push(2);
            walk_expr(arg, path);
            path.pop();
        }
        Expr::Let { rhs, body, .. } => {
            path.push(1);
            walk_expr(rhs, path);
            path.pop();
            path.push(2);
            walk_expr(body, path);
            path.pop();
        }
        Expr::If { cond, then_, else_ } => {
            for (slot, sub) in [(1u64, cond.as_mut()), (2, then_.as_mut()), (3, else_.as_mut())] {
                path.push(slot);
                walk_expr(sub, path);
                path.pop();
            }
        }
        Expr::Record { fields, rest } => {
            for (i, (_, sub)) in fields.iter_mut().enumerate() {
                path.push(10 + i as u64);
                walk_expr(sub, path);
                path.pop();
            }
            if let Some(r) = rest {
                path.push(2);
                walk_expr(r, path);
                path.pop();
            }
        }
        Expr::Proj { obj, .. } => {
            path.push(1);
            walk_expr(obj, path);
            path.pop();
        }
        Expr::Tag { payload, .. } => {
            if let Some(p) = payload {
                path.push(1);
                walk_expr(p, path);
                path.pop();
            }
        }
        Expr::Match { scrut, arms } => {
            path.push(1);
            walk_expr(scrut, path);
            path.pop();
            for (i, arm) in arms.iter_mut().enumerate() {
                path.push(10 + i as u64);
                arm.id = path_key(path);
                path.push(1);
                walk_pat(&mut arm.inner.pat, path);
                path.pop();
                path.push(2);
                walk_expr(&mut arm.inner.body, path);
                path.pop();
                path.pop();
            }
        }
        Expr::Bin { left, right, .. } => {
            path.push(1);
            walk_expr(left, path);
            path.pop();
            path.push(2);
            walk_expr(right, path);
            path.pop();
        }
    }
}

pub fn parse(src: &str) -> Result<NExpr, ParseError> {
    let toks = lex(src)?;
    let mut e = Parser::new(toks).parse_program()?;
    let mut path = Vec::new();
    walk_expr(&mut e, &mut path);
    Ok(e)
}

pub fn node_kind(e: &Expr) -> &'static str {
    match e {
        Expr::Unit => "unit",
        Expr::Int(_) => "int",
        Expr::Bool(_) => "bool",
        Expr::Str(_) => "string",
        Expr::Var(_) => "var",
        Expr::Lam { .. } => "lambda",
        Expr::App { .. } => "app",
        Expr::Let { .. } => "let",
        Expr::If { .. } => "if",
        Expr::Record { .. } => "record",
        Expr::Proj { .. } => "proj",
        Expr::Tag { .. } => "tag",
        Expr::Match { .. } => "match",
        Expr::Bin { .. } => "bin",
    }
}

pub fn collect_nodes(expr: &NExpr, out: &mut Vec<(NodeId, Span, &'static str)>) {
    out.push((expr.id, expr.span, node_kind(&expr.inner)));
    match &expr.inner {
        Expr::Lam { body, .. } => collect_nodes(body, out),
        Expr::App { fun, arg } => {
            collect_nodes(fun, out);
            collect_nodes(arg, out);
        }
        Expr::Let { rhs, body, .. } => {
            collect_nodes(rhs, out);
            collect_nodes(body, out);
        }
        Expr::If { cond, then_, else_ } => {
            collect_nodes(cond, out);
            collect_nodes(then_, out);
            collect_nodes(else_, out);
        }
        Expr::Record { fields, rest } => {
            for (_, f) in fields {
                collect_nodes(f, out);
            }
            if let Some(r) = rest {
                collect_nodes(r, out);
            }
        }
        Expr::Proj { obj, .. } => collect_nodes(obj, out),
        Expr::Tag { payload, .. } => {
            if let Some(p) = payload {
                collect_nodes(p, out);
            }
        }
        Expr::Match { scrut, arms } => {
            collect_nodes(scrut, out);
            for a in arms {
                out.push((a.id, a.span, "arm"));
                out.push((a.inner.pat.id, a.inner.pat.span, "pattern"));
                collect_nodes(&a.inner.body, out);
            }
        }
        Expr::Bin { left, right, .. } => {
            collect_nodes(left, out);
            collect_nodes(right, out);
        }
        Expr::Unit | Expr::Int(_) | Expr::Bool(_) | Expr::Str(_) | Expr::Var(_) => {}
    }
}
