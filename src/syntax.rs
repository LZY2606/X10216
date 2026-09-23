//! 词法与语法分析：生成带源码范围 (span) 与稳定路径标识的 AST。
//! 节点身份 = 从根出发的子节点路径（如 "0/1/2"），变量改名不会令路径漂移。

pub type Span = (usize, usize);

#[derive(Clone, Debug)]
pub struct Node {
    pub path: String,
    pub span: Span,
    pub kind: Expr,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i64),
    Str(String),
    Bool(bool),
    Unit,
    Var(String),
    Lam(String, Box<Node>),
    App(Box<Node>, Box<Node>),
    Let { is_rec: bool, name: String, bind: Box<Node>, body: Box<Node> },
    If(Box<Node>, Box<Node>, Box<Node>),
    Record(Vec<(String, Node)>),
    Field(Box<Node>, String),
    Variant(String, Option<Box<Node>>),
    Match(Box<Node>, Vec<Case>),
}

#[derive(Clone, Debug)]
pub struct Case {
    pub tag: String,
    pub binder: Option<String>,
    pub body: Node,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub is_rec: bool,
    pub name: String,
    pub expr: Node,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

// ---------------- Lexer ----------------

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Int(i64),
    Str(String),
    Ident(String),
    UIdent(String),
    Let, Rec, In, Fun, If, Then, Else, Match, True, False,
    LParen, RParen, LBrace, RBrace, Comma, Dot, Eq, Arrow, Bar,
    Eof,
}

fn lex(src: &str) -> Result<Vec<(Tok, Span)>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() { i += 1; continue; }
        if c == '-' && i + 1 < b.len() && b[i + 1] == b'-' {
            while i < b.len() && b[i] != b'\n' { i += 1; }
            continue;
        }
        let start = i;
        let tok = match c {
            '(' => { i += 1; Tok::LParen }
            ')' => { i += 1; Tok::RParen }
            '{' => { i += 1; Tok::LBrace }
            '}' => { i += 1; Tok::RBrace }
            ',' => { i += 1; Tok::Comma }
            '.' => { i += 1; Tok::Dot }
            '=' => { i += 1; Tok::Eq }
            '|' => { i += 1; Tok::Bar }
            '-' => {
                if i + 1 < b.len() && b[i + 1] == b'>' { i += 2; Tok::Arrow }
                else { return Err(ParseError { message: "意外的 '-'".into(), span: (i, i + 1) }); }
            }
            '"' => {
                i += 1;
                let mut s = String::new();
                while i < b.len() && b[i] != b'"' {
                    if b[i] == b'\\' && i + 1 < b.len() {
                        i += 1;
                        s.push(match b[i] as char { 'n' => '\n', 't' => '\t', x => x });
                    } else { s.push(b[i] as char); }
                    i += 1;
                }
                if i >= b.len() { return Err(ParseError { message: "字符串未闭合".into(), span: (start, i) }); }
                i += 1;
                Tok::Str(s)
            }
            c if c.is_ascii_digit() => {
                let mut n: i64 = 0;
                while i < b.len() && (b[i] as char).is_ascii_digit() {
                    n = n * 10 + (b[i] - b'0') as i64; i += 1;
                }
                Tok::Int(n)
            }
            c if c.is_alphabetic() || c == '_' => {
                while i < b.len() && ((b[i] as char).is_alphanumeric() || b[i] == b'_') { i += 1; }
                let w = &src[start..i];
                match w {
                    "let" => Tok::Let, "rec" => Tok::Rec, "in" => Tok::In,
                    "fun" => Tok::Fun, "if" => Tok::If, "then" => Tok::Then,
                    "else" => Tok::Else, "match" => Tok::Match,
                    "true" => Tok::True, "false" => Tok::False,
                    _ if w.chars().next().unwrap().is_uppercase() => Tok::UIdent(w.to_string()),
                    _ => Tok::Ident(w.to_string()),
                }
            }
            _ => return Err(ParseError { message: format!("无法识别的字符 '{}'", c), span: (i, i + 1) }),
        };
        out.push((tok, (start, i)));
    }
    out.push((Tok::Eof, (src.len(), src.len())));
    Ok(out)
}

// ---------------- Parser ----------------

struct Parser {
    toks: Vec<(Tok, Span)>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Tok { &self.toks[self.pos].0 }
    fn span(&self) -> Span { self.toks[self.pos].1 }
    fn next(&mut self) -> (Tok, Span) {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() { self.pos += 1; }
        t
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Span, ParseError> {
        if *self.peek() == t { Ok(self.next().1) } else {
            Err(ParseError { message: format!("期望 {}，实际遇到 {:?}", what, self.peek()), span: self.span() })
        }
    }
    fn ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().clone() {
            Tok::Ident(s) => { let sp = self.next().1; Ok((s, sp)) }
            t => Err(ParseError { message: format!("期望标识符，实际遇到 {:?}", t), span: self.span() }),
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut stmts = Vec::new();
        while *self.peek() != Tok::Eof {
            let idx = stmts.len();
            let (ls, _) = (self.span(), ());
            let start = self.expect(Tok::Let, "let")?.0;
            let _ = ls;
            let is_rec = if *self.peek() == Tok::Rec { self.next(); true } else { false };
            let (name, _) = self.ident()?;
            self.expect(Tok::Eq, "=")?;
            let expr = self.parse_expr(&idx.to_string())?;
            let end = expr.span.1;
            stmts.push(Stmt { is_rec, name, expr, span: (start, end) });
        }
        Ok(Program { stmts })
    }

    fn parse_expr(&mut self, path: &str) -> Result<Node, ParseError> {
        match self.peek().clone() {
            Tok::Let => {
                let (start, _) = (self.next().1.0, ());
                let is_rec = if *self.peek() == Tok::Rec { self.next(); true } else { false };
                let (name, _) = self.ident()?;
                self.expect(Tok::Eq, "=")?;
                let bind = self.parse_expr(&format!("{}/0", path))?;
                self.expect(Tok::In, "in")?;
                let body = self.parse_expr(&format!("{}/1", path))?;
                let end = body.span.1;
                Ok(Node { path: path.into(), span: (start, end), kind: Expr::Let { is_rec, name, bind: Box::new(bind), body: Box::new(body) } })
            }
            Tok::Fun => {
                let start = self.next().1.0;
                let (param, _) = self.ident()?;
                self.expect(Tok::Arrow, "->")?;
                let body = self.parse_expr(&format!("{}/0", path))?;
                let end = body.span.1;
                Ok(Node { path: path.into(), span: (start, end), kind: Expr::Lam(param, Box::new(body)) })
            }
            Tok::If => {
                let start = self.next().1.0;
                let c = self.parse_expr(&format!("{}/0", path))?;
                self.expect(Tok::Then, "then")?;
                let t = self.parse_expr(&format!("{}/1", path))?;
                self.expect(Tok::Else, "else")?;
                let e = self.parse_expr(&format!("{}/2", path))?;
                let end = e.span.1;
                Ok(Node { path: path.into(), span: (start, end), kind: Expr::If(Box::new(c), Box::new(t), Box::new(e)) })
            }
            Tok::Match => {
                let start = self.next().1.0;
                let scrut = self.parse_expr(&format!("{}/0", path))?;
                self.expect(Tok::LBrace, "{")?;
                let mut cases = Vec::new();
                while *self.peek() == Tok::Bar {
                    let cstart = self.next().1.0;
                    let tag = match self.next() {
                        (Tok::UIdent(t), _) => t,
                        (t, sp) => return Err(ParseError { message: format!("期望变体标签，实际遇到 {:?}", t), span: sp }),
                    };
                    let binder = match self.peek().clone() {
                        Tok::Ident(s) => { self.next(); Some(s) }
                        _ => None,
                    };
                    self.expect(Tok::Arrow, "->")?;
                    let body = self.parse_expr(&format!("{}/{}/0", path, cases.len() + 1))?;
                    let cend = body.span.1;
                    cases.push(Case { tag, binder, body, span: (cstart, cend) });
                }
                let end = self.expect(Tok::RBrace, "}")?.1;
                Ok(Node { path: path.into(), span: (start, end), kind: Expr::Match(Box::new(scrut), cases) })
            }
            _ => self.parse_app(path),
        }
    }

    fn atom_starts(&self) -> bool {
        matches!(self.peek(), Tok::Int(_) | Tok::Str(_) | Tok::Ident(_) | Tok::UIdent(_)
            | Tok::True | Tok::False | Tok::LParen | Tok::LBrace)
    }

    fn parse_app(&mut self, path: &str) -> Result<Node, ParseError> {
        let mut f = self.parse_postfix(&format!("{}/0", path))?;
        let mut nargs = 0usize;
        while self.atom_starts() {
            nargs += 1;
            let a = self.parse_postfix(&format!("{}/{}", path, nargs))?;
            let span = (f.span.0, a.span.1);
            f = Node { path: path.into(), span, kind: Expr::App(Box::new(f), Box::new(a)) };
        }
        Ok(f)
    }

    fn parse_postfix(&mut self, path: &str) -> Result<Node, ParseError> {
        let mut e = self.parse_atom(&format!("{}/0", path))?;
        let mut n = 0usize;
        while *self.peek() == Tok::Dot {
            self.next();
            let (field, _) = self.ident()?;
            n += 1;
            let span = (e.span.0, self.toks[self.pos.saturating_sub(1)].1.1);
            e = Node { path: path.into(), span, kind: Expr::Field(Box::new(e), field) };
        }
        Ok(e)
    }

    fn parse_atom(&mut self, path: &str) -> Result<Node, ParseError> {
        let (tok, sp) = self.next();
        let kind = match tok {
            Tok::Int(n) => Expr::Int(n),
            Tok::Str(s) => Expr::Str(s),
            Tok::True => Expr::Bool(true),
            Tok::False => Expr::Bool(false),
            Tok::Ident(s) => Expr::Var(s),
            Tok::UIdent(tag) => {
                let arg = if self.atom_starts() {
                    Some(Box::new(self.parse_atom(&format!("{}/0", path))?))
                } else { None };
                let end = arg.as_ref().map(|a| a.span.1).unwrap_or(sp.1);
                return Ok(Node { path: path.into(), span: (sp.0, end), kind: Expr::Variant(tag, arg) });
            }
            Tok::LParen => {
                if *self.peek() == Tok::RParen {
                    let end = self.next().1.1;
                    return Ok(Node { path: path.into(), span: (sp.0, end), kind: Expr::Unit });
                }
                let mut e = self.parse_expr(path)?;
                let end = self.expect(Tok::RParen, ")")?.1;
                e.span = (sp.0, end);
                return Ok(e);
            }
            Tok::LBrace => {
                let mut fields = Vec::new();
                if *self.peek() != Tok::RBrace {
                    loop {
                        let (fname, _) = self.ident()?;
                        self.expect(Tok::Eq, "=")?;
                        let fexpr = self.parse_expr(&format!("{}/{}", path, fields.len()))?;
                        fields.push((fname, fexpr));
                        if *self.peek() == Tok::Comma { self.next(); } else { break; }
                    }
                }
                let end = self.expect(Tok::RBrace, "}")?.1;
                return Ok(Node { path: path.into(), span: (sp.0, end), kind: Expr::Record(fields) });
            }
            t => return Err(ParseError { message: format!("期望表达式，实际遇到 {:?}", t), span: sp }),
        };
        Ok(Node { path: path.into(), span: sp, kind })
    }
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    let toks = lex(src)?;
    let mut p = Parser { toks, pos: 0 };
    p.parse_program()
}
