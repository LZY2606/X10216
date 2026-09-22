// Lexer + parser for the small HM language used by the slicing bench.
// Nothing here does inference; it only produces a raw, source-spanned AST.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
    pub fn join(self, other: Span) -> Span {
        Span { lo: self.lo.min(other.lo), hi: self.hi.max(other.hi) }
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl ParseError {
    fn at(lo: usize, hi: usize, message: impl Into<String>) -> Self {
        ParseError { span: Span::new(lo, hi), message: message.into() }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Str(String),
    Ident(String),
    Label(String),
    Tag(String),
    // punctuation
    LParen, RParen, LBrace, RBrace, Comma, Semicolon, Dot, Arrow,
    Eq, EqEq, NotEq, Lt, Le, Gt, Ge, Plus, Minus, Star, Slash, Percent,
    AndAnd, OrOr, Bang, Backslash,
    // keywords
    Let, Rec, In, Fn, If, Then, Else, True, False, Match, With, End,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub lo: usize,
    pub hi: usize,
}

pub fn lex(src: &str) -> Result<Vec<Token>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    loop {
        while i < b.len() && (b[i] as char).is_whitespace() {
            i += 1;
        }
        if i >= b.len() {
            break;
        }
        let lo = i;
        let c = b[i] as char;
        // comments
        if c == '/' && i + 1 < b.len() && b[i + 1] == b'/' {
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        if c == '/' && i + 1 < b.len() && b[i + 1] == b'*' {
            i += 2;
            let mut depth = 1;
            while i < b.len() && depth > 0 {
                if i + 1 < b.len() && b[i] == b'/' && b[i + 1] == b'*' {
                    depth += 1;
                    i += 2;
                } else if i + 1 < b.len() && b[i] == b'*' && b[i + 1] == b'/' {
                    depth -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if depth > 0 {
                return Err(ParseError::at(lo, i, "unterminated block comment"));
            }
            continue;
        }
        if c.is_ascii_digit() {
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[lo..i].parse().map_err(|_| {
                ParseError::at(lo, i, "integer literal out of range")
            })?;
            out.push(Token { tok: Tok::Int(n), lo, hi: i });
            continue;
        }
        if c == '"' {
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1];
                    match e {
                        b'n' => s.push('\n'),
                        b't' => s.push('\t'),
                        b'"' => s.push('"'),
                        b'\\' => s.push('\\'),
                        _ => return Err(ParseError::at(i, i + 2, "bad escape")),
                    }
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(ParseError::at(lo, i, "unterminated string"));
            }
            i += 1;
            out.push(Token { tok: Tok::Str(s), lo, hi: i });
            continue;
        }
        if c == '`' {
            i += 1;
            let s0 = i;
            while i < b.len() && is_ident_cont(b[i] as char) {
                i += 1;
            }
            if i == s0 {
                return Err(ParseError::at(lo, i + 1, "empty tag"));
            }
            out.push(Token { tok: Tok::Tag(src[s0..i].to_string()), lo, hi: i });
            continue;
        }
        if c == '_' || c.is_ascii_alphabetic() {
            i += 1;
            while i < b.len() && is_ident_cont(b[i] as char) {
                i += 1;
            }
            let word = &src[lo..i];
            let t = match word {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fn" => Tok::Fn,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "true" => Tok::True,
                "false" => Tok::False,
                "match" => Tok::Match,
                "with" => Tok::With,
                "end" => Tok::End,
                other => Tok::Ident(other.to_string()),
            };
            out.push(Token { tok: t, lo, hi: i });
            continue;
        }
        macro_rules! p1 {
            ($t:expr) => {{
                i += 1;
                out.push(Token { tok: $t, lo, hi: i });
                continue;
            }};
        }
        macro_rules! p2 {
            ($ch:expr, $t:expr, $u:expr) => {{
                if i + 1 < b.len() && b[i + 1] == $ch {
                    i += 2;
                    out.push(Token { tok: $t, lo, hi: i });
                } else {
                    i += 1;
                    out.push(Token { tok: $u, lo, hi: i });
                }
                continue;
            }};
        }
        match c {
            '(' => p1!(Tok::LParen),
            ')' => p1!(Tok::RParen),
            '{' => p1!(Tok::LBrace),
            '}' => p1!(Tok::RBrace),
            ',' => p1!(Tok::Comma),
            ';' => p1!(Tok::Semicolon),
            '.' => p1!(Tok::Dot),
            '+' => p1!(Tok::Plus),
            '-' => p2!(b'>', Tok::Arrow, Tok::Minus),
            '*' => p1!(Tok::Star),
            '/' => p1!(Tok::Slash),
            '%' => p1!(Tok::Percent),
            '<' => {
                if i + 1 < b.len() && b[i + 1] == b'=' {
                    i += 2;
                    out.push(Token { tok: Tok::Le, lo, hi: i });
                } else {
                    i += 1;
                    out.push(Token { tok: Tok::Lt, lo, hi: i });
                }
                continue;
            }
            '>' => {
                if i + 1 < b.len() && b[i + 1] == b'=' {
                    i += 2;
                    out.push(Token { tok: Tok::Ge, lo, hi: i });
                } else {
                    i += 1;
                    out.push(Token { tok: Tok::Gt, lo, hi: i });
                }
                continue;
            }
            '=' => p2!(b'=', Tok::EqEq, Tok::Eq),
            '!' => p2!(b'=', Tok::NotEq, Tok::Bang),
            '&' => {
                if i + 1 < b.len() && b[i + 1] == b'&' {
                    i += 2;
                    out.push(Token { tok: Tok::AndAnd, lo, hi: i });
                    continue;
                }
                return Err(ParseError::at(i, i + 1, "expected `&&`"));
            }
            '|' => {
                if i + 1 < b.len() && b[i + 1] == b'|' {
                    i += 2;
                    out.push(Token { tok: Tok::OrOr, lo, hi: i });
                    continue;
                }
                return Err(ParseError::at(i, i + 1, "expected `||`"));
            }
            '\\' => p1!(Tok::Backslash),
            _ => return Err(ParseError::at(i, i + 1, format!("unexpected character `{}`", c))),
        }
    }
    out.push(Token { tok: Tok::Eof, lo: src.len(), hi: src.len() });
    Ok(out)
}

fn is_ident_cont(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric() || c == '\''
}

// ---------------------------------------------------------------- AST

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod, Eq, Neq, Lt, Le, Gt, Ge, And, Or,
}

#[derive(Clone, Debug)]
pub enum FieldInit {
    Punct { name: String, val: Spanned<Expr> },
    Shorthand { name: String, span: Span },
    Spread { alias: Option<String>, val: Spanned<Expr>, span: Span },
}

impl FieldInit {
    pub fn span(&self) -> Span {
        match self {
            FieldInit::Punct { val, .. } => val.span,
            FieldInit::Shorthand { span, .. } => *span,
            FieldInit::Spread { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Wild,
    Var(String),
    Tag { tag: String, payload: Option<Box<Spanned<Pattern>>> },
}

#[derive(Clone, Debug)]
pub struct Clause {
    pub pat: Spanned<Pattern>,
    pub body: Spanned<Expr>,
    pub span: Span,
}

pub struct LetStmt {
    pub rec: bool,
    pub name: String,
    pub args: Vec<String>,
    pub value: Spanned<Expr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Str(String),
    Unit,
    Var(String),
    Bin { op: BinOp, l: Box<Spanned<Expr>>, r: Box<Spanned<Expr>> },
    Neg(Box<Spanned<Expr>>),
    Not(Box<Spanned<Expr>>),
    Lam { args: Vec<String>, body: Box<Spanned<Expr>> },
    App { fun: Box<Spanned<Expr>>, arg: Box<Spanned<Expr>> },
    Let {
        rec: bool,
        name: String,
        args: Vec<String>,
        value: Box<Spanned<Expr>>,
        body: Box<Spanned<Expr>>,
    },
    If {
        cond: Box<Spanned<Expr>>,
        then_e: Box<Spanned<Expr>>,
        else_e: Box<Spanned<Expr>>,
    },
    Record { fields: Vec<FieldInit> },
    Extend { alias: Option<String>, fields: Vec<FieldInit>, base: Box<Spanned<Expr>> },
    Select { obj: Box<Spanned<Expr>>, label: String },
    Variant { tag: String, payload: Option<Box<Spanned<Expr>>> },
    Match { scrut: Box<Spanned<Expr>>, clauses: Vec<Clause>, has_default: bool },
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

pub fn parse(src: &str) -> Result<Spanned<Expr>, ParseError> {
    let toks = lex(src)?;
    let mut p = Parser { src, toks, pos: 0 };
    let e = p.parse_seq_block(&[])?;
    if !matches!(p.peek().tok, Tok::Eof) {
        let t = p.peek();
        return Err(ParseError::at(t.lo, t.hi, format!("unexpected token after expression: {}", tok_name(&t.tok))));
    }
    Ok(e)
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Int(_) => "integer".into(),
        Tok::Bool(_) => "boolean".into(),
        Tok::Str(_) => "string".into(),
        Tok::Ident(s) => format!("identifier `{}`", s),
        Tok::Label(s) | Tok::Tag(s) => format!("`{}`", s),
        Tok::Eof => "end of input".into(),
        other => format!("`{}`", punct(other)),
    }
}

fn punct(t: &Tok) -> &'static str {
    match t {
        Tok::LParen => "(", Tok::RParen => ")", Tok::LBrace => "{", Tok::RBrace => "}",
        Tok::Comma => ",", Tok::Semicolon => ";", Tok::Dot => ".", Tok::Arrow => "->",
        Tok::Eq => "=", Tok::EqEq => "==", Tok::NotEq => "!=", Tok::Lt => "<",
        Tok::Le => "<=", Tok::Gt => ">", Tok::Ge => ">=", Tok::Plus => "+",
        Tok::Minus => "-", Tok::Star => "*", Tok::Slash => "/", Tok::Percent => "%",
        Tok::AndAnd => "&&", Tok::OrOr => "||", Tok::Bang => "!", Tok::Backslash => "\\",
        Tok::Let => "let", Tok::Rec => "rec", Tok::In => "in", Tok::Fn => "fn",
        Tok::If => "if", Tok::Then => "then", Tok::Else => "else", Tok::True => "true",
        Tok::False => "false", Tok::Match => "match", Tok::With => "with", Tok::End => "end",
        Tok::Bool(_) | Tok::Int(_) | Tok::Str(_) | Tok::Ident(_) | Tok::Label(_)
        | Tok::Tag(_) | Tok::Eof => "?",
    }
}

struct Parser<'a> {
    src: &'a str,
    toks: Vec<Token>,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Token {
        &self.toks[self.pos]
    }
    fn peek2(&self) -> &Token {
        self.toks.get(self.pos + 1).unwrap_or_else(|| self.toks.last().unwrap())
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
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
    fn expect(&mut self, t: Tok) -> Result<Token, ParseError> {
        if std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(&t) {
            Ok(self.bump())
        } else {
            let x = self.peek().clone();
            Err(ParseError::at(x.lo, x.hi, format!("expected {}, found {}", tok_name(&t), tok_name(&x.tok))))
        }
    }
}

impl<'a> Parser<'a> {
    // Block with let statements (separated/terminated by ';') and a final
    // expression. Terminators (')', 'end', ...) only end the block.
    fn parse_seq_block(&mut self, terminators: &[Tok]) -> Result<Spanned<Expr>, ParseError> {
        let mut stmts: Vec<Spanned<LetStmt>> = Vec::new();
        loop {
            let at_end = matches!(self.peek().tok, Tok::Eof)
                || terminators.iter().any(|t| {
                    std::mem::discriminant(t) == std::mem::discriminant(&self.peek().tok)
                });
            if at_end || !matches!(self.peek().tok, Tok::Let) {
                break;
            }
            let s = self.parse_let_stmt()?;
            stmts.push(s);
            self.eat(&Tok::Semicolon);
        }
        let mut result = self.parse_expr()?;
        while let Some(s) = stmts.pop() {
            let LetStmt { rec, name, args, value, span } = s.node;
            let lo = span.lo;
            let value = if args.is_empty() {
                value
            } else {
                wrap_fn(args, value)
            };
            result = Spanned {
                span: Span { lo, hi: result.span.hi },
                node: Expr::Let {
                    rec,
                    name,
                    args: vec![],
                    value: Box::new(value),
                    body: Box::new(result),
                },
            };
        }
        Ok(result)
    }

    fn parse_let_stmt(&mut self) -> Result<Spanned<LetStmt>, ParseError> {
        let lo = self.peek().lo;
        self.expect(Tok::Let)?;
        let rec = self.eat(&Tok::Rec);
        let name = self.expect_ident()?;
        let mut args = Vec::new();
        while let Tok::Ident(_) = self.peek().tok {
            if matches!(self.peek2().tok, Tok::Eq | Tok::In) {
                break;
            }
            args.push(self.expect_ident()?);
        }
        self.expect(Tok::Eq)?;
        let value = self.parse_expr()?;
        let span = Span::new(lo, value.span.hi as usize);
        Ok(Spanned {
            span,
            node: LetStmt { rec, name, args, value, span },
        })
    }

    fn expect_ident(&mut self) -> Result<String, ParseError> {
        let x = self.peek().clone();
        match self.bump().tok {
            Tok::Ident(s) => Ok(s),
            _ => Err(ParseError::at(
                x.lo,
                x.hi,
                format!("expected identifier, found {}", tok_name(&x.tok)),
            )),
        }
    }
}

fn wrap_fn(args: Vec<String>, body: Spanned<Expr>) -> Spanned<Expr> {
    // span is taken from the let, fixed up by the caller; approximate here
    let lo = body.span.lo;
    build_lam(lo as usize, args, body)
}

fn build_lam(lo: usize, args: Vec<String>, body: Spanned<Expr>) -> Spanned<Expr> {
    let hi = body.span.hi;
    let mut e = body;
    for arg in args.into_iter().rev() {
        e = Spanned {
            span: Span::new(lo, hi as usize),
            node: Expr::Lam { args: vec![arg], body: Box::new(e) },
        };
    }
    e
}

impl<'a> Parser<'a> {
    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        // `let ... in` expressions
        if matches!(self.peek().tok, Tok::Let) {
            return self.parse_let_in();
        }
        if matches!(self.peek().tok, Tok::Fn | Tok::Backslash) {
            return self.parse_lam();
        }
        if matches!(self.peek().tok, Tok::If) {
            return self.parse_if();
        }
        if matches!(self.peek().tok, Tok::Match) {
            return self.parse_match();
        }
        self.parse_binop(0)
    }

    fn parse_let_in(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let lo = self.peek().lo;
        self.expect(Tok::Let)?;
        let rec = self.eat(&Tok::Rec);
        let name = self.expect_ident()?;
        let mut args = Vec::new();
        while let Tok::Ident(_) = self.peek().tok {
            if matches!(self.peek2().tok, Tok::Eq | Tok::In) {
                break;
            }
            args.push(self.expect_ident()?);
        }
        self.expect(Tok::Eq)?;
        let value = self.parse_expr()?;
        self.expect(Tok::In)?;
        let body = self.parse_expr()?;
        let value = if args.is_empty() { value } else { wrap_fn(args, value) };
        Ok(Spanned {
            span: Span::new(lo, body.span.hi as usize),
            node: Expr::Let { rec, name, args: vec![], value: Box::new(value), body: Box::new(body) },
        })
    }

    fn parse_lam(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let lo = self.peek().lo;
        self.bump(); // fn or \
        let mut args = Vec::new();
        loop {
            match &self.peek().tok {
                Tok::Ident(s) => {
                    args.push(s.clone());
                    self.bump();
                }
                Tok::Arrow => break,
                _ => {
                    let x = self.peek().clone();
                    return Err(ParseError::at(
                        x.lo, x.hi, format!("expected parameter or `->`, found {}", tok_name(&x.tok)),
                    ));
                }
            }
        }
        self.expect(Tok::Arrow)?;
        let body = self.parse_expr()?;
        Ok(build_lam(lo, args, body))
    }

    fn parse_if(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let lo = self.peek().lo;
        self.expect(Tok::If)?;
        let cond = self.parse_expr()?;
        self.expect(Tok::Then)?;
        let then_e = self.parse_expr()?;
        self.expect(Tok::Else)?;
        let else_e = self.parse_expr()?;
        Ok(Spanned {
            span: Span::new(lo, else_e.span.hi as usize),
            node: Expr::If { cond: Box::new(cond), then_e: Box::new(then_e), else_e: Box::new(else_e) },
        })
    }

    fn parse_match(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let lo = self.peek().lo;
        self.expect(Tok::Match)?;
        let scrut = self.parse_expr()?;
        self.expect(Tok::With)?;
        let mut clauses = Vec::new();
        let mut has_default = false;
        loop {
            if matches!(self.peek().tok, Tok::End) {
                break;
            }
            let clo = self.peek().lo;
            let pat = self.parse_pattern()?;
            if matches!(pat.node, Pattern::Wild) {
                has_default = true;
            }
            self.expect(Tok::Arrow)?;
            let body = self.parse_clause_body()?;
            let span = Span::new(clo, body.span.hi as usize);
            clauses.push(Clause { pat, body, span });
            if !self.eat(&Tok::Comma) && !self.eat(&Tok::Semicolon) {
                break;
            }
        }
        self.expect(Tok::End)?;
        if clauses.is_empty() {
            return Err(ParseError::at(lo, self.peek().lo, "match needs at least one clause"));
        }
        Ok(Spanned {
            span: Span::new(lo, self.toks[self.pos.min(self.toks.len() - 1)].lo),
            node: Expr::Match { scrut: Box::new(scrut), clauses, has_default },
        })
    }

    // Clause bodies are terminated by clause separators or `end`.
    fn parse_clause_body(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_seq_block(&[Tok::Comma, Tok::Semicolon, Tok::End])
    }

    fn parse_pattern(&mut self) -> Result<Spanned<Pattern>, ParseError> {
        let t = self.peek().clone();
        match t.tok {
            Tok::Ident(s) if s == "_" => {
                self.bump();
                Ok(Spanned { span: Span::new(t.lo, t.hi), node: Pattern::Wild })
            }
            Tok::Ident(s) => {
                self.bump();
                Ok(Spanned { span: Span::new(t.lo, t.hi), node: Pattern::Var(s) })
            }
            Tok::Tag(tag) => {
                self.bump();
                let payload = if matches!(self.peek().tok, Tok::LParen | Tok::Ident(_)) {
                    // tag(x) or tag x; only the parenthesized/atom form is supported
                    if self.eat(&Tok::LParen) {
                        let p = self.parse_pattern()?;
                        self.expect(Tok::RParen)?;
                        Some(Box::new(p))
                    } else {
                        let p = self.parse_pattern()?;
                        Some(Box::new(p))
                    }
                } else {
                    None
                };
                let hi = payload.as_ref().map(|p| p.span.hi).unwrap_or(t.hi as u32);
                Ok(Spanned { span: Span::new(t.lo, hi as usize), node: Pattern::Tag { tag, payload } })
            }
            _ => Err(ParseError::at(t.lo, t.hi, format!("expected a pattern, found {}", tok_name(&t.tok)))),
        }
    }
}

fn binop_info(t: &Tok) -> Option<(BinOp, u8, bool)> {
    Some(match t {
        Tok::OrOr => (BinOp::Or, 1, true),
        Tok::AndAnd => (BinOp::And, 2, true),
        Tok::EqEq => (BinOp::Eq, 3, false),
        Tok::NotEq => (BinOp::Neq, 3, false),
        Tok::Lt => (BinOp::Lt, 4, false),
        Tok::Le => (BinOp::Le, 4, false),
        Tok::Gt => (BinOp::Gt, 4, false),
        Tok::Ge => (BinOp::Ge, 4, false),
        Tok::Plus => (BinOp::Add, 5, false),
        Tok::Minus => (BinOp::Sub, 5, false),
        Tok::Star => (BinOp::Mul, 6, false),
        Tok::Slash => (BinOp::Div, 6, false),
        Tok::Percent => (BinOp::Mod, 6, false),
        _ => return None,
    })
}

impl<'a> Parser<'a> {
    fn parse_binop(&mut self, min_bp: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut left = self.parse_unary()?;
        loop {
            let (op, bp, right_assoc) = match binop_info(&self.peek().tok) {
                Some(x) => x,
                None => break,
            };
            if bp < min_bp {
                break;
            }
            let t = self.bump();
            let next_min = if right_assoc { bp } else { bp + 1 };
            let right = self.parse_binop(next_min)?;
            left = Spanned {
                span: Span::new(left.span.lo as usize, right.span.hi as usize),
                node: Expr::Bin { op, l: Box::new(left), r: Box::new(right) },
            };
            let _ = t;
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Spanned<Expr>, ParseError> {
        if matches!(self.peek().tok, Tok::Minus) {
            let t = self.bump();
            let e = self.parse_unary()?;
            return Ok(Spanned {
                span: Span::new(t.lo, e.span.hi as usize),
                node: Expr::Neg(Box::new(e)),
            });
        }
        if matches!(self.peek().tok, Tok::Bang) {
            let t = self.bump();
            let e = self.parse_unary()?;
            return Ok(Spanned {
                span: Span::new(t.lo, e.span.hi as usize),
                node: Expr::Not(Box::new(e)),
            });
        }
        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut e = self.parse_atom()?;
        loop {
            if matches!(self.peek().tok, Tok::Dot) {
                let dot = self.bump();
                let lab = self.expect_label()?;
                e = Spanned {
                    span: Span::new(e.span.lo as usize, dot.hi + lab.len()),
                    node: Expr::Select { obj: Box::new(e), label: lab.clone() },
                };
                let _ = dot;
                continue;
            }
            // application: atom following an atom
            if self.starts_atom() {
                let arg = self.parse_atom()?;
                e = Spanned {
                    span: Span::new(e.span.lo as usize, arg.span.hi as usize),
                    node: Expr::App { fun: Box::new(e), arg: Box::new(arg) },
                };
                continue;
            }
            break;
        }
        Ok(e)
    }

    fn expect_label(&mut self) -> Result<String, ParseError> {
        // labels are ordinary identifiers after a dot
        let x = self.peek().clone();
        match self.bump().tok {
            Tok::Ident(s) => Ok(s),
            _ => Err(ParseError::at(x.lo, x.hi, format!("expected field name, found {}", tok_name(&x.tok)))),
        }
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek().tok,
            Tok::Int(_)
                | Tok::Str(_)
                | Tok::True
                | Tok::False
                | Tok::Ident(_)
                | Tok::Tag(_)
                | Tok::LParen
                | Tok::LBrace
        )
    }

    fn parse_atom(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        match t.tok {
            Tok::Int(n) => {
                self.bump();
                Ok(Spanned { span: Span::new(t.lo, t.hi), node: Expr::Int(n) })
            }
            Tok::Str(s) => {
                self.bump();
                Ok(Spanned { span: Span::new(t.lo, t.hi), node: Expr::Str(s) })
            }
            Tok::True => {
                self.bump();
                Ok(Spanned { span: Span::new(t.lo, t.hi), node: Expr::Bool(true) })
            }
            Tok::False => {
                self.bump();
                Ok(Spanned { span: Span::new(t.lo, t.hi), node: Expr::Bool(false) })
            }
            Tok::Ident(s) => {
                self.bump();
                Ok(Spanned { span: Span::new(t.lo, t.hi), node: Expr::Var(s) })
            }
            Tok::LParen => self.parse_paren(),
            Tok::LBrace => self.parse_record(),
            Tok::Tag(tag) => {
                self.bump();
                let payload = if self.starts_atom() {
                    Some(Box::new(self.parse_atom()?))
                } else {
                    None
                };
                let hi = payload.as_ref().map(|p| p.span.hi).unwrap_or(t.hi as u32);
                Ok(Spanned { span: Span::new(t.lo, hi as usize), node: Expr::Variant { tag, payload } })
            }
            _ => Err(ParseError::at(t.lo, t.hi, format!("expected an expression, found {}", tok_name(&t.tok)))),
        }
    }

    fn parse_paren(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let lo = self.peek().lo;
        self.expect(Tok::LParen)?;
        if self.eat(&Tok::RParen) {
            return Ok(Spanned { span: Span::new(lo, lo + 2), node: Expr::Unit });
        }
        let e = self.parse_seq_block(&[Tok::RParen])?;
        let close = self.expect(Tok::RParen)?;
        let mut span = e.span;
        span.lo = lo as u32;
        span.hi = close.hi as u32;
        Ok(Spanned { span, node: e.node })
    }
}

impl<'a> Parser<'a> {
    // { name, other = expr, ..base }
    fn parse_record(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let lo = self.peek().lo;
        self.expect(Tok::LBrace)?;
        let mut fields: Vec<FieldInit> = Vec::new();
        let mut base: Option<Spanned<Expr>> = None;
        loop {
            if matches!(self.peek().tok, Tok::RBrace) {
                break;
            }
            // spread: `..expr`
            if matches!(self.peek().tok, Tok::Dot) && matches!(self.peek2().tok, Tok::Dot) {
                let d1 = self.bump();
                let d2 = self.bump();
                let span = Span::new(d1.lo, d2.hi);
                let val = self.parse_atom()?;
                if base.is_some() {
                    return Err(ParseError::at(span.lo as usize, span.hi as usize, "only one `..base` spread is allowed"));
                }
                base = Some(val);
            } else {
                let name_tok = self.peek().clone();
                let name = match name_tok.tok {
                    Tok::Ident(s) => {
                        self.bump();
                        s
                    }
                    _ => {
                        return Err(ParseError::at(
                            name_tok.lo,
                            name_tok.hi,
                            format!("expected field name, found {}", tok_name(&name_tok.tok)),
                        ))
                    }
                };
                if self.eat(&Tok::Eq) {
                    let val = self.parse_expr()?;
                    fields.push(FieldInit::Punct { name, val });
                } else {
                    fields.push(FieldInit::Shorthand {
                        name,
                        span: Span::new(name_tok.lo, name_tok.hi),
                    });
                }
            }
            if !self.eat(&Tok::Comma) && !self.eat(&Tok::Semicolon) {
                break;
            }
        }
        let close = self.expect(Tok::RBrace)?;
        let node = match base {
            Some(base) => Expr::Extend {
                alias: None,
                fields,
                base: Box::new(base),
            },
            None => Expr::Record { fields },
        };
        Ok(Spanned { span: Span::new(lo, close.hi), node })
    }
}
