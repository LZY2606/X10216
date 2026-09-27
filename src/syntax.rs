// Lexer, AST and parser for the tiny HM-style language, with source spans.
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
    pub fn merge(&self, other: Span) -> Span {
        Span { lo: self.lo.min(other.lo), hi: self.hi.max(other.hi) }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokKind {
    Int(i64), Str(String), Ident(String), Tag(String),
    Let, Rec, In, If, Then, Else, Match, With, True, False, Fun,
    Arrow, Eq, Plus, Minus, Star, Backslash,
    LParen, RParen, LBrace, RBrace, Dot, Comma, Pipe,
    Underscore, Equal, Colon2, Eof,
}

#[derive(Clone, Debug)]
pub struct Token { pub kind: TokKind, pub span: Span }

#[derive(Clone, Debug)]
pub struct ParseError { pub message: String, pub span: Span }

pub struct Lexer<'a> { s: &'a str, p: usize }

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self { Lexer { s, p: 0 } }
    fn peek(&self) -> Option<char> { self.s[self.p..].chars().next() }
    fn peek2(&self) -> Option<char> { self.s[self.p..].chars().nth(1) }
    fn starts(&self, t: &str) -> bool { self.s[self.p..].starts_with(t) }
    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.p += c.len_utf8();
        Some(c)
    }

    pub fn lex(mut self) -> Result<Vec<Token>, ParseError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            let lo = self.p;
            let Some(c) = self.peek() else {
                out.push(Token { kind: TokKind::Eof, span: Span::new(lo, lo) });
                return Ok(out);
            };
            if c.is_ascii_digit() {
                let mut n = 0i64;
                while let Some(d) = self.peek() && d.is_ascii_digit() {
                    n = n.wrapping_mul(10).wrapping_add((d as u8 - b'0') as i64);
                    self.p += 1;
                }
                out.push(Token { kind: TokKind::Int(n), span: Span::new(lo, self.p) });
            } else if c == '"' {
                self.p += 1;
                let mut s = String::new();
                loop {
                    match self.bump() {
                        None => return Err(ParseError { message: "字符串未结束".into(), span: Span::new(lo, self.p) }),
                        Some('"') => break,
                        Some('\\') => {
                            let e = self.bump().ok_or(ParseError { message: "字符串转义未结束".into(), span: Span::new(lo, self.p) })?;
                            match e {
                                'n' => s.push('\n'), 't' => s.push('\t'),
                                '\\' => s.push('\\'), '"' => s.push('"'),
                                o => s.push(o),
                            }
                        }
                        Some(ch) => s.push(ch),
                    }
                }
                out.push(Token { kind: TokKind::Str(s), span: Span::new(lo, self.p) });
            } else if c == '#' {
                self.p += 1;
                let start = self.p;
                while let Some(d) = self.peek() && (d.is_ascii_alphanumeric() || d == '_') {
                    self.p += d.len_utf8();
                }
                if self.p == start {
                    return Err(ParseError { message: "# 后缺少变体标签名".into(), span: Span::new(lo, self.p) });
                }
                let name = self.s[start..self.p].to_string();
                out.push(Token { kind: TokKind::Tag(name), span: Span::new(lo, self.p) });
            } else if c.is_ascii_alphabetic() || c == '_' {
                let start = self.p;
                while let Some(d) = self.peek() && (d.is_ascii_alphanumeric() || d == '_') {
                    self.p += d.len_utf8();
                }
                let name = self.s[start..self.p].to_string();
                let kind = match name.as_str() {
                    "let" => TokKind::Let, "rec" => TokKind::Rec, "in" => TokKind::In,
                    "if" => TokKind::If, "then" => TokKind::Then, "else" => TokKind::Else,
                    "match" => TokKind::Match, "with" => TokKind::With,
                    "true" => TokKind::True, "false" => TokKind::False, "fun" => TokKind::Fun,
                    "_" => TokKind::Underscore,
                    _ => TokKind::Ident(name),
                };
                out.push(Token { kind, span: Span::new(lo, self.p) });
            } else {
                let (kind, len) = match (c, self.peek2()) {
                    ('-', Some('>')) => (TokKind::Arrow, 2),
                    ('=', Some('=')) => (TokKind::Eq, 2),
                    (':', Some(':')) => (TokKind::Colon2, 2),
                    ('=', _) => (TokKind::Equal, 1),
                    ('+', _) => (TokKind::Plus, 1),
                    ('-', _) => (TokKind::Minus, 1),
                    ('*', _) => (TokKind::Star, 1),
                    ('\\', _) => (TokKind::Backslash, 1),
                    ('(', _) => (TokKind::LParen, 1),
                    (')', _) => (TokKind::RParen, 1),
                    ('{', _) => (TokKind::LBrace, 1),
                    ('}', _) => (TokKind::RBrace, 1),
                    ('.', _) => (TokKind::Dot, 1),
                    (',', _) => (TokKind::Comma, 1),
                    ('|', _) => (TokKind::Pipe, 1),
                    _ => return Err(ParseError { message: format!("无法识别的字符 {:?}", c), span: Span::new(lo, lo + c.len_utf8()) }),
                };
                self.p += len;
                out.push(Token { kind, span: Span::new(lo, self.p) });
            }
        }
    }

    fn skip_ws(&mut self) {
        loop {
            while let Some(c) = self.peek() && c.is_whitespace() { self.p += c.len_utf8(); }
            if self.starts("//") {
                while let Some(c) = self.peek() && c != '\n' { self.p += c.len_utf8(); }
                continue;
            }
            if self.starts("(*") {
                let start = self.p;
                self.p += 2;
                let mut depth = 1;
                while depth > 0 {
                    if self.starts("(*") { depth += 1; self.p += 2; }
                    else if self.starts("*)") { depth -= 1; self.p += 2; }
                    else if self.bump().is_none() { break; }
                }
                let _ = start;
                continue;
            }
            break;
        }
    }
}

pub type NodeId = usize;

#[derive(Clone, Debug)]
pub enum ExprKind {
    Err,
    Unit,
    Int(i64),
    Bool(bool),
    Str(String),
    Var(String),
    Fun { param: String, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Bin { op: BinOp, lhs: NodeId, rhs: NodeId },
    Let { rec: bool, name: String, bound: NodeId, body: NodeId },
    If { cond: NodeId, then_e: NodeId, else_e: NodeId },
    Record { fields: Vec<(String, NodeId)>, ext: Option<NodeId> },
    Field { obj: NodeId, label: String },
    Tag { label: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<Arm> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp { Add, Sub, Mul, Eq }

#[derive(Clone, Debug)]
pub struct Arm {
    pub span: Span,
    pub label: String,
    pub binder: Option<String>,
    pub body: NodeId,
}

#[derive(Clone, Debug)]
pub struct Node {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
}

pub struct Ast {
    pub nodes: Vec<Rc<Node>>,
    pub root: NodeId,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Rc<Node> { &self.nodes[id] }
}

pub struct Parser {
    toks: Vec<Token>,
    i: usize,
    nodes: Vec<Rc<Node>>,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(toks: Vec<Token>) -> Self {
        Parser { toks, i: 0, nodes: Vec::new(), errors: Vec::new() }
    }

    fn cur(&self) -> &TokKind { &self.toks[self.i].kind }
    fn span(&self) -> Span { self.toks[self.i].span }
    fn at(&self, k: &TokKind) -> bool { self.cur() == k }
    fn bump_tok(&mut self) -> Token {
        let t = self.toks[self.i].clone();
        if !matches!(t.kind, TokKind::Eof) { self.i += 1; }
        t
    }
    fn eat(&mut self, k: &TokKind) -> bool {
        if self.at(k) { self.bump_tok(); true } else { false }
    }
    fn expect(&mut self, k: &TokKind, what: &str) -> bool {
        if self.eat(k) { true } else {
            self.errors.push(ParseError {
                message: format!("缺少 {}", what),
                span: self.span(),
            });
            false
        }
    }
    fn push(&mut self, kind: ExprKind, span: Span) -> NodeId {
        let id = self.nodes.len();
        self.nodes.push(Rc::new(Node { id, kind, span }));
        id
    }
    fn mk_err(&mut self, span: Span) -> NodeId { self.push(ExprKind::Err, span) }

    pub fn parse_program(mut self) -> (Ast, Vec<ParseError>) {
        let root = self.parse_expr(false);
        if !self.at(&TokKind::Eof) {
            let sp = self.span();
            self.errors.push(ParseError { message: "表达式后有多余符号".into(), span: sp });
        }
        let root = if self.nodes.is_empty() { self.mk_err(self.span()) } else { root };
        (Ast { nodes: self.nodes, root }, self.errors)
    }

    // stop_pipe: within match arm bodies, stop at `|`.
    fn parse_expr(&mut self, stop_pipe: bool) -> NodeId {
        if self.at(&TokKind::Let) { self.parse_let(stop_pipe) }
        else if self.at(&TokKind::If) { self.parse_if(stop_pipe) }
        else if self.at(&TokKind::Fun) || self.at(&TokKind::Backslash) { self.parse_fun(stop_pipe) }
        else { self.parse_infix(stop_pipe) }
    }

    fn parse_let(&mut self, stop_pipe: bool) -> NodeId {
        let lo = self.span().lo;
        self.bump_tok();
        let rec = self.eat(&TokKind::Rec);
        let name = if let TokKind::Ident(n) = self.cur().clone() {
            self.bump_tok(); n
        } else {
            self.errors.push(ParseError { message: "let 后需要标识符".into(), span: self.span() });
            "_".into()
        };
        self.expect(&TokKind::Equal, "=");
        let bound = self.parse_expr(false);
        // Bare `let x = e` (no in): body is the bound expression (demo convenience).
        if self.eat(&TokKind::In) {
            let body = self.parse_expr(stop_pipe);
            let hi = self.nodes[body].span.hi;
            self.push(ExprKind::Let { rec, name, bound, body }, Span::new(lo, hi))
        } else {
            let hi = self.nodes[bound].span.hi;
            let body = self.push(ExprKind::Unit, Span::new(hi, hi));
            self.push(ExprKind::Let { rec, name, bound, body }, Span::new(lo, hi))
        }
    }

    fn parse_if(&mut self, stop_pipe: bool) -> NodeId {
        let lo = self.span().lo;
        self.bump_tok();
        let cond = self.parse_expr(false);
        self.expect(&TokKind::Then, "then");
        let then_e = self.parse_expr(false);
        self.expect(&TokKind::Else, "else");
        let else_e = self.parse_expr(stop_pipe);
        let hi = self.nodes[else_e].span.hi;
        self.push(ExprKind::If { cond, then_e, else_e }, Span::new(lo, hi))
    }

    fn parse_fun(&mut self, stop_pipe: bool) -> NodeId {
        let lo = self.span().lo;
        self.bump_tok();
        let param = if let TokKind::Ident(n) = self.cur().clone() {
            self.bump_tok(); n
        } else {
            self.errors.push(ParseError { message: "fun 后需要参数名".into(), span: self.span() });
            "_".into()
        };
        self.expect(&TokKind::Arrow, "->");
        let body = self.parse_expr(stop_pipe);
        let hi = self.nodes[body].span.hi;
        self.push(ExprKind::Fun { param, body }, Span::new(lo, hi))
    }

    fn parse_infix(&mut self, stop_pipe: bool) -> NodeId {
        let mut left = self.parse_app(stop_pipe);
        loop {
            if stop_pipe && self.at(&TokKind::Pipe) { break; }
            let op = match self.cur() {
                TokKind::Plus => BinOp::Add,
                TokKind::Minus => BinOp::Sub,
                TokKind::Star => BinOp::Mul,
                TokKind::Eq => BinOp::Eq,
                _ => break,
            };
            let sp = self.span();
            self.bump_tok();
            let rhs = self.parse_app(stop_pipe);
            let hi = self.nodes[rhs].span.hi;
            left = self.push(ExprKind::Bin { op, lhs: left, rhs: rhs }, sp.merge(Span::new(sp.lo, hi)));
        }
        left
    }

    fn parse_app(&mut self, stop_pipe: bool) -> NodeId {
        let mut head = self.parse_postfix(stop_pipe)?;
        loop {
            if stop_pipe && self.at(&TokKind::Pipe) { break; }
            if self.starts_atom() {
                let arg = self.parse_atom();
                let hi = self.nodes[arg].span.hi;
                let lo = self.nodes[head].span.lo;
                head = self.push(ExprKind::App { fun: head, arg }, Span::new(lo, hi));
            } else { break; }
        }
        head
    }

    fn starts_atom(&self) -> bool {
        matches!(self.cur(),
            TokKind::Int(_) | TokKind::Str(_) | TokKind::Ident(_) | TokKind::Tag(_)
            | TokKind::True | TokKind::False | TokKind::LParen | TokKind::LBrace
            | TokKind::Fun | TokKind::Backslash)
    }

    fn parse_postfix(&mut self, stop_pipe: bool) -> Result<NodeId, NodeId> {
        let mut e = self.parse_atom();
        while self.at(&TokKind::Dot) {
            let dot = self.span();
            self.bump_tok();
            if let TokKind::Ident(l) = self.cur().clone() {
                self.bump_tok();
                let hi = self.nodes[e].span.hi.max(dot.hi);
                e = self.push(ExprKind::Field { obj: e, label: l }, Span::new(self.nodes[e].span.lo, hi));
            } else {
                self.errors.push(ParseError { message: "'.' 后需要字段名".into(), span: self.span() });
                let _ = stop_pipe;
                return Err(e);
            }
        }
        Ok(e)
    }

    fn parse_atom(&mut self) -> NodeId {
        let sp = self.span();
        match self.cur().clone() {
            TokKind::Int(n) => {
                self.bump_tok();
                self.push(ExprKind::Int(n), sp)
            }
            TokKind::Str(s) => {
                self.bump_tok();
                self.push(ExprKind::Str(s), sp)
            }
            TokKind::True => {
                self.bump_tok();
                self.push(ExprKind::Bool(true), sp)
            }
            TokKind::False => {
                self.bump_tok();
                self.push(ExprKind::Bool(false), sp)
            }
            TokKind::Ident(n) => {
                self.bump_tok();
                // qualified label? `x::field` kept as Var for now (unused syntax).
                let _ = self.eat(&TokKind::Colon2);
                self.push(ExprKind::Var(n), sp)
            }
            TokKind::LParen => {
                self.bump_tok();
                if self.eat(&TokKind::RParen) {
                    // unit represented as empty record
                    self.push(ExprKind::Record { fields: vec![], ext: None }, sp.merge(self.toks[self.i.saturating_sub(1)].span))
                } else {
                    let e = self.parse_expr(false);
                    if !self.eat(&TokKind::RParen) {
                        self.errors.push(ParseError { message: "缺少右括号 )".into(), span: self.span() });
                    }
                    e
                }
            }
            TokKind::LBrace => self.parse_record(),
            TokKind::Tag(l) => {
                self.bump_tok();
                if self.starts_atom() {
                    let payload = self.parse_atom();
                    let hi = self.nodes[payload].span.hi;
                    self.push(ExprKind::Tag { label: l, payload: Some(payload) }, Span::new(sp.lo, hi))
                } else {
                    self.push(ExprKind::Tag { label: l, payload: None }, sp)
                }
            }
            TokKind::Match => self.parse_match(),
            TokKind::Fun | TokKind::Backslash => self.parse_fun(false),
            TokKind::Eof => {
                self.errors.push(ParseError { message: "需要表达式".into(), span: sp });
                self.mk_err(sp)
            }
            other => {
                self.errors.push(ParseError { message: format!("此处不能以 {:?} 开始表达式", tok_name(&other)), span: sp });
                self.bump_tok();
                self.mk_err(sp)
            }
        }
    }

    fn parse_record(&mut self) -> NodeId {
        let lo = self.span().lo;
        self.bump_tok(); // {
        let mut fields = Vec::new();
        let mut ext = None;
        let mut hi = self.span().lo;
        if !self.at(&TokKind::RBrace) {
            loop {
                if self.at(&TokKind::Ident(_)) && self.toks.get(self.i + 1).map(|t| matches!(t.kind, TokKind::Equal)).unwrap_or(false) {
                    if let TokKind::Ident(l) = self.cur().clone() {
                        self.bump_tok();
                        self.bump_tok(); // =
                        let v = self.parse_expr(false);
                        hi = self.nodes[v].span.hi;
                        fields.push((l, v));
                    }
                } else if self.at(&TokKind::Dot) || self.at(&TokKind::Dot) {
                    self.bump_tok();
                    let e = self.parse_expr(false);
                    hi = self.nodes[e].span.hi;
                    ext = Some(e);
                } else {
                    let e = self.parse_expr(false);
                    hi = self.nodes[e].span.hi;
                    ext = Some(e);
                    break;
                }
                if self.eat(&TokKind::Comma) {
                    if self.at(&TokKind::RBrace) { break; }
                    continue;
                }
                break;
            }
        }
        if let Some(t) = self.toks.get(self.i) {
            if matches!(t.kind, TokKind::RBrace) { hi = hi.max(t.span.hi); }
        }
        if !self.eat(&TokKind::RBrace) {
            self.errors.push(ParseError { message: "缺少右花括号 }".into(), span: self.span() });
        }
        self.push(ExprKind::Record { fields, ext }, Span::new(lo, hi))
    }

    fn parse_match(&mut self) -> NodeId {
        let lo = self.span().lo;
        self.bump_tok(); // match
        let scrut = self.parse_app(false);
        self.expect(&TokKind::With, "with");
        let mut arms = Vec::new();
        // first arm may omit a leading `|`
        if !self.at(&TokKind::Pipe) {
            arms.push(self.parse_arm());
        }
        while self.at(&TokKind::Pipe) {
            self.bump_tok();
            if self.at(&TokKind::Eof) { break; }
            arms.push(self.parse_arm());
        }
        let hi = arms.last().map(|a| a.span.hi).unwrap_or(self.span().hi);
        self.push(ExprKind::Match { scrut, arms }, Span::new(lo, hi))
    }

    fn parse_arm(&mut self) -> Arm {
        let lo = self.span().lo;
        let label = if let TokKind::Tag(l) = self.cur().clone() {
            self.bump_tok();
            l
        } else if self.at(&TokKind::Underscore) {
            self.bump_tok();
            self.expect(&TokKind::Arrow, "->");
            let body = self.parse_expr(true);
            return Arm { span: Span::new(lo, self.nodes[body].span.hi), label: "_".into(), binder: None, body };
        } else {
            self.errors.push(ParseError { message: "match 分支需要 #标签 模式".into(), span: self.span() });
            "_".into()
        };
        let binder = if let TokKind::Ident(n) = self.cur().clone() {
            self.bump_tok();
            Some(n)
        } else {
            None
        };
        self.expect(&TokKind::Arrow, "->");
        let body = self.parse_expr(true);
        Arm { span: Span::new(lo, self.nodes[body].span.hi), label, binder, body }
    }
}

pub fn tok_name(k: &TokKind) -> &'static str {
    match k {
        TokKind::Int(_) => "整数", TokKind::Str(_) => "字符串", TokKind::Ident(_) => "标识符",
        TokKind::Tag(_) => "标签", TokKind::Let => "let", TokKind::Rec => "rec", TokKind::In => "in",
        TokKind::If => "if", TokKind::Then => "then", TokKind::Else => "else",
        TokKind::Match => "match", TokKind::With => "with", TokKind::True => "true",
        TokKind::False => "false", TokKind::Fun => "fun", TokKind::Arrow => "->",
        TokKind::Eq => "==", TokKind::Plus => "+", TokKind::Minus => "-", TokKind::Star => "*",
        TokKind::Backslash => "\\", TokKind::LParen => "(", TokKind::RParen => ")",
        TokKind::LBrace => "{", TokKind::RBrace => "}", TokKind::Dot => ".", TokKind::Comma => ",",
        TokKind::Pipe => "|", TokKind::Underscore => "_", TokKind::Equal => "=",
        TokKind::Colon2 => "::", TokKind::Eof => "文件末尾",
    }
}

pub fn parse(src: &str) -> Result<(Ast, Vec<ParseError>), ParseError> {
    let toks = Lexer::new(src).lex()?;
    Ok(Parser::new(toks).parse_program())
}
