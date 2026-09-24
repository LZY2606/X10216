//! 词法与语法分析：生成带来源范围与稳定身份的 AST。
//!
//! 节点稳定身份（`NodeId` 的字符串键）由结构路径决定，
//! 绑定处出现的变量名不进入路径，因此“变量改名”不会让下游节点身份漂移；
//! 但 record/variant 的字段名是数据，改字段名会使对应子树身份变化。

use serde::Serialize;

/// 半开字节区间 [start, end)。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok {
    Int(i64),
    Lower(String),
    Upper(String),
    // 关键字
    Let, In, Rec, Fun, True, False, Match, With, Underscore,
    // 标点
    Equal, Arrow, FatArrow, Backslash, Dot, Comma, LParen, RParen,
    LBrace, RBrace, Pipe, Colon, ColonColon, Star, Plus, Minus,
    // record 扩展
    LBraceAngle, RBraceAngle,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

fn is_ident_start(b: u8) -> bool {
    b == b'_' || (b as char).is_ascii_alphabetic()
}
fn is_ident_cont(b: u8) -> bool {
    b == b'_' || b == b'\'' || (b as char).is_ascii_alphanumeric()
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0 }
    }

    fn skip_ws(&mut self) {
        loop {
            while self.pos < self.src.len() && (self.src[self.pos] as char).is_whitespace() {
                self.pos += 1;
            }
            // 行注释 --
            if self.pos + 1 < self.src.len() && self.src[self.pos] == b'-' && self.src[self.pos + 1] == b'-' {
                while self.pos < self.src.len() && self.src[self.pos] != b'\n' {
                    self.pos += 1;
                }
                continue;
            }
            break;
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        let mut out = Vec::new();
        loop {
            self.skip_ws();
            if self.pos >= self.src.len() {
                break;
            }
            let start = self.pos;
            let b = self.src[self.pos];
            let single = |t: Tok, me: &mut Self| {
                me.pos += 1;
                Token { tok: t, span: Span::new(start, me.pos) }
            };
            let tok = match b {
                b'(' => single(Tok::LParen, &mut self),
                b')' => single(Tok::RParen, &mut self),
                b'{' => {
                    if self.peek1() == Some(b'<') {
                        self.pos += 2;
                        Token { tok: Tok::LBraceAngle, span: Span::new(start, self.pos) }
                    } else {
                        single(Tok::LBrace, &mut self)
                    }
                }
                b'}' => single(Tok::RBrace, &mut self),
                b'>' => {
                    if self.peek1() == Some(b'}') {
                        self.pos += 2;
                        Token { tok: Tok::RBraceAngle, span: Span::new(start, self.pos) }
                    } else {
                        return Err(LexError {
                            message: "无法识别的字符 '>'".to_string(),
                            span: Span::new(start, start + 1),
                        });
                    }
                }
                b'<' => {
                    return Err(LexError {
                        message: "无法识别的字符 '<'（record 扩展请使用 {< ... >}）".to_string(),
                        span: Span::new(start, start + 1),
                    });
                }
                b'.' => single(Tok::Dot, &mut self),
                b',' => single(Tok::Comma, &mut self),
                b'+' => single(Tok::Plus, &mut self),
                b'*' => single(Tok::Star, &mut self),
                b'|' => single(Tok::Pipe, &mut self),
                b'\\' => single(Tok::Backslash, &mut self),
                b'-' => {
                    if self.peek1() == Some(b'>') {
                        self.pos += 2;
                        Token { tok: Tok::Arrow, span: Span::new(start, self.pos) }
                    } else if self.peek1() == Some(b'-') {
                        // 被 skip_ws 处理，兜底
                        self.pos += 1;
                        continue;
                    } else {
                        self.pos += 1;
                        Token { tok: Tok::Minus, span: Span::new(start, self.pos) }
                    }
                }
                b'=' => {
                    if self.peek1() == Some(b'>') {
                        self.pos += 2;
                        Token { tok: Tok::FatArrow, span: Span::new(start, self.pos) }
                    } else {
                        self.pos += 1;
                        Token { tok: Tok::Equal, span: Span::new(start, self.pos) }
                    }
                }
                b':' => {
                    if self.peek1() == Some(b':') {
                        self.pos += 2;
                        Token { tok: Tok::ColonColon, span: Span::new(start, self.pos) }
                    } else {
                        self.pos += 1;
                        Token { tok: Tok::Colon, span: Span::new(start, self.pos) }
                    }
                }
                b'0'..=b'9' => {
                    let mut n: i64 = 0;
                    while self.pos < self.src.len() && self.src[self.pos].is_ascii_digit() {
                        n = n.saturating_mul(10).saturating_add((self.src[self.pos] - b'0') as i64);
                        self.pos += 1;
                    }
                    Token { tok: Tok::Int(n), span: Span::new(start, self.pos) }
                }
                _ if is_ident_start(b) => {
                    while self.pos < self.src.len() && is_ident_cont(self.src[self.pos]) {
                        self.pos += 1;
                    }
                    let word = &self.src[start..self.pos];
                    let word = std::str::from_utf8(word).unwrap().to_string();
                    let tok = match word.as_str() {
                        "let" => Tok::Let,
                        "in" => Tok::In,
                        "rec" => Tok::Rec,
                        "fun" => Tok::Fun,
                        "true" => Tok::True,
                        "false" => Tok::False,
                        "match" => Tok::Match,
                        "with" => Tok::With,
                        "_" => Tok::Underscore,
                        _ => {
                            if word.chars().next().unwrap().is_ascii_uppercase() {
                                Tok::Upper(word)
                            } else {
                                Tok::Lower(word)
                            }
                        }
                    };
                    Token { tok, span: Span::new(start, self.pos) }
                }
                _ => {
                    return Err(LexError {
                        message: format!("无法识别的字符 {:?}", b as char),
                        span: Span::new(start, start + 1),
                    });
                }
            };
            out.push(tok);
        }
        Ok(out)
    }

    fn peek1(&self) -> Option<u8> {
        self.src.get(self.pos + 1).copied()
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// AST
// ---------------------------------------------------------------------------

/// 稳定节点身份。`key` 是结构路径，跨编辑可复用；`idx` 是本次解析在 arena 中的下标。
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct NodeId {
    pub key: String,
    pub idx: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    Var(String),
    Fun { param: String, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Record { fields: Vec<(String, NodeId)> },
    Proj { record: NodeId, label: String },
    Extend {
        label: String,
        value: NodeId,
        base: NodeId,
        other: NodeId,
    },
    Tag { tag: String, payload: Option<NodeId> },
    Let {
        rec: bool,
        name: String,
        bind: NodeId,
        body: NodeId,
    },
    Match {
        scrut: NodeId,
        arms: Vec<MatchArm>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct MatchArm {
    pub tag: String,
    /// `Some(name)` 绑定载荷，`None` 表示无载荷分支。
    pub binder: Option<String>,
    pub body: NodeId,
}

#[derive(Clone, Debug, Serialize)]
pub struct AstNode {
    pub id: NodeId,
    pub expr: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize)]
pub struct Program {
    pub nodes: Vec<AstNode>,
    pub root: NodeId,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct Parser {
    toks: Vec<Token>,
    pos: usize,
    nodes: Vec<AstNode>,
}

impl Parser {
    fn parse(src: &str) -> Result<Program, ParseError> {
        let toks = Lexer::new(src).tokenize().map_err(|e| ParseError {
            message: e.message,
            span: e.span,
        })?;
        let mut p = Parser { toks, pos: 0, nodes: Vec::new() };
        if p.at_end() {
            return Err(ParseError {
                message: "程序为空".to_string(),
                span: Span::new(0, 0),
            });
        }
        let root = p.parse_expr("r")?;
        if !p.at_end() {
            let t = p.peek_tok().unwrap();
            return Err(ParseError {
                message: "表达式之后还有无法解析的内容".to_string(),
                span: t.span,
            });
        }
        Ok(Program { nodes: p.nodes, root })
    }

    fn at_end(&self) -> bool {
        self.pos >= self.toks.len()
    }
    fn peek_tok(&self) -> Option<&Token> {
        self.toks.get(self.pos)
    }
    fn peek(&self) -> Option<&Tok> {
        self.toks.get(self.pos).map(|t| &t.tok)
    }
    fn advance(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        self.pos += 1;
        t
    }

    fn err(&self, message: impl Into<String>, span: Span) -> ParseError {
        ParseError { message: message.into(), span }
    }
    fn err_here(&self, message: impl Into<String>) -> ParseError {
        match self.peek_tok() {
            Some(t) => self.err(message, t.span),
            None => self.err(message, Span::new(self.src_end(), self.src_end())),
        }
    }
    fn src_end(&self) -> usize {
        self.toks.last().map(|t| t.span.end).unwrap_or(0)
    }

    fn alloc(&mut self, key: String, expr: Expr, span: Span) -> NodeId {
        let idx = self.nodes.len();
        let id = NodeId { key, idx };
        self.nodes.push(AstNode { id: id.clone(), expr, span });
        id
    }

    fn expect(&mut self, wanted: &Tok, desc: &str) -> Result<Token, ParseError> {
        match self.peek() {
            Some(t) if std::mem::discriminant(t) == std::mem::discriminant(wanted) => {
                Ok(self.advance())
            }
            _ => Err(self.err_here(format!("期望 {}", desc))),
        }
    }

    /// 入口：按优先级逐层解析。
    fn parse_expr(&mut self, key: &str) -> Result<NodeId, ParseError> {
        match self.peek() {
            Some(Tok::Let) => self.parse_let(key),
            Some(Tok::Fun) | Some(Tok::Backslash) => self.parse_fun(key),
            Some(Tok::Match) => self.parse_match(key),
            _ => self.parse_app(key),
        }
    }

    fn parse_let(&mut self, key: &str) -> Result<NodeId, ParseError> {
        let start = self.advance().span.start; // let
        let rec = if matches!(self.peek(), Some(Tok::Rec)) {
            self.advance();
            true
        } else {
            false
        };
        let name_tok = self.advance();
        let name = match name_tok.tok {
            Tok::Lower(s) => s,
            _ => return Err(self.err("let 后需要小写标识符", name_tok.span)),
        };
        self.expect(&Tok::Equal, "'='")?;
        let bind = self.parse_expr(&format!("{}.b", key))?;
        self.expect(&Tok::In, "'in'")?;
        let body = self.parse_expr(&format!("{}.i", key))?;
        let span = Span::new(start, self.prev_end());
        Ok(self.alloc(key.to_string(), Expr::Let { rec, name, bind, body }, span))
    }

    fn parse_fun(&mut self, key: &str) -> Result<NodeId, ParseError> {
        let start = self.advance().span.start; // fun 或 \
        let param_tok = self.advance();
        let param = match param_tok.tok {
            Tok::Lower(s) => s,
            _ => return Err(self.err("函数参数需要小写标识符", param_tok.span)),
        };
        self.expect(&Tok::Arrow, "'->'")?;
        let body = self.parse_expr(&format!("{}.f", key))?;
        let span = Span::new(start, self.prev_end());
        Ok(self.alloc(key.to_string(), Expr::Fun { param, body }, span))
    }

    fn parse_match(&mut self, key: &str) -> Result<NodeId, ParseError> {
        let start = self.advance().span.start; // match
        let scrut = self.parse_app(&format!("{}.s", key))?;
        self.expect(&Tok::With, "'with'")?;
        let mut arms = Vec::new();
        let mut i = 0;
        loop {
            self.expect(&Tok::Pipe, "'|' 分支")?;
            let tag_tok = self.advance();
            let tag = match tag_tok.tok {
                Tok::Upper(s) => s,
                _ => return Err(self.err("分支标签需要大写构造子", tag_tok.span)),
            };
            let binder = match self.peek() {
                Some(Tok::Lower(_)) => {
                    let t = self.advance();
                    if let Tok::Lower(s) = t.tok { Some(s) } else { unreachable!() }
                }
                Some(Tok::Underscore) => {
                    self.advance();
                    None
                }
                _ => None,
            };
            self.expect(&Tok::FatArrow, "'=>'")?;
            let body = self.parse_expr(&format!("{}.a{}.e", key, i))?;
            arms.push(MatchArm { tag, binder, body });
            i += 1;
            if !matches!(self.peek(), Some(Tok::Pipe)) {
                break;
            }
        }
        let span = Span::new(start, self.prev_end());
        Ok(self.alloc(key.to_string(), Expr::Match { scrut, arms }, span))
    }

    /// 应用：左结合，参数是 proj/atom。
    fn parse_app(&mut self, key: &str) -> Result<NodeId, ParseError> {
        let mut func = self.parse_proj(&format!("{}.f0", key))?;
        let mut i = 1;
        loop {
            let is_arg = matches!(
                self.peek(),
                Some(Tok::Int(_))
                    | Some(Tok::Lower(_))
                    | Some(Tok::True)
                    | Some(Tok::False)
                    | Some(Tok::LParen)
                    | Some(Tok::LBrace)
                    | Some(Tok::LBraceAngle)
                    | Some(Tok::Upper(_))
            );
            if !is_arg {
                break;
            }
            let arg = self.parse_proj(&format!("{}.a{}", key, i))?;
            let span = Span::new(self.span_of(&func).start, self.prev_end());
            func = self.alloc(
                format!("{}.app{}", key, i),
                Expr::App { fun: func, arg },
                span,
            );
            i += 1;
        }
        Ok(func)
    }

    fn span_of(&self, id: &NodeId) -> Span {
        self.nodes[id.idx].span
    }
    fn prev_end(&self) -> usize {
        self.toks[self.pos.saturating_sub(1)].span.end
    }

    /// 投影与 record 扩展后缀。
    fn parse_proj(&mut self, key: &str) -> Result<NodeId, ParseError> {
        let mut node = self.parse_atom(key)?;
        loop {
            match self.peek() {
                Some(Tok::Dot) => {
                    self.advance();
                    // 普通投影 .label，或扩展 {{ label = e | .. }} 用专用关键字处理
                    let lbl_tok = self.advance();
                    let label = match lbl_tok.tok {
                        Tok::Lower(s) => s,
                        _ => return Err(self.err("字段名需要小写标识符", lbl_tok.span)),
                    };
                    let span = Span::new(self.span_of(&node).start, lbl_tok.span.end);
                    node = self.alloc(
                        format!("{}.p{}", key, label),
                        Expr::Proj { record: node, label },
                        span,
                    );
                }
                    extend_set_other(&mut self.nodes, &node, other);
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_atom(&mut self, key: &str) -> Result<NodeId, ParseError> {
        let t = self.peek_tok().cloned();
        let t = match t {
            Some(t) => t,
            None => return Err(self.err_here("期望表达式")),
        };
        match t.tok {
            Tok::Int(n) => {
                self.advance();
                Ok(self.alloc(key.to_string(), Expr::Int(n), t.span))
            }
            Tok::True => {
                self.advance();
                Ok(self.alloc(key.to_string(), Expr::Bool(true), t.span))
            }
            Tok::False => {
                self.advance();
                Ok(self.alloc(key.to_string(), Expr::Bool(false), t.span))
            }
            Tok::Lower(s) => {
                self.advance();
                Ok(self.alloc(key.to_string(), Expr::Var(s), t.span))
            }
            Tok::Upper(tag) => {
                self.advance();
                // 可选载荷
                let payload = match self.peek() {
                    Some(Tok::Int(_)) | Some(Tok::Lower(_)) | Some(Tok::LParen)
                    | Some(Tok::LBrace) | Some(Tok::LBraceAngle)
                    | Some(Tok::True) | Some(Tok::False)
                    | Some(Tok::Upper(_)) => {
                        Some(self.parse_atom(&format!("{}.p", key))?)
                    }
                    _ => None,
                };
                let end = payload.as_ref().map(|p| self.span_of(p).end).unwrap_or(t.span.end);
                Ok(self.alloc(key.to_string(), Expr::Tag { tag, payload }, Span::new(t.span.start, end)))
            }
            Tok::LParen => {
                self.advance();
                let e = self.parse_expr(key)?;
                self.expect(&Tok::RParen, "')'")?;
                Ok(e)
            }
            Tok::LBrace => self.parse_record(key, t.span.start),
            Tok::LBraceAngle => self.parse_extend(key, t.span.start),
            _ => Err(self.err("无法开始一个表达式", t.span)),
        }
    }

    fn parse_extend(&mut self, key: &str, start: usize) -> Result<NodeId, ParseError> {
        // {< label = value with base | other >}
        self.advance();
        let lbl_tok = self.advance();
        let label = match lbl_tok.tok {
            Tok::Lower(s) => s,
            _ => return Err(self.err("扩展字段需要小写标识符", lbl_tok.span)),
        };
        self.expect(&Tok::Equal, "'='")?;
        let value = self.parse_expr(&format!("{}.v{}", key, label))?;
        self.expect(&Tok::With, "'with'")?;
        let base = self.parse_expr(&format!("{}.b{}", key, label))?;
        self.expect(&Tok::Pipe, "'|'")?;
        let other = self.parse_expr(&format!("{}.o{}", key, label))?;
        self.expect(&Tok::RBraceAngle, "'>}'")?;
        let span = Span::new(start, self.prev_end());
        Ok(self.alloc(
            key.to_string(),
            Expr::Extend { label, value, base, other },
            span,
        ))
    }

    fn parse_record(&mut self, key: &str, start: usize) -> Result<NodeId, ParseError> {
        self.expect(&Tok::LBrace, "'{'")?;
        let mut fields = Vec::new();
        loop {
            if matches!(self.peek(), Some(Tok::RBrace)) {
                break;
            }
            let lbl_tok = self.advance();
            let label = match lbl_tok.tok {
                Tok::Lower(s) => s,
                _ => return Err(self.err("字段名需要小写标识符", lbl_tok.span)),
            };
            self.expect(&Tok::Equal, "'='")?;
            let val = self.parse_expr(&format!("{}.f{}", key, label))?;
            fields.push((label, val));
            match self.peek() {
                Some(Tok::Comma) => {
                    self.advance();
                }
                Some(Tok::RBrace) => {}
                _ => return Err(self.err_here("字段之间需要 ','，或用 '}' 结束")),
            }
        }
        self.expect(&Tok::RBrace, "'}'")?;
        let span = Span::new(start, self.prev_end());
        Ok(self.alloc(key.to_string(), Expr::Record { fields }, span))
    }
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    Parser::parse(src)
}
