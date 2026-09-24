//! 词法分析、语法分析与稳定的节点身份。
//!
//! 节点身份（`NodeId`）基于“结构路径”：从 AST 根到节点的子位置序列。
//! 它不依赖变量名或生成顺序，因此：
//! - 局部编辑只影响路径变化的节点；
//! - 给某个绑定改名不会让引用它的节点身份漂移。

use serde::{Deserialize, Serialize};
use std::fmt;
use std::ops::Range;

/// 源码字节范围。
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
}

/// 结构路径段。
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PathSeg(pub String);

impl PathSeg {
    pub fn new(s: impl Into<String>) -> Self {
        PathSeg(s.into())
    }
}

/// 稳定的 AST 节点身份：结构路径（相对根）。
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct NodeId {
    pub path: Vec<PathSeg>,
}

impl NodeId {
    pub fn root() -> Self {
        NodeId { path: vec![] }
    }
    pub fn child(&self, seg: impl Into<String>) -> Self {
        let mut path = self.path.clone();
        path.push(PathSeg::new(seg));
        NodeId { path }
    }
    pub fn depth(&self) -> usize {
        self.path.len()
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.path.is_empty() {
            write!(f, "$")
        } else {
            for (i, seg) in self.path.iter().enumerate() {
                if i > 0 {
                    write!(f, "/")?;
                }
                write!(f, "{}", seg.0)?;
            }
            Ok(())
        }
    }
}

// ---------- 词法 ----------

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    // 字面量与名字
    Int(i64),
    Str(String),
    Lower(String),
    Upper(String),
    Tag(String), // `Foo
    // 关键字
    Let, Rec, In, If, Then, Else, Fun, True, False, Unit, Match, With, End,
    Fn, Arrow2, // fn / ->
    // 标点
    LParen, RParen, LBrace, RBrace, Dot, Comma, Eq, Colon, Semicolon,
    Plus, Minus, Star, EqEq, Lt, Backslash, // \
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LexError {
    pub span: Span,
    pub msg: String,
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}
fn is_ident_cont(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric() || c == '\''
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let bytes = src.as_bytes();
    let mut i = 0usize;
    let mut out: Vec<Token> = Vec::new();
    let b = |i: usize| -> Option<char> { bytes.get(i).map(|&x| x as char) };
    loop {
        while i < bytes.len() && matches!(b(i), Some(c) if c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            i += 1;
        }
        if i >= bytes.len() {
            break;
        }
        let start = i;
        let c = b(i).unwrap();
        let mk = |tok: Tok, end: usize| Token { tok, span: Span::new(start, end) };
        match c {
            '(' => { i += 1; out.push(mk(Tok::LParen, i)); }
            ')' => { i += 1; out.push(mk(Tok::RParen, i)); }
            '{' => { i += 1; out.push(mk(Tok::LBrace, i)); }
            '}' => { i += 1; out.push(mk(Tok::RBrace, i)); }
            '.' => { i += 1; out.push(mk(Tok::Dot, i)); }
            ',' => { i += 1; out.push(mk(Tok::Comma, i)); }
            ':' => { i += 1; out.push(mk(Tok::Colon, i)); }
            ';' => {
                i += 1;
                if b(i) == Some(';') { i += 1; }
                out.push(mk(Tok::Semicolon, i));
            }
            '+' => { i += 1; out.push(mk(Tok::Plus, i)); }
            '-' => {
                i += 1;
                if b(i) == Some('>') {
                    i += 1;
                    out.push(mk(Tok::Arrow2, i));
                } else {
                    out.push(mk(Tok::Minus, i));
                }
            }
            '*' => { i += 1; out.push(mk(Tok::Star, i)); }
            '=' => {
                i += 1;
                if b(i) == Some('=') {
                    i += 1;
                    out.push(mk(Tok::EqEq, i));
                } else {
                    out.push(mk(Tok::Eq, i));
                }
            }
            '<' => { i += 1; out.push(mk(Tok::Lt, i)); }
            '\\' => { i += 1; out.push(mk(Tok::Backslash, i)); }
            '`' => {
                i += 1;
                let s0 = i;
                while matches!(b(i), Some(ch) if is_ident_cont(ch)) {
                    i += 1;
                }
                if i == s0 {
                    return Err(LexError { span: Span::new(start, i), msg: "反引号后缺少构造子名".into() });
                }
                let name = src[s0..i].to_string();
                out.push(mk(Tok::Tag(name), i));
            }
            '0'..='9' => {
                let s0 = i;
                while matches!(b(i), Some(ch) if ch.is_ascii_digit()) {
                    i += 1;
                }
                let n: i64 = src[s0..i].parse().map_err(|_| LexError {
                    span: Span::new(s0, i),
                    msg: "整数字面量超出范围".into(),
                })?;
                out.push(mk(Tok::Int(n), i));
            }
            '"' => {
                i += 1;
                let mut s = String::new();
                loop {
                    match b(i) {
                        None => return Err(LexError { span: Span::new(start, i), msg: "字符串未闭合".into() }),
                        Some('"') => { i += 1; break; }
                        Some('\\') => {
                            i += 1;
                            match b(i) {
                                Some('n') => s.push('\n'),
                                Some('t') => s.push('\t'),
                                Some('\\') => s.push('\\'),
                                Some('"') => s.push('"'),
                                Some(other) => return Err(LexError {
                                    span: Span::new(start, i + 1),
                                    msg: format!("未知转义 \\{other}"),
                                }),
                                None => return Err(LexError { span: Span::new(start, i), msg: "字符串未闭合".into() }),
                            }
                            i += 1;
                        }
                        Some(ch) => { s.push(ch); i += 1; }
                    }
                }
                out.push(mk(Tok::Str(s), i));
            }
            ch if is_ident_start(ch) => {
                let s0 = i;
                while matches!(b(i), Some(x) if is_ident_cont(x)) {
                    i += 1;
                }
                let word = &src[s0..i];
                let tok = match word {
                    "let" => Tok::Let,
                    "rec" => Tok::Rec,
                    "in" => Tok::In,
                    "if" => Tok::If,
                    "then" => Tok::Then,
                    "else" => Tok::Else,
                    "fun" => Tok::Fun,
                    "true" => Tok::True,
                    "false" => Tok::False,
                    "unit" => Tok::Unit,
                    "match" => Tok::Match,
                    "with" => Tok::With,
                    "end" => Tok::End,
                    "fn" => Tok::Fn,
                    _ if ch.is_ascii_uppercase() => Tok::Upper(word.to_string()),
                    _ => Tok::Lower(word.to_string()),
                };
                out.push(mk(tok, i));
            }
            _ => {
                return Err(LexError {
                    span: Span::new(start, i + 1),
                    msg: format!("意外的字符 {c:?}"),
                });
            }
        }
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(bytes.len(), bytes.len()) });
    Ok(out)
}

// ---------- AST ----------

/// 一个带身份与范围的 AST 节点。
#[derive(Clone, Debug)]
pub struct Node<T> {
    pub id: NodeId,
    pub span: Span,
    pub data: T,
}

pub type ExprNode = Node<Expr>;

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i64),
    Str(String),
    Bool(bool),
    Unit,
    Var(String),
    /// 顶层类型构造子（目前可用于 unit 等，保留）
    Lam {
        param: String,
        param_span: Span,
        annot: Option<TypeNode>,
        body: Box<ExprNode>,
    },
    App {
        fun: Box<ExprNode>,
        arg: Box<ExprNode>,
    },
    Let {
        rec: bool,
        name: String,
        name_span: Span,
        annot: Option<TypeNode>,
        value: Box<ExprNode>,
        body: Option<Box<ExprNode>>,
    },
    If {
        cond: Box<ExprNode>,
        then: Box<ExprNode>,
        els: Box<ExprNode>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<ExprNode>,
        rhs: Box<ExprNode>,
    },
    Record(Vec<RecordField>),
    Field {
        record: Box<ExprNode>,
        label: String,
        label_span: Span,
    },
    Tag {
        tag: String,
        payload: Option<Box<ExprNode>>,
    },
    Match {
        scrut: Box<ExprNode>,
        arms: Vec<MatchArm>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp { Add, Sub, Mul, Eq, Lt }

#[derive(Clone, Debug)]
pub struct RecordField {
    pub label: String,
    pub label_span: Span,
    pub value: ExprNode,
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pat: PatNode,
    pub body: ExprNode,
}

#[derive(Clone, Debug)]
pub enum Pat {
    Wild,
    Var(String),
    Tag { tag: String, payload: Option<Box<PatNode>> },
}

pub type PatNode = Node<Pat>;

#[derive(Clone, Debug)]
pub enum Ty {
    /// 小写开头的类型变量（如 a、row）
    Var(String),
    /// Int / Bool / String / Unit / 大写构造
    Con(String, Vec<TypeNode>),
    Fun(Box<TypeNode>, Box<TypeNode>),
    Record(RowNode),
    Variant(RowNode),
}

pub type TypeNode = Node<Ty>;

#[derive(Clone, Debug)]
pub enum RowTail {
    /// 封闭行（没有 |）
    Closed,
    /// 开放行，尾部行变量名
    Open(String, Span),
}

#[derive(Clone, Debug)]
pub struct RowFieldTy {
    pub label: String,
    pub label_span: Span,
    pub ty: TypeNode,
}

#[derive(Clone, Debug)]
pub struct Row {
    pub fields: Vec<RowFieldTy>,
    pub tail: RowTail,
}

pub type RowNode = Node<Row>;

#[derive(Clone, Debug)]
pub struct Decl {
    pub rec: bool,
    pub name: String,
    pub name_span: Span,
    pub annot: Option<TypeNode>,
    pub value: ExprNode,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub decls: Vec<Decl>,
    /// 结尾可选表达式；缺省为 unit
    pub final_expr: Option<ExprNode>,
}

// ---------- 解析错误 ----------

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}: {}", self.span.start, self.span.end, self.msg)
    }
}

// ---------- Parser ----------

struct Parser<'a> {
    toks: Vec<Token>,
    pos: usize,
    src: &'a str,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn peek_span(&self) -> Span {
        self.toks[self.pos].span.clone()
    }
    fn at(&self, t: &Tok) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(t)
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos < self.toks.len() - 1 {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.at(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.at(&t) {
            Ok(self.bump())
        } else {
            Err(ParseError {
                span: self.peek_span(),
                msg: format!("期望 {what}，但遇到 {}", tok_name(self.peek())),
            })
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut decls = Vec::new();
        loop {
            if self.at(&Tok::Eof) {
                break;
            }
            if self.at(&Tok::Let) {
                let d = self.parse_decl()?;
                decls.push(d);
                if self.at(&Tok::Semicolon) {
                    self.bump();
                }
            } else {
                break;
            }
        }
        let final_expr = if self.at(&Tok::Eof) {
            None
        } else {
            let e = self.parse_expr(&NodeId::root())?;
            if self.at(&Tok::Semicolon) {
                self.bump();
            }
            if !self.at(&Tok::Eof) {
                return Err(ParseError {
                    span: self.peek_span(),
                    msg: format!("顶层声明之间缺少 let；意外遇到 {}", tok_name(self.peek())),
                });
            }
            Some(e)
        };
        Ok(Program { decls, final_expr })
    }

    fn parse_decl(&mut self) -> Result<Decl, ParseError> {
        let start = self.expect(Tok::Let, "let")?.span.start;
        let rec = self.eat(&Tok::Rec);
        let name_tok = self.expect_name()?;
        let annot = if self.eat(&Tok::Colon) {
            Some(self.parse_type(&NodeId::root().child(format!("decl:{name_tok.0}/annot")))?)
        } else {
            None
        };
        self.expect(Tok::Eq, "'='")?;
        let value = self.parse_expr(&NodeId::root().child(format!("decl:{name_tok.0}/val")))?;
        let end = value.span.end;
        Ok(Decl {
            rec,
            name: name_tok.0,
            name_span: name_tok.1,
            annot,
            value,
            span: Span::new(start, end),
        })
    }

    fn expect_name(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().clone() {
            Tok::Lower(n) => {
                let t = self.bump();
                Ok((n, t.span))
            }
            other => Err(ParseError {
                span: self.peek_span(),
                msg: format!("期望标识符，但遇到 {}", tok_name(&other)),
            }),
        }
    }

    // ---- 类型 ----

    fn parse_type(&mut self, id: &NodeId) -> Result<TypeNode, ParseError> {
        // 右结合函数类型
        let lhs = self.parse_type_atom(id.child("fun_l"))?;
        if self.at(&Tok::Arrow2) {
            let sp0 = lhs.span.start;
            self.bump();
            let rhs = self.parse_type(id.child("fun_r"))?;
            let sp1 = rhs.span.end;
            Ok(mk_node(id, Span::new(sp0, sp1), Ty::Fun(Box::new(lhs), Box::new(rhs))))
        } else {
            Ok(lhs)
        }
    }

    fn parse_type_atom(&mut self, id: &NodeId) -> Result<TypeNode, ParseError> {
        match self.peek().clone() {
            Tok::LBrace => self.parse_row_ty(id, true),
            Tok::Backslash => {
                self.bump();
                self.parse_row_ty(id, false)
            }
            Tok::Lower(n) => {
                let t = self.bump();
                Ok(mk_node(id, t.span.clone(), Ty::Var(n)))
            }
            Tok::Upper(n) => {
                let t = self.bump();
                // 简单的类型实参（零或多个原子）
                let mut args = Vec::new();
                let mut k = 0;
                loop {
                    if matches!(self.peek(), Tok::Lower(_) | Tok::Upper(_) | Tok::LBrace | Tok::Backslash | Tok::LParen) {
                        let arg = self.parse_type_atom(&id.child(format!("arg{k}")))?;
                        args.push(arg);
                        k += 1;
                    } else {
                        break;
                    }
                }
                Ok(mk_node(id, t.span.clone(), Ty::Con(n, args)))
            }
            Tok::LParen => {
                self.bump();
                let t = self.parse_type(id)?;
                self.expect(Tok::RParen, "')'")?;
                Ok(t)
            }
            other => Err(ParseError {
                span: self.peek_span(),
                msg: format!("期望类型，但遇到 {}", tok_name(&other)),
            }),
        }
    }

    fn parse_row_ty(&mut self, id: &NodeId, is_record: bool) -> Result<TypeNode, ParseError> {
        let open_tok = if is_record { self.expect(Tok::LBrace, "'{'")? } else { self.expect(Tok::LBrace, "'{'")? };
        let start = open_tok.span.start;
        let mut fields = Vec::new();
        let mut tail = RowTail::Closed;
        loop {
            if self.at(&Tok::RBrace) { break; }
            if self.at(&Tok::PipeCompat) {
                self.bump();
                match self.peek().clone() {
                    Tok::Lower(n) => {
                        let t = self.bump();
                        tail = RowTail::Open(n, t.span);
                    }
                    other => return Err(ParseError {
                        span: self.peek_span(),
                        msg: format!("'|' 后需要行变量，但遇到 {}", tok_name(&other)),
                    }),
                }
                break;
            }
            let (label, label_span) = self.expect_name()?;
            self.expect(Tok::Colon, "':'")?;
            let ty = self.parse_type(&id.child(format!("f:{label}")))?;
            fields.push(RowFieldTy { label, label_span, ty });
            if !self.eat(&Tok::Comma) { break; }
        }
        let end = self.expect(Tok::RBrace, "'}'")?.span.end;
        let row = mk_node(id, Span::new(start, end), Row { fields, tail });
        let data = if is_record { Ty::Record(row) } else { Ty::Variant(row) };
        // 复用同一范围
        Ok(mk_node(id, Span::new(start, end), data))
    }

    // ---- 表达式 ----

    fn parse_expr(&mut self, id: &NodeId) -> Result<ExprNode, ParseError> {
        self.parse_binop(id, 0)
    }

    fn op_info(t: &Tok) -> Option<(BinOp, u8)> {
        match t {
            Tok::EqEq => Some((BinOp::Eq, 1)),
            Tok::Lt => Some((BinOp::Lt, 1)),
            Tok::Plus | Tok::Minus => Some((BinOp::Add, 2)),
            Tok::Star => Some((BinOp::Mul, 3)),
            _ => None,
        }
    }
