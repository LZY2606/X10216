//! 词法、AST 与递归下降解析器。

use serde::Serialize;
use std::collections::HashMap;

pub type NodeId = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start as u32, end: end as u32 }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum EKind {
    LitInt(i64),
    LitStr(String),
    LitBool(bool),
    Var(String),
    Lam { var: String, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Let {
        rec: bool,
        name: String,
        bound: NodeId,
        body: NodeId,
    },
    Ann { expr: NodeId },
    Record(Vec<(String, NodeId)>),
    Proj { field: String, expr: NodeId },
    Variant { tag: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<NodeId> },
    Arm { tag: String, var: Option<String>, body: NodeId },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ENode {
    pub id: NodeId,
    pub span: Span,
    #[serde(flatten)]
    pub kind: EKind,
}

impl ENode {
    pub fn children(&self) -> Vec<NodeId> {
        match &self.kind {
            EKind::Lam { body, .. } => vec![*body],
            EKind::App { fun, arg } => vec![*fun, *arg],
            EKind::Let { bound, body, .. } => vec![*bound, *body],
            EKind::Ann { expr } => vec![*expr],
            EKind::Record(fs) => fs.iter().map(|(_, n)| *n).collect(),
            EKind::Proj { expr, .. } => vec![*expr],
            EKind::Variant { payload, .. } => payload.iter().copied().collect(),
            EKind::Match { scrut, arms } => {
                let mut v = vec![*scrut];
                v.extend(arms);
                v
            }
            EKind::Arm { body, .. } => vec![*body],
            _ => vec![],
        }
    }

    pub fn kind_tag(&self) -> &'static str {
        match &self.kind {
            EKind::LitInt(_) => "LitInt",
            EKind::LitStr(_) => "LitStr",
            EKind::LitBool(_) => "LitBool",
            EKind::Var(_) => "Var",
            EKind::Lam { .. } => "Lam",
            EKind::App { .. } => "App",
            EKind::Let { .. } => "Let",
            EKind::Ann { .. } => "Ann",
            EKind::Record(_) => "Record",
            EKind::Proj { .. } => "Proj",
            EKind::Variant { .. } => "Variant",
            EKind::Match { .. } => "Match",
            EKind::Arm { .. } => "Arm",
        }
    }
}

/// 注解中的（单态）类型表达式。
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", content = "data")]
pub enum TypeAst {
    TCon(String),
    TVar(String),
    TFun(Box<TypeAst>, Box<TypeAst>),
    TRecord(Vec<(String, TypeAst)>),
    TVariant(Vec<(String, TypeAst)>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ann {
    pub span: Span,
    pub ty: TypeAst,
}

#[derive(Clone, Debug, Default)]
pub struct Ast {
    pub root: Option<NodeId>,
    pub nodes: HashMap<NodeId, ENode>,
    pub anns: HashMap<NodeId, Ann>,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &ENode {
        &self.nodes[&id]
    }
}

// ---------------- 词法 ----------------

#[derive(Clone, Debug, PartialEq)]
enum TokKind {
    Int(i64),
    Str(String),
    Ident(String),
    /// 标签：以 `#` 开头。
    Tag(String),
    Let, Rec, In, Fun, True, False, Match, With, Colon, Dot,
    Eq, Arrow, Backslash, LParen, RParen, LBrace, RBrace,
    Bar, Eof,
}

#[derive(Clone, Debug)]
struct Tok {
    kind: TokKind,
    start: usize,
    end: usize,
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self {
        Lexer { src: src.as_bytes(), pos: 0 }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let c = self.peek()?;
        self.pos += 1;
        Some(c)
    }

    fn lex(mut self) -> Result<Vec<Tok>, ParseError> {
        let mut out = Vec::new();
        loop {
            while let Some(c) = self.peek() {
                if c.is_ascii_whitespace() { self.pos += 1; } else if c == b'(' && self.src.get(self.pos + 1) == Some(&b'*') {
                    self.pos += 2;
                    let mut depth = 1usize;
                    while depth > 0 {
                        if self.src.get(self.pos) == Some(&b'(') && self.src.get(self.pos + 1) == Some(&b'*') {
                            depth += 1; self.pos += 2;
                        } else if self.src.get(self.pos) == Some(&b'*') && self.src.get(self.pos + 1) == Some(&b')') {
                            depth -= 1; self.pos += 2;
                        } else if self.bump().is_none() {
                            return Err(ParseError { span: Span::new(self.src.len(), self.src.len()), msg: "注释未闭合".into() });
                        }
                    }
                } else { break; }
            }
            let start = self.pos;
            let Some(c) = self.peek() else {
                out.push(Tok { kind: TokKind::Eof, start, end: start });
                break;
            };
            if c.is_ascii_digit() {
                let mut n = 0i64;
                while let Some(d) = self.peek() {
                    if d.is_ascii_digit() {
                        n = n.saturating_mul(10).saturating_add((d - b'0') as i64);
                        self.pos += 1;
                    } else { break; }
                }
                out.push(Tok { kind: TokKind::Int(n), start, end: self.pos });
            } else if c == b'"' {
                self.pos += 1;
                let mut s = String::new();
                loop {
                    match self.bump() {
                        Some(b'"') => break,
                        Some(b'\\') => match self.bump() {
                            Some(b'n') => s.push('\n'),
                            Some(b't') => s.push('\t'),
                            Some(b'"') => s.push('"'),
                            Some(b'\\') => s.push('\\'),
                            Some(other) => s.push(other as char),
                            None => return Err(ParseError { span: Span::new(start, self.pos), msg: "字符串未闭合".into() }),
                        },
                        Some(ch) => s.push(ch as char),
                        None => return Err(ParseError { span: Span::new(start, self.pos), msg: "字符串未闭合".into() }),
                    }
                }
                out.push(Tok { kind: TokKind::Str(s), start, end: self.pos });
            } else if c == b'#' {
                self.pos += 1;
                let lstart = self.pos;
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_alphanumeric() || ch == b'_' { self.pos += 1; } else { break; }
                }
                if self.pos == lstart {
                    return Err(ParseError { span: Span::new(start, self.pos), msg: "# 后缺少标签名".into() });
                }
                let name = std::str::from_utf8(&self.src[lstart..self.pos]).unwrap().to_string();
                out.push(Tok { kind: TokKind::Tag(name), start, end: self.pos });
            } else if c.is_ascii_alphabetic() || c == b'_' {
                while let Some(ch) = self.peek() {
                    if ch.is_ascii_alphanumeric() || ch == b'_' { self.pos += 1; } else { break; }
                }
                let word = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
                let kind = match word {
                    "let" => TokKind::Let,
                    "rec" => TokKind::Rec,
                    "in" => TokKind::In,
                    "fun" => TokKind::Fun,
                    "true" => TokKind::True,
                    "false" => TokKind::False,
                    "match" => TokKind::Match,
                    "with" => TokKind::With,
                    _ => TokKind::Ident(word.to_string()),
                };
                out.push(Tok { kind, start, end: self.pos });
            } else {
                self.pos += 1;
                let kind = match c {
                    b':' => TokKind::Colon,
                    b'.' => TokKind::Dot,
                    b'=' => TokKind::Eq,
                    b'\\' => TokKind::Backslash,
                    b'(' => TokKind::LParen,
                    b')' => TokKind::RParen,
                    b'{' => TokKind::LBrace,
                    b'}' => TokKind::RBrace,
                    b'|' => TokKind::Bar,
                    b'-' if self.peek() == Some(b'>') => { self.pos += 1; TokKind::Arrow }
                    _ => return Err(ParseError { span: Span::new(start, self.pos), msg: format!("无法识别的字符 {:?}", c as char) }),
                };
                out.push(Tok { kind, start, end: self.pos });
            }
        }
        Ok(out)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

// ---------------- 解析器 ----------------

struct Parser {
    toks: Vec<Tok>,
    pos: usize,
    nodes: HashMap<NodeId, ENode>,
    anns: HashMap<NodeId, Ann>,
    next: NodeId,
}

impl Parser {
    fn new(toks: Vec<Tok>) -> Self {
        Parser { toks, pos: 0, next: 1, nodes: HashMap::new(), anns: HashMap::new() }
    }

    fn cur(&self) -> &Tok {
        &self.toks[self.pos]
    }

    fn at(&self, k: &TokKind) -> bool {
        std::mem::discriminant(&self.toks[self.pos].kind) == std::mem::discriminant(k)
    }

    fn eat(&mut self, k: TokKind) -> bool {
        if self.at(&k) { self.pos += 1; true } else { false }
    }

    fn expect(&mut self, k: TokKind, what: &str) -> Result<Tok, ParseError> {
        if self.at(&k) {
            let t = self.toks[self.pos].clone();
            self.pos += 1;
            Ok(t)
        } else {
            let t = self.cur().clone();
            Err(ParseError { span: Span::new(t.start, t.end), msg: format!("期望 {what}") })
        }
    }

    fn alloc(&mut self, span: Span, kind: EKind) -> NodeId {
        let id = self.next;
        self.next += 1;
        self.nodes.insert(id, ENode { id, span, kind });
        id
    }

    fn ident(&mut self) -> Result<(String, Span), ParseError> {
        if let TokKind::Ident(s) = &self.cur().kind {
            let t = self.cur().clone();
            self.pos += 1;
            Ok((s.clone(), Span::new(t.start, t.end)))
        } else {
            let t = self.cur().clone();
            Err(ParseError { span: Span::new(t.start, t.end), msg: "期望标识符".into() })
        }
    }

    fn tag(&mut self) -> Result<(String, Span), ParseError> {
        if let TokKind::Tag(s) = self.cur().kind {
            let t = self.cur().clone();
            self.pos += 1;
            Ok((s.clone(), Span::new(t.start, t.end)))
        } else {
            let t = self.cur().clone();
            Err(ParseError { span: Span::new(t.start, t.end), msg: "期望标签 #名字".into() })
        }
    }

    fn parse_program(&mut self) -> Result<NodeId, ParseError> {
        let e = self.parse_expr()?;
        if !self.at(&TokKind::Eof) {
            let t = self.cur().clone();
            return Err(ParseError { span: Span::new(t.start, t.end), msg: "表达式后有多余符号".into() });
        }
        Ok(e)
    }

    fn parse_expr(&mut self) -> Result<NodeId, ParseError> {
        // 无类型注解的一层（在 ann 下避免循环）
        let mut e = self.parse_ann()?;
        while matches!(self.cur().kind, TokKind::Colon) {
            let t = self.cur().clone();
            self.pos += 1;
            let (ty, tend) = self.parse_type()?;
            let span = Span::new(self.nodes[&e].span.start, tend);
            let id = self.alloc(span, EKind::Ann { expr: e });
            self.anns.insert(id, Ann { span: Span::new(t.start, tend), ty });
            e = id;
        }
        Ok(e)
    }

    fn parse_ann(&mut self) -> Result<NodeId, ParseError> {
        self.parse_fun()
    }

    fn parse_fun(&mut self) -> Result<NodeId, ParseError> {
        if matches!(self.cur().kind, TokKind::Backslash | TokKind::Fun) {
            let start = self.cur().start;
            self.pos += 1;
            let (var, _) = self.ident()?;
            self.expect(TokKind::Arrow, "`->`")?;
            let body = self.parse_expr()?;
            let end = self.nodes[&body].span.end;
            return Ok(self.alloc(Span::new(start, end as usize), EKind::Lam { var, body }));
        }
        if self.at(&TokKind::Let) {
            let start = self.cur().start;
            self.pos += 1;
            let rec = self.eat(TokKind::Rec);
            let (name, _) = self.ident()?;
            self.expect(TokKind::Eq, "`=`")?;
            let bound = self.parse_expr()?;
            self.expect(TokKind::In, "`in`")?;
            let body = self.parse_expr()?;
            let end = self.nodes[&body].span.end;
            return Ok(self.alloc(Span::new(start, end as usize),
                EKind::Let { rec, name, bound, body }));
        }
        self.parse_app()
    }

    fn parse_app(&mut self) -> Result<NodeId, ParseError> {
        let mut e = self.parse_atom()?;
        loop {
            if matches!(self.cur().kind,
                TokKind::Int(_) | TokKind::Str(_) | TokKind::Ident(_) | TokKind::Tag(_)
                | TokKind::True | TokKind::False | TokKind::LParen | TokKind::LBrace
                | TokKind::Backslash | TokKind::Fun | TokKind::Let | TokKind::Match)
            {
                // 投影
                if self.at(&TokKind::Dot) {
                    self.pos += 1;
                    let (field, tspan) = self.ident()?;
                    e = self.alloc(Span::new(self.nodes[&e].span.start, tspan.end as usize),
                        EKind::Proj { field, expr: e });
                    continue;
                }
                let arg = self.parse_atom()?;
                e = self.alloc(Span::new(self.nodes[&e].span.start, self.nodes[&arg].span.end as usize),
                    EKind::App { fun: e, arg });
            } else if self.at(&TokKind::Dot) {
                self.pos += 1;
                let (field, tspan) = self.ident()?;
                e = self.alloc(Span::new(self.nodes[&e].span.start, tspan.end as usize),
                    EKind::Proj { field, expr: e });
            } else { break; }
        }
        Ok(e)
    }

    fn parse_atom(&mut self) -> Result<NodeId, ParseError> {
        let t = self.cur().clone();
        match &t.kind {
            TokKind::Int(n) => { self.pos += 1; Ok(self.alloc(Span::new(t.start, t.end), EKind::LitInt(*n))) }
            TokKind::Str(s) => { self.pos += 1; Ok(self.alloc(Span::new(t.start, t.end), EKind::LitStr(s.clone()))) }
            TokKind::True => { self.pos += 1; Ok(self.alloc(Span::new(t.start, t.end), EKind::LitBool(true))) }
            TokKind::False => { self.pos += 1; Ok(self.alloc(Span::new(t.start, t.end), EKind::LitBool(false))) }
            TokKind::Ident(s) => { self.pos += 1; Ok(self.alloc(Span::new(t.start, t.end), EKind::Var(s.clone()))) }
            TokKind::LParen => {
                self.pos += 1;
                let e = self.parse_expr()?;
                self.expect(TokKind::RParen, "`)`")?;
                Ok(e)
            }
            TokKind::LBrace => self.parse_record(),
            TokKind::Tag(_) => self.parse_variant(),
            TokKind::Match => self.parse_match(),
            _ => Err(ParseError { span: Span::new(t.start, t.end), msg: "期望表达式".into() }),
        }
    }

    fn parse_record(&mut self) -> Result<NodeId, ParseError> {
        let start = self.cur().start;
        self.pos += 1;
        let mut fields = Vec::new();
        let mut seen = std::collections::HashSet::new();
        if !self.at(&TokKind::RBrace) {
            loop {
                let (name, nspan) = self.ident()?;
                self.expect(TokKind::Eq, "`=`")?;
                let val = self.parse_expr()?;
                if !seen.insert(name.clone()) {
                    return Err(ParseError { span: nspan, msg: format!("字段 `{name}` 重复") });
                }
                fields.push((name, val));
                if !self.eat(TokKind::Bar) { break; }
            }
        }
        let close = self.expect(TokKind::RBrace, "`}`")?;
        Ok(self.alloc(Span::new(start, close.end), EKind::Record(fields)))
    }

    fn parse_variant(&mut self) -> Result<NodeId, ParseError> {
        let (tag, span) = self.tag()?;
        // 若后面紧跟原子，则载荷就是该原子（单参数变体注入）。
        let starts_payload = matches!(self.cur().kind,
            TokKind::Int(_) | TokKind::Str(_) | TokKind::Ident(_) | TokKind::Tag(_)
            | TokKind::True | TokKind::False | TokKind::LParen | TokKind::LBrace);
        if starts_payload {
            let payload = self.parse_atom()?;
            let end = self.nodes[&payload].span.end;
            Ok(self.alloc(Span::new(span.start, end), EKind::Variant { tag, payload: Some(payload) }))
        } else {
            Ok(self.alloc(span, EKind::Variant { tag, payload: None }))
        }
    }

    fn parse_match(&mut self) -> Result<NodeId, ParseError> {
        let start = self.cur().start;
        self.pos += 1;
        let scrut = self.parse_expr()?;
        self.expect(TokKind::With, "`with`")?;
        let mut arms = Vec::new();
        let mut seen_tags = std::collections::HashSet::new();
        loop {
            self.expect(TokKind::Bar, "`|` 分支")?;
            let astart = self.cur().start;
            let (tag, tspan) = self.tag()?;
            if !seen_tags.insert(tag.clone()) {
                return Err(ParseError { span: tspan, msg: format!("分支标签 `#{tag}` 重复") });
            }
            let var = if let TokKind::Ident(_) = self.cur().kind {
                Some(self.ident()?.0)
            } else { None };
            self.expect(TokKind::Arrow, "`->`")?;
            let body = self.parse_expr()?;
            let aend = self.nodes[&body].span.end;
            let arm = self.alloc(Span::new(astart, aend as usize),
                EKind::Arm { tag, var, body });
            arms.push(arm);
            if !self.at(&TokKind::Bar) { break; }
        }
        let end = self.cur().start;
        Ok(self.alloc(Span::new(start, end), EKind::Match { scrut, arms }))
    }

    // ----- 类型注解 -----
    fn parse_type(&mut self) -> Result<(TypeAst, usize), ParseError> {
        let lhs = self.parse_type_atom()?;
        if self.eat(TokKind::Arrow) {
            let (rhs, end) = self.parse_type()?;
            Ok((TypeAst::TFun(Box::new(lhs), Box::new(rhs)), end))
        } else {
            let end = self.cur().start;
            Ok((lhs, end))
        }
    }

    fn parse_type_atom(&mut self) -> Result<TypeAst, ParseError> {
        let t = self.cur().clone();
        match &t.kind {
            TokKind::Ident(s) => {
                self.pos += 1;
                Ok(if s.starts_with('_') || s == "row" {
                    TypeAst::TVar(s.clone())
                } else {
                    TypeAst::TCon(s.clone())
                })
            }
            TokKind::LBrace => {
                self.pos += 1;
                let mut fs = Vec::new();
                if !self.at(&TokKind::RBrace) {
                    loop {
                        let (name, _) = self.ident()?;
                        self.expect(TokKind::Colon, "`:`")?;
                        let ty = self.parse_type_atom()?;
                        fs.push((name, ty));
                        if !self.eat(TokKind::Bar) { break; }
                    }
                }
                self.expect(TokKind::RBrace, "`}`")?;
                Ok(TypeAst::TRecord(fs))
            }
            TokKind::LParen => {
                self.pos += 1;
                let (ty, _) = self.parse_type()?;
                self.expect(TokKind::RParen, "`)`")?;
                Ok(ty)
            }
            _ => Err(ParseError { span: Span::new(t.start, t.end), msg: "期望类型".into() }),
        }
    }
}

/// 解析为一棵带临时身份的 AST（身份在增量模块中重映射）。
pub fn parse(src: &str) -> Result<Ast, ParseError> {
    let toks = Lexer::new(src).lex()?;
    let mut p = Parser::new(toks);
    let root = p.parse_program()?;
    Ok(Ast { root: Some(root), nodes: p.nodes, anns: p.anns })
}
