//! Lexer, parser and a flat AST with stable identity paths.
//!
//! Each node gets a structural identity path (e.g. `let/rhs/app/arg[0]`).
//! Binder *names* never appear in paths; a variable reference resolves to
//! the binding's structural path, so renaming a variable does not shift
//! identities (see [`Ast::var_path`]).

use std::collections::HashMap;

pub type NodeId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: u32, hi: u32) -> Self {
        Span { lo, hi }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArmPat {
    Wild,
    /// `Tag` or `Tag x`
    Tag,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArmBody {
    /// nullary tag: body node itself
    Nullary,
    /// `Tag x -> b`, binder node then body
    Bind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeKind {
    LitInt(i64),
    LitStr,
    LitBool(bool),
    Var,
    Lam { binder: NodeId, body: NodeId },
    App { fun: NodeId, args: Vec<NodeId> },
    Let {
        rec: bool,
        binder: NodeId,
        rhs: NodeId,
        body: Option<NodeId>,
    },
    If { cond: NodeId, then_e: NodeId, else_e: NodeId },
    Record { fields: Vec<(String, NodeId)> },
    Field { obj: NodeId, label: String },
    Variant { label: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<Arm> },
    /// leaf for binders; never generated as an expression
    Binder,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arm {
    pub pat: ArmPat,
    pub label: Option<String>,
    pub binder: Option<NodeId>,
    pub body: NodeId,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub span: Span,
    pub name: Option<String>,
    pub role: String,
    pub path: String,
    pub label: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub nodes: Vec<Node>,
    pub root: NodeId,
    /// node id of a top-level let binding (in order)
    pub top_level: Vec<NodeId>,
}

impl Ast {
    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id as usize]
    }
    pub fn root_node(&self) -> &Node {
        self.node(self.root)
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}: {}", self.span.lo, self.span.hi, self.message)
    }
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub enum TokKind {
    Int(i64),
    Str(String),
    Ident(String),
    Upper(String),
    // keywords
    Let, Rec, In, Fun, If, Then, Else, True, False, Match, With, Underscore,
    // punctuation
    LParen, RParen, LBrace, RBrace, Comma, Eq, Arrow, Dot, Colon, Semicolon,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Tok {
    pub kind: TokKind,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Tok>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0usize;
    let mut toks = Vec::new();
    let mk = |kind: TokKind, lo: usize, hi: usize| Tok {
        kind,
        span: Span::new(lo as u32, hi as u32),
    };
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '(' && b.get(i + 1) == Some(&b'*') {
            let start = i;
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
            if depth > 0 {
                return Err(ParseError {
                    span: Span::new(start as u32, src.len() as u32),
                    message: "未闭合的注释 (* ... *)".into(),
                });
            }
            continue;
        }
        if c.is_ascii_digit() {
            let start = i;
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[start..i].parse().map_err(|_| ParseError {
                span: Span::new(start as u32, i as u32),
                message: "整数字面量溢出".into(),
            })?;
            toks.push(mk(TokKind::Int(n), start, i));
            continue;
        }
        if c == '"' {
            let start = i;
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    s.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(ParseError {
                    span: Span::new(start as u32, src.len() as u32),
                    message: "未闭合的字符串".into(),
                });
            }
            i += 1;
            toks.push(mk(TokKind::Str(s), start, i));
            continue;
        }
        if c == '_'
            && (i + 1 >= b.len()
                || !(b[i + 1] as char).is_alphanumeric()
                && b[i + 1] != b'_')
        {
            toks.push(mk(TokKind::Underscore, i, i + 1));
            i += 1;
            continue;
        }
        if c.is_alphabetic() || c == '_' {
            let start = i;
            while i < b.len()
                && ((b[i] as char).is_alphanumeric() || b[i] == b'_')
            {
                i += 1;
            }
            let word = &src[start..i];
            let kind = match word {
                "let" => TokKind::Let,
                "rec" => TokKind::Rec,
                "in" => TokKind::In,
                "fun" => TokKind::Fun,
                "if" => TokKind::If,
                "then" => TokKind::Then,
                "else" => TokKind::Else,
                "true" => TokKind::True,
                "false" => TokKind::False,
                "match" => TokKind::Match,
                "with" => TokKind::With,
                _ => {
                    let first = word.chars().next().unwrap();
                    if first.is_uppercase() {
                        TokKind::Upper(word.to_string())
                    } else {
                        TokKind::Ident(word.to_string())
                    }
                }
            };
            toks.push(mk(kind, start, i));
            continue;
        }
        let two = if i + 1 < b.len() { Some(&src[i..i + 2]) } else { None };
        if two == Some("->") {
            toks.push(mk(TokKind::Arrow, i, i + 2));
            i += 2;
            continue;
        }
        let one = match c {
            '(' => TokKind::LParen,
            ')' => TokKind::RParen,
            '{' => TokKind::LBrace,
            '}' => TokKind::RBrace,
            ',' => TokKind::Comma,
            '=' => TokKind::Eq,
            '.' => TokKind::Dot,
            ':' => TokKind::Colon,
            ';' => TokKind::Semicolon,
            _ => {
                return Err(ParseError {
                    span: Span::new(i as u32, (i + 1) as u32),
                    message: format!("无法识别的字符 {:?}", c),
                });
            }
        };
        toks.push(mk(one, i, i + 1));
        i += 1;
    }
    toks.push(mk(TokKind::Eof, src.len(), src.len()));
    Ok(toks)
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

struct P<'a> {
    src: &'a str,
    toks: Vec<Tok>,
    pos: usize,
    nodes: Vec<Node>,
    /// variable name -> stack of binder node ids
    scopes: Vec<HashMap<String, NodeId>>,
    top_level: Vec<NodeId>,
}

impl<'a> P<'a> {
    fn new(src: &'a str, toks: Vec<Tok>) -> Self {
        let mut p = P {
            src,
            toks,
            pos: 0,
            nodes: Vec::new(),
            scopes: vec![HashMap::new()],
            top_level: Vec::new(),
        };
        p
    }
    fn peek(&self) -> &Tok {
        &self.toks[self.pos]
    }
    fn at(&self, k: &TokKind) -> bool {
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(k)
    }
    fn bump(&mut self) -> Tok {
        let t = self.toks[self.pos].clone();
        if !matches!(t.kind, TokKind::Eof) {
            self.pos += 1;
        }
        t
    }
    fn expect(&mut self, k: TokKind, what: &str) -> Result<Tok, ParseError> {
        if self.at(&k) {
            Ok(self.bump())
        } else {
            let t = self.peek().clone();
            Err(ParseError {
                span: t.span,
                message: format!("期望 {}，但遇到 {}", what, tok_desc(&t.kind)),
            })
        }
    }
    fn err<T>(&self, span: Span, msg: impl Into<String>) -> Result<T, ParseError> {
        Err(ParseError { span, message: msg.into() })
    }
    fn alloc(&mut self, kind: NodeKind, span: Span, name: Option<String>, role: String, path: String, label: Option<String>) -> NodeId {

        let id = self.nodes.len() as NodeId;
        self.nodes.push(Node { id, kind, span, name, role, path, label });
        id
    }
    fn binder_at(&mut self, t: &Tok, path: String) -> NodeId {
        let name = match &t.kind {
            TokKind::Ident(s) => s.clone(),
            _ => String::new(),
        };
        let id = self.alloc(NodeKind::Binder, t.span, Some(name.clone()), "binder".into(), path, None);
        self.scopes.last_mut().unwrap().insert(name, id);
        id
    }
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }
    fn resolve(&self, name: &str) -> Option<NodeId> {
        for sc in self.scopes.iter().rev() {
            if let Some(id) = sc.get(name) {
                return Some(*id);
            }
        }
        None
    }
    fn binding_token(&self, id: NodeId) -> String {
        let n = &self.nodes[id as usize];
        n.path.replace('/', ".")
    }
}

pub fn tok_desc(k: &TokKind) -> String {
    match k {
        TokKind::Int(_) => "整数".into(),
        TokKind::Str(_) => "字符串".into(),
        TokKind::Ident(s) => format!("标识符 `{}`", s),
        TokKind::Upper(s) => format!("标签 `{}`", s),
        TokKind::Let => "`let`".into(),
        TokKind::Rec => "`rec`".into(),
        TokKind::In => "`in`".into(),
        TokKind::Fun => "`fun`".into(),
        TokKind::If => "`if`".into(),
        TokKind::Then => "`then`".into(),
        TokKind::Else => "`else`".into(),
        TokKind::True | TokKind::False => "布尔值".into(),
        TokKind::Match => "`match`".into(),
        TokKind::With => "`with`".into(),
        TokKind::Underscore => "`_`".into(),
        TokKind::LParen => "`(`".into(),
        TokKind::RParen => "`)`".into(),
        TokKind::LBrace => "`{`".into(),
        TokKind::RBrace => "`}`".into(),
        TokKind::Comma => "`,`".into(),
        TokKind::Eq => "`=`".into(),
        TokKind::Arrow => "`->`".into(),
        TokKind::Dot => "`.`".into(),
        TokKind::Colon => "`:`".into(),
        TokKind::Semicolon => "`;`".into(),
        TokKind::Eof => "输入结尾".into(),
    }
}

// ---------- expression parsing ----------

impl<'a> P<'a> {
    /// Parse an expression carrying inherited path prefix `prefix`.
    fn expr(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        match &self.peek().kind.clone() {
            TokKind::Fun => self.lam(prefix),
            TokKind::Let => self.let_expr(prefix, false),
            TokKind::If => self.if_expr(prefix),
            TokKind::Match => self.match_expr(prefix),
            TokKind::LBrace => self.record(prefix),
            _ => self.app_or_variant(prefix),
        }
    }

    fn lam(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        let start = self.bump().span.lo;
        self.push_scope();
        let bt = self.expect(TokKind::Ident(String::new()), "函数参数名")?;
        let binder = self.binder_at(&bt, format!("{}/lam/binder", prefix));
        self.expect(TokKind::Arrow, "`->`")?;
        let body = if self.at(&TokKind::Fun) {
            self.lam(&format!("{}/lam/body", prefix))?
        } else {
            self.expr(&format!("{}/lam/body", prefix))?
        };
        let hi = self.nodes[body as usize].span.hi;
        self.pop_scope();
        Ok(self.alloc(
            NodeKind::Lam { binder, body },
            Span::new(start, hi),
            None,
            "lam".into(),
            format!("{}/lam", prefix),
            None,
        ))
    }

    fn let_expr(&mut self, prefix: &str, top: bool) -> Result<NodeId, ParseError> {
        let start = self.bump().span.lo; // let
        let rec = if self.at(&TokKind::Rec) { self.bump(); true } else { false };
        let bt = self.expect(TokKind::Ident(String::new()), "let 绑定名")?;
        self.expect(TokKind::Eq, "`=`")?;
        if rec {
            self.push_scope(); // rec scope: binder visible in rhs
            let binder = self.binder_at(&bt, format!("{}/let/binder", prefix));
            let rhs = self.expr(&format!("{}/let/rhs", prefix))?;
            self.pop_scope();
            self.push_scope(); // body scope
            let body = self.let_body(prefix, top)?;
            self.finish_let(start, prefix, true, binder, rhs, body, top)
        } else {
            let rhs = self.expr(&format!("{}/let/rhs", prefix))?;
            self.push_scope(); // body scope
            let binder = self.binder_at(&bt, format!("{}/let/binder", prefix));
            let body = self.let_body(prefix, top)?;
            self.finish_let(start, prefix, false, binder, rhs, body, top)
        }
    }

    fn let_body(&mut self, prefix: &str, top: bool) -> Result<Option<NodeId>, ParseError> {
        if self.at(&TokKind::In) {
            self.bump();
            Ok(Some(self.expr(&format!("{}/let/body", prefix))?))
        } else if top {
            Ok(None)
        } else {
            self.err(self.peek().span, "非顶层的 let 必须以 `in` 接续表达式")
        }
    }

    fn finish_let(
        &mut self,
        start: u32,
        prefix: &str,
        rec: bool,
        binder: NodeId,
        rhs: NodeId,
        body: Option<NodeId>,
        top: bool,
    ) -> Result<NodeId, ParseError> {
        let hi = body.map(|b| self.nodes[b as usize].span.hi)
            .unwrap_or_else(|| self.nodes[rhs as usize].span.hi);
        self.pop_scope();
        let id = self.alloc(
            NodeKind::Let { rec, binder, rhs, body },
            Span::new(start, hi),
            None,
            "let".into(),
            format!("{}/let", prefix),
            None,
        );
        if top {
            self.top_level.push(id);
        }
        Ok(id)
    }

    fn if_expr(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        let start = self.bump().span.lo;
        let cond = self.expr(&format!("{}/if/cond", prefix))?;
        self.expect(TokKind::Then, "`then`")?;
        let then_e = self.expr(&format!("{}/if/then", prefix))?;
        self.expect(TokKind::Else, "`else`")?;
        let else_e = self.expr(&format!("{}/if/else", prefix))?;
        let hi = self.nodes[else_e as usize].span.hi;
        Ok(self.alloc(
            NodeKind::If { cond, then_e, else_e },
            Span::new(start, hi),
            None,
            "if".into(),
            format!("{}/if", prefix),
            None,
        ))
    }

    fn record(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        let start = self.bump().span.lo; // {
        let mut fields = Vec::new();
        if !self.at(&TokKind::RBrace) {
            loop {
                let lt = self.expect(TokKind::Ident(String::new()), "字段名")?;
                let label = match lt.kind {
                    TokKind::Ident(s) => s,
                    _ => unreachable!(),
                };
                self.expect(TokKind::Eq, "`=`")?;
                let v = self.expr(&format!("{}/rec/field[{}]", prefix, label))?;
                if fields.iter().any(|(l, _): &(String, NodeId)| *l == label) {
                    return self.err(lt.span, format!("记录字段 `{}` 重复", label));
                }
                fields.push((label, v));
                if self.at(&TokKind::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        let end = self.expect(TokKind::RBrace, "`}`")?;
        Ok(self.alloc(
            NodeKind::Record { fields },
            Span::new(start, end.span.hi),
            None,
            "record".into(),
            format!("{}/record", prefix),
            None,
        ))
    }

    fn match_expr(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        let start = self.bump().span.lo; // match
        let scrut = self.expr(&format!("{}/match/scrut", prefix))?;
        self.expect(TokKind::With, "`with`")?;
        let mut arms = Vec::new();
        loop {
            self.expect(TokKind::Arrow, "分支 `->`（或结束 match）")?;
            let (pat, label, binder_tok) = match self.peek().kind.clone() {
                TokKind::Underscore => {
                    self.bump();
                    (ArmPat::Wild, None, None)
                }
                TokKind::Upper(s) => {
                    let t = self.bump();
                    match self.peek().kind.clone() {
                        TokKind::Ident(x) => {
                            let bt = self.bump();
                            (ArmPat::Tag, Some(s), Some((x, bt, t.span)))
                        }
                        _ => (ArmPat::Tag, Some(s), None),
                    }
                }
                other => {
                    return self.err(self.peek().span, format!("match 分支模式应为标签或 `_`，遇到 {}", tok_desc(&other)));
                }
            };
            let mut binder_id = None;
            let body_prefix;
            let arm_idx = arms.len();
            match &binder_tok {
                Some((_, bt, tag_span)) => {
                    self.push_scope();
                    let bpath = format!("{}/match/arm[{}:{}/binder", prefix, arm_idx, label.as_ref().unwrap());
                    let bid = self.binder_at(bt, bpath);
                    binder_id = Some(bid);
                    body_prefix = format!(
                        "{}/match/arm[{}:{}]/body",
                        prefix,
                        arm_idx,
                        label.as_ref().unwrap()
                    );
                    let _ = tag_span;
                }
                None => {
                    body_prefix = format!(
                        "{}/match/arm[{}:{}/null]/body",
                        prefix,
                        arm_idx,
                        label.as_deref().unwrap_or("_")
                    );
                }
            }
            self.expect(TokKind::Arrow, "模式后的 `->`")?;
            let body = self.expr(&body_prefix)?;
            if binder_id.is_some() {
                self.pop_scope();
            }
            arms.push(Arm { pat, label, binder: binder_id, body });
            if matches!(self.peek().kind, TokKind::Arrow) {
                continue;
            }
            break;
        }
        let hi = {
            let last = arms.last().unwrap().body;
            self.nodes[last as usize].span.hi
        };
        Ok(self.alloc(
            NodeKind::Match { scrut, arms },
            Span::new(start, hi),
            None,
            "match".into(),
            format!("{}/match", prefix),
            None,
        ))
    }
}

// ---------- atoms, postfix, application, top-level ----------

impl<'a> P<'a> {
    fn postfix(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        let mut e = self.atom(prefix)?;
        loop {
            if self.at(&TokKind::Dot) {
                self.bump();
                let lt = self.expect(TokKind::Ident(String::new()), "字段名")?;
                let label = match lt.kind {
                    TokKind::Ident(s) => s,
                    _ => unreachable!(),
                };
                let hi = lt.span.hi;
                let lo = self.nodes[e as usize].span.lo;
                e = self.alloc(
                    NodeKind::Field { obj: e, label: label.clone() },
                    Span::new(lo, hi),
                    None,
                    "field".into(),
                    format!("{}/field[{}]", prefix, label),
                    Some(label),
                );
            } else {
                break;
            }
        }
        Ok(e)
    }

    fn atom(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        let t = self.peek().clone();
        match t.kind {
            TokKind::Int(n) => {
                self.bump();
                Ok(self.alloc(NodeKind::LitInt(n), t.span, None, "int".into(), format!("{}/int", prefix), None))
            }
            TokKind::Str(s) => {
                self.bump();
                Ok(self.alloc(NodeKind::LitStr, t.span, None, "string".into(), format!("{}/string", prefix), Some(s)))
            }
            TokKind::True => {
                self.bump();
                Ok(self.alloc(NodeKind::LitBool(true), t.span, None, "bool".into(), format!("{}/bool", prefix), None))
            }
            TokKind::False => {
                self.bump();
                Ok(self.alloc(NodeKind::LitBool(false), t.span, None, "bool".into(), format!("{}/bool", prefix), None))
            }
            TokKind::Upper(tag) => {
                self.bump();
                Ok(self.alloc(
                    NodeKind::Variant { label: tag.clone(), payload: None },
                    t.span,
                    None,
                    "variant".into(),
                    format!("{}/variant[{}]", prefix, tag),
                    Some(tag),
                ))
            }
            TokKind::Ident(name) => {
                self.bump();
                match self.resolve(&name) {
                    Some(bid) => {
                        let tok = self.binding_token(bid);
                        Ok(self.alloc(
                            NodeKind::Var,
                            t.span,
                            Some(name),
                            "var".into(),
                            format!("{}/var@{}", prefix, tok),
                            None,
                        ))
                    }
                    None => self.err(t.span, format!("未绑定的变量 `{}`", name)),
                }
            }
            TokKind::LParen => {
                self.bump();
                let e = self.expr(prefix)?;
                self.expect(TokKind::RParen, "`)`")?;
                Ok(e)
            }
            ref other => self.err(t.span, format!("需要表达式，遇到 {}", tok_desc(other))),
        }
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek().kind,
            TokKind::Int(_)
                | TokKind::Str(_)
                | TokKind::True
                | TokKind::False
                | TokKind::Upper(_)
                | TokKind::Ident(_)
                | TokKind::LParen
                | TokKind::LBrace
        )
    }

    fn next_starts_atom(&self) -> bool {
        // lookahead one token (skips nothing; tokens carry no layout)
        if self.pos + 1 >= self.toks.len() {
            return false;
        }
        matches!(
            self.toks[self.pos + 1].kind,
            TokKind::Int(_)
                | TokKind::Str(_)
                | TokKind::True
                | TokKind::False
                | TokKind::Ident(_)
                | TokKind::LParen
        )
    }

    fn app_or_variant(&mut self, prefix: &str) -> Result<NodeId, ParseError> {
        // `Tag payload` -> variant with payload (no nullary node left behind)
        if let TokKind::Upper(tag) = self.peek().kind.clone() {
            if self.next_starts_atom() {
                let tt = self.bump();
                let payload = self.postfix(&format!("{}/variant/payload", prefix))?;
                let hi = self.nodes[payload as usize].span.hi;
                return Ok(self.alloc(
                    NodeKind::Variant { label: tag.clone(), payload: Some(payload) },
                    Span::new(tt.span.lo, hi),
                    None,
                    "variant".into(),
                    format!("{}/variant[{}]", prefix, tag),
                    Some(tag),
                ));
            }
        }
        let head = self.postfix(prefix)?;
        let mut args = Vec::new();
        while self.starts_atom() && !matches!(self.peek().kind, TokKind::LBrace) {
            let ap = format!("{}/app/arg[{}]", prefix, args.len());
            args.push(self.postfix(&ap)?);
        }
        if args.is_empty() {
            Ok(head)
        } else {
            let lo = self.nodes[head as usize].span.lo;
            let hi = self.nodes[*args.last().unwrap() as usize].span.hi;
            Ok(self.alloc(
                NodeKind::App { fun: head, args },
                Span::new(lo, hi),
                None,
                "app".into(),
                format!("{}/app", prefix),
                None,
            ))
        }
    }
}

// ---------- top-level program ----------

impl<'a> P<'a> {
    fn program(&mut self) -> Result<NodeId, ParseError> {
        // Sequence of top-level `let ...;` declarations, then a final
        // expression. A terminal let without `in` is the implicit body.
        let mut last: Option<NodeId> = None;
        let mut decl_no = 0usize;
        while self.at(&TokKind::Let) {
            decl_no += 1;
            let prefix = format!("root/toplet#{}", decl_no);
            let id = self.let_expr(&prefix, true)?;
            last = Some(id);
            if self.at(&TokKind::Semicolon) {
                self.bump();
                continue;
            }
            if matches!(self.peek().kind, TokKind::Eof) {
                break;
            }
            return self.err(self.peek().span, "顶层声明之间需要 `;` 分隔");
        }
        let root = if matches!(self.peek().kind, TokKind::Eof) {
            match last {
                Some(id) => id,
                None => return self.err(self.peek().span, "空程序"),
            }
        } else {
            let e = self.expr("root")?;
            self.make_seq(last, e)?
        };
        if !matches!(self.peek().kind, TokKind::Eof) {
            return self.err(self.peek().span, format!("输入末尾有多余符号 {}", tok_desc(&self.peek().kind)));
        }
        Ok(root)
    }

    /// Wrap top-level lets + final expression into a single chain.
    fn make_seq(&mut self, last_let: Option<NodeId>, final_e: NodeId) -> Result<NodeId, ParseError> {
        if last_let.is_none() {
            return Ok(final_e);
        }
        // Rebuild chain bottom-up: each top-level let currently has body=None.
        // Allocate wrapper nodes that reuse the same binder/rhs paths by
        // constructing synthetic Let nodes.
        let top = self.top_level.clone();
        // Synthetic wrappers nest the declarations. Binder/rhs subtrees keep
        // their original stable paths; only the wrapper's own path is marked
        // `top` so it never collides with a nested `let`.
        let mut body = final_e;
        for (idx, &lid) in top.iter().rev().enumerate() {
            let n = self.nodes[lid as usize].clone();
            let (rec, binder, rhs) = match n.kind {
                NodeKind::Let { rec, binder, rhs, .. } => (rec, binder, rhs),
                _ => unreachable!(),
            };
            let span = Span::new(n.span.lo, self.nodes[body as usize].span.hi);
            let path = n.path.clone();
            body = self.alloc(
                NodeKind::Let { rec, binder, rhs, body: Some(body) },
                span,
                None,
                "let".into(),
                path,
                None,
            );
        }
        Ok(body)
    }
}

pub fn parse(src: &str) -> Result<Ast, ParseError> {
    let toks = lex(src)?;
    let mut p = P::new(src, toks);
    let root = p.program()?;
    Ok(Ast { nodes: p.nodes, root, top_level: p.top_level })
}

impl Node {
    /// Direct child node ids (binder + expression children).
    pub fn children(&self) -> Vec<NodeId> {
        let mut out = Vec::new();
        match self.kind {
            NodeKind::Lam { binder, body } => {
                out.push(binder);
                out.push(body);
            }
            NodeKind::App { fun, ref args } => {
                out.push(fun);
                out.extend(args);
            }
            NodeKind::Let { binder, rhs, body, .. } => {
                out.push(binder);
                out.push(rhs);
                if let Some(b) = body {
                    out.push(b);
                }
            }
            NodeKind::If { cond, then_e, else_e } => {
                out.push(cond);
                out.push(then_e);
                out.push(else_e);
            }
            NodeKind::Record { ref fields } => out.extend(fields.iter().map(|(_, id)| *id)),
            NodeKind::Field { obj, .. } => out.push(obj),
            NodeKind::Variant { payload, .. } => {
                if let Some(p) = payload {
                    out.push(p);
                }
            }
            NodeKind::Match { scrut, ref arms } => {
                out.push(scrut);
                for a in arms {
                    if let Some(b) = a.binder {
                        out.push(b);
                    }
                    out.push(a.body);
                }
            }
            _ => {}
        }
        out
    }
}
