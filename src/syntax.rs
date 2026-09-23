// 词法/语法分析：源码 -> 带稳定节点身份与来源范围的 AST。
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Pat {
    PVar { name: String, span: Span, nid: u32 },
    PWild { span: Span, nid: u32 },
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::PVar { span, .. } | Pat::PWild { span, .. } => *span,
        }
    }
    pub fn nid(&self) -> u32 {
        match self {
            Pat::PVar { nid, .. } | Pat::PWild { nid, .. } => *nid,
        }
    }
    // 结构签名：变量改名不影响。
    pub fn signature(&self) -> String {
        match self {
            Pat::PVar { .. } => "pvar".into(),
            Pat::PWild { .. } => "pwild".into(),
        }
    }
    pub fn walk_exprs(&self) {}
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Expr {
    Int { value: i64, span: Span, nid: u32 },
    Bool { value: bool, span: Span, nid: u32 },
    Str { value: String, span: Span, nid: u32 },
    Var { name: String, span: Span, nid: u32 },
    Lam { param: Pat, body: Box<Expr>, span: Span, nid: u32 },
    App { fun: Box<Expr>, arg: Box<Expr>, span: Span, nid: u32 },
    Let { is_rec: bool, name: Pat, value: Box<Expr>, body: Box<Expr>, span: Span, nid: u32 },
    If { cond: Box<Expr>, then_e: Box<Expr>, else_e: Box<Expr>, span: Span, nid: u32 },
    Record { fields: Vec<(String, Expr)>, span: Span, nid: u32 },
    Field { record: Box<Expr>, label: String, span: Span, nid: u32 },
    Variant { label: String, payload: Option<Box<Expr>>, span: Span, nid: u32 },
    Match { scrutinee: Box<Expr>, arms: Vec<(Pat, Expr)>, span: Span, nid: u32 },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int { span, .. }
            | Expr::Bool { span, .. }
            | Expr::Str { span, .. }
            | Expr::Var { span, .. }
            | Expr::Lam { span, .. }
            | Expr::App { span, .. }
            | Expr::Let { span, .. }
            | Expr::If { span, .. }
            | Expr::Record { span, .. }
            | Expr::Field { span, .. }
            | Expr::Variant { span, .. }
            | Expr::Match { span, .. } => *span,
        }
    }
    pub fn nid(&self) -> u32 {
        match self {
            Expr::Int { nid, .. }
            | Expr::Bool { nid, .. }
            | Expr::Str { nid, .. }
            | Expr::Var { nid, .. }
            | Expr::Lam { nid, .. }
            | Expr::App { nid, .. }
            | Expr::Let { nid, .. }
            | Expr::If { nid, .. }
            | Expr::Record { nid, .. }
            | Expr::Field { nid, .. }
            | Expr::Variant { nid, .. }
            | Expr::Match { nid, .. } => *nid,
        }
    }
    // 结构标签（增量节点对齐用）。变量名与字面量值不参与，改名/改数字不漂移。
    pub fn signature(&self) -> String {
        match self {
            Expr::Int { .. } => "int".into(),
            Expr::Bool { .. } => "bool".into(),
            Expr::Str { .. } => "str".into(),
            Expr::Var { .. } => "var".into(),
            Expr::Lam { .. } => "lam".into(),
            Expr::App { .. } => "app".into(),
            Expr::Let { is_rec, .. } => format!("let{}", if *is_rec { "rec" } else { "" }),
            Expr::If { .. } => "if".into(),
            Expr::Record { fields, .. } => {
                let mut labels: Vec<&str> = fields.iter().map(|(l, _)| l.as_str()).collect();
                labels.sort();
                format!("rec[{}]", labels.join(","))
            }
            Expr::Field { label, .. } => format!("field:{label}"),
            Expr::Variant { label, .. } => format!("variant:{label}"),
            Expr::Match { arms, .. } => format!("match:{}", arms.len()),
        }
    }
    pub fn walk(&self, f: &mut impl FnMut(&Expr)) {
        f(self);
        self.children(&mut |e| e.walk(f));
    }
    pub fn children(&self, f: &mut impl FnMut(&Expr)) {
        match self {
            Expr::Lam { body, .. } => f(body),
            Expr::App { fun, arg, .. } => {
                f(fun);
                f(arg);
            }
            Expr::Let { value, body, .. } => {
                f(value);
                f(body);
            }
            Expr::If { cond, then_e, else_e, .. } => {
                f(cond);
                f(then_e);
                f(else_e);
            }
            Expr::Record { fields, .. } => {
                for (_, e) in fields {
                    f(e);
                }
            }
            Expr::Field { record, .. } => f(record),
            Expr::Variant { payload, .. } => {
                if let Some(p) = payload {
                    f(p);
                }
            }
            Expr::Match { scrutinee, arms, .. } => {
                f(scrutinee);
                for (_, e) in arms {
                    f(e);
                }
            }
            _ => {}
        }
    }
    pub fn walk_mut(&mut self, f: &mut impl FnMut(&mut Expr)) {
        match self {
            Expr::Lam { body, .. } => body.walk_mut(f),
            Expr::App { fun, arg, .. } => {
                fun.walk_mut(f);
                arg.walk_mut(f);
            }
            Expr::Let { value, body, .. } => {
                value.walk_mut(f);
                body.walk_mut(f);
            }
            Expr::If { cond, then_e, else_e, .. } => {
                cond.walk_mut(f);
                then_e.walk_mut(f);
                else_e.walk_mut(f);
            }
            Expr::Record { fields, .. } => {
                for (_, e) in fields {
                    e.walk_mut(f);
                }
            }
            Expr::Field { record, .. } => record.walk_mut(f),
            Expr::Variant { payload, .. } => {
                if let Some(p) = payload {
                    p.walk_mut(f);
                }
            }
            Expr::Match { scrutinee, arms, .. } => {
                scrutinee.walk_mut(f);
                for (_, e) in arms {
                    e.walk_mut(f);
                }
            }
            _ => {}
        }
        f(self);
    }
    pub fn nid_map(&self) -> BTreeMap<u32, Span> {
        let mut map = BTreeMap::new();
        self.walk(&mut |e| {
            map.insert(e.nid(), e.span());
        });
        map
    }
}

// ---------------- 词法 ----------------

#[derive(Debug, Clone, PartialEq)]
pub enum TokKind {
    Int(i64),
    Bool(bool),
    Str(String),
    Ident(String),
    Sym(&'static str),
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokKind,
    pub span: Span,
}

const SYMBOLS: &[&str] = &[
    "->", "|>", "|", ".", ",", ";", "=", "(", ")", "{", "}", "[", "]", "+", "==", "&&", "`",
];
const KEYWORDS: &[&str] = &[
    "let", "rec", "in", "fn", "fun", "if", "then", "else", "match", "with", "true", "false",
];

pub struct Lexer<'a> {
    src: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            src,
            bytes: src.as_bytes(),
            pos: 0,
        }
    }

    fn peek(&self, off: usize) -> Option<u8> {
        self.bytes.get(self.pos + off).copied()
    }

    fn skip_trivia(&mut self) {
        loop {
            match self.peek(0) {
                Some(b' ') | Some(b'\t') | Some(b'\r') | Some(b'\n') => self.pos += 1,
                Some(b'/') if self.peek(1) == Some(b'/') => {
                    while let Some(c) = self.peek(0) {
                        if c == b'\n' {
                            break;
                        }
                        self.pos += 1;
                    }
                }
                _ => break,
            }
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, ParseError> {
        let mut out = Vec::new();
        loop {
            self.skip_trivia();
            let start = self.pos;
            let Some(c) = self.peek(0) else {
                out.push(Token { kind: TokKind::Eof, span: Span::new(start, start) });
                break;
            };
            if c.is_ascii_digit() {
                let mut val = 0i64;
                while let Some(d) = self.peek(0) {
                    if !d.is_ascii_digit() {
                        break;
                    }
                    val = val.saturating_mul(10).saturating_add((d - b'0') as i64);
                    self.pos += 1;
                }
                out.push(Token { kind: TokKind::Int(val), span: Span::new(start, self.pos) });
            } else if c == b'"' {
                self.pos += 1;
                let mut s = String::new();
                loop {
                    let Some(b) = self.peek(0) else {
                        return Err(ParseError::new("字符串未闭合", Span::new(start, self.pos)));
                    };
                    if b == b'"' {
                        self.pos += 1;
                        break;
                    }
                    if b == b'\\' {
                        self.pos += 1;
                        match self.peek(0) {
                            Some(b'n') => {
                                s.push('\n');
                                self.pos += 1;
                            }
                            Some(b't') => {
                                s.push('\t');
                                self.pos += 1;
                            }
                            Some(b'"') => {
                                s.push('"');
                                self.pos += 1;
                            }
                            Some(b'\\') => {
                                s.push('\\');
                                self.pos += 1;
                            }
                            _ => {
                                return Err(ParseError::new(
                                    "未知转义字符",
                                    Span::new(self.pos, self.pos + 1),
                                ))
                            }
                        }
                    } else {
                        let width = if b < 0x80 {
                            1
                        } else if b >> 5 == 0b110 {
                            2
                        } else if b >> 4 == 0b1110 {
                            3
                        } else {
                            4
                        };
                        let end = (self.pos + width).min(self.bytes.len());
                        s.push_str(
                            std::str::from_utf8(&self.bytes[self.pos..end]).map_err(|_| {
                                ParseError::new("非法 UTF-8", Span::new(self.pos, end))
                            })?,
                        );
                        self.pos = end;
                    }
                }
                out.push(Token { kind: TokKind::Str(s), span: Span::new(start, self.pos) });
            } else if c == b'_' || c.is_ascii_alphabetic() {
                while let Some(d) = self.peek(0) {
                    if d == b'_' || d.is_ascii_alphanumeric() {
                        self.pos += 1;
                    } else {
                        break;
                    }
                }
                let word = &self.src[start..self.pos];
                let kind = if KEYWORDS.contains(&word) {
                    TokKind::Sym(Box::leak(word.to_string().into_boxed_str()))
                } else {
                    TokKind::Ident(word.to_string())
                };
                out.push(Token { kind, span: Span::new(start, self.pos) });
            } else {
                let mut matched: Option<&'static str> = None;
                for sym in SYMBOLS {
                    if self.bytes[self.pos..].starts_with(sym.as_bytes()) {
                        matched = Some(sym);
                        break;
                    }
                }
                if let Some(sym) = matched {
                    self.pos += sym.len();
                    out.push(Token { kind: TokKind::Sym(sym), span: Span::new(start, self.pos) });
                } else {
                    return Err(ParseError::new("无法识别的字符", Span::new(start, start + 1)));
                }
            }
        }
        Ok(out)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        ParseError { message: message.into(), span }
    }
}

// ---------------- 语法分析 ----------------

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
    next_nid: u32,
}

impl Parser {
    // id_start：增量分析时从旧树复用 nid，新节点从 max+1 开始。
    pub fn new(toks: Vec<Token>, id_start: u32) -> Self {
        Parser { toks, idx: 0, next_nid: id_start }
    }

    fn alloc(&mut self) -> u32 {
        let id = self.next_nid;
        self.next_nid = id + 1;
        id
    }

    pub fn max_id(&self) -> u32 {
        self.next_nid.saturating_sub(1)
    }

    fn cur(&self) -> &Token {
        &self.toks[self.idx]
    }
    fn cur_kind(&self) -> &TokKind {
        &self.toks[self.idx].kind
    }
    fn span_of(&self, i: usize) -> Span {
        self.toks[i].span
    }
    fn at_sym(&self, s: &str) -> bool {
        matches!(self.cur_kind(), TokKind::Sym(w) if *w == s)
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.idx].clone();
        if self.idx + 1 < self.toks.len() {
            self.idx += 1;
        }
        t
    }
    fn eat_sym(&mut self, s: &str) -> bool {
        if self.at_sym(s) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect_sym(&mut self, s: &str) -> Result<Span, ParseError> {
        if self.at_sym(s) {
            Ok(self.bump().span)
        } else {
            Err(ParseError::new(
                format!("期望 '{s}'"),
                self.cur().span,
            ))
        }
    }

    fn eat_ident(&mut self) -> Option<(String, Span)> {
        if let TokKind::Ident(name) = self.cur_kind() {
            let t = self.bump();
            Some((if let TokKind::Ident(n) = t.kind { n } else { unreachable!() }, t.span))
        } else {
            None
        }
    }

    fn parse_pat(&mut self) -> Result<Pat, ParseError> {
        let t = self.cur().clone();
        match &t.kind {
            TokKind::Ident(name) => {
                self.bump();
                Ok(Pat::PVar { name: name.clone(), span: t.span, nid: self.alloc() })
            }
            TokKind::Sym(w) if *w == "_" => {
                self.bump();
                Ok(Pat::PWild { span: t.span, nid: self.alloc() })
            }
            _ => Err(ParseError::new("期望变量或 '_' 模式", t.span)),
        }
    }

    pub fn parse_program(src: &str, id_start: u32) -> Result<ParseOutcome, ParseError> {
        let toks = Lexer::new(src).tokenize()?;
        let mut p = Parser::new(toks, id_start);
        let expr = p.parse_expr()?;
        if !matches!(p.cur_kind(), TokKind::Eof) {
            return Err(ParseError::new("表达式后有多余的 token", p.cur().span));
        }
        Ok(ParseOutcome { root: expr, max_id: p.max_id() })
    }

    // expr := let-expr | if-expr | fn-expr | ops
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.cur_kind() {
            TokKind::Sym(w) if *w == "let" => self.parse_let(),
            TokKind::Sym(w) if *w == "if" => self.parse_if(),
            TokKind::Sym(w) if *w == "fn" || *w == "fun" => self.parse_lam(),
            TokKind::Sym(w) if *w == "match" => {
                let m = self.parse_match()?;
                self.parse_postfix(m)
            }
            _ => self.parse_ops(),
        }
    }

    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let is_rec = self.eat_sym("rec");
        let name = self.parse_pat()?;
        self.expect_sym("=")?;
        let value = self.parse_expr()?;
        self.expect_sym("in")?;
        let body = self.parse_expr()?;
        let span = Span::new(start, body.span().end);
        Ok(Expr::Let {
            is_rec,
            name,
            value: Box::new(value),
            body: Box::new(body),
            span,
            nid: self.alloc(),
        })
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let cond = self.parse_expr()?;
        self.expect_sym("then")?;
        let then_e = self.parse_expr()?;
        self.expect_sym("else")?;
        let else_e = self.parse_expr()?;
        let span = Span::new(start, else_e.span().end);
        Ok(Expr::If {
            cond: Box::new(cond),
            then_e: Box::new(then_e),
            else_e: Box::new(else_e),
            span,
            nid: self.alloc(),
        })
    }

    fn parse_lam(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let param = self.parse_pat()?;
        self.expect_sym("->")?;
        let body = self.parse_expr()?;
        let span = Span::new(start, body.span().end);
        Ok(Expr::Lam {
            param,
            body: Box::new(body),
            span,
            nid: self.alloc(),
        })
    }

    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let scrutinee = self.parse_ops()?;
        self.expect_sym("with")?;
        let mut arms = Vec::new();
        loop {
            self.expect_sym("|")?;
            let pat = self.parse_pat()?;
            self.expect_sym("->")?;
            let body = self.parse_expr()?;
            arms.push((pat, body));
            if !self.at_sym("|") {
                break;
            }
        }
        let end = arms.last().map(|(_, e)| e.span().end).unwrap_or(start);
        Ok(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
            span: Span::new(start, end),
            nid: self.alloc(),
        })
    }

    // 二元运算符优先级：&& < == < +
    fn parse_ops(&mut self) -> Result<Expr, ParseError> {
        self.parse_and()
    }
    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_eq()?;
        while self.at_sym("&&") {
            let op = self.bump();
            let right = self.parse_eq()?;
            let span = left.span().merge(right.span());
            left = self.binop("and", op.span.start, left, right, span);
        }
        Ok(left)
    }
    fn parse_eq(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_add()?;
        while self.at_sym("==") {
            let op = self.bump();
            let right = self.parse_add()?;
            let span = left.span().merge(right.span());
            left = self.binop("eq", op.span.start, left, right, span);
        }
        Ok(left)
    }
    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_app()?;
        while self.at_sym("+") {
            let op = self.bump();
            let right = self.parse_app()?;
            let span = left.span().merge(right.span());
            left = self.binop("add", op.span.start, left, right, span);
        }
        Ok(left)
    }

    // 把内建运算符脱糖为应用： (BUILTIN x) y
    fn binop(&mut self, builtin: &str, op_start: usize, l: Expr, r: Expr, span: Span) -> Expr {
        let var = Expr::Var {
            name: builtin.to_string(),
            span: Span::new(op_start, op_start),
            nid: self.alloc(),
        };
        let lhs_span = var.span().merge(l.span());
        let app_l = Expr::App {
            fun: Box::new(var),
            arg: Box::new(l),
            span: lhs_span,
            nid: self.alloc(),
        };
        Expr::App {
            fun: Box::new(app_l),
            arg: Box::new(r),
            span,
            nid: self.alloc(),
        }
    }

    fn parse_app(&mut self) -> Result<Expr, ParseError> {
        let atom = self.parse_atom()?;
        let mut head = self.parse_postfix(atom)?;
        // 应用左结合：只要下一个 token 可以开始原子表达式，且不在边界上，就继续。
        while self.starts_atom() && !self.at_app_boundary() {
            let raw = self.parse_atom()?;
            let arg = self.parse_postfix(raw)?;
            let span = head.span().merge(arg.span());
            head = Expr::App {
                fun: Box::new(head),
                arg: Box::new(arg),
                span,
                nid: self.alloc(),
            };
        }
        Ok(head)
    }

    fn at_app_boundary(&self) -> bool {
        match self.cur_kind() {
            TokKind::Sym(w) => matches!(
                *w,
                "," | "}" | ")" | ";" | "in" | "then" | "else" | "|" | "="
            ),
            _ => false,
        }
    }

    fn starts_atom(&self) -> bool {
        match self.cur_kind() {
            TokKind::Int(_) | TokKind::Bool(_) | TokKind::Str(_) | TokKind::Ident(_) => true,
            TokKind::Sym(w) => matches!(*w, "(" | "{" | "`" | "true" | "false"),
            _ => false,
        }
    }

    // 后缀：字段访问 e.l 与管道 a |> f
    fn parse_postfix(&mut self, mut e: Expr) -> Result<Expr, ParseError> {
        loop {
            if self.eat_sym(".") {
                let (label, lspan) = self
                    .eat_ident()
                    .ok_or_else(|| ParseError::new("'.' 后期望字段名", self.cur().span))?;
                let span = e.span().merge(lspan);
                e = Expr::Field { record: Box::new(e), label, span, nid: self.alloc() };
            } else if self.at_sym("|>") {
                self.bump();
                let raw = self.parse_atom()?;
                let fun = self.parse_postfix(raw)?;
                let span = e.span().merge(fun.span());
                e = Expr::App {
                    fun: Box::new(fun),
                    arg: Box::new(e),
                    span,
                    nid: self.alloc(),
                };
            } else {
                break;
            }
        }
        Ok(e)
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let t = self.cur().clone();
        let expr = match &t.kind {
            TokKind::Int(v) => {
                self.bump();
                Expr::Int { value: *v, span: t.span, nid: self.alloc() }
            }
            TokKind::Bool(v) => {
                self.bump();
                Expr::Bool { value: *v, span: t.span, nid: self.alloc() }
            }
            TokKind::Str(s) => {
                self.bump();
                Expr::Str { value: s.clone(), span: t.span, nid: self.alloc() }
            }
            TokKind::Ident(name) => {
                self.bump();
                Expr::Var { name: name.clone(), span: t.span, nid: self.alloc() }
            }
            TokKind::Sym(w) => match *w {
                "(" => {
                    self.bump();
                    let inner = self.parse_expr()?;
                    self.expect_sym(")")?;
                    inner
                }
                "{" => self.parse_record()?,
                "`" => self.parse_variant()?,
                "true" => {
                    self.bump();
                    Expr::Bool { value: true, span: t.span, nid: self.alloc() }
                }
                "false" => {
                    self.bump();
                    Expr::Bool { value: false, span: t.span, nid: self.alloc() }
                }
                "let" => return self.parse_let(),
                "if" => return self.parse_if(),
                "fn" | "fun" => return self.parse_lam(),
                "match" => {
                    let m = self.parse_match()?;
                    return self.parse_postfix(m);
                }
                _ => return Err(ParseError::new("此处不能开始表达式", t.span)),
            },
            TokKind::Eof => return Err(ParseError::new("表达式意外结束", t.span)),
        };
        Ok(expr)
    }

    fn parse_record(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect_sym("{")?.start;
        let mut fields = Vec::new();
        if !self.at_sym("}") {
            loop {
                let label = {
                    let t = self.eat_ident();
                    t.ok_or_else(|| ParseError::new("期望字段名", self.cur().span))?.0
                };
                self.expect_sym("=")?;
                let value = self.parse_expr()?;
                fields.push((label, value));
                if !self.eat_sym(",") {
                    break;
                }
                if self.at_sym("}") {
                    break;
                }
            }
        }
        let end = self.expect_sym("}")?.end;
        Ok(Expr::Record { fields, span: Span::new(start, end), nid: self.alloc() })
    }

    // variant：`Label 或 `Label(e)
    fn parse_variant(&mut self) -> Result<Expr, ParseError> {
        let start = self.expect_sym("`")?.start;
        let (label, lspan) = {
            let t = self.eat_ident();
            t.ok_or_else(|| ParseError::new("反引号后期望变体构造子名", self.cur().span))?
        };
        let mut end = lspan.end;
        let payload = if self.at_sym("(") {
            self.bump();
            let inner = self.parse_expr()?;
            end = self.expect_sym(")")?.end;
            Some(Box::new(inner))
        } else {
            None
        };
        Ok(Expr::Variant { label, payload, span: Span::new(start, end), nid: self.alloc() })
    }
}

pub struct ParseOutcome {
    pub root: Expr,
    pub max_id: u32,
}
