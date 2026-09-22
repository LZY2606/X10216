//! 词法分析、语法分析与 AST 定义。每个节点都带源码区间 [`Span`] 与稳定路径 [`NodeId`]。

use serde::Serialize;

/// 半开字节区间 [start, end)。
#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn merge(self, other: Span) -> Span {
        Span::new(self.start.min(other.start), self.end.max(other.end))
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// 解析错误，带源码范围而不是只给行号。
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl ParseError {
    fn at(span: Span, message: impl Into<String>) -> Self {
        ParseError {
            span,
            message: message.into(),
        }
    }
}

/// 表达式种类。
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum ExprK {
    Unit,
    Int(i64),
    Bool(bool),
    Str(String),
    Var(String),
    Lam { param: String, body: Expr },
    App { fun: Expr, arg: Expr },
    Let {
        name: String,
        rec: bool,
        bound: Expr,
        body: Expr,
    },
    If { cond: Expr, then_b: Expr, else_b: Expr },
    Record(Vec<(String, Expr)>),
    Field { record: Expr, label: String },
    Tag { label: String, payload: Option<Expr> },
    Match { scrut: Expr, arms: Vec<MatchArm> },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct MatchArm {
    pub pattern: Pat,
    pub body: Expr,
}

/// variant 匹配模式：`Tag(x)` 或 `Tag`，外加通配 `_`。
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum Pat {
    Wild(Span),
    Tag {
        span: Span,
        label: String,
        binding: Option<String>,
    },
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::Wild(s) => *s,
            Pat::Tag { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Expr {
    #[serde(flatten)]
    pub k: ExprK,
    pub span: Span,
    /// 稳定路径，例如 `l0.bound/app.arg`；编辑后只要结构位置相同就一致。
    pub path: String,
}

impl Expr {
    fn mk(k: ExprK, span: Span, path: String) -> Self {
        Expr { k, span, path }
    }
}

/// 顶层条目：`let` / `let rec` 绑定，或一个独立表达式。
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind")]
pub enum Item {
    Let {
        span: Span,
        path: String,
        name: String,
        rec: bool,
        bound: Expr,
    },
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Program {
    pub items: Vec<Item>,
    pub source: String,
}

// ---------------------------------------------------------------------------
// 词法
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
enum TokK {
    Ident(String),
    Int(i64),
    Str(String),
    // 标点 / 关键字保留原始拼写
    Punct(String),
}

#[derive(Clone, Debug)]
struct Tok {
    k: TokK,
    span: Span,
}

fn lex(src: &str) -> Result<Vec<Tok>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut toks = Vec::new();
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        // 行注释 --
        if c == '-' && b.get(i + 1) == Some(&b'-') {
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        let start = i;
        if c.is_ascii_digit() {
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[start..i].parse().map_err(|_| {
                ParseError::at(Span::new(start, i), "整数字面量超出范围")
            })?;
            toks.push(Tok {
                k: TokK::Int(n),
                span: Span::new(start, i),
            });
            continue;
        }
        if c == '_' || c.is_ascii_alphabetic() {
            while i < b.len()
                && ((b[i] as char).is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'\'')
            {
                i += 1;
            }
            toks.push(Tok {
                k: TokK::Ident(src[start..i].to_string()),
                span: Span::new(start, i),
            });
            continue;
        }
        if c == '"' {
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                let ch = b[i] as char;
                if ch == '\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    match e {
                        'n' => s.push('\n'),
                        't' => s.push('\t'),
                        '\\' => s.push('\\'),
                        '"' => s.push('"'),
                        other => s.push(other),
                    }
                    i += 2;
                } else {
                    s.push(ch);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(ParseError::at(
                    Span::new(start, i),
                    "字符串字面量未闭合",
                ));
            }
            i += 1; // 结束引号
            toks.push(Tok {
                k: TokK::Str(s),
                span: Span::new(start, i),
            });
            continue;
        }
        // 多字符标点
        let two: String = src[i..].chars().take(2).collect();
        if two.len() >= 2 && matches!(two.as_str(), "->" | ";;" | "==" | "<>" | ">=" | "<=") {
            i += two.len();
            toks.push(Tok {
                k: TokK::Punct(two),
                span: Span::new(start, i),
            });
            continue;
        }
        if "(){}=.;,:+-><*|".contains(c) {
            i += 1;
            toks.push(Tok {
                k: TokK::Punct(c.to_string()),
                span: Span::new(start, i),
            });
            continue;
        }
        return Err(ParseError::at(
            Span::new(start, start + 1),
            format!("无法识别的字符 {:?}", c),
        ));
    }
    Ok(toks)
}

// ---------------------------------------------------------------------------
// 语法
// ---------------------------------------------------------------------------

struct Parser<'a> {
    toks: Vec<Tok>,
    pos: usize,
    src: &'a str,
}

/// 解析入口。
pub fn parse(src: &str) -> Result<Program, ParseError> {
    let toks = lex(src)?;
    let mut p = Parser {
        toks,
        pos: 0,
        src,
    };
    let mut items = Vec::new();
    let mut idx = 0;
    while !p.at_end() {
        items.push(p.parse_item(&format!("l{}", idx))?);
        idx += 1;
        while p.eat_punct(";;") {}
    }
    Ok(Program {
        items,
        source: src.to_string(),
    })
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Option<&Tok> {
        self.toks.get(self.pos)
    }
    fn at_end(&self) -> bool {
        self.pos >= self.toks.len()
    }
    fn peek_ident(&self) -> Option<&str> {
        match &self.peek()?.k {
            TokK::Ident(s) => Some(s),
            _ => None,
        }
    }
    fn is_punct(&self, s: &str) -> bool {
        matches!(&self.peek().map(|t| &t.k), Some(TokK::Punct(p)) if p == s)
    }
    fn eat_punct(&mut self, s: &str) -> bool {
        if self.is_punct(s) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn eat_kw(&mut self, kw: &str) -> bool {
        if self.peek_ident() == Some(kw) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn last_span(&self) -> Span {
        self.toks
            .last()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::new(self.src.len(), self.src.len()))
    }
    fn expect_punct(&mut self, s: &str) -> Result<Span, ParseError> {
        if self.eat_punct(s) {
            Ok(self.toks[self.pos - 1].span)
        } else {
            let span = self.peek().map(|t| t.span).unwrap_or_else(|| self.last_span());
            Err(ParseError::at(
                span,
                format!("期望 {:?}", s),
            ))
        }
    }
    fn expect_ident(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().map(|t| (&t.k, t.span)) {
            Some((TokK::Ident(s), sp)) => {
                let name = s.clone();
                self.pos += 1;
                Ok((name, sp))
            }
            _ => {
                let span = self.peek().map(|t| t.span).unwrap_or_else(|| self.last_span());
                Err(ParseError::at(span, "期望标识符"))
            }
        }
    }

    fn parse_item(&mut self, path: &str) -> Result<Item, ParseError> {
        let start = self.peek().map(|t| t.span.start).unwrap_or(self.src.len());
        if self.peek_ident() == Some("let") {
            self.pos += 1;
            let rec = self.eat_kw("rec");
            let (name, _) = self.expect_ident()?;
            self.expect_punct("=")?;
            let bound = self.parse_expr(&format!("{}/bound", path), 0)?;
            let end = bound.span.end;
            while self.eat_punct(";;") {}
            Ok(Item::Let {
                span: Span::new(start, end),
                path: path.to_string(),
                name,
                rec,
                bound,
            })
        } else {
            let e = self.parse_expr(path, 0)?;
            Ok(Item::Expr(e))
        }
    }

    fn binop(op: &str) -> Option<(&'static str, u8)> {
        Some(match op {
            "|" => ("__bor", 1),
            "&" => ("__band", 2),
            "==" | "<>" | "<" | ">" | "<=" | ">=" => ("__eq", 3),
            "+" | "-" => ("__add", 4),
            "*" => ("__mul", 5),
            _ => return None,
        })
    }

    fn parse_expr(&mut self, path: &str, min_prec: u8) -> Result<Expr, ParseError> {
        let first = self.parse_unary(&format!("{}/b0", path))?;
        self.parse_expr_tail(path, min_prec, first, 1)
    }

    fn parse_expr_tail(
        &mut self,
        path: &str,
        min_prec: u8,
        mut left: Expr,
        mut seq: usize,
    ) -> Result<Expr, ParseError> {
        loop {
            let op = match &self.peek().map(|t| &t.k) {
                Some(TokK::Punct(p)) => p.clone(),
                _ => break,
            };
            let Some((builtin, prec)) = Self::binop(&op) else {
                break;
            };
            if prec < min_prec {
                break;
            }
            let op_span = self.peek().unwrap().span;
            self.pos += 1;
            let right =
                self.parse_expr(&format!("{}/b{}", path, seq + 1), prec + 1)?;
            let span = left.span.merge(right.span);
            let fvar = Expr::mk(
                ExprK::Var(builtin.to_string()),
                op_span,
                format!("{}/op{}", path, seq),
            );
            let app1 = Expr::mk(
                ExprK::App {
                    fun: fvar,
                    arg: left,
                },
                span,
                format!("{}/la{}", path, seq),
            );
            left = Expr::mk(
                ExprK::App {
                    fun: app1,
                    arg: right,
                },
                span,
                format!("{}/bin{}", path, seq),
            );
            seq += 1;
        }
        Ok(left)
    }

    fn parse_unary(&mut self, path: &str) -> Result<Expr, ParseError> {
        if self.is_punct("-") {
            let sp = self.peek().unwrap().span;
            self.pos += 1;
            let e = self.parse_unary(&format!("{}/neg", path))?;
            let zero = Expr::mk(ExprK::Int(0), sp, format!("{}/negz", path));
            let span = sp.merge(e.span);
            let sub = Expr::mk(ExprK::Var("__add".into()), sp, format!("{}/negop", path));
            return Ok(Expr::mk(
                ExprK::App {
                    fun: Expr::mk(
                        ExprK::App { fun: sub, arg: zero },
                        span,
                        format!("{}/negapp1", path),
                    ),
                    arg: e,
                },
                span,
                path.to_string(),
            ));
        }
        self.parse_app(path)
    }

    fn parse_app(&mut self, path: &str) -> Result<Expr, ParseError> {
        let mut fun = self.parse_atom(&format!("{}/head", path))?;
        let mut n = 0;
        // 字段访问 e.label
        loop {
            if self.eat_punct(".") {
                let (label, lspan) = self.expect_ident()?;
                let span = fun.span.merge(lspan);
                fun = Expr::mk(
                    ExprK::Field {
                        record: fun,
                        label,
                    },
                    span,
                    format!("{}/fld{}", path, n),
                );
                n += 1;
                continue;
            }
            // 应用：atom 紧跟一个可作为实参开头的 token
            if self.can_start_atom() {
                let arg = self.parse_atom(&format!("{}/arg{}", path, n))?;
                let span = fun.span.merge(arg.span);
                fun = Expr::mk(ExprK::App { fun, arg }, span, format!("{}/app{}", path, n));
                n += 1;
                continue;
            }
            break;
        }
        Ok(fun)
    }

    fn can_start_atom(&self) -> bool {
        match self.peek().map(|t| &t.k) {
            Some(TokK::Int(_)) | Some(TokK::Str(_)) | Some(TokK::Punct(p))
                if p == "(" || p == "{" =>
            {
                true
            }
            Some(TokK::Ident(s)) => {
                !matches!(s.as_str(), "then" | "else" | "in" | "with" | "rec")
            }
            _ => false,
        }
    }

    fn parse_atom(&mut self, path: &str) -> Result<Expr, ParseError> {
        let tok = match self.peek() {
            Some(t) => t.clone(),
            None => {
                return Err(ParseError::at(self.last_span(), "意外的程序结尾"));
            }
        };
        match &tok.k {
            TokK::Int(n) => {
                self.pos += 1;
                Ok(Expr::mk(ExprK::Int(*n), tok.span, path.to_string()))
            }
            TokK::Str(s) => {
                self.pos += 1;
                Ok(Expr::mk(ExprK::Str(s.clone()), tok.span, path.to_string()))
            }
            TokK::Punct(p) if p == "(" => {
                self.pos += 1;
                if self.eat_punct(")") {
                    return Ok(Expr::mk(ExprK::Unit, tok.span, path.to_string()));
                }
                let inner = self.parse_expr(path, 0)?;
                self.expect_punct(")")?;
                Ok(inner)
            }
            TokK::Punct(p) if p == "{" => {
                self.pos += 1;
                let mut fields = Vec::new();
                if !self.is_punct("}") {
                    loop {
                        let (label, _) = self.expect_ident()?;
                        self.expect_punct("=")?;
                        let value =
                            self.parse_expr(&format!("{}/{}", path, label.replace('\'', "p")), 0)?;
                        fields.push((label, value));
                        if self.eat_punct(",") {
                            if self.is_punct("}") {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                let end = self.expect_punct("}")?;
                Ok(Expr::mk(
                    ExprK::Record(fields),
                    tok.span.merge(end),
                    path.to_string(),
                ))
            }
            TokK::Ident(kw) => match kw.as_str() {
                "true" | "false" => {
                    self.pos += 1;
                    Ok(Expr::mk(
                        ExprK::Bool(kw == "true"),
                        tok.span,
                        path.to_string(),
                    ))
                }
                "fun" => self.parse_lam(path, tok.span),
                "let" => self.parse_let(path, tok.span),
                "if" => self.parse_if(path, tok.span),
                "match" => self.parse_match(path, tok.span),
                "`" | _ => self.parse_var_or_tag(path, tok),
            },
            _ => Err(ParseError::at(tok.span, "此处不能开始一个表达式")),
        }
    }

    fn parse_var_or_tag(&mut self, path: &str, tok: Tok) -> Result<Expr, ParseError> {
        let TokK::Ident(name) = &tok.k else { unreachable!() };
        // variant 标签：首字母大写，可后跟 payload；unit payload 写作 Tag()
        let is_tag = name
            .chars()
            .next()
            .map(|c| c.is_ascii_uppercase())
            .unwrap_or(false);
        if is_tag && self.is_punct("(") {
            let paren = self.peek().unwrap().span;
            self.pos += 1;
            if self.eat_punct(")") {
                return Ok(Expr::mk(
                    ExprK::Tag {
                        label: name.clone(),
                        payload: None,
                    },
                    tok.span.merge(paren),
                    path.to_string(),
                ));
            }
            let payload = self.parse_expr(&format!("{}/payload", path), 0)?;
            let end = self.expect_punct(")")?;
            return Ok(Expr::mk(
                ExprK::Tag {
                    label: name.clone(),
                    payload: Some(payload),
                },
                tok.span.merge(end),
                path.to_string(),
            ));
        }
        self.pos += 1;
        Ok(Expr::mk(
            if is_tag {
                ExprK::Tag {
                    label: name.clone(),
                    payload: None,
                }
            } else {
                ExprK::Var(name.clone())
            },
            tok.span,
            path.to_string(),
        ))
    }

    fn parse_lam(&mut self, path: &str, start: Span) -> Result<Expr, ParseError> {
        // fun x -> e   （仅单个参数；柯里化用嵌套 fun）
        self.expect_kw_any(); // 消费 'fun'
        let (param, _) = self.expect_ident()?;
        self.expect_punct("->")?;
        let body = self.parse_expr(&format!("{}/body", path), 0)?;
        let span = start.merge(body.span);
        Ok(Expr::mk(
            ExprK::Lam { param, body },
            span,
            path.to_string(),
        ))
    }

    fn expect_kw_any(&mut self) {
        self.pos += 1;
    }

    fn parse_let(&mut self, path: &str, start: Span) -> Result<Expr, ParseError> {
        self.pos += 1; // let
        let rec = self.eat_kw("rec");
        let (name, _) = self.expect_ident()?;
        self.expect_punct("=")?;
        let bound = self.parse_expr(&format!("{}/bound", path), 0)?;
        self.expect_punct("in")?;
        let body = self.parse_expr(&format!("{}/body", path), 0)?;
        let span = start.merge(body.span);
        Ok(Expr::mk(
            ExprK::Let {
                name,
                rec,
                bound,
                body,
            },
            span,
            path.to_string(),
        ))
    }

    fn parse_if(&mut self, path: &str, start: Span) -> Result<Expr, ParseError> {
        self.pos += 1;
        let cond = self.parse_expr(&format!("{}/cond", path), 0)?;
        self.expect_punct("then")?;
        let then_b = self.parse_expr(&format!("{}/then", path), 0)?;
        self.expect_punct("else")?;
        let else_b = self.parse_expr(&format!("{}/else", path), 0)?;
        let span = start.merge(else_b.span);
        Ok(Expr::mk(
            ExprK::If {
                cond,
                then_b,
                else_b,
            },
            span,
            path.to_string(),
        ))
    }

    fn parse_match(&mut self, path: &str, start: Span) -> Result<Expr, ParseError> {
        self.pos += 1;
        let scrut = self.parse_expr(&format!("{}/scrut", path), 0)?;
        self.expect_punct("with")?;
        let mut arms = Vec::new();
        let mut n = 0;
        loop {
            self.expect_punct("|")?;
            let pspan = self.peek().map(|t| t.span).unwrap_or(self.last_span());
            let pattern = if self.eat_punct("_") {
                Pat::Wild(pspan)
            } else {
                let (label, lspan) = self.expect_ident()?;
                let binding = if self.is_punct("(") {
                    self.pos += 1;
                    let (x, _) = self.expect_ident()?;
                    self.expect_punct(")")?;
                    Some(x)
                } else {
                    None
                };
                Pat::Tag {
                    span: pspan.merge(lspan),
                    label,
                    binding,
                }
            };
            self.expect_punct("->")?;
            let body = self.parse_expr(&format!("{}/arm{}", path, n), 0)?;
            arms.push(MatchArm { pattern, body });
            n += 1;
            if self.is_punct("|") {
                continue;
            }
            break;
        }
        let end = arms.last().map(|a| a.body.span).unwrap_or(scrut.span);
        Ok(Expr::mk(
            ExprK::Match { scrut, arms },
            start.merge(end),
            path.to_string(),
        ))
    }
}
