//! 递归下降解析器。语法：
//!
//! prog   := binding*
//! binding:= ["rec"] ident "=" expr
//! expr   := "fn" ident+ "->" expr
//!         | "let" ["rec"] ident "=" expr "in" expr
//!         | "if" expr "then" expr "else" expr
//!         | "match" expr "with" arm ("|" arm)*
//!         | app
//! app    := atom+
//! atom   := int | "true" | "false" | ident | "`"label atom
//!         | "{" [label "=" expr ("," ...)] [".."] "}"
//!         | atom "." label
//!         | "{" atom "with" label "=" expr ("," ...) "}"
//!         | "(" expr ")"

use crate::ast::*;
use crate::lexer::{lex, Tok, TokKind};
use crate::span::Span;

pub struct Parser<'a> {
    toks: Vec<Tok>,
    pos: usize,
    src: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Result<Self, (String, Span)> {
        Ok(Self {
            toks: lex(src)?,
            pos: 0,
            src,
        })
    }

    fn peek(&self) -> &TokKind {
        &self.toks[self.pos].kind
    }
    fn tok(&self) -> &Tok {
        &self.toks[self.pos]
    }
    fn at(&self, k: &TokKind) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(k)
    }
    fn bump(&mut self) -> Tok {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, k: TokKind) -> bool {
        if self.at(&k) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, k: TokKind, what: &str) -> Result<Tok, (String, Span)> {
        if self.at(&k) {
            Ok(self.bump())
        } else {
            Err((
                format!("期望 {}，但遇到 {}", what, self.desc()),
                self.tok().span,
            ))
        }
    }
    fn desc(&self) -> String {
        match self.peek() {
            TokKind::Eof => "文件末尾".to_string(),
            TokKind::Let => "`let`".into(),
            TokKind::Rec => "`rec`".into(),
            TokKind::In => "`in`".into(),
            TokKind::Fn => "`fn`".into(),
            TokKind::If => "`if`".into(),
            TokKind::Then => "`then`".into(),
            TokKind::Else => "`else`".into(),
            TokKind::Match => "`match`".into(),
            TokKind::With => "`with`".into(),
            TokKind::True => "`true`".into(),
            TokKind::False => "`false`".into(),
            TokKind::Wild => "`_`".into(),
            TokKind::DotDot => "`..`".into(),
            TokKind::Dot => "`.`".into(),
            TokKind::Eq => "`=`".into(),
            TokKind::Arrow => "`->`".into(),
            TokKind::Bar => "`|`".into(),
            TokKind::LParen => "`(`".into(),
            TokKind::RParen => "`)`".into(),
            TokKind::LBrace => "`{`".into(),
            TokKind::RBrace => "`}`".into(),
            TokKind::Comma => "`,`".into(),
            TokKind::Int(n) => format!("整数 {}", n),
            TokKind::Ident(s) => format!("标识符 {}", s),
            TokKind::Label(s) => format!("标签 `{}", s),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, (String, Span)> {
        let mut bindings = Vec::new();
        let mut idx = 0;
        while !self.at(&TokKind::Eof) {
            bindings.push(self.parse_binding(ROOT.child("bind", idx))?);
            idx += 1;
        }
        Ok(Program {
            nid: ROOT,
            bindings,
        })
    }

    fn parse_binding(&mut self, nid: NodeId) -> Result<Binding, (String, Span)> {
        let rec_kw = self.eat(TokKind::Rec);
        let name_t = self.expect(TokKind::Ident(String::new()), "标识符")?;
        let name = if let TokKind::Ident(s) = name_t.kind {
            s
        } else {
            unreachable!()
        };
        self.expect(TokKind::Eq, "`=`")?;
        let expr = self.parse_expr(nid.child("value", 0))?;
        Ok(Binding {
            nid,
            span: Span::new(name_t.span.start, expr.span().end),
            rec_kw,
            name,
            name_span: name_t.span,
            expr,
        })
    }

    fn parse_params(&mut self, nid: NodeId) -> Result<Vec<(String, Span, NodeId)>, (String, Span)> {
        let mut params = Vec::new();
        let mut i = 0;
        loop {
            match self.peek().clone() {
                TokKind::Ident(s) => {
                    let t = self.bump();
                    let pnid = nid.child("param", i);
                    params.push((s, t.span, pnid));
                    i += 1;
                }
                TokKind::Wild => {
                    let t = self.bump();
                    let pnid = nid.child("param", i);
                    params.push(("_".into(), t.span, pnid));
                    i += 1;
                }
                _ => break,
            }
        }
        if params.is_empty() {
            return Err((
                format!("`fn` 后至少需要一个参数，但遇到 {}", self.desc()),
                self.tok().span,
            ));
        }
        Ok(params)
    }
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self, nid: NodeId) -> Result<Expr, (String, Span)> {
        let start = self.tok().span.start;
        match self.peek().clone() {
            TokKind::Fn => {
                self.bump();
                let params = self.parse_params(nid)?;
                self.expect(TokKind::Arrow, "`->`")?;
                let body = self.parse_expr(nid.child("body", 0))?;
                Ok(Expr::Lam {
                    nid,
                    span: Span::new(start, body.span().end),
                    params,
                    body: Box::new(body),
                })
            }
            TokKind::Let => {
                self.bump();
                let rec_kw = self.eat(TokKind::Rec);
                let name_t = self.expect(TokKind::Ident(String::new()), "标识符")?;
                let name = ident_of(name_t.kind);
                self.expect(TokKind::Eq, "`=`")?;
                let bound = self.parse_expr(nid.child("bound", 0))?;
                self.expect(TokKind::In, "`in`")?;
                let body = self.parse_expr(nid.child("body", 0))?;
                Ok(Expr::Let {
                    nid,
                    span: Span::new(start, body.span().end),
                    rec_kw,
                    name,
                    name_span: name_t.span,
                    bound: Box::new(bound),
                    body: Box::new(body),
                })
            }
            TokKind::If => {
                self.bump();
                let cond = self.parse_expr(nid.child("cond", 0))?;
                self.expect(TokKind::Then, "`then`")?;
                let then_e = self.parse_expr(nid.child("then", 0))?;
                self.expect(TokKind::Else, "`else`")?;
                let else_e = self.parse_expr(nid.child("else", 0))?;
                Ok(Expr::If {
                    nid,
                    span: Span::new(start, else_e.span().end),
                    cond: Box::new(cond),
                    then_e: Box::new(then_e),
                    else_e: Box::new(else_e),
                })
            }
            TokKind::Match => {
                self.bump();
                let scrut = self.parse_expr(nid.child("scrut", 0))?;
                self.expect(TokKind::With, "`with`")?;
                self.eat(TokKind::Bar);
                let mut arms = Vec::new();
                let mut i = 0;
                loop {
                    arms.push(self.parse_arm(nid.child("arm", i))?);
                    i += 1;
                    if !self.eat(TokKind::Bar) {
                        break;
                    }
                }
                if arms.is_empty() {
                    return Err(("match 至少需要一个分支".into(), self.tok().span));
                }
                let end = arms.last().unwrap().span.end;
                Ok(Expr::Match {
                    nid,
                    span: Span::new(start, end),
                    scrutinee: Box::new(scrut),
                    arms,
                })
            }
            _ => self.parse_app(nid),
        }
    }

    fn parse_arm(&mut self, nid: NodeId) -> Result<MatchArm, (String, Span)> {
        let pattern = self.parse_pattern(nid.child("pat", 0))?;
        self.expect(TokKind::Arrow, "`->`")?;
        let body = self.parse_expr(nid.child("body", 0))?;
        Ok(MatchArm {
            nid,
            span: Span::new(pattern.span().start, body.span().end),
            pattern,
            body,
        })
    }

    fn parse_pattern(&mut self, nid: NodeId) -> Result<Pattern, (String, Span)> {
        match self.peek().clone() {
            TokKind::Wild => {
                let t = self.bump();
                Ok(Pattern::Var {
                    nid,
                    span: t.span,
                    name: "_".into(),
                })
            }
            TokKind::Ident(s) => {
                let t = self.bump();
                Ok(Pattern::Var {
                    nid,
                    span: t.span,
                    name: s,
                })
            }
            TokKind::Label(l) => {
                let t = self.bump();
                let payload = if matches!(
                    self.peek(),
                    TokKind::LParen | TokKind::Ident(_) | TokKind::Label(_)
                ) {
                    Some(Box::new(self.parse_pattern(nid.child("payload", 0))?))
                } else {
                    None
                };
                let end = payload
                    .as_ref()
                    .map(|p| p.span().end)
                    .unwrap_or(t.span.end);
                Ok(Pattern::Variant {
                    nid,
                    span: Span::new(t.span.start, end),
                    label: l,
                    label_span: Span::new(t.span.start + 1, t.span.end),
                    payload,
                })
            }
            _ => Err((
                format!("分支模式应为变量或 `标签，但遇到 {}", self.desc()),
                self.tok().span,
            )),
        }
    }
}

impl<'a> Parser<'a> {
    /// 应用：atom+，二元投影在这里后处理。
    fn parse_app(&mut self, nid: NodeId) -> Result<Expr, (String, Span)> {
        let mut head = self.parse_atom(nid.child("head", 0))?;
        let mut args: Vec<Expr> = Vec::new();
        let mut end_span = head.span();
        loop {
            if self.at(&TokKind::Dot) {
                self.bump();
                let lab = self.expect(TokKind::Label(String::new()), "字段标签（如 `.name`）")?;
                let l = label_of(lab.kind);
                let span = Span::new(head.span().start, lab.span.end);
                head = Expr::Proj {
                    nid: nid.child("proj", args.len()),
                    span,
                    record: Box::new(head),
                    label: l,
                    label_span: Span::new(lab.span.start + 1, lab.span.end),
                };
                end_span = lab.span;
                continue;
            }
            if self.starts_atom() {
                let a = self.parse_atom(nid.child("arg", args.len()))?;
                end_span = a.span();
                args.push(a);
                continue;
            }
            break;
        }
        if args.is_empty() {
            Ok(head)
        } else {
            Ok(Expr::App {
                nid,
                span: Span::new(head.span().start, end_span.end),
                fun: Box::new(head),
                args,
            })
        }
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek(),
            TokKind::Int(_)
                | TokKind::True
                | TokKind::False
                | TokKind::Ident(_)
                | TokKind::Label(_)
                | TokKind::LParen
                | TokKind::LBrace
        )
    }

    fn parse_atom(&mut self, nid: NodeId) -> Result<Expr, (String, Span)> {
        let start = self.tok().span.start;
        match self.peek().clone() {
            TokKind::Int(v) => {
                let t = self.bump();
                Ok(Expr::Lit {
                    nid,
                    span: t.span,
                    lit: Literal::Int,
                    value: v.to_string(),
                })
            }
            TokKind::True | TokKind::False => {
                let t = self.bump();
                Ok(Expr::Lit {
                    nid,
                    span: t.span,
                    lit: Literal::Bool,
                    value: if matches!(t.kind, TokKind::True) {
                        "true".into()
                    } else {
                        "false".into()
                    },
                })
            }
            TokKind::Ident(_) => {
                let t = self.bump();
                Ok(Expr::Var {
                    nid,
                    span: t.span,
                    name: ident_of(t.kind),
                })
            }
            TokKind::Label(_) => {
                // `tag payload
                let lab = self.bump();
                let payload = self.parse_atom(nid.child("payload", 0))?;
                Ok(Expr::Inject {
                    nid,
                    span: Span::new(start, payload.span().end),
                    label: label_of(lab.kind),
                    label_span: Span::new(lab.span.start + 1, lab.span.end),
                    payload: Box::new(payload),
                })
            }
            TokKind::LParen => {
                self.bump();
                let inner = self.parse_expr(nid.child("inner", 0))?;
                let close = self.expect(TokKind::RParen, "`)`")?;
                Ok(Expr::Paren {
                    nid,
                    span: Span::new(start, close.span.end),
                    inner: Box::new(inner),
                })
            }
            TokKind::LBrace => self.parse_brace(nid, start),
            other => Err((
                format!("表达式不能以 {} 开始", tok_desc(&other)),
                self.tok().span,
            )),
        }
    }

    /// `{ fields.. }`（记录字面量，可选尾 `..` 开放）或
    /// `{ expr with fields.. }`（记录更新）。
    fn parse_brace(&mut self, nid: NodeId, start: usize) -> Result<Expr, (String, Span)> {
        self.expect(TokKind::LBrace, "`{`")?;
        // 判定更新：先试解析一个 label = ... 还是 expr with
        if let TokKind::Label(first) = self.peek().clone() {
            // 也可能是 `{ `a with ... }`（更新），需要再看两个 token
            let save = self.pos;
            self.bump(); // label
            if self.at(&TokKind::Eq) {
                self.pos = save;
                return self.parse_record_lit(nid, start);
            }
            // label 后面不是 '='：可能是注入表达式作 with 的主体
            self.pos = save;
        }
        if !self.at(&TokKind::RBrace) && !self.at(&TokKind::DotDot) {
            // 尝试解析主体，再期待 with
            let save = self.pos;
            match self.parse_expr(nid.child("record", 0)) {
                Ok(record) if self.at(&TokKind::With) => {
                    self.bump();
                    let mut fields = Vec::new();
                    fields.push(self.parse_field(nid.child("field", 0))?);
                    while self.eat(TokKind::Comma) {
                        if self.at(&TokKind::RBrace) {
                            break;
                        }
                        fields.push(self.parse_field(nid.child("field", fields.len()))?);
                    }
                    let close = self.expect(TokKind::RBrace, "`}`")?;
                    return Ok(Expr::Update {
                        nid,
                        span: Span::new(start, close.span.end),
                        record: Box::new(record),
                        fields,
                    });
                }
                _ => self.pos = save,
            }
        }
        self.parse_record_lit(nid, start)
    }

    fn parse_record_lit(&mut self, nid: NodeId, start: usize) -> Result<Expr, (String, Span)> {
        let mut fields = Vec::new();
        let mut open = false;
        if !self.at(&TokKind::RBrace) && !self.at(&TokKind::DotDot) {
            fields.push(self.parse_field(nid.child("field", 0))?);
            while self.eat(TokKind::Comma) {
                if self.at(&TokKind::RBrace) {
                    break;
                }
                if self.at(&TokKind::DotDot) {
                    break;
                }
                fields.push(self.parse_field(nid.child("field", fields.len()))?);
            }
        }
        if self.eat(TokKind::DotDot) {
            open = true;
        }
        let close = self.expect(TokKind::RBrace, "`}`")?;
        // 重复字段提前在解析层报错，带来源范围
        for i in 0..fields.len() {
            for j in (i + 1)..fields.len() {
                if fields[i].0 == fields[j].0 {
                    return Err((
                        format!("记录字段 `{}` 重复出现", fields[i].0),
                        fields[j].1,
                    ));
                }
            }
        }
        Ok(Expr::Record {
            nid,
            span: Span::new(start, close.span.end),
            fields,
            open,
        })
    }

    fn parse_field(
        &mut self,
        nid: NodeId,
    ) -> Result<(String, Span, Expr), (String, Span)> {
        let lab = self.expect(TokKind::Label(String::new()), "字段标签（如 `.name`）")?;
        let name = label_of(lab.kind);
        let label_span = Span::new(lab.span.start + 1, lab.span.end);
        self.expect(TokKind::Eq, "字段后的 `=`")?;
        let value = self.parse_expr(nid.child("value", 0))?;
        Ok((name, label_span, value))
    }
}

fn ident_of(k: TokKind) -> String {
    if let TokKind::Ident(s) = k {
        s
    } else {
        unreachable!()
    }
}
fn label_of(k: TokKind) -> String {
    if let TokKind::Label(s) = k {
        s
    } else {
        unreachable!()
    }
}
fn tok_desc(k: &TokKind) -> String {
    match k {
        TokKind::Eof => "文件末尾".to_string(),
        TokKind::Let => "`let`".into(),
        TokKind::Rec => "`rec`".into(),
        TokKind::In => "`in`".into(),
        TokKind::Fn => "`fn`".into(),
        TokKind::If => "`if`".into(),
        TokKind::Then => "`then`".into(),
        TokKind::Else => "`else`".into(),
        TokKind::Match => "`match`".into(),
        TokKind::With => "`with`".into(),
        TokKind::True => "`true`".into(),
        TokKind::False => "`false`".into(),
        TokKind::Wild => "`_`".into(),
        TokKind::DotDot => "`..`".into(),
        TokKind::Dot => "`.`".into(),
        TokKind::Eq => "`=`".into(),
        TokKind::Arrow => "`->`".into(),
        TokKind::Bar => "`|`".into(),
        TokKind::LParen => "`(`".into(),
        TokKind::RParen => "`)`".into(),
        TokKind::LBrace => "`{`".into(),
        TokKind::RBrace => "`}`".into(),
        TokKind::Comma => "`,`".into(),
        TokKind::Int(n) => format!("整数 {}", n),
        TokKind::Ident(s) => format!("标识符 {}", s),
        TokKind::Label(s) => format!("标签 `{}", s),
    }
}

pub fn parse(src: &str) -> Result<Program, (String, Span)> {
    Parser::new(src)?.parse_program()
}
