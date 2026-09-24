//! 递归下降 + 优先级爬升解析器。
//!
//! 语法（见 README 示例）：
//! 程序 = 顶层 `let` 声明序列，后接一个可选的结果表达式；
//! 表达式内的 `let ... in ...` 必须显式 `in`。

use crate::ast::*;
use crate::lexer::{lex, Tok, Token};

pub fn parse(src: &str) -> Result<Program, ParseError> {
    let tokens = lex(src).map_err(|(span, message)| ParseError { span, message })?;
    let mut p = Parser { toks: tokens, pos: 0, source: src };
    let mut prog = Program { decls: Vec::new(), result: None, source: src.to_string() };
    p.skip_lines();
    let mut decl_idx = 0;
    loop {
        p.skip_lines();
        if p.peek().tok == Tok::Eof {
            break;
        }
        if let Tok::Let = p.peek().tok {
            let (rec, name, _ann, value, span) = p.parse_top_let(decl_idx)?;
            prog.decls.push(Decl {
                id: format!("d{}", decl_idx),
                rec,
                name,
                ann: None,
                value,
                span,
            });
            decl_idx += 1;
            // 允许声明后无换行直接 EOF
            p.skip_lines();
        } else {
            let e = p.parse_expr("r", &|t| {
                matches!(t, Tok::Newline | Tok::Eof)
            })?;
            prog.result = Some(e);
            p.skip_lines();
            if p.peek().tok != Tok::Eof {
                let tok = p.peek().clone();
                return Err(ParseError {
                    span: tok.span,
                    message: "结果表达式之后只允许文件结束".to_string(),
                });
            }
            break;
        }
    }
    Ok(prog)
}

struct Parser<'a> {
    toks: Vec<Token>,
    pos: usize,
    source: &'a str,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Token {
        &self.toks[self.pos]
    }
    fn peek2(&self) -> &Token {
        self.toks.get(self.pos + 1).unwrap_or(self.toks.last().unwrap())
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.pos += 1;
        }
        t
    }
    fn err(&self, span: Span, message: impl Into<String>) -> ParseError {
        ParseError { span, message: message.into() }
    }
    fn expect(&mut self, t: &Tok) -> Result<Token, ParseError> {
        if std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(t) {
            Ok(self.bump())
        } else {
            Err(self.err(self.peek().span, format!("期望 {:?}，实际为 {:?}", t, self.peek().tok)))
        }
    }
    fn skip_lines(&mut self) {
        while matches!(self.peek().tok, Tok::Newline | Tok::Semicolon) {
            self.pos += 1;
        }
    }
    fn is_uppercase(s: &str) -> bool {
        s.chars().next().map(|c| c.is_ascii_uppercase()).unwrap_or(false)
    }
    fn is_lower_ident(s: &str) -> bool {
        s.chars().next().map(|c| c.is_ascii_lowercase() || c == '_').unwrap_or(false)
    }

    fn parse_top_let(&mut self, decl_idx: usize) -> Result<(bool, String, Option<SurfaceTy>, Node, Span), ParseError> {
        let lo = self.expect(&Tok::Let)?.span.lo;
        let rec = if matches!(self.peek().tok, Tok::Rec) {
            self.bump();
            true
        } else {
            false
        };
        let name_tok = self.expect_ident_lower()?;
        self.expect(&Tok::Eq)?;
        let value = self.parse_expr(&format!("d{}/e", decl_idx), &|t| {
            matches!(t, Tok::Newline | Tok::Semicolon | Tok::Eof)
        })?;
        // 上面前缀会在外层修正
        let hi = value.span.hi;
        Ok((rec, name_tok, None, value, Span::new(lo as usize, hi as usize)))
    }

    fn expect_ident_lower(&mut self) -> Result<String, ParseError> {
        match &self.peek().tok {
            Tok::Ident(s) if Self::is_lower_ident(s) => {
                let s = s.clone();
                self.bump();
                Ok(s)
            }
            _ => Err(self.err(self.peek().span, "期望小写标识符")),
        }
    }

    /// 解析表达式。`stop` 判定哪些 token 终结当前表达式。
    fn parse_expr(
        &mut self,
        prefix: &str,
        stop: &dyn Fn(&Tok) -> bool,
    ) -> Result<Node, ParseError> {
        self.parse_fun_or_let(prefix, stop)
    }

    fn parse_fun_or_let(
        &mut self,
        prefix: &str,
        stop: &dyn Fn(&Tok) -> bool,
    ) -> Result<Node, ParseError> {
        if matches!(self.peek().tok, Tok::Fun | Tok::Backslash) {
            let lo = self.bump().span.lo;
            let param = self.expect_ident_lower()?;
            self.expect(&Tok::Arrow)?;
            let body = self.parse_expr(&format!("{}/b", prefix), stop)?;
            let span = Span::new(lo as usize, body.span.hi as usize);
            return Ok(Node::new(prefix, Expr_::Fun { param, body: Box::new(body) }, span));
        }
        if matches!(self.peek().tok, Tok::Let) {
            let lo = self.bump().span.lo;
            let rec = if matches!(self.peek().tok, Tok::Rec) {
                self.bump();
                true
            } else {
                false
            };
            let name = self.expect_ident_lower()?;
            self.expect(&Tok::Eq)?;
            let vstop = |t: &Tok| matches!(t, Tok::In);
            let value = self.parse_expr(&format!("{}/v", prefix), &vstop)?;
            self.expect(&Tok::In)?;
            let body = self.parse_expr(&format!("{}/e", prefix), stop)?;
            let span = Span::new(lo as usize, body.span.hi as usize);
            return Ok(Node::new(
                prefix,
                Expr_::Let { rec, name, ann: None, value: Box::new(value), body: Box::new(body) },
                span,
            ));
        }
        self.parse_if(prefix, stop)
    }

    fn parse_if(
        &mut self,
        prefix: &str,
        stop: &dyn Fn(&Tok) -> bool,
    ) -> Result<Node, ParseError> {
        if matches!(self.peek().tok, Tok::If) {
            let lo = self.bump().span.lo;
            let cond = self.parse_expr(&format!("{}/c", prefix), &|t| matches!(t, Tok::Then))?;
            self.expect(&Tok::Then)?;
            let then_b = self.parse_expr(&format!("{}/t", prefix), &|t| matches!(t, Tok::Else))?;
            self.expect(&Tok::Else)?;
            let else_b = self.parse_expr(&format!("{}/l", prefix), stop)?;
            let span = Span::new(lo as usize, else_b.span.hi as usize);
            return Ok(Node::new(
                prefix,
                Expr_::If {
                    cond: Box::new(cond),
                    then_b: Box::new(then_b),
                    else_b: Box::new(else_b),
                },
                span,
            ));
        }
        self.parse_match(prefix, stop)
    }

    fn parse_match(
        &mut self,
        prefix: &str,
        stop: &dyn Fn(&Tok) -> bool,
    ) -> Result<Node, ParseError> {
        if matches!(self.peek().tok, Tok::Match) {
            let lo = self.bump().span.lo;
            let scrut = self.parse_expr(&format!("{}/s", prefix), &|t| matches!(t, Tok::With))?;
            self.expect(&Tok::With)?;
            // 允许 `with | A => ... | B => ...` 或直接 `with A => ...`
            let mut arms = Vec::new();
            let mut idx = 0;
            loop {
                if !matches!(self.peek().tok, Tok::Pipe) {
                    break;
                }
                self.bump();
                let pat = self.parse_pat()?;
                self.expect(&Tok::FatArrow)?;
                let is_last_guard = |t: &Tok| {
                    stop(t) || matches!(t, Tok::Pipe)
                };
                let e = self.parse_expr(&format!("{}/a{}", prefix, idx), &is_last_guard)?;
                arms.push((pat, e));
                idx += 1;
            }
            if arms.is_empty() {
                return Err(self.err(self.peek().span, "match 后至少需要一个 `| 分支`"));
            }
            let hi = arms.last().unwrap().1.span.hi;
            return Ok(Node::new(
                prefix,
                Expr_::Match { scrut: Box::new(scrut), arms },
                Span::new(lo as usize, hi as usize),
            ));
        }
        self.parse_app(prefix, stop)
    }

    fn parse_pat(&mut self) -> Result<Pat, ParseError> {
        match &self.peek().tok.clone() {
            Tok::Wild => {
                self.bump();
                Ok(Pat::Wild)
            }
            Tok::Ident(s) if Self::is_uppercase(s) => {
                let label = s.clone();
                self.bump();
                let payload = if matches!(self.peek().tok, Tok::Ident(_)) {
                    let v = self.expect_ident_lower()?;
                    Some(v)
                } else {
                    None
                };
                Ok(Pat::PTag(label, payload))
            }
            Tok::Ident(s) if Self::is_lower_ident(s) => {
                let v = s.clone();
                self.bump();
                Ok(Pat::PVar(v))
            }
            _ => Err(self.err(self.peek().span, "期望模式：`_`、小写变量或 `Tag`/`Tag x`")),
        }
    }

    fn parse_app(
        &mut self,
        prefix: &str,
        stop: &dyn Fn(&Tok) -> bool,
    ) -> Result<Node, ParseError> {
        let mut func = self.parse_postfix(&format!("{}/f", prefix), stop)?;
        let mut arg_idx = 0;
        loop {
            if stop(&self.peek().tok) || self.starts_no_atom() {
                break;
            }
            let arg = self.parse_postfix(&format!("{}/x{}", prefix, arg_idx), stop)?;
            let span = Span::merge(func.span, arg.span);
            func = Node::new(
                prefix,
                Expr_::App { fun: Box::new(func), arg: Box::new(arg) },
                span,
            );
            arg_idx += 1;
        }
        Ok(func)
    }

    fn starts_no_atom(&self) -> bool {
        matches!(
            self.peek().tok,
            Tok::RParen
                | Tok::RBrace
                | Tok::Comma
                | Tok::Eq
                | Tok::Arrow
                | Tok::FatArrow
                | Tok::Then
                | Tok::Else
                | Tok::In
                | Tok::With
                | Tok::Pipe
                | Tok::Colon
                | Tok::Semicolon
                | Tok::Newline
                | Tok::Eof
        )
    }

    fn parse_postfix(
        &mut self,
        prefix: &str,
        stop: &dyn Fn(&Tok) -> bool,
    ) -> Result<Node, ParseError> {
        let mut node = self.parse_atom(prefix, stop)?;
        loop {
            if matches!(self.peek().tok, Tok::Dot) {
                let dot = self.bump();
                let label_tok = self.peek().clone();
                let label = match &label_tok.tok {
                    Tok::Ident(s) if Self::is_lower_ident(s) => s.clone(),
                    _ => {
                        return Err(self.err(label_tok.span, "`.` 后期望小写字段名"));
                    }
                };
                self.bump();
                let span = Span::merge(node.span, label_tok.span);
                node = Node::new(
                    prefix,
                    Expr_::Proj { record: Box::new(node), label },
                    span,
                );
                let _ = dot;
                continue;
            }
            // `{ label = expr, .. }` 后缀：对已有表达式做行扩展（多态行）
            if matches!(self.peek().tok, Tok::LBrace) {
                let brace_tok = self.bump();
                let label_tok = self.peek().clone();
                let label = match &label_tok.tok {
                    Tok::Ident(s) if Self::is_lower_ident(s) => s.clone(),
                    _ => {
                        return Err(self.err(
                            label_tok.span,
                            "行扩展 `e { field = .. }` 中需要小写字段名",
                        ));
                    }
                };
                self.bump();
                self.expect(&Tok::Eq)?;
                let value = self.parse_expr(&format!("{}/w", prefix), &|t| {
                    matches!(t, Tok::RBrace | Tok::Comma)
                })?;
                self.expect(&Tok::RBrace).map_err(|e| {
                    let _ = brace_tok;
                    e
                })?;
                let span = Span::merge(node.span, value.span);
                node = Node::new(
                    prefix,
                    Expr_::Extend { label, value: Box::new(value), rest: Box::new(node) },
                    span,
                );
                continue;
            }
            break;
        }
        Ok(node)
    }

    fn parse_atom(
        &mut self,
        prefix: &str,
        stop: &dyn Fn(&Tok) -> bool,
    ) -> Result<Node, ParseError> {
        let tok = self.peek().clone();
        let node = match &tok.tok {
            Tok::Int(n) => {
                self.bump();
                Node::new(prefix, Expr_::Int(*n), tok.span)
            }
            Tok::Bool(b) => {
                self.bump();
                Node::new(prefix, Expr_::Bool(*b), tok.span)
            }
            Tok::Str(s) => {
                self.bump();
                Node::new(prefix, Expr_::Str(s.clone()), tok.span)
            }
            Tok::LParen => {
                self.bump();
                self.skip_lines();
                let e = self.parse_expr(prefix, &|t| {
                    matches!(t, Tok::RParen) || stop(t)
                })?;
                self.skip_lines();
                self.expect(&Tok::RParen)?;
                let _ = e.span;
                e
            }
            Tok::LBrace => self.parse_record(prefix)?,
            Tok::Ident(name) => {
                if Self::is_uppercase(name) {
                    // variant 注入：`Tag` 或 `Tag e`，载荷必须带括号或原子
                    self.bump();
                    let payload = if !self.starts_no_atom() {
                        let p = self.parse_atom(&format!("{}/p", prefix), stop)?;
                        Some(Box::new(p))
                    } else {
                        None
                    };
                    let hi = payload.as_ref().map(|p| p.span.hi).unwrap_or(tok.span.hi);
                    Node::new(prefix, Expr_::Variant { label: name.clone(), payload }, Span::new(tok.span.lo as usize, hi as usize))
                } else {
                    self.bump();
                    Node::new(prefix, Expr_::Var(name.clone()), tok.span)
                }
            }
            _ => {
                return Err(self.err(tok.span, format!("此处无法开始表达式（遇到 {:?}）", tok.tok)));
            }
        };
        Ok(node)
    }

    fn parse_record(&mut self, prefix: &str) -> Result<Node, ParseError> {
        let lbrace = self.expect(&Tok::LBrace)?;
        let mut fields = Vec::new();
        self.skip_lines();
        if matches!(self.peek().tok, Tok::RBrace) {
            self.bump();
            return Ok(Node::new(prefix, Expr_::Record { fields }, lbrace.span));
        }
        loop {
            self.skip_lines();
            let label_tok = self.peek().clone();
            let label = match &label_tok.tok {
                Tok::Ident(s) if Self::is_lower_ident(s) => s.clone(),
                _ => return Err(self.err(label_tok.span, "记录字段必须是小写标识符")),
            };
            self.bump();
            self.expect(&Tok::Eq)?;
            let value =
                self.parse_expr(&format!("{}/f{}", prefix, fields.len()), &|t| {
                    matches!(t, Tok::Comma | Tok::RBrace)
                })?;
            fields.push((label, value));
            self.skip_lines();
            if matches!(self.peek().tok, Tok::Comma) {
                self.bump();
                continue;
            }
            break;
        }
        self.skip_lines();
        let rbrace = self.expect(&Tok::RBrace)?;
        Ok(Node::new(
            prefix,
            Expr_::Record { fields },
            Span::merge(lbrace.span, rbrace.span),
        ))
    }
}
