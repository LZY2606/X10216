//! 递归下降解析器。
//!
//! 语法：
//!   program := let* body
//!   let     := "let" "rec"? ident (":" ty)? "=" expr ";"
//!   expr    := let-expr | if-expr | lam | match-expr | ann
//!   ann     := arrow (":" ty)?
//!   arrow   := fn ("->" arrow)?
//!   fn      := atom+                  // 左结合应用
//!   postfix := atom ("." ident | "{" fields "}" 作为扩展在 atom 内处理)
//!   atom    := int | bool-lit | "()" | string | ident
//!            | "(" expr ")" | "{" record "}" | tag-expr
//!   record  := (ident "=" expr ("," ...)*)?
//!   tag     := "#" ident (atom)?
//!   match   := "match" expr "with" clause+
//!   clause  := pat "->" expr ";"

use crate::ast::*;
use crate::lexer::{lex, Tok, TokKind};
use crate::span::{Gid, Span};

pub struct Parser {
    toks: Vec<Tok>,
    pos: usize,
    next_gid: Gid,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

type PResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(src: &str) -> Result<Self, ParseError> {
        let toks = lex(src).map_err(|e| ParseError { span: e.span, msg: e.msg })?;
        Ok(Parser { toks, pos: 0, next_gid: 1 })
    }

    fn gid(&mut self) -> Gid {
        let g = self.next_gid;
        self.next_gid += 1;
        g
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos]
    }
    fn peek_kind(&self) -> &TokKind {
        &self.peek().kind
    }
    fn span(&self) -> Span {
        self.peek().span
    }
    fn bump(&mut self) -> Tok {
        let t = self.toks[self.pos].clone();
        if !matches!(t.kind, TokKind::Eof) {
            self.pos += 1;
        }
        t
    }
    fn eat_op(&mut self, op: &str) -> bool {
        if let TokKind::Op(o) = self.peek_kind() {
            if o == op {
                self.bump();
                return true;
            }
        }
        false
    }
    fn expect_op(&mut self, op: &str) -> PResult<Span> {
        let sp = self.span();
        if self.eat_op(op) {
            Ok(sp)
        } else {
            Err(ParseError { span: sp, msg: format!("期望 '{}'", op) })
        }
    }
    fn eat_kw(&mut self, kw: &str) -> bool {
        if let TokKind::Str(s) = self.peek_kind() {
            if s == kw {
                self.bump();
                return true;
            }
        }
        false
    }
    fn expect_ident(&mut self) -> PResult<(String, Span)> {
        let sp = self.span();
        match self.peek_kind().clone() {
            TokKind::Str(s) if !is_keyword(&s) => {
                self.bump();
                Ok((s, sp))
            }
            _ => Err(ParseError { span: sp, msg: "期望标识符".into() }),
        }
    }

    pub fn parse_program(&mut self) -> PResult<Program> {
        let mut items = Vec::new();
        loop {
            if let TokKind::Str(s) = self.peek_kind() {
                if s == "let" {
                    items.push(self.parse_let_item()?);
                    continue;
                }
            }
            break;
        }
        let body = self.parse_expr()?;
        if !matches!(self.peek_kind(), TokKind::Eof) {
            return Err(ParseError { span: self.span(), msg: "程序尾部有多余 token".into() });
        }
        let mut root = RawNode {
            gid: self.gid(),
            label: "program".into(),
            text: None,
            span: body.span,
            children: Vec::new(),
        };
        for it in &items {
            root.children.push(self.raw_for_item(it));
        }
        root.children.push(self.raw_for_node(&body));
        let root_span = root.children.iter().map(|c| c.span).fold(body.span, |a, b| a.join(b));
        root.span = root_span;
        Ok(Program { items, body, root_gid: root.gid, raw: root })
    }

    fn parse_let_item(&mut self) -> PResult<LetItem> {
        let start = self.expect_kw("let").then_some(self.toks[self.pos.saturating_sub(1)].span);
        let start = start.unwrap_or_else(|| self.span());
        let rec = self.eat_kw("rec");
        let (name, _) = self.expect_ident()?;
        let ann = if self.eat_op(":") {
            let sp0 = self.span();
            let a = self.parse_ty()?;
            Some((a, self.gid_at(sp0), sp0.join(self.prev_span())))
        } else {
            None
        };
        self.expect_op("=")?;
        let value = self.parse_expr()?;
        let semi = self.eat_op(";");
        let end = if semi { self.prev_span().hi } else { value.span.hi };
        Ok(LetItem {
            rec,
            name,
            ann,
            value,
            span: Span::new(start.lo, end),
            gid: self.gid(),
        })
    }

    fn prev_span(&self) -> Span {
        self.toks[self.pos.saturating_sub(1)].span
    }
    fn gid_at(&mut self, _sp: Span) -> Gid {
        self.gid()
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek_kind(), TokKind::Eof)
    }

    pub fn parse_expr(&mut self) -> PResult<Node> {
        // let 表达式
        if let TokKind::Str(s) = self.peek_kind() {
            if s == "let" {
                return self.parse_let_expr();
            }
            if s == "if" {
                return self.parse_if();
            }
            if s == "fun" {
                return self.parse_lam();
            }
            if s == "match" {
                return self.parse_match();
            }
        }
        let mut node = self.parse_ann()?;
        // 可选类型标注
        if self.eat_op(":") {
            let ty = self.parse_ty()?;
            let span = node.span.join(self.prev_span());
            let expr = Expr::Ann(Box::new(node), ty);
            node = Node { expr, gid: self.gid(), span };
        }
        Ok(node)
    }

    fn parse_let_expr(&mut self) -> PResult<Node> {
        let start = self.expect_kw("let").then_some(self.toks[self.pos.saturating_sub(1)].span);
        let start = start.unwrap_or_else(|| self.span());
        let rec = self.eat_kw("rec");
        let (name, _) = self.expect_ident()?;
        let ann = if self.eat_op(":") {
            Some(self.parse_ty()?)
        } else {
            None
        };
        self.expect_op("=")?;
        let value = self.parse_expr()?;
        // let 表达式既允许 "in" 也允许 ";"
        if !(self.eat_kw("in") || self.eat_op(";")) {
            return Err(ParseError { span: self.span(), msg: "期望 'in' 或 ';'".into() });
        }
        let rest = self.parse_expr()?;
        let span = Span::new(start.lo, rest.span.hi);
        Ok(Node {
            expr: Expr::Let { rec, name, ann, value: Box::new(value), rest: Box::new(rest) },
            gid: self.gid(),
            span,
        })
    }

    fn parse_if(&mut self) -> PResult<Node> {
        let start = self.peek().span;
        self.eat_kw("if");
        let c = self.parse_expr()?;
        if !self.eat_kw("then") {
            return Err(ParseError { span: self.span(), msg: "期望 'then'".into() });
        }
        let t = self.parse_expr()?;
        if !self.eat_kw("else") {
            return Err(ParseError { span: self.span(), msg: "期望 'else'".into() });
        }
        let e = self.parse_expr()?;
        let span = Span::new(start.lo, e.span.hi);
        Ok(Node { expr: Expr::If(Box::new(c), Box::new(t), Box::new(e)), gid: self.gid(), span })
    }

    fn parse_lam(&mut self) -> PResult<Node> {
        let start = self.peek().span;
        self.eat_kw("fun");
        let (param, _) = self.expect_ident()?;
        let ann = if self.eat_op(":") { Some(self.parse_ty()?) } else { None };
        if !self.eat_op("->") {
            return Err(ParseError { span: self.span(), msg: "期望 '->'".into() });
        }
        let body = self.parse_expr()?;
        let span = Span::new(start.lo, body.span.hi);
        Ok(Node { expr: Expr::Lam { param, ann, body: Box::new(body) }, gid: self.gid(), span })
    }

    fn parse_match(&mut self) -> PResult<Node> {
        let start = self.peek().span;
        self.eat_kw("match");
        let scrut = self.parse_expr()?;
        if !self.eat_kw("with") {
            return Err(ParseError { span: self.span(), msg: "期望 'with'".into() });
        }
        let mut arms = Vec::new();
        loop {
            let pat = self.parse_pat()?;
            if !self.eat_op("->") {
                return Err(ParseError { span: self.span(), msg: "期望 '->'".into() });
            }
            let body = self.parse_expr()?;
            arms.push((pat, body));
            if !self.eat_op(";") {
                break;
            }
            // 继续看下一条分支
            match self.peek_kind() {
                TokKind::Op(o) if o == "|" || o == "_" => {}
                TokKind::Str(s) if s == "#" => {}
                _ => break,
            }
        }
        if arms.is_empty() {
            return Err(ParseError { span: start, msg: "match 至少需要一条分支".into() });
        }
        let end = arms.last().unwrap().1.span;
        let span = Span::new(start.lo, end.hi);
        Ok(Node { expr: Expr::Match(Box::new(scrut), arms), gid: self.gid(), span })
    }

    fn parse_pat(&mut self) -> PResult<Pat> {
        if self.eat_op("#") {
            let (tag, _) = self.expect_ident()?;
            // 可选绑定：#Some x
            let bind = if let TokKind::Str(s) = self.peek_kind() {
                if !is_keyword(s) {
                    Some(self.expect_ident()?.0)
                } else {
                    None
                }
            } else {
                None
            };
            Ok(Pat::Tag(tag, bind))
        } else if self.eat_op("_") {
            Ok(Pat::Wild)
        } else {
            Err(ParseError { span: self.span(), msg: "期望模式 (#Tag 或 _)".into() })
        }
    }
