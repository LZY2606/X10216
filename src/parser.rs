//! 词法分析与递归下降解析。
use crate::ast::*;

#[derive(Clone, Debug, PartialEq, Eq)]
enum TokKind {
    Int(i64),
    Ident(String),
    LParen, RParen,
    LBrace, RBrace,
    Dot, Comma, Eq, Arrow, FatArrow, Semi,
    KwLet, KwRec, KwIn, KwIf, KwThen, KwElse, KwTrue, KwFalse,
    KwFun, KwMatch, KwWith, KwUnit,
    Plus, Minus, Star, EqEq,
    Backtick, Pipe,
    Eof,
}

#[derive(Clone, Debug)]
struct Tok { kind: TokKind, span: Span }

struct Lexer<'a> { src: &'a [u8], pos: usize, toks: Vec<Tok>, errors: Vec<ParseError> }

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Self { Lexer { src: src.as_bytes(), pos: 0, toks: vec![], errors: vec![] } }

    fn peek(&self) -> Option<u8> { self.src.get(self.pos).copied() }
    fn peek2(&self) -> Option<u8> { self.src.get(self.pos + 1).copied() }
    fn bump(&mut self) -> Option<u8> { let c = self.peek()?; self.pos += 1; Some(c) }

    fn run(mut self) -> (Vec<Tok>, Vec<ParseError>) {
        while let Some(c) = self.peek() {
            match c {
                b' ' | b'\t' | b'\n' | b'\r' => { self.pos += 1; }
                b'(' => { let s = self.pos; self.pos += 1; self.push(TokKind::LParen, s); }
                b')' => { let s = self.pos; self.pos += 1; self.push(TokKind::RParen, s); }
                b'{' => { let s = self.pos; self.pos += 1; self.push(TokKind::LBrace, s); }
                b'}' => { let s = self.pos; self.pos += 1; self.push(TokKind::RBrace, s); }
                b'.' => { let s = self.pos; self.pos += 1; self.push(TokKind::Dot, s); }
                b',' => { let s = self.pos; self.pos += 1; self.push(TokKind::Comma, s); }
                b'+' => { let s = self.pos; self.pos += 1; self.push(TokKind::Plus, s); }
                b'*' => { let s = self.pos; self.pos += 1; self.push(TokKind::Star, s); }
                b'`' => { let s = self.pos; self.pos += 1; self.push(TokKind::Backtick, s); }
                b'|' => { let s = self.pos; self.pos += 1; self.push(TokKind::Pipe, s); }
                b'-' if self.peek2() == Some(b'>') => {
                    let s = self.pos; self.pos += 2; self.push(TokKind::Arrow, s);
                }
                b'-' => { let s = self.pos; self.pos += 1; self.push(TokKind::Minus, s); }
                b'=' if self.peek2() == Some(b'=') => {
                    let s = self.pos; self.pos += 2; self.push(TokKind::EqEq, s);
                }
                b'=' if self.peek2() == Some(b'>') => {
                    let s = self.pos; self.pos += 2; self.push(TokKind::FatArrow, s);
                }
                b'=' => { let s = self.pos; self.pos += 1; self.push(TokKind::Eq, s); }
                b';' => { let s = self.pos; self.pos += 1; self.push(TokKind::Semi, s); }
                b'0'..=b'9' => self.lex_number(),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.lex_ident(),
                other => {
                    let s = self.pos; self.pos += 1;
                    self.errors.push(ParseError { span: Span::new(s, self.pos), message: format!("无法识别的字符 {:?}", other as char) });
                }
            }
        }
        let s = self.pos;
        self.toks.push(Tok { kind: TokKind::Eof, span: Span::new(s, s) });
        (self.toks, self.errors)
    }

    fn push(&mut self, kind: TokKind, start: usize) {
        self.toks.push(Tok { kind, span: Span::new(start, self.pos) });
    }

    fn lex_number(&mut self) {
        let start = self.pos;
        let mut val: i64 = 0;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                val = val.saturating_mul(10).saturating_add((c - b'0') as i64);
                self.pos += 1;
            } else { break; }
        }
        self.push(TokKind::Int(val), start);
    }

    fn lex_ident(&mut self) {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == b'_' { self.pos += 1; } else { break; }
        }
        let word = std::str::from_utf8(&self.src[start..self.pos]).unwrap().to_string();
        let kind = match word.as_str() {
            "let" => TokKind::KwLet,
            "rec" => TokKind::KwRec,
            "in" => TokKind::KwIn,
            "if" => TokKind::KwIf,
            "then" => TokKind::KwThen,
            "else" => TokKind::KwElse,
            "true" => TokKind::KwTrue,
            "false" => TokKind::KwFalse,
            "fun" => TokKind::KwFun,
            "match" => TokKind::KwMatch,
            "with" => TokKind::KwWith,
            "unit" => TokKind::KwUnit,
            _ => TokKind::Ident(word),
        };
        self.push(kind, start);
    }
}

struct Parser {
    toks: Vec<Tok>,
    pos: usize,
    nodes: Vec<NodeInfo>,
    errors: Vec<ParseError>,
}

impl Parser {
    fn new(toks: Vec<Tok>, lex_errors: Vec<ParseError>) -> Self {
        Parser { toks, pos: 0, errors: lex_errors, nodes: vec![] }
    }

    fn cur(&self) -> &Tok { &self.toks[self.pos] }
    fn at(&self, k: &TokKind) -> bool { std::mem::discriminant(&self.cur().kind) == std::mem::discriminant(k) }
    fn span(&self) -> Span { self.cur().span }

    fn bump(&mut self) -> Tok {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() { self.pos += 1; }
        t
    }

    fn eat(&mut self, k: TokKind) -> Option<Tok> {
        if self.at(&k) { Some(self.bump()) } else { None }
    }

    fn expect(&mut self, k: TokKind, what: &str) -> Option<Tok> {
        if self.at(&k) { Some(self.bump()) }
        else {
            self.errors.push(ParseError {
                span: self.span(),
                message: format!("期望 {}，但遇到 {}", what, tok_desc(&self.cur().kind)),
            });
            None
        }
    }

    fn new_node(&mut self, kind: NodeKind, span: Span, label: String) -> NodeId {
        let id = NodeId(self.nodes.len() as u32);
        self.nodes.push(NodeInfo { id, kind, span, label });
        id
    }

    fn parse_program(&mut self) -> Expr {
        let start = self.span().start;
        let mut e = self.parse_expr();
        while self.eat(TokKind::Semi).is_some() {
            let rhs = self.parse_expr();
            let span = Span::new(start, self.prev_end());
            let nid = self.new_node(NodeKind::Let, span, "(sequence)".into());
            e = Expr::Let { span, nid, rec_: false, name: "_".into(), name_span: Span::new(start,start),
                value: Box::new(e), body: Some(Box::new(rhs)) };
        }
        e
    }

    fn prev_end(&self) -> usize {
        self.toks[self.pos.saturating_sub(1)].span.end
    }

    /// expr := let-expr | if-expr | fun-expr | match-expr | binop
    fn parse_expr(&mut self) -> Expr {
        match &self.cur().kind {
            TokKind::KwLet => self.parse_let(),
            TokKind::KwIf => self.parse_if(),
            TokKind::KwFun => self.parse_fun(),
            TokKind::KwMatch => self.parse_match(),
            _ => self.parse_binop(0),
        }
    }

    fn parse_let(&mut self) -> Expr {
        let start = self.bump().span.start;
        let rec_ = self.eat(TokKind::KwRec).is_some();
        let name_tok = self.expect_ident("绑定名");
        let name = name_tok.as_ref().map(|t| ident_of(t)).unwrap_or_default();
        let name_span = name_tok.as_ref().map(|t| t.span).unwrap_or_else(|| Span::new(start,start));
        self.expect(TokKind::Eq, "'='");
        let value = self.parse_expr();
        self.eat(TokKind::KwIn);
        let body = if self.at(&TokKind::Eof) { None } else { Some(Box::new(self.parse_expr())) };
        let end = body.as_ref().map(|b| b.span().end).unwrap_or_else(|| value.span().end);
        let span = Span::new(start, end);
        let nid = self.new_node(NodeKind::Let, span, format!("let {}", name));
        Expr::Let { span, nid, rec_, name, name_span, value: Box::new(value), body }
    }

    fn parse_if(&mut self) -> Expr {
        let start = self.bump().span.start;
        let cond = self.parse_expr();
        self.expect(TokKind::KwThen, "'then'");
        let then_e = self.parse_expr();
        self.expect(TokKind::KwElse, "'else'");
        let else_e = self.parse_expr();
        let span = Span::new(start, else_e.span().end);
        let nid = self.new_node(NodeKind::If, span, "if".into());
        Expr::If { span, nid, cond: Box::new(cond), then_e: Box::new(then_e), else_e: Box::new(else_e) }
    }

    fn parse_fun(&mut self) -> Expr {
        let start = self.bump().span.start;
        let param_tok = self.expect_ident("参数名");
        let param = param_tok.as_ref().map(|t| ident_of(t)).unwrap_or_default();
        let param_span = param_tok.as_ref().map(|t| t.span).unwrap_or_else(|| Span::new(start,start));
        let param_nid = self.new_node(NodeKind::Var, param_span, format!("参数 {}", param));
        self.expect(TokKind::Arrow, "'->'");
        let body = self.parse_expr();
        let span = Span::new(start, body.span().end);
        let nid = self.new_node(NodeKind::Lam, span, format!("λ {}", param));
        Expr::Lam { span, nid, param, param_span, param_nid, body: Box::new(body) }
    }

    fn parse_match(&mut self) -> Expr {
        let start = self.bump().span.start;
        let scrutinee = self.parse_app();
        self.expect(TokKind::KwWith, "'with'");
        let mut arms = vec![];
        self.eat(TokKind::Pipe);
        while !self.at(&TokKind::Eof) && !self.at(&TokKind::KwLet)
            && !self.at(&TokKind::KwIn) && !self.at(&TokKind::KwThen) && !self.at(&TokKind::KwElse) {
            arms.push(self.parse_arm());
            if self.eat(TokKind::Pipe).is_none() { break; }
        }
        let end = arms.last().map(|a| a.body.span().end).unwrap_or_else(|| scrutinee.span().end);
        let span = Span::new(start, end);
        let nid = self.new_node(NodeKind::Match, span, "match".into());
        Expr::Match { span, nid, scrutinee: Box::new(scrutinee), arms }
    }

    fn parse_arm(&mut self) -> MatchArm {
        // 可选反引号
        let bt = self.eat(TokKind::Backtick);
        let label_tok = self.expect_label();
        let label = label_tok.as_ref().map(|t| label_of(t)).unwrap_or_default();
        let label_span = match (&bt, &label_tok) {
            (Some(b), Some(t)) => Span::merge(b.span, t.span),
            (_, Some(t)) => t.span,
            _ => Span::new(0, 0),
        };
        let (payload, payload_span) = if self.eat(TokKind::LParen).is_some() {
            let p = self.expect_ident("载荷变量");
            let pname = p.as_ref().map(|t| ident_of(t));
            let pspan = p.as_ref().map(|t| t.span);
            self.expect(TokKind::RParen, "')'");
            (pname, pspan)
        } else { (None, None) };
        let payload_nid = match payload_span {
            Some(sp) => self.new_node(NodeKind::Var, sp, format!("载荷 {}", payload.clone().unwrap_or_default())),
            None => self.new_node(NodeKind::Unit, label_span, "无载荷".into()),
        };
        self.expect(TokKind::FatArrow, "'=>'");
        let body = self.parse_expr();
        MatchArm { label, label_span, payload, payload_span, payload_nid, body }
    }

    /// Pratt 表达式（= 优先级最低）。
    fn parse_binop(&mut self, min_prec: u8) -> Expr {
        let mut lhs = self.parse_app();
        loop {
            let (op, prec) = match &self.cur().kind {
                TokKind::Plus => (BinOpKind::Add, 1),
                TokKind::Minus => (BinOpKind::Sub, 1),
                TokKind::Star => (BinOpKind::Mul, 2),
                TokKind::EqEq => (BinOpKind::Eq, 1),
                _ => break,
            };
            if prec < min_prec { break; }
            let op_tok = self.bump();
            let rhs = self.parse_binop(prec + 1);
            let span = Span::new(lhs.span().start, rhs.span().end);
            let nid = self.new_node(NodeKind::BinOp, span, binop_name(&op).into());
            lhs = Expr::BinOp { span, nid, op, lhs: Box::new(lhs), rhs: Box::new(rhs) };
            let _ = op_tok;
        }
        lhs
    }

    /// app := atom+
    fn parse_app(&mut self) -> Expr {
        let mut e = self.parse_postfix();
        loop {
            if self.starts_atom() {
                let arg = self.parse_postfix();
                let span = Span::new(e.span().start, arg.span().end);
                let nid = self.new_node(NodeKind::App, span, "应用".into());
                e = Expr::App { span, nid, fun: Box::new(e), arg: Box::new(arg) };
            } else { break; }
        }
        e
    }

    fn starts_atom(&self) -> bool {
        matches!(self.cur().kind,
            TokKind::Int(_) | TokKind::Ident(_) | TokKind::LParen | TokKind::LBrace
            | TokKind::Backtick | TokKind::KwTrue | TokKind::KwFalse | TokKind::KwUnit)
    }

    /// postfix := atom ('.' label)*
    fn parse_postfix(&mut self) -> Expr {
        let mut e = self.parse_atom();
        while self.eat(TokKind::Dot).is_some() {
            let lbl = self.expect_label();
            let (name, fspan) = match &lbl {
                Some(t) => (label_of(t), t.span),
                None => (String::new(), self.span()),
            };
            let span = Span::new(e.span().start, fspan.end);
            let nid = self.new_node(NodeKind::Proj, span, format!(".{}", name));
            e = Expr::Proj { span, nid, record: Box::new(e), field: name, field_span: fspan };
        }
        e
    }

    fn parse_atom(&mut self) -> Expr {
        let t = self.cur().clone();
        match &t.kind {
            TokKind::Int(v) => {
                let v = *v;
                self.bump();
                let nid = self.new_node(NodeKind::LitInt, t.span, v.to_string());
                Expr::LitInt { span: t.span, nid, value: v }
            }
            TokKind::KwTrue | TokKind::KwFalse => {
                let v = matches!(t.kind, TokKind::KwTrue);
                self.bump();
                let nid = self.new_node(NodeKind::LitBool, t.span, v.to_string());
                Expr::LitBool { span: t.span, nid, value: v }
            }
            TokKind::KwUnit => {
                self.bump();
                let nid = self.new_node(NodeKind::Unit, t.span, "()".into());
                Expr::Unit { span: t.span, nid }
            }
            TokKind::Ident(name) => {
                let name = name.clone();
                self.bump();
                let nid = self.new_node(NodeKind::Var, t.span, name.clone());
                Expr::Var { span: t.span, nid, name }
            }
            TokKind::Backtick => self.parse_variant(),
            TokKind::LParen => {
                self.bump();
                let e = self.parse_expr();
                self.expect(TokKind::RParen, "')'");
                e
            }
            TokKind::LBrace => self.parse_record(),
            _ => {
                self.errors.push(ParseError { span: t.span, message: format!("期望表达式，但遇到 {}", tok_desc(&t.kind)) });
                self.bump();
                let nid = self.new_node(NodeKind::Unit, t.span, "<错误>".into());
                Expr::Unit { span: t.span, nid }
            }
        }
    }

    fn parse_variant(&mut self) -> Expr {
        let start = self.bump().span.start;
        let lbl = self.expect_label();
        let (label, label_span) = match &lbl {
            Some(t) => (label_of(t), t.span),
            None => (String::new(), self.span()),
        };
        let payload = if self.eat(TokKind::LParen).is_some() {
            let p = self.parse_expr();
            self.expect(TokKind::RParen, "')'");
            Some(Box::new(p))
        } else { None };
        let end = payload.as_ref().map(|p| p.span().end).unwrap_or(label_span.end);
        let span = Span::new(start, end);
        let nid = self.new_node(NodeKind::Variant, span, format!("`{}", label));
        Expr::Variant { span, nid, label, label_span, payload }
    }

    fn parse_record(&mut self) -> Expr {
        let start = self.bump().span.start;
        let mut fields = vec![];
        if !self.at(&TokKind::RBrace) {
            loop {
                let lbl = self.expect_label();
                let (name, name_span) = match &lbl {
                    Some(t) => (label_of(t), t.span),
                    None => (String::new(), self.span()),
                };
                self.expect(TokKind::Eq, "'='");
                let value = self.parse_expr();
                fields.push(FieldExpr { name, name_span, value });
                if self.eat(TokKind::Comma).is_some() { continue; }
                break;
            }
        }
        self.expect(TokKind::RBrace, "'}'");
        let span = Span::new(start, self.prev_end());
        let nid = self.new_node(NodeKind::Record, span, "record".into());
        Expr::Record { span, nid, fields }
    }

    fn expect_ident(&mut self, what: &str) -> Option<Tok> {
        if matches!(self.cur().kind, TokKind::Ident(_)) { Some(self.bump()) }
        else {
            self.errors.push(ParseError { span: self.span(), message: format!("期望{}，但遇到 {}", what, tok_desc(&self.cur().kind)) });
            None
        }
    }

    /// 标签（字段名/变体名）：首字母大写的标识符。
    fn expect_label(&mut self) -> Option<Tok> {
        if let TokKind::Ident(s) = &self.cur().kind {
            if s.chars().next().map(|c| c.is_ascii_uppercase()).unwrap_or(false) {
                return Some(self.bump());
            }
        }
        self.errors.push(ParseError { span: self.span(), message: format!("期望首字母大写的标签，但遇到 {}", tok_desc(&self.cur().kind)) });
        None
    }
}

fn ident_of(t: &Tok) -> String {
    if let TokKind::Ident(s) = &t.kind { s.clone() } else { String::new() }
}
fn label_of(t: &Tok) -> String { ident_of(t) }
fn binop_name(op: &BinOpKind) -> &'static str {
    match op { BinOpKind::Add => "+", BinOpKind::Sub => "-", BinOpKind::Mul => "*", BinOpKind::Eq => "==" }
}
fn tok_desc(k: &TokKind) -> String {
    match k {
        TokKind::Int(v) => format!("整数 {}", v),
        TokKind::Ident(s) => format!("标识符 {}", s),
        TokKind::LParen => "'('".into(), TokKind::RParen => "')'".into(),
        TokKind::LBrace => "'{'".into(), TokKind::RBrace => "'}'".into(),
        TokKind::Dot => "'.'".into(), TokKind::Comma => "','".into(),
        TokKind::Eq => "'='".into(), TokKind::Arrow => "'->'".into(), TokKind::FatArrow => "'=>'".into(),
        TokKind::KwLet => "'let'".into(), TokKind::KwRec => "'rec'".into(), TokKind::KwIn => "'in'".into(),
        TokKind::KwIf => "'if'".into(), TokKind::KwThen => "'then'".into(), TokKind::KwElse => "'else'".into(),
        TokKind::KwTrue => "'true'".into(), TokKind::KwFalse => "'false'".into(),
        TokKind::KwFun => "'fun'".into(), TokKind::KwMatch => "'match'".into(), TokKind::KwWith => "'with'".into(),
        TokKind::KwUnit => "'unit'".into(),
        TokKind::Plus => "'+'".into(), TokKind::Minus => "'-'".into(), TokKind::Star => "'*'".into(),
        TokKind::EqEq => "'=='".into(), TokKind::Backtick => "'`'".into(), TokKind::Pipe => "'|'".into(),
        TokKind::Semi => "';'".into(), TokKind::Eof => "文件结束".into(),
    }
}

/// 解析入口。
pub fn parse(src: &str) -> Program {
    let (toks, lex_errors) = Lexer::new(src).run();
    let mut p = Parser::new(toks, lex_errors);
    let root = if src.trim().is_empty() {
        let nid = p.new_node(NodeKind::Unit, Span::new(0, 0), "空程序".into());
        Expr::Unit { span: Span::new(0, 0), nid }
    } else {
        p.parse_program()
    };
    Program { root, nodes: p.nodes, errors: p.errors }
}

/// 供 web/incremental 层使用的公开入口。
pub fn parse_pub(src: &str) -> Program { parse(src) }
