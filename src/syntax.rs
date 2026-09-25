//! Lexer + parser for the tiny HM-style language, plus binder resolution
//! and stable node-key assignment.

use std::collections::HashMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Ident(String),
    Upper(String),
    Label(String),
    Int(i64),
    Str(String),
    // keywords
    Let, Rec, In, Fun, Arrow, DArrow, Eq, If, Then, Else, True, False,
    Match, With, Extend,
    // punctuation
    LParen, RParen, LBrace, RBrace, Comma, Dot, Semicolon, Colon, Bar,
    // operators
    Plus, Minus, Star, Slash, Lt, Gt, Le, Ge, EqEq, PlusEq,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '(' && i + 1 < b.len() && b[i + 1] == b'*' {
            let start = i;
            i += 2;
            let mut depth = 1;
            while i < b.len() && depth > 0 {
                if i + 1 < b.len() && b[i] == b'(' && b[i + 1] == b'*' {
                    depth += 1;
                    i += 2;
                } else if i + 1 < b.len() && b[i] == b'*' && b[i + 1] == b')' {
                    depth -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if depth > 0 {
                return Err(ParseError {
                    span: Span::new(start, src.len()),
                    message: "未闭合的注释 (* … *)".to_string(),
                });
            }
            continue;
        }
        let start = i;
        if c.is_ascii_digit() {
            let mut n: i64 = 0;
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                n = n.saturating_mul(10).saturating_add((b[i] - b'0') as i64);
                i += 1;
            }
            out.push(Token { tok: Tok::Int(n), span: Span::new(start, i) });
            continue;
        }
        if c == '"' {
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] as char != '"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    s.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        _ => e,
                    });
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(ParseError {
                    span: Span::new(start, src.len()),
                    message: "未闭合的字符串".to_string(),
                });
            }
            i += 1;
            out.push(Token { tok: Tok::Str(s), span: Span::new(start, i) });
            continue;
        }
        if c == '`' {
            i += 1;
            let s = read_ident(b, &mut i);
            out.push(Token { tok: Tok::Label(s), span: Span::new(start, i) });
            continue;
        }
        if c == '_' || c.is_alphabetic() {
            let s = read_ident(b, &mut i);
            i = {
                let mut j = i;
                // recompute end: read_ident advanced `i` via &mut already
                let _ = &mut j;
                i
            };
            let tok = match s.as_str() {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fun" => Tok::Fun,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "true" => Tok::True,
                "false" => Tok::False,
                "match" => Tok::Match,
                "with" => Tok::With,
                "extend" => Tok::Extend,
                _ => {
                    if starts_upper(&s) {
                        Tok::Upper(s)
                    } else {
                        Tok::Ident(s)
                    }
                }
            };
            out.push(Token { tok, span: Span::new(start, i) });
            continue;
        }
        let two: Option<Tok> = if i + 1 < b.len() {
            match &src[i..i + 2] {
                "->" => Some(Tok::Arrow),
                "=>" => Some(Tok::DArrow),
                "<=" => Some(Tok::Le),
                ">=" => Some(Tok::Ge),
                "==" => Some(Tok::EqEq),
                "|+" | "+|" => None,
                _ => None,
            }
        } else {
            None
        };
        if let Some(t) = two {
            out.push(Token { tok: t, span: Span::new(start, i + 2) });
            i += 2;
            continue;
        }
        let one = match c {
            '(' => Tok::LParen,
            ')' => Tok::RParen,
            '{' => Tok::LBrace,
            '}' => Tok::RBrace,
            ',' => Tok::Comma,
            '.' => Tok::Dot,
            ';' => Tok::Semicolon,
            ':' => Tok::Colon,
            '|' => Tok::Bar,
            '+' => Tok::Plus,
            '-' => Tok::Minus,
            '*' => Tok::Star,
            '/' => Tok::Slash,
            '<' => Tok::Lt,
            '>' => Tok::Gt,
            '=' => Tok::Eq,
            _ => {
                return Err(ParseError {
                    span: Span::new(i, i + 1),
                    message: format!("无法识别的字符 {:?}", c),
                })
            }
        };
        out.push(Token { tok: one, span: Span::new(start, i + 1) });
        i += 1;
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(src.len(), src.len()) });
    Ok(out)
}

fn read_ident(b: &[u8], i: &mut usize) -> String {
    let start = *i;
    while *i < b.len() {
        let c = b[*i] as char;
        if c == '_' || c.is_alphanumeric() {
            *i += 1;
        } else {
            break;
        }
    }
    String::from_utf8_lossy(&b[start..*i]).to_string()
}

fn starts_upper(s: &str) -> bool {
    s.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
}

pub type NId = usize;
pub type BindId = usize;

#[derive(Clone, Debug)]
pub struct Binder {
    pub name: String,
    pub span: Span,
    pub binding_node: NId,
    pub key: String,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub label: String,
    pub label_span: Span,
    pub value: NId,
    pub punned: bool,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub tag: String,
    pub tag_span: Span,
    pub payload: Option<(String, Span, BindId)>,
    pub body: NId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Var { name: String, resolved: Option<BindId> },
    Int(i64),
    Str(String),
    Bool(bool),
    BinOp { op: BinOp, left: NId, right: NId },
    If { cond: NId, then_e: NId, else_e: NId },
    Fun { param: BindId, body: NId },
    App { fun: NId, arg: NId },
    Record { fields: Vec<Field>, extends: Option<NId> },
    FieldGet { record: NId, label: String, label_span: Span },
    Variant { tag: String, tag_span: Span, payload: Option<NId> },
    Match { scrutinee: NId, arms: Vec<Arm> },
    Let {
        rec: bool,
        binder: BindId,
        init: NId,
        next: NId,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    Plus, Minus, Star, Slash, Lt, Gt, Le, Ge, EqEq,
}

#[derive(Clone, Debug)]
pub struct Node {
    pub expr: Expr,
    pub span: Span,
    pub key: String,
}

#[derive(Clone, Debug)]
pub struct TopLet {
    pub rec: bool,
    pub binder: BindId,
    pub init: NId,
    pub span: Span,
}

#[derive(Clone, Debug, Default)]
pub struct Ast {
    pub nodes: Vec<Node>,
    pub binders: Vec<Binder>,
    pub tops: Vec<TopLet>,
    pub errors: Vec<ParseError>,
}

impl Ast {
    fn add_node(&mut self, expr: Expr, span: Span) -> NId {
        let id = self.nodes.len();
        self.nodes.push(Node { expr, span, key: String::new() });
        id
    }
    fn add_binder(&mut self, name: String, span: Span, binding_node: NId) -> BindId {
        let id = self.binders.len();
        self.binders.push(Binder {
            name,
            span,
            binding_node,
            key: String::new(),
        });
        id
    }
}

struct Parser<'a> {
    toks: &'a [Token],
    pos: usize,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.peek() == t {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: &Tok, what: &str) -> bool {
        if self.eat(t) {
            true
        } else {
            self.errors.push(ParseError {
                span: self.span(),
                message: format!("期望 {}，但遇到 {}", what, tok_desc(self.peek())),
            });
            false
        }
    }
    fn err<T>(&mut self, span: Span, msg: String, ast: &mut Ast) -> NId {
        self.errors.push(ParseError { span, message: msg });
        ast.add_node(Expr::Int(0), span)
    }
}

fn tok_desc(t: &Tok) -> String {
    match t {
        Tok::Ident(s) => format!("标识符 “{}”", s),
        Tok::Upper(s) => format!("构造器 “{}”", s),
        Tok::Label(s) => format!("标签 “`{}`", s),
        Tok::Int(_) => "整数".to_string(),
        Tok::Str(_) => "字符串".to_string(),
        Tok::Eof => "文件结尾".to_string(),
        _ => format!("{:?}", t),
    }
}

pub fn parse(src: &str) -> Ast {
    let mut ast = Ast::default();
    let toks = match lex(src) {
        Ok(t) => t,
        Err(e) => {
            ast.errors.push(e);
            return ast;
        }
    };
    let mut p = Parser { toks: &toks, pos: 0, errors: Vec::new() };
    parse_program(&mut p, &mut ast, src);
    ast.errors.append(&mut p.errors);
    resolve(&mut ast, src);
    assign_keys(&mut ast);
    ast
}

fn parse_program(p: &mut Parser, ast: &mut Ast, src: &str) {
    while !matches!(p.peek(), Tok::Eof) {
        let start = p.span().start;
        if !p.eat(&Tok::Let) {
            let sp = p.span();
            p.errors.push(ParseError {
                span: sp,
                message: format!("顶层只允许 let 定义，但遇到 {}", tok_desc(p.peek())),
            });
            // recover by skipping to next let or eof
            while !matches!(p.peek(), Tok::Eof | Tok::Let) {
                p.bump();
            }
            continue;
        }
        let rec = p.eat(&Tok::Rec);
        let (name, name_span) = match p.bump().tok {
            Tok::Ident(s) => {
                let sp = p.toks[p.pos.saturating_sub(1)].span;
                (s, sp)
            }
            other => {
                let sp = p.span();
                p.errors.push(ParseError {
                    span: sp,
                    message: format!("let 后需要标识符，但遇到 {}", tok_desc(&other)),
                });
                ("_err".to_string(), sp)
            }
        };
        let placeholder = ast.add_node(Expr::Int(0), name_span);
        let binder = ast.add_binder(name, name_span, placeholder);
        // optional parameters: sugar fun
        let mut params: Vec<(String, Span)> = Vec::new();
        while let Tok::Ident(_) = p.peek() {
            let t = p.bump();
            if let Tok::Ident(s) = t.tok {
                params.push((s, t.span));
            }
        }
        if !p.expect(&Tok::Eq, "“=”") {
            // try recover to next top-level let
            while !matches!(p.peek(), Tok::Eof | Tok::Let) {
                p.bump();
            }
            let init = ast.add_node(Expr::Int(0), name_span);
            ast.nodes[placeholder].expr = Expr::Var {
                name: ast.binders[binder].name.clone(),
                resolved: None,
            };
            ast.tops.push(TopLet { rec, binder, init, span: Span::new(start, name_span.end) });
            continue;
        }
        let mut body = parse_expr(p, ast, src);
        for (pn, psp) in params.into_iter().rev() {
            let pb = ast.add_binder(pn, psp, body);
            body = ast.add_node(Expr::Fun { param: pb, body }, Span::new(psp.start, ast.nodes[body].span.end));
        }
        ast.nodes[placeholder].expr = Expr::Var {
            name: ast.binders[binder].name.clone(),
            resolved: None,
        };
        let end = ast.nodes[body].span.end;
        ast.tops.push(TopLet { rec, binder, init: body, span: Span::new(start, end) });
        p.eat(&Tok::Semicolon);
    }
}

fn parse_expr(p: &mut Parser, ast: &mut Ast, src: &str) -> NId {
    parse_fun_let(p, ast, src)
}

fn parse_binder_ident(p: &mut Parser, ast: &mut Ast, name_span: Span, name: String, at_node: NId) -> BindId {
    ast.add_binder(name, name_span, at_node)
}

fn parse_fun_let(p: &mut Parser, ast: &mut Ast, src: &str) -> NId {
    if matches!(p.peek(), Tok::Fun) {
        let start = p.bump().span.start;
        let (pname, psp) = match p.bump().tok {
            Tok::Ident(s) => {
                let sp = p.toks[p.pos - 1].span;
                (s, sp)
            }
            other => {
                let sp = p.span();
                p.errors.push(ParseError {
                    span: sp,
                    message: format!("fun 后需要参数名，但遇到 {}", tok_desc(&other)),
                });
                ("_".to_string(), sp)
            }
        };
        if !p.expect(&Tok::Arrow, "“->”") {}
        let placeholder = ast.add_node(Expr::Int(0), psp);
        let binder = parse_binder_ident(p, ast, psp, pname, placeholder);
        let body = parse_fun_let(p, ast, src);
        let end = ast.nodes[body].span.end;
        let id = ast.add_node(Expr::Fun { param: binder, body }, Span::new(start, end));
        ast.nodes[placeholder].expr = Expr::Var {
            name: ast.binders[binder].name.clone(),
            resolved: None,
        };
        return id;
    }
    if matches!(p.peek(), Tok::Let) {
        let start = p.bump().span.start;
        let rec = p.eat(&Tok::Rec);
        let (name, name_span) = match p.bump().tok {
            Tok::Ident(s) => (s, p.toks[p.pos - 1].span),
            other => {
                let sp = p.span();
                p.errors.push(ParseError {
                    span: sp,
                    message: format!("let 后需要标识符，但遇到 {}", tok_desc(&other)),
                });
                ("_".to_string(), sp)
            }
        };
        let placeholder = ast.add_node(Expr::Int(0), name_span);
        let binder = ast.add_binder(name, name_span, placeholder);
        let mut params: Vec<(String, Span)> = Vec::new();
        while let Tok::Ident(_) = p.peek() {
            let t = p.bump();
            if let Tok::Ident(s) = t.tok {
                params.push((s, t.span));
            }
        }
        p.expect(&Tok::Eq, "“=”");
        let mut init = parse_expr(p, ast, src);
        for (pn, psp) in params.into_iter().rev() {
            let pb = ast.add_binder(pn, psp, init);
            init = ast.add_node(
                Expr::Fun { param: pb, body: init },
                Span::new(psp.start, ast.nodes[init].span.end),
            );
        }
        p.expect(&Tok::In, "“in”");
        let next = parse_expr(p, ast, src);
        let end = ast.nodes[next].span.end;
        ast.nodes[placeholder].expr = Expr::Var {
            name: ast.binders[binder].name.clone(),
            resolved: None,
        };
        return ast.add_node(
            Expr::Let { rec, binder, init, next },
            Span::new(start, end),
        );
    }
    parse_if_match(p, ast, src)
}

fn parse_if_match(p: &mut Parser, ast: &mut Ast, src: &str) -> NId {
    if matches!(p.peek(), Tok::If) {
        let start = p.bump().span.start;
        let cond = parse_expr(p, ast, src);
        p.expect(&Tok::Then, "“then”");
        let then_e = parse_expr(p, ast, src);
        p.expect(&Tok::Else, "“else”");
        let else_e = parse_expr(p, ast, src);
        let end = ast.nodes[else_e].span.end;
        return ast.add_node(
            Expr::If { cond, then_e, else_e },
            Span::new(start, end),
        );
    }
    if matches!(p.peek(), Tok::Match) {
        let start = p.bump().span.start;
        let scrutinee = parse_expr(p, ast, src);
        p.expect(&Tok::With, "“with”");
        let mut arms = Vec::new();
        loop {
            p.eat(&Tok::Bar);
            match p.peek().clone() {
                Tok::Upper(_) | Tok::Label(_) => {}
                _ => break,
            }
            let arm_start = p.span().start;
            let tt = p.bump();
            let (tag, tag_span) = match tt.tok {
                Tok::Upper(s) => (s, tt.span),
                Tok::Label(s) => (s, tt.span),
                _ => unreachable!(),
            };
            let payload = if let Tok::Ident(s) = p.peek().clone() {
                let t = p.bump();
                let placeholder = ast.add_node(Expr::Int(0), t.span);
                let b = ast.add_binder(s, t.span, placeholder);
                ast.nodes[placeholder].expr = Expr::Var {
                    name: ast.binders[b].name.clone(),
                    resolved: None,
                };
                Some((ast.binders[b].name.clone(), t.span, b))
            } else {
                None
            };
            p.expect(&Tok::DArrow, "“=>”");
            let body = parse_fun_let(p, ast, src);
            let end = ast.nodes[body].span.end;
            arms.push(Arm { tag, tag_span, payload, body, span: Span::new(arm_start, end) });
        }
        if arms.is_empty() {
            p.errors.push(ParseError {
                span: p.span(),
                message: "match 需要至少一个分支 `标签 => …`".to_string(),
            });
            let sp = p.span();
            return ast.add_node(Expr::Match { scrutinee, arms: Vec::new() }, Span::new(start, sp.end));
        }
        let end = arms.last().unwrap().span.end;
        return ast.add_node(Expr::Match { scrutinee, arms }, Span::new(start, end));
    }
    parse_binop(p, ast, src, 0)
}

fn binop_of(t: &Tok) -> Option<(BinOp, u8)> {
    Some(match t {
        Tok::EqEq => (BinOp::EqEq, 1),
        Tok::Lt => (BinOp::Lt, 2),
        Tok::Gt => (BinOp::Gt, 2),
        Tok::Le => (BinOp::Le, 2),
        Tok::Ge => (BinOp::Ge, 2),
        Tok::Plus => (BinOp::Plus, 3),
        Tok::Minus => (BinOp::Minus, 3),
        Tok::Star => (BinOp::Star, 4),
        Tok::Slash => (BinOp::Slash, 4),
        _ => return None,
    })
}

fn parse_binop(p: &mut Parser, ast: &mut Ast, src: &str, min_prec: u8) -> NId {
    let mut left = parse_unary(p, ast, src);
    loop {
        let (op, prec) = match binop_of(p.peek()) {
            Some(v) => v,
            None => break,
        };
        if prec < min_prec {
            break;
        }
        let ot = p.bump();
        let right = parse_binop(p, ast, src, prec + 1);
        let end = ast.nodes[right].span.end;
        left = ast.add_node(
            Expr::BinOp { op, left, right },
            Span::new(ast.nodes[left].span.start, end),
        );
        let _ = ot;
    }
    left
}

fn parse_unary(p: &mut Parser, ast: &mut Ast, src: &str) -> NId {
    if matches!(p.peek(), Tok::Minus) {
        let t = p.bump();
        let e = parse_postfix(p, ast, src);
        let zero = ast.add_node(Expr::Int(0), t.span);
        let end = ast.nodes[e].span.end;
        return ast.add_node(
            Expr::BinOp { op: BinOp::Minus, left: zero, right: e },
            Span::new(t.span.start, end),
        );
    }
    parse_postfix(p, ast, src)
}

fn parse_postfix(p: &mut Parser, ast: &mut Ast, src: &str) -> NId {
    let mut e = parse_atom(p, ast, src);
    loop {
        if matches!(p.peek(), Tok::Dot) {
            p.bump();
            let (label, label_span) = if let Tok::Ident(s) = p.peek().clone() {
                let t = p.bump();
                (s, t.span)
            } else {
                let sp = p.span();
                p.errors.push(ParseError { span: sp, message: "“.” 后需要字段名".to_string() });
                ("_".to_string(), sp)
            };
            let end = label_span.end;
            e = ast.add_node(
                Expr::FieldGet { record: e, label, label_span },
                Span::new(ast.nodes[e].span.start, end),
            );
            continue;
        }
        if starts_atom(p.peek()) {
            let arg = parse_atom(p, ast, src);
            let end = ast.nodes[arg].span.end;
            e = ast.add_node(
                Expr::App { fun: e, arg },
                Span::new(ast.nodes[e].span.start, end),
            );
            continue;
        }
        break;
    }
    e
}

fn starts_atom(t: &Tok) -> bool {
    matches!(
        t,
        Tok::Ident(_)
            | Tok::Upper(_)
            | Tok::Label(_)
            | Tok::Int(_)
            | Tok::Str(_)
            | Tok::True
            | Tok::False
            | Tok::LParen
            | Tok::LBrace
    )
}

fn parse_atom(p: &mut Parser, ast: &mut Ast, src: &str) -> NId {
    let t = p.peek().clone();
    match t {
        Tok::Int(n) => {
            let tk = p.bump();
            ast.add_node(Expr::Int(n), tk.span)
        }
        Tok::Str(s) => {
            let tk = p.bump();
            ast.add_node(Expr::Str(s), tk.span)
        }
        Tok::True => {
            let tk = p.bump();
            ast.add_node(Expr::Bool(true), tk.span)
        }
        Tok::False => {
            let tk = p.bump();
            ast.add_node(Expr::Bool(false), tk.span)
        }
        Tok::Ident(s) => {
            let tk = p.bump();
            ast.add_node(Expr::Var { name: s, resolved: None }, tk.span)
        }
        Tok::Upper(_) | Tok::Label(_) => parse_variant_or_extend(p, ast, src),
        Tok::LParen => {
            let l = p.bump().span;
            let e = parse_expr(p, ast, src);
            if !p.expect(&Tok::RParen, "“)”") {
                return ast.add_node(Expr::Int(0), l);
            }
            let r = p.toks[p.pos - 1].span;
            // preserve span including parens
            let mut node = ast.nodes[e].clone();
            node.span = Span::new(l.start, r.end);
            ast.nodes[e] = node;
            e
        }
        Tok::LBrace => parse_record(p, ast, src),
        other => {
            let sp = p.span();
            p.errors.push(ParseError {
                span: sp,
                message: format!("期望表达式，但遇到 {}", tok_desc(&other)),
            });
            ast.add_node(Expr::Int(0), sp)
        }
    }
}

fn parse_variant_or_extend(p: &mut Parser, ast: &mut Ast, _src: &str) -> NId {
    let tt = p.bump();
    let (tag, tag_span) = match tt.tok {
        Tok::Upper(s) => (s, tt.span),
        Tok::Label(s) => (s, tt.span),
        _ => unreachable!(),
    };
    if tag == "extend" || tag == "Extend" {
        // should not reach: lowercase extend is keyword, but `extend label is possible
    }
    if starts_atom(p.peek()) {
        let payload = parse_atom(p, ast, _src);
        let end = ast.nodes[payload].span.end;
        ast.add_node(Expr::Variant { tag, tag_span, payload: Some(payload) }, Span::new(tag_span.start, end))
    } else {
        ast.add_node(Expr::Variant { tag, tag_span, payload: None }, tag_span)
    }
}

fn parse_record(p: &mut Parser, ast: &mut Ast, src: &str) -> NId {
    let lb = p.bump().span;
    let mut fields = Vec::new();
    let mut extends = None;
    let mut saw_extend = false;
    loop {
        if matches!(p.peek(), Tok::RBrace | Tok::Eof) {
            break;
        }
        if matches!(p.peek(), Tok::Extend) {
            let t = p.bump();
            let e = parse_postfix(p, ast, src);
            extends = Some(e);
            saw_extend = true;
            let _ = t;
        } else if let Tok::Ident(s) = p.peek().clone() {
            let lt = p.bump();
            let fstart = lt.span.start;
            if matches!(p.peek(), Tok::Eq) {
                p.bump();
                let e = parse_expr(p, ast, src);
                let end = ast.nodes[e].span.end;
                fields.push(Field { label: s, label_span: lt.span, value: e, punned: false, span: Span::new(fstart, end) });
            } else {
                // punning: label stands for same-named variable
                let vid = ast.add_node(Expr::Var { name: s.clone(), resolved: None }, lt.span);
                fields.push(Field { label: s, label_span: lt.span, value: vid, punned: true, span: lt.span });
            }
        } else {
            let sp = p.span();
            p.errors.push(ParseError { span: sp, message: "记录中需要字段名".to_string() });
            p.bump();
            continue;
        }
        if !p.eat(&Tok::Comma) {
            break;
        }
    }
    let rb = if p.expect(&Tok::RBrace, "“}”") {
        p.toks[p.pos - 1].span
    } else {
        p.span()
    };
    let span = Span::new(lb.start, rb.end);
    let _ = saw_extend;
    ast.add_node(Expr::Record { fields, extends }, span)
}

fn children_of(e: &Expr) -> Vec<(NId, &'static str)> {
    match e {
        Expr::Var { .. } | Expr::Int(_) | Expr::Str(_) | Expr::Bool(_) => vec![],
        Expr::BinOp { left, right, .. } => vec![(*left, "l"), (*right, "r")],
        Expr::If { cond, then_e, else_e } => {
            vec![(*cond, "c"), (*then_e, "t"), (*else_e, "e")]
        }
        Expr::Fun { body, .. } => vec![(*body, "body")],
        Expr::App { fun, arg } => vec![(*fun, "f"), (*arg, "a")],
        Expr::Record { fields, extends } => {
            let mut v: Vec<(NId, &'static str)> = Vec::new();
            // ordinal-based roles so inserting a field shifts later fields (structural edit),
            // while a rename never changes any key.
            for (idx, f) in fields.iter().enumerate() {
                v.push((f.value, "f"));
                let _ = idx;
            }
            if let Some(e) = extends {
                v.push((*e, "ext"));
            }
            v
        }
        Expr::FieldGet { record, .. } => vec![(*record, "rec")],
        Expr::Variant { payload, .. } => match payload {
            Some(p) => vec![(*p, "p")],
            None => vec![],
        },
        Expr::Match { scrutinee, arms } => {
            let mut v = vec![(*scrutinee, "s")];
            for a in arms {
                v.push((a.body, "arm"));
            }
            v
        }
        Expr::Let { init, next, .. } => vec![(*init, "i"), (*next, "n")],
    }
}

struct Resolver<'a> {
    ast: &'a mut Ast,
    scope: Vec<(String, BindId)>,
}

fn resolve(ast: &mut Ast, _src: &str) {
    let mut r = Resolver { ast, scope: Vec::new() };
    let tops: Vec<(bool, BindId, NId)> = r
        .ast
        .tops
        .iter()
        .map(|t| (t.rec, t.binder, t.init))
        .collect();
    for (rec, binder, init) in tops {
        if rec {
            let name = r.ast.binders[binder].name.clone();
            r.scope.push((name, binder));
        }
        r.resolve_node(init);
        if !rec {
            let name = r.ast.binders[binder].name.clone();
            r.scope.push((name, binder));
        }
    }
}

impl<'a> Resolver<'a> {
    fn lookup(&self, name: &str) -> Option<BindId> {
        self.scope.iter().rev().find(|(n, _)| n == name).map(|(_, b)| *b)
    }

    fn resolve_node(&mut self, n: NId) {
        let expr = self.ast.nodes[n].expr.clone();
        match expr {
            Expr::Var { name, resolved: None } => {
                if let Some(b) = self.lookup(&name) {
                    if let Expr::Var { resolved, .. } = &mut self.ast.nodes[n].expr {
                        *resolved = Some(b);
                    }
                } else if &name != "_" {
                    let span = self.ast.nodes[n].span;
                    self.ast.errors.push(ParseError {
                        span,
                        message: format!("未绑定的变量 “{}”", name),
                    });
                }
            }
            Expr::Fun { param, body } => {
                let name = self.ast.binders[param].name.clone();
                self.scope.push((name, param));
                self.resolve_node(body);
                self.scope.pop();
            }
            Expr::Let { rec, binder, init, next } => {
                if rec {
                    let name = self.ast.binders[binder].name.clone();
                    self.scope.push((name, binder));
                }
                self.resolve_node(init);
                if !rec {
                    let name = self.ast.binders[binder].name.clone();
                    self.scope.push((name, binder));
                }
                self.resolve_node(next);
                self.scope.pop();
            }
            Expr::Match { scrutinee, arms } => {
                self.resolve_node(scrutinee);
                let arm_data: Vec<(Option<(String, BindId)>, NId)> = arms
                    .iter()
                    .map(|a| (a.payload.as_ref().map(|(n, _, b)| (n.clone(), *b)), a.body))
                    .collect();
                for (payload, body) in arm_data {
                    let depth_before = self.scope.len();
                    if let Some((name, b)) = payload {
                        self.scope.push((name, b));
                    }
                    self.resolve_node(body);
                    self.scope.truncate(depth_before);
                }
            }
            other => {
                for (ch, _role) in children_of(&other) {
                    self.resolve_node(ch);
                }
            }
        }
    }
}

fn assign_keys(ast: &mut Ast) {
    // Assign binder keys first through a traversal, then node keys.
    let tops: Vec<(BindId, NId)> = ast.tops.iter().map(|t| (t.binder, t.init)).collect();
    for (idx, (binder, init)) in tops.iter().enumerate() {
        let bkey = format!("top{}:b{}", idx, binder);
        ast.binders[*binder].key = bkey;
        key_visit(ast, *init, &format!("top{}", idx));
    }
}

fn key_visit(ast: &mut Ast, n: NId, key: &str) {
    if !ast.nodes[n].key.is_empty() {
        return;
    }
    ast.nodes[n].key = key.to_string();
    let expr = ast.nodes[n].expr.clone();
    match &expr {
        Expr::Fun { param, body } => {
            ast.binders[*param].key = format!("{}:b{}", key, param);
            key_visit(ast, *body, &format!("{}/body", key));
        }
        Expr::Let { binder, init, next, .. } => {
            ast.binders[*binder].key = format!("{}:b{}", key, binder);
            key_visit(ast, *init, &format!("{}/i", key));
            key_visit(ast, *next, &format!("{}/n", key));
        }
        Expr::Match { scrutinee, arms } => {
            key_visit(ast, *scrutinee, &format!("{}/s", key));
            for (idx, a) in arms.iter().enumerate() {
                if let Some((_, _, b)) = a.payload {
                    ast.binders[b].key = format!("{}/arm{}:b{}", key, idx, b);
                }
                key_visit(ast, a.body, &format!("{}/arm{}", key, idx));
            }
        }
        Expr::Record { fields, extends } => {
            for (idx, f) in fields.iter().enumerate() {
                key_visit(ast, f.value, &format!("{}/f{}#{}", key, idx, f.label));
            }
            if let Some(e) = extends {
                key_visit(ast, *e, &format!("{}/ext", key));
            }
        }
        _ => {
            for (idx, (ch, role)) in children_of(&expr).iter().enumerate() {
                key_visit(ast, *ch, &format!("{}/{}{}", key, role, idx));
            }
        }
    }
}
