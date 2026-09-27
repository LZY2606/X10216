use crate::ast::{Arm, AstNode, Expr};
use crate::lexer::{Lexer, Tok, Token};
use crate::span::{NodeId, Span};

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn parse(src: &str) -> Result<AstNode, ParseError> {
        let toks = Lexer::new(src).tokenize().map_err(|e| ParseError {
            span: e.span,
            msg: e.msg,
        })?;
        let mut p = Parser { toks, idx: 0 };
        let node = p.parse_seq(&NodeId("0".to_string()))?;
        if !matches!(p.peek().tok, Tok::Eof) {
            return Err(p.err(p.peek().span, "期望表达式结束"));
        }
        Ok(node)
    }

    fn err(&self, span: Span, msg: impl Into<String>) -> ParseError {
        ParseError { span, msg: msg.into() }
    }

    fn peek(&self) -> &Token {
        &self.toks[self.idx]
    }
    fn peek2(&self) -> Token {
        self.toks.get(self.idx + 1).cloned().unwrap_or_else(|| self.toks[self.idx].clone())
    }
    fn advance(&mut self) -> Token {
        let t = self.toks[self.idx].clone();
        if self.idx + 1 < self.toks.len() {
            self.idx += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(t) {
            self.advance();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(&t) {
            Ok(self.advance())
        } else {
            Err(self.err(self.peek().span, format!("期望 {what}")))
        }
    }

    fn mk(&self, id: NodeId, lo: usize, hi: usize, expr: Expr, children: Vec<AstNode>) -> AstNode {
        AstNode { id, span: Span::new(lo, hi), expr, children }
    }

    // seq: atom (';' atom)* ; let ... in handled inside
    fn parse_seq(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        let first = self.parse_let(id)?;
        if self.eat(&Tok::Semicolon) {
            let second = self.parse_seq(&id.child(2))?;
            let span = Span::new(first.span.lo, second.span.hi);
            return Ok(self.mk(id.clone(), span.lo, span.hi,
                Expr::Seq { first: Box::new(strip(first.clone())), second: Box::new(strip(second.clone())) },
                vec![first, second]));
        }
        Ok(first)
    }

    fn parse_let(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        if !matches!(self.peek().tok, Tok::Let) {
            return self.parse_if(id);
        }
        let lo = self.peek().span.lo;
        self.advance();
        let rec = self.eat(&Tok::Rec);
        let name_tok = self.expect(Tok::Ident("".to_string()), "变量名")?;
        let name = ident_str(name_tok.tok.clone());
        self.expect(Tok::Eq, "'='")?;
        let value = self.parse_seq(&id.child(0))?;
        let (body, hi) = if self.eat(&Tok::In) {
            let b = self.parse_seq(&id.child(1))?;
            let h = b.span.hi;
            (Some(b), h)
        } else {
            (None, value.span.hi)
        };
        let expr = Expr::Let {
            name,
            rec,
            value: Box::new(strip(value.clone())),
            body: body.clone().map(strip).map(Box::new),
        };
        let mut children = vec![value];
        if let Some(b) = body {
            children.push(b);
        }
        Ok(self.mk(id.clone(), lo, hi, expr, children))
    }

    fn parse_if(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        if !matches!(self.peek().tok, Tok::If) {
            return self.parse_fn(id);
        }
        let lo = self.advance().span.lo;
        let cond = self.parse_seq(&id.child(0))?;
        self.expect(Tok::Then, "'then'")?;
        let then_e = self.parse_seq(&id.child(1))?;
        self.expect(Tok::Else, "'else'")?;
        let else_e = self.parse_seq(&id.child(2))?;
        let hi = else_e.span.hi;
        let expr = Expr::If {
            cond: Box::new(strip(cond.clone())),
            then_e: Box::new(strip(then_e.clone())),
            else_e: Box::new(strip(else_e.clone())),
        };
        Ok(self.mk(id.clone(), lo, hi, expr, vec![cond, then_e, else_e]))
    }

    fn parse_fn(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        if !matches!(self.peek().tok, Tok::Fn) && !matches!(self.peek().tok, Tok::Backslash) {
            return self.parse_arrow(id);
        }
        let lo = self.advance().span.lo;
        let param_tok = self.expect(Tok::Ident("".to_string()), "参数名")?;
        let param = ident_str(param_tok.tok.clone());
        if !matches!(self.peek().tok, Tok::Arrow) && !matches!(self.peek().tok, Tok::FatArrow) {
            return Err(self.err(self.peek().span, "期望 '->' 或 '=>'"));
        }
        self.advance();
        let body = self.parse_seq(&id.child(0))?;
        let hi = body.span.hi;
        let expr = Expr::Lam { param, body: Box::new(strip(body.clone())) };
        Ok(self.mk(id.clone(), lo, hi, expr, vec![body]))
    }

    fn parse_arrow(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        // only `match` introduces special syntax here; arrows live in fn
        if matches!(self.peek().tok, Tok::Match) {
            return self.parse_match(id);
        }
        self.parse_app(id)
    }

    fn parse_app(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        let mut fun = self.parse_postfix(&id.child(0))?;
        loop {
            if starts_atom(&self.peek().tok) {
                let arg_id = app_arg_id(&fun.id);
                let arg = self.parse_postfix(&arg_id)?;
                let hi = arg.span.hi;
                let expr = Expr::App {
                    fun: Box::new(strip(fun.clone())),
                    arg: Box::new(strip(arg.clone())),
                };
                let fid = fun.id.clone();
                fun = self.mk(fid.clone(), fun.span.lo, hi, expr, vec![fun, arg]);
            } else {
                break;
            }
        }
        Ok(fun)
    }

    fn parse_postfix(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        let mut node = self.parse_atom(id)?;
        while matches!(self.peek().tok, Tok::Dot) {
            let dot = self.advance();
            let name = match self.peek().tok.clone() {
                Tok::Ident(s) => s,
                Tok::Backtick(s) => s,
                _ => return Err(self.err(dot.span, "'.' 后期望字段名")),
            };
            self.advance();
            let hi = self.toks[self.idx.saturating_sub(1)].span.hi;
            let nid = node.id.clone();
            let expr = Expr::Field { record: Box::new(strip(node.clone())), name };
            node = self.mk(nid, node.span.lo, hi, expr, vec![node]);
        }
        Ok(node)
    }

    fn parse_match(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        let lo = self.advance().span.lo; // match
        let scrutinee = self.parse_postfix(&id.child(0))?;
        self.expect(Tok::With, "'with'")?;
        self.expect(Tok::LBrace, "'{'")?;
        let mut arms = Vec::new();
        let mut arm_children = Vec::new();
        let mut wildcard = None;
        let mut child_counter = 1;
        while !matches!(self.peek().tok, Tok::RBrace | Tok::Eof) {
            if matches!(self.peek().tok, Tok::Underscore) {
                self.advance();
                self.expect(Tok::FatArrow, "'=>'")?;
                let wc = self.parse_seq(&id.child(child_counter))?;
                wildcard = Some(wc.clone());
                arm_children.push(wc);
                child_counter += 1;
            } else {
                let tag_tok = self.expect(Tok::Upper("".to_string()), "变体构造子（大写开头）")?;
                let tag = upper_str(tag_tok.tok.clone());
                let binding = if let Tok::Ident(s) = &self.peek().tok {
                    let s = s.clone();
                    self.advance();
                    Some(s)
                } else {
                    None
                };
                self.expect(Tok::FatArrow, "'=>'")?;
                let body = self.parse_seq(&id.child(child_counter))?;
                arms.push(Arm { tag, binding, body: strip(body.clone()) });
                arm_children.push(body);
                child_counter += 1;
            }
            if !self.eat(&Tok::Comma) {
                break;
            }
        }
        let hi_tok = self.expect(Tok::RBrace, "'}'")?;
        let mut children = vec![scrutinee];
        children.extend(arm_children);
        let expr = Expr::Match {
            scrutinee: Box::new(strip(children[0].clone())),
            arms,
            wildcard: wildcard.map(strip).map(Box::new),
        };
        Ok(self.mk(id.clone(), lo, hi_tok.span.hi, expr, children))
    }

    fn parse_atom(&mut self, id: &NodeId) -> Result<AstNode, ParseError> {
        let lo = self.peek().span.lo;
        match self.peek().tok.clone() {
            Tok::Int(n) => {
                let hi = self.advance().span.hi;
                Ok(self.mk(id.clone(), lo, hi, Expr::Int { val: n, name: None }, vec![]))
            }
            Tok::Bool(b) => {
                let hi = self.advance().span.hi;
                Ok(self.mk(id.clone(), lo, hi, Expr::Bool { val: b }, vec![]))
            }
            Tok::Str(s) => {
                let hi = self.advance().span.hi;
                Ok(self.mk(id.clone(), lo, hi, Expr::Str { val: s }, vec![]))
            }
            Tok::Ident(name) => {
                let hi = self.advance().span.hi;
                Ok(self.mk(id.clone(), lo, hi, Expr::Var { name }, vec![]))
            }
            Tok::Upper(tag) => {
                self.advance();
                let mut children = vec![];
                if starts_atom(&self.peek().tok) {
                    let payload = self.parse_postfix(&id.child(0))?;
                    let hi = payload.span.hi;
                    let expr = Expr::Variant { tag, payload: Some(Box::new(strip(payload.clone()))) };
                    children.push(payload);
                    return Ok(self.mk(id.clone(), lo, hi, expr, children));
                }
                let hi = self.toks[self.idx.saturating_sub(1)].span.hi;
                Ok(self.mk(id.clone(), lo, hi, Expr::Variant { tag, payload: None }, vec![]))
            }
            Tok::LParen => {
                self.advance();
                let inner = self.parse_seq(id)?;
                let close = self.expect(Tok::RParen, "')'")?;
                let mut node = inner;
                node.span = Span::new(lo, close.span.hi);
                Ok(node)
            }
            Tok::LBrace => {
                self.advance();
                let mut fields: Vec<(String, Expr)> = Vec::new();
                let mut children = Vec::new();
                let mut i = 0;
                while !matches!(self.peek().tok, Tok::RBrace | Tok::Eof) {
                    let name = match self.peek().tok.clone() {
                        Tok::Ident(s) => s,
                        Tok::Backtick(s) => s,
                        _ => return Err(self.err(self.peek().span, "期望字段名")),
                    };
                    self.advance();
                    self.expect(Tok::Eq, "'='")?;
                    let val = self.parse_seq(&id.child(i))?;
                    fields.push((name, strip(val.clone())));
                    children.push(val);
                    i += 1;
                    if !self.eat(&Tok::Comma) {
                        break;
                    }
                }
                let rb = self.expect(Tok::RBrace, "'}'")?;
                Ok(self.mk(id.clone(), lo, rb.span.hi, Expr::RecordLit { fields }, children))
            }
            other => Err(self.err(self.peek().span, format!("无法作为表达式开头: {other:?}"))),
        }
    }
}

fn ident_str(t: Tok) -> String {
    match t {
        Tok::Ident(s) => s,
        _ => String::new(),
    }
}
fn upper_str(t: Tok) -> String {
    match t {
        Tok::Upper(s) => s,
        _ => String::new(),
    }
}

pub fn strip(n: AstNode) -> Expr {
    n.expr
}

fn starts_atom(t: &Tok) -> bool {
    matches!(
        t,
        Tok::Int(_)
            | Tok::Bool(_)
            | Tok::Str(_)
            | Tok::Ident(_)
            | Tok::Upper(_)
            | Tok::LParen
            | Tok::LBrace
            | Tok::Fn
            | Tok::Backslash
            | Tok::If
            | Tok::Match
            | Tok::Let
    )
}

// Stable identity for the argument subtree of an application chain.
fn app_arg_id(fun_id: &NodeId) -> NodeId {
    NodeId(format!("{}@arg", fun_id.0))
}
