use super::ast::*;
use super::lexer::{lex, Tok, Token};

pub struct Parser {
    toks: Vec<Token>,
    pos: usize,
    pub nodes: Vec<Node>,
}

impl Parser {
    pub fn parse(src: &str) -> Result<(Vec<Node>, NodeId), ParseError> {
        let toks = lex(src).map_err(|(message, lo, hi)| ParseError { message, lo, hi })?;
        let mut p = Parser { toks, pos: 0, nodes: Vec::new() };
        let root = p.expr("root")?;
        if !matches!(p.peek().tok, Tok::Eof) {
            let t = p.peek().clone();
            return Err(ParseError { message: format!("unexpected token {:?}", t.tok), lo: t.lo, hi: t.hi });
        }
        Ok((p.nodes, root))
    }

    fn peek(&self) -> &Token {
        &self.toks[self.pos]
    }
    fn at(&self, t: &Tok) -> bool {
        std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(t)
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos < self.toks.len() - 1 {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: Tok) -> bool {
        if self.at(&t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.at(&t) {
            Ok(self.bump())
        } else {
            let tk = self.peek().clone();
            Err(ParseError { message: format!("expected {}, found {:?}", what, tk.tok), lo: tk.lo, hi: tk.hi })
        }
    }

    fn mk(&mut self, expr: Expr, span: Span, anchor: String) -> NodeId {
        let id = NodeId(self.nodes.len() as u32);
        self.nodes.push(Node { id, expr, span, anchor });
        id
    }

    fn ident(&mut self) -> Result<(String, Span), ParseError> {
        if let Tok::Ident(s) = &self.peek().tok {
            let t = self.bump();
            Ok((s.clone(), t.span()))
        } else {
            let tk = self.peek().clone();
            Err(ParseError { message: format!("expected identifier, found {:?}", tk.tok), lo: tk.lo, hi: tk.hi })
        }
    }

    fn expr(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        match self.peek().tok {
            Tok::Let => self.let_expr(anchor),
            Tok::Fun => self.fun_expr(anchor),
            Tok::If => self.if_expr(anchor),
            Tok::Match => self.match_expr(anchor),
            _ => self.rel(anchor),
        }
    }

    fn let_expr(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let lo = self.bump().lo;
        let rec = self.eat(Tok::Rec);
        let (name, name_span) = self.ident()?;
        self.expect(Tok::Eq, "'='")?;
        let init = self.expr(&format!("{}/i", anchor))?;
        self.expect(Tok::In, "'in'")?;
        let body = self.expr(&format!("{}/d", anchor))?;
        let hi = self.nodes[body.0 as usize].span.hi;
        Ok(self.mk(Expr::Let { rec, name, name_span, init, body }, Span::new(lo, hi), anchor.to_string()))
    }

    fn fun_expr(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let lo = self.bump().lo;
        let (param, param_span) = self.ident()?;
        self.expect(Tok::Arrow, "'->'")?;
        let body = self.expr(&format!("{}/b", anchor))?;
        let hi = self.nodes[body.0 as usize].span.hi;
        Ok(self.mk(Expr::Lam { param, param_span, body }, Span::new(lo, hi), anchor.to_string()))
    }

    fn if_expr(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let lo = self.bump().lo;
        let cond = self.rel(&format!("{}/c", anchor))?;
        self.expect(Tok::Then, "'then'")?;
        let then = self.expr(&format!("{}/t", anchor))?;
        self.expect(Tok::Else, "'else'")?;
        let els = self.expr(&format!("{}/e", anchor))?;
        // represented as: (if cond then then else els) == match on Bool; desugar to App of builtin?
        // We model directly with a constraint set: cond=Bool, result unifies then/els.
        let hi = self.nodes[els.0 as usize].span.hi;
        Ok(self.mk(Expr::Ite { cond, then_b: then, else_b: els }, Span::new(lo, hi), anchor.to_string()))
    }

    fn match_expr(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let lo = self.bump().lo;
        let scrut = self.rel(&format!("{}/s", anchor))?;
        self.expect(Tok::With, "'with'")?;
        let mut arms = Vec::new();
        self.eat(Tok::Pipe);
        loop {
            let pat = if self.eat(Tok::Underscore) {
                ArmPat::Wild
            } else {
                let (tag, _) = self.ident()?;
                let param = if let Tok::Ident(_) = self.peek().tok {
                    Some(self.ident()?.0)
                } else {
                    None
                };
                ArmPat::Tag { tag, param }
            };
            self.expect(Tok::Arrow, "'->'")?;
            let ai = arms.len();
            let body = self.expr(&format!("{}/a{}", anchor, ai))?;
            arms.push(Arm { pat, body });
            if !self.eat(Tok::Pipe) {
                break;
            }
        }
        let hi = self.nodes[arms.last().unwrap().body.0 as usize].span.hi;
        Ok(self.mk(Expr::Match { scrut, arms }, Span::new(lo, hi), anchor.to_string()))
    }

    fn rel(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let mut left = self.add(anchor)?;
        if self.eat(Tok::EqEq) {
            let rhs = self.add(&format!("{}/r", anchor))?;
            let span = Span::union(self.node_span(left), self.node_span(rhs));
            left = self.mk(Expr::Bin { op: "==".into(), lhs: left, rhs: rhs }, span, anchor.to_string());
        }
        Ok(left)
    }

    fn add(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let mut left = self.mul(&format!("{}/l0", anchor))?;
        let mut idx = 1;
        loop {
            let op = if self.eat(Tok::Plus) {
                "+"
            } else if self.eat(Tok::Minus) {
                "-"
            } else {
                break;
            };
            let rhs = self.mul(&format!("{}/r{}", anchor, idx - 1))?;
            let span = Span::union(self.node_span(left), self.node_span(rhs));
            // bin node keeps anchor with index
            left = self.mk(
                Expr::Bin { op: op.into(), lhs: left, rhs },
                span,
                format!("{}+{}", anchor, idx),
            );
            idx += 1;
        }
        Ok(left)
    }

    fn mul(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let mut left = self.app(&format!("{}/l0", anchor))?;
        let mut idx = 1;
        while self.eat(Tok::Star) {
            let rhs = self.app(&format!("{}/r{}", anchor, idx - 1))?;
            let span = Span::union(self.node_span(left), self.node_span(rhs));
            left = self.mk(Expr::Bin { op: "*".into(), lhs: left, rhs }, span, format!("{}*{}", anchor, idx));
            idx += 1;
        }
        Ok(left)
    }

    fn node_span(&self, id: NodeId) -> Span {
        self.nodes[id.0 as usize].span
    }

    fn app(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let mut func = self.postfix(&format!("{}/f", anchor))?;
        let mut i = 0;
        while self.starts_atom() {
            let arg = self.postfix(&format!("{}/x{}", anchor, i))?;
            let span = Span::union(self.node_span(func), self.node_span(arg));
            func = self.mk(Expr::App { fun: func, arg }, span, format!("{}@{}", anchor, i));
            i += 1;
        }
        Ok(func)
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek().tok,
            Tok::Int(_)
                | Tok::True | Tok::False
                | Tok::Ident(_)
                | Tok::Underscore
                | Tok::LParen
                | Tok::LBrace
                | Tok::LBracket
        )
    }

    fn postfix(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let mut e = self.atom(anchor)?;
        while self.eat(Tok::Dot) {
            let (name, _) = self.ident()?;
            let span = self.node_span(e);
            let span = Span::new(span.lo, self.toks[self.pos.saturating_sub(1)].hi);
            e = self.mk(Expr::Field { obj: e, name }, span, format!("{}.{}", anchor, name));
        }
        Ok(e)
    }

    fn atom(&mut self, anchor: &str) -> Result<NodeId, ParseError> {
        let t = self.peek().clone();
        match t.tok {
            Tok::Int(v) => {
                self.bump();
                Ok(self.mk(Expr::Int(v), t.span(), anchor.to_string()))
            }
            Tok::True => {
                self.bump();
                Ok(self.mk(Expr::Bool(true), t.span(), anchor.to_string()))
            }
            Tok::False => {
                self.bump();
                Ok(self.mk(Expr::Bool(false), t.span(), anchor.to_string()))
            }
            Tok::Ident(name) => {
                self.bump();
                Ok(self.mk(Expr::Var(name), t.span(), anchor.to_string()))
            }
            Tok::Underscore => {
                self.bump();
                Ok(self.mk(Expr::Var("_".into()), t.span(), anchor.to_string()))
            }
            Tok::LParen => {
                self.bump();
                if self.eat(Tok::RParen) {
                    return Ok(self.mk(Expr::Unit, Span::new(t.lo, self.toks[self.pos.saturating_sub(1)].hi), anchor.to_string()));
                }
                let inner = self.expr(anchor)?;
                let r = self.expect(Tok::RParen, "')'")?;
                let span = Span::new(t.lo, r.hi);
                let expr = self.nodes[inner.0 as usize].expr.clone();
                Ok(self.mk(expr, span, anchor.to_string() + ")"))
            }
            Tok::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                if !self.at(&Tok::RBrace) {
                    loop {
                        let (name, _) = self.ident()?;
                        self.expect(Tok::Eq, "'='")?;
                        let fi = fields.len();
                        let value = self.expr(&format!("{}/f{}", anchor, fi))?;
                        fields.push(Field { name, value });
                        if !self.eat(Tok::Comma) {
                            break;
                        }
                    }
                }
                let r = self.expect(Tok::RBrace, "'}'")?;
                Ok(self.mk(Expr::Record(fields), Span::new(t.lo, r.hi), anchor.to_string()))
            }
            Tok::LBracket => {
                self.bump();
                let (tag, _) = self.ident()?;
                let payload = if self.starts_atom() {
                    Some(self.postfix(&format!("{}/p", anchor))?)
                } else {
                    None
                };
                let r = self.expect(Tok::RBracket, "']'")?;
                Ok(self.mk(Expr::Variant { tag, payload }, Span::new(t.lo, r.hi), anchor.to_string()))
            }
            _ => Err(ParseError { message: format!("expected expression, found {:?}", t.tok), lo: t.lo, hi: t.hi }),
        }
    }
}
