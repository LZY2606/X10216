use crate::ast::*;
use crate::lexer::{lex, Tok, Token};
use crate::span::Span;

pub struct Parser {
    toks: Vec<Token>,
    pos: usize,
    next_id: u32,
}

type PResult<T> = Result<T, (Span, String)>;

impl Parser {
    pub fn parse(src: &str) -> Result<Node<Program>, (Span, String)> {
        let toks = lex(src)?;
        let mut p = Parser { toks, pos: 0, next_id: 0 };
        p.program()
    }

    fn nid(&mut self) -> NId {
        let id = NId(self.next_id);
        self.next_id += 1;
        id
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> PResult<Token> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            Ok(self.bump())
        } else {
            Err((self.span(), format!("期望 {}", what)))
        }
    }

    fn node<N>(&mut self, span: Span, node: N) -> Node<N> {
        Node { nid: self.nid(), span, node }
    }

    fn program(&mut self) -> PResult<Node<Program>> {
        let lo = self.span();
        let mut items = Vec::new();
        loop {
            if matches!(self.peek(), Tok::Eof) {
                break;
            }
            items.push(self.item()?);
            if matches!(self.peek(), Tok::Eof) {
                break;
            }
            self.expect(Tok::Semi, "`;` 分隔顶层声明")?;
        }
        let hi = self.toks[self.pos.saturating_sub(1)].span;
        Ok(self.node(Span::merge(lo, hi), Program::Program(items)))
    }

    fn item(&mut self) -> PResult<Node<Item>> {
        let lo = self.span();
        if matches!(self.peek(), Tok::Let) {
            let (rec, pat, body) = self.let_binding()?;
            let hi = body.span;
            Ok(self.node(Span::merge(lo, hi), Item::Let { rec, pat, body }))
        } else {
            let e = self.expr()?;
            let hi = e.span;
            Ok(self.node(Span::merge(lo, hi), Item::Expr(e)))
        }
    }

    fn let_binding(&mut self) -> PResult<(bool, Node<Pat>, Node<Expr>)> {
        self.expect(Tok::Let, "`let`")?;
        let rec = self.eat(&Tok::Rec);
        let pat = self.pat()?;
        self.expect(Tok::Eq, "`=`")?;
        let body = self.expr()?;
        Ok((rec, pat, body))
    }

    fn pat(&mut self) -> PResult<Node<Pat>> {
        let lo = self.span();
        match self.peek().clone() {
            Tok::Underscore => {
                let t = self.bump();
                Ok(self.node(t.span, Pat::Wild))
            }
            Tok::Ident(n) => {
                let t = self.bump();
                Ok(self.node(t.span, Pat::Var(n)))
            }
            Tok::Int(v) => {
                let t = self.bump();
                Ok(self.node(t.span, Pat::Lit(Lit::Int(v))))
            }
            Tok::True => {
                let t = self.bump();
                Ok(self.node(t.span, Pat::Lit(Lit::Bool(true))))
            }
            Tok::False => {
                let t = self.bump();
                Ok(self.node(t.span, Pat::Lit(Lit::Bool(false))))
            }
            Tok::Str(_) => {
                let t = self.bump();
                if let Tok::Str(s) = t.tok {
                    Ok(self.node(t.span, Pat::Lit(Lit::Str(s))))
                } else {
                    unreachable!()
                }
            }
            Tok::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                if !matches!(self.peek(), Tok::RBrace) {
                    loop {
                        let name_t = self.expect_ident("记录字段名")?;
                        self.expect(Tok::Eq, "字段后 `=`")?;
                        let v = self.pat()?;
                        fields.push((name_t.tok_name(), v));
                        if !self.eat(&Tok::Comma) {
                            break;
                        }
                    }
                }
                let rb = self.expect(Tok::RBrace, "`}`")?;
                Ok(self.node(Span::merge(lo, rb.span), Pat::Record(fields)))
            }
            Tok::Tag(name) => {
                let t = self.bump();
                let payload = if matches!(self.peek(), Tok::LParen)
                    || matches!(self.peek(), Tok::LBrace)
                    || matches!(self.peek(), Tok::Tag(_))
                    || matches!(self.peek(), Tok::Ident(_))
                    || matches!(self.peek(), Tok::Underscore)
                    || matches!(self.peek(), Tok::Int(_))
                    || matches!(self.peek(), Tok::True)
                    || matches!(self.peek(), Tok::False)
                    || matches!(self.peek(), Tok::Str(_))
                {
                    Some(Box::new(self.pat()?))
                } else {
                    None
                };
                let hi = payload.as_ref().map(|p| p.span).unwrap_or(t.span);
                Ok(self.node(Span::merge(lo, hi), Pat::Variant(name, payload)))
            }
            _ => Err((self.span(), "期望一个模式（变量、通配符、字面量、记录或变体）".into())),
        }
    }

    fn expect_ident(&mut self, what: &str) -> PResult<NamedTok> {
        if let Tok::Ident(n) = self.peek().clone() {
            let t = self.bump();
            if let Tok::Ident(n) = t.tok {
                return Ok(NamedTok { span: t.span, name: n });
            }
        }
        Err((self.span(), format!("期望 {}", what)))
    }
