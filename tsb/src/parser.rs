//! Recursive-descent parser with Pratt expression parsing.

use crate::ast::*;
use crate::lexer::{lex, Tok, Token};
use crate::span::Span;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

pub fn parse(src: &str) -> Result<Ast, ParseError> {
    let tokens = lex(src).map_err(|e| ParseError { msg: e.msg, span: e.span })?;
    let mut p = Parser { toks: tokens, pos: 0, src, ast: Ast::default() };
    let root = p.parse_expr()?;
    p.expect(&Tok::Eof)?;
    p.ast.root = Some(root);
    p.assign_keys();
    Ok(p.ast)
}

struct Parser<'a> {
    toks: Vec<Token>,
    pos: usize,
    src: &'a str,
    ast: Ast,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn peek_span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos < self.toks.len() - 1 {
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
    fn expect(&mut self, t: &Tok) -> Result<Token, ParseError> {
        if self.peek() == t {
            Ok(self.bump())
        } else {
            Err(self.err(format!("expected {}, found {}", tok_name(t), tok_name(self.peek()))))
        }
    }
    fn err(&self, msg: String) -> ParseError {
        ParseError { msg, span: self.peek_span() }
    }

    fn push(&mut self, expr: Expr, span: Span) -> NodeId {
        let id = NodeId(self.ast.nodes.len());
        self.ast.nodes.push(Node { id, expr, span, key: String::new() });
        id
    }

    fn parse_lower_name(&mut self) -> Result<(String, Span), ParseError> {
        match self.bump().tok {
            Tok::Lower(s) => Ok((s, self.toks[self.pos.wrapping_sub(1)].span)),
            other => Err(ParseError {
                msg: format!("expected identifier, found {}", tok_name(&other)),
                span: self.peek_span(),
            }),
        }
    }
}
