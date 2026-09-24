pub mod lex;
pub mod expr;
pub mod atom;

use crate::ast::*;
use lex::{Tok, Token};

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
    pub nodes: Vec<NodeInfo>,
    pub errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(toks: Vec<Token>) -> Self {
        Parser { toks, idx: 0, nodes: Vec::new(), errors: Vec::new() }
    }

    fn cur(&self) -> &Token {
        &self.toks[self.idx]
    }
    fn peek_tok(&self, ahead: usize) -> &Tok {
        &self.toks[(self.idx + ahead).min(self.toks.len() - 1)].tok
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.idx].clone();
        if self.idx + 1 < self.toks.len() {
            self.idx += 1;
        }
        t
    }
    fn eat(&mut self, want: &Tok) -> bool {
        if std::mem::discriminant(&self.cur().tok) == std::mem::discriminant(want) {
            // loose discriminant compare; do exact below
            if self.is_(want) {
                self.bump();
                return true;
            }
        }
        false
    }
    fn is_(&self, want: &Tok) -> bool {
        match (&self.cur().tok, want) {
            (Tok::Lower(a), Tok::Lower(b)) => a == b,
            (Tok::Upper(a), Tok::Upper(b)) => a == b,
            (Tok::Int(_), Tok::Int(_)) => true,
            (Tok::Str(_), Tok::Str(_)) => true,
            (a, b) => std::mem::discriminant(a) == std::mem::discriminant(b),
        }
    }
    fn expect(&mut self, want: Tok, what: &str) -> Option<Token> {
        if self.is_(&want) {
            Some(self.bump())
        } else {
            let span = self.cur().span;
            self.errors.push(ParseError { span, message: format!("expected {}", what) });
            None
        }
    }

    fn span_from(&self, start: Span) -> Span {
        let end = if self.idx == 0 {
            start
        } else {
            self.toks[self.idx.saturating_sub(1)].span
        };
        Span::new(start.lo as usize, end.hi as usize)
    }

    fn mk(&mut self, id: NodeId, span: Span, kind: &str, label: Option<String>) {
        self.nodes.push(NodeInfo { id, span, kind: kind.to_string(), label });
    }

    fn err(&mut self, span: Span, msg: impl Into<String>) {
        self.errors.push(ParseError { span, message: msg.into() });
    }

    fn sync(&mut self) {
        // Recover at a boundary token.
        loop {
            match self.cur().tok {
                Tok::Eof | Tok::Let => return,
                Tok::Pipe | Tok::Comma | Tok::RParen | Tok::RBrace | Tok::RBracket => return,
                _ => self.bump(),
            }
        }
    }
}

pub fn parse(source: &str) -> Program {
    let (toks, lex_errs) = lex::Lexer::new(source).tokenize();
    let mut p = Parser::new(toks);
    let root_id = NodeId(seed_str("program-root"));
    let root = p.parse_expr(root_id, Edge::Child(0));
    if !p.is_(&Tok::Eof) {
        p.err(p.cur().span, "unexpected trailing tokens");
    }
    for (span, message) in lex_errs {
        p.errors.push(ParseError { span, message });
    }
    p.nodes.sort_by_key(|n| (n.span.lo, n.id));
    Program { root, nodes: p.nodes, source: source.to_string(), parse_errors: p.errors }
}
