use crate::ast::*;
use crate::lex::{lex, Tok, Token};
use std::collections::HashMap;

#[derive(Clone, serde::Serialize)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

struct RawNode {
    span: Span,
    expr: Expr,
}

struct P {
    toks: Vec<Token>,
    pos: usize,
    raws: Vec<RawNode>,
    scopes: Vec<HashMap<String, u32>>,
    depth: u32,
    builtins: &'static [&'static str],
}

const BUILTINS: &[&str] = &[
    "print", "add", "sub", "mul", "div", "eq", "lt", "gt", "and", "or",
    "intToString", "stringLength", "not",
];

pub fn parse(src: &str) -> Result<Ast, ParseError> {
    let toks = lex(src).map_err(|e| ParseError {
        msg: e.msg,
        span: Span(e.at, e.at + 1),
    })?;
    let mut p = P {
        toks,
        pos: 0,
        raws: Vec::new(),
        scopes: vec![HashMap::new()],
        depth: 0,
        builtins: BUILTINS,
    };
    let mut items = Vec::new();
    if p.peek().is_some() {
        loop {
            let node = p.parse_top_let()?;
            items.push(node);
            if p.peek().is_none() {
                break;
            }
        }
    }
    // fill parent + stable structural keys bottom-up
    let n = p.raws.len();
    let mut parents: Vec<Option<NodeId>> = vec![None; n];
    for (i, r) in p.raws.iter().enumerate() {
        for c in children_of(&r.expr) {
            parents[c as usize] = Some(i as NodeId);
        }
    }
    let mut keys = vec![0u64; n];
    for i in (0..n).rev() {
        keys[i] = key_of(&p.raws[i].expr, &keys);
    }
    let nodes = p
        .raws
        .into_iter()
        .enumerate()
        .map(|(i, r)| Node {
            id: i as NodeId,
            span: r.span,
            parent: parents[i],
            key: keys[i],
            expr: r.expr,
        })
        .collect();
    Ok(Ast {
        source: src.to_string(),
        nodes,
        program: Program { items },
    })
}

impl P {
    fn peek(&self) -> Option<&Tok> {
        self.toks.get(self.pos).map(|t| &t.tok)
    }
    fn at(&self) -> u32 {
        self.toks.get(self.pos).map(|t| t.start).unwrap_or(u32::MAX)
    }
    fn span_of(&self, idx: usize) -> Span {
        Span(self.toks[idx].start, self.toks[idx].end)
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        self.pos += 1;
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.peek() == Some(t) {
            self.pos += 1;
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.peek() == Some(&t) {
            Ok(self.bump())
        } else {
            let at = self.at();
            Err(ParseError {
                msg: format!("expected {what}"),
                span: Span(at, at + 1),
            })
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.depth += 1;
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.depth -= 1;
    }
    fn bind(&mut self, name: &str) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), self.depth);
    }
    fn resolve(&self, name: &str) -> Option<(u32, bool)> {
        for s in self.scopes.iter().rev() {
            if let Some(d) = s.get(name) {
                return Some((self.depth - d, false));
            }
        }
        if self.builtins.contains(&name) {
            Some((u32::MAX, true))
        } else {
            None
        }
    }

    fn mk(&mut self, span: Span, expr: Expr) -> NodeId {
        let id = self.raws.len() as NodeId;
        self.raws.push(RawNode { span, expr });
        id
    }

    fn parse_top_let(&mut self) -> Result<NodeId, ParseError> {
        let start = self.at();
        self.expect(Tok::Let, "`let`")?;
        let rec = self.eat(&Tok::Rec);
        let name_tok = self.expect(Tok::Lower(String::new()), "binding name")?.clone();
        let name = if let Tok::Lower(n) = name_tok.tok {
            n
        } else {
            unreachable!()
        };
        let annot = if self.eat(&Tok::Colon) {
            Some(self.parse_ty()?)
        } else {
            None
        };
        self.expect(Tok::Equals, "`=`")?;
        // top-level bindings live in a shared scope (sequential; recursion allowed if rec)
        let value = if rec {
            self.bind(&name);
            let v = self.parse_expr()?;
            self.pop_scope();
            // rebind at outer (we added an extra scope above; keep simple: re-bind at current top)
            self.bind(&name);
            v
        } else {
            let v = self.parse_expr()?;
            self.bind(&name);
            v
        };
        let end = self.at();
        let _ = end;
        let span = Span(start, self.prev_end());
        // body is synthetic "the rest of program": represented via items at call site;
        // here top-level let is modeled as a Let whose body is an empty Seq marker
        let body = self.mk(Span(u32::MAX, u32::MAX), Expr::Seq { exprs: vec![] });
        Ok(self.mk(
            span,
            Expr::Let {
                rec,
                name,
                annot,
                value,
                body,
            },
        ))
    }

    fn prev_end(&self) -> u32 {
        self.toks
            .get(self.pos.saturating_sub(1))
            .map(|t| t.end)
            .unwrap_or(0)
    }
