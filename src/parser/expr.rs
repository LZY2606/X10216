use crate::ast::*;
use crate::parser::Parser;
use lex::Tok;

pub fn keyed_arg(n: usize) -> String {
    format!("arg@{}", n)
}
pub fn keyed_app(n: usize) -> String {
    format!("app@{}", n)
}

fn app_depth(e: &Spanned<Expr>) -> usize {
    let mut depth = 0;
    let mut cur = e;
    while let Expr::App { fun, .. } = &cur.inner {
        depth += 1;
        cur = fun;
    }
    depth
}

impl Parser {
    pub(crate) fn parse_expr(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        match &self.cur().tok {
            Tok::Fn => self.parse_fun(parent, edge),
            Tok::Let => self.parse_let(parent, edge),
            Tok::Match => self.parse_match(parent, edge),
            _ => self.parse_postfix(parent, edge),
        }
    }

    fn parse_fun(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        let start = self.bump().span;
        let id = parent.child(edge);
        let (param, param_span) = self
            .expect_ident("parameter name")
            .unwrap_or_else(|| (String::from("_"), start));
        self.expect(Tok::FatArrow, "=>");
        let body = self.parse_expr(id, Edge::Child(1));
        let span = self.span_from(start);
        self.mk(id, span, "fun", Some(param.clone()));
        self.mk_ident(id, Edge::Child(0), param_span, "param", param.clone());
        Spanned { node: id, span, inner: Expr::Fun { param, param_span, body: Box::new(body) } }
    }

    pub(crate) fn expect_ident(&mut self, what: &str) -> Option<(String, Span)> {
        let span = self.cur().span;
        match &self.cur().tok {
            Tok::Lower(s) | Tok::Upper(s) => {
                let s = s.clone();
                self.bump();
                Some((s, span))
            }
            Tok::Underscore => {
                self.bump();
                Some(("_".to_string(), span))
            }
            _ => {
                self.err(span, format!("expected {}", what));
                None
            }
        }
    }

    pub(crate) fn mk_ident(&mut self, parent: NodeId, edge: Edge, span: Span, kind: &str, name: String) {
        let id = parent.child(edge);
        self.mk(id, span, kind, Some(name));
    }

    fn parse_let(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        let start = self.bump().span;
        let id = parent.child(edge);
        let rec = if self.is_(&Tok::Rec) {
            self.bump();
            true
        } else {
            false
        };
        let (name, name_span) = self
            .expect_ident("binding name")
            .unwrap_or_else(|| (String::from("_"), start));
        self.expect(Tok::Equals, "=");
        let rhs = self.parse_expr(id, Edge::Child(0));
        let body = self.parse_expr(id, Edge::Child(1));
        let span = self.span_from(start);
        self.mk(id, span, "let", Some(name.clone()));
        self.mk_ident(id, Edge::Keyed(keyed_label("binder")), name_span, "binder", name.clone());
        Spanned {
            node: id,
            span,
            inner: Expr::Let {
                bind: LetBind { rec, name, name_span, rhs },
                body: Box::new(body),
            },
        }
    }

    fn parse_postfix(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        let mut expr = self.parse_atom(parent, edge);
        loop {
            if self.is_(&Tok::Dot) {
                let dot = self.bump().span;
                let start = expr.span;
                let id = expr.node.child(Edge::Keyed(keyed_label("field-expr")));
                let (label, label_span) = self
                    .expect_ident("field label")
                    .unwrap_or_else(("_".to_string(), dot));
                let span = Span::new(expr.span.lo as usize, label_span.hi as usize);
                self.mk(id, span, "field", Some(label.clone()));
                self.mk_ident(
                    id,
                    Edge::Keyed(keyed_label(&label)),
                    label_span,
                    "field-label",
                    label.clone(),
                );
                expr = Spanned {
                    node: id,
                    span,
                    inner: Expr::Field { object: Box::new(expr), label, label_span },
                };
                continue;
            }
            if self.starts_atom() {
                let depth = app_depth(&expr);
                let new_id = expr.node.child(Edge::Keyed(keyed_label(&keyed_app(depth))));
                let arg = self.parse_atom(new_id, Edge::Keyed(keyed_label(&keyed_arg(depth))));
                let span = Span::new(expr.span.lo as usize, arg.span.hi as usize);
                self.mk(new_id, span, "app", None);
                expr = Spanned {
                    node: new_id,
                    span,
                    inner: Expr::App { fun: Box::new(expr), arg: Box::new(arg) },
                };
                continue;
            }
            break;
        }
        expr
    }

    fn starts_atom(&self) -> bool {
        matches!(
            &self.cur().tok,
            Tok::Int(_)
                | Tok::Str(_)
                | Tok::Lower(_)
                | Tok::Upper(_)
                | Tok::True
                | Tok::False
                | Tok::LParen
                | Tok::LBrace
                | Tok::LBracket
        )
    }
}
