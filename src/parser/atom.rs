use crate::ast::*;
use crate::parser::Parser;
use lex::Tok;

impl Parser {
    pub(crate) fn parse_atom(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        let tok = self.cur().tok.clone();
        match tok {
            Tok::Int(v) => {
                let t = self.bump();
                let id = parent.child(edge);
                self.mk(id, t.span, "lit-int", None);
                Spanned { node: id, span: t.span, inner: Expr::Lit(Lit::Int(v)) }
            }
            Tok::Str(s) => {
                let t = self.bump();
                let id = parent.child(edge);
                self.mk(id, t.span, "lit-str", None);
                Spanned { node: id, span: t.span, inner: Expr::Lit(Lit::Str(s)) }
            }
            Tok::True | Tok::False => {
                let t = self.bump();
                let id = parent.child(edge);
                self.mk(id, t.span, "lit-int", None);
                Spanned { node: id, span: t.span, inner: Expr::Lit(Lit::Int(1)) }
            }
            Tok::LParen => {
                let start = self.bump();
                if self.is_(&Tok::RParen) {
                    let end = self.bump().span;
                    let id = parent.child(edge);
                    let span = Span::new(start.span.lo as usize, end.hi as usize);
                    self.mk(id, span, "lit-unit", None);
                    return Spanned { node: id, span, inner: Expr::Lit(Lit::Unit) };
                }
                let e = self.parse_expr(parent, edge);
                self.expect(Tok::RParen, ")");
                e
            }
            Tok::LBrace => self.parse_record(parent, edge),
            Tok::LBracket | Tok::Upper(_) => self.parse_variant(parent, edge),
            Tok::Lower(name) => {
                let t = self.bump();
                let id = parent.child(edge);
                self.mk(id, t.span, "var", Some(name.clone()));
                Spanned { node: id, span: t.span, inner: Expr::Var(name) }
            }
            _ => {
                let span = self.cur().span;
                self.err(span, "expected an expression");
                let id = parent.child(edge);
                self.mk(id, span, "error", None);
                self.sync();
                Spanned { node: id, span, inner: Expr::Lit(Lit::Unit) }
            }
        }
    }

    fn parse_record(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        let start = self.bump().span;
        let id = parent.child(edge);
        let mut fields = Vec::new();
        if !self.is_(&Tok::RBrace) {
            loop {
                let saved = self.idx;
                let label_tok = self.cur().clone();
                let label = match &label_tok.tok {
                    Tok::Lower(s) | Tok::Upper(s) => s.clone(),
                    _ => {
                        self.err(label_tok.span, "expected field label");
                        self.sync();
                        break;
                    }
                };
                self.bump();
                let _ = saved;
                self.expect(Tok::Equals, "=");
                let value = self.parse_expr(id, Edge::Keyed(keyed_label(&label)));
                self.mk_ident(
                    id,
                    Edge::Keyed(keyed_label(&format!("label@{}", label))),
                    label_tok.span,
                    "field-label",
                    label.clone(),
                );
                fields.push(RecordField { name: label, name_span: label_tok.span, value });
                if self.is_(&Tok::Comma) {
                    self.bump();
                    if self.is_(&Tok::RBrace) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        let end = self.expect(Tok::RBrace, "}").map(|t| t.span).unwrap_or_else(|| self.cur().span);
        let span = Span::new(start.lo as usize, end.hi as usize);
        self.mk(id, span, "record", None);
        Spanned { node: id, span, inner: Expr::Record(fields) }
    }

    fn parse_variant(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        let start_tok = self.bump();
        let (tag, tag_span) = if let Tok::Upper(s) = &start_tok.tok {
            (s.clone(), start_tok.span)
        } else {
            // bracket form: Tag payload? we parse [ Tag payload ]
            let t = self.cur().clone();
            if let Tok::Upper(s) = &t.tok {
                let s = s.clone();
                self.bump();
                (s, t.span)
            } else {
                self.err(t.span, "expected variant tag");
                ("_".to_string(), t.span)
            }
        };
        let id = parent.child(edge);
        self.mk_ident(
            id,
            Edge::Keyed(keyed_label(&format!("tag@{}", tag))),
            tag_span,
            "variant-tag",
            tag.clone(),
        );
        let payload = if self.starts_atom() {
            Some(Box::new(self.parse_atom(id, Edge::Child(0))))
        } else {
            None
        };
        let mut end = tag_span;
        if start_tok.tok == Tok::LBracket {
            end = self.expect(Tok::RBracket, "]").map(|t| t.span).unwrap_or(end);
        }
        let span = Span::new(start_tok.span.lo as usize, end.hi as usize);
        self.mk(id, span, "variant", Some(tag.clone()));
        Spanned { node: id, span, inner: Expr::Variant { tag, tag_span, payload } }
    }

    fn parse_match(&mut self, parent: NodeId, edge: Edge) -> Spanned<Expr> {
        let start = self.bump().span;
        let id = parent.child(edge);
        let scrut = self.parse_expr(id, Edge::Child(0));
        self.expect(Tok::With, "with");
        let mut arms = Vec::new();
        loop {
            if !self.is_(&Tok::Pipe) {
                if arms.is_empty() {
                    self.err(self.cur().span, "expected at least one '|' arm");
                }
                break;
            }
            self.bump();
            let wildcard = self.is_(&Tok::Underscore);
            let (tag, tag_span) = if wildcard {
                let t = self.bump();
                ("_".to_string(), t.span)
            } else if let Tok::Upper(s) = &self.cur().tok {
                let s = s.clone();
                let t = self.bump();
                (s, t.span)
            } else {
                self.err(self.cur().span, "expected variant tag or '_'");
                self.sync();
                continue;
            };
            let arm_id = id.child(Edge::Keyed(keyed_label(&format!("arm@{}", tag))));
            self.mk_ident(
                arm_id,
                Edge::Child(0),
                tag_span,
                "match-tag",
                tag.clone(),
            );
            let (binding, binding_span) = match &self.cur().tok {
                Tok::Lower(s) => {
                    let s = s.clone();
                    let t = self.bump();
                    (Some(s), Some(t.span))
                }
                _ => (None, None),
            };
            self.expect(Tok::FatArrow, "=>");
            let body = self.parse_expr(arm_id, Edge::Child(1));
            arms.push(MatchArm { tag, tag_span, wildcard, binding, binding_span, body });
            if !self.is_(&Tok::Pipe) {
                break;
            }
        }
        let span = self.span_from(start);
        self.mk(id, span, "match", None);
        Spanned { node: id, span, inner: Expr::Match { scrut: Box::new(scrut), arms } }
    }
}
use crate::parser::lex::Tok as _Tok;
