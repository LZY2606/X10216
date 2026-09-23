use serde::Serialize;

use crate::lexer::{Lexer, Tok, Token};
use crate::span::Span;

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AstNodeData {
    LitInt { value: i64 },
    LitBool { value: bool },
    LitUnit,
    Var { name: String },
    Lam { param: String, body_path: String },
    App { fun_path: String, arg_path: String },
    Let {
        name: String,
        rec: bool,
        bind_path: String,
        body_path: String,
    },
    Record { fields: Vec<FieldInit> },
    Proj { record_path: String, label: String },
    Variant { tag: String, payload_path: Option<String> },
    Match { scrut_path: String, arms: Vec<ArmData> },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct FieldInit {
    pub label: String,
    pub value_path: String,
    #[serde(rename = "span")]
    pub label_span: Span,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct ArmData {
    pub tag: String,
    pub binder: Option<String>,
    pub expr_path: String,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct AstNode {
    pub path: String,
    pub span: Span,
    #[serde(flatten)]
    pub data: AstNodeData,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

// ---------- raw tree built while parsing ----------

enum Raw {
    LitInt(i64, Span),
    LitBool(bool, Span),
    LitUnit(Span),
    Var(String, Span),
    Lam(String, Box<Raw>, Span),
    App(Box<Raw>, Box<Raw>, Span),
    Let(String, bool, Box<Raw>, Box<Raw>, Span),
    Record(Vec<(String, Span, Box<Raw>)>, Span),
    Proj(Box<Raw>, String, Span),
    Variant(String, Option<Box<Raw>>, Span),
    Match(Box<Raw>, Vec<(String, Option<String>, Span, Box<Raw>)>, Span),
}

impl Raw {
    fn span(&self) -> Span {
        match self {
            Raw::LitInt(_, s)
            | Raw::LitBool(_, s)
            | Raw::LitUnit(s)
            | Raw::Var(_, s)
            | Raw::Lam(_, _, s)
            | Raw::App(_, _, s)
            | Raw::Let(_, _, _, _, s)
            | Raw::Record(_, s)
            | Raw::Proj(_, _, s)
            | Raw::Variant(_, _, s)
            | Raw::Match(_, _, s) => *s,
        }
    }
}

struct Parser {
    toks: Vec<Token>,
    idx: usize,
}

impl Parser {
    fn peek(&self) -> &Tok {
        &self.toks[self.idx].tok
    }
    fn peek_span(&self) -> Span {
        self.toks[self.idx].span
    }
    fn at(&self, t: &Tok) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(t)
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.idx].clone();
        if self.idx + 1 < self.toks.len() {
            self.idx += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.at(t) {
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
            Err(ParseError {
                msg: format!("expected {}, found {}", what, tok_name(self.peek())),
                span: self.peek_span(),
            })
        }
    }

    // atom := int | bool | unit | lower | '(' expr ')'
    //       | record '{' field,.. '}' | tag [atom]
    fn parse_atom(&mut self) -> Result<Raw, ParseError> {
        let tok = self.bump();
        let raw = match tok.tok {
            Tok::Int(n) => Raw::LitInt(n, tok.span),
            Tok::True => Raw::LitBool(true, tok.span),
            Tok::False => Raw::LitBool(false, tok.span),
            Tok::Unit => Raw::LitUnit(tok.span),
            Tok::Lower(name) => Raw::Var(name, tok.span),
            Tok::LParen => {
                let e = self.parse_expr()?;
                self.expect(Tok::RParen, "')'")?;
                return Ok(e);
            }
            Tok::Record => {
                self.expect(Tok::LBrace, "'{'")?;
                let mut fields = Vec::new();
                if !self.at(&Tok::RBrace) {
                    loop {
                        let lab = self.expect_label()?;
                        let eq = self.expect(Tok::Eq, "'='")?;
                        let _ = eq;
                        let val = self.parse_expr()?;
                        fields.push((lab.0, lab.1, Box::new(val)));
                        if !self.eat(&Tok::Comma) {
                            break;
                        }
                    }
                }
                let end = self.expect(Tok::RBrace, "'}'")?;
                Raw::Record(fields, Span::merge(tok.span, end.span))
            }
            Tok::Upper(tag) => {
                if matches!(self.peek(), Tok::Int(_) | Tok::True | Tok::False | Tok::Unit
                    | Tok::Lower(_) | Tok::LParen | Tok::Record | Tok::Upper(_))
                {
                    let payload = self.parse_atom()?;
                    Raw::Variant(tag, Some(Box::new(payload)), Span::merge(tok.span, payload.span()))
                } else {
                    Raw::Variant(tag, None, tok.span)
                }
            }
            Tok::Fun => {
                let name_tok = self.expect(Tok::Lower(String::new()), "parameter name")?;
                let name = match name_tok.tok {
                    Tok::Lower(n) => n,
                    _ => unreachable!(),
                };
                self.expect(Tok::DArrow, "'->'")?;
                let body = self.parse_expr()?;
                return Ok(Raw::Lam(name, Box::new(body), Span::merge(tok.span, body.span())));
            }
            Tok::Let => return self.parse_let(tok.span),
            Tok::Match => return self.parse_match(tok.span),
            Tok::LBracket => {
                return Err(ParseError {
                    msg: "'[' is reserved for row extension annotations".into(),
                    span: tok.span,
                });
            }
            other => {
                return Err(ParseError {
                    msg: format!("unexpected token {}", tok_name(&other)),
                    span: tok.span,
                });
            }
        };
        Ok(raw)
    }

    fn expect_label(&mut self) -> Result<(String, Span), ParseError> {
        if let Tok::Label(l) = self.peek().clone() {
            let t = self.bump();
            Ok((l, t.span))
        } else if let Tok::Lower(l) = self.peek().clone() {
            let t = self.bump();
            Ok((l, t.span))
        } else {
            Err(ParseError {
                msg: format!("expected field label, found {}", tok_name(self.peek())),
                span: self.peek_span(),
            })
        }
    }

    fn parse_let(&mut self, start: Span) -> Result<Raw, ParseError> {
        let rec = self.eat(&Tok::Rec);
        let name_tok = self.expect(Tok::Lower(String::new()), "binding name")?;
        let name = match name_tok.tok {
            Tok::Lower(n) => n,
            _ => unreachable!(),
        };
        self.expect(Tok::Eq, "'='")?;
        let bind = self.parse_expr()?;
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr()?;
        Ok(Raw::Let(name, rec, Box::new(bind), Box::new(body), Span::merge(start, body.span())))
    }

    fn parse_match(&mut self, start: Span) -> Result<Raw, ParseError> {
        let scrut = self.parse_atom()?;
        self.expect(Tok::With, "'with'")?;
        let mut arms = Vec::new();
        loop {
            self.eat(&Tok::Pipe);
            let tag_tok = self.expect(Tok::Upper(String::new()), "variant tag")?;
            let tag = match tag_tok.tok {
                Tok::Upper(t) => t,
                _ => unreachable!(),
            };
            let binder = if let Tok::Lower(n) = self.peek().clone() {
                self.bump();
                Some(n)
            } else {
                None
            };
            self.expect(Tok::DArrow, "'->'")?;
            let e = self.parse_expr()?;
            arms.push((tag, binder, tag_tok.span, Box::new(e)));
            if !self.at(&Tok::Pipe) {
                break;
            }
        }
        let span = Span::merge(start, arms.last().unwrap().3.span());
        Ok(Raw::Match(Box::new(scrut), arms, span))
    }

    // postfix projection, then left-assoc application
    fn parse_postfix(&mut self) -> Result<Raw, ParseError> {
        let mut e = self.parse_atom()?;
        while self.eat(&Tok::Dot) {
            let (label, lspan) = self.expect_label()?;
            let span = Span::merge(e.span(), Span::new(lspan.end, lspan.end));
            e = Raw::Proj(Box::new(e), label, span);
        }
        Ok(e)
    }

    fn starts_expr(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_) | Tok::True | Tok::False | Tok::Unit | Tok::Lower(_) | Tok::LParen
                | Tok::Record | Tok::Upper(_) | Tok::Fun | Tok::Let | Tok::Match
        )
    }

    fn parse_expr(&mut self) -> Result<Raw, ParseError> {
        let mut func = self.parse_postfix()?;
        loop {
            if self.starts_expr() && !self.at(&Tok::Let) {
                let arg = self.parse_postfix()?;
                let span = Span::merge(func.span(), arg.span());
                func = Raw::App(Box::new(func), Box::new(arg), span);
            } else {
                break;
            }
        }
        Ok(func)
    }
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Int(_) => "integer".into(),
        Tok::Bool(_) => "bool".into(),
        Tok::Lower(s) => format!("identifier '{}'", s),
        Tok::Upper(s) => format!("tag '{}'", s),
        Tok::Label(s) => format!("label '{}'", s),
        Tok::Let => "'let'".into(),
        Tok::Rec => "'rec'".into(),
        Tok::In => "'in'".into(),
        Tok::Fun => "'fun'".into(),
        Tok::True => "'true'".into(),
        Tok::False => "'false'".into(),
        Tok::If => "'if'".into(),
        Tok::Then => "'then'".into(),
        Tok::Else => "'else'".into(),
        Tok::Record => "'record'".into(),
        Tok::Variant => "'variant'".into(),
        Tok::Match => "'match'".into(),
        Tok::With => "'with'".into(),
        Tok::Unit => "'unit'".into(),
        Tok::LParen => "'('".into(),
        Tok::RParen => "')'".into(),
        Tok::LBrace => "'{'".into(),
        Tok::RBrace => "'}'".into(),
        Tok::LBracket => "'['".into(),
        Tok::RBracket => "']'".into(),
        Tok::Comma => "','".into(),
        Tok::Eq => "'='".into(),
        Tok::Colon => "':'".into(),
        Tok::Dot => "'.'".into(),
        Tok::DArrow => "'->'".into(),
        Tok::Pipe => "'|'".into(),
        Tok::Star => "'*'".into(),
        Tok::Eof => "end of input".into(),
    }
}

fn finalize(raw: &Raw, path: &str, out: &mut Vec<AstNode>) -> String {
    let mk = |data: AstNodeData, span: Span, out: &mut Vec<AstNode>, p: String| {
        out.push(AstNode {
            path: p.clone(),
            span,
            data,
        });
        p
    };
    match raw {
        Raw::LitInt(n, s) => {
            mk(AstNodeData::LitInt { value: *n }, *s, out, path.to_string())
        }
        Raw::LitBool(b, s) => {
            mk(AstNodeData::LitBool { value: *b }, *s, out, path.to_string())
        }
        Raw::LitUnit(s) => mk(AstNodeData::LitUnit, *s, out, path.to_string()),
        Raw::Var(name, s) => mk(
            AstNodeData::Var {
                name: name.clone(),
            },
            *s,
            out,
            path.to_string(),
        ),
        Raw::Lam(param, body, s) => {
            let body_path = finalize(body, &format!("{}/body", path), out);
            mk(
                AstNodeData::Lam {
                    param: param.clone(),
                    body_path,
                },
                *s,
                out,
                path.to_string(),
            )
        }
        Raw::App(fun, arg, s) => {
            let fun_path = finalize(fun, &format!("{}/fun", path), out);
            let arg_path = finalize(arg, &format!("{}/arg", path), out);
            mk(
                AstNodeData::App { fun_path, arg_path },
                *s,
                out,
                path.to_string(),
            )
        }
        Raw::Let(name, rec, bind, body, s) => {
            let bind_path = finalize(bind, &format!("{}/bind", path), out);
            let body_path = finalize(body, &format!("{}/in", path), out);
            mk(
                AstNodeData::Let {
                    name: name.clone(),
                    rec: *rec,
                    bind_path,
                    body_path,
                },
                *s,
                out,
                path.to_string(),
            )
        }
        Raw::Record(fields, s) => {
            let mut field_inits = Vec::new();
            for (i, (label, lspan, val)) in fields.iter().enumerate() {
                let value_path =
                    finalize(val, &format!("{}/f{}/v", path, i), out);
                field_inits.push(FieldInit {
                    label: label.clone(),
                    value_path,
                    label_span: *lspan,
                });
            }
            mk(
                AstNodeData::Record {
                    fields: field_inits,
                },
                *s,
                out,
                path.to_string(),
            )
        }
        Raw::Proj(record, label, s) => {
            let record_path = finalize(record, &format!("{}/of", path), out);
            mk(
                AstNodeData::Proj {
                    record_path,
                    label: label.clone(),
                },
                *s,
                out,
                path.to_string(),
            )
        }
        Raw::Variant(tag, payload, s) => {
            let payload_path = match payload {
                Some(p) => Some(finalize(p, &format!("{}/payload", path), out)),
                None => None,
            };
            mk(
                AstNodeData::Variant {
                    tag: tag.clone(),
                    payload_path,
                },
                *s,
                out,
                path.to_string(),
            )
        }
        Raw::Match(scrut, arms, s) => {
            let scrut_path = finalize(scrut, &format!("{}/scrut", path), out);
            let mut arm_data = Vec::new();
            for (i, (tag, binder, aspan, e)) in arms.iter().enumerate() {
                let expr_path = finalize(e, &format!("{}/arm{}/e", path, i), out);
                arm_data.push(ArmData {
                    tag: tag.clone(),
                    binder: binder.clone(),
                    expr_path,
                    span: *aspan,
                });
            }
            mk(
                AstNodeData::Match {
                    scrut_path,
                    arms: arm_data,
                },
                *s,
                out,
                path.to_string(),
            )
        }
    }
}

pub fn parse(src: &str) -> Result<(Vec<AstNode>, String), ParseError> {
    let toks = Lexer::new(src).tokenize().map_err(|e| ParseError {
        msg: e.msg,
        span: Span::new(e.pos, e.pos + 1),
    })?;
    let mut parser = Parser { toks, idx: 0 };
    let raw = parser.parse_expr()?;
    if !parser.at(&Tok::Eof) {
        return Err(ParseError {
            msg: format!("unexpected token after expression: {}", tok_name(parser.peek())),
            span: parser.peek_span(),
        });
    }
    let mut nodes = Vec::new();
    let root = finalize(&raw, "n0", &mut nodes);
    Ok((nodes, root))
}
