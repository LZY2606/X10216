use crate::ast::*;
use crate::lexer::*;
use crate::span::Span;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Clone, Debug, Serialize)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

pub struct Program {
    pub root: Pid,
    pub nodes: HashMap<Pid, Node>,
}

struct Parser {
    toks: Vec<Tok>,
    pos: usize,
    nodes: HashMap<Pid, Node>,
}

impl Parser {
    fn peek(&self) -> &TokK {
        &self.toks[self.pos].k
    }
    fn span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn bump(&mut self) -> Tok {
        let t = self.toks[self.pos].clone();
        if !matches!(t.k, TokK::Eof) {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, k: &TokK) -> Option<Tok> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(k) {
            Some(self.bump())
        } else {
            None
        }
    }
    fn expect(&mut self, k: TokK, what: &str) -> Result<Tok, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&k) {
            Ok(self.bump())
        } else {
            Err(ParseError { span: self.span(), msg: format!("期望 {}", what) })
        }
    }

    fn mk(&mut self, parent: Pid, role: u32, span: Span, expr: ExprE) -> Pid {
        let p = pid(parent, self.nodes.len() as u32, role);
        // 极端碰撞保护：序号递增保证唯一
        let mut p = p;
        while self.nodes.contains_key(&p) {
            p = pid(parent, self.nodes.len() as u32 + 1, role);
        }
        self.nodes.insert(p, Node { pid: p, span, expr });
        p
    }

    fn parse_program(&mut self) -> Result<Pid, ParseError> {
        let root = self.parse_expr(Pid::ROOT, 0)?;
        if !matches!(self.peek(), TokK::Eof) {
            return Err(ParseError { span: self.span(), msg: "表达式后仍有多余 token".into() });
        }
        Ok(root)
    }

    fn parse_expr(&mut self, parent: Pid, role: u32) -> Result<Pid, ParseError> {
        let start = self.span();
        let pid0 = match self.peek().clone() {
            TokK::Let => {
                self.bump();
                let rec_flag = self.eat(&TokK::Rec).is_some();
                let name_tok = self.expect(TokK::Lower(String::new()), "变量名")?;
                let name = match name_tok.k {
                    TokK::Lower(s) => s,
                    _ => unreachable!(),
                };
                self.expect(TokK::Eq, "'='")?;
                let value = self.parse_expr(parent, 1)?;
                self.expect(TokK::In, "'in'")?;
                let body = self.parse_expr(parent, 2)?;
                let end = self.toks[self.pos.saturating_sub(1)].span.end;
                let rec_pid = if rec_flag {
                    // 用一个稳定伪 pid 标记递归绑定名出现处
                    Some(pid(parent, 7, 7))
                } else {
                    None
                };
                self.mk(
                    parent,
                    role,
                    Span::new(start.start as usize, end as usize),
                    ExprE::Let { name, value, body, rec_pid },
                )
            }
            TokK::Fun => {
                self.bump();
                let param_tok = self.expect(TokK::Lower(String::new()), "参数名")?;
                let param = match param_tok.k {
                    TokK::Lower(s) => s,
                    _ => unreachable!(),
                };
                self.expect(TokK::Arrow, "'->'")?;
                let body = self.parse_expr(parent, 1)?;
                let end = self.toks[self.pos.saturating_sub(1)].span.end;
                self.mk(
                    parent,
                    role,
                    Span::new(start.start as usize, end as usize),
                    ExprE::Fun { param, body },
                )
            }
            _ => return self.parse_bin(parent, role, 0),
        };
        Ok(pid0)
    }

    fn parse_bin(&mut self, parent: Pid, role: u32, prec: u8) -> Result<Pid, ParseError> {
        let mut lhs = self.parse_app(parent, role)?;
        loop {
            let op = match self.peek() {
                TokK::Plus => BinOp::Add,
                TokK::EqEq => BinOp::Eq,
                _ => break,
            };
            let op_prec = match op {
                BinOp::Add => 4,
                BinOp::Eq => 2,
            };
            if op_prec < prec {
                break;
            }
            self.bump();
            let rhs = self.parse_bin(parent, role, op_prec + 1)?;
            let span = self.nodes[&lhs].span.join(self.nodes[&rhs].span);
            lhs = self.mk(parent, role, span, ExprE::Bin { op, lhs, rhs });
        }
        Ok(lhs)
    }

    fn parse_app(&mut self, parent: Pid, role: u32) -> Result<Pid, ParseError> {
        let mut callee = self.parse_atom(parent, role)?;
        loop {
            if atom_start(self.peek()) {
                let arg = self.parse_atom(parent, 10)?;
                let span = self.nodes[&callee].span.join(self.nodes[&arg].span);
                callee = self.mk(parent, role, span, ExprE::App { fun: callee, arg });
            } else {
                break;
            }
        }
        Ok(callee)
    }

    fn parse_atom(&mut self, parent: Pid, role: u32) -> Result<Pid, ParseError> {
        let start = self.span();
        match self.peek().clone() {
            TokK::Int(n) => {
                self.bump();
                Ok(self.mk(parent, role, start, ExprE::Lit(Lit::Int(n))))
            }
            TokK::Str(_s) => {
                self.bump();
                let end = self.toks[self.pos - 1].span;
                Ok(self.mk(parent, role, end, ExprE::Lit(Lit::Str)))
            }
            TokK::True | TokK::False => {
                let t = self.bump();
                Ok(self.mk(parent, role, t.span, ExprE::Lit(Lit::Bool(matches!(t.k, TokK::True)))))
            }
            TokK::Unit => {
                let t = self.bump();
                Ok(self.mk(parent, role, t.span, ExprE::Lit(Lit::Unit)))
            }
            TokK::Lower(name) => {
                let t = self.bump();
                Ok(self.mk(parent, role, t.span, ExprE::Var { name }))
            }
            TokK::Upper(tag) => {
                self.bump();
                let payload = if atom_start(self.peek()) {
                    Some(self.parse_atom(parent, 20)?)
                } else {
                    None
                };
                let end = match payload {
                    Some(p) => self.nodes[&p].span,
                    None => start,
                };
                Ok(self.mk(parent, role, start.join(end), ExprE::Tag { tag, payload }))
            }
            TokK::LParen => {
                self.bump();
                let e = self.parse_expr(parent, role)?;
                self.expect(TokK::RParen, "')'")?;
                Ok(e)
            }
            TokK::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                if !matches!(self.peek(), TokK::RBrace) {
                    loop {
                        let lbl = self.expect(TokK::Lower(String::new()), "字段名")?;
                        let label = match lbl.k {
                            TokK::Lower(s) => s,
                            _ => unreachable!(),
                        };
                        self.expect(TokK::Colon, "':'")?;
                        let e = self.parse_expr(parent, 30 + fields.len() as u32)?;
                        fields.push((label, e));
                        if self.eat(&TokK::Comma).is_none() {
                            break;
                        }
                    }
                }
                let close = self.expect(TokK::RBrace, "'}'")?;
                Ok(self.mk(
                    parent,
                    role,
                    start.join(close.span),
                    ExprE::Record { fields },
                ))
            }
            TokK::LBrack => {
                self.bump();
                let scrut = self.parse_expr(parent, 40)?;
                self.expect(TokK::With, "'with'")?;
                self.expect(TokK::LBrace, "'{'")?;
                let mut arms = Vec::new();
                if !matches!(self.peek(), TokK::RBrace) {
                    loop {
                        let tag_tok = self.expect(TokK::Upper(String::new()), "变体标签（大写开头）")?;
                        let tag = match tag_tok.k {
                            TokK::Upper(s) => s,
                            _ => unreachable!(),
                        };
                        let binding = if let TokK::Lower(_) = self.peek() {
                            if let TokK::Lower(s) = self.bump().k {
                                Some(s)
                            } else {
                                unreachable!()
                            }
                        } else {
                            None
                        };
                        self.expect(TokK::Arrow, "'->'")?;
                        let expr = self.parse_expr(parent, 60 + arms.len() as u32)?;
                        arms.push(Arm { tag, binding, expr });
                        if self.eat(&TokK::Semi).is_none() {
                            break;
                        }
                    }
                }
                let close = self.expect(TokK::RBrace, "'}'")?;
                self.expect(TokK::RBrack, "']'")?;
                Ok(self.mk(
                    parent,
                    role,
                    start.join(close.span),
                    ExprE::Match { scrut, arms },
                ))
            }
            _ => Err(ParseError { span: start, msg: "期望表达式".into() }),
        }
    }
}

fn atom_start(k: &TokK) -> bool {
    matches!(
        k,
        TokK::Int(_)
            | TokK::Str(_)

            | TokK::True
            | TokK::False
            | TokK::Unit
            | TokK::Lower(_)
            | TokK::Upper(_)
            | TokK::LParen
            | TokK::LBrace
    )
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    let toks = match lex(src) {
        Ok(t) => t,
        Err(e) => return Err(ParseError { span: e.span, msg: e.msg }),
    };
    let mut p = Parser { toks, pos: 0, nodes: HashMap::new() };
    let root = p.parse_program()?;
    Ok(Program { root, nodes: p.nodes })
}
