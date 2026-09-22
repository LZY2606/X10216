use crate::ast::{Expr, ExprKind, Span, SrcError};
use crate::lexer::{lex, Tok, Token};

pub fn parse(src: &str) -> Result<Expr, SrcError> {
    let toks = lex(src)?;
    let mut p = Parser { toks, pos: 0 };
    let e = p.parse_expr("0")?;
    if p.peek() != &Tok::Eof {
        return Err(p.err("期望表达式结束"));
    }
    Ok(e)
}

struct Parser {
    toks: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }

    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }

    fn err(&self, msg: &str) -> SrcError {
        SrcError {
            message: msg.into(),
            span: self.toks[self.pos].span,
        }
    }

    fn expect(&mut self, t: &Tok, what: &str) -> Result<Token, SrcError> {
        if self.peek() == t {
            Ok(self.bump())
        } else {
            Err(self.err(&format!("期望 {what},却遇到 {:?}", self.peek())))
        }
    }

    fn ident(&mut self) -> Result<String, SrcError> {
        match self.peek().clone() {
            Tok::Ident(x) => {
                self.bump();
                Ok(x)
            }
            _ => Err(self.err("期望标识符")),
        }
    }

    fn last_end(&self) -> usize {
        self.toks
            .get(self.pos.wrapping_sub(1))
            .map(|t| t.span.end)
            .unwrap_or(0)
    }

    fn mk(&self, path: &str, start_tok: usize, kind: ExprKind) -> Expr {
        Expr {
            id: path.to_string(),
            span: Span {
                start: self.toks[start_tok].span.start,
                end: self.last_end(),
            },
            kind,
        }
    }

    fn parse_expr(&mut self, path: &str) -> Result<Expr, SrcError> {
        let start = self.pos;
        match self.peek().clone() {
            Tok::Let => {
                self.bump();
                let rec_ = if self.peek() == &Tok::Rec {
                    self.bump();
                    true
                } else {
                    false
                };
                let name = self.ident()?;
                self.expect(&Tok::Eq, "=")?;
                let value = self.parse_expr(&format!("{path}.0"))?;
                self.expect(&Tok::In, "in")?;
                let body = self.parse_expr(&format!("{path}.1"))?;
                Ok(self.mk(
                    path,
                    start,
                    ExprKind::Let {
                        rec_,
                        name,
                        value: Box::new(value),
                        body: Box::new(body),
                    },
                ))
            }
            Tok::Fun => {
                self.bump();
                let param = self.ident()?;
                self.expect(&Tok::Arrow, "->")?;
                let body = self.parse_expr(&format!("{path}.0"))?;
                Ok(self.mk(
                    path,
                    start,
                    ExprKind::Fun {
                        param,
                        body: Box::new(body),
                    },
                ))
            }
            Tok::If => {
                self.bump();
                let cond = self.parse_expr(&format!("{path}.0"))?;
                self.expect(&Tok::Then, "then")?;
                let then_ = self.parse_expr(&format!("{path}.1"))?;
                self.expect(&Tok::Else, "else")?;
                let else_ = self.parse_expr(&format!("{path}.2"))?;
                Ok(self.mk(
                    path,
                    start,
                    ExprKind::If {
                        cond: Box::new(cond),
                        then_: Box::new(then_),
                        else_: Box::new(else_),
                    },
                ))
            }
            Tok::Match => {
                self.bump();
                let scrut = self.parse_add(&format!("{path}.0"))?;
                self.expect(&Tok::With, "with")?;
                let mut arms = Vec::new();
                let mut i = 1;
                while self.peek() == &Tok::Bar {
                    self.bump();
                    self.expect(&Tok::Hash, "#")?;
                    let label = self.ident()?;
                    let binder = self.ident()?;
                    self.expect(&Tok::Arrow, "->")?;
                    let body = self.parse_expr(&format!("{path}.{i}"))?;
                    arms.push((label, binder, body));
                    i += 1;
                }
                if arms.is_empty() {
                    return Err(self.err("match 至少需要一个分支"));
                }
                Ok(self.mk(
                    path,
                    start,
                    ExprKind::Match {
                        scrut: Box::new(scrut),
                        arms,
                    },
                ))
            }
            _ => self.parse_add(path),
        }
    }

    fn parse_add(&mut self, path: &str) -> Result<Expr, SrcError> {
        let start = self.pos;
        let mut left = self.parse_app(&format!("{path}.0"))?;
        if self.peek() == &Tok::Plus {
            self.bump();
            let right = self.parse_add(&format!("{path}.1"))?;
            return Ok(self.mk(
                path,
                start,
                ExprKind::Add(Box::new(left), Box::new(right)),
            ));
        }
        restamp(&mut left, &format!("{path}.0"), path);
        Ok(left)
    }

    fn parse_app(&mut self, path: &str) -> Result<Expr, SrcError> {
        let start = self.pos;
        let mut func = self.parse_postfix(&format!("{path}.0"))?;
        let mut args = Vec::new();
        let mut i = 1;
        while starts_atom(self.peek()) {
            args.push(self.parse_postfix(&format!("{path}.{i}"))?);
            i += 1;
        }
        if args.is_empty() {
            restamp(&mut func, &format!("{path}.0"), path);
            return Ok(func);
        }
        let n = args.len();
        let mut result = func;
        for (j, arg) in args.into_iter().enumerate() {
            let id = if j + 1 == n {
                path.to_string()
            } else {
                format!("{path}.app{j}")
            };
            result = Expr {
                id,
                span: Span {
                    start: self.toks[start].span.start,
                    end: arg.span.end,
                },
                kind: ExprKind::App {
                    fun: Box::new(result),
                    arg: Box::new(arg),
                },
            };
        }
        Ok(result)
    }

    fn parse_postfix(&mut self, path: &str) -> Result<Expr, SrcError> {
        let mut base = self.parse_atom(&format!("{path}.0"))?;
        let mut labels = Vec::new();
        while self.peek() == &Tok::Dot {
            self.bump();
            labels.push(self.ident()?);
        }
        if labels.is_empty() {
            restamp(&mut base, &format!("{path}.0"), path);
            return Ok(base);
        }
        let n = labels.len();
        let mut result = base;
        for (j, label) in labels.into_iter().enumerate() {
            let id = if j + 1 == n {
                path.to_string()
            } else {
                format!("{path}.fld{j}")
            };
            let end = self.last_end();
            result = Expr {
                id,
                span: Span {
                    start: result.span.start,
                    end,
                },
                kind: ExprKind::Field {
                    base: Box::new(result),
                    label,
                },
            };
        }
        Ok(result)
    }

    fn parse_atom(&mut self, path: &str) -> Result<Expr, SrcError> {
        let start = self.pos;
        match self.peek().clone() {
            Tok::Int(n) => {
                self.bump();
                Ok(self.mk(path, start, ExprKind::Int(n)))
            }
            Tok::True => {
                self.bump();
                Ok(self.mk(path, start, ExprKind::Bool(true)))
            }
            Tok::False => {
                self.bump();
                Ok(self.mk(path, start, ExprKind::Bool(false)))
            }
            Tok::Ident(x) => {
                self.bump();
                Ok(self.mk(path, start, ExprKind::Var(x)))
            }
            Tok::LParen => {
                self.bump();
                if self.peek() == &Tok::RParen {
                    self.bump();
                    Ok(self.mk(path, start, ExprKind::Unit))
                } else {
                    let e = self.parse_expr(path)?;
                    self.expect(&Tok::RParen, ")")?;
                    Ok(e)
                }
            }
            Tok::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                if self.peek() != &Tok::RBrace {
                    loop {
                        let name = self.ident()?;
                        self.expect(&Tok::Eq, "=")?;
                        let v = self.parse_expr(&format!("{path}.{}", fields.len()))?;
                        fields.push((name, v));
                        if self.peek() == &Tok::Comma {
                            self.bump();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(&Tok::RBrace, "}")?;
                Ok(self.mk(path, start, ExprKind::Record { fields }))
            }
            Tok::Hash => {
                self.bump();
                let label = self.ident()?;
                if !starts_atom(self.peek()) {
                    return Err(self.err("变体构造器需要一个参数(无参数请写 ())"));
                }
                let arg = self.parse_postfix(&format!("{path}.0"))?;
                Ok(self.mk(
                    path,
                    start,
                    ExprKind::Variant {
                        label,
                        arg: Box::new(arg),
                    },
                ))
            }
            _ => Err(self.err("期望表达式")),
        }
    }
}

fn starts_atom(t: &Tok) -> bool {
    matches!(
        t,
        Tok::Ident(_) | Tok::Int(_) | Tok::True | Tok::False | Tok::LParen | Tok::LBrace | Tok::Hash
    )
}

/// 当某个语法位置没有产生新的组合节点时,把子树的身份从临时路径
/// 重写到父路径,保持节点身份只取决于语法结构路径。
fn restamp(e: &mut Expr, from: &str, to: &str) {
    let dotted = format!("{from}.");
    if e.id == from {
        e.id = to.to_string();
    } else if let Some(suffix) = e.id.strip_prefix(&dotted) {
        e.id = format!("{to}.{suffix}");
    }
    match &mut e.kind {
        ExprKind::Int(_) | ExprKind::Bool(_) | ExprKind::Unit | ExprKind::Var(_) => {}
        ExprKind::Fun { body, .. } => restamp(body, from, to),
        ExprKind::App { fun, arg } => {
            restamp(fun, from, to);
            restamp(arg, from, to);
        }
        ExprKind::Add(a, b) => {
            restamp(a, from, to);
            restamp(b, from, to);
        }
        ExprKind::Let { value, body, .. } => {
            restamp(value, from, to);
            restamp(body, from, to);
        }
        ExprKind::If {
            cond,
            then_,
            else_,
        } => {
            restamp(cond, from, to);
            restamp(then_, from, to);
            restamp(else_, from, to);
        }
        ExprKind::Record { fields } => {
            for (_, v) in fields {
                restamp(v, from, to);
            }
        }
        ExprKind::Field { base, .. } => restamp(base, from, to),
        ExprKind::Variant { arg, .. } => restamp(arg, from, to),
        ExprKind::Match { scrut, arms } => {
            restamp(scrut, from, to);
            for (_, _, body) in arms {
                restamp(body, from, to);
            }
        }
    }
}
