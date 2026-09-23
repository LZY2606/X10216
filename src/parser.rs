//! 递归下降解析器。NodePath 在解析时按子节点序号分配，
//! 与变量名无关：改名不会造成身份漂移。

use crate::ast::{Expr, ExprKind};
use crate::common::{NodePath, Span};
use crate::lexer::{Lexer, Tok, Token};

pub struct Parser {
    toks: Vec<Token>,
    idx: usize,
}

impl Parser {
    pub fn parse(src: &str) -> Result<Expr, (String, Span)> {
        let toks = Lexer::new(src).tokenize()?;
        let mut p = Parser { toks, idx: 0 };
        let e = p.expr(&NodePath::root())?;
        if !matches!(p.peek().tok, Tok::Eof) {
            return Err(("表达式结束后仍有多余记号".into(), p.peek().span));
        }
        Ok(e)
    }

    fn peek(&self) -> &Token {
        &self.toks[self.idx]
    }
    fn at(&self, t: &Tok) -> bool {
        std::mem::discriminant(&self.peek().tok) == std::mem::discriminant(t)
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
    fn expect(&mut self, t: &Tok, what: &str) -> Result<Token, (String, Span)> {
        if self.at(t) {
            Ok(self.bump())
        } else {
            Err((format!("期望 {}", what), self.peek().span))
        }
    }

    fn expr(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        match self.peek().tok {
            Tok::Let => self.let_expr(path),
            Tok::If => self.if_expr(path),
            Tok::Case => self.case_expr(path),
            Tok::Fun | Tok::Backslash => self.fun_expr(path),
            _ => self.app_expr(path),
        }
    }

    fn mk<F: FnOnce(Span, NodePath) -> Expr>(&self, path: &NodePath, start: usize, end: usize, f: F) -> Expr {
        f(Span::new(start, end), path.clone())
    }

    fn let_expr(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        let start = self.bump().span.start;
        let rec = self.eat(&Tok::Rec);
        let name = match self.bump().tok {
            Tok::Ident(n) => n,
            _ => return Err(("let 后需要标识符".into(), self.peek().span)),
        };
        self.expect(&Tok::Eq, "'='")?;
        let bind = self.expr(&path.child(0))?;
        self.expect(&Tok::In, "'in'")?;
        let body = self.expr(&path.child(1))?;
        Ok(Expr {
            kind: ExprKind::Let { rec, name, bind: Box::new(bind), body: Box::new(body) },
            span: Span::new(start, body.span.end),
            path: path.clone(),
        })
    }

    fn if_expr(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        let start = self.bump().span.start;
        let cond = self.expr(&path.child(0))?;
        self.expect(&Tok::Then, "'then'")?;
        let then_e = self.expr(&path.child(1))?;
        self.expect(&Tok::Else, "'else'")?;
        let else_e = self.expr(&path.child(2))?;
        Ok(Expr {
            kind: ExprKind::If {
                cond: Box::new(cond),
                then_e: Box::new(then_e),
                else_e: Box::new(else_e),
            },
            span: Span::new(start, else_e.span.end),
            path: path.clone(),
        })
    }

    fn fun_expr(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        let start = self.bump().span.start;
        let param = match self.bump().tok {
            Tok::Ident(n) => n,
            _ => return Err(("fn 后需要参数名".into(), self.peek().span)),
        };
        self.expect(&Tok::Arrow, "'->'")?;
        let body = self.expr(&path.child(0))?;
        Ok(Expr {
            kind: ExprKind::Lam { param, body: Box::new(body) },
            span: Span::new(start, body.span.end),
            path: path.clone(),
        })
    }

    fn case_expr(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        let start = self.bump().span.start;
        let scrut = self.app_expr(&path.child(0))?;
        self.expect(&Tok::Of, "'of'")?;
        let tag = match self.bump().tok {
            Tok::Upper(t) => t,
            _ => return Err(("case 分支需要大写构造子 Tag(x)".into(), self.peek().span)),
        };
        self.expect(&Tok::LParen, "'('")?;
        let binder = match self.bump().tok {
            Tok::Ident(n) => n,
            _ => return Err(("构造子后需要 (变量)".into(), self.peek().span)),
        };
        self.expect(&Tok::RParen, "')'")?;
        self.expect(&Tok::Arrow, "'->'")?;
        let branch = self.expr(&path.child(1))?;
        if !self.eat(&Tok::Semicolon) && !self.eat(&Tok::Pipe) {
            return Err(("分支后需要 ';' 或 '|' 分隔".into(), self.peek().span));
        }
        match &self.peek().tok {
            Tok::Ident(n) if n == "_" => {
                self.bump();
            }
            _ => return Err(("case 需要 '_' 缺省分支".into(), self.peek().span)),
        }
        self.expect(&Tok::Arrow, "'->'")?;
        let default = self.expr(&path.child(2))?;
        let end_tok = self.expect(&Tok::End, "'end'")?;
        Ok(Expr {
            kind: ExprKind::Case {
                scrut: Box::new(scrut),
                tag,
                binder,
                branch: Box::new(branch),
                default: Box::new(default),
            },
            span: Span::new(start, end_tok.span.end),
            path: path.clone(),
        })
    }

    /// app := postfix+（左结合）
    fn app_expr(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        let mut func = self.postfix(&path.child(1))?;
        let mut i = 2u32;
        while self.starts_atom() {
            let arg = self.postfix(&path.child(i))?;
            let span = Span::merge(func.span, arg.span);
            func = Expr {
                kind: ExprKind::App { fun: Box::new(func), arg: Box::new(arg) },
                span,
                path: path.clone(),
            };
            i += 1;
        }
        Ok(func)
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek().tok,
            Tok::Int(_) | Tok::Str | Tok::True | Tok::False | Tok::Ident(_) | Tok::Upper(_)
                | Tok::LParen | Tok::LBrace
        )
    }

    /// postfix := atom ('.' field)*
    fn postfix(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        let mut e = self.atom(path)?;
        let mut i = 0u32;
        while self.eat(&Tok::Dot) {
            let field_tok = self.bump();
            let f = match field_tok.tok {
                Tok::Ident(n) => n,
                _ => return Err(("'.' 后需要字段名".into(), field_tok.span)),
            };
            let span = Span::new(e.span.start, field_tok.span.end);
            e = Expr {
                kind: ExprKind::Proj { record: Box::new(e), field: f },
                span,
                path: path.child(100 + i),
            };
            i += 1;
        }
        Ok(e)
    }

    fn atom(&mut self, path: &NodePath) -> Result<Expr, (String, Span)> {
        let t0 = self.peek().clone();
        match t0.tok {
            Tok::Int(n) => {
                self.bump();
                Ok(Expr { kind: ExprKind::Int(n), span: t0.span, path: path.clone() })
            }
            Tok::True | Tok::False => {
                self.bump();
                Ok(Expr { kind: ExprKind::Bool(matches!(t0.tok, Tok::True)), span: t0.span, path: path.clone() })
            }
            Tok::Str => {
                self.bump();
                Ok(Expr { kind: ExprKind::Str, span: t0.span, path: path.clone() })
            }
            Tok::Ident(n) => {
                self.bump();
                if n == "_" {
                    return Err(("单独的 '_' 不能作为表达式".into(), t0.span));
                }
                Ok(Expr { kind: ExprKind::Var(n), span: t0.span, path: path.clone() })
            }
            Tok::LParen => {
                self.bump();
                let e = self.expr(path)?;
                let close = self.expect(&Tok::RParen, "')'")?;
                Ok(Expr { kind: e.kind, span: Span::new(t0.span.start, close.span.end), path: path.clone() })
            }
            Tok::LBrace => self.record(path, t0.span.start),
            Tok::Upper(_) => self.variant(path, t0),
            _ => Err(("无法解析的表达式开头".into(), t0.span)),
        }
    }

    fn record(&mut self, path: &NodePath, start: usize) -> Result<Expr, (String, Span)> {
        self.expect(&Tok::LBrace, "'{'")?;
        let mut fields = Vec::new();
        let mut i = 0u32;
        loop {
            if self.at(&Tok::RBrace) {
                break;
            }
            let name = match self.bump().tok {
                Tok::Ident(n) => n,
                _ => return Err(("记录字段需要标识符".into(), self.peek().span)),
            };
            self.expect(&Tok::Eq, "'='")?;
            let val = self.expr(&path.child(i))?;
            fields.push((name, val));
            i += 1;
            if !self.eat(&Tok::Comma) {
                break;
            }
        }
        let close = self.expect(&Tok::RBrace, "'}'")?;
        Ok(Expr {
            kind: ExprKind::Record { fields },
            span: Span::new(start, close.span.end),
            path: path.clone(),
        })
    }

    fn variant(&mut self, path: &NodePath, first: Token) -> Result<Expr, (String, Span)> {
        let tag = match first.tok {
            Tok::Upper(t) => t,
            _ => unreachable!(),
        };
        self.expect(&Tok::LParen, "'('（构造子必须带载荷 Tag(e)；无载荷用 Tag(0)）")?;
        let payload = self.expr(&path.child(0))?;
        let close = self.expect(&Tok::RParen, "')'")?;
        Ok(Expr {
            kind: ExprKind::Variant { tag, payload: Box::new(payload) },
            span: Span::new(first.span.start, close.span.end),
            path: path.clone(),
        })
    }
}
