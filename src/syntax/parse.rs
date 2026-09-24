use super::ast::*;
use super::lex::{lex, Tok, Token};

#[derive(Clone, Debug)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

struct Parser<'a> {
    toks: Vec<Token>,
    pos: usize,
    src: &'a str,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn at(&self, t: &Tok) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(t)
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
    fn eat(&mut self, t: &Tok) -> Option<Token> {
        if self.at(t) {
            Some(self.bump())
        } else {
            None
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.at(&t) {
            Ok(self.bump())
        } else {
            Err(ParseError {
                msg: format!("期望 {}，但遇到 {}", what, tok_name(self.peek())),
                span: self.span(),
            })
        }
    }

    fn program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        while !self.at(&Tok::Eof) {
            let item = self.item()?;
            items.push(item);
            if self.at(&Tok::Semicolon) {
                self.bump();
                while self.at(&Tok::Semicolon) {
                    self.bump();
                }
            } else if !self.at(&Tok::Eof) {
                return Err(ParseError {
                    msg: "顶层定义之间需要用 ; 分隔".into(),
                    span: self.span(),
                });
            }
        }
        Ok(Program { items })
    }

    fn item(&mut self) -> Result<Item, ParseError> {
        self.expect(Tok::Let, "let")?;
        let rec = self.eat(&Tok::Rec).is_some();
        let name = self.expect_ident()?;
        self.expect(Tok::Eq, "=")?;
        let expr = self.expr()?;
        Ok(Item {
            name: name.node,
            rec,
            expr,
        })
    }

    fn expect_ident(&mut self) -> Result<Spanned<String>, ParseError> {
        if let Tok::Ident(s) = self.peek().clone() {
            let t = self.bump();
            Ok(Spanned {
                node: s,
                span: t.span,
                nid: 0,
            })
        } else {
            Err(ParseError {
                msg: format!("期望标识符，但遇到 {}", tok_name(self.peek())),
                span: self.span(),
            })
        }
    }

    fn expr(&mut self) -> Result<SExpr, ParseError> {
        self.let_expr()
    }

    fn let_expr(&mut self) -> Result<SExpr, ParseError> {
        if !self.at(&Tok::Let) {
            return self.if_expr();
        }
        let start = self.span().start;
        self.bump();
        let rec = self.eat(&Tok::Rec).is_some();
        let name = self.expect_ident()?.node;
        self.expect(Tok::Eq, "=")?;
        let bound = self.expr()?;
        self.expect(Tok::In, "in")?;
        let body = self.expr()?;
        let end = body.span.end;
        Ok(plain(
            Expr::Let {
                rec,
                name,
                bound: Box::new(bound),
                body: Box::new(body),
            },
            start,
            end,
        ))
    }

    fn if_expr(&mut self) -> Result<SExpr, ParseError> {
        if !self.at(&Tok::If) {
            return self.fn_expr();
        }
        let start = self.span().start;
        self.bump();
        let cond = self.expr()?;
        self.expect(Tok::Then, "then")?;
        let then_e = self.expr()?;
        self.expect(Tok::Else, "else")?;
        let else_e = self.expr()?;
        let end = else_e.span.end;
        Ok(plain(
            Expr::If {
                cond: Box::new(cond),
                then_e: Box::new(then_e),
                else_e: Box::new(else_e),
            },
            start,
            end,
        ))
    }

    fn fn_expr(&mut self) -> Result<SExpr, ParseError> {
        if !self.at(&Tok::Fn) {
            return self.binop();
        }
        let start = self.span().start;
        self.bump();
        let param = self.expect_ident()?.node;
        self.expect(Tok::Arrow, "->")?;
        let body = self.expr()?;
        let end = body.span.end;
        Ok(plain(
            Expr::Fn {
                param,
                body: Box::new(body),
            },
            start,
            end,
        ))
    }

    fn binop(&mut self) -> Result<SExpr, ParseError> {
        // 两级：比较 <；加减 + -；乘 *。
        self.level_cmp()
    }

    fn binary_level(
        &mut self,
        next: fn(&mut Parser) -> Result<SExpr, ParseError>,
        ops: &[Tok],
    ) -> Result<SExpr, ParseError> {
        let mut left = next(self)?;
        loop {
            let op_tok = self.toks[self.pos].clone();
            let op = match &op_tok.tok {
                Tok::Plus => '+',
                Tok::Minus => '-',
                Tok::Star => '*',
                Tok::Lt => '<',
                Tok::Eq => '=',
                _ => break,
            };
            if !ops.iter().any(|t| std::mem::discriminant(t) == std::mem::discriminant(&op_tok.tok))
            {
                break;
            }
            self.bump();
            let rhs = next(self)?;
            let end = rhs.span.end;
            left = plain(
                Expr::Binop {
                    op,
                    lhs: Box::new(left),
                    rhs: Box::new(rhs),
                },
                op_tok.span.start,
                end,
            );
        }
        Ok(left)
    }

    fn level_cmp(&mut self) -> Result<SExpr, ParseError> {
        self.binary_level(Parser::level_add, &[Tok::Lt, Tok::Eq])
    }
    fn level_add(&mut self) -> Result<SExpr, ParseError> {
        self.binary_level(Parser::level_mul, &[Tok::Plus, Tok::Minus])
    }
    fn level_mul(&mut self) -> Result<SExpr, ParseError> {
        self.binary_level(Parser::postfix, &[Tok::Star])
    }

    /// 应用与投影（左结合）。变体标签是应用的一种。
    fn postfix(&mut self) -> Result<SExpr, ParseError> {
        let mut e = self.atom()?;
        loop {
            if let Tok::Field(f) = self.peek().clone() {
                let ft = self.bump();
                let end = ft.span.end;
                let start = e.span.start;
                e = plain(
                    Expr::Project {
                        record: Box::new(e),
                        field: f,
                    },
                    start,
                    end,
                );
                continue;
            }
            // 应用：原子紧跟原子。为避免吞掉后续结构，只在出现可启动表达式的 token 时进行。
            if self.starts_atom() {
                let arg = self.atom()?;
                let end = arg.span.end;
                let start = e.span.start;
                // Tag 应用在 atom 中已构造为 Variant；此处处理普通函数应用。
                e = match e.node {
                    _ => plain(
                        Expr::App {
                            fun: Box::new(e),
                            arg: Box::new(arg),
                        },
                        start,
                        end,
                    ),
                };
                continue;
            }
            break;
        }
        Ok(e)
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_)
                | Tok::Ident(_)
                | Tok::Tag(_)
                | Tok::True
                | Tok::False
                | Tok::LParen
                | Tok::LBrace
                | Tok::Fn
                | Tok::If
                | Tok::Let
                | Tok::Match
        )
    }

    fn atom(&mut self) -> Result<SExpr, ParseError> {
        let t = self.toks[self.pos].clone();
        let start = t.span.start;
        let node_e: SExpr = match t.tok {
            Tok::Int(n) => {
                self.bump();
                plain(Expr::Int(n), start, t.span.end)
            }
            Tok::True => {
                self.bump();
                plain(Expr::Bool(true), start, t.span.end)
            }
            Tok::False => {
                self.bump();
                plain(Expr::Bool(false), start, t.span.end)
            }
            Tok::Ident(s) => {
                self.bump();
                plain(Expr::Ident(s), start, t.span.end)
            }
            Tok::Tag(tag) => {
                self.bump();
                // 变体构造必须带载荷：Tag payload
                let payload = self.atom()?;
                let end = payload.span.end;
                plain(
                    Expr::Variant {
                        tag,
                        payload: Box::new(payload),
                    },
                    start,
                    end,
                )
            }
            Tok::LParen => {
                self.bump();
                if self.eat(&Tok::RParen).is_some() {
                    plain(Expr::Unit, start, self.toks[self.pos.saturating_sub(1)].span.end)
                } else {
                    let inner = self.expr()?;
                    let rp = self.expect(Tok::RParen, ")")?;
                    plain(Expr::Paren(Box::new(inner)), start, rp.span.end)
                }
            }
            Tok::LBrace => self.record(start)?,
            Tok::Match => self.match_expr(start)?,
            _ => {
                return Err(ParseError {
                    msg: format!("此处不能以 {} 开始表达式", tok_name(self.peek())),
                    span: self.span(),
                });
            }
        };
        Ok(node_e)
    }

    fn record(&mut self, start: u32) -> Result<SExpr, ParseError> {
        self.expect(Tok::LBrace, "{")?;
        let mut fields: Vec<(String, SExpr)> = Vec::new();
        let mut rest: Option<SExpr> = None;
        if !self.at(&Tok::RBrace) {
            loop {
                if self.at(&Tok::Bar) {
                    self.bump();
                    rest = Some(self.expr()?);
                    break;
                }
                let name = self.expect_ident()?.node;
                self.expect(Tok::Eq, "=")?;
                let val = self.expr()?;
                fields.push((name, val));
                if self.eat(&Tok::Semicolon).is_some() {
                    if self.at(&Tok::RBrace) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let rb = self.expect(Tok::RBrace, "}")?;
        let end = rb.span.end;
        // 从右向左折叠扩展；无 rest 时为闭记录。
        let base = match rest {
            Some(r) => r,
            None => plain(Expr::Record { fields: vec![] }, start, end),
        };
        let mut e = base;
        for (field, value) in fields.into_iter().rev() {
            let s = e.span.start;
            e = plain(
                Expr::Extend {
                    field,
                    value: Box::new(value),
                    rest: Box::new(e),
                },
                s,
                end,
            );
        }
        Ok(e)
    }

    fn match_expr(&mut self, start: u32) -> Result<SExpr, ParseError> {
        self.expect(Tok::Match, "match")?;
        let scrut = self.expr()?;
        self.expect(Tok::With, "with")?;
        self.eat(&Tok::Bar);
        let clause_tag = if let Tok::Tag(s) = self.peek().clone() {
            self.bump();
            s
        } else {
            return Err(ParseError {
                msg: "match 分支应以大写变体标签开始".into(),
                span: self.span(),
            });
        };
        let pat_start = self.span().start;
        let pat = self.pattern(pat_start)?;
        self.expect(Tok::Fat, "=>")?;
        let clause_body = self.expr()?;
        self.expect(Tok::Semicolon, ";")?;
        self.eat(&Tok::Bar);
        self.expect(Tok::Underscore, "_")?;
        self.expect(Tok::Fat, "=>")?;
        let default = self.expr()?;
        let end = default.span.end;
        Ok(plain(
            Expr::Match {
                scrut: Box::new(scrut),
                clause_tag,
                clause_pat: pat,
                clause_body: Box::new(clause_body),
                default: Box::new(default),
            },
            start,
            end,
        ))
    }

    fn pattern(&mut self, start: u32) -> Result<SPat, ParseError> {
        let t = self.toks[self.pos].clone();
        match t.tok {
            Tok::Underscore => {
                self.bump();
                Ok(SPat::new(Pat::Wild, t.span, 0))
            }
            Tok::Ident(s) => {
                self.bump();
                Ok(SPat::new(Pat::Binding(s), t.span, 0))
            }
            Tok::LParen => {
                self.bump();
                let rp = self.expect(Tok::RParen, ")")?;
                Ok(SPat::new(
                    Pat::Unit,
                    Span {
                        start,
                        end: rp.span.end,
                    },
                    0,
                ))
            }
            _ => Err(ParseError {
                msg: format!("期望匹配模式，但遇到 {}", tok_name(self.peek())),
                span: self.span(),
            }),
        }
    }
}

fn plain(node: Expr, start: u32, end: u32) -> SExpr {
    SExpr::new(
        node,
        Span { start, end },
        0,
    )
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Eof => "文件结尾".into(),
        Tok::Int(n) => format!("整数 {}", n),
        Tok::Ident(s) => format!("标识符 {}", s),
        Tok::Tag(s) => format!("变体标签 {}", s),
        Tok::Field(s) => format!(".{}", s),
        Tok::Let => "let".into(),
        Tok::Rec => "rec".into(),
        Tok::In => "in".into(),
        Tok::If => "if".into(),
        Tok::Then => "then".into(),
        Tok::Else => "else".into(),
        Tok::Fn => "fn".into(),
        Tok::Match => "match".into(),
        Tok::With => "with".into(),
        Tok::True => "true".into(),
        Tok::False => "false".into(),
        Tok::Eq => "=".into(),
        Tok::Arrow => "->".into(),
        Tok::Fat => "=>".into(),
        Tok::Plus => "+".into(),
        Tok::Minus => "-".into(),
        Tok::Star => "*".into(),
        Tok::Lt => "<".into(),
        Tok::LParen => "(".into(),
        Tok::RParen => ")".into(),
        Tok::LBrace => "{".into(),
        Tok::RBrace => "}".into(),
        Tok::Semicolon => ";".into(),
        Tok::Bar => "|".into(),
        Tok::Underscore => "_".into(),
    }
}
