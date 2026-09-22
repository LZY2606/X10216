use serde::Serialize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn union(a: Span, b: Span) -> Span {
        Span { lo: a.lo.min(b.lo), hi: a.hi.max(b.hi) }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Expr {
    Int(i64, Span),
    Bool(bool, Span),
    Str(String, Span),
    Var(String, Span),
    Fun { param: String, body: Box<Expr>, span: Span },
    App { fun: Box<Expr>, arg: Box<Expr>, span: Span },
    Bin { op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>, span: Span },
    Let { name: String, rec: bool, value: Box<Expr>, body: Box<Expr>, span: Span },
    Record { fields: Vec<(String, Expr)>, span: Span },
    Proj { obj: Box<Expr>, field: String, span: Span },
    Variant { label: String, payload: Option<Box<Expr>>, span: Span },
    Match { scrut: Box<Expr>, arms: Vec<(String, Option<String>, Expr)>, span: Span },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum BinOp {
    Add,
    Sub,
    Eq,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, s)
            | Expr::Bool(_, s)
            | Expr::Str(_, s)
            | Expr::Var(_, s)
            | Expr::Fun { span: s, .. }
            | Expr::App { span: s, .. }
            | Expr::Bin { span: s, .. }
            | Expr::Let { span: s, .. }
            | Expr::Record { span: s, .. }
            | Expr::Proj { span: s, .. }
            | Expr::Variant { span: s, .. }
            | Expr::Match { span: s, .. } => *s,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    BoolLit(bool),
    Str(String),
    Ident(String),
    Kw(&'static str),
    Punct(char),
    Arrow,
    Eq,
    Dot,
    Hash,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, (Span, String)> {
    let b: Vec<char> = src.chars().collect();
    let mut i = 0;
    let byte_of = |ci: usize| b[..ci].iter().map(|c| c.len_utf8()).sum::<usize>();
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i];
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '-' && i + 1 < b.len() && b[i + 1] == '-' {
            while i < b.len() && b[i] != '\n' {
                i += 1;
            }
            continue;
        }
        let lo = byte_of(i);
        if c.is_ascii_digit() {
            let start = i;
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            let text: String = b[start..i].iter().collect();
            let hi = byte_of(i);
            out.push(Token { tok: Tok::Int(text.parse().unwrap()), span: Span { lo, hi } });
            continue;
        }
        if c == '"' {
            i += 1;
            let start = i;
            while i < b.len() && b[i] != '"' {
                if b[i] == '\\' && i + 1 < b.len() {
                    i += 1;
                }
                i += 1;
            }
            if i >= b.len() {
                return Err((Span { lo, hi: byte_of(i) }, "字符串缺少右引号".into()));
            }
            let text: String = b[start..i].iter().collect();
            i += 1;
            let hi = byte_of(i);
            out.push(Token { tok: Tok::Str(text), span: Span { lo, hi } });
            continue;
        }
        if c.is_alphabetic() || c == '_' {
            let start = i;
            while i < b.len() && (b[i].is_alphanumeric() || b[i] == '_') {
                i += 1;
            }
            let word: String = b[start..i].iter().collect();
            let hi = byte_of(i);
            let tok = match word.as_str() {
                "let" => Tok::Kw("let"),
                "rec" => Tok::Kw("rec"),
                "in" => Tok::Kw("in"),
                "fun" => Tok::Kw("fun"),
                "match" => Tok::Kw("match"),
                "true" => Tok::BoolLit(true),
                "false" => Tok::BoolLit(false),
                _ => Tok::Ident(word),
            };
            out.push(Token { tok, span: Span { lo, hi } });
            continue;
        }
        let two = if i + 1 < b.len() { format!("{}{}", b[i], b[i + 1]) } else { String::new() };
        if two == "->" {
            i += 2;
            out.push(Token { tok: Tok::Arrow, span: Span { lo, hi: byte_of(i) } });
            continue;
        }
        if two == "==" {
            i += 2;
            out.push(Token { tok: Tok::Eq, span: Span { lo, hi: byte_of(i) } });
            continue;
        }
        if c == '=' {
            i += 1;
            out.push(Token { tok: Tok::Punct('='), span: Span { lo, hi: byte_of(i) } });
            continue;
        }
        if c == '.' {
            i += 1;
            out.push(Token { tok: Tok::Dot, span: Span { lo, hi: byte_of(i) } });
            continue;
        }
        if c == '#' {
            i += 1;
            out.push(Token { tok: Tok::Hash, span: Span { lo, hi: byte_of(i) } });
            continue;
        }
        if "+-(){}<>,;:|".contains(c) {
            i += 1;
            out.push(Token { tok: Tok::Punct(c), span: Span { lo, hi: byte_of(i) } });
            continue;
        }
        Err((Span { lo, hi: byte_of((i + 1).min(b.len())) }, format!("无法识别的字符 {c:?}")))?
    }
    out.push(Token { tok: Tok::Eof, span: Span { lo: src.len(), hi: src.len() } });
    Ok(out)
}

struct P {
    toks: Vec<Token>,
    pos: usize,
}

impl P {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn last_span(&self) -> Span {
        self.toks[self.pos.saturating_sub(1)].span
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }
    fn eat_punct(&mut self, c: char) -> bool {
        if matches!(self.peek(), Tok::Punct(x) if *x == c) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect_punct(&mut self, c: char) -> Result<(), (Span, String)> {
        if self.eat_punct(c) {
            Ok(())
        } else {
            Err((self.span(), format!("缺少 '{c}'")))
        }
    }
    fn expect_kw(&mut self, kw: &str) -> Result<(), (Span, String)> {
        if matches!(self.peek(), Tok::Kw(k) if *k == kw) {
            self.bump();
            Ok(())
        } else {
            Err((self.span(), format!("缺少关键字 {kw}")))
        }
    }
    fn is_boundary(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Eof
                | Tok::Punct(')')
                | Tok::Punct('}')
                | Tok::Punct(',')
                | Tok::Punct(';')
                | Tok::Punct('|')
                | Tok::Punct('>')
                | Tok::Punct(':')
                | Tok::Punct('+')
                | Tok::Punct('-')
                | Tok::Eq
                | Tok::Kw(_)
        )
    }
    fn ident(&mut self) -> Result<(String, Span), (Span, String)> {
        if let Tok::Ident(n) = self.peek().clone() {
            let s = self.span();
            self.bump();
            Ok((n, s))
        } else {
            Err((self.span(), "需要标识符".into()))
        }
    }

    fn expr(&mut self) -> Result<Expr, (Span, String)> {
        self.let_expr()
    }

    fn let_expr(&mut self) -> Result<Expr, (Span, String)> {
        if !matches!(self.peek(), Tok::Kw(k) if *k == "let") {
            return self.fun_expr();
        }
        let lo = self.span();
        self.bump();
        let rec = self.expect_kw("rec").is_ok();
        let (name, _) = self.ident()?;
        self.expect_punct('=')?;
        let value = Box::new(self.let_expr()?);
        self.expect_kw("in")?;
        let body = Box::new(self.expr()?);
        Ok(Expr::Let { name, rec, value, body, span: Span::union(lo, self.last_span()) })
    }

    fn fun_expr(&mut self) -> Result<Expr, (Span, String)> {
        if !matches!(self.peek(), Tok::Kw(k) if *k == "fun") {
            return self.bin_expr();
        }
        let lo = self.span();
        self.bump();
        let (param, _) = self.ident()?;
        self.expect_punct('|')?;
        let body = Box::new(self.expr()?);
        Ok(Expr::Fun { param, body, span: Span::union(lo, self.last_span()) })
    }

    fn bin_expr(&mut self) -> Result<Expr, (Span, String)> {
        let mut lhs = self.app_expr()?;
        loop {
            let (op, s) = match self.peek() {
                Tok::Punct('+') => (BinOp::Add, self.span()),
                Tok::Punct('-') => (BinOp::Sub, self.span()),
                Tok::Eq => (BinOp::Eq, self.span()),
                _ => break,
            };
            self.bump();
            let rhs = self.app_expr()?;
            lhs = Expr::Bin {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: Span::union(s, self.last_span()),
            };
        }
        Ok(lhs)
    }

    fn app_expr(&mut self) -> Result<Expr, (Span, String)> {
        let mut head = self.proj_expr()?;
        while !self.is_boundary() && !matches!(self.peek(), Tok::Dot) {
            let arg = self.proj_expr()?;
            let span = Span::union(head.span(), self.last_span());
            head = Expr::App {
                fun: Box::new(head),
                arg: Box::new(arg),
                span,
            };
        }
        Ok(head)
    }

    fn proj_expr(&mut self) -> Result<Expr, (Span, String)> {
        let mut e = self.atom()?;
        while matches!(self.peek(), Tok::Dot) {
            self.bump();
            let (field, s) = self.ident()?;
            let span = Span::union(e.span(), s);
            e = Expr::Proj { obj: Box::new(e), field, span };
        }
        Ok(e)
    }

    fn atom(&mut self) -> Result<Expr, (Span, String)> {
        let t = self.peek().clone();
        let s = self.span();
        match t {
            Tok::Int(n) => {
                self.bump();
                Ok(Expr::Int(n, s))
            }
            Tok::BoolLit(b) => {
                self.bump();
                Ok(Expr::Bool(b, s))
            }
            Tok::Str(x) => {
                self.bump();
                Ok(Expr::Str(x, s))
            }
            Tok::Ident(n) => {
                self.bump();
                Ok(Expr::Var(n, s))
            }
            Tok::Hash => {
                self.bump();
                let (label, ls) = self.ident()?;
                let payload = if !self.is_boundary() && !matches!(self.peek(), Tok::Dot) {
                    Some(Box::new(self.atom()?))
                } else {
                    None
                };
                Ok(Expr::Variant { label, payload, span: Span::union(s, ls) })
            }
            Tok::Punct('(') => {
                self.bump();
                let e = self.expr()?;
                self.expect_punct(')')?;
                Ok(e)
            }
            Tok::Punct('{') => self.record(),
            Tok::Kw(k) if k == "match" => self.match_expr(s),
            _ => Err((s, format!("意外的记号 {:?}", t))),
        }
    }

    fn record(&mut self) -> Result<Expr, (Span, String)> {
        let lo = self.span();
        self.bump();
        let mut fields = Vec::new();
        if !matches!(self.peek(), Tok::Punct('}')) {
            loop {
                let (name, _) = self.ident()?;
                self.expect_punct('=')?;
                let e = self.expr()?;
                fields.push((name, e));
                if self.eat_punct(',') || self.eat_punct(';') {
                    if matches!(self.peek(), Tok::Punct('}')) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        self.expect_punct('}')?;
        Ok(Expr::Record { fields, span: Span::union(lo, self.last_span()) })
    }

    fn match_expr(&mut self, lo: Span) -> Result<Expr, (Span, String)> {
        self.bump();
        let scrut = Box::new(self.app_expr()?);
        self.expect_punct('<')?;
        let mut arms = Vec::new();
        loop {
            self.expect_punct('|')?;
            self.expect_punct('#' )?;
            let (label, _) = self.ident()?;
            let binder = if matches!(self.peek(), Tok::Ident(_)) {
                Some(self.ident()?.0)
            } else {
                None
            };
            self.expect_punct(':')?;
            let body = self.expr()?;
            arms.push((label, binder, body));
            if matches!(self.peek(), Tok::Punct('>')) {
                break;
            }
        }
        self.expect_punct('>')?;
        Ok(Expr::Match { scrut, arms, span: Span::union(lo, self.last_span()) })
    }
}

pub fn parse(src: &str) -> Result<Expr, (Span, String)> {
    let toks = lex(src)?;
    let mut p = P { toks, pos: 0 };
    let e = p.expr()?;
    if !matches!(p.peek(), Tok::Eof) {
        return Err((p.span(), "表达式结束后仍有多余记号".into()));
    }
    Ok(e)
}
