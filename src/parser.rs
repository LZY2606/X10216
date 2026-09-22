use crate::ast::{Arm, Expr, ExprK, NodeId};
use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

pub struct Parser {
    src: Vec<u8>,
    pos: usize,
    next_id: u32,
    pub errors: Vec<ParseError>,
}

type P<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(src: &str) -> Self {
        Parser { src: src.as_bytes().to_vec(), pos: 0, next_id: 0, errors: Vec::new() }
    }

    pub fn parse_program(mut self) -> Result<Expr, ParseError> {
        self.skip_ws();
        let mut e = self.parse_expr()?;
        loop {
            self.skip_ws();
            if self.pos >= self.src.len() {
                break;
            }
            if self.peek_str(";") {
                self.pos += 1;
                self.skip_ws();
                if self.pos >= self.src.len() {
                    break;
                }
                let rhs = self.parse_expr()?;
                let sp = Span::merge(e.span, rhs.span);
                e = self.node(
                    sp,
                    ExprK::Let { rec: false, name: "_".to_string(), bind: e, body: rhs },
                );
            } else {
                return Err(self.err(format!("未识别的输入: '{}'", self.rest_head())));
            }
        }
        Ok(e)
    }

    fn nid(&mut self) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        id
    }

    fn node(&mut self, span: Span, k: ExprK) -> Expr {
        Expr { id: self.nid(), span, k: Box::new(k) }
    }

    fn arm(&mut self, tag: String, var: String, body: Expr) -> Arm {
        Arm { id: self.nid(), tag, var, body }
    }

    fn err(&self, msg: String) -> ParseError {
        let end = (self.pos + 1).min(self.src.len());
        ParseError { msg, span: Span::new(self.pos, end.max(self.pos)) }
    }

    fn rest_head(&self) -> String {
        let end = (self.pos + 12).min(self.src.len());
        String::from_utf8_lossy(&self.src[self.pos..end]).to_string()
    }

    fn peek(&self, ch: u8) -> bool {
        self.src.get(self.pos) == Some(&ch)
    }

    fn peek_str(&self, s: &str) -> bool {
        self.src[self.pos..].starts_with(s.as_bytes())
    }

    fn is_ident_start(ch: u8) -> bool {
        ch.is_ascii_alphabetic() || ch == b'_'
    }
    fn is_ident(ch: u8) -> bool {
        ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'!' || ch == b'?'
    }

    fn skip_ws(&mut self) {
        loop {
            while self.pos < self.src.len()
                && (self.src[self.pos] as char).is_whitespace()
            {
                self.pos += 1;
            }
            if self.peek_str("//") {
                while self.pos < self.src.len() && self.src[self.pos] != b'\n' {
                    self.pos += 1;
                }
                continue;
            }
            if self.peek_str("/*") {
                self.pos += 2;
                while self.pos < self.src.len() && !self.peek_str("*/") {
                    self.pos += 1;
                }
                if self.peek_str("*/") {
                    self.pos += 2;
                }
                continue;
            }
            break;
        }
    }

    fn keyword(&mut self, kw: &str) -> bool {
        let start = self.pos;
        self.skip_ws();
        if self.peek_str(kw) {
            let after = self.pos + kw.len();
            let ok_after = self
                .src
                .get(after)
                .map(|c| !Self::is_ident(*c))
                .unwrap_or(true);
            if ok_after {
                self.pos += kw.len();
                return true;
            }
        }
        self.pos = start;
        false
    }

    fn expect(&mut self, ch: u8) -> P<()> {
        self.skip_ws();
        if self.peek(ch) {
            self.pos += 1;
            Ok(())
        } else {
            Err(self.err(format!("期望 '{}'", ch as char)))
        }
    }

    fn parse_ident_raw(&mut self) -> P<String> {
        self.skip_ws();
        let start = self.pos;
        if self.pos < self.src.len() && Self::is_ident_start(self.src[self.pos]) {
            self.pos += 1;
            while self.pos < self.src.len() && Self::is_ident(self.src[self.pos]) {
                self.pos += 1;
            }
            Ok(String::from_utf8_lossy(&self.src[start..self.pos]).to_string())
        } else {
            Err(self.err("期望标识符".to_string()))
        }
    }

    fn parse_ident(&mut self) -> P<String> {
        let id = self.parse_ident_raw()?;
        match id.as_str() {
            "let" | "rec" | "in" | "if" | "then" | "else" | "match" | "with" | "true"
            | "false" => Err(ParseError {
                msg: format!("'{}' 是关键字，不能作为标识符", id),
                span: Span::new(self.pos - id.len(), self.pos),
            }),
            _ => Ok(id),
        }
    }

    fn parse_tag(&mut self) -> P<String> {
        self.skip_ws();
        if self.peek(b'`') {
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.src.len()
                && (self.src[self.pos].is_ascii_alphanumeric() || self.src[self.pos] == b'_')
            {
                self.pos += 1;
            }
            if start == self.pos {
                return Err(self.err("期望标签名".to_string()));
            }
            Ok(String::from_utf8_lossy(&self.src[start..self.pos]).to_string())
        } else {
            Err(self.err("期望 '`' 标签".to_string()))
        }
    }

    fn parse_expr(&mut self) -> P<Expr> {
        if self.keyword("let") {
            return self.parse_let(false);
        }
        if self.keyword("if") {
            return self.parse_if();
        }
        if self.keyword("match") {
            return self.parse_match();
        }
        self.parse_binop(0)
    }

    fn parse_let(&mut self, _kw: bool) -> P<Expr> {
        let start = self.pos - 3;
        let rec = self.keyword("rec");
        let name = self.parse_ident()?;
        self.expect(b'=')?;
        let bind = self.parse_expr()?;
        self.keyword("in");
        let body = self.parse_expr()?;
        let span = Span::new(start, body.span.end as usize);
        Ok(self.node(span, ExprK::Let { rec, name, bind, body }))
    }

    fn parse_if(&mut self) -> P<Expr> {
        let start = self.pos - 2;
        let c = self.parse_expr()?;
        if !self.keyword("then") {
            return Err(self.err("期望 'then'".to_string()));
        }
        let t = self.parse_expr()?;
        if !self.keyword("else") {
            return Err(self.err("期望 'else'".to_string()));
        }
        let e = self.parse_expr()?;
        let span = Span::new(start, e.span.end as usize);
        Ok(self.node(span, ExprK::If { c, t, e }))
    }

    fn parse_match(&mut self) -> P<Expr> {
        let start = self.pos - 5;
        let e = self.parse_expr()?;
        if !self.keyword("with") {
            return Err(self.err("期望 'with'".to_string()));
        }
        let mut arms = Vec::new();
        loop {
            self.skip_ws();
            if self.peek_str("|") {
                self.pos += 1;
            } else if !arms.is_empty() {
                break;
            }
            let tag = self.parse_tag()?;
            let var = if self.pos < self.src.len() && Self::is_ident_start(self.src[self.pos]) {
                self.parse_ident_raw()?
            } else {
                "_unit".to_string()
            };
            self.skip_ws();
            if !self.peek_str("=>") {
                return Err(self.err("期望 '=>'".to_string()));
            }
            self.pos += 2;
            let body = self.parse_expr()?;
            arms.push(self.arm(tag, var, body));
            self.skip_ws();
            if !self.peek_str("|") {
                break;
            }
        }
        let end = arms.last().map(|a| a.body.span.end).unwrap_or(e.span.end);
        let span = Span::new(start, end as usize);
        Ok(self.node(span, ExprK::Match { e, arms }))
    }

    fn ops() -> &'static [(&'static str, i32)] {
        &[
            ("||", 1),
            ("&&", 2),
            ("==", 3), ("!=", 3),
            ("<=", 3), (">=", 3), ("<", 3), (">", 3),
            ("++", 4),
            ("+", 5), ("-", 5),
            ("*", 6), ("/", 6),
        ]
    }

    fn op_prec(&self) -> Option<(&'static str, i32)> {
        self.skip_ws();
        let mut best: Option<(&'static str, i32)> = None;
        for (op, p) in Self::ops() {
            if self.peek_str(op) {
                if best.map(|(_, bp)| *p > bp).unwrap_or(true) {
                    best = Some((op, *p));
                }
            }
        }
        best
    }

    fn parse_binop(&mut self, min_prec: i32) -> P<Expr> {
        let mut left = self.parse_app()?;
        loop {
            let save = self.pos;
            match self.op_prec() {
                Some((op, p)) if p >= min_prec => {
                    self.pos = save;
                    self.skip_ws();
                    self.pos += op.len();
                    let right = self.parse_binop(p + 1)?;
                    let span = Span::merge(left.span, right.span);
                    left = self.node(
                        span,
                        ExprK::Op { op: op.to_string(), l: left, r: right },
                    );
                }
                _ => {
                    self.pos = save;
                    break;
                }
            }
        }
        Ok(left)
    }

    fn parse_app(&mut self) -> P<Expr> {
        let mut f = self.parse_atom()?;
        loop {
            let save = self.pos;
            self.skip_ws();
            if self.starts_atom() {
                let a = self.parse_atom()?;
                let span = Span::merge(f.span, a.span);
                f = self.node(span, ExprK::App { f, a });
            } else if self.peek(b'.') {
                self.pos += 1;
                let name = self.parse_ident_raw()?;
                let span = Span::new(f.span.start as usize, self.pos);
                f = self.node(span, ExprK::Field { e: f, name });
            } else {
                self.pos = save;
                break;
            }
        }
        Ok(f)
    }

    fn starts_atom(&mut self) -> bool {
        self.pos < self.src.len()
            && (self.src[self.pos].is_ascii_digit()
                || Self::is_ident_start(self.src[self.pos])
                || self.peek(b'(')
                || self.peek(b'{')
                || self.peek(b'`'))
    }

    fn parse_atom(&mut self) -> P<Expr> {
        self.skip_ws();
        let start = self.pos;
        if self.peek(b'(') {
            self.pos += 1;
            let e = self.parse_expr()?;
            self.expect(b')')?;
            return Ok(e);
        }
        if self.peek(b'{') {
            return self.parse_record(start);
        }
        if self.peek(b'`') {
            let tag = self.parse_tag()?;
            let payload = if self.starts_atom() {
                self.parse_atom()?
            } else {
                self.unit_expr(Span::new(self.pos, self.pos))
            };
            let span = Span::merge(Span::new(start, start + 1), payload.span);
            return Ok(self.node(span, ExprK::Variant { tag, payload }));
        }
        if self.pos < self.src.len() && self.src[self.pos].is_ascii_digit() {
            while self.pos < self.src.len() && self.src[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
            let n: i64 = String::from_utf8_lossy(&self.src[start..self.pos])
                .parse()
                .unwrap_or(0);
            return Ok(self.node(Span::new(start, self.pos), ExprK::Int(n)));
        }
        if self.peek(b'"') {
            return self.parse_string(start);
        }
        if Self::is_ident_start(*self.src.get(self.pos).ok_or_else(|| {
            self.err("意外的输入结束".to_string())
        })?) {
            let id = self.parse_ident_raw()?;
            let span = Span::new(start, self.pos);
            return match id.as_str() {
                "true" => Ok(self.node(span, ExprK::Bool(true))),
                "false" => Ok(self.node(span, ExprK::Bool(false))),
                "fun" => self.parse_lam(start),
                _ => Ok(self.node(span, ExprK::Var(id))),
            };
        }
        Err(self.err("期望表达式".to_string()))
    }

    fn unit_expr(&mut self, sp: Span) -> Expr {
        self.node(sp, ExprK::Record(Vec::new()))
    }

    fn parse_lam(&mut self, start: usize) -> P<Expr> {
        let param = self.parse_ident()?;
        self.skip_ws();
        if !self.peek_str("=>") {
            return Err(self.err("期望 '=>'".to_string()));
        }
        self.pos += 2;
        let body = self.parse_expr()?;
        let span = Span::new(start, body.span.end as usize);
        Ok(self.node(span, ExprK::Lam { param, body }))
    }

    fn parse_string(&mut self, start: usize) -> P<Expr> {
        self.pos += 1;
        let mut out = String::new();
        while self.pos < self.src.len() && !self.peek(b'"') {
            if self.peek(b'\\') && self.pos + 1 < self.src.len() {
                self.pos += 1;
                let c = self.src[self.pos] as char;
                out.push(match c {
                    'n' => '\n',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    other => other,
                });
            } else {
                out.push(self.src[self.pos] as char);
            }
            self.pos += 1;
        }
        self.expect(b'"')?;
        Ok(self.node(Span::new(start, self.pos), ExprK::Str(out)))
    }

    fn parse_record(&mut self, start: usize) -> P<Expr> {
        self.pos += 1;
        let mut fields: Vec<(String, Expr)> = Vec::new();
        self.skip_ws();
        if self.peek(b'}') {
            self.pos += 1;
            return Ok(self.node(Span::new(start, self.pos), ExprK::Record(fields)));
        }
        loop {
            self.skip_ws();
            if self.peek_str("..") {
                self.pos += 2;
                let e = self.parse_atom()?;
                // 展开语法糖：由约束生成器处理；这里记录为特殊字段 "$rest"
                fields.push(("..".to_string(), e));
            } else {
                let name = self.parse_ident_raw()?;
                self.expect(b'=')?;
                let e = self.parse_expr()?;
                fields.push((name, e));
            }
            self.skip_ws();
            if self.peek(b',') {
                self.pos += 1;
                self.skip_ws();
                if self.peek(b'}') {
                    self.pos += 1;
                    break;
                }
            } else {
                self.expect(b'}')?;
                break;
            }
        }
        Ok(self.node(Span::new(start, self.pos), ExprK::Record(fields))))
    }
}

pub fn parse(src: &str) -> Result<Expr, ParseError> {
    Parser::new(src).parse_program()
}
