//! Lexer, parser and stable node identities for the tiny HM language.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo: lo as u32, hi: hi as u32 }
    }
    pub fn merge(a: Span, b: Span) -> Span {
        Span { lo: a.lo.min(b.lo), hi: a.hi.max(b.hi) }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Nid(pub u32);

#[derive(Clone, Debug)]
pub struct NodeId {
    pub nid: Nid,
    pub path: String,
    pub chash: u64,
}

fn fnv1a32(s: &str) -> u32 {
    let mut h: u32 = 0x811c9dc5;
    for b in s.bytes() {
        h ^= b as u32;
        h = h.wrapping_mul(0x01000193);
    }
    h
}

fn fnv1a64(parts: &[String]) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for p in parts {
        for b in p.bytes() {
            h ^= b as u64;
            h = h.wrapping_mul(0x100000001b3);
        }
        h ^= 0xff;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pat {
    PVar { name: String, span: Span },
    PTag { tag: String, payload: Option<Box<Pat>>, span: Span },
}

impl Pat {
    pub fn span(&self) -> Span {
        match self {
            Pat::PVar { span, .. } | Pat::PTag { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Var { name: String, span: Span },
    Int { value: i64, span: Span },
    Bool { value: bool, span: Span },
    Str { value: String, span: Span },
    Unit { span: Span },
    Abs { param: String, body: Box<Expr>, span: Span },
    App { fun: Box<Expr>, arg: Box<Expr>, span: Span },
    Let { rec: bool, name: String, rhs: Box<Expr>, body: Box<Expr>, span: Span },
    If { cond: Box<Expr>, then_e: Box<Expr>, else_e: Box<Expr>, span: Span },
    Record { fields: Vec<(String, Expr)>, ext: Option<Box<Expr>>, span: Span },
    Proj { record: Box<Expr>, label: String, span: Span },
    Tag { tag: String, payload: Option<Box<Expr>>, span: Span },
    Match { scrut: Box<Expr>, arms: Vec<(Pat, Expr)>, span: Span },
    Seq { left: Box<Expr>, right: Box<Expr>, span: Span },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Var { span, .. }
            | Expr::Int { span, .. }
            | Expr::Bool { span, .. }
            | Expr::Str { span, .. }
            | Expr::Unit { span }
            | Expr::Abs { span, .. }
            | Expr::App { span, .. }
            | Expr::Let { span, .. }
            | Expr::If { span, .. }
            | Expr::Record { span, .. }
            | Expr::Proj { span, .. }
            | Expr::Tag { span, .. }
            | Expr::Match { span, .. }
            | Expr::Seq { span, .. } => *span,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub root: Expr,
    pub ids: std::collections::HashMap<Nid, NodeId>,
    pub nid_of: std::collections::HashMap<*const Expr, Nid>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error [{}..{}]: {}", self.span.lo, self.span.hi, self.message)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Str(String),
    Name(String),
    Tag(String),
    Let,
    Rec,
    In,
    Fn,
    FatArrow,
    If,
    Then,
    Else,
    Match,
    With,
    True,
    False,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Eq,
    Semicolon,
    Comma,
    Dot,
    Pipe,
    Underscore,
    Eof,
}

#[derive(Clone, Debug)]
struct Token {
    tok: Tok,
    span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, ParseError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mk = |tok: Tok, lo: usize, hi: usize| Token { tok, span: Span::new(lo, hi) };
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '/' && i + 1 < b.len() && b[i + 1] == b'/' {
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        if c == '/' && i + 1 < b.len() && b[i + 1] == b'*' {
            let lo = i;
            i += 2;
            let mut depth = 1;
            while i < b.len() && depth > 0 {
                if i + 1 < b.len() && b[i] == b'/' && b[i + 1] == b'*' {
                    depth += 1;
                    i += 2;
                } else if i + 1 < b.len() && b[i] == b'*' && b[i + 1] == b'/' {
                    depth -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if depth > 0 {
                return Err(ParseError { span: Span::new(lo, b.len()), message: "unterminated block comment".into() });
            }
            continue;
        }
        if c.is_ascii_digit() {
            let lo = i;
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let v: i64 = src[lo..i].parse().unwrap_or(i64::MAX);
            out.push(mk(Tok::Int(v), lo, i));
            continue;
        }
        if c == '"' {
            let lo = i;
            i += 1;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    s.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        _ => e,
                    });
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err(ParseError { span: Span::new(lo, i), message: "unterminated string".into() });
            }
            i += 1;
            out.push(mk(Tok::Str(s), lo, i));
            continue;
        }
        if c == '`' {
            let lo = i;
            i += 1;
            let lo2 = i;
            while i < b.len() && ((b[i] as char).is_alphanumeric() || b[i] == b'_' || b[i] == b'-') {
                i += 1;
            }
            if lo2 == i {
                return Err(ParseError { span: Span::new(lo, i), message: "empty tag".into() });
            }
            out.push(mk(Tok::Tag(src[lo2..i].to_string()), lo, i));
            continue;
        }
        if c.is_alphabetic() || c == '_' {
            let lo = i;
            while i < b.len() && ((b[i] as char).is_alphanumeric() || b[i] == b'_' || b[i] == b'\'') {
                i += 1;
            }
            let word = &src[lo..i];
            let tok = match word {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fn" => Tok::Fn,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "match" => Tok::Match,
                "with" => Tok::With,
                "true" => Tok::True,
                "false" => Tok::False,
                "_" => Tok::Underscore,
                _ => Tok::Name(word.to_string()),
            };
            out.push(mk(tok, lo, i));
            continue;
        }
        let (tok, n) = match c {
            '(' => (Tok::LParen, 1),
            ')' => (Tok::RParen, 1),
            '{' => (Tok::LBrace, 1),
            '}' => (Tok::RBrace, 1),
            '=' => (Tok::Eq, 1),
            ';' => (Tok::Semicolon, 1),
            ',' => (Tok::Comma, 1),
            '.' => (Tok::Dot, 1),
            '|' => (Tok::Pipe, 1),
            '-' if i + 1 < b.len() && b[i + 1] == b'>' => (Tok::FatArrow, 2),
            _ => {
                return Err(ParseError { span: Span::new(i, i + 1), message: format!("unexpected character {:?}", c) });
            }
        };
        out.push(mk(tok, i, i + n));
        i += n;
    }
    out.push(mk(Tok::Eof, b.len(), b.len()));
    Ok(out)
}

struct Parser {
    toks: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            Ok(self.bump())
        } else {
            Err(ParseError { span: self.span(), message: format!("expected {}, found {:?}", what, self.peek()) })
        }
    }

    fn parse_program(&mut self) -> Result<Expr, ParseError> {
        let e = self.parse_seq()?;
        self.expect(Tok::Eof, "end of input")?;
        Ok(e)
    }

    fn parse_seq(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_let()?;
        while self.eat(&Tok::Semicolon) {
            let lo = e.span().lo;
            if matches!(self.peek(), Tok::Eof) {
                break;
            }
            let r = self.parse_let()?;
            let hi = r.span().hi;
            e = Expr::Seq { left: Box::new(e), right: Box::new(r), span: Span::new(lo, hi) };
        }
        Ok(e)
    }

    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        if !matches!(self.peek(), Tok::Let) {
            return self.parse_fn();
        }
        let lo = self.bump().span.lo;
        let rec = self.eat(&Tok::Rec);
        let name = match self.bump().tok {
            Tok::Name(n) => n,
            other => {
                return Err(ParseError { span: self.span(), message: format!("expected binding name, found {:?}", other) })
            }
        };
        self.expect(Tok::Eq, "'='")?;
        let rhs = self.parse_seq()?;
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_seq()?;
        let hi = body.span().hi;
        Ok(Expr::Let { rec, name, rhs: Box::new(rhs), body: Box::new(body), span: Span::new(lo, hi) })
    }

    fn parse_fn(&mut self) -> Result<Expr, ParseError> {
        if !matches!(self.peek(), Tok::Fn) {
            return self.parse_if();
        }
        let lo = self.bump().span.lo;
        let param = match self.bump().tok {
            Tok::Name(n) => n,
            Tok::Underscore => "_".to_string(),
            other => {
                return Err(ParseError { span: self.span(), message: format!("expected parameter name, found {:?}", other) })
            }
        };
        self.expect(Tok::FatArrow, "'=>'")?;
        let body = self.parse_seq()?;
        let hi = body.span().hi;
        Ok(Expr::Abs { param, body: Box::new(body), span: Span::new(lo, hi) })
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        if !matches!(self.peek(), Tok::If) {
            return self.parse_app();
        }
        let lo = self.bump().span.lo;
        let cond = self.parse_seq()?;
        self.expect(Tok::Then, "'then'")?;
        let then_e = self.parse_seq()?;
        self.expect(Tok::Else, "'else'")?;
        let else_e = self.parse_seq()?;
        let hi = else_e.span().hi;
        Ok(Expr::If { cond: Box::new(cond), then_e: Box::new(then_e), else_e: Box::new(else_e), span: Span::new(lo, hi) })
    }

    fn parse_app(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_postfix()?;
        loop {
            let start = match self.peek() {
                Tok::Int(_) | Tok::Str(_) | Tok::Name(_) | Tok::Tag(_) | Tok::True | Tok::False | Tok::LParen
                | Tok::LBrace => self.span().lo,
                _ => break,
            };
            let arg = self.parse_postfix()?;
            let hi = arg.span().hi;
            e = Expr::App { fun: Box::new(e), arg: Box::new(arg), span: Span::new(start.min(e.span().lo), hi) };
        }
        Ok(e)
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut e = self.parse_atom()?;
        while self.eat(&Tok::Dot) {
            let lab = match self.bump().tok {
                Tok::Name(n) => n,
                other => return Err(ParseError { span: self.span(), message: format!("expected field label, found {:?}", other) }),
            };
            let hi = self.toks[self.pos.saturating_sub(1)].span.hi;
            e = Expr::Proj { record: Box::new(e), label: lab, span: Span::new(e.span().lo, hi) };
        }
        Ok(e)
    }

    fn parse_pat(&mut self) -> Result<Pat, ParseError> {
        match self.peek().clone() {
            Tok::Underscore => {
                let t = self.bump();
                Ok(Pat::PVar { name: "_".into(), span: t.span })
            }
            Tok::Name(n) => {
                let t = self.bump();
                Ok(Pat::PVar { name: n, span: t.span })
            }
            Tok::Tag(tag) => {
                let t = self.bump();
                let payload = if matches!(self.peek(), Tok::LParen) {
                    Some(Box::new(self.parse_pat()?))
                } else {
                    None
                };
                Ok(Pat::PTag { tag, payload, span: t.span })
            }
            other => Err(ParseError { span: self.span(), message: format!("expected pattern, found {:?}", other) }),
        }
    }

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        let t = self.bump();
        let lo = t.span.lo;
        match t.tok {
            Tok::Int(v) => Ok(Expr::Int { value: v, span: t.span }),
            Tok::Str(s) => Ok(Expr::Str { value: s, span: t.span }),
            Tok::True => Ok(Expr::Bool { value: true, span: t.span }),
            Tok::False => Ok(Expr::Bool { value: false, span: t.span }),
            Tok::Name(n) => Ok(Expr::Var { name: n, span: t.span }),
            Tok::Tag(tag) => {
                let payload = if self.eat(&Tok::LParen) {
                    let p = self.parse_seq()?;
                    self.expect(Tok::RParen, "')'")?;
                    Some(Box::new(p))
                } else {
                    None
                };
                let hi = self.toks[self.pos.saturating_sub(1)].span.hi;
                Ok(Expr::Tag { tag, payload, span: Span::new(lo, hi) })
            }
            Tok::LParen => {
                if self.eat(&Tok::RParen) {
                    return Ok(Expr::Unit { span: t.span });
                }
                let e = self.parse_seq()?;
                self.expect(Tok::RParen, "')'")?;
                Ok(e)
            }
            Tok::LBrace => self.parse_record_rest(lo),
            Tok::Match => {
                let scrut = self.parse_seq()?;
                self.expect(Tok::With, "'with'")?;
                let mut arms = Vec::new();
                loop {
                    self.eat(&Tok::Pipe);
                    let pat = self.parse_pat()?;
                    self.expect(Tok::FatArrow, "'=>'")?;
                    let body = self.parse_seq()?;
                    arms.push((pat, body));
                    if !self.eat(&Tok::Pipe) {
                        break;
                    }
                }
                let hi = arms.last().map(|(_, b)| b.span().hi).unwrap_or(lo);
                Ok(Expr::Match { scrut: Box::new(scrut), arms, span: Span::new(lo, hi) })
            }
            other => Err(ParseError { span: t.span, message: format!("unexpected token {:?}", other) }),
        }
    }

    fn parse_record_rest(&mut self, lo: u32) -> Result<Expr, ParseError> {
        let mut fields: Vec<(String, Expr)> = Vec::new();
        let mut labels = std::collections::HashSet::new();
        let mut ext = None;
        if !self.eat(&Tok::RBrace) {
            loop {
                if self.eat(&Tok::Pipe) {
                    let base = self.parse_postfix()?;
                    ext = Some(Box::new(base));
                    break;
                }
                let lab = match self.bump().tok {
                    Tok::Name(n) => n,
                    other => return Err(ParseError { span: self.span(), message: format!("expected field label, found {:?}", other) }),
                };
                self.expect(Tok::Eq, "'='")?;
                let val = self.parse_seq()?;
                if !labels.insert(lab.clone()) {
                    return Err(ParseError { span: val.span(), message: format!("duplicate field '{}'", lab) });
                }
                fields.push((lab, val));
                if self.eat(&Tok::Pipe) {
                    let base = self.parse_postfix()?;
                    ext = Some(Box::new(base));
                    break;
                }
                if !self.eat(&Tok::Comma) {
                    break;
                }
            }
            self.expect(Tok::RBrace, "'}}'")?;
        }
        let hi = self.toks[self.pos.saturating_sub(1)].span.hi;
        Ok(Expr::Record { fields, ext, span: Span::new(lo, hi) })
    }
}

/// Assign stable identities to every expression node by tree path.
pub fn assign_ids(root: &Expr) -> (std::collections::HashMap<Nid, NodeId>, std::collections::HashMap<*const Expr, Nid>) {
    let mut ids: std::collections::HashMap<Nid, NodeId> = std::collections::HashMap::new();
    let mut nid_of: std::collections::HashMap<*const Expr, Nid> = std::collections::HashMap::new();
    fn children(e: &Expr) -> Vec<(String, &Expr)> {
        match e {
            Expr::Var { .. } | Expr::Int { .. } | Expr::Bool { .. } | Expr::Str { .. } | Expr::Unit { .. } => vec![],
            Expr::Abs { body, .. } => vec![("b".into(), body)],
            Expr::App { fun, arg, .. } => vec![("f".into(), fun), ("a".into(), arg)],
            Expr::Let { rhs, body, .. } => vec![("r".into(), rhs), ("b".into(), body)],
            Expr::If { cond, then_e, else_e, .. } => {
                vec![("c".into(), cond), ("t".into(), then_e), ("e".into(), else_e)]
            }
            Expr::Record { fields, ext, .. } => {
                let mut v: Vec<(String, &Expr)> = fields.iter().map(|(l, e)| (format!("f:{}", l), e)).collect();
                if let Some(x) = ext {
                    v.push(("x".into(), x));
                }
                v
            }
            Expr::Proj { record, .. } => vec![("r".into(), record)],
            Expr::Tag { payload, .. } => payload.iter().map(|p| ("p".into(), p.as_ref())).collect(),
            Expr::Match { scrut, arms, .. } => {
                let mut v = vec![("s".into(), scrut.as_ref())];
                for (i, (_, b)) in arms.iter().enumerate() {
                    v.push((format!("m{}", i), b));
                }
                v
            }
            Expr::Seq { left, right, .. } => vec![("l".into(), left), ("r".into(), right)],
        }
    }
    fn kind_sig(e: &Expr) -> String {
        match e {
            Expr::Var { .. } => "var".into(),
            Expr::Int { .. } => "int".into(),
            Expr::Bool { .. } => "bool".into(),
            Expr::Str { .. } => "str".into(),
            Expr::Unit { .. } => "unit".into(),
            Expr::Abs { .. } => "abs".into(),
            Expr::App { .. } => "app".into(),
            Expr::Let { rec, .. } => if *rec { "letrec".into() } else { "let".into() },
            Expr::If { .. } => "if".into(),
            Expr::Record { fields, ext, .. } => {
                let mut s = String::from("rec{");
                for (l, _) in fields {
                    s.push_str(l);
                    s.push(',');
                }
                if ext.is_some() {
                    s.push('+');
                }
                s.push('}');
                s
            }
            Expr::Proj { label, .. } => format!("proj:{}", label),
            Expr::Tag { tag, payload, .. } => {
                if payload.is_some() {
                    format!("tag:{}!", tag)
                } else {
                    format!("tag:{}", tag)
                }
            }
            Expr::Match { arms, .. } => format!("match:{}", arms.len()),
            Expr::Seq { .. } => "seq".into(),
        }
    }
    fn walk(e: &Expr, path: String, ids: &mut std::collections::HashMap<Nid, NodeId>, nid_of: &mut std::collections::HashMap<*const Expr, Nid>) -> u64 {
        let mut parts = vec![kind_sig(e)];
        for (seg, c) in children(e) {
            let h = walk(c, format!("{}/{}", path, seg), ids, nid_of);
            parts.push(format!("{}:{:x}", seg, h));
        }
        let chash = fnv1a64(&parts);
        let nid = Nid(fnv1a32(&path));
        ids.insert(nid, NodeId { nid, path: path.clone(), chash });
        nid_of.insert(e as *const Expr, nid);
        chash
    }
    walk(root, "$".into(), &mut ids, &mut nid_of);
    (ids, nid_of)
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    let toks = lex(src)?;
    let mut p = Parser { toks, pos: 0 };
    let root = p.parse_program()?;
    let (ids, nid_of) = assign_ids(&root);
    Ok(Program { root, ids, nid_of })
}
