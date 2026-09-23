//! Lexer, parser and alpha-invariant identity keys for the mini HM language.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt::Write as _;

pub type Span = (usize, usize);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node {
    pub id: u32,
    pub span: Span,
    pub kind: ExprK,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExprK {
    Int(i64),
    Bool(bool),
    Var(String),
    Fun { param: String, body: Box<Node> },
    App(Box<Node>, Box<Node>),
    Let { name: String, rec_: bool, value: Box<Node>, body: Box<Node> },
    If(Box<Node>, Box<Node>, Box<Node>),
    Record(Vec<(String, Node)>),
    Proj(Box<Node>, String),
    Variant(String, Box<Node>),
    Match(Box<Node>, Vec<(String, String, Node)>),
}

#[derive(Debug, Clone, PartialEq)]
enum Tok {
    Let, Rec, In, Fun, Arrow, If, Then, Else, Match, With, Bar,
    Eq, LBrace, RBrace, LParen, RParen, Comma, Dot, Backtick,
    Ident(String), Int(i64), True, False, Eof,
}

#[derive(Debug, Clone)]
struct Lexed {
    tok: Tok,
    span: Span,
}

fn lex(src: &str) -> Result<Vec<Lexed>, String> {
    let b = src.as_bytes();
    let mut i = 0usize;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i] as char;
        if c.is_whitespace() { i += 1; continue; }
        if c == '-' && i + 1 < b.len() && b[i + 1] == b'-' {
            while i < b.len() && b[i] != b'\n' { i += 1; }
            continue;
        }
        let start = i;
        let tok = match c {
            '=' => { i += 1; Tok::Eq }
            '{' => { i += 1; Tok::LBrace }
            '}' => { i += 1; Tok::RBrace }
            '(' => { i += 1; Tok::LParen }
            ')' => { i += 1; Tok::RParen }
            ',' => { i += 1; Tok::Comma }
            '.' => { i += 1; Tok::Dot }
            '`' => { i += 1; Tok::Backtick }
            '|' => { i += 1; Tok::Bar }
            '-' => {
                if i + 1 < b.len() && b[i + 1] == b'>' { i += 2; Tok::Arrow }
                else { return Err(format!("unexpected '-' at {}", i)); }
            }
            _ if c.is_ascii_digit() => {
                let mut v: i64 = 0;
                while i < b.len() && (b[i] as char).is_ascii_digit() {
                    v = v * 10 + (b[i] - b'0') as i64;
                    i += 1;
                }
                Tok::Int(v)
            }
            _ if c.is_alphabetic() || c == '_' => {
                let s0 = i;
                while i < b.len() && ((b[i] as char).is_alphanumeric() || b[i] == b'_') { i += 1; }
                match &src[s0..i] {
                    "let" => Tok::Let, "rec" => Tok::Rec, "in" => Tok::In,
                    "fun" => Tok::Fun, "if" => Tok::If, "then" => Tok::Then,
                    "else" => Tok::Else, "match" => Tok::Match, "with" => Tok::With,
                    "true" => Tok::True, "false" => Tok::False,
                    s => Tok::Ident(s.to_string()),
                }
            }
            _ => return Err(format!("unexpected char {:?} at {}", c, i)),
        };
        out.push(Lexed { tok, span: (start, i) });
    }
    out.push(Lexed { tok: Tok::Eof, span: (src.len(), src.len()) });
    Ok(out)
}

pub struct Parser {
    toks: Vec<Lexed>,
    pos: usize,
    next_id: u32,
}

impl Parser {
    pub fn new(src: &str) -> Result<Self, String> {
        Ok(Parser { toks: lex(src)?, pos: 0, next_id: 0 })
    }

    fn peek(&self) -> &Tok { &self.toks[self.pos].tok }
    fn span_of(&self) -> Span { self.toks[self.pos].span }
    fn bump(&mut self) -> Lexed { let t = self.toks[self.pos].clone(); self.pos += 1; t }

    fn expect(&mut self, t: Tok) -> Result<Lexed, String> {
        if *self.peek() == t { Ok(self.bump()) }
        else { Err(format!("expected {:?}, found {:?} at {:?}", t, self.peek(), self.span_of())) }
    }

    fn node(&mut self, start: usize, kind: ExprK) -> Node {
        let end = self.toks[self.pos.saturating_sub(1)].span.1;
        let id = self.next_id;
        self.next_id += 1;
        Node { id, span: (start, end), kind }
    }

    pub fn parse_expr(&mut self) -> Result<Node, String> {
        match self.peek().clone() {
            Tok::Let => self.parse_let(),
            Tok::Fun => {
                let s = self.span_of().0;
                self.bump();
                let param = self.parse_ident()?;
                self.expect(Tok::Arrow)?;
                let body = self.parse_expr()?;
                Ok(self.node(s, ExprK::Fun { param, body: Box::new(body) }))
            }
            Tok::If => {
                let s = self.span_of().0;
                self.bump();
                let c = self.parse_expr()?;
                self.expect(Tok::Then)?;
                let t = self.parse_expr()?;
                self.expect(Tok::Else)?;
                let e = self.parse_expr()?;
                Ok(self.node(s, ExprK::If(Box::new(c), Box::new(t), Box::new(e))))
            }
            Tok::Match => {
                let s = self.span_of().0;
                self.bump();
                let scrut = self.parse_expr()?;
                self.expect(Tok::With)?;
                let mut arms = Vec::new();
                while *self.peek() == Tok::Bar {
                    self.bump();
                    self.expect(Tok::Backtick)?;
                    let label = self.parse_ident()?;
                    let binder = self.parse_ident()?;
                    self.expect(Tok::Arrow)?;
                    let body = self.parse_expr()?;
                    arms.push((label, binder, body));
                }
                if arms.is_empty() { return Err("match needs at least one arm".into()); }
                Ok(self.node(s, ExprK::Match(Box::new(scrut), arms)))
            }
            _ => self.parse_app(),
        }
    }

    fn parse_let(&mut self) -> Result<Node, String> {
        let s = self.span_of().0;
        self.bump();
        let rec_ = if *self.peek() == Tok::Rec { self.bump(); true } else { false };
        let name = self.parse_ident()?;
        self.expect(Tok::Eq)?;
        let value = self.parse_expr()?;
        self.expect(Tok::In)?;
        let body = self.parse_expr()?;
        Ok(self.node(s, ExprK::Let { name, rec_, value: Box::new(value), body: Box::new(body) }))
    }

    fn parse_ident(&mut self) -> Result<String, String> {
        match self.peek().clone() {
            Tok::Ident(s) => { self.bump(); Ok(s) }
            t => Err(format!("expected identifier, found {:?} at {:?}", t, self.span_of())),
        }
    }

    fn parse_app(&mut self) -> Result<Node, String> {
        let mut e = self.parse_proj()?;
        loop {
            match self.peek() {
                Tok::Ident(_) | Tok::Int(_) | Tok::True | Tok::False | Tok::LParen
                | Tok::LBrace | Tok::Backtick => {
                    let s = e.span.0;
                    let arg = self.parse_proj()?;
                    e = self.node(s, ExprK::App(Box::new(e), Box::new(arg)));
                }
                _ => break,
            }
        }
        Ok(e)
    }

    fn parse_proj(&mut self) -> Result<Node, String> {
        let mut e = self.parse_atom()?;
        while *self.peek() == Tok::Dot {
            let s = e.span.0;
            self.bump();
            let label = self.parse_ident()?;
            e = self.node(s, ExprK::Proj(Box::new(e), label));
        }
        Ok(e)
    }

    fn parse_atom(&mut self) -> Result<Node, String> {
        let s = self.span_of().0;
        match self.peek().clone() {
            Tok::Int(v) => { self.bump(); Ok(self.node(s, ExprK::Int(v))) }
            Tok::True => { self.bump(); Ok(self.node(s, ExprK::Bool(true))) }
            Tok::False => { self.bump(); Ok(self.node(s, ExprK::Bool(false))) }
            Tok::Ident(name) => { self.bump(); Ok(self.node(s, ExprK::Var(name))) }
            Tok::LParen => {
                self.bump();
                let e = self.parse_expr()?;
                self.expect(Tok::RParen)?;
                Ok(e)
            }
            Tok::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                if *self.peek() != Tok::RBrace {
                    loop {
                        let label = self.parse_ident()?;
                        self.expect(Tok::Eq)?;
                        let val = self.parse_expr()?;
                        fields.push((label, val));
                        if *self.peek() == Tok::Comma { self.bump(); } else { break; }
                    }
                }
                self.expect(Tok::RBrace)?;
                Ok(self.node(s, ExprK::Record(fields)))
            }
            Tok::Backtick => {
                self.bump();
                let label = self.parse_ident()?;
                let payload = self.parse_atom()?;
                Ok(self.node(s, ExprK::Variant(label, Box::new(payload))))
            }
            t => Err(format!("unexpected {:?} at {:?}", t, self.span_of())),
        }
    }
}

pub fn parse(src: &str) -> Result<Node, String> {
    let mut p = Parser::new(src)?;
    let e = p.parse_expr()?;
    if *p.peek() != Tok::Eof {
        return Err(format!("trailing input at {:?}", p.span_of()));
    }
    Ok(e)
}

// ---------- alpha-invariant identity keys ----------

/// Structural, alpha-invariant key for a subtree. Bound variables (fun
/// parameters, let-bound names, match binders) are replaced by de Bruijn
/// levels, so renaming a binder does not change the key of its subtree.
pub fn node_key(n: &Node, env: &mut Vec<String>) -> String {
    let mut s = String::new();
    write_key(n, env, &mut s);
    s
}

fn write_key(n: &Node, env: &mut Vec<String>, out: &mut String) {
    match &n.kind {
        ExprK::Int(v) => { let _ = write!(out, "i{};", v); }
        ExprK::Bool(b) => { let _ = write!(out, "b{};", b); }
        ExprK::Var(name) => {
            match env.iter().rposition(|x| x == name) {
                Some(d) => { let _ = write!(out, "v#{};", d); }
                None => { let _ = write!(out, "v${};", name); }
            }
        }
        ExprK::Fun { param, body } => {
            out.push_str("fun(");
            env.push(param.clone());
            write_key(body, env, out);
            env.pop();
            out.push(')');
        }
        ExprK::App(f, a) => {
            out.push_str("app(");
            write_key(f, env, out);
            write_key(a, env, out);
            out.push(')');
        }
        ExprK::Let { name, rec_, value, body } => {
            let _ = write!(out, "let{}(", rec_);
            if *rec_ {
                env.push(name.clone());
                write_key(value, env, out);
                write_key(body, env, out);
                env.pop();
            } else {
                write_key(value, env, out);
                env.push(name.clone());
                write_key(body, env, out);
                env.pop();
            }
            out.push(')');
        }
        ExprK::If(c, t, e) => {
            out.push_str("if(");
            write_key(c, env, out);
            write_key(t, env, out);
            write_key(e, env, out);
            out.push(')');
        }
        ExprK::Record(fields) => {
            out.push_str("rec{");
            for (l, v) in fields {
                let _ = write!(out, "{}=", l);
                write_key(v, env, out);
            }
            out.push('}');
        }
        ExprK::Proj(e, l) => {
            out.push_str("proj.");
            out.push_str(l);
            out.push('(');
            write_key(e, env, out);
            out.push(')');
        }
        ExprK::Variant(l, p) => {
            let _ = write!(out, "variant`{}(", l);
            write_key(p, env, out);
            out.push(')');
        }
        ExprK::Match(scrut, arms) => {
            out.push_str("match(");
            write_key(scrut, env, out);
            for (label, binder, body) in arms {
                let _ = write!(out, "|`{}=", label);
                env.push(binder.clone());
                write_key(body, env, out);
                env.pop();
            }
            out.push(')');
        }
    }
}

/// Collect (node_id, key) for every node, with binder scope tracked so keys
/// are alpha-invariant. Also returns the binder-scope path per node (names of
/// enclosing binders), used as part of the incremental cache context.
pub fn collect_keys(root: &Node) -> BTreeMap<u32, (String, Vec<String>)> {
    let mut map = BTreeMap::new();
    let mut env: Vec<String> = Vec::new();
    let mut scope: Vec<String> = Vec::new();
    collect_keys_rec(root, &mut env, &mut scope, &mut map);
    map
}

fn collect_keys_rec(
    n: &Node,
    env: &mut Vec<String>,
    scope: &mut Vec<String>,
    map: &mut BTreeMap<u32, (String, Vec<String>)>,
) {
    let key = node_key(n, &mut env.clone());
    map.insert(n.id, (key, scope.clone()));
    match &n.kind {
        ExprK::Int(_) | ExprK::Bool(_) | ExprK::Var(_) => {}
        ExprK::Fun { param, body } => {
            env.push(param.clone());
            scope.push(format!("fun:{}", param));
            collect_keys_rec(body, env, scope, map);
            scope.pop();
            env.pop();
        }
        ExprK::App(f, a) => {
            collect_keys_rec(f, env, scope, map);
            collect_keys_rec(a, env, scope, map);
        }
        ExprK::Let { name, rec_, value, body } => {
            if *rec_ {
                env.push(name.clone());
                scope.push(format!("letrec:{}", name));
                collect_keys_rec(value, env, scope, map);
                collect_keys_rec(body, env, scope, map);
                scope.pop();
                env.pop();
            } else {
                collect_keys_rec(value, env, scope, map);
                env.push(name.clone());
                scope.push(format!("let:{}", name));
                collect_keys_rec(body, env, scope, map);
                scope.pop();
                env.pop();
            }
        }
        ExprK::If(c, t, e) => {
            collect_keys_rec(c, env, scope, map);
            collect_keys_rec(t, env, scope, map);
            collect_keys_rec(e, env, scope, map);
        }
        ExprK::Record(fields) => {
            for (_, v) in fields { collect_keys_rec(v, env, scope, map); }
        }
        ExprK::Proj(e, _) => collect_keys_rec(e, env, scope, map),
        ExprK::Variant(_, p) => collect_keys_rec(p, env, scope, map),
        ExprK::Match(scrut, arms) => {
            collect_keys_rec(scrut, env, scope, map);
            for (label, binder, body) in arms {
                env.push(binder.clone());
                scope.push(format!("arm:{}:{}", label, binder));
                collect_keys_rec(body, env, scope, map);
                scope.pop();
                env.pop();
            }
        }
    }
}
