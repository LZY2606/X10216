//! Parser: grammar over raw arena, then stable numbering and name resolution.

use super::ast::*;
use super::lex::{lex, Tok, Token};

#[derive(Clone)]
struct Raw {
    node: Node,
    span: Span,
}

#[derive(Clone, Debug)]
pub struct ParseError { pub msg: String, pub span: Span }

type R = usize;

struct P {
    toks: Vec<Token>,
    pos: usize,
    arena: Vec<Raw>,
    errors: Vec<ParseError>,
}

impl P {
    fn alloc(&mut self, node: Node, span: Span) -> R {
        self.arena.push(Raw { node, span });
        self.arena.len() - 1
    }
    fn cur(&self) -> &Tok { &self.toks[self.pos].tok }
    fn span(&self) -> Span { self.toks[self.pos].span }
    fn bump(&mut self) -> Token { let t = self.toks[self.pos].clone(); self.pos += 1; t }
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(self.cur()) == std::mem::discriminant(t) { self.bump(); true } else { false }
    }
    fn expect(&mut self, t: Tok, what: &str) -> bool {
        if self.eat(&t) { true } else {
            self.errors.push(ParseError { msg: format!("期望 {}", what), span: self.span() });
            false
        }
    }
    fn err(&mut self, msg: impl Into<String>, span: Span) {
        self.errors.push(ParseError { msg: msg.into(), span });
    }

    fn expr(&mut self) -> R { self.seq() }

    fn seq(&mut self) -> R {
        let lo = self.span().lo();
        let mut items = vec![self.expr1()];
        while self.eat(&Tok::Semi) {
            if matches!(self.cur(), Tok::Eof | Tok::Rb) { break; }
            items.push(self.expr1());
        }
        if items.len() == 1 { items[0] }
        else {
            let hi = self.arena[items[items.len() - 1]].span.hi();
            self.alloc(Node::Seq { items }, Span(lo, hi))
        }
    }

    fn expr1(&mut self) -> R {
        if matches!(self.cur(), Tok::Let) { return self.let_expr(false); }
        self.if_expr()
    }

    fn let_expr(&mut self, rec_seen: bool) -> R {
        let lo = self.bump().span.lo();
        let rec = rec_seen || self.eat(&Tok::Rec);
        let name = self.binder();
        let value = if self.eat(&Tok::Eq) { self.expr() } else {
            self.err("let 需要 '='", self.span());
            self.alloc(Node::Var { name: name.0, resolved: None }, name.1)
        };
        let body = if self.eat(&Tok::In) { Some(self.expr()) } else { None };
        let hi = body.map(|b| self.arena[b].span.hi()).unwrap_or_else(|| self.arena[value].span.hi());
        self.alloc(Node::Let {
            rec,
            bind: Bind { id: BindId(0), span: name.1, name: name.0, recursive: rec },
            value, body,
        }, Span(lo, hi))
    }

    fn if_expr(&mut self) -> R {
        if !matches!(self.cur(), Tok::If) { return self.lam(); }
        let lo = self.bump().span.lo();
        let cond = self.expr();
        if !self.eat(&Tok::Then) { self.err("期望 then", self.span()); }
        let then_b = self.expr();
        if !self.eat(&Tok::Else) { self.err("期望 else", self.span()); }
        let else_b = self.expr();
        let hi = self.arena[else_b].span.hi();
        self.alloc(Node::If { cond, then_b, else_b }, Span(lo, hi))
    }

    fn lam(&mut self) -> R {
        if !matches!(self.cur(), Tok::Fn) { return self.binop(0); }
        let lo = self.bump().span.lo();
        let name = self.binder();
        if !self.eat(&Tok::Arrow) { self.err("期望 ->", self.span()); }
        let body = self.expr();
        let hi = self.arena[body].span.hi();
        self.alloc(Node::Lam {
            bind: Bind { id: BindId(0), span: name.1, name: name.0, recursive: false },
            body,
        }, Span(lo, hi))
    }

    fn prec(&self) -> Option<(BinOp, u8)> {
        Some(match self.cur() {
            Tok::Plus => (BinOp::Add, 4),
            Tok::Minus => (BinOp::Sub, 4),
            Tok::Eq2 => (BinOp::Eq2, 3),
            Tok::Lt => (BinOp::Lt, 3),
            _ => return None,
        })
    }

    fn binop(&mut self, limit: u8) -> R {
        let mut lhs = self.postfix();
        while let Some((op, p)) = self.prec() {
            if p < limit { break; }
            self.bump();
            let rhs = self.binop(p + 1);
            let lo = self.arena[lhs].span.lo();
            let hi = self.arena[rhs].span.hi();
            lhs = self.alloc(Node::Bin { op, lhs, rhs }, Span(lo, hi));
        }
        lhs
    }

    fn postfix(&mut self) -> R {
        let mut e = self.atom();
        loop {
            if self.eat(&Tok::Dot) {
                match self.bump_field() {
                    Ok(field) => {
                        let hi = self.span_of_last_field(field.1);
                        e = self.alloc(Node::Proj { target: e, field: field.0 }, Span(self.arena[e].span.lo(), hi));
                    }
                    Err(sp) => self.err("'.' 后需要字段名", sp),
                }
            } else { break; }
        }
        // juxtaposition application, greedy
        let lo = self.arena[e].span.lo();
        let mut args = Vec::new();
        while self.starts_atom() {
            args.push(self.postfix());
        }
        for a in args {
            let hi = self.arena[a].span.hi();
            e = self.alloc(Node::App { fun: e, arg: a }, Span(lo, hi));
        }
        e
    }

    fn span_of_last_field(&self, name_span: Span) -> u32 { name_span.hi() }

    fn bump_field(&mut self) -> Result<(String, Span), Span> {
        if let Tok::Ident(s) = self.cur().clone() {
            let t = self.bump();
            return Ok((s, t.span));
        }
        Err(self.span())
    }

    fn starts_atom(&self) -> bool {
        matches!(self.cur(),
            Tok::Int(_) | Tok::Str(_) | Tok::True | Tok::False | Tok::Ident(_)
            | Tok::Lp | Tok::Lb | Tok::Hash | Tok::Fn | Tok::If | Tok::Match | Tok::Under)
    }

    fn binder(&mut self) -> (String, Span) {
        match self.cur().clone() {
            Tok::Ident(s) => { let t = self.bump(); (s, t.span) }
            Tok::Under => { let t = self.bump(); ("_".to_string(), t.span) }
            _ => { let sp = self.span(); self.err("期望标识符", sp); ("<invalid>".to_string(), sp) }
        }
    }

    fn atom(&mut self) -> R {
        let sp = self.span();
        match self.cur().clone() {
            Tok::Int(v) => { self.bump(); self.alloc(Node::Int { value: v }, sp) }
            Tok::True => { self.bump(); self.alloc(Node::Bool { value: true }, sp) }
            Tok::False => { self.bump(); self.alloc(Node::Bool { value: false }, sp) }
            Tok::Str(s) => { self.bump(); self.alloc(Node::Str { value: s }, sp) }
            Tok::Under => {
                self.bump();
                self.alloc(Node::Var { name: "_".into(), resolved: None }, sp)
            }
            Tok::Ident(s) => { self.bump(); self.alloc(Node::Var { name: s, resolved: None }, sp) }
            Tok::Lp => {
                self.bump();
                let e = self.seq();
                if !self.eat(&Tok::Rp) { self.err("期望 ')'", self.span()); }
                e
            }
            Tok::Lb => self.record(),
            Tok::Hash => self.tag(),
            Tok::Fn => self.lam(),
            Tok::If => self.if_expr(),
            Tok::Match => self.match_expr(),
            _ => {
                self.bump();
                self.err("无法解析的表达式起点", sp);
                self.alloc(Node::Var { name: "<invalid>".into(), resolved: None }, sp)
            }
        }
    }

    fn record(&mut self) -> R {
        let lo = self.bump().span.lo();
        let mut fields = Vec::new();
        let mut rest = None;
        let mut hi = lo + 1;
        while !matches!(self.cur(), Tok::Rb | Tok::Eof) {
            if self.eat(&Tok::Dot2) {
                let e = self.expr();
                hi = self.arena[e].span.hi();
                rest = Some(e);
            } else if let Tok::Ident(f) = self.cur().clone() {
                let fsp = self.bump().span;
                let _ = fsp;
                if !self.eat(&Tok::Eq) { self.err("记录字段需要 '='", self.span()); }
                let e = self.expr();
                hi = self.arena[e].span.hi();
                fields.push((f, e));
            } else {
                self.err("记录中需要字段名", self.span());
                break;
            }
            if self.eat(&Tok::Comma) { continue; }
            break;
        }
        if self.eat(&Tok::Rb) { hi = self.toks[self.pos - 1].span.hi(); }
        else { self.err("期望 '}'", self.span()); }
        self.alloc(Node::Record { fields, rest }, Span(lo, hi))
    }

    fn tag(&mut self) -> R {
        let lo = self.bump().span.lo();
        let label = match self.cur().clone() {
            Tok::Ident(s) => { self.bump(); s }
            _ => { self.err("# 后需要标签名", self.span()); String::new() }
        };
        let mut payload = None;
        let mut hi = self.toks[self.pos - 1].span.hi();
        if self.starts_atom() {
            let e = self.atom();
            hi = self.arena[e].span.hi();
            payload = Some(e);
        }
        self.alloc(Node::Tag { label, payload }, Span(lo, hi))
    }

    fn match_expr(&mut self) -> R {
        let lo = self.bump().span.lo();
        let scrut = self.expr();
        if !self.eat(&Tok::With) { self.err("期望 with", self.span()); }
        let mut arms = Vec::new();
        let mut hi = lo;
        loop {
            if self.eat(&Tok::Pipe) {}
            else if matches!(self.cur(), Tok::Eof) { self.err("match 缺少分支", self.span()); break; }
            else if !self.starts_pat() { break; }
            let pat = self.pat();
            if !self.eat(&Tok::Fat) { self.err("分支需要 =>", self.span()); }
            let e = self.expr();
            hi = self.arena[e].span.hi();
            arms.push(Arm { pat, expr: e });
            if self.eat(&Tok::Pipe) { continue; }
            break;
        }
        self.alloc(Node::Match { scrut, arms }, Span(lo, hi))
    }

    fn starts_pat(&self) -> bool {
        matches!(self.cur(), Tok::Ident(_) | Tok::Under | Tok::Lb | Tok::Hash)
    }

    fn pat(&mut self) -> Pat {
        match self.cur().clone() {
            Tok::Under => { self.bump(); Pat::PWild }
            Tok::Ident(_) => {
                let (name, sp) = self.binder();
                Pat::PVar { bind: Bind { id: BindId(0), span: sp, name, recursive: false } }
            }
            Tok::Lb => self.record_pat(),
            Tok::Hash => self.tag_pat(),
            _ => { let sp = self.span(); self.err("无法解析的模式", sp); Pat::PWild }
        }
    }

    fn record_pat(&mut self) -> Pat {
        self.bump();
        let mut fields = Vec::new();
        let mut rest = false;
        while !matches!(self.cur(), Tok::Rb | Tok::Eof) {
            if self.eat(&Tok::Dot2) {
                rest = true;
                if let Tok::Ident(s) = self.cur().clone() { self.bump(); let _ = s; }
            } else if let Tok::Ident(f) = self.cur().clone() {
                self.bump();
                let p = if self.eat(&Tok::Eq) { self.pat() } else {
                    Pat::PVar { bind: Bind { id: BindId(0), span: self.toks[self.pos - 1].span, name: f.clone(), recursive: false } }
                };
                fields.push((f, p));
            } else { self.err("记录模式中需要字段名", self.span()); break; }
            if self.eat(&Tok::Comma) { continue; }
            break;
        }
        if !self.eat(&Tok::Rb) { self.err("期望 '}'", self.span()); }
        Pat::PRecord { fields, rest }
    }

    fn tag_pat(&mut self) -> Pat {
        self.bump();
        let label = match self.cur().clone() {
            Tok::Ident(s) => { self.bump(); s }
            _ => { self.err("# 后需要标签名", self.span()); String::new() }
        };
        let mut payload = None;
        if self.starts_pat() {
            payload = Some(Box::new(self.pat()));
        }
        Pat::PTag { label, payload }
    }
}

// ---------- top level: raw parse -> numbered AST + resolution ----------

pub struct ParseOutput {
    pub ast: Ast,
    pub parse_errors: Vec<ParseError>,
    pub unbound: Vec<(String, Span)>,
}

fn mix(parent: u64, slot: u64, content: u64) -> u64 {
    let mut h = Hasher::new();
    h.tag("path");
    h.bytes(&parent.to_le_bytes());
    h.bytes(&slot.to_le_bytes());
    h.bytes(&content.to_le_bytes());
    h.fin()
}

fn binder_id(parent: u64, slot: u64, rec: bool) -> BindId {
    let mut h = Hasher::new();
    h.bytes(&parent.to_le_bytes());
    h.bytes(&slot.to_le_bytes());
    h.bytes(&content_hash(Content::Binder { rec }).to_le_bytes());
    BindId(h.fin())
}

struct Num<'a> {
    raw: &'a [Raw],
    out: Vec<AstNode>,
    arm_pats: Vec<(NodeId, Pat)>,
}

impl<'a> Num<'a> {
    fn run_node(&mut self, r: R, parent: u64, slot: u64) -> NodeId {
        let raw_node = self.raw[r].node.clone();
        let span = self.raw[r].span;
        // recurse children first (deterministic slot order), collecting slot counter
        let mut idx: u64 = 0;
        let mut next = || { let v = idx; idx += 1; v };
        let node = match raw_node {
            Node::Program { decls } => {
                let decls = decls.into_iter().map(|d| self.run_node(d, 0, next())).collect();
                Node::Program { decls }
            }
            Node::Let { rec, mut bind, value, body } => {
                let nid_pre = mix(parent, slot, content_hash(Content::N(&Node::Let { rec, bind: bind.clone(), value: 0, body })));
                bind.id = binder_id(nid_pre, 0, rec);
                let v = self.run_node(value, nid_pre, 10);
                let b = body.map(|x| self.run_node(x, nid_pre, 20));
                Node::Let { rec, bind, value: v, body: b }
            }
            Node::Lam { mut bind, body } => {
                let nid_pre = mix(parent, slot, content_hash(Content::N(&Node::Lam { bind: bind.clone(), body: 0 })));
                bind.id = binder_id(nid_pre, 0, false);
                let b = self.run_node(body, nid_pre, 10);
                Node::Lam { bind, body: b }
            }
            Node::App { fun, arg } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::App { fun: 0, arg: 0 })));
                let f = self.run_node(fun, nid, 0);
                let a = self.run_node(arg, nid, 1);
                Node::App { fun: f, arg: a }
            }
            n @ Node::Var { .. } => n,
            n @ Node::Int { .. } | n @ Node::Bool { .. } | n @ Node::Str { .. } => n,
            Node::Bin { op, lhs, rhs } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::Bin { op, lhs: 0, rhs: 0 })));
                let l = self.run_node(lhs, nid, 0);
                let rr = self.run_node(rhs, nid, 1);
                Node::Bin { op, lhs: l, rhs: rr }
            }
            Node::If { cond, then_b, else_b } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::If { cond: 0, then_b: 0, else_b: 0 })));
                let c = self.run_node(cond, nid, 0);
                let t = self.run_node(then_b, nid, 1);
                let e = self.run_node(else_b, nid, 2);
                Node::If { cond: c, then_b: t, else_b: e }
            }
            Node::Seq { items } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::Seq { items: vec![0; items.len()] })));
                let items = items.into_iter().enumerate().map(|(i, x)| self.run_node(x, nid, i as u64)).collect();
                Node::Seq { items }
            }
            Node::Record { fields, rest } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::Record {
                    fields: fields.iter().map(|(f, _)| (f.clone(), 0)).collect(),
                    rest: rest.map(|_| 0),
                })));
                let mut ni = 0u64;
                let fields = fields.into_iter().map(|(f, e)| {
                    let e = self.run_node(e, nid, ni); ni += 1; (f, e)
                }).collect();
                let rest = rest.map(|e| self.run_node(e, nid, 10_000));
                Node::Record { fields, rest }
            }
            Node::Proj { target, field } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::Proj { target: 0, field: field.clone() })));
                let t = self.run_node(target, nid, 0);
                Node::Proj { target: t, field }
            }
            Node::Tag { label, payload } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::Tag { label: label.clone(), payload: payload.map(|_| 0) })));
                let payload = payload.map(|e| self.run_node(e, nid, 0));
                Node::Tag { label, payload }
            }
            Node::Match { scrut, arms } => {
                let nid = mix(parent, slot, content_hash(Content::N(&Node::Match { scrut: 0, arms: vec![] })));
                let s = self.run_node(scrut, nid, 0);
                let arms = arms.into_iter().enumerate().map(|(ai, arm)| {
                    let apid = mix(nid, 100 + ai as u64, content_hash(Content::Arm));
                    let pat = self.run_pat(arm.pat, apid, 0);
                    self.arm_pats.push((apid, Pat::PWild));
                    let expr = self.run_node(arm.expr, apid, 1);
                    // replace the placeholder record with the actual pat
                    let last = self.arm_pats.last_mut().unwrap();
                    last.1 = pat;
                    Arm { pat, expr }
                }).collect();
                Node::Match { scrut: s, arms }
            }
        };
        let cid = mix(parent, slot, content_hash(Content::N(&node)));
        self.out.push(AstNode { id: cid, span, node });
        cid
    }

    fn run_pat(&mut self, p: Pat, owner: NodeId, path: u64) -> Pat {
        let mut h = Hasher::new();
        h.bytes(&owner.to_le_bytes());
        h.bytes(&path.to_le_bytes());
        h.bytes(&content_hash(Content::Pat(&p)).to_le_bytes());
        let pid = h.fin();
        let mut sub = |i: u64| mix(pid, i, 0);
        match p {
            Pat::PVar { mut bind } => {
                bind.id = binder_id(pid, 0, false);
                Pat::PVar { bind }
            }
            Pat::PWild => Pat::PWild,
            Pat::PRecord { fields, rest } => {
                let fields = fields.into_iter().enumerate().map(|(i, (f, q))| {
                    let q = self.run_pat(q, owner, path * 100 + i as u64 + 1);
                    let _ = sub(i as u64);
                    (f, q)
                }).collect();
                Pat::PRecord { fields, rest }
            }
            Pat::PTag { label, payload } => {
                let payload = payload.map(|q| Box::new(self.run_pat(*q, owner, path * 100 + 1)));
                Pat::PTag { label, payload }
            }
        }
    }
}

// ---------- name resolution ----------

struct Resolver<'a> {
    ast: &'a mut Ast,
    scopes: Vec<Vec<(String, BindId)>>,
    unbound: Vec<(String, Span)>,
    rec_self: Option<BindId>,
}

impl<'a> Resolver<'a> {
    fn lookup(&self, name: &str) -> Option<BindId> {
        for s in self.scopes.iter().rev() {
            for (n, id) in s.iter().rev() {
                if n == name { return Some(*id); }
            }
        }
        None
    }
    fn bind(&mut self, name: String, id: BindId) {
        self.scopes.last_mut().unwrap().push((name, id));
    }
    fn push(&mut self) { self.scopes.push(Vec::new()); }
    fn pop(&mut self) { self.scopes.pop(); }

    fn resolve_pat_binds(&mut self, p: &Pat) {
        match p {
            Pat::PVar { bind } => self.bind(bind.name.clone(), bind.id),
            Pat::PWild => {}
            Pat::PRecord { fields, .. } => for (_, q) in fields { self.resolve_pat_binds(q); }
            Pat::PTag { payload, .. } => if let Some(q) = payload { self.resolve_pat_binds(q); }
        }
    }

    fn walk(&mut self, id: NodeId) {
        let node = self.ast.node(id).node.clone();
        match node {
            Node::Program { decls } => {
                self.push();
                for d in decls { self.walk(d); }
                self.pop();
            }
            Node::Let { rec, bind, value, body } => {
                if rec {
                    self.bind(bind.name.clone(), bind.id);
                    self.walk(value);
                } else {
                    self.walk(value);
                    self.bind(bind.name.clone(), bind.id);
                }
                if let Some(b) = body { self.walk(b); }
            }
            Node::Lam { bind, body } => {
                self.push();
                if bind.name != "_" { self.bind(bind.name.clone(), bind.id); }
                self.walk(body);
                self.pop();
            }
            Node::App { fun, arg } => { self.walk(fun); self.walk(arg); }
            Node::Var { name, .. } => {
                if name == "_" || name == "<invalid>" { return; }
                match self.lookup(&name) {
                    Some(bid) => self.set_var(id, bid),
                    None => {
                        let sp = self.ast.node(id).span;
                        self.unbound.push((name.clone(), sp));
                    }
                }
            }
            Node::Bin { lhs, rhs, .. } => { self.walk(lhs); self.walk(rhs); }
            Node::If { cond, then_b, else_b } => { self.walk(cond); self.walk(then_b); self.walk(else_b); }
            Node::Seq { items } => for x in items { self.walk(x); }
            Node::Record { fields, rest } => {
                for (_, e) in fields { self.walk(e); }
                if let Some(e) = rest { self.walk(e); }
            }
            Node::Proj { target, .. } => self.walk(target),
            Node::Tag { payload, .. } => if let Some(e) = payload { self.walk(e); },
            Node::Match { scrut, arms } => {
                self.walk(scrut);
                // branches share the outer scope; each arm gets a fresh inner scope
                for arm in &arms {
                    self.push();
                    self.resolve_pat_binds(&arm.pat);
                    self.walk(arm.expr);
                    self.pop();
                }
            }
            Node::Int { .. } | Node::Bool { .. } | Node::Str { .. } => {}
        }
    }

    fn set_var(&mut self, id: NodeId, bid: BindId) {
        for n in self.ast.nodes.iter_mut() {
            if n.id == id {
                if let Node::Var { resolved, .. } = &mut n.node {
                    *resolved = Some(bid);
                }
            }
        }
    }
}

pub fn parse(src: &str) -> Result<ParseOutput, ParseError> {
    let toks = lex(src).map_err(|e| ParseError { msg: e.msg, span: e.span })?;
    let mut p = P { toks, pos: 0, arena: Vec::new(), errors: Vec::new() };
    // top-level: zero or more `let rec? id = expr` (body-less), in order
    let mut decls: Vec<R> = Vec::new();
    while !matches!(p.cur(), Tok::Eof) {
        match p.cur() {
            Tok::Let => decls.push(p.let_expr(false)),
            _ => {
                // allow a bare expression at top level
                decls.push(p.seq());
            }
        }
        if !p.eat(&Tok::Semi) {
            if !matches!(p.cur(), Tok::Eof) { p.err("顶层声明之间需要 ';'", p.span()); }
        }
    }
    let span_all = Span(0, src.len() as u32);
    let root = p.alloc(Node::Program { decls }, span_all);
    let raw = std::mem::take(&mut p.arena);
    let errors = std::mem::take(&mut p.errors);

    let mut num = Num { raw: &raw, out: Vec::new(), arm_pats: Vec::new() };
    let root_id = num.run_node(root, 0, 0);
    // node ids are postorder; sort nodes by id for stable presentation
    let mut nodes = num.out;
    nodes.sort_by_key(|n| n.id);
    let mut ast = Ast { root: root_id, nodes, source: src.to_string() };

    let mut resolver = Resolver { ast: &mut ast, scopes: Vec::new(), unbound: Vec::new(), rec_self: None };
    resolver.walk(root_id);

    Ok(ParseOutput { ast, parse_errors: errors, unbound: resolver.unbound })
}
