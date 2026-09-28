use crate::ast::*;
use crate::lexer::{lex, Tok, Token};
use crate::span::Span;

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}

pub struct Parser {
    toks: Vec<Token>,
    pos: usize,
    nodes: Vec<Node>,
}

fn tok_name(t: &Tok) -> String {
    match t {
        Tok::Int(n) => format!("integer {}", n),
        Tok::Name(n) => format!("identifier '{}'", n),
        Tok::Tag(n) => format!("tag '`{}'", n),
        Tok::Arrow => "'->'".into(),
        Tok::FatArrow => "'=>'".into(),
        Tok::LBrace => "'{'".into(),
        Tok::RBrace => "'}'".into(),
        Tok::LBracket => "'['".into(),
        Tok::RBracket => "']'".into(),
        Tok::LParen => "'('".into(),
        Tok::RParen => "')'".into(),
        Tok::Eq => "'='".into(),
        Tok::Colon => "':'".into(),
        Tok::Comma => "','".into(),
        Tok::Semi => "';'".into(),
        Tok::Dot => "'.'".into(),
        Tok::Pipe => "'|'".into(),
        Tok::Underscore => "'_'".into(),
        Tok::Let => "'let'".into(),
        Tok::Rec => "'rec'".into(),
        Tok::In => "'in'".into(),
        Tok::Fun => "'fun'".into(),
        Tok::Match => "'match'".into(),
        Tok::With => "'with'".into(),
        Tok::True => "'true'".into(),
        Tok::False => "'false'".into(),
        Tok::Eof => "end of input".into(),
    }
}

impl Parser {
    fn new(toks: Vec<Token>) -> Self {
        Parser { toks, pos: 0, nodes: Vec::new() }
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos].tok
    }
    fn span(&self) -> Span {
        self.toks[self.pos].span
    }
    fn at(&self, t: &Tok) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(t)
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.pos += 1;
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
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(&t) {
            Ok(self.bump())
        } else {
            Err(ParseError { span: self.span(), msg: format!("expected {}, found {}", what, tok_name(self.peek())) })
        }
    }
    fn name(&mut self) -> Result<String, ParseError> {
        match self.peek().clone() {
            Tok::Name(n) => {
                self.bump();
                Ok(n)
            }
            _ => Err(ParseError { span: self.span(), msg: format!("expected identifier, found {}", tok_name(self.peek())) }),
        }
    }

    fn alloc(&mut self, path: NodePath, expr: Expr, span: Span) -> NodeId {
        let id = NodeId(self.nodes.len() as u32);
        self.nodes.push(Node { id, path, expr, span });
        id
    }

    fn parse_program(&mut self) -> Result<NodeId, ParseError> {
        // Program := top-let* expr
        //   top-let := 'let' ['rec'] name [':' type] '=' expr
        let mut idx = 0u32;
        let mut result = self.parse_expr(NodePath(vec![PathStep::Root]))?;
        while self.at(&Tok::Let) {
            let let_tok = self.bump();
            let rec = self.eat(&Tok::Rec);
            let name = self.name()?;
            let annot = if self.eat(&Tok::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(Tok::Eq, "'=' after binding")?;
            let def = self.parse_expr(NodePath(vec![PathStep::Decl(idx), PathStep::LetDef]))?;
            let def = match annot {
                Some(ty) => {
                    let span = let_tok.span.join(&self.nodes[def.0 as usize].span);
                    self.alloc(NodePath(vec![PathStep::Decl(idx), PathStep::Annot]), Expr::Annot { expr: def, ty }, span)
                }
                None => def,
            };
            // result currently is the continuation; reparent it as the body of this let.
            self.repath(result, NodePath(vec![PathStep::Decl(idx), PathStep::LetBody]));
            let span = let_tok.span.join(&self.nodes[result.0 as usize].span);
            result = self.alloc(NodePath(vec![PathStep::Root]), Expr::Let { rec, name, def, body: result }, span);
            idx += 1;
        }
        Ok(result)
    }

    fn repath(&mut self, id: NodeId, path: NodePath) {
        // Collect subtree with suffix relative to its current root depth.
        let depth = self.nodes[id.0 as usize].path.0.len();
        let mut collected: Vec<(NodeId, Vec<PathStep>)> = Vec::new();
        fn collect(nodes: &[Node], nid: NodeId, depth: usize, out: &mut Vec<(NodeId, Vec<PathStep>)>) {
            let suffix = nodes[nid.0 as usize].path.0[depth..].to_vec();
            out.push((nid, suffix));
            let children: Vec<NodeId> = match &nodes[nid.0 as usize].expr {
                Expr::Fun { body, .. } => vec![*body],
                Expr::App { fun, args } => {
                    let mut v = vec![*fun];
                    v.extend(args);
                    v
                }
                Expr::Let { def, body, .. } => vec![*def, *body],
                Expr::Record { fields } => fields.iter().map(|(_, n)| *n).collect(),
                Expr::Field { recv, .. } => vec![*recv],
                Expr::Variant { payload, .. } => payload.iter().copied().collect(),
                Expr::Match { scrut, arms } => {
                    let mut v = vec![*scrut];
                    v.extend(arms.iter().map(|(_, n)| *n));
                    v
                }
                Expr::Annot { expr, .. } => vec![*expr],
                _ => vec![],
            };
            for c in children {
                collect(nodes, c, depth, out);
            }
        }
        collect(&self.nodes, id, depth, &mut collected);
        for (nid, suffix) in collected {
            let mut np = path.0.clone();
            np.extend(suffix);
            self.nodes[nid.0 as usize].path = NodePath(np);
        }
    }

    fn parse_expr(&mut self, path: NodePath) -> Result<NodeId, ParseError> {
        if self.at(&Tok::Let) {
            return self.parse_let(path);
        }
        if self.at(&Tok::Fun) {
            return self.parse_fun(path);
        }
        if self.at(&Tok::Match) {
            return self.parse_match(path);
        }
        self.parse_app(path)
    }

    fn parse_let(&mut self, path: NodePath) -> Result<NodeId, ParseError> {
        let let_tok = self.bump();
        let rec = self.eat(&Tok::Rec);
        let name = self.name()?;
        let annot = if self.eat(&Tok::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(Tok::Eq, "'=' after let binding")?;
        let def = self.parse_expr(path.push(PathStep::LetDef))?;
        let def = match annot {
            Some(ty) => {
                let span = let_tok.span.join(&self.nodes[def.0 as usize].span);
                self.alloc(path.push(PathStep::Annot), Expr::Annot { expr: def, ty }, span)
            }
            None => def,
        };
        self.expect(Tok::In, "'in' after let definition")?;
        let body = self.parse_expr(path.push(PathStep::LetBody))?;
        let span = let_tok.span.join(&self.nodes[body.0 as usize].span);
        Ok(self.alloc(path, Expr::Let { rec, name, def, body }, span))
    }

    fn parse_fun(&mut self, path: NodePath) -> Result<NodeId, ParseError> {
        let fun_tok = self.bump();
        let param = self.name()?;
        self.expect(Tok::FatArrow, "'=>' after function parameter")?;
        let body = self.parse_expr(path.push(PathStep::FunBody))?;
        let span = fun_tok.span.join(&self.nodes[body.0 as usize].span);
        Ok(self.alloc(path, Expr::Fun { param, body }, span))
    }

    fn parse_match(&mut self, path: NodePath) -> Result<NodeId, ParseError> {
        let m_tok = self.bump();
        let scrut = self.parse_app(path.push(PathStep::MatchScrutinee))?;
        self.expect(Tok::With, "'with' after match scrutinee")?;
        let mut arms = Vec::new();
        let mut k = 0u32;
        loop {
            self.expect(Tok::Pipe, "'|' for match arm")?;
            let pat = self.parse_pat()?;
            self.expect(Tok::FatArrow, "'=>' in match arm")?;
            let body = self.parse_app(path.push(PathStep::MatchArmBody(k)))?;
            arms.push((pat, body));
            k += 1;
            if !self.at(&Tok::Pipe) {
                break;
            }
        }
        let span = m_tok.span.join(&self.toks[self.pos.saturating_sub(1)].span);
        Ok(self.alloc(path, Expr::Match { scrut, arms }, span))
    }

    fn parse_pat(&mut self) -> Result<Pat, ParseError> {
        if self.eat(&Tok::Underscore) {
            return Ok(Pat::Wild);
        }
        if let Tok::Tag(t) = self.peek().clone() {
            self.bump();
            if matches!(self.peek(), Tok::LParen) {
                self.bump();
                let inner = self.parse_pat()?;
                self.expect(Tok::RParen, "')'")?;
                return Ok(Pat::Tag { tag: t, payload: Some(Box::new(inner)) });
            }
            return Ok(Pat::Tag { tag: t, payload: None });
        }
        let n = self.name()?;
        Ok(Pat::Var(n))
    }

    fn parse_app(&mut self, path: NodePath) -> Result<NodeId, ParseError> {
        let mut head = self.parse_atom(path.push(PathStep::AppFn))?;
        let mut args: Vec<NodeId> = Vec::new();
        loop {
            if self.starts_atom() {
                let k = args.len() as u32;
                let arg = self.parse_atom(path.push(PathStep::AppArg(k)))?;
                args.push(arg);
            } else if self.at(&Tok::Dot) {
                self.bump();
                let field = self.name()?;
                let span = self.nodes[head.0 as usize].span;
                head = self.alloc(path.push(PathStep::FieldAccess), Expr::Field { recv: head, field }, span.join(self.toks[self.pos.saturating_sub(1)].span));
            } else if self.at(&Tok::Colon) {
                self.bump();
                let ty = self.parse_type()?;
                let span = self.nodes[head.0 as usize].span;
                head = self.alloc(path.push(PathStep::Annot), Expr::Annot { expr: head, ty }, span);
            } else {
                break;
            }
        }
        if args.is_empty() {
            // re-root the single head path: head was allocated at path/app.fn, move to path.
            self.repath(head, path);
            return Ok(head);
        }
        let span = self.nodes[head.0 as usize].span.join(&self.nodes[*args.last().unwrap()].span);
        Ok(self.alloc(path, Expr::App { fun: head, args }, span))
    }

    fn starts_atom(&self) -> bool {
        matches!(
            self.peek(),
            Tok::Int(_)
                | Tok::Name(_)
                | Tok::Tag(_)
                | Tok::True
                | Tok::False
                | Tok::LParen
                | Tok::LBrace
                | Tok::Fun
                | Tok::Match
        )
    }

    fn parse_atom(&mut self, path: NodePath) -> Result<NodeId, ParseError> {
        let span0 = self.span();
        match self.peek().clone() {
            Tok::Int(n) => {
                self.bump();
                Ok(self.alloc(path, Expr::Int(n), span0.join(self.toks[self.pos.saturating_sub(1)].span)))
            }
            Tok::True => {
                self.bump();
                Ok(self.alloc(path, Expr::Bool(true), span0))
            }
            Tok::False => {
                self.bump();
                Ok(self.alloc(path, Expr::Bool(false), span0))
            }
            Tok::Name(n) => {
                self.bump();
                Ok(self.alloc(path, Expr::Var(n), span0))
            }
            Tok::Tag(t) => {
                self.bump();
                let (payload, end) = if self.at(&Tok::LParen) {
                    self.bump();
                    let p = self.parse_expr(path.push(PathStep::VariantPayload))?;
                    self.expect(Tok::RParen, "')'")?;
                    (Some(p), self.toks[self.pos.saturating_sub(1)].span)
                } else {
                    (None, span0)
                };
                Ok(self.alloc(path, Expr::Variant { tag: t, payload }, span0.join(end)))
            }
            Tok::LParen => {
                self.bump();
                let e = self.parse_expr(path.push(PathStep::Paren))?;
                self.expect(Tok::RParen, "')'")?;
                self.repath(e, path);
                Ok(e)
            }
            Tok::LBrace => self.parse_record(path),
            Tok::Fun => self.parse_fun(path),
            Tok::Match => self.parse_match(path),
            _ => Err(ParseError { span: span0, msg: format!("expected an expression, found {}", tok_name(self.peek())) }),
        }
    }

    fn parse_record(&mut self, path: NodePath) -> Result<NodeId, ParseError> {
        let brace = self.bump();
        let mut fields = Vec::new();
        let mut k = 0u32;
        if !self.at(&Tok::RBrace) {
            loop {
                let name = self.name()?;
                self.expect(Tok::Eq, "'=' for record field")?;
                let val = self.parse_expr(path.push(PathStep::RecordField(k)))?;
                fields.push((name, val));
                k += 1;
                if self.eat(&Tok::Semi) || self.eat(&Tok::Comma) {
                    if self.at(&Tok::RBrace) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let close = self.expect(Tok::RBrace, "'}' to close record")?;
        Ok(self.alloc(path, Expr::Record { fields }, brace.span.join(close.span)))
    }

    fn parse_type(&mut self) -> Result<TypeAst, ParseError> {
        let lhs = self.parse_type_atom()?;
        if self.at(&Tok::Arrow) {
            self.bump();
            let rhs = self.parse_type()?;
            return Ok(TypeAst::TFun(Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    fn parse_type_atom(&mut self) -> Result<TypeAst, ParseError> {
        match self.peek().clone() {
            Tok::Name(n) => {
                self.bump();
                Ok(TypeAst::TName(n))
            }
            Tok::Underscore => {
                self.bump();
                Ok(TypeAst::TRowVar("_".into()))
            }
            Tok::LBrace => {
                self.bump();
                let mut fields = Vec::new();
                let mut open = false;
                if !self.at(&Tok::RBrace) {
                    loop {
                        if self.at(&Tok::Underscore) {
                            self.bump();
                            open = true;
                            break;
                        }
                        let name = self.name()?;
                        self.expect(Tok::Colon, "':' for field type")?;
                        let ty = self.parse_type()?;
                        fields.push((name, ty));
                        if self.eat(&Tok::Comma) || self.eat(&Tok::Semi) {
                            if self.at(&Tok::RBrace) {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect(Tok::RBrace, "'}' to close record type")?;
                Ok(TypeAst::TRecord(fields, !open))
            }
            Tok::LBracket => {
                self.bump();
                let mut cases = Vec::new();
                let mut open = false;
                if !self.at(&Tok::RBracket) {
                    loop {
                        if self.at(&Tok::Underscore) {
                            self.bump();
                            open = true;
                            break;
                        }
                        let tag = match self.peek().clone() {
                            Tok::Tag(t) => {
                                self.bump();
                                t
                            }
                            _ => return Err(ParseError { span: self.span(), msg: "expected variant tag".into() }),
                        };
                        let payload = if self.eat(&Tok::Colon) {
                            Some(self.parse_type()?)
                        } else {
                            None
                        };
                        cases.push((tag, payload));
                        if self.eat(&Tok::Pipe) || self.eat(&Tok::Comma) {
                            if self.at(&Tok::RBracket) {
                                break;
                            }
                            continue;
                        }
                        break;
                    }
                }
                self.expect(Tok::RBracket, "']' to close variant type")?;
                Ok(TypeAst::TVariant(cases, !open))
            }
            Tok::LParen => {
                self.bump();
                let t = self.parse_type()?;
                self.expect(Tok::RParen, "')'")?;
                Ok(t)
            }
            _ => Err(ParseError { span: self.span(), msg: "expected a type".into() }),
        }
    }
}

pub fn parse(src: &str) -> Result<Program, ParseError> {
    let toks = lex(src).map_err(|e| ParseError { span: e.span, msg: e.msg })?;
    let mut p = Parser::new(toks);
    let root = p.parse_program()?;
    if !p.at(&Tok::Eof) {
        return Err(ParseError { span: p.span(), msg: format!("unexpected {}", tok_name(p.peek())) });
    }
    Ok(Program { nodes: p.nodes, root })
}
