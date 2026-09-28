// 手写递归下降解析器，生成扁平化 AST：所有节点保存在 Vec 中，引用使用 NodeId。
// 节点 ID 在单次解析内稳定；跨编辑的身份复用由 diff 模块负责。
use crate::lexer::{lex, Span, Tok, Token};
use serde::Serialize;

pub type NodeId = u32;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub span: Span,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "t", content = "c")]
pub enum NodeKind {
    Int(i64),
    Str(String),
    Bool(bool),
    Var(String),
    Upper(String),
    Lam { param: NodeId, body: NodeId },
    Param { name: String },
    App { fun: NodeId, arg: NodeId },
    Let { rec: bool, name: NodeId, bind: NodeId, body: NodeId },
    Bind { name: String },
    Record { fields: Vec<(String, NodeId)>, closed: bool },
    Field { object: NodeId, label: String },
    Variant { label: String, payload: Option<NodeId>, open: bool },
    Match { scrutinee: NodeId, arms: Vec<Arm> },
    If { cond: NodeId, then: NodeId, els: NodeId },
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Arm {
    pub pat: PatKind,
    pub pat_span: Span,
    pub expr: NodeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "t", content = "c")]
pub enum PatKind {
    Wild,
    Var(String),
    Con { label: String, payload: Option<String> },
}

#[derive(Debug, Clone)]
pub struct AST {
    pub src: String,
    pub nodes: Vec<Node>,
    pub root: NodeId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

struct Parser<'a> {
    src: &'a str,
    toks: Vec<Token>,
    pos: usize,
    nodes: Vec<Node>,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> &Token {
        &self.toks[self.pos]
    }
    fn at(&self, t: Tok) -> bool {
        self.peek().tok == t
    }
    fn bump(&mut self) -> Token {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }
    fn eat(&mut self, t: Tok) -> bool {
        if self.at(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok, what: &str) -> Result<Token, ParseError> {
        if self.at(t) {
            Ok(self.bump())
        } else {
            let p = self.peek().clone();
            Err(ParseError {
                span: p.span.clone(),
                message: format!("期望 {}，但遇到 {}", what, describe(&p)),
            })
        }
    }

    fn mk(&mut self, kind: NodeKind, span: Span, text: String) -> NodeId {
        let id = self.nodes.len() as NodeId;
        self.nodes.push(Node { id, kind, span, text });
        id
    }

    fn parse_expr(&mut self) -> Result<NodeId, ParseError> {
        if self.at(Tok::Fn) {
            return self.parse_lam();
        }
        if self.at(Tok::If) {
            return self.parse_if();
        }
        if self.at(Tok::Match) {
            return self.parse_match();
        }
        if self.at(Tok::Let) {
            return self.parse_let();
        }
        self.parse_postfix()
    }

    fn parse_lam(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span;
        let tok = self.expect(Tok::Ident, "函数参数名")?;
        let param = self.mk(NodeKind::Param { name: tok.text.clone() }, tok.span.clone(), tok.text.clone());
        self.expect(Tok::Arrow, "'->'")?;
        let body = self.parse_expr()?;
        let span = start.merge(&self.nodes[body as usize].span);
        Ok(self.mk(NodeKind::Lam { param, body }, span, format!("fn {}", tok.text)))
    }

    fn parse_if(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span;
        let cond = self.parse_expr()?;
        self.expect(Tok::Then, "'then'")?;
        let then = self.parse_expr()?;
        self.expect(Tok::Else, "'else'")?;
        let els = self.parse_expr()?;
        let span = start.merge(&self.nodes[els as usize].span);
        Ok(self.mk(NodeKind::If { cond, then, els }, span, "if".to_string()))
    }

    fn parse_let(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span;
        let rec = self.eat(Tok::Rec);
        let name_tok = self.expect(Tok::Ident, "let 绑定名")?;
        let name = self.mk(
            NodeKind::Bind { name: name_tok.text.clone() },
            name_tok.span.clone(),
            name_tok.text.clone(),
        );
        self.expect(Tok::Eq, "'='")?;
        let bind = self.parse_expr()?;
        self.expect(Tok::In, "'in'")?;
        let body = self.parse_expr()?;
        let span = start.merge(&self.nodes[body as usize].span);
        Ok(self.mk(NodeKind::Let { rec, name, bind, body }, span, format!("let {}{}", if rec { "rec " } else { "" }, name_tok.text)))
    }

    fn parse_match(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span;
        let scrutinee = self.parse_postfix()?;
        self.expect(Tok::With, "'with'")?;
        let mut arms = Vec::new();
        loop {
            self.eat(Tok::Pipe);
            if self.at_end_of_match() {
                break;
            }
            let (pat, pat_span) = self.parse_pat()?;
            self.expect(Tok::FatArrow, "'=>'")?;
            let expr = self.parse_expr()?;
            arms.push(Arm { pat, pat_span, expr });
            if !self.eat(Tok::Pipe) {
                break;
            }
        }
        if arms.is_empty() {
            let p = self.peek().clone();
            return Err(ParseError { span: p.span, message: "match 至少需要一个分支".to_string() });
        }
        let span = start.merge(&self.nodes[arms.last().unwrap().expr as usize].span);
        Ok(self.mk(NodeKind::Match { scrutinee, arms }, span, "match".to_string()))
    }

    fn at_end_of_match(&self) -> bool {
        matches!(self.peek().tok, Tok::Eof)
    }

    fn parse_pat(&mut self) -> Result<(PatKind, Span), ParseError> {
        if self.at(Tok::Underscore) {
            let u = self.bump();
            return Ok((PatKind::Wild, u.span.clone()));
        }
        let t = self.bump();
        match t.tok {
            Tok::Ident => Ok((PatKind::Var(t.text.clone()), t.span.clone())),
            Tok::UpperIdent => {
                let mut payload = None;
                let span = if self.at(Tok::Ident) {
                    let v = self.bump();
                    let s = t.span.merge(&v.span);
                    payload = Some(v.text);
                    s
                } else {
                    t.span.clone()
                };
                Ok((PatKind::Con { label: t.text, payload }, span))
            }
            _ => Err(ParseError {
                span: t.span.clone(),
                message: format!("分支模式应为变量、大写构造器或 '_'，但遇到 {}", describe(&t)),
            }),
        }
    }

    // 原子表达式后处理：函数应用与记录字段访问。
    fn parse_postfix(&mut self) -> Result<NodeId, ParseError> {
        let mut node = self.parse_atom()?;
        loop {
            if self.at(Tok::LParen) {
                let arg = self.parse_paren()?;
                let span = self.nodes[node as usize].span.merge(&self.nodes[arg as usize].span);
                let text = format!("app@{}..{}", span.0, span.1);
                node = self.mk(NodeKind::App { fun: node, arg }, span, text);
            } else if self.at(Tok::Dot) {
                self.bump();
                let label_tok = self.expect(Tok::Ident, "字段名")?;
                let span = self.nodes[node as usize].span.merge(&label_tok.span);
                node = self.mk(NodeKind::Field { object: node, label: label_tok.text }, span, format!(".{}", label_tok.text));
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_paren(&mut self) -> Result<NodeId, ParseError> {
        let lp = self.expect(Tok::LParen, "'('")?;
        if self.at(Tok::RParen) {
            let rp = self.bump();
            return Ok(self.mk(NodeKind::Unit, lp.span.merge(&rp.span), "()".to_string()));
        }
        let e = self.parse_expr()?;
        self.expect(Tok::RParen, "')'")?;
        Ok(e)
    }

    fn parse_atom(&mut self) -> Result<NodeId, ParseError> {
        let t = self.peek().clone();
        match t.tok {
            Tok::IntLit => {
                self.bump();
                Ok(self.mk(NodeKind::Int(t.int.unwrap()), t.span.clone(), t.text.clone()))
            }
            Tok::StrLit => {
                self.bump();
                Ok(self.mk(NodeKind::Str(t.text.clone()), t.span.clone(), format!("{:?}", t.text)))
            }
            Tok::True => {
                self.bump();
                Ok(self.mk(NodeKind::Bool(true), t.span.clone(), "true".to_string()))
            }
            Tok::False => {
                self.bump();
                Ok(self.mk(NodeKind::Bool(false), t.span.clone(), "false".to_string()))
            }
            Tok::LParen => self.parse_paren(),
            Tok::LBrace => self.parse_record(),
            Tok::LBracket => self.parse_variant(),
            Tok::UpperIdent => {
                self.bump();
                let payload = if self.at(Tok::LParen) {
                    Some(self.parse_paren()?)
                } else {
                    None
                };
                let end = payload.map(|p| self.nodes[p as usize].span.clone()).unwrap_or_else(|| t.span.clone());
                let span = t.span.merge(&end);
                Ok(self.mk(NodeKind::Variant { label: t.text.clone(), payload, open: false }, span, format!("[{}]", t.text)))
            }
            Tok::Backtick => self.parse_open_variant(),
            Tok::Ident => {
                self.bump();
                Ok(self.mk(NodeKind::Var(t.text.clone()), t.span.clone(), t.text.clone()))
            }
            _ => Err(ParseError {
                span: t.span.clone(),
                message: format!("期望表达式，但遇到 {}", describe(&t)),
            }),
        }
    }

    fn parse_record(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span;
        let mut fields = Vec::new();
        let mut closed = true;
        if !self.at(Tok::RBrace) {
            loop {
                if self.eat(Tok::Pipe) {
                    closed = false;
                    break;
                }
                let label = self.expect(Tok::Ident, "记录字段名")?;
                self.expect(Tok::Eq, "'='")?;
                let value = self.parse_expr()?;
                fields.push((label.text, value));
                if !self.eat(Tok::Comma) {
                    break;
                }
            }
        } else {
            let rb = self.bump();
            let span = start.merge(&rb.span);
            return Ok(self.mk(NodeKind::Record { fields, closed }, span, "{}".to_string()));
        }
        let end = self.expect(Tok::RBrace, "'}'")?;
        let mut dup = std::collections::HashSet::new();
        for (l, _) in &fields {
            if !dup.insert(l.clone()) {
                return Err(ParseError { span: start.merge(&end.span), message: format!("记录字段 '{}' 重复", l) });
            }
        }
        let span = start.merge(&end.span);
        Ok(self.mk(NodeKind::Record { fields, closed }, span, "{…}".to_string()))
    }

    // 封闭变体字面量 [Label payload?]
    fn parse_variant(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span;
        let label = self.expect(Tok::UpperIdent, "变体构造器（大写开头）")?;
        let payload = if self.at(Tok::LParen) {
            Some(self.parse_paren()?)
        } else {
            None
        };
        let end = self.expect(Tok::RBracket, "']'")?;
        let span = start.merge(&end.span);
        Ok(self.mk(NodeKind::Variant { label: label.text, payload, open: false }, span, "[…]".to_string()))
    }

    // 开放变体字面量 `Label payload?
    fn parse_open_variant(&mut self) -> Result<NodeId, ParseError> {
        let start = self.bump().span;
        let label = self.expect(Tok::UpperIdent, "变体构造器（大写开头）")?;
        let payload = if self.at(Tok::LParen) {
            Some(self.parse_paren()?)
        } else {
            None
        };
        let end = payload.map(|p| self.nodes[p as usize].span.clone()).unwrap_or_else(label.span.clone());
        let span = start.merge(&end);
        Ok(self.mk(NodeKind::Variant { label: label.text, payload, open: true }, span, format!("`{}", label.text)))
    }
}

fn describe(t: &Token) -> String {
    match t.tok {
        Tok::Eof => "输入结束".to_string(),
        Tok::Ident => format!("标识符 '{}'", t.text),
        Tok::UpperIdent => format!("构造器 '{}'", t.text),
        Tok::IntLit => format!("整数 {}", t.text),
        Tok::StrLit => "字符串".to_string(),
        _ => format!("'{}'", t.text),
    }
}

pub fn parse(src: &str) -> Result<AST, ParseError> {
    let toks = lex(src).map_err(|e| ParseError { span: e.span, message: e.message })?;
    let mut p = Parser { src, toks, pos: 0, nodes: Vec::new() };
    let root = p.parse_expr()?;
    if !p.at(Tok::Eof) {
        let t = p.peek().clone();
        return Err(ParseError { span: t.span.clone(), message: format!("表达式结束后仍有多余内容 {}", describe(&t)) });
    }
    // Unit 的占位 span 修复：无法做，保持空范围。
    Ok(AST { src: src.to_string(), nodes: p.nodes, root })
}
