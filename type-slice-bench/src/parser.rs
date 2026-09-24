//! 手写词法分析 + Pratt 语法分析。
//!
//! 语言（小 HM 风格）：
//!   prog     ::= (let-bind | rec-bind)* expr?
//!   expr     ::= lit | var | `tag payload? | {f=e,..} | e.f | `tag
//!              | fun pat+ -> e | let (rec)? bind {and bind} in e
//!              | match e with p -> e {| p -> e}
//!              | if e then e else e | ref e | !e | (e : T)
//!              | e e | e BIN e
//!   pat      ::= _ | var | lit | `tag pat? | {f=p,..(,..)?} | (p)
//!   ty       ::= con | tvar | ty -> ty | {f:ty,..} | [ `tag:ty, .. ] | ty ref
//!
//! 顶层 `let` 序列后可放一个表达式作为程序结果类型。

use super::syntax::*;
use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Str(String),
    Name(String),   // 小写/下划线开头：变量、关键字、字段
    Tag(String),    // 反引号开头：变体标签（不含 `）
    Upper(String),  // 大写开头：类型构造器
    TVar(String),   // 单引号开头：类型变量
    Arrow,
    FatArrow,
    Colon,
    Comma,
    Dot,
    Eq,
    Assign,
    LPar,
    RPar,
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    Backtick,
    Bang,
    Pipe,
    Bin(String), // + - * / == != < > <= >= && || :: ++
    Eof,
}

#[derive(Clone, Debug)]
struct Tk {
    tok: Tok,
    lo: usize,
    hi: usize,
}

pub struct Lexer<'a> {
    b: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer { b: src.as_bytes(), pos: 0 }
    }

    fn skip_ws(&mut self) {
        loop {
            while self.pos < self.b.len() && matches!(self.b[self.pos], b' ' | b'\t' | b'\n' | b'\r') {
                self.pos += 1;
            }
            if self.pos + 1 < self.b.len() && self.b[self.pos] == b'(' && self.b[self.pos + 1] == b'*' {
                self.pos += 2;
                let mut depth = 1;
                while self.pos < self.b.len() && depth > 0 {
                    if self.pos + 1 < self.b.len() && self.b[self.pos] == b'(' && self.b[self.pos + 1] == b'*' {
                        depth += 1;
                        self.pos += 2;
                    } else if self.pos + 1 < self.b.len() && self.b[self.pos] == b'*' && self.b[self.pos + 1] == b')' {
                        depth -= 1;
                        self.pos += 2;
                    } else {
                        self.pos += 1;
                    }
                }
            } else {
                break;
            }
        }
    }

    pub fn next_tok(&mut self) -> Result<Tk, ParseError> {
        self.skip_ws();
        let lo = self.pos;
        if self.pos >= self.b.len() {
            return Ok(Tk { tok: Tok::Eof, lo, hi: self.pos });
        }
        let c = self.b[self.pos];
        // 数字
        if c.is_ascii_digit() {
            while self.pos < self.b.len() && self.b[self.pos].is_ascii_digit() {
                self.pos += 1;
            }
            let s = std::str::from_utf8(&self.b[lo..self.pos]).unwrap();
            return Ok(Tk { tok: Tok::Int(s.parse().unwrap()), lo, hi: self.pos });
        }
        // 字符串
        if c == b'"' {
            self.pos += 1;
            let mut out = String::new();
            while self.pos < self.b.len() && self.b[self.pos] != b'"' {
                let ch = self.b[self.pos];
                if ch == b'\\' && self.pos + 1 < self.b.len() {
                    self.pos += 1;
                    let e = self.b[self.pos];
                    out.push(match e {
                        b'n' => '\n',
                        b't' => '\t',
                        b'\\' => '\\',
                        b'"' => '"',
                        _ => e as char,
                    });
                    self.pos += 1;
                } else {
                    out.push(ch as char);
                    self.pos += 1;
                }
            }
            if self.pos >= self.b.len() {
                return Err(ParseError { msg: "字符串未闭合".into(), span: Span::new(lo, self.pos) });
            }
            self.pos += 1;
            return Ok(Tk { tok: Tok::Str(out), lo, hi: self.pos });
        }
        // 名字 / 关键字
        if c == b'_' || c.is_ascii_lowercase() {
            while self.pos < self.b.len()
                && (self.b[self.pos] == b'_' || self.b[self.pos].is_ascii_alphanumeric() || self.b[self.pos] == b'\'')
            {
                self.pos += 1;
            }
            let s = std::str::from_utf8(&self.b[lo..self.pos]).unwrap().to_string();
            let tok = match s.as_str() {
                "true" => Tok::Bool(true),
                "false" => Tok::Bool(false),
                _ => Tok::Name(s),
            };
            return Ok(Tk { tok, lo, hi: self.pos });
        }
        // 类型构造器 / 类型变量（'a）
        if c.is_ascii_uppercase() {
            while self.pos < self.b.len()
                && (self.b[self.pos] == b'_' || self.b[self.pos].is_ascii_alphanumeric())
            {
                self.pos += 1;
            }
            let s = std::str::from_utf8(&self.b[lo..self.pos]).unwrap().to_string();
            return Ok(Tk { tok: Tok::Upper(s), lo, hi: self.pos });
        }
        if c == b'\'' {
            // 'a（类型变量），也接受 '' 非法情况
            self.pos += 1;
            let vlo = self.pos;
            if self.pos < self.b.len()
                && (self.b[self.pos] == b'_' || self.b[self.pos].is_ascii_lowercase())
            {
                while self.pos < self.b.len()
                    && (self.b[self.pos] == b'_' || self.b[self.pos].is_ascii_alphanumeric() || self.b[self.pos] == b'\'')
                {
                    self.pos += 1;
                }
                let s = std::str::from_utf8(&self.b[vlo..self.pos]).unwrap().to_string();
                return Ok(Tk { tok: Tok::TVar(s), lo, hi: self.pos });
            }
            return Err(ParseError { msg: "类型变量应为 '小写字母".into(), span: Span::new(lo, self.pos) });
        }
        // 变体标签
        if c == b'`' {
            self.pos += 1;
            let tlo = self.pos;
            while self.pos < self.b.len()
                && (self.b[self.pos] == b'_' || self.b[self.pos].is_ascii_alphanumeric())
            {
                self.pos += 1;
            }
            if tlo == self.pos {
                return Err(ParseError { msg: "变体标签不能为空".into(), span: Span::new(lo, self.pos) });
            }
            let s = std::str::from_utf8(&self.b[tlo..self.pos]).unwrap().to_string();
            return Ok(Tk { tok: Tok::Tag(s), lo, hi: self.pos });
        }
        // 标点 / 运算符
        let two: Option<Tok> = if self.pos + 1 < self.b.len() {
            let s = std::str::from_utf8(&self.b[lo..self.pos + 2]).unwrap();
            match s {
                "->" => Some(Tok::Arrow),
                "=>" => Some(Tok::FatArrow),
                ":=" => Some(Tok::Assign),
                "==" => Some(Tok::Bin("==".into())),
                "!=" => Some(Tok::Bin("!=".into())),
                "<=" => Some(Tok::Bin("<=".into())),
                ">=" => Some(Tok::Bin(">=".into())),
                "&&" => Some(Tok::Bin("&&".into())),
                "||" => Some(Tok::Bin("||".into())),
                "::" => Some(Tok::Bin("::".into())),
                "++" => Some(Tok::Bin("++".into())),
                _ => None,
            }
        } else {
            None
        };
        if let Some(t) = two {
            self.pos += 2;
            return Ok(Tk { tok: t, lo, hi: self.pos });
        }
        let one = match c {
            b':' => Tok::Colon,
            b',' => Tok::Comma,
            b'.' => Tok::Dot,
            b'=' => Tok::Eq,
            b'(' => Tok::LPar,
            b')' => Tok::RPar,
            b'{' => Tok::LBrace,
            b'}' => Tok::RBrace,
            b'[' => Tok::LBrack,
            b']' => Tok::RBrack,
            b'|' => Tok::Pipe,
            b'!' => Tok::Bang,
            b'+' | b'-' | b'*' | b'/' | b'<' | b'>' => Tok::Bin((c as char).to_string()),
            _ => {
                return Err(ParseError { msg: format!("无法识别字符 {:?}", c as char), span: Span::new(lo, lo + 1) });
            }
        };
        self.pos += 1;
        Ok(Tk { tok: one, lo, hi: self.pos })
    }
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub msg: String,
    pub span: Span,
}

// 结构角色常量（参与 NodeId 派生）。
const R_PAT: u64 = 1;
const R_BODY: u64 = 2;
const R_FUN: u64 = 3;
const R_ARG: u64 = 4;
const R_RHS: u64 = 5;
const R_ANN: u64 = 6;
const R_FIELD_VAL: u64 = 7;
const R_PAYLOAD: u64 = 8;
const R_OBJ: u64 = 9;
const R_SCRUT: u64 = 10;
const R_ARM: u64 = 11;
const R_COND: u64 = 12;
const R_THEN: u64 = 13;
const R_ELSE: u64 = 14;
const R_TOP: u64 = 15;
const R_RESULT: u64 = 16;
const R_OPL: u64 = 17;
const R_OPR: u64 = 18;
const R_TYARG: u64 = 19;
const R_TYFIELD: u64 = 20;
const R_INNER: u64 = 21;
const R_TARGET: u64 = 22;
const R_VALUE: u64 = 23;
const R_TOPPAT: u64 = 24;
const R_TOPRHS: u64 = 25;
const R_TOPANN: u64 = 26;
const R_LET: u64 = 27;
const R_LAM: u64 = 28;
const R_APP: u64 = 29;
const R_REC: u64 = 30;
const R_VARIANT: u64 = 31;
const R_FIELD: u64 = 32;
const R_MATCH: u64 = 33;
const R_IF: u64 = 34;
const R_REF: u64 = 35;
const R_DEREF: u64 = 36;
const R_ANNEXPR: u64 = 37;
const R_ARMPAT: u64 = 38;
const R_ARMEXPR: u64 = 39;
const R_PATPAY: u64 = 40;
const R_PATFIELD: u64 = 41;
const R_RECFIELD: u64 = 42;
const R_TYREC: u64 = 43;
const R_TYVAR: u64 = 44;

pub fn parse(src: &str) -> Result<Ast, ParseError> {
    let mut p = Parser::new(src)?;
    p.parse_program()?;
    p.expect(Tok::Eof)?;
    Ok(p.ast)
}

struct Parser {
    toks: Vec<Tk>,
    idx: usize,
    ast: Ast,
}

impl Parser {
    fn new(src: &str) -> Result<Self, ParseError> {
        let mut lx = Lexer::new(src);
        let mut toks = Vec::new();
        loop {
            let t = lx.next_tok()?;
            let eof = matches!(t.tok, Tok::Eof);
            toks.push(t);
            if eof {
                break;
            }
        }
        Ok(Parser { toks, idx: 0, ast: Ast::default() })
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.idx].tok
    }
    fn peek_tk(&self) -> &Tk {
        &self.toks[self.idx]
    }
    fn bump(&mut self) -> Tk {
        let t = self.toks[self.idx].clone();
        if !matches!(t.tok, Tok::Eof) {
            self.idx += 1;
        }
        t
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if std::mem::discriminant(self.peek()) == std::mem::discriminant(t) {
            // 仅用于无载荷关键字比较
            if matches!(t, Tok::Name(_) | Tok::Bin(_)) {
                if self.peek() == t {
                    self.bump();
                    return true;
                }
                false
            } else {
                self.bump();
                true
            }
        } else {
            false
        }
    }
    fn expect(&mut self, t: Tok) -> Result<Tk, ParseError> {
        if self.peek() == &t {
            Ok(self.bump())
        } else {
            let tk = self.peek_tk().clone();
            Err(ParseError { msg: format!("期望 {:?}，遇到 {:?}", t, tk.tok), span: Span::new(tk.lo, tk.hi) })
        }
    }
    fn eat_name(&mut self, s: &str) -> bool {
        if matches!(self.peek(), Tok::Name(n) if n == s) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn add_expr(&mut self, id: NodeId, lo: usize, hi: usize, e: Expr) -> NodeId {
        self.ast.exprs.push(ExprNode { id, span: Span::new(lo, hi), node: e });
        id
    }
    fn add_pat(&mut self, id: NodeId, lo: usize, hi: usize, p: Pat) -> NodeId {
        self.ast.pats.push(PatNode { id, span: Span::new(lo, hi), node: p });
        id
    }
    fn add_ty(&mut self, id: NodeId, lo: usize, hi: usize, t: TyAst) -> NodeId {
        self.ast.tys.push(TyNode { id, span: Span::new(lo, hi), node: t });
        id
    }

    fn parse_program(&mut self) -> Result<(), ParseError> {
        let mut items = Vec::new();
        let mut tops = Vec::new();
        loop {
            match self.peek() {
                Tok::Name(n) if n == "let" => {
                    let lo = self.peek_tk().lo;
                    let (binds, rec_, hi) = self.parse_let_bind_seq()?;
                    // 顶层绑定：无 in
                    for bind in binds {
                        tops.push(TopBind { bind, rec_ });
                    }
                    let _ = (lo, hi);
                }
                Tok::Eof => break,
                _ => {
                    let e = self.parse_expr(NodeId::ROOT, R_RESULT, 0)?;
                    self.ast.program.result = Some(e);
                    items.push(e);
                    break;
                }
            }
        }
        self.ast.program.tops = tops;
        // Program 节点（容器，span 不重要）
        let span = self.toks.last().map(|t| t.hi).unwrap_or(0);
        self.add_expr(
            NodeId::ROOT,
            0,
            span,
            Expr::Program { items, result: self.ast.program.result },
        );
        Ok(())
    }

    /// 解析 `let [rec] p [: T] = e {and ...}`；要求已在 let 关键字上。
    fn parse_let_bind_seq(&mut self) -> Result<(Vec<LetBind>, bool, usize), ParseError> {
        let let_tk = self.bump(); // let
        let _ = let_tk;
        let rec_ = self.eat_name("rec");
        let mut binds = Vec::new();
        loop {
            let pat = self.parse_pat(NodeId::ROOT, R_TOPPAT, binds.len() as u64)?;
            let ann = if self.eat(&Tok::Colon) {
                Some(self.parse_ty(NodeId::ROOT, R_TOPANN, binds.len() as u64)?)
            } else {
                None
            };
            self.expect(Tok::Eq)?;
            let rhs = self.parse_expr(NodeId::ROOT, R_TOPRHS, binds.len() as u64)?;
            binds.push(LetBind { pat, rhs, ann });
            if self.eat_name("and") {
                continue;
            }
            break;
        }
        let hi = self.toks[self.idx.saturating_sub(1)].hi;
        Ok((binds, rec_, hi))
    }

    // ---------- 表达式 ----------

    pub(crate) fn parse_expr(&mut self, parent: NodeId, role: u64, index: u64) -> Result<NodeId, ParseError> {
        let id = parent.child(role, index);
        self.parse_expr_at(id, 0)
    }

    fn bin_info(op: &str) -> Option<(u8, &'static str)> {
        Some(match op {
            "||" => (1, "||"),
            "&&" => (2, "&&"),
            "==" | "!=" => (3, op),
            "<" | ">" | "<=" | ">=" => (4, op),
            "++" | "::" => (5, op),
            "+" | "-" => (6, op),
            "*" | "/" => (7, op),
            _ => return None,
        })
    }

    fn parse_expr_at(&mut self, id: NodeId, min_bp: u8) -> Result<NodeId, ParseError> {
        let lo = self.peek_tk().lo;
        let mut lhs = self.parse_unary(id, lo)?;

        loop {
            // 后缀：字段访问 / 调用参数 / 赋值
            match self.peek() {
                Tok::Dot => {
                    self.bump();
                    let name = match self.peek().clone() {
                        Tok::Name(n) => n,
                        other => {
                            return Err(ParseError { msg: format!("字段名应为标识符，遇到 {:?}", other), span: self.peek_tk().span() });
                        }
                    };
                    let flo = self.peek_tk().lo;
                    let fhi = self.peek_tk().hi;
                    self.bump();
                    let fid = id.child(R_FIELD, field_role(&name));
                    let inner = self.ast.enode(lhs).unwrap().id;
                    let obj = lhs;
                    // 可能接 := 赋值
                    if self.eat(&Tok::Assign) {
                        let val = self.parse_expr(fid, R_VALUE, 0)?;
                        let hi = self.prev_hi();
                        lhs = self.add_expr(fid, lo, hi, Expr::Assign { target: self.make_field(obj, name, flo, fhi, inner)?, value: val });
                    } else {
                        lhs = self.make_field(obj, name, flo, fhi, inner)?;
                        let _ = fid;
                    }
                    continue;
                }
                Tok::Bin(op) if op == "=" => break,
                _ => {}
            }

            // 二元运算符
            let op = match self.peek().clone() {
                Tok::Bin(op) => op,
                _ => break,
            };
            let (bp, _name) = match Self::bin_info(&op) {
                Some(x) => x,
                None => break,
            };
            if bp < min_bp {
                break;
            }
            let op_tk = self.bump();
            // 右结合（::）用 bp，左结合用 bp+1
            let rbp = if op == "::" { bp } else { bp + 1 };
            let rhs = self.parse_expr_at(id.child(R_OPR, 0), rbp)?;
            let hi = self.prev_hi();
            // 重建左值：把 lhs 包进新 id.child(R_OPL,0) —— 但节点 id 已固定，
            // 这里用统一“应用”表示：内置运算符视为应用到一个内部 Var。
            lhs = self.make_binop(id, lo, hi, &op, op_tk.lo, lhs, rhs)?;
            continue;
        }
        Ok(lhs)
    }

    fn prev_hi(&self) -> usize {
        self.toks[self.idx.saturating_sub(1)].hi
    }

    /// 把二元运算符改写为应用：`(fun -> <op>) lhs rhs`，内置名字以 `(+)` 等表示。
    fn make_binop(
        &mut self,
        id: NodeId,
        lo: usize,
        hi: usize,
        op: &str,
        op_lo: usize,
        lhs: NodeId,
        rhs: NodeId,
    ) -> Result<NodeId, ParseError> {
        let vid = id.child(R_OPL, 0);
        let vname = format!("({})", op);
        self.add_expr(vid, op_lo, self.toks[self.idx.saturating_sub(1)].hi, Expr::Var(vname));
        // 参数的顺序：先 rhs? 需要两个参数节点；lhs 已用某 id。
        // 为保持确定 id：args[0] = id.child(R_ARG,0), args[1] = id.child(R_ARG,1)
        let a0 = id.child(R_ARG, 0);
        let a1 = id.child(R_ARG, 1);
        let _ = (a0, a1);
        // lhs 的既有 id 不会等于 a0（lhs 由 id.child(R_OPL,0) 开始解析）。
        // 因此这里把 lhs 原样作为 args[0]，rhs 作为 args[1]，图上仍可展示。
        self.add_expr(id, lo, hi, Expr::App { fun: vid, args: vec![lhs, rhs] });
        Ok(id)
    }

    fn make_field(&mut self, obj: NodeId, name: String, flo: usize, fhi: usize, inner: NodeId) -> Result<NodeId, ParseError> {
        let fid = inner.child(R_FIELD, field_role(&name));
        let _ = (flo, fhi);
        self.add_expr(fid, self.ast.enode(obj).unwrap().span.lo as usize, fhi, Expr::Field { obj, label: name });
        Ok(fid)
    }

    fn parse_unary(&mut self, id: NodeId, lo: usize) -> Result<NodeId, ParseError> {
        match self.peek().clone() {
            Tok::Bang => {
                self.bump();
                let e = self.parse_expr(id, R_INNER, 0)?;
                let hi = self.prev_hi();
                Ok(self.add_expr(id, lo, hi, Expr::Deref(e)))
            }
            Tok::Name(n) if n == "ref" && self.is_keyword_start() => {
                self.bump();
                let e = self.parse_expr(id, R_INNER, 0)?;
                let hi = self.prev_hi();
                Ok(self.add_expr(id, lo, hi, Expr::NewRef(e)))
            }
            _ => self.parse_app(id, lo),
        }
    }

    fn is_keyword_start(&self) -> bool {
        // ref 后跟表达式即可（不与普通变量消歧——这里保留 ref 作为关键字）。
        true
    }
