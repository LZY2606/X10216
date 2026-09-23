// 词法分析、语法分析、源码范围（span）与跨编辑稳定的身份指纹。
//
// 设计要点：
// - 每个 AST 节点都有唯一、确定的 NodeId（解析顺序下标），并携带完整 Span。
// - 节点身份（fp）在解析后的一次遍历中计算，分两部分：
//     * path_key：由“在父节点中的槽位”决定，变量改名不会改变其它节点身份。
//     * content  ：由子树的语义内容决定（不含字节偏移），用于判断节点语义是否复用。
// - 约束、类型变量都从 fp 派生，从而保证改名不导致身份漂移。

use serde::Serialize;
use std::fmt;

pub type NodeId = usize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Default)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
    pub fn merge(self, other: Span) -> Span {
        Span {
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum PatKind {
    Var(String),
    Wild,
    Tag(String, Option<NodeId>),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
    /// 绑定引入的变量节点位置（用于生成 binder key）。
    pub binder_slot: usize,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Arm {
    pub pat: Pat,
    pub body: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Field {
    pub label: String,
    pub label_span: Span,
    pub value: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum EK {
    Int(i64),
    Str(String),
    Bool(bool),
    Var(String),
    Fun { param: Pat, body: NodeId },
    App { fun: NodeId, arg: NodeId },
    Let {
        rec: bool,
        pat: Pat,
        bind: NodeId,
        body: Option<NodeId>,
    },
    If { cond: NodeId, then: NodeId, els: NodeId },
    Record { fields: Vec<Field>, closed: bool },
    Proj { obj: NodeId, label: String, label_span: Span },
    Tag { label: String, payload: Option<NodeId> },
    Match { scrut: NodeId, arms: Vec<Arm> },
    Unit,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Expr {
    pub kind: EK,
    pub span: Span,
    /// 路径身份：由槽位决定（跨编辑复用判定）。
    pub fp: u64,
    /// 语义内容哈希（不含字节偏移）。
    pub content: u64,
    /// 标识符解析到的声明节点 fp（Var 节点填充）。
    pub binder_fp: Option<u64>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum TopItem {
    Let {
        rec: bool,
        pat: Pat,
        bind: NodeId,
        span: Span,
    },
    Expr(NodeId),
}

#[derive(Clone, Debug, Serialize, PartialEq)]
pub struct Program {
    pub items: Vec<TopItem>,
    pub exprs: Vec<Expr>,
    pub source: String,
}

// ---------------- 词法 ----------------

#[derive(Clone, Debug, PartialEq)]
pub enum TokKind {
    Int(i64),
    Str(String),
    Ident(String),
    TagLit(String),
    Let, Rec, In, Fun, If, Then, Else, True, False, Match, With, Underscore,
    Eq, Arrow, FatArrow, Dot, Comma, Semicolon, LBrace, RBrace, LParen, RParen,
    Pipe, Star,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    kind: TokKind,
    span: Span,
}

#[derive(Clone, Debug)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}: {}", self.span.lo, self.span.hi, self.message)
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_alphabetic()
}
fn is_ident_cont(c: char) -> bool {
    c == '_' || c == '\'' || c.is_alphanumeric()
}

pub fn lex(source: &str) -> Result<Vec<Token>, ParseError> {
    let bytes = source.as_bytes();
    let mut i = 0usize;
    let mut out = Vec::new();
    let b = |i: usize| bytes[i] as char;
    while i < bytes.len() {
        let c = b(i);
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        // 行注释 --
        if c == '-' && i + 1 < bytes.len() && b(i + 1) == '-' {
            while i < bytes.len() && b(i) != '\n' {
                i += 1;
            }
            continue;
        }
        let lo = i;
        if c.is_ascii_digit() {
            let start = i;
            while i < bytes.len() && b(i).is_ascii_digit() {
                i += 1;
            }
            let text = &source[start..i];
            let n: i64 = text.parse().map_err(|_| ParseError {
                span: Span::new(lo, i),
                message: "整数字面量越界".into(),
            })?;
            out.push(Token { kind: TokKind::Int(n), span: Span::new(lo, i) });
            continue;
        }
        if c == '"' {
            i += 1;
            let start = i;
            while i < bytes.len() && b(i) != '"' {
                if b(i) == '\\' && i + 1 < bytes.len() {
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if i >= bytes.len() {
                return Err(ParseError {
                    span: Span::new(lo, bytes.len()),
                    message: "字符串缺少结束引号".into(),
                });
            }
            let text = unescape(&source[start..i]);
            i += 1;
            out.push(Token { kind: TokKind::Str(text), span: Span::new(lo, i) });
            continue;
        }
        if c == '`' {
            i += 1;
            let start = i;
            if i >= bytes.len() || !is_ident_start(b(i)) {
                return Err(ParseError {
                    span: Span::new(lo, i),
                    message: "变体标签 ` 后应为标识符".into(),
                });
            }
            i += 1;
            while i < bytes.len() && is_ident_cont(b(i)) {
                i += 1;
            }
            let name = source[start..i].to_string();
            out.push(Token { kind: TokKind::TagLit(name), span: Span::new(lo, i) });
            continue;
        }
        if is_ident_start(c) {
            let start = i;
            i += 1;
            while i < bytes.len() && is_ident_cont(b(i)) {
                i += 1;
            }
            let word = &source[start..i];
            let kind = match word {
                "let" => TokKind::Let,
                "rec" => TokKind::Rec,
                "in" => TokKind::In,
                "fun" => TokKind::Fun,
                "if" => TokKind::If,
                "then" => TokKind::Then,
                "else" => TokKind::Else,
                "true" => TokKind::True,
                "false" => TokKind::False,
                "match" => TokKind::Match,
                "with" => TokKind::With,
                "_" => TokKind::Underscore,
                _ => TokKind::Ident(word.to_string()),
            };
            out.push(Token { kind, span: Span::new(start, i) });
            continue;
        }
        let two: Option<TokKind> = if i + 1 < bytes.len() {
            match &source[i..i + 2] {
                "->" => Some(TokKind::Arrow),
                "=>" => Some(TokKind::FatArrow),
                _ => None,
            }
        } else {
            None
        };
        if let Some(k) = two {
            out.push(Token { kind: k, span: Span::new(i, i + 2) });
            i += 2;
            continue;
        }
        let one = match c {
            '=' => TokKind::Eq,
            '.' => TokKind::Dot,
            ',' => TokKind::Comma,
            ';' => TokKind::Semicolon,
            '{' => TokKind::LBrace,
            '}' => TokKind::RBrace,
            '(' => TokKind::LParen,
            ')' => TokKind::RParen,
            '|' => TokKind::Pipe,
            '*' => TokKind::Star,
            _ => {
                return Err(ParseError {
                    span: Span::new(i, i + 1),
                    message: format!("无法识别的字符 {:?}", c),
                });
            }
        };
        out.push(Token { kind: one, span: Span::new(i, i + 1) });
        i += 1;
    }
    out.push(Token { kind: TokKind::Eof, span: Span::new(bytes.len(), bytes.len()) });
    Ok(out)
}

fn unescape(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    let mut chars = raw.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('"') => out.push('"'),
                Some('\\') => out.push('\\'),
                Some(other) => out.push(other),
                None => {}
            }
        } else {
            out.push(c);
        }
    }
    out
}
