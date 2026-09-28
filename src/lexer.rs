// 手写词法分析器：为每个 token 记录字节范围（来源切片的基础）。
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn len(&self) -> usize {
        self.1.saturating_sub(self.0)
    }
    pub fn is_empty(&self) -> bool {
        self.0 >= self.1
    }
    pub fn merge(&self, other: &Span) -> Span {
        Span(self.0.min(other.0), self.1.max(other.1))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Tok {
    Let, Rec, In, Fn, Arrow, FatArrow, If, Then, Else, True, False,
    Match, With, Underscore,
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Eq, Comma, Dot, Colon, ColonColon, Pipe, Backtick,
    IntLit, StrLit, Ident, UpperIdent, Eof,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Token {
    pub tok: Tok,
    pub text: String,
    pub int: Option<i64>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LexError {
    pub span: Span,
    pub message: String,
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    while i < b.len() {
        let c = b[i];
        if c.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        let start = i;
        if c.is_ascii_digit() {
            while i < b.len() && b[i].is_ascii_digit() {
                i += 1;
            }
            let text = &src[start..i];
            let int = text.parse::<i64>().map_err(|_| LexError {
                span: Span(start, i),
                message: "整数字面量超出范围".to_string(),
            })?;
            out.push(Token { tok: Tok::IntLit, text: text.to_string(), int: Some(int), span: Span(start, i) });
            continue;
        }
        if c == b'"' {
            i += 1;
            let mut value = String::new();
            loop {
                if i >= b.len() {
                    return Err(LexError { span: Span(start, i), message: "字符串未闭合".to_string() });
                }
                let ch = b[i];
                if ch == b'"' {
                    i += 1;
                    break;
                }
                if ch == b'\\' {
                    i += 1;
                    if i >= b.len() {
                        return Err(LexError { span: Span(start, i), message: "字符串转义未完成".to_string() });
                    }
                    match b[i] {
                        b'n' => value.push('\n'),
                        b't' => value.push('\t'),
                        b'\\' => value.push('\\'),
                        b'"' => value.push('"'),
                        b'r' => value.push('\r'),
                        b'0' => value.push('\0'),
                        other => value.push(other as char),
                    }
                    i += 1;
                } else {
                    // 收集 UTF-8 字符
                    let rest = &src[i..];
                    let ch = rest.chars().next().unwrap();
                    value.push(ch);
                    i += ch.len_utf8();
                }
            }
            out.push(Token { tok: Tok::StrLit, text: value, int: None, span: Span(start, i) });
            continue;
        }
        if c == b'_' || c.is_ascii_alphabetic() {
            while i < b.len() {
                let ch = b[i];
                if ch == b'_' || ch.is_ascii_alphanumeric() || (ch >= 0x80) {
                    i += 1;
                } else {
                    break;
                }
            }
            let text = &src[start..i];
            let tok = match text {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fn" => Tok::Fn,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "true" => Tok::True,
                "false" => Tok::False,
                "match" => Tok::Match,
                "with" => Tok::With,
                "_" => Tok::Underscore,
                _ => {
                    if text.chars().next().unwrap().is_ascii_uppercase() {
                        Tok::UpperIdent
                    } else {
                        Tok::Ident
                    }
                }
            };
            out.push(Token { tok, text: text.to_string(), int: None, span: Span(start, i) });
            continue;
        }
        // 多字符 / 单字符符号
        let two: &str = if i + 2 <= b.len() { &src[i..i + 2] } else { "" };
        let (tok, n) = match two {
            "->" => (Tok::Arrow, 2),
            "=>" => (Tok::FatArrow, 2),
            "::" => (Tok::ColonColon, 2),
            "`" => (Tok::Backtick, 1),
            _ => {
                let t = match c {
                    b'(' => Tok::LParen,
                    b')' => Tok::RParen,
                    b'{' => Tok::LBrace,
                    b'}' => Tok::RBrace,
                    b'[' => Tok::LBracket,
                    b']' => Tok::RBracket,
                    b'=' => Tok::Eq,
                    b',' => Tok::Comma,
                    b'.' => Tok::Dot,
                    b':' => Tok::Colon,
                    b'|' => Tok::Pipe,
                    b'`' => Tok::Backtick,
                    _ => {
                        return Err(LexError {
                            span: Span(i, i + 1),
                            message: format!("无法识别的字符 {:?}", c as char),
                        });
                    }
                };
                (t, 1)
            }
        };
        let end = i + n;
        out.push(Token { tok, text: src[i..end].to_string(), int: None, span: Span(start, end) });
        i = end;
    }
    let len = b.len();
    out.push(Token { tok: Tok::Eof, text: String::new(), int: None, span: Span(len, len) });
    Ok(out)
}
