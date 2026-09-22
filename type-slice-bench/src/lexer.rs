//! 词法分析：产生带 Span 的 token 流。

use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Tok {
    pub kind: TokKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokKind {
    Int(i64),
    Str(String),
    Ident(String),
    // 关键字原样存放在 Str 中，由 parser 判断
    Op(String),
    Eof,
}

pub fn lex(src: &str) -> Result<Vec<Tok>, LexError> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    loop {
        while i < b.len() && (b[i] as char).is_whitespace() {
            i += 1;
        }
        if i >= b.len() {
            break;
        }
        let lo = i;
        let c = b[i] as char;
        if c.is_ascii_digit() {
            let mut n = 0i64;
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                n = n.wrapping_mul(10).wrapping_add((b[i] - b'0') as i64);
                i += 1;
            }
            out.push(Tok { kind: TokKind::Int(n), span: Span::new(lo, i) });
        } else if c.is_ascii_alphabetic() || c == '_' {
            while i < b.len() && {
                let ch = b[i] as char;
                ch.is_ascii_alphanumeric() || ch == '_' || ch == '\''
            } {
                i += 1;
            }
            let s = &src[lo..i];
            out.push(Tok { kind: TokKind::Str(s.to_string()), span: Span::new(lo, i) });
        } else if c == '"' {
            i += 1;
            let start = i;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                let ch = b[i] as char;
                if ch == '\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    match e {
                        'n' => s.push('\n'),
                        't' => s.push('\t'),
                        '"' => s.push('"'),
                        '\\' => s.push('\\'),
                        other => s.push(other),
                    }
                    i += 2;
                } else {
                    s.push(ch);
                    i += 1;
                }
            }
            let _ = start;
            if i >= b.len() {
                return Err(LexError { span: Span::new(lo, i), msg: "字符串未闭合".into() });
            }
            i += 1; // 收尾引号
            out.push(Tok { kind: TokKind::Ident(s), span: Span::new(lo, i) });
        } else {
            // 多字符操作符
            let two: Option<&str> = if i + 1 < b.len() {
                Some(&src[i..i + 2])
            } else {
                None
            };
            let two_ops = ["->", "==", "!=", "<=", ">=", "&&", "||"];
            if let Some(t) = two {
                if two_ops.contains(&t) {
                    out.push(Tok { kind: TokKind::Op(t.to_string()), span: Span::new(lo, lo + 2) });
                    i += 2;
                    continue;
                }
            }
            let one: &str = &src[i..i + 1];
            if "(){}[],.;:+-*/=<>|#@".contains(c) {
                out.push(Tok { kind: TokKind::Op(one.to_string()), span: Span::new(lo, lo + 1) });
                i += 1;
            } else {
                return Err(LexError {
                    span: Span::new(lo, i + 1),
                    msg: format!("无法识别的字符 {:?}", c),
                });
            }
        }
    }
    out.push(Tok { kind: TokKind::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub span: Span,
    pub msg: String,
}
