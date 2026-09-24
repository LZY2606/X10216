//! 词法分析：输出带字节范围的 token 流。

use crate::ast::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Int(i64),
    Bool(bool),
    Str(String),
    Ident(String),
    // 关键字
    Let, Rec, In, Fun, If, Then, Else, True, False, Match, With, Wild,
    // 标点
    LParen, RParen, LBrace, RBrace,
    Eq, Arrow, FatArrow, Dot, Comma, Colon, Semicolon, Pipe, Backslash,
    // 行结尾（用于推断 let 隐式 in）
    Newline,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

pub fn lex(src: &str) -> Result<Vec<Token>, (Span, String)> {
    let b = src.as_bytes();
    let mut i = 0;
    let mut out = Vec::new();
    let mut at_line_start = true;
    let mut paren_depth = 0i32;
    let mut brace_depth = 0i32;
    while i < b.len() {
        let c = b[i] as char;
        if c == ' ' || c == '\t' || c == '\r' {
            at_line_start = false;
            i += 1;
            continue;
        }
        if c == '\n' {
            if at_line_start {
                // 忽略仅含缩进的换行
                i += 1;
                continue;
            }
            if paren_depth == 0 && brace_depth == 0 {
                out.push(Token { tok: Tok::Newline, span: Span::new(i, i + 1) });
            }
            i += 1;
            at_line_start = true;
            continue;
        }
        // 注释 # ... 至行尾
        if c == '#' {
            while i < b.len() && b[i] != b'\n' {
                i += 1;
            }
            continue;
        }
        let lo = i;
        if c.is_ascii_digit() {
            while i < b.len() && (b[i] as char).is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[lo..i].parse().map_err(|_| {
                (Span::new(lo, i), "整数字面量超出范围".to_string())
            })?;
            out.push(Token { tok: Tok::Int(n), span: Span::new(lo, i) });
            at_line_start = false;
            continue;
        }
        if c == '_' || c.is_ascii_alphabetic() {
            while i < b.len()
                && ((b[i] as char).is_ascii_alphanumeric() || b[i] == b'_' || b[i] == b'\'')
            {
                i += 1;
            }
            let word = &src[lo..i];
            let tok = match word {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "fun" => Tok::Fun,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "true" => Tok::True,
                "false" => Tok::False,
                "match" => Tok::Match,
                "with" => Tok::With,
                "_" => Tok::Wild,
                _ => Tok::Ident(word.to_string()),
            };
            let tok = match tok {
                Tok::True => Tok::Bool(true),
                Tok::False => Tok::Bool(false),
                t => t,
            };
            out.push(Token { tok, span: Span::new(lo, i) });
            at_line_start = false;
            continue;
        }
        if c == '"' {
            i += 1;
            let start = i;
            let mut s = String::new();
            while i < b.len() && b[i] != b'"' {
                if b[i] == b'\\' && i + 1 < b.len() {
                    let e = b[i + 1] as char;
                    s.push(match e {
                        'n' => '\n',
                        't' => '\t',
                        '"' => '"',
                        '\\' => '\\',
                        other => other,
                    });
                    i += 2;
                } else {
                    s.push(b[i] as char);
                    i += 1;
                }
            }
            if i >= b.len() {
                return Err((Span::new(lo, i), "字符串字面量未闭合".to_string()));
            }
            i += 1; // 跳过右引号
            let _ = start;
            out.push(Token { tok: Tok::Str(s), span: Span::new(lo, i) });
            at_line_start = false;
            continue;
        }
        // 双字符与单字符
        let two = if i + 1 < b.len() { &src[i..i + 2] } else { "" };
        let (tok, n) = match two {
            "->" => (Tok::Arrow, 2),
            "=>" => (Tok::FatArrow, 2),
            _ => match c {
                '(' => {
                    paren_depth += 1;
                    (Tok::LParen, 1)
                }
                ')' => {
                    paren_depth -= 1;
                    (Tok::RParen, 1)
                }
                '{' => {
                    brace_depth += 1;
                    (Tok::LBrace, 1)
                }
                '}' => {
                    brace_depth -= 1;
                    (Tok::RBrace, 1)
                }
                '=' => (Tok::Eq, 1),
                '.' => (Tok::Dot, 1),
                ',' => (Tok::Comma, 1),
                ':' => (Tok::Colon, 1),
                ';' => (Tok::Semicolon, 1),
                '|' => (Tok::Pipe, 1),
                '\\' => (Tok::Backslash, 1),
                _ => {
                    return Err((
                        Span::new(i, i + 1),
                        format!("无法识别的字符 {:?}", c),
                    ))
                }
            },
        };
        i += n;
        out.push(Token { tok, span: Span::new(lo, i) });
        at_line_start = false;
    }
    out.push(Token { tok: Tok::Eof, span: Span::new(b.len(), b.len()) });
    Ok(out)
}
