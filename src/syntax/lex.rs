use super::ast::Span;

#[derive(Clone, PartialEq, Debug)]
pub enum Tok {
    Int(i64),
    Ident(String),
    Tag(String), // 大写开头：变体标签
    /// 小写字段名（记录字段、投影）
    Field(String),
    Let,
    Rec,
    In,
    If,
    Then,
    Else,
    Fn,
    Match,
    With,
    True,
    False,
    Eq,     // =
    Arrow,  // ->
    Fat,    // =>
    Plus,
    Minus,
    Star,
    Lt,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Bar,
    Underscore,
    Eof,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub tok: Tok,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub msg: String,
    pub span: Span,
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}
fn is_ident_cont(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric() || c == '\''
}

pub fn lex(src: &str) -> Result<Vec<Token>, LexError> {
    let bytes = src.as_bytes();
    let mut i = 0usize;
    let mut out = Vec::new();
    let mk = |tok: Tok, start: usize, end: usize| Token {
        tok,
        span: Span {
            start: start as u32,
            end: end as u32,
        },
    };
    while i < bytes.len() {
        let c = bytes[i] as char;
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        if c == '(' && bytes.get(i + 1) == Some(&b'*') {
            let start = i;
            i += 2;
            let mut depth = 1;
            while i < bytes.len() && depth > 0 {
                if bytes[i] == b'(' && bytes.get(i + 1) == Some(&b'*') {
                    depth += 1;
                    i += 2;
                } else if bytes[i] == b'*' && bytes.get(i + 1) == Some(&b')') {
                    depth -= 1;
                    i += 2;
                } else {
                    i += 1;
                }
            }
            if depth > 0 {
                return Err(LexError {
                    msg: "未闭合的注释 (*".into(),
                    span: Span {
                        start: start as u32,
                        end: start as u32 + 2,
                    },
                });
            }
            continue;
        }
        let start = i;
        if c.is_ascii_digit() {
            while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                i += 1;
            }
            let n: i64 = src[start..i]
                .parse()
                .map_err(|_| LexError {
                    msg: "整数字面量超出范围".into(),
                    span: Span {
                        start: start as u32,
                        end: i as u32,
                    },
                })?;
            out.push(mk(Tok::Int(n), start, i));
            continue;
        }
        if is_ident_start(c) {
            i += 1;
            while i < bytes.len() && is_ident_cont(bytes[i] as char) {
                i += 1;
            }
            let word = &src[start..i];
            let tok = match word {
                "let" => Tok::Let,
                "rec" => Tok::Rec,
                "in" => Tok::In,
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "fn" => Tok::Fn,
                "match" => Tok::Match,
                "with" => Tok::With,
                "true" => Tok::True,
                "false" => Tok::False,
                "_" => Tok::Underscore,
                _ if c.is_ascii_uppercase() => Tok::Tag(word.to_string()),
                _ => Tok::Ident(word.to_string()),
            };
            out.push(mk(tok, start, i));
            continue;
        }
        // 字段名以点号引导时在解析器处理；这里只处理符号。
        macro_rules! push {
            ($t:expr, $n:expr) => {{
                i += $n;
                out.push(mk($t, start, i));
            }};
        }
        match c {
            '=' => {
                if bytes.get(i + 1) == Some(&b'>') {
                    push!(Tok::Fat, 2);
                } else {
                    push!(Tok::Eq, 1);
                }
            }
            '-' if bytes.get(i + 1) == Some(&b'>') => push!(Tok::Arrow, 2),
            '+' => push!(Tok::Plus, 1),
            '-' => push!(Tok::Minus, 1),
            '*' => push!(Tok::Star, 1),
            '<' => push!(Tok::Lt, 1),
            '(' => push!(Tok::LParen, 1),
            ')' => push!(Tok::RParen, 1),
            '{' => push!(Tok::LBrace, 1),
            '}' => push!(Tok::RBrace, 1),
            ';' => push!(Tok::Semicolon, 1),
            '|' => push!(Tok::Bar, 1),
            '.' => {
                // 字段：.name（小写开头，普通标识符字符）
                if let Some(&n) = bytes.get(i + 1) {
                    let nc = n as char;
                    if nc.is_ascii_lowercase() {
                        i += 2;
                        while i < bytes.len() && is_ident_cont(bytes[i] as char) {
                            i += 1;
                        }
                        out.push(mk(Tok::Field(src[start + 1..i].to_string()), start, i));
                        continue;
                    }
                }
                return Err(LexError {
                    msg: format!("意外的字符 {:?}", c),
                    span: Span {
                        start: start as u32,
                        end: start as u32 + 1,
                    },
                });
            }
            _ => {
                return Err(LexError {
                    msg: format!("意外的字符 {:?}", c),
                    span: Span {
                        start: start as u32,
                        end: start as u32 + 1,
                    },
                });
            }
        }
    }
    out.push(mk(Tok::Eof, i, i));
    Ok(out)
}
