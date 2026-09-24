//! 词法、语法与稳定节点身份。

pub mod ast;
pub mod lex;
pub mod parse;

pub use ast::*;
pub use parse::{parse, ParseError};

#[inline]
fn fnv_update(h: &mut u64, bytes: &[u8]) {
    for &b in bytes {
        *h ^= b as u64;
        *h = h.wrapping_mul(0x100000001b3);
    }
}

#[inline]
fn child(parent: u64, role: &str) -> u64 {
    let mut h = parent;
    fnv_update(&mut h, &[0x2f]);
    fnv_update(&mut h, role.as_bytes());
    h
}

fn assign_pat(p: &mut SPat, id: u64) {
    p.nid = id;
    match &mut p.node {
        Pat::Wild | Pat::Binding(_) | Pat::Unit => {}
    }
}

fn assign(e: &mut SExpr, id: u64) {
    e.nid = id;
    match &mut e.node {
        Expr::Int(_)
        | Expr::Bool(_)
        | Expr::Unit
        | Expr::Ident(_) => {}
        Expr::Binop { lhs, rhs, op } => {
            let mut role = String::from("op:");
            role.push(*op);
            let _ = role;
            assign(lhs, child(id, "l"));
            assign(rhs, child(id, "r"));
        }
        Expr::If {
            cond,
            then_e,
            else_e,
        } => {
            assign(cond, child(id, "c"));
            assign(then_e, child(id, "t"));
            assign(else_e, child(id, "e"));
        }
        Expr::Fn { body, .. } => {
            assign(body, child(id, "body"));
        }
        Expr::App { fun, arg } => {
            assign(fun, child(id, "f"));
            assign(arg, child(id, "a"));
        }
        Expr::Let {
            bound, body, ..
        } => {
            assign(bound, child(id, "bound"));
            assign(body, child(id, "body"));
        }
        Expr::Record { fields } => {
            for (i, (_, fe)) in fields.iter_mut().enumerate() {
                assign(fe, child(id, &format!("fld{}", i)));
            }
        }
        Expr::Extend {
            field,
            value,
            rest,
        } => {
            assign(value, child(id, &format!("extv:{}", field)));
            assign(rest, child(id, &format!("extr:{}", field)));
        }
        Expr::Project { record, .. } => {
            assign(record, child(id, "proj"));
        }
        Expr::Variant { payload, .. } => {
            assign(payload, child(id, "payload"));
        }
        Expr::Match {
            scrut,
            clause_pat,
            clause_body,
            default,
            ..
        } => {
            assign(scrut, child(id, "scrut"));
            assign_pat(clause_pat, child(id, "pat"));
            assign(clause_body, child(id, "cbody"));
            assign(default, child(id, "default"));
        }
        Expr::Paren(inner) => {
            assign(inner, child(id, "paren"));
        }
    }
}

/// 为整棵程序计算稳定节点 id：只依赖结构角色与父路径，不依赖变量名或源码偏移。
pub fn assign_ids(prog: &mut Program) {
    for (i, item) in prog.items.iter_mut().enumerate() {
        let id = child(0xcbf29ce484222325, &format!("item{}", i));
        assign(&mut item.expr, id);
    }
}
