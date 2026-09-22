//! 稳定身份:对 AST 做 α-规范化(绑定变量用 de Bruijn 索引)后取结构哈希。
//! 变量一致改名不会改变身份;缓存 key 另含作用域与泛化环境签名。
use crate::syntax::{BinOp, Expr, Node, Program};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

pub type SemId = u64;

pub fn hash_str(s: &str) -> u64 {
    let mut h = DefaultHasher::new();
    s.hash(&mut h);
    h.finish()
}

fn canon(e: &Node, bound: &mut Vec<String>, out: &mut String) {
    match &e.kind {
        Expr::Int(n) => out.push_str(&format!("(i{})", n)),
        Expr::Bool(b) => out.push_str(&format!("(b{})", b)),
        Expr::Var(x) => {
            // 内层绑定优先:de Bruijn 索引;自由变量保留名字。
            match bound.iter().rposition(|b| b == x) {
                Some(i) => out.push_str(&format!("(v#{})", bound.len() - 1 - i)),
                None => out.push_str(&format!("(v{})", x)),
            }
        }
        Expr::Fun { body, .. } => {
            out.push_str("(fun ");
            bound.push(String::new());
            canon(body, bound, out);
            bound.pop();
            out.push(')');
        }
        Expr::App(f, a) => {
            out.push_str("(app ");
            canon(f, bound, out);
            canon(a, bound, out);
            out.push(')');
        }
        Expr::Let { rec_, rhs, body, .. } => {
            out.push_str(if *rec_ { "(letrec " } else { "(let " });
            bound.push(String::new());
            if *rec_ {
                canon(rhs, bound, out);
            } else {
                bound.pop();
                canon(rhs, bound, out);
                bound.push(String::new());
            }
            canon(body, bound, out);
            bound.pop();
            out.push(')');
        }
        Expr::If(c, t, e) => {
            out.push_str("(if ");
            canon(c, bound, out);
            canon(t, bound, out);
            canon(e, bound, out);
            out.push(')');
        }
        Expr::BinOp(op, l, r) => {
            let s = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Eq => "==",
                BinOp::Lt => "<",
            };
            out.push_str(&format!("(op{} ", s));
            canon(l, bound, out);
            canon(r, bound, out);
            out.push(')');
        }
        Expr::Record(fs) => {
            out.push_str("(rec");
            for (n, v) in fs {
                out.push_str(&format!("({}=", n));
                canon(v, bound, out);
                out.push(')');
            }
            out.push(')');
        }
        Expr::Field(e, n) => {
            out.push_str(&format!("(fld{} ", n));
            canon(e, bound, out);
            out.push(')');
        }
        Expr::Inj(tag, e) => {
            out.push_str(&format!("(inj{} ", tag));
            canon(e, bound, out);
            out.push(')');
        }
        Expr::Match(s, cases) => {
            out.push_str("(match ");
            canon(s, bound, out);
            for (tag, _, body) in cases {
                out.push_str(&format!("(case{} ", tag));
                bound.push(String::new());
                canon(body, bound, out);
                bound.pop();
                out.push(')');
            }
            out.push(')');
        }
    }
}

pub fn sem_id(e: &Node) -> SemId {
    let mut s = String::new();
    canon(e, &mut Vec::new(), &mut s);
    hash_str(&s)
}

/// 为程序中每个表达式节点预先计算语义身份。
pub fn sem_map(prog: &Program) -> HashMap<crate::syntax::ExprId, SemId> {
    let mut m = HashMap::new();
    fn go(e: &Node, m: &mut HashMap<crate::syntax::ExprId, SemId>) {
        m.insert(e.id, sem_id(e));
        match &e.kind {
            Expr::Int(_) | Expr::Bool(_) | Expr::Var(_) => {}
            Expr::Fun { body, .. } => go(body, m),
            Expr::App(a, b) => {
                go(a, m);
                go(b, m);
            }
            Expr::Let { rhs, body, .. } => {
                go(rhs, m);
                go(body, m);
            }
            Expr::If(a, b, c) => {
                go(a, m);
                go(b, m);
                go(c, m);
            }
            Expr::BinOp(_, a, b) => {
                go(a, m);
                go(b, m);
            }
            Expr::Record(fs) => {
                for (_, v) in fs {
                    go(v, m);
                }
            }
            Expr::Field(e, _) => go(e, m),
            Expr::Inj(_, e) => go(e, m),
            Expr::Match(s, cases) => {
                go(s, m);
                for (_, _, b) in cases {
                    go(b, m);
                }
            }
        }
    }
    for l in &prog.lets {
        go(&l.rhs, &mut m);
    }
    if let Some(t) = &prog.tail {
        go(t, &mut m);
    }
    m
}
