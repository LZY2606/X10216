//! 从 AST 生成带来源的类型约束（不做求解）。

use crate::constraint::{Cid, CInfo};
use crate::syntax::ast::*;
use crate::types::{ConstraintKind, Row, RowTail, Scheme, TypeTerm};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Binding {
    /// 词法变量：直接是某个未约束类型变量（参数、rec 自身）。
    Mono(u32),
    /// let 泛化绑定：方案在 Gen 处生成，这里记录其 value 节点与 root。
    Poly { value_node: NodeId, root: u32 },
}

#[derive(Clone, Debug)]
struct Frame {
    names: HashMap<String, Binding>,
}

pub struct Generator<'a> {
    src: &'a str,
    pub constraints: Vec<CInfo>,
    /// 所有新鲜变量的“出生约束”。
    pub origins: HashMap<u32, Cid>,
    /// 节点结果类型（用于图与最终类型展示）。
    pub node_ty: Vec<(NodeId, u32)>,
    frames: Vec<Frame>,
    /// 每个 Gen 节点给可见绑定方案计算的确定性摘要（key 用）。
    scope_sig: Vec<String>,
    next_var: u32,
    depth: u32,
    cidx: usize,
}

impl<'a> Generator<'a> {
    pub fn new(src: &'a str) -> Self {
        Generator {
            src,
            constraints: Vec::new(),
            origins: HashMap::new(),
            node_ty: Vec::new(),
            frames: vec![Frame {
                names: HashMap::new(),
            }],
            scope_sig: Vec::new(),
            next_var: 0,
            depth: 0,
            cidx: 0,
        }
    }

    fn fresh(&mut self, birth: Option<&Cid>) -> u32 {
        let v = self.next_var;
        self.next_var += 1;
        if let Some(c) = birth {
            self.origins.insert(v, c.clone());
        }
        v
    }

    fn push_scope(&mut self) {
        self.frames.push(Frame {
            names: HashMap::new(),
        });
    }
    fn pop_scope(&mut self) {
        self.frames.pop();
    }
    fn bind(&mut self, name: String, b: Binding) {
        self.frames.last_mut().unwrap().names.insert(name, b);
    }
    fn lookup(&self, name: &str) -> Option<Binding> {
        for f in self.frames.iter().rev() {
            if let Some(b) = f.names.get(name) {
                return Some(b.clone());
            }
        }
        None
    }

    fn span_text(&self, s: Span) -> &'a str {
        &self.src[s.start as usize..s.end as usize]
    }

    fn cid_of(&self, nid: NodeId, sub: &str) -> Cid {
        format!("n{:x}.{}", nid, sub)
    }

    /// key = cid + 词法作用域路径 + 可见泛化环境摘要。
    fn key_of(&self, cid: &str) -> String {
        let scope = self
            .scope_sig
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("/");
        format!("{}|{}", cid, scope)
    }

    fn emit(
        &mut self,
        nid: NodeId,
        sub: &str,
        span: Span,
        kind: ConstraintKind,
        desc: String,
        cons: crate::constraint::Constraint,
    ) -> Cid {
        let cid = self.cid_of(nid, sub);
        let key = self.key_of(&cid);
        self.constraints.push(CInfo {
            cid: cid.clone(),
            key,
            cons,
            kind,
            span,
            node: nid,
            desc,
        });
        cid
    }

    fn con(
        &mut self,
        nid: NodeId,
        sub: &str,
        span: Span,
        desc: String,
        target: u32,
        mut build: impl FnMut(&mut TermBuilder) -> TypeTerm,
    ) {
        let cid = self.cid_of(nid, sub);
        let mut tb = TermBuilder {
            next: self.next_var,
            births: Vec::new(),
        };
        let term = build(&mut tb);
        self.next_var = tb.next;
        for (v, sub2) in tb.births.drain(..) {
            self.origins.insert(v, format!("{}.{}", cid, sub2));
        }
        let key = self.key_of(&cid);
        self.constraints.push(CInfo {
            cid: cid.clone(),
            key,
            cons: crate::constraint::Constraint::Con { term, target },
            kind: ConstraintKind::Con,
            span,
            node: nid,
            desc,
        });
    }

    fn unify(&mut self, nid: NodeId, sub: &str, span: Span, desc: String, a: u32, b: u32) {
        self.emit(
            nid,
            sub,
            span,
            ConstraintKind::Unify,
            desc,
            crate::constraint::Constraint::Unify { left: a, right: b },
        );
    }
}

/// 在一条 Con 约束内创建其内部类型变量（出生证明指向该 Con）。
pub struct TermBuilder {
    next: u32,
    births: Vec<(u32, String)>,
}

impl TermBuilder {
    pub fn var(&mut self, sub: &str) -> TypeTerm {
        let v = self.next;
        self.next += 1;
        self.births.push((v, sub.to_string()));
        TypeTerm::TVar(v)
    }
    pub fn row_var(&mut self, sub: &str) -> u32 {
        let v = self.next;
        self.next += 1;
        self.births.push((v, sub.to_string()));
        v
    }
    pub fn closed(fields: Vec<(String, TypeTerm)>) -> Row {
        Row::closed(fields)
    }
    pub fn open(fields: Vec<(String, TypeTerm)>, tail: u32) -> Row {
        Row::open(fields, tail)
    }
}

impl<'a> Generator<'a> {
    pub fn generate_program(mut self, prog: &Program) -> Generated {
        for (i, item) in prog.items.iter().enumerate() {
            let _ = i;
            self.gen_top(item);
        }
        Generated {
            constraints: self.constraints,
            origins: self.origins,
            node_ty: self.node_ty,
        }
    }

    fn gen_top(&mut self, item: &Item) {
        let root = self.fresh(None);
        let name = item.name.clone();
        let bound_ty = self.gen_expr(&item.expr, Some(root));
        // 顶层非递归绑定：与 bound 统一后在全局深度 0 泛化。
        if !item.rec {
            self.unify(
                item.expr.nid,
        "top",
                item.expr.span,
                format!("顶层绑定 {} 的结果类型", name),
                root,
                bound_ty,
            );
            let non_exp = item.expr.node.is_value();
            self.emit(
                item.expr.nid,
                "gen",
                item.expr.span,
                ConstraintKind::Gen,
                format!(
                    "{} 顶层绑定 {} 的类型{}",
                    if non_exp { "泛化" } else { "弱泛化(value restriction)" },
                    name,
                    if non_exp { "" } else { "（非值，不泛化自由变量）" }
                ),
                crate::constraint::Constraint::Gen {
                    value_node: item.expr.nid,
                    root,
                    depth: 0,
                    non_expansive: non_exp,
                    rec: false,
                    rec_vars: vec![],
                    name,
                },
            );
            self.bind(name, Binding::Poly { value_node: item.expr.nid, root });
        } else {
            // rec：root 直接作为递归名的单态变量在作用域中。
            self.unify(
                item.expr.nid,
                "toprec",
                item.expr.span,
                format!("递归绑定 {} 的结果类型", name),
                root,
                bound_ty,
            );
            let non_exp = item.expr.node.is_value();
            self.emit(
                item.expr.nid,
                "gen",
                item.expr.span,
                ConstraintKind::Gen,
                format!(
                    "{} 递归顶层绑定 {}（递归绑定不泛化）",
                    if non_exp { "泛化" } else { "弱泛化(value restriction)" },
                    name
                ),
                crate::constraint::Constraint::Gen {
                    value_node: item.expr.nid,
                    root,
                    depth: 0,
                    non_expansive: non_exp,
                    rec: true,
                    rec_vars: vec![root],
                    name,
                },
            );
            self.bind(name, Binding::Mono(root));
        }
    }

    /// 返回表达式的结果类型变量；`forced` 用于让绑定表达式直接写到给定 root。
    fn gen_expr(&mut self, e: &SExpr, forced: Option<u32>) -> u32 {
        let nid = e.nid;
        let span = e.span;
        let result = forced.unwrap_or_else(|| {
            // 节点结果变量的“出生证明”来自该节点的第一条 Con/Inst；这里先不登记。
            self.fresh(None)
        });
        if forced.is_none() {
            self.node_ty.push((nid, result));
        } else {
            self.node_ty.push((nid, result));
        }
        match &e.node {
            Expr::Int(_) => {
                self.con(nid, "con", span, format!("整数 {}", self.span_text(span)), result, |_| {
                    TypeTerm::TCon("int".into())
                });
            }
            Expr::Bool(_) => {
                self.con(nid, "con", span, "布尔字面量".into(), result, |_| {
                    TypeTerm::TCon("bool".into())
                });
            }
            Expr::Unit => {
                self.con(nid, "con", span, "单位值 ()".into(), result, |_| {
                    TypeTerm::TCon("unit".into())
                });
            }
            Expr::Ident(name) => self.gen_ident(nid, span, name, result),
            Expr::Binop { op, lhs, rhs } => self.gen_binop(nid, span, *op, lhs, rhs, result),
            Expr::If {
                cond,
                then_e,
                else_e,
            } => {
                let ct = self.gen_expr(cond, None);
                self.con(nid, "bool", cond.span, "if 条件为 bool".into(), ct, |_| {
                    TypeTerm::TCon("bool".into())
                });
                let tt = self.gen_expr(then_e, None);
                let et = self.gen_expr(else_e, None);
                self.unify(nid, "then", then_e.span, "then 分支与结果类型一致".into(), result, tt);
                self.unify(nid, "else", else_e.span, "else 分支与结果类型一致".into(), result, et);
            }
            Expr::Fn { param, body } => {
                let pt = self.fresh(None);
                self.push_scope();
                self.bind(param.clone(), Binding::Mono(pt));
                let bt = self.gen_expr(body, None);
                self.pop_scope();
                self.con(
                    nid,
                    "con",
                    span,
                    format!("函数 {} -> ...", param),
                    result,
                    move |_| TypeTerm::fun(TypeTerm::TVar(pt), TypeTerm::TVar(bt)),
                );
            }
            Expr::App { fun, arg } => {
                let ft = self.gen_expr(fun, None);
                let at = self.gen_expr(arg, None);
                self.con(
                    nid,
                    "con",
                    span,
                    format!("应用 {} 于参数", self.span_text(fun.span)),
                    result,
                    move |tb| TypeTerm::fun(TypeTerm::TVar(at), tb.var("ret")),
                );
                self.unify(nid, "app", span, "函数类型与被应用函数一致".into(), ft, result);
            }
            Expr::Let {
                rec,
                name,
                bound,
                body,
            } => self.gen_let(nid, span, *rec, name, bound, body, result),
            Expr::Record { fields } => {
                // 解析器只对空记录产生该节点。
                debug_assert!(fields.is_empty());
                self.con(nid, "con", span, "空记录 { }".into(), result, |_| {
                    TypeTerm::record(Row::empty_closed())
                });
            }
            Expr::Extend {
                field,
                value,
                rest,
            } => self.gen_extend(nid, span, field, value, rest, result),
            Expr::Project { record, field } => {
                let rt = self.gen_expr(record, None);
                self.con(
                    nid,
                    "con",
                    span,
                    format!("投影 .{} 的记录类型", field),
                    rt,
                    move |tb| {
                        let tail = tb.row_var("tail");
                        TypeTerm::record(tb.open(
                            vec![(field.clone(), TypeTerm::TVar(result))],
                            tail,
                        ))
                    },
                );
            }
            Expr::Variant { tag, payload } => {
                let pt = self.gen_expr(payload, None);
                self.con(
                    nid,
                    "con",
                    span,
                    format!("变体构造 {}（载荷）", tag),
                    result,
                    move |tb| {
                        let tail = tb.row_var("tail");
                        TypeTerm::variant(tb.open(vec![(tag.clone(), TypeTerm::TVar(pt))], tail))
                    },
                );
            }
            Expr::Match {
                scrut,
                clause_tag,
                clause_pat,
                clause_body,
                default,
            } => self.gen_match(
                nid,
                span,
                scrut,
                clause_tag,
                clause_pat,
                clause_body,
                default,
                result,
            ),
            Expr::Paren(inner) => {
                let it = self.gen_expr(inner, None);
                self.unify(nid, "paren", span, "括号内表达式类型".into(), result, it);
            }
        }
        result
    }

    fn gen_ident(&mut self, nid: NodeId, span: Span, name: &str, result: u32) {
        match self.lookup(name) {
            None => {
                self.emit(
                    nid,
                    "err",
                    span,
                    ConstraintKind::Error,
                    format!("未绑定的标识符 '{}'", name),
                    crate::constraint::Constraint::Error {
                        message: format!("未绑定的标识符 '{}'", name),
                    },
                );
            }
            Some(Binding::Mono(v)) => {
                self.unify(
                    nid,
                    "ref",
                    span,
                    format!("引用参数/递归绑定 {}", name),
                    result,
                    v,
                );
            }
            Some(Binding::Poly { value_node, root }) => {
                // 方案在求解时才知道，这里先用占位方案（body=root，空量化）；
                // 真正的方案由对应 Gen 步骤写入共享表，Inst 求解时按 value_node 取。
                let cid = self.emit(
                    nid,
                    "inst",
                    span,
                    ConstraintKind::Inst,
                    format!("使用多态绑定 {}（实例化其方案）", name),
                    crate::constraint::Constraint::Inst {
                        scheme: Scheme {
                            vars: vec![],
                            row_vars: vec![],
                            body: TypeTerm::TVar(root),
                            weak: vec![],
                        },
                        target: result,
                        node: value_node,
                    },
                );
                let _ = cid;
            }
        }
    }

    fn gen_binop(
        &mut self,
        nid: NodeId,
        span: Span,
        op: char,
        lhs: &SExpr,
        rhs: &SExpr,
        result: u32,
    ) {
        let lt = self.gen_expr(lhs, None);
        let rt = self.gen_expr(rhs, None);
        let (lcon, rcon, rescon) = match op {
            '=' | '<' => ("int", "int", "bool"),
            _ => ("int", "int", "int"),
        };
        self.con(nid, "lop", lhs.span, format!("{} 左操作数为 int", op), lt, move |_| {
            TypeTerm::TCon(lcon.into())
        });
        self.con(nid, "rop", rhs.span, format!("{} 右操作数为 int", op), rt, move |_| {
            TypeTerm::TCon(rcon.into())
        });
        self.con(nid, "res", span, format!("{} 的结果类型", op), result, move |_| {
            TypeTerm::TCon(rescon.into())
        });
    }

    fn gen_let(
        &mut self,
        nid: NodeId,
        span: Span,
        rec: bool,
        name: &str,
        bound: &SExpr,
        body: &SExpr,
        result: u32,
    ) {
        self.depth += 1;
        if rec {
            // 递归：先在新作用域放一个单态变量。
            let self_v = self.fresh(None);
            let bt = self.gen_expr(bound, Some(self_v));
            let _ = bt;
            self.push_scope();
            self.bind(name.to_string(), Binding::Mono(self_v));
            self.scope_sig.push(format!("letrec:n{:x}", nid));
            let rt = self.gen_expr(body, None);
            self.scope_sig.pop();
            self.pop_scope();
            self.unify(
                nid,
                "letres",
                body.span,
                format!("let rec {} in ... 的结果", name),
                result,
                rt,
            );
            let non_exp = bound.node.is_value();
            self.emit(
                nid,
                "gen",
                bound.span,
                ConstraintKind::Gen,
                format!(
                    "递归绑定 {}（递归名不参与泛化）",
                    name
                ),
                crate::constraint::Constraint::Gen {
                    value_node: bound.nid,
                    root: self_v,
                    depth: self.depth,
                    non_expansive: non_exp,
                    rec: true,
                    rec_vars: vec![self_v],
                    name: name.to_string(),
                },
            );
        } else {
            let root = self.fresh(None);
            let bt = self.gen_expr(bound, Some(root));
            let _ = bt;
            let non_exp = bound.node.is_value();
            self.emit(
                nid,
                "gen",
                bound.span,
                ConstraintKind::Gen,
                format!(
                    "{}let 绑定 {}（{}）",
                    "",
                    name,
                    if non_exp {
                        "非膨胀值，自由变量可泛化"
                    } else {
                        "膨胀表达式，应用 value restriction"
                    }
                ),
                crate::constraint::Constraint::Gen {
                    value_node: bound.nid,
                    root,
                    depth: self.depth,
                    non_expansive: non_exp,
                    rec: false,
                    rec_vars: vec![],
                    name: name.to_string(),
                },
            );
            self.push_scope();
            self.bind(
                name.to_string(),
                Binding::Poly {
                    value_node: bound.nid,
                    root,
                },
            );
            self.scope_sig
                .push(format!("let:n{:x}:{}", nid, genv_hint(non_exp)));
            let rt = self.gen_expr(body, None);
            self.scope_sig.pop();
            self.pop_scope();
            self.unify(
                nid,
                "letres",
                body.span,
                format!("let {} in ... 的结果", name),
                result,
                rt,
            );
        }
        self.depth -= 1;
        let _ = span;
    }

    fn gen_extend(
        &mut self,
        nid: NodeId,
        span: Span,
        field: &str,
        value: &SExpr,
        rest: &SExpr,
        result: u32,
    ) {
        let vt = self.gen_expr(value, None);
        let rt = self.gen_expr(rest, None);
        // result 是 { field = vt | rho }，其中 rho 是新行尾行变量，rt 必须是该行。
        self.con(
            nid,
            "con",
            span,
            format!("记录扩展 {{ {} = .. | .. }}", field),
            result,
            move |tb| {
                let tail = tb.row_var("tail");
                TypeTerm::record(tb.open(vec![(field.to_string(), TypeTerm::TVar(vt))], tail))
            },
        );
        // 无法直接约束“rest 是去掉该字段后的行”：由统一算法在行合并时处理；
        // 这里把 rt 与 result 统一即可（result 携带一个独立开放尾）。
        self.unify(
            nid,
            "ext",
            span,
            format!("扩展前记录与 {{ {} = .. | .. }} 的其余字段一致", field),
            result,
            rt,
        );
    }

    #[allow(clippy::too_many_arguments)]
    fn gen_match(
        &mut self,
        nid: NodeId,
        span: Span,
        scrut: &SExpr,
        clause_tag: &str,
        clause_pat: &SPat,
        clause_body: &SExpr,
        default: &SExpr,
        result: u32,
    ) {
        let st = self.gen_expr(scrut, None);
        // 载荷类型
        let pt = self.fresh(None);
        // 分支变量行：开放（可含有其它标签）。
        self.con(
            nid,
            "scrutrow",
            scrut.span,
            format!("被匹配值必须是含 {} 的变体行", clause_tag),
            st,
            move |tb| {
                let tail = tb.row_var("vtail");
                TypeTerm::variant(tb.open(vec![(clause_tag.to_string(), TypeTerm::TVar(pt))], tail))
            },
        );
        self.push_scope();
        match &clause_pat.node {
            Pat::Wild | Pat::Unit => {}
            Pat::Binding(b) => self.bind(b.clone(), Binding::Mono(pt)),
        }
        let cbt = self.gen_expr(clause_body, None);
        self.pop_scope();
        let dt = self.gen_expr(default, None);
        self.unify(
            nid,
            "cbody",
            clause_body.span,
            format!("分支 {} 的结果类型", clause_tag),
            result,
            cbt,
        );
        self.unify(nid, "default", default.span, "默认分支的结果类型".into(), result, dt);
        let _ = span;
    }
}

fn genv_hint(non_exp: bool) -> char {
    if non_exp {
        'g'
    } else {
        'w'
    }
}

/// 生成器产物。
pub struct Generated {
    pub constraints: Vec<CInfo>,
    pub origins: HashMap<u32, Cid>,
    pub node_ty: Vec<(NodeId, u32)>,
}

/// 对外入口。
pub fn generate(src: &str, prog: &Program) -> Generated {
    Generator::new(src).generate_program(prog)
}
