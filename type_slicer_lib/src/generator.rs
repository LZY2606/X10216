//! Constraint generation. Traverses the identity-stable AST and emits ordered
//! actions (introduce metas, instantiate/generalize schemes, unify, primitives).

use crate::model::*;
use crate::syntax::*;
use crate::types::TCon;
use std::collections::HashMap;

const FNV_OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
const FNV_PRIME: u64 = 0x0000_0100_0000_01b3;

fn mix(h: &mut u64, v: u64) {
    *h = (*h ^ v).wrapping_mul(FNV_PRIME);
}
fn mix_str(h: &mut u64, s: &str) {
    for b in s.bytes() {
        mix(h, b as u64);
    }
}

#[derive(Clone)]
enum Binding {
    Mono(MetaKey),
    Poly(SchemeKey),
    Builtin(String),
}

struct Gen {
    actions: Vec<Action>,
    counters: HashMap<NodeId, u32>,
    level: u32,
    scope_stack: Vec<NodeId>,
    env: Vec<(String, Binding)>,
    let_chain: Vec<(NodeId, String)>,
    node_types: Vec<(NodeId, MetaKey)>,
}

impl Gen {
    fn new() -> Self {
        Gen {
            actions: Vec::new(),
            counters: HashMap::new(),
            level: 0,
            scope_stack: Vec::new(),
            env: vec![
                ("print".into(), Binding::Builtin("print".into())),
            ],
            let_chain: Vec::new(),
            node_types: Vec::new(),
        }
    }

    fn next_seq(&mut self, node: NodeId) -> u32 {
        let c = self.counters.entry(node).or_insert(0);
        let s = *c;
        *c += 1;
        s
    }

    fn action_id(&self, node: NodeId, seq: u32) -> ActionId {
        ActionId(format!("n{:x}#{}", node.0, seq))
    }

    fn scope_key(&self) -> u64 {
        let mut h = FNV_OFFSET;
        for n in &self.scope_stack {
            mix(&mut h, n.0);
        }
        h
    }

    fn genv_key(&self) -> u64 {
        let mut h = FNV_OFFSET;
        for (n, name) in &self.let_chain {
            mix(&mut h, n.0);
            mix_str(&mut h, name);
        }
        h
    }

    fn emit(&mut self, node: NodeId, span: Span, title: String, kind: ActionKind) {
        let seq = self.next_seq(node);
        self.actions.push(Action {
            id: self.action_id(node, seq),
            node,
            span,
            seq,
            scope_key: self.scope_key(),
            genv_key: self.genv_key(),
            title,
            kind,
        });
    }

    fn intro_at(&mut self, node: NodeId, span: Span, var_kind: &str, label: String) -> MetaKey {
        let seq = self.next_seq(node);
        let key = MetaKey(format!("n{:x}#{}", node.0, seq));
        self.actions.push(Action {
            id: self.action_id(node, seq),
            node,
            span,
            seq,
            scope_key: self.scope_key(),
            genv_key: self.genv_key(),
            title: format!("引入变量 {label}"),
            kind: ActionKind::Intro {
                key: key.clone(),
                var_kind: var_kind.into(),
                level: self.level,
                label,
            },
        });
        key
    }

    fn intro(&mut self, e: &NExpr, var_kind: &str, label: String) -> MetaKey {
        self.intro_at(e.id, e.span, var_kind, label)
    }

    fn unify(&mut self, e: &NExpr, left: STerm, right: STerm, relation: &str) {
        self.emit(
            e.id,
            e.span,
            format!("统一：{relation}"),
            ActionKind::Unify { left, right, relation: relation.into() },
        );
    }

    fn lookup(&self, name: &str) -> Option<Binding> {
        for (n, b) in self.env.iter().rev() {
            if n == name {
                return Some(b.clone());
            }
        }
        None
    }

    fn use_binding(&mut self, e: &NExpr, name: &str, b: Binding) -> MetaKey {
        let m = self.intro(e, "star", format!("{name} 的使用类型"));
        match b {
            Binding::Mono(k) => {
                self.unify(e, STerm::v(m.0.clone()), STerm::v(k.0), "变量共享同一类型（非泛化）");
                m
            }
            Binding::Poly(sk) => {
                self.emit(
                    e.id,
                    e.span,
                    format!("实例化多态绑定 {name}"),
                    ActionKind::Instantiate { scheme: SchemeRef::Local(sk), result: m.clone() },
                );
                m
            }
            Binding::Builtin(bi) => {
                self.emit(
                    e.id,
                    e.span,
                    format!("实例化内建多态值 {name}"),
                    ActionKind::Instantiate { scheme: SchemeRef::Builtin(bi), result: m.clone() },
                );
                m
            }
        }
    }
}

fn is_value(e: &NExpr) -> bool {
    match &e.inner {
        Expr::Unit | Expr::Int(_) | Expr::Bool(_) | Expr::Str(_) => true,
        Expr::Lam { .. } => true,
        Expr::Tag { payload, .. } => payload.as_ref().map_or(true, |p| is_value(p)),
        Expr::Record { fields, rest } => {
            fields.iter().all(|(_, f)| is_value(f)) && rest.as_ref().map_or(true, |r| is_value(r))
        }
        _ => false,
    }
}

impl Gen {
    fn gen_expr(&mut self, e: &NExpr) -> MetaKey {
        let m = self.gen_inner(e);
        self.node_types.push((e.id, m.clone()));
        m
    }

    fn gen_inner(&mut self, e: &NExpr) -> MetaKey {
        match &e.inner {
            Expr::Unit => {
                let m = self.intro(e, "star", "unit 字面量类型".into());
                self.unify(e, STerm::v(m.0.clone()), STerm::con(TCon::Unit), "字面量 () : unit");
                m
            }
            Expr::Int(_) => {
                let m = self.intro(e, "star", "整数字面量类型".into());
                self.unify(e, STerm::v(m.0.clone()), STerm::con(TCon::Int), "整数字面量 : int");
                m
            }
            Expr::Bool(_) => {
                let m = self.intro(e, "star", "布尔字面量类型".into());
                self.unify(e, STerm::v(m.0.clone()), STerm::con(TCon::Bool), "布尔字面量 : bool");
                m
            }
            Expr::Str(_) => {
                let m = self.intro(e, "star", "字符串字面量类型".into());
                self.unify(e, STerm::v(m.0.clone()), STerm::con(TCon::String), "字符串字面量 : string");
                m
            }
            Expr::Var(name) => match self.lookup(name) {
                Some(b) => self.use_binding(e, name, b),
                None => {
                    let m = self.intro(e, "star", format!("未绑定变量 {name} 的类型"));
                    self.emit(
                        e.id,
                        e.span,
                        format!("错误：变量 {name} 未定义"),
                        ActionKind::Error { message: format!("变量 {name} 不在作用域中") },
                    );
                    m
                }
            },
            Expr::Lam { param, body } => {
                let p = self.intro(e, "star", format!("参数 {param} 的类型"));
                let r = self.intro(e, "star", "函数结果类型".into());
                self.scope_stack.push(e.id);
                self.env.push((param.clone(), Binding::Mono(p.clone())));
                let body_m = self.gen_expr(body);
                self.env.pop();
                self.scope_stack.pop();
                self.unify(e, STerm::v(body_m.0.clone()), STerm::v(r.0.clone()), "函数体决定结果类型");
                let m = self.intro(e, "star", "lambda 表达式类型".into());
                self.unify(
                    e,
                    STerm::v(m.0.clone()),
                    STerm::arrow(STerm::v(p.0), STerm::v(r.0)),
                    "lambda 是函数类型",
                );
                m
            }
            Expr::App { fun, arg } => {
                let f = self.gen_expr(fun);
                let a = self.gen_expr(arg);
                let r = self.intro(e, "star", "应用结果类型".into());
                self.unify(
                    e,
                    STerm::v(f.0),
                    STerm::arrow(STerm::v(a.0), STerm::v(r.0.clone())),
                    "被应用者必须是函数",
                );
                r
            }
            Expr::Let { rec, name, rhs, body } => self.gen_let(e, *rec, name, rhs, body),
            Expr::If { cond, then_, else_ } => {
                let c = self.gen_expr(cond);
                self.unify(e, STerm::v(c.0), STerm::con(TCon::Bool), "if 条件必须是 bool");
                let t = self.gen_expr(then_);
                let el = self.gen_expr(else_);
                self.unify(e, STerm::v(t.0.clone()), STerm::v(el.0), "两个分支类型一致");
                let m = self.intro(e, "star", "if 表达式类型".into());
                self.unify(e, STerm::v(m.0.clone()), STerm::v(t.0), "if 结果取分支类型");
                m
            }
            Expr::Record { fields, rest } => {
                let mut sf: Vec<(String, STerm)> = Vec::new();
                for (label, fe) in fields {
                    let fm = self.gen_expr(fe);
                    sf.push((label.clone(), STerm::v(fm.0)));
                }
                let row = if let Some(r) = rest {
                    let rm = self.gen_expr(r);
                    SRow::open(sf, STerm::v(rm.0))
                } else {
                    SRow::closed(sf)
                };
                let m = self.intro(e, "star", "记录类型".into());
                self.unify(e, STerm::v(m.0.clone()), STerm::Rec(row), "记录字面量构造记录类型");
                m
            }
            Expr::Proj { obj, field } => {
                let o = self.gen_expr(obj);
                let m = self.intro(e, "star", format!("字段 {field} 的类型"));
                let rho = self.intro(e, "row", "记录其余字段（行尾）".into());
                self.unify(
                    e,
                    STerm::v(o.0),
                    STerm::Rec(SRow::open(vec![(field.clone(), STerm::v(m.0.clone()))], STerm::v(rho.0))),
                    &format!("投影 .{field} 要求该字段存在"),
                );
                m
            }
            Expr::Tag { label, payload } => {
                let pt = match payload {
                    Some(pe) => STerm::v(self.gen_expr(pe).0),
                    None => STerm::con(TCon::Unit),
                };
                let m = self.intro(e, "star", "变体值类型".into());
                let rho = self.intro(e, "row", "变体其余标签（行尾）".into());
                self.unify(
                    e,
                    STerm::v(m.0.clone()),
                    STerm::Variant(SRow::open(vec![(label.clone(), pt)], STerm::v(rho.0))),
                    &format!("标签 `{label} 构造变体"),
                );
                m
            }
            Expr::Match { scrut, arms } => {
                let s = self.gen_expr(scrut);
                let result = self.intro(e, "star", "match 结果类型".into());
                let tail = self.intro(e, "row", "未被分支覆盖的变体标签".into());
                let mut specs = Vec::new();
                for arm in arms {
                    self.scope_stack.push(arm.id);
                    let pm = if let Pat::Tag(_, Some(binder)) = &arm.inner.pat.inner {
                        if binder.is_empty() {
                            None
                        } else {
                            let pm = self.intro_at(
                                arm.id,
                                arm.span,
                                "star",
                                format!("绑定 {binder} 的类型"),
                            );
                            self.env.push((binder.clone(), Binding::Mono(pm.clone())));
                            Some(pm)
                        }
                    } else {
                        None
                    };
                    let body_m = self.gen_expr(&arm.inner.body);
                    if pm.is_some() {
                        self.env.pop();
                    }
                    self.scope_stack.pop();
                    if let Pat::Tag(label, _) = &arm.inner.pat.inner {
                        specs.push(MatchArmSpec {
                            arm_node: arm.id,
                            label: label.clone(),
                            payload: pm,
                            body: body_m,
                        });
                    }
                }
                self.emit(
                    e.id,
                    e.span,
                    "match：统一判别式、各分支与结果".into(),
                    ActionKind::Match { scrut: s, result: result.clone(), arms: specs, tail },
                );
                result
            }
            Expr::Bin { op, left, right } => {
                let l = self.gen_expr(left);
                let r = self.gen_expr(right);
                let m = self.intro(e, "star", "二元运算结果类型".into());
                let (result_con, rel) = match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul => ("int", "算术运算：int -> int -> int"),
                    BinOp::Eq | BinOp::Lt => ("bool", "比较运算：int -> int -> bool"),
                };
                self.emit(
                    e.id,
                    e.span,
                    format!("内建运算 {}", op.symbol()),
                    ActionKind::Prim {
                        op: op.symbol().into(),
                        operands: vec![l, r],
                        result: m.clone(),
                        result_con: result_con.into(),
                    },
                );
                let _ = rel;
                m
            }
        }
    }

    fn gen_let(
        &mut self,
        e: &NExpr,
        rec: bool,
        name: &str,
        rhs: &NExpr,
        body: &NExpr,
    ) -> MetaKey {
        let old_level = self.level;
        let new_level = self.level + 1;
        self.level = new_level;
        self.emit(
            e.id,
            e.span,
            format!("进入 let {name} 的泛化层"),
            ActionKind::EnterScope { level: new_level },
        );
        self.scope_stack.push(e.id);
        self.let_chain.push((e.id, name.to_string()));
        let scheme = SchemeKey(format!("s{:x}", e.id.0));

        let rhs_m = if rec {
            let rec_m = self.intro(e, "star", format!("递归绑定 {name} 的自引用类型"));
            self.env.push((name.to_string(), Binding::Mono(rec_m.clone())));
            let rm = self.gen_expr(rhs);
            self.env.pop();
            self.unify(e, STerm::v(rec_m.0.clone()), STerm::v(rm.0.clone()), "递归自引用与右值一致");
            rm
        } else {
            self.gen_expr(rhs)
        };

        let value = is_value(rhs);
        self.emit(
            e.id,
            e.span,
            if value {
                format!("泛化 {name}（语法值，可多态）")
            } else {
                format!("{name} 受 value restriction 限制（不泛化）")
            },
            ActionKind::Generalize {
                scheme: scheme.clone(),
                result: rhs_m.clone(),
                at_level: new_level,
                is_value: value,
                role: name.to_string(),
            },
        );
        self.emit(e.id, e.span, "离开泛化层".into(), ActionKind::ExitScope);
        self.level = old_level;

        self.env.push((name.to_string(), Binding::Poly(scheme)));
        let body_m = self.gen_expr(body);
        self.env.pop();
        self.let_chain.pop();
        self.scope_stack.pop();

        let m = self.intro(e, "star", "let 表达式类型".into());
        self.unify(e, STerm::v(m.0.clone()), STerm::v(body_m.0), "let 结果取函数体类型");
        m
    }
}

pub fn generate(root: &NExpr) -> ConstraintProgram {
    let mut g = Gen::new();
    let m = g.gen_expr(root);
    ConstraintProgram {
        root: root.id,
        root_meta: m,
        node_types: g.node_types,
        actions: g.actions,
    }
}
