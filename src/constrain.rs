//! 约束生成:遍历 AST,产出带表达式/span 来源的约束序列。
//! 每条约束的稳定 id = hash(语义身份, 表达式内序号),编辑后身份不漂移。
use crate::ident::{hash_str, SemId};
use crate::syntax::{BinOp, Expr, ExprId, Node, Span};
use crate::types::{ConId, Prov, Row, Ty, VarId};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Origin {
    pub expr: Option<ExprId>,
    pub span: Span,
    pub note: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ConstraintKind {
    Eq { lhs: Ty, rhs: Ty },
    /// let 绑定且右值为语法值:泛化后入环境
    Generalize { name: String, ty: Ty },
    /// 单态绑定:lambda 参数 / value restriction / rec 内部
    BindMono { name: String, ty: Ty, why: String },
    Pop,
    /// 使用变量:target = name 的实例
    Instantiate { name: String, target: Ty },
    Unbound { name: String },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Constraint {
    pub id: ConId,
    pub kind: ConstraintKind,
    pub origin: Origin,
    /// 求解后填充的可读文本(缓存随约束一起复用)
    pub text: String,
    pub reused: bool,
}

impl Constraint {
    pub fn is_eq(&self) -> bool {
        matches!(self.kind, ConstraintKind::Eq { .. })
    }
}

pub fn is_value(e: &Node) -> bool {
    match &e.kind {
        Expr::Int(_) | Expr::Bool(_) | Expr::Fun { .. } | Expr::Var(_) => true,
        Expr::Record(fs) => fs.iter().all(|(_, v)| is_value(v)),
        Expr::Inj(_, v) => is_value(v),
        _ => false,
    }
}

pub struct Gen {
    pub next_var: VarId,
    pub constraints: Vec<Constraint>,
    pub expr_types: Vec<(ExprId, Ty)>,
    sem: HashMap<ExprId, SemId>,
    ctr: HashMap<ExprId, u32>,
    /// 作用域:名字 -> 定义处语义身份(用于未绑定检查与环境签名)
    pub scope: Vec<(String, SemId)>,
}

impl Gen {
    pub fn new(sem: HashMap<ExprId, SemId>) -> Self {
        Gen {
            next_var: 0,
            constraints: Vec::new(),
            expr_types: Vec::new(),
            sem,
            ctr: HashMap::new(),
            scope: Vec::new(),
        }
    }

    pub fn fresh(&mut self) -> VarId {
        let v = self.next_var;
        self.next_var += 1;
        v
    }

    fn key_for(&mut self, expr: ExprId) -> ConId {
        let c = self.ctr.entry(expr).or_insert(0);
        let sem = self.sem.get(&expr).copied().unwrap_or(0);
        let k = hash_str(&format!("{}:{}", sem, *c));
        *c += 1;
        k
    }

    fn emit(&mut self, expr: ExprId, span: Span, note: &str, kind: ConstraintKind) -> ConId {
        let id = self.key_for(expr);
        self.constraints.push(Constraint {
            id,
            kind,
            origin: Origin {
                expr: Some(expr),
                span,
                note: note.to_string(),
            },
            text: String::new(),
            reused: false,
        });
        id
    }

    fn eq(&mut self, e: &Node, lhs: Ty, rhs: Ty, note: &str) -> ConId {
        let id = self.key_for(e.id);
        self.constraints.push(Constraint {
            id,
            kind: ConstraintKind::Eq {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
            },
            origin: Origin {
                expr: Some(e.id),
                span: e.span,
                note: note.to_string(),
            },
            text: String::new(),
            reused: false,
        });
        id
    }

    fn record(&mut self, e: &Node, ty: &Ty) {
        self.expr_types.push((e.id, ty.clone()));
    }

    pub fn gen(&mut self, e: &Node) -> Ty {
        let ty = self.gen_inner(e);
        self.record(e, &ty);
        ty
    }

    fn gen_inner(&mut self, e: &Node) -> Ty {
        let p = Prov {
            expr: Some(e.id),
            con: None,
        };
        match &e.kind {
            Expr::Int(_) => Ty::Int(p),
            Expr::Bool(_) => Ty::Bool(p),
            Expr::Var(x) => {
                if self.scope.iter().any(|(n, _)| n == x) {
                    let target = Ty::Var(self.fresh(), p);
                    self.emit(
                        e.id,
                        e.span,
                        &format!("使用变量 {}", x),
                        ConstraintKind::Instantiate {
                            name: x.clone(),
                            target: target.clone(),
                        },
                    );
                    target
                } else {
                    self.emit(
                        e.id,
                        e.span,
                        &format!("未绑定变量 {}", x),
                        ConstraintKind::Unbound { name: x.clone() },
                    );
                    Ty::Var(self.fresh(), p)
                }
            }
            Expr::Fun { param, body } => {
                let a = Ty::Var(self.fresh(), p);
                let sem = self.sem.get(&e.id).copied().unwrap_or(0);
                self.emit(
                    e.id,
                    e.span,
                    &format!("参数 {} 单态绑定", param),
                    ConstraintKind::BindMono {
                        name: param.clone(),
                        ty: a.clone(),
                        why: "lambda 参数".into(),
                    },
                );
                self.scope.push((param.clone(), sem));
                let b = self.gen(body);
                self.scope.pop();
                self.emit(e.id, e.span, "离开参数作用域", ConstraintKind::Pop);
                Ty::Fun(Box::new(a), Box::new(b), p)
            }
            Expr::App(f, a) => {
                let tf = self.gen(f);
                let ta = self.gen(a);
                let r = Ty::Var(self.fresh(), p);
                let cid = self.key_for(e.id);
                let fun = Ty::Fun(
                    Box::new(ta),
                    Box::new(r.clone()),
                    Prov {
                        expr: Some(e.id),
                        con: Some(cid),
                    },
                );
                self.constraints.push(Constraint {
                    id: cid,
                    kind: ConstraintKind::Eq {
                        lhs: tf,
                        rhs: fun,
                    },
                    origin: Origin {
                        expr: Some(e.id),
                        span: e.span,
                        note: "函数应用:函数类型 = 参数 -> 结果".into(),
                    },
                    text: String::new(),
                    reused: false,
                });
                r
            }
            Expr::Let {
                rec_,
                name,
                rhs,
                body,
            } => {
                self.gen_let(e, *rec_, name, rhs);
                let sem = self.sem.get(&rhs.id).copied().unwrap_or(0);
                self.scope.push((name.clone(), sem));
                let b = self.gen(body);
                self.scope.pop();
                self.emit(e.id, e.span, "离开 let 作用域", ConstraintKind::Pop);
                b
            }
            Expr::If(c, t, el) => {
                let tc = self.gen(c);
                self.eq(c, tc, Ty::Bool(p), "if 条件须为 Bool");
                let tt = self.gen(t);
                let te = self.gen(el);
                self.eq(e, tt.clone(), te, "if 两分支类型一致");
                tt
            }
            Expr::BinOp(op, l, r) => {
                let tl = self.gen(l);
                let tr = self.gen(r);
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul => {
                        self.eq(l, tl, Ty::Int(p), "算术运算左操作数须为 Int");
                        self.eq(r, tr, Ty::Int(p), "算术运算右操作数须为 Int");
                        Ty::Int(p)
                    }
                    BinOp::Lt => {
                        self.eq(l, tl, Ty::Int(p), "比较左操作数须为 Int");
                        self.eq(r, tr, Ty::Int(p), "比较右操作数须为 Int");
                        Ty::Bool(p)
                    }
                    BinOp::Eq => {
                        self.eq(e, tl, tr, "== 两侧类型一致");
                        Ty::Bool(p)
                    }
                }
            }
            Expr::Record(fs) => {
                let mut fields = BTreeMap::new();
                for (n, v) in fs {
                    fields.insert(n.clone(), self.gen(v));
                }
                Ty::Record(
                    Row {
                        fields,
                        tail: None,
                        prov: p,
                    },
                    p,
                )
            }
            Expr::Field(rec, name) => {
                let trec = self.gen(rec);
                let a = Ty::Var(self.fresh(), p);
                let rho = self.fresh();
                let cid = self.key_for(e.id);
                let want = Ty::Record(
                    Row {
                        fields: BTreeMap::from([(name.clone(), a.clone())]),
                        tail: Some(rho),
                        prov: Prov {
                            expr: Some(e.id),
                            con: Some(cid),
                        },
                    },
                    Prov {
                        expr: Some(e.id),
                        con: Some(cid),
                    },
                );
                self.constraints.push(Constraint {
                    id: cid,
                    kind: ConstraintKind::Eq {
                        lhs: trec,
                        rhs: want,
                    },
                    origin: Origin {
                        expr: Some(e.id),
                        span: e.span,
                        note: format!("字段访问 .{} :记录须含该字段", name),
                    },
                    text: String::new(),
                    reused: false,
                });
                a
            }
            Expr::Inj(tag, payload) => {
                let tp = self.gen(payload);
                let rho = self.fresh();
                Ty::Variant(
                    Row {
                        fields: BTreeMap::from([(tag.clone(), tp)]),
                        tail: Some(rho),
                        prov: p,
                    },
                    p,
                )
            }
            Expr::Match(scrut, cases) => {
                let ts = self.gen(scrut);
                let result = Ty::Var(self.fresh(), p);
                let rho = self.fresh();
                let mut fields = BTreeMap::new();
                let mut arms = Vec::new();
                for (tag, var, _) in cases {
                    let a = Ty::Var(self.fresh(), p);
                    fields.insert(tag.clone(), a.clone());
                    arms.push((var.clone(), a));
                }
                let cid = self.key_for(e.id);
                let want = Ty::Variant(
                    Row {
                        fields,
                        tail: Some(rho),
                        prov: Prov {
                            expr: Some(e.id),
                            con: Some(cid),
                        },
                    },
                    Prov {
                        expr: Some(e.id),
                        con: Some(cid),
                    },
                );
                self.constraints.push(Constraint {
                    id: cid,
                    kind: ConstraintKind::Eq {
                        lhs: ts,
                        rhs: want,
                    },
                    origin: Origin {
                        expr: Some(e.id),
                        span: e.span,
                        note: "match:被匹配值须为含这些分支的 variant".into(),
                    },
                    text: String::new(),
                    reused: false,
                });
                for ((_, _, body), (var, a)) in cases.iter().zip(arms) {
                    let sem = self.sem.get(&body.id).copied().unwrap_or(0);
                    self.emit(
                        body.id,
                        body.span,
                        &format!("分支变量 {} 单态绑定", var),
                        ConstraintKind::BindMono {
                            name: var.clone(),
                            ty: a,
                            why: "match 分支变量".into(),
                        },
                    );
                    self.scope.push((var.clone(), sem));
                    let tb = self.gen(body);
                    self.scope.pop();
                    self.emit(body.id, body.span, "离开分支作用域", ConstraintKind::Pop);
                    self.eq(body, tb, result.clone(), "match 各分支结果类型一致");
                }
                result
            }
        }
    }

    /// let / let rec 的约束序列(不弹出作用域,由调用者处理顶层环境)。
    pub fn gen_let(&mut self, e: &Node, rec_: bool, name: &str, rhs: &Node) {
        if rec_ {
            let v = Ty::Var(
                self.fresh(),
                Prov {
                    expr: Some(rhs.id),
                    con: None,
                },
            );
            self.emit(
                rhs.id,
                rhs.span,
                &format!("rec 内部单态绑定 {}", name),
                ConstraintKind::BindMono {
                    name: name.to_string(),
                    ty: v.clone(),
                    why: "let rec 内部".into(),
                },
            );
            let sem = self.sem.get(&rhs.id).copied().unwrap_or(0);
            self.scope.push((name.to_string(), sem));
            let t_rhs = self.gen(rhs);
            self.scope.pop();
            self.eq(rhs, v.clone(), t_rhs, "rec 绑定:名称类型 = 右值类型");
            if is_value(rhs) {
                self.emit(rhs.id, rhs.span, "rec 内部作用域结束", ConstraintKind::Pop);
                self.emit(
                    rhs.id,
                    rhs.span,
                    &format!("泛化绑定 {}", name),
                    ConstraintKind::Generalize {
                        name: name.to_string(),
                        ty: v,
                    },
                );
            }
        } else {
            let t_rhs = self.gen(rhs);
            if is_value(rhs) {
                self.emit(
                    rhs.id,
                    rhs.span,
                    &format!("泛化绑定 {}(右值为语法值)", name),
                    ConstraintKind::Generalize {
                        name: name.to_string(),
                        ty: t_rhs,
                    },
                );
            } else {
                self.emit(
                    rhs.id,
                    rhs.span,
                    &format!("值限制:{} 右值不是语法值,不泛化", name),
                    ConstraintKind::BindMono {
                        name: name.to_string(),
                        ty: t_rhs,
                        why: "value restriction".into(),
                    },
                );
            }
        }
    }
}
