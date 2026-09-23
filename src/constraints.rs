// 约束生成：AST -> 有序事件流（声明变量 / 实例化 / 泛化 / 统一）。
//
// 设计要点：
// - 每个事件拥有稳定 key：由节点身份 + 子结构角色 + 作用域链 + 泛化环境摘要组成。
// - 统一变量名 = "n<节点>_<角色>"，变量改名不会改变身份。
// - 统一约束带方向（left/right），使 row 的“缺字段/多字段”可报告。
use crate::syntax::{Expr, Pat};
use crate::types::{RowFlavor, RowTail, RowType, Type, VarOrigin};
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Event {
    /// 声明一个推导变量
    Declare {
        key: String,
        var: String,
        nid: u32,
        origin: VarOrigin,
        purpose: String,
        level: u32,
    },
    /// 把已声明变量绑定到一个具体结构（在 solver 中作为该变量根的描述项）
    Define {
        key: String,
        var: String,
        term: Type,
        nid: u32,
    },
    /// 实例化一个多态方案为新变量
    Instantiate {
        key: String,
        scheme_var: String,
        copies: Vec<(String, Type)>,
        result: Type,
        nid: u32,
        gen_env: String,
    },
    /// 泛化（value restriction 通过 generalizable 列表体现：
    /// 非值时该列表为空并附带原因）
    Generalize {
        key: String,
        target: String,
        vars: Vec<String>,
        nid: u32,
        is_value: bool,
        reason: String,
        scope: String,
    },
    /// 方向化统一约束
    Equate {
        key: String,
        cid: String,
        left: Type,
        right: Type,
        /// left/right 的来源角色描述
        left_role: String,
        right_role: String,
        /// 触发该约束的表达式节点
        nid: u32,
        scope: String,
        gen_env: String,
        detail: String,
    },
}

impl Event {
    pub fn key(&self) -> &str {
        match self {
            Event::Declare { key, .. }
            | Event::Define { key, .. }
            | Event::Instantiate { key, .. }
            | Event::Generalize { key, .. }
            | Event::Equate { key, .. } => key,
        }
    }
    pub fn nid(&self) -> u32 {
        match self {
            Event::Declare { nid, .. }
            | Event::Define { nid, .. }
            | Event::Instantiate { nid, .. }
            | Event::Generalize { nid, .. }
            | Event::Equate { nid, .. } => *nid,
        }
    }
}

#[derive(Debug, Clone)]
struct EnvEntry {
    name: String,
    binding: Binding,
}

#[derive(Debug, Clone)]
enum Binding {
    Monomorphic { var: String },
    Scheme { quant: Vec<String>, body: Type },
}

#[derive(Debug, Clone)]
struct ScopeFrame {
    nid: u32,
    kind: &'static str,
}

pub struct Generator<'a> {
    expr: &'a Expr,
    events: Vec<Event>,
    counter: u32,
    scope: Vec<ScopeFrame>,
    gen_level: u32,
    /// 泛化环境摘要：记录当前可见的多态绑定 key（有序、去重）
    gen_env: Vec<String>,
}

fn type_key(t: &Type) -> String {
    match t {
        Type::Var(v) => format!("v:{v}"),
        Type::Con { name } => format!("c:{name}"),
        Type::Arrow(a, b) => format!("arr({},{})", type_key(a), type_key(b)),
        Type::Row(r) => {
            let fs: Vec<String> =
                r.fields.iter().map(|(l, ty)| format!("{l}={}", type_key(ty))).collect();
            let tail = match &r.tail {
                RowTail::Empty => "e".into(),
                RowTail::Open { var } => format!("o:{var}"),
            };
            let fl = match r.flavor {
                RowFlavor::Record => "R",
                RowFlavor::Variant => "V",
            };
            format!("{fl}[{};{}]", fs.join(","), tail)
        }
    }
}

impl<'a> Generator<'a> {
    pub fn run(expr: &Expr) -> Vec<Event> {
        let mut g = Generator {
            expr,
            events: Vec::new(),
            counter: 0,
            scope: vec![ScopeFrame { nid: 0, kind: "top" }],
            gen_level: 0,
            gen_env: Vec::new(),
        };
        let top = g.fresh(g.expr.nid(), "top", VarOrigin::Infer);
        let result = g.gen(expr, &Vec::new());
        g.emit_equate(
            g.expr.nid(),
            "program_result",
            Type::var(top.clone()),
            result,
            "程序最终类型",
            "顶层表达式",
            "程序结果约束",
        );
        g.events
    }

    fn unique(&mut self, tag: &str) -> String {
        let c = self.counter;
        self.counter += 1;
        format!("{tag}#{c}")
    }

    fn scope_key(&self) -> String {
        let parts: Vec<String> =
            self.scope.iter().map(|f| format!("{}:{}", f.kind, f.nid)).collect();
        parts.join(">")
    }
    fn gen_env_key(&self) -> String {
        self.gen_env.join(",")
    }

    fn emit(&mut self, e: Event) {
        self.events.push(e);
    }

    fn fresh(&mut self, nid: u32, purpose: impl AsRef<str>, origin: VarOrigin) -> String {
        let purpose = purpose.as_ref();
        let var = format!("n{nid}_{purpose}");
        // 同名变量可能重复（如多个 arm 分支共享某角色），加唯一后缀，
        // 但后缀顺序在给定 AST 下是确定的（生成顺序固定）。
        let var = if self.events.iter().any(|e| match e {
            Event::Declare { var: v, .. } => v == &var,
            _ => false,
        }) {
            let u = self.unique("u");
            format!("n{nid}_{purpose}_{u}")
        } else {
            var
        };
        let scope = self.scope_key();
        let gen_env = self.gen_env_key();
        self.emit(Event::Declare {
            key: format!("decl|{scope}|{gen_env}|{var}"),
            var: var.clone(),
            nid,
            origin,
            purpose: purpose.to_string(),
            level: self.gen_level,
        });
        var
    }

    fn emit_equate(
        &mut self,
        nid: u32,
        role_key: impl AsRef<str>,
        left: Type,
        right: Type,
        left_role: &str,
        right_role: &str,
        detail: &str,
    ) {
        let role_key = role_key.as_ref();
        let scope = self.scope_key();
        let gen_env = self.gen_env_key();
        // 无方向内容指纹（参与“多个同样小核心”判定时保持稳定）
        let mut pair = [type_key(&left), type_key(&right)];
        pair.sort();
        let key =
            format!("eq|{scope}|{gen_env}|n{nid}|{role_key}|{}+{}", pair[0], pair[1]);
        let cid = format!("C{}", self.events.iter().filter(|e| matches!(e, Event::Equate{..})).count());
        self.emit(Event::Equate {
            key,
            cid,
            left,
            right,
            left_role: left_role.to_string(),
            right_role: right_role.to_string(),
            nid,
            scope,
            gen_env,
            detail: detail.to_string(),
        });
    }

    fn push_scope(&mut self, nid: u32, kind: &'static str) {
        self.scope.push(ScopeFrame { nid, kind });
    }
    fn pop_scope(&mut self) {
        self.scope.pop();
    }

    // env: (模式 nid 字符串, Binding)。模式 nid 保证改名后仍稳定。
    fn gen(&mut self, e: &Expr, env: &Vec<EnvEntry>) -> Type {
        match e {
            Expr::Int { nid, .. } => {
                let v = self.fresh(*nid, "lit_int", VarOrigin::Infer);
                self.emit(Event::Define {
                    key: format!("def|{}|{}", self.scope_key(), v),
                    var: v.clone(),
                    term: Type::int(),
                    nid: *nid,
                });
                Type::var(v)
            }
            Expr::Bool { nid, .. } => {
                let v = self.fresh(*nid, "lit_bool", VarOrigin::Infer);
                self.emit(Event::Define {
                    key: format!("def|{}|{}", self.scope_key(), v),
                    var: v.clone(),
                    term: Type::bool(),
                    nid: *nid,
                });
                Type::var(v)
            }
            Expr::Str { nid, .. } => {
                let v = self.fresh(*nid, "lit_str", VarOrigin::Infer);
                self.emit(Event::Define {
                    key: format!("def|{}|{}", self.scope_key(), v),
                    var: v.clone(),
                    term: Type::str(),
                    nid: *nid,
                });
                Type::var(v)
            }
            Expr::Var { name, nid, .. } => self.gen_var(name, *nid, env),
            Expr::Lam { param, body, nid, .. } => {
                let pvar = self.fresh(param.nid(), "param", VarOrigin::Infer);
                let mut inner = env.to_vec();
                inner.push(EnvEntry {
                    name: pat_name(param),
                    binding: Binding::Monomorphic { var: pvar.clone() },
                });
                self.push_scope(*nid, "lam");
                let bt = self.gen(body, &inner);
                self.pop_scope();
                Type::arrow(Type::var(pvar), bt)
            }
            Expr::App { fun, arg, nid, .. } => {
                let ft = self.gen(fun, env);
                let at = self.gen(arg, env);
                let res = self.fresh(*nid, "app_result", VarOrigin::Infer);
                self.emit_equate(
                    *nid,
                    "app_domain",
                    ft,
                    Type::arrow(at, Type::var(res.clone())),
                    "函数位置",
                    "参数 -> 结果",
                    "函数类型必须是箭头类型并与参数匹配",
                );
                Type::var(res)
            }
            Expr::Let { is_rec, name, value, body, nid, .. } => {
                self.gen_let(*is_rec, *nid, name, value, body, env)
            }
            Expr::If { cond, then_e, else_e, nid, .. } => {
                let ct = self.gen(cond, env);
                self.emit_equate(
                    *nid,
                    "if_cond",
                    ct,
                    Type::bool(),
                    "条件",
                    "Bool",
                    "if 的条件必须是布尔类型",
                );
                let tt = self.gen(then_e, env);
                let et = self.gen(else_e, env);
                self.emit_equate(
                    *nid,
                    "if_branches",
                    tt.clone(),
                    et,
                    "then 分支",
                    "else 分支",
                    "两个分支类型必须一致",
                );
                tt
            }
            Expr::Record { fields, nid, .. } => {
                let mut out = Vec::new();
                for (label, fe) in fields {
                    let ft = self.gen(fe, env);
                    out.push((label.clone(), ft));
                }
                Type::record(out, RowTail::Empty)
            }
            Expr::Field { record, label, nid, .. } => {
                let rt = self.gen(record, env);
                let res = self.fresh(*nid, "field_result", VarOrigin::Infer);
                let tail = self.fresh(*nid, "field_tail", VarOrigin::RowTail);
                let row = Type::record(
                    vec![(label.clone(), Type::var(res.clone()))],
                    RowTail::Open { var: tail },
                );
                self.emit_equate(
                    *nid,
                    format!("field_{label}").as_str(),
                    rt,
                    row,
                    "被访问的记录",
                    &format!("含字段 {label} 的行"),
                    "字段访问要求记录包含该字段",
                );
                Type::var(res)
            }
            Expr::Variant { label, payload, nid, .. } => {
                let pt = if let Some(p) = payload {
                    self.gen(p, env)
                } else {
                    Type::unit()
                };
                let tail = self.fresh(*nid, "variant_tail", VarOrigin::RowTail);
                Type::variant(vec![(label.clone(), Some(pt))], RowTail::Open { var: tail })
            }
            Expr::Match { scrutinee, arms, nid, .. } => {
                self.gen_match(*nid, scrutinee, arms, env)
            }
        }
    }
}

impl<'a> Generator<'a> {
    fn builtin_scheme(name: &str) -> Option<Type> {
        let a = Type::var("a");
        Some(match name {
            "add" => Type::arrow(Type::int(), Type::arrow(Type::int(), Type::int())),
            "eq" => Type::arrow(a.clone(), Type::arrow(a, Type::bool())),
            "and" => Type::arrow(Type::bool(), Type::arrow(Type::bool(), Type::bool())),
            _ => return None,
        })
    }

    fn gen_var(&mut self, name: &str, nid: u32, env: &[EnvEntry]) -> Type {
        // 最近绑定优先（shadowing）
        if let Some(entry) = env.iter().rev().find(|e| e.name == name) {
            return self.use_binding(nid, &entry.binding, name);
        }
        if let Some(scheme) = Self::builtin_scheme(name) {
            return self.instantiate_builtin(nid, name, &scheme);
        }
        // 未绑定变量：当成全新推断变量，并让后续统一暴露错误
        let v = self.fresh(nid, "unbound", VarOrigin::Infer);
        self.emit(Event::Generalize {
            key: format!("unbound|{name}|n{nid}"),
            target: v.clone(),
            vars: vec![],
            nid,
            is_value: false,
            reason: format!("名字 '{name}' 不在作用域中"),
            scope: self.scope_key(),
        });
        Type::var(v)
    }

    fn use_binding(&mut self, nid: u32, b: &Binding, name: &str) -> Type {
        match b {
            Binding::Monomorphic { var } => Type::var(var.clone()),
            Binding::Scheme { quant, body } => {
                self.instantiate_scheme(nid, name, quant.clone(), body.clone())
            }
        }
    }

    fn instantiate_scheme(
        &mut self,
        nid: u32,
        name: &str,
        quant: Vec<String>,
        body: Type,
    ) -> Type {
        let mut copies = Vec::new();
        let mut mapping: BTreeMap<String, String> = BTreeMap::new();
        for q in &quant {
            let fresh = self.fresh(nid, format!("inst_{}", short(q)).as_str(), VarOrigin::Inst);
            mapping.insert(q.clone(), fresh.clone());
            copies.push((q.clone(), Type::var(fresh)));
        }
        let result = substitute(&body, &mapping);
        let gen_env = self.gen_env_key();
        self.emit(Event::Instantiate {
            key: format!("inst|{gen_env}|n{nid}|{name}"),
            scheme_var: name.to_string(),
            copies,
            result: result.clone(),
            nid,
            gen_env,
        });
        result
    }

    fn instantiate_builtin(&mut self, nid: u32, name: &str, scheme: &Type) -> Type {
        // 收集 scheme 中的自由变量并整体实例化
        let mut fvs = Vec::new();
        collect_vars(scheme, &mut fvs);
        fvs.sort();
        fvs.dedup();
        let mut copies = Vec::new();
        let mut mapping: BTreeMap<String, String> = BTreeMap::new();
        for q in fvs {
            let fresh = self.fresh(nid, format!("bip_{}", short(&q)).as_str(), VarOrigin::Inst);
            mapping.insert(q.clone(), fresh.clone());
            copies.push((q.clone(), Type::var(fresh)));
        }
        let result = substitute(scheme, &mapping);
        let gen_env = self.gen_env_key();
        self.emit(Event::Instantiate {
            key: format!("inst|{gen_env}|n{nid}|builtin_{name}"),
            scheme_var: name.to_string(),
            copies,
            result: result.clone(),
            nid,
            gen_env,
        });
        result
    }

    fn gen_let(
        &mut self,
        is_rec: bool,
        nid: u32,
        name: &Pat,
        value: &Expr,
        body: &Expr,
        env: &Vec<EnvEntry>,
    ) -> Type {
        self.gen_level += 1;
        let level = self.gen_level;
        self.push_scope(nid, "let");

        let value_t = if is_rec {
            // 递归：先给绑定名一个单态变量（value restriction：递归绑定保持单态）
            let rec_var = self.fresh(name.nid(), "rec_self", VarOrigin::Infer);
            let _ = level;
            let mut value_env = env.to_vec();
            value_env.push(EnvEntry {
                name: pat_name(name),
                binding: Binding::Monomorphic { var: rec_var.clone() },
            });
            let vt = self.gen(value, &value_env);
            self.emit_equate(
                nid,
                "rec_self_eq",
                Type::var(rec_var.clone()),
                vt,
                "递归绑定自身",
                "右侧表达式",
                "递归绑定的类型必须与其定义一致",
            );
            Type::var(rec_var)
        } else {
            self.gen(value, env)
        };

        // 泛化（value restriction）
        let is_value = matches!(
            value,
            Expr::Lam { .. } | Expr::Int { .. } | Expr::Bool { .. } | Expr::Str { .. }
                | Expr::Record { .. }
                | Expr::Variant { .. }
        );
        let b = if is_value {
            let root = match &value_t {
                Type::Var(v) => v.clone(),
                _ => {
                    // 合成一个承载变量（如 record 直接给出）
                    let h = self.fresh(nid, "let_carry", VarOrigin::Infer);
                    self.emit_equate(
                        nid,
                        "let_carry",
                        Type::var(h.clone()),
                        value_t.clone(),
                        "let 绑定",
                        "右侧表达式",
                        "承载绑定类型",
                    );
                    h
                }
            };
            let quant = self.collect_generalizable(&root);
            self.emit(Event::Generalize {
                key: format!("gen|n{nid}|{}", pat_key(name)),
                target: root.clone(),
                vars: quant.clone(),
                nid,
                is_value: true,
                reason: "右侧为语法值，允许泛化".into(),
                scope: self.scope_key(),
            });
            Binding::Scheme { quant, body: value_t }
        } else {
            let root = match &value_t {
                Type::Var(v) => v.clone(),
                _ => {
                    let h = self.fresh(nid, "let_carry", VarOrigin::Infer);
                    self.emit_equate(
                        nid,
                        "let_carry",
                        Type::var(h.clone()),
                        value_t.clone(),
                        "let 绑定",
                        "右侧表达式",
                        "承载绑定类型",
                    );
                    h
                }
            };
            self.emit(Event::Generalize {
                key: format!("gen|n{nid}|{}", pat_key(name)),
                target: root.clone(),
                vars: vec![],
                nid,
                is_value: false,
                reason: "右侧不是语法值（value restriction），保持单态".into(),
                scope: self.scope_key(),
            });
            Binding::Monomorphic { var: root }
        };

        // 泛化环境摘要：进入 body 前登记该多态绑定
        if matches!(b, Binding::Scheme { .. }) {
            self.gen_env.push(format!("g:{}", name_span_key(name)));
        }

        let was_scheme = matches!(b, Binding::Scheme { .. });
        let mut body_env = env.to_vec();
        body_env.push(EnvEntry { name: pat_name(name), binding: b });
        let bt = self.gen(body, &body_env);

        if was_scheme {
            self.gen_env.pop();
        }
        self.pop_scope();
        self.gen_level -= 1;
        bt
    }

    // 收集可在当前泛化层级量化的变量：
    // 以 root 为起点可达、level == 当前层 且未被外层捕获。
    // 事件阶段尚不知道并查集连通关系，因此生成“候选集合”由 solver 细化：
    // 这里列出所有在当前 scope 声明且出现在 value_t 中的变量名。
    fn collect_generalizable(&self, root: &str) -> Vec<String> {
        let scope_prefix = self.scope_key();
        let mut reachable = Vec::new();
        collect_from_events_for_scope(&self.events, &scope_prefix, root, &mut reachable);
        reachable.sort();
        reachable.dedup();
        reachable
    }

    fn gen_match(
        &mut self,
        nid: u32,
        scrutinee: &Expr,
        arms: &[(Pat, Expr)],
        env: &Vec<EnvEntry>,
    ) -> Type {
        let st = self.gen(scrutinee, env);
        // 每个 arm 引入一个单态绑定；scrutinee 与每个 arm 模式类型统一
        let result = self.fresh(nid, "match_result", VarOrigin::Infer);
        for (i, (pat, body)) in arms.iter().enumerate() {
            let pv = self.fresh(pat.nid(), format!("arm{i}_pat").as_str(), VarOrigin::Infer);
            self.emit_equate(
                scrutinee.nid(),
                &format!("match_arm{i}_pattern"),
                st.clone(),
                Type::var(pv.clone()),
                "被匹配对象",
                &format!("第 {} 个分支模式", i + 1),
                "分支模式必须与被匹配对象类型相容",
            );
            let mut inner = env.to_vec();
            inner.push(EnvEntry {
                name: pat_name(pat),
                binding: Binding::Monomorphic { var: pv },
            });
            self.push_scope(nid, "match");
            let bt = self.gen(body, &inner);
            self.pop_scope();
            self.emit_equate(
                nid,
                &format!("match_arm{i}_result"),
                Type::var(result.clone()),
                bt,
                "match 结果",
                &format!("第 {} 个分支结果", i + 1),
                "所有分支结果类型必须一致",
            );
        }
        Type::var(result)
    }
}

fn pat_key(p: &Pat) -> String {
    format!("pat{}", p.nid())
}
fn pat_name(p: &Pat) -> String {
    match p {
        Pat::PVar { name, .. } => name.clone(),
        Pat::PWild { .. } => "_".to_string(),
    }
}
fn name_span_key(p: &Pat) -> String {
    match p {
        Pat::PVar { name, nid, .. } => format!("{name}@{nid}"),
        Pat::PWild { nid, .. } => format!("_@{nid}"),
    }
}
fn short(v: &str) -> String {
    v.replace('#', "_").replace('n', "")
}

pub fn substitute(t: &Type, mapping: &BTreeMap<String, String>) -> Type {
    match t {
        Type::Var(v) => {
            if let Some(f) = mapping.get(v) {
                Type::var(f)
            } else {
                Type::var(v)
            }
        }
        Type::Con { .. } => t.clone(),
        Type::Arrow(a, b) => Type::arrow(substitute(a, mapping), substitute(b, mapping)),
        Type::Row(r) => {
            let fields = r
                .fields
                .iter()
                .map(|(l, ty)| (l.clone(), substitute(ty, mapping)))
                .collect();
            let tail = match &r.tail {
                RowTail::Empty => RowTail::Empty,
                RowTail::Open { var } => RowTail::Open {
                    var: mapping.get(var).cloned().unwrap_or_else(|| var.clone()),
                },
            };
            Type::Row(RowType { flavor: r.flavor, fields, tail })
        }
    }
}

fn collect_vars(t: &Type, out: &mut Vec<String>) {
    match t {
        Type::Var(v) => out.push(v.clone()),
        Type::Con { .. } => {}
        Type::Arrow(a, b) => {
            collect_vars(a, out);
            collect_vars(b, out);
        }
        Type::Row(r) => {
            for (_, ty) in &r.fields {
                collect_vars(ty, out);
            }
            if let RowTail::Open { var } = &r.tail {
                out.push(var.clone());
            }
        }
    }
}

// 仅根据当前 scope 内声明并从 root 结构可达的变量给出候选；
// 真实可达集合由 solver 在执行 Generalize 时依据并查集确定。
fn collect_from_events_for_scope(
    events: &[Event],
    _scope: &str,
    root: &str,
    out: &mut Vec<String>,
) {
    // 简化且稳健：找到以 root 出现的 Define/Equate 结构，收集其中变量。
    let mut stack = vec![root.to_string()];
    let mut seen = std::collections::BTreeSet::new();
    while let Some(v) = stack.pop() {
        if !seen.insert(v.clone()) {
            continue;
        }
        for ev in events {
            let term: Option<Type> = match ev {
                Event::Define { var, term, .. } if var == &v => Some(term.clone()),
                Event::Equate { left, right, .. } => {
                    if contains_var(left, &v) {
                        Some(right.clone())
                    } else if contains_var(right, &v) {
                        Some(left.clone())
                    } else {
                        None
                    }
                }
                _ => None,
            };
            if let Some(t) = term {
                let mut vs = Vec::new();
                collect_vars(&t, &mut vs);
                for x in vs {
                    stack.push(x);
                }
            }
        }
        out.push(v);
    }
}

fn contains_var(t: &Type, v: &str) -> bool {
    let mut vs = Vec::new();
    collect_vars(t, &mut vs);
    vs.iter().any(|x| x == v)
}
