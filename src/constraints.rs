// 约束生成：把 AST 转成扁平约束列表。
// key 跨编辑稳定；cache_key 额外包含作用域链与泛化环境指纹，
// 因此重命名不导致身份漂移，而作用域/泛化环境改变会让缓存失效。
use crate::lexer::Span;
use crate::parser::{AST, Arm, NodeId, NodeKind, PatKind};
use crate::types::{ProvSet, RowTail, Ty, TV};
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "k", content = "v")]
pub enum Intro {
    /// 非递归 let：进入新的泛化层级
    PushLevel,
    /// match 分支开始，可能引入若干模式变量
    Branch(Vec<Intro>),
    LamParam { gen_node: u32, name: String, tv: TV },
    PatVar { gen_node: u32, name: String, tv: TV },
    RecStart { gen_node: u32, name: String, tv: TV },
    LetName { gen_node: u32, name: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "k", content = "v")]
pub enum ConstraintKind {
    /// 作用域/层级开始。多个 Begin 可共享同一 frame（let 的 PushLevel 与 LetName）。
    Begin { frame: u32, intro: Intro },
    EndScope { frame: u32 },
    Instantiate { var: String, gen_node: u32, rec: bool, target: TV },
    Generalize { bind_node: u32, bind_type: TV, name: String, rec: bool, mono: Vec<TV> },
    Equal {
        left: Ty,
        right: Ty,
        expected: Option<u32>,
        actual: Option<u32>,
        label: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Constraint {
    pub cid: u32,
    pub key: String,
    pub cache_key: String,
    pub kind: ConstraintKind,
    pub span: Span,
    pub prov: ProvSet,
}

impl Constraint {
    pub fn is_semantic(&self) -> bool {
        // Begin/EndScope 只管理环境，不参与可满足性（不可被禁用为“错误核心”成员）
        !matches!(self.kind, ConstraintKind::Begin { .. } | ConstraintKind::EndScope { .. })
    }
}

#[derive(Debug, Clone)]
pub struct GenResult {
    pub constraints: Vec<Constraint>,
    pub root_ty: TV,
    pub tv_count: u32,
    pub undef: Vec<(String, Span)>,
}

#[derive(Debug, Clone)]
struct Binding {
    gen_node: u32,
    rec: bool,
}

struct GC<'a> {
    ast: &'a AST,
    cs: Vec<Constraint>,
    tvc: u32,
    env: Vec<(String, Vec<Binding>)>,
    /// 每个活动帧引入的名字，EndScope 时弹出；shadowing 安全
    frame_names: Vec<Vec<String>>,
    gen_env: std::collections::HashMap<u32, String>,
    undef: Vec<(String, Span)>,
}

impl<'a> GC<'a> {
    fn fresh(&mut self) -> TV {
        let t = self.tvc;
        self.tvc += 1;
        t
    }

    fn prov(&self, node: u32) -> ProvSet {
        ProvSet::node(node, Some(self.ast.nodes[node as usize].span.clone()))
    }

    fn lookup(&self, name: &str) -> Option<Vec<Binding>> {
        self.env.iter().rev().find_map(|(n, v)| if n == name { Some(v.clone()) } else { None })
    }

    fn push_binding(&mut self, name: String, b: Binding) {
        if let Some(slot) = self.env.iter_mut().find(|(n, _)| n == &name) {
            slot.1.push(b);
        } else {
            self.env.push((name, vec![b]));
        }
        if let Some(f) = self.frame_names.last_mut() {
            f.push(name_or_dummy(&b));
        }
    }

    fn begin_frame(&mut self) {
        self.frame_names.push(Vec::new());
    }

    fn end_frame(&mut self) {
        let names = self.frame_names.pop().unwrap();
        for nm in names {
            if nm == DUMMY {
                continue;
            }
            let slot = self.env.iter_mut().find(|(n, _)| n == &nm).unwrap();
            slot.1.pop();
        }
    }

    fn scope_chain(&self) -> String {
        (0..self.frame_names.len() as u32).map(|x| x.to_string()).collect::<Vec<_>>().join(">")
    }

    fn emit(&mut self, kind: ConstraintKind, span: Span, key_node: u32, suffix: &str, prov: ProvSet) {
        let cid = self.cs.len() as u32;
        let key = format!("n{}.{}", key_node, suffix);
        let envfp = match &kind {
            ConstraintKind::Instantiate { gen_node, .. } => {
                self.gen_env.get(gen_node).cloned().unwrap_or_else(|| "?".into())
            }
            ConstraintKind::Generalize { bind_node, .. } => {
                self.gen_env.get(bind_node).cloned().unwrap_or_else(|| "self".into())
            }
            _ => String::new(),
        };
        let cache_key = format!("{}|s={}|g={}", key, self.scope_chain(), envfp);
        self.cs.push(Constraint { cid, key, cache_key, kind, span, prov });
    }

    fn eq(&mut self, span: Span, node: u32, suffix: &str, left: Ty, right: Ty,
          expected: Option<u32>, actual: Option<u32>, label: impl Into<String>) {
        let prov = self.prov(node);
        self.emit(ConstraintKind::Equal { left, right, expected, actual, label: label.into() },
                  span, node, suffix, prov);
    }

    fn env_fp(&self) -> String {
        let mut parts: Vec<String> = self
            .env
            .iter()
            .map(|(name, stack)| {
                let top = stack.last().unwrap();
                format!("{}@{}", name, top.gen_node)
            })
            .collect();
        parts.sort();
        parts.join(",")
    }

    fn arm_frame(&self, match_node: u32, idx: usize) -> u32 {
        0x4000_0000u32.wrapping_add(match_node.wrapping_mul(64)).wrapping_add(idx as u32 + 1)
    }
}

const DUMMY: &str = "\u{0001}";
fn name_or_dummy(b: &Binding) -> String {
    let _ = b;
    DUMMY.to_string()
}

pub fn generate(ast: &AST) -> GenResult {
    let mut g = GC {
        ast,
        cs: Vec::new(),
        tvc: 0,
        env: Vec::new(),
        frame_names: Vec::new(),
        gen_env: std::collections::HashMap::new(),
        undef: Vec::new(),
    };
    g.begin_frame(); // 顶层帧：让 push_binding 始终有地方挂名字
    let root = g.gen_node(ast.root);
    let t = match root { Ty::Var(t) => t, _ => unreachable!() };
    GenResult { constraints: g.cs, root_ty: t, tv_count: g.tvc, undef: g.undef }
}

impl<'a> GC<'a> {
    fn push_named(&mut self, name: String, b: Binding) {
        if let Some(slot) = self.env.iter_mut().find(|(n, _)| n == &name) {
            slot.1.push(b);
        } else {
            self.env.push((name.clone(), vec![b]));
        }
        self.frame_names.last_mut().unwrap().push(name);
    }

    fn gen_pat(&mut self, arm: &Arm, intros: &mut Vec<Intro>) {
        match &arm.pat {
            PatKind::Wild => {}
            PatKind::Var(name) => {
                let tv = self.fresh();
                intros.push(Intro::PatVar { gen_node: arm.expr, name: name.clone(), tv });
            }
            PatKind::Con { label, payload } => {
                let tv = self.fresh();
                if let Some(pname) = payload {
                    intros.push(Intro::PatVar { gen_node: arm.expr, name: pname.clone(), tv });
                }
                // 变体标签约束直接在 gen_node 中生成；这里记录 tv 到临时映射
                self.arm_payload.insert((arm.expr, label.clone()), tv);
            }
        }
    }

    fn gen_node(&mut self, node: NodeId) -> Ty {
        let n = self.ast.nodes[node as usize].clone();
        let span = n.span.clone();
        match &n.kind {
            NodeKind::Int(_) => self.prim(node, span, Ty::int(), "整数字面量"),
            NodeKind::Str(_) => self.prim(node, span, Ty::str(), "字符串字面量"),
            NodeKind::Bool(_) => self.prim(node, span, Ty::bool(), "布尔字面量"),
            NodeKind::Unit => self.prim(node, span, Ty::unit(), "单位值"),
            NodeKind::Var(name) => {
                let t = self.fresh();
                match self.lookup(name) {
                    Some(stack) => {
                        let b = stack.last().unwrap().clone();
                        self.emit(
                            ConstraintKind::Instantiate {
                                var: name.clone(),
                                gen_node: b.gen_node,
                                rec: b.rec,
                                target: t,
                            },
                            span, node, "inst", self.prov(node),
                        );
                    }
                    None => {
                        self.undef.push((name.clone(), span.clone()));
                        self.eq(
                            span, node, "undef",
                            Ty::Con { name: format!("?{}", name), args: vec![] }, Ty::Var(t),
                            Some(node), Some(node), format!("未定义变量 '{}'", name),
                        );
                    }
                }
                Ty::Var(t)
            }
            NodeKind::Lam { param, body } => {
                let pname = match &self.ast.nodes[*param as usize].kind {
                    NodeKind::Param { name } => name.clone(),
                    _ => unreachable!(),
                };
                let ptv = self.fresh();
                let frame = *param;
                self.begin_frame();
                self.emit(
                    ConstraintKind::Begin { frame, intro: Intro::LamParam { gen_node: *param, name: pname, tv: ptv } },
                    span.clone(), node, "begin", self.prov(node),
                );
                self.push_named(
                    match &self.ast.nodes[*param as usize].kind { NodeKind::Param { name } => name.clone(), _ => unreachable!() },
                    Binding { gen_node: *param, rec: false },
                );
                let bt = self.gen_node(*body);
                self.emit(ConstraintKind::EndScope { frame }, span.clone(), node, "end", self.prov(node));
                self.end_frame();
                let t = self.fresh();
                self.eq(n.span.clone(), node, "lam", Ty::arrow(Ty::Var(ptv), bt), Ty::Var(t),
                        Some(node), Some(*body), "函数抽象");
                Ty::Var(t)
            }
            NodeKind::App { fun, arg } => {
                let tf = self.gen_node(*fun);
                let ta = self.gen_node(*arg);
                let tr = self.fresh();
                self.eq(span.clone(), node, "app", Ty::arrow(ta, Ty::Var(tr)), tf,
                        Some(node), Some(*fun), "函数应用要求函数类型");
                Ty::Var(tr)
            }
            NodeKind::If { cond, then, els } => {
                let tc = self.gen_node(*cond);
                self.eq(span.clone(), node, "ifcond", Ty::bool(), tc, Some(node), Some(*cond), "if 条件必须是 Bool");
                let tt = self.gen_node(*then);
                let te = self.gen_node(*els);
                self.eq(n.span.clone(), node, "ifbranches", tt.clone(), te, Some(*then), Some(*els), "两个分支类型必须一致");
                let t = self.fresh();
                self.eq(span.clone(), node, "ifres", Ty::Var(t), tt, Some(node), Some(*then), "if 结果类型");
                Ty::Var(t)
            }
            NodeKind::Record { fields, closed } => {
                let mut ftys: Vec<(String, Ty)> = Vec::new();
                for (label, fnode) in fields {
                    let ft = self.gen_node(*fnode);
                    ftys.push((label.clone(), ft));
                }
                let tail = if *closed { RowTail::Closed } else { RowTail::Open(self.fresh()) };
                let t = self.fresh();
                self.eq(span.clone(), node, "record", Ty::record(ftys, tail), Ty::Var(t),
                        Some(node), Some(node), "记录字面量");
                Ty::Var(t)
            }
            NodeKind::Field { object, label } => {
                let to = self.gen_node(*object);
                let t = self.fresh();
                let rest = self.fresh();
                self.eq(
                    span.clone(), node, "field",
                    Ty::record(vec![(label.clone(), Ty::Var(t))], RowTail::Open(rest)),
                    to,
                    Some(node), Some(*object), format!("字段访问 .{}", label),
                );
                Ty::Var(t)
            }
            NodeKind::Variant { label, payload, open } => {
                let pt = match payload {
                    Some(p) => self.gen_node(*p),
                    None => Ty::unit(),
                };
                let tail = if *open { RowTail::Open(self.fresh()) } else { RowTail::Closed };
                let t = self.fresh();
                self.eq(span.clone(), node, "variant",
                        Ty::variant(vec![(label.clone(), pt)], tail), Ty::Var(t),
                        Some(node), payload.or(Some(node)), "变体构造");
                Ty::Var(t)
            }
            NodeKind::Match { scrutinee, arms } => {
                let ts = self.gen_node(*scrutinee);
                let result = self.fresh();
                for (i, arm) in arms.iter().enumerate() {
                    let frame = self.arm_frame(node, i);
                    self.begin_frame();
                    let mut intros = Vec::new();
                    let mut tag: Option<(String, TV)> = None;
                    match &arm.pat {
                        PatKind::Wild => {}
                        PatKind::Var(name) => {
                            let tv = self.fresh();
                            intros.push(Intro::PatVar { gen_node: arm.expr, name: name.clone(), tv });
                        }
                        PatKind::Con { label, payload } => {
                            let tv = self.fresh();
                            if let Some(pname) = payload {
                                intros.push(Intro::PatVar { gen_node: arm.expr, name: pname.clone(), tv });
                            }
                            tag = Some((label.clone(), tv));
                        }
                    }
                    self.emit(
                        ConstraintKind::Begin { frame, intro: Intro::Branch(intros) },
                        arm.pat_span.clone(), node, &format!("arm{}b", i), self.prov(node),
                    );
                    if let Some((label, tv)) = tag {
                        self.eq(
                            arm.pat_span.clone(), node, &format!("arm{}tag", i),
                            Ty::variant(vec![(label, Ty::Var(tv))], RowTail::Open(self.fresh())),
                            ts.clone(),
                            Some(node), Some(*scrutinee), format!("分支要求变体 {}", label),
                        );
                    }
                    for it in &intros {
                        if let Intro::PatVar { gen_node, name, tv } = it {
                            self.push_named(name.clone(), Binding { gen_node: *gen_node, rec: false });
                            // 模式变量在该分支内：绑定为任意类型（通过 fresh tv 与 payload/整体判定式的约束建立）
                            // Wild/Var 分支对判定式没有标签约束（兜底分支）。
                        }
                    }
                    let tb = self.gen_node(arm.expr);
                    self.eq(n.span.clone(), node, &format!("arm{}res", i), Ty::Var(result), tb,
                            Some(node), Some(arm.expr), "match 分支结果一致");
                    self.emit(ConstraintKind::EndScope { frame }, n.span.clone(),
                              node, &format!("arm{}e", i), self.prov(node));
                    self.end_frame();
                }
                Ty::Var(result)
            }
            NodeKind::Let { rec, name, bind, body } => self.gen_let(node, *rec, *name, *bind, *body),
            NodeKind::Param { .. } | NodeKind::Bind { .. } => unreachable!("辅助节点不会被生成"),
        }
    }

    fn prim(&mut self, node: NodeId, span: Span, ty: Ty, label: &str) -> Ty {
        let t = self.fresh();
        self.eq(span, node, "lit", ty, Ty::Var(t), Some(node), Some(node), label);
        Ty::Var(t)
    }
}
