//! 端到端分析：结构指纹 → 约束生成（含缓存键）→ 求解 → 结果视图。
use crate::ast::*;
use crate::parser::parse;
use crate::constraints::*;
use crate::solve::*;
use crate::types::*;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

/// FNV-1a 双 64 → 128 位键。
struct Fnv { lo: u64, hi: u64 }
impl Fnv {
    fn new() -> Self { Fnv { lo: 0xcbf29ce484222325, hi: 0x84222d51de235e13 } }
    fn byte(&mut self, b: u8) {
        self.lo ^= b as u64;
        self.lo = self.lo.wrapping_mul(0x100000001b3);
        self.hi = self.hi.wrapping_add(self.lo.rotate_left(13) ^ b as u64);
        self.hi = self.hi.wrapping_mul(0x100000001b3);
    }
    fn bytes(&mut self, bs: &[u8]) { for b in bs { self.byte(*b); } }
    fn u(&mut self, x: u64) { self.bytes(&x.to_le_bytes()); }
    fn str(&mut self, s: &str) { self.u(s.len() as u64); self.bytes(s.as_bytes()); }
    fn finish(self) -> u128 { ((self.hi as u128) << 64) | self.lo as u128 }
}

/// 结构指纹（不含位置）。重命名只影响作用域链，不改变树结构本身。
fn fingerprint(e: &Expr) -> u64 {
    let mut f = Fnv::new();
    fp_expr(e, &mut f);
    f.lo ^ f.hi
}

fn kind_byte(k: NodeKind) -> u8 {
    match k {
        NodeKind::Program => 1, NodeKind::Let => 2, NodeKind::LitInt => 3, NodeKind::LitBool => 4,
        NodeKind::Var => 5, NodeKind::Lam => 6, NodeKind::App => 7, NodeKind::BinOp => 8,
        NodeKind::If => 9, NodeKind::Record => 10, NodeKind::Field => 11, NodeKind::Proj => 12,
        NodeKind::Variant => 13, NodeKind::Match => 14, NodeKind::Arm => 15, NodeKind::Unit => 16,
    }
}

fn fp_expr(e: &Expr, f: &mut Fnv) {
    f.byte(kind_byte(e.kind()));
    match e {
        Expr::LitInt { value, .. } => f.u(*value as u64),
        Expr::LitBool { value, .. } => f.byte(*value as u8),
        Expr::Let { rec_, .. } => f.byte(*rec_ as u8),
        Expr::BinOp { op, .. } => f.byte(match op {
            BinOpKind::Add => 1, BinOpKind::Sub => 2, BinOpKind::Mul => 3, BinOpKind::Eq => 4,
        }),
        Expr::Proj { field, .. } => f.str(field),
        Expr::Variant { label, .. } => f.str(label),
        Expr::Record { fields, .. } => {
            f.u(fields.len() as u64);
            for fld in fields { f.str(&fld.name); }
        }
        Expr::Match { arms, .. } => {
            f.u(arms.len() as u64);
            for a in arms { f.str(&a.label); f.byte(a.payload.is_some() as u8); }
        }
        _ => {}
    }
    let mut kids = vec![];
    e.children(&mut kids);
    f.u(kids.len() as u64);
    for c in kids { fp_expr(c, f); }
}

fn is_value(e: &Expr) -> bool {
    matches!(e,
        Expr::LitInt { .. } | Expr::LitBool { .. } | Expr::Unit { .. }
        | Expr::Lam { .. } | Expr::Record { .. } | Expr::Variant { .. })
}

fn op_name(op: &BinOpKind) -> &'static str {
    match op { BinOpKind::Add => "+", BinOpKind::Sub => "-", BinOpKind::Mul => "*", BinOpKind::Eq => "==" }
}

#[derive(Clone, Copy)]
struct ScopeSlot {
    kind: u8,
    name_h: u64,
    node_fp: u64,
    gen_h: u64,
}

struct GenCx {
    cons: Vec<Constraint>,
    fps: BTreeMap<NodeId, u64>,
    scope: Vec<ScopeSlot>,
    cid_seq: u32,
}

impl GenCx {
    fn new() -> Self { GenCx { cons: vec![], fps: BTreeMap::new(), scope: vec![], cid_seq: 0 } }

    fn toggleable(c: &Con) -> bool {
        !matches!(c, Con::ScopeBegin { .. } | Con::ScopeEnd { .. }
            | Con::GenBind { .. } | Con::RecBegin { .. } | Con::RecEnd { .. })
    }

    fn add(&mut self, node: NodeId, span: Span, kind: &str, label: String, con: Con) {
        let id = CId(self.cid_seq);
        let toggleable = Self::toggleable(&con);
        let key = self.make_key(node, &con);
        self.cid_seq += 1;
        self.cons.push(Constraint { id, node, span, kind: kind.into(), label, con, key, toggleable });
    }

    /// 约束缓存键：结构指纹 ⊕ 作用域链（含绑定名哈希与泛化标志）⊕ 约束种类与序号。
    fn make_key(&self, node: NodeId, con: &Con) -> u128 {
        let mut f = Fnv::new();
        f.u(self.fps.get(&node).copied().unwrap_or(0));
        f.u(self.scope.len() as u64);
        for s in &self.scope {
            f.byte(s.kind);
            f.u(s.name_h);
            f.u(s.node_fp);
            f.u(s.gen_h);
        }
        // 约束“形状”：种类标签 + 涉及的节点指纹，保证不同约束不撞键。
        let tag: u8 = match con {
            Con::ExpectPrim { prim, .. } => *prim as u8 + 10,
            Con::MkArrow { .. } => 20,
            Con::MkRecord { closed, .. } => if *closed { 30 } else { 31 },
            Con::MkVariant { .. } => 40,
            Con::Eq { pol, .. } => match pol { Polarity::Equal => 50, Polarity::ExpectedActual => 51 },
            Con::VarUse { name, .. } => { f.str(name); 60 }
            Con::ScopeBegin { .. } => 70,
            Con::BindLocal { .. } => 72,
            Con::ScopeEnd { .. } => 71,
            Con::GenBind { is_value, .. } => if *is_value { 80 } else { 81 },
            Con::RecBegin { .. } => 90,
            Con::RecEnd { is_value, .. } => if *is_value { 91 } else { 92 },
            Con::Fail { .. } => 100,
        };
        f.byte(tag);
        for n in con_nodes(con) { f.u(self.fps.get(&n).copied().unwrap_or(0)); }
        // 同节点同类约束的稳定次序（生成器结构决定）。
        f.u(self.cid_seq as u64);
        f.finish()
    }

    fn push(&mut self, slot: ScopeSlot) { self.scope.push(slot); }
    fn pop(&mut self) { self.scope.pop(); }
    fn name_h(s: &str) -> u64 { let mut f = Fnv::new(); f.str(s); f.lo ^ f.hi }
    fn gen_h(b: BindKind, is_value: bool) -> u64 {
        let mut f = Fnv::new();
        f.byte(match b {
            BindKind::LamParam => 1, BindKind::LetPoly => 2, BindKind::LetMono => 3,
            BindKind::Rec => 4, BindKind::ArmPayload => 5,
        });
        f.byte(is_value as u8);
        f.lo ^ f.hi
    }

    fn gen(&mut self, e: &Expr) {
        match e {
            Expr::Unit { nid, span, .. } => self.add(*nid, *span, "prim", "= unit".into(),
                Con::ExpectPrim { node: *nid, prim: Prim::Unit, span: *span }),
            Expr::LitInt { nid, span, .. } => self.add(*nid, *span, "prim", "= int".into(),
                Con::ExpectPrim { node: *nid, prim: Prim::Int, span: *span }),
            Expr::LitBool { nid, span, .. } => self.add(*nid, *span, "prim", "= bool".into(),
                Con::ExpectPrim { node: *nid, prim: Prim::Bool, span: *span }),
            Expr::Var { nid, name, span, .. } => self.add(*nid, *span, "var",
                format!("使用 {}", name), Con::VarUse { node: *nid, name: name.clone(), span: *span }),

            Expr::Lam { nid, span, param, param_span, param_nid, body } => {
                self.add(*nid, *span, "scope", format!("λ 参数 {}", param),
                    Con::ScopeBegin { level_up: true, span: *param_span });
                self.add(*param_nid, *param_span, "bind", format!("绑定参数 {}", param),
                    Con::BindLocal { name: param.clone(), node: *param_nid,
                        kind: BindKind::LamParam, span: *param_span });
                self.push(ScopeSlot { kind: 1, name_h: Self::name_h(param),
                    node_fp: self.fps[param_nid], gen_h: Self::gen_h(BindKind::LamParam, false) });
                self.gen(body);
                self.pop();
                self.add(*nid, *span, "arrow", format!("{} -> 结果", param),
                    Con::MkArrow { node: *nid, arg: *param_nid, ret: body.nid(), span: *span });
                self.add(*nid, *span, "scope_end", "离开 λ".into(),
                    Con::ScopeEnd { level_down: true, span: *span });
            }

            Expr::App { nid, span, fun, arg } => {
                self.gen(fun);
                self.gen(arg);
                self.add(*nid, *span, "apply", "调用：函数类型 = 参数 -> 结果".into(),
                    Con::MkArrow { node: fun.nid(), arg: arg.nid(), ret: *nid, span: *span });
            }

            Expr::BinOp { nid, span, op, lhs, rhs } => {
                self.gen(lhs);
                self.gen(rhs);
                let lbl = op_name(op);
                self.add(lhs.nid(), lhs.span(), "prim", format!("{} 左=int", lbl),
                    Con::ExpectPrim { node: lhs.nid(), prim: Prim::Int, span: lhs.span() });
                self.add(rhs.nid(), rhs.span(), "prim", format!("{} 右=int", lbl),
                    Con::ExpectPrim { node: rhs.nid(), prim: Prim::Int, span: rhs.span() });
                let prim = if matches!(op, BinOpKind::Eq) { Prim::Bool } else { Prim::Int };
                self.add(*nid, *span, "prim", format!("{} 结果", lbl),
                    Con::ExpectPrim { node: *nid, prim, span: *span });
            }

            Expr::If { nid, span, cond, then_e, else_e } => {
                self.gen(cond);
                self.gen(then_e);
                self.gen(else_e);
                self.add(cond.nid(), cond.span(), "prim", "条件=bool".into(),
                    Con::ExpectPrim { node: cond.nid(), prim: Prim::Bool, span: cond.span() });
                self.add(then_e.nid(), then_e.span(), "eq", "结果=then".into(),
                    Con::Eq { a: *nid, b: then_e.nid(), pol: Polarity::Equal,
                        label: "结果=then".into(), span: then_e.span() });
                self.add(else_e.nid(), else_e.span(), "eq", "结果=else（期望/实际）".into(),
                    Con::Eq { a: *nid, b: else_e.nid(), pol: Polarity::ExpectedActual,
                        label: "结果=else".into(), span: else_e.span() });
            }

            Expr::Record { nid, span, fields } => {
                let mut seen = BTreeSet::new();
                let mut flds = vec![];
                for fld in fields {
                    self.gen(&fld.value);
                    flds.push((fld.name.clone(), fld.value.nid(), fld.name_span));
                    if !seen.insert(fld.name.clone()) {
                        self.add(fld.value.nid(), fld.name_span, "fail",
                            format!("重复字段 {}", fld.name),
                            Con::Fail { message: format!("记录中字段 '{}' 重复出现", fld.name),
                                span: fld.name_span, node: fld.value.nid() });
                    }
                }
                self.add(*nid, *span, "record", "构造封闭记录".into(),
                    Con::MkRecord { node: *nid, fields: flds, closed: true, span: *span });
            }

            Expr::Proj { nid, span, record, field, field_span } => {
                self.gen(record);
                self.add(*nid, *span, "proj", format!("投影 .{}", field),
                    Con::MkRecord { node: record.nid(),
                        fields: vec![(field.clone(), *nid, *field_span)], closed: false, span: *span });
            }

            Expr::Variant { nid, span, label, label_span, payload } => {
                let payload = payload.as_ref().map(|p| { self.gen(p); p.nid() })
                    .unwrap_or_else(|| NodeId(u32::MAX));
                self.add(*nid, *span, "variant", format!("构造 `{}", label),
                    Con::MkVariant { node: *nid, label: label.clone(), payload,
                        open: true, span: *label_span });
            }

            Expr::Match { nid, span, scrutinee, arms } => {
                self.gen(scrutinee);
                for arm in arms {
                    // 1) 开作用域并先绑定载荷（载荷类型在与 scrutinee 标签统一时确定）
                    self.add(arm.payload_nid, arm.label_span, "scope",
                        format!("分支 `{} 作用域", arm.label),
                        Con::ScopeBegin { level_up: true, span: arm.label_span });
                    if let Some(pname) = &arm.payload {
                        self.add(arm.payload_nid, arm.payload_span.unwrap_or(arm.label_span), "bind",
                            format!("绑定载荷 {}", pname),
                            Con::BindLocal { name: pname.clone(), node: arm.payload_nid,
                                kind: BindKind::ArmPayload,
                                span: arm.payload_span.unwrap_or(arm.label_span) });
                        self.push(ScopeSlot { kind: 5, name_h: Self::name_h(pname),
                            node_fp: self.fps[&arm.payload_nid],
                            gen_h: Self::gen_h(BindKind::ArmPayload, false) });
                    } else {
                        // 无载荷：payload_nid 是 unit
                        self.add(arm.payload_nid, arm.label_span, "prim", "无载荷=unit".into(),
                            Con::ExpectPrim { node: arm.payload_nid, prim: crate::constraints::Prim::Unit,
                                span: arm.label_span });
                    }
                    // 2) scrutinee 必须含该标签，其载荷类型与绑定变量一致
                    self.add(scrutinee.nid(), arm.label_span, "variant",
                        format!("匹配标签 `{}", arm.label),
                        Con::MkVariant { node: scrutinee.nid(), label: arm.label.clone(),
                            payload: arm.payload_nid, open: true, span: arm.label_span });
                    // 3) 生成 body（此时 payload 已在环境中）
                    self.gen(&arm.body);
                    if arm.payload.is_some() { self.pop(); }
                    self.add(arm.body.nid(), arm.body.span(), "eq",
                        format!("match 结果 = `{} 分支", arm.label),
                        Con::Eq { a: *nid, b: arm.body.nid(), pol: Polarity::ExpectedActual,
                            label: format!("结果 = `{} 分支", arm.label), span: arm.body.span() });
                    self.add(arm.payload_nid, arm.label_span, "scope_end",
                        format!("离开 `{} 分支", arm.label),
                        Con::ScopeEnd { level_down: true, span: arm.label_span });
                }
            }

            Expr::Let { nid, span, rec_, name, name_span, value, body } => {
                if *rec_ {
                    self.add(*nid, *name_span, "scope", format!("rec 锚点 {}", name),
                        Con::RecBegin { name: name.clone(), anchor: value.nid(), span: *name_span });
                    self.push(ScopeSlot { kind: 4, name_h: Self::name_h(name),
                        node_fp: self.fps[&value.nid()], gen_h: Self::gen_h(BindKind::Rec, is_value(value)) });
                    self.gen(value);
                    self.pop();
                    self.add(*nid, *span, "rec_end", format!("泛化(rec) {}", name),
                        Con::RecEnd { name: name.clone(), anchor: value.nid(),
                            is_value: is_value(value), span: *span });
                    if let Some(b) = body {
                        self.gen(b);
                        self.add(b.nid(), b.span(), "eq", format!("let rec {} 的结果", name),
                            Con::Eq { a: *nid, b: b.nid(), pol: Polarity::Equal,
                                label: format!("let rec {} 结果=续体", name), span: b.span() });
                    }
                } else {
                    self.gen(value);
                    self.add(*nid, *name_span, "gen", format!("泛化绑定 {}", name),
                        Con::GenBind { name: name.clone(), value: value.nid(),
                            is_value: is_value(value), span: *name_span });
                    let bk = BindKind::LetPoly; // 单/多态由执行时决定，键中仍用值标志近似
                    self.push(ScopeSlot { kind: 3, name_h: Self::name_h(name),
                        node_fp: self.fps[&value.nid()], gen_h: Self::gen_h(bk, is_value(value)) });
                    if let Some(b) = body {
                        self.gen(b);
                        self.add(b.nid(), b.span(), "eq", format!("let {} 的结果", name),
                            Con::Eq { a: *nid, b: b.nid(), pol: Polarity::Equal,
                                label: format!("let {} 结果=续体", name), span: b.span() });
                    }
                    self.pop();
                }
            }
        }
    }
}

fn con_nodes(c: &Con) -> Vec<NodeId> {
    match c {
        Con::ExpectPrim { node, .. } => vec![*node],
        Con::MkArrow { node, arg, ret, .. } => vec![*node, *arg, *ret],
        Con::MkRecord { node, fields, .. } => {
            let mut v = vec![*node];
            for (_, n, _) in fields { v.push(*n); }
            v
        }
        Con::MkVariant { node, payload, .. } => vec![*node, *payload],
        Con::Eq { a, b, .. } => vec![*a, *b],
        Con::VarUse { node, .. } => vec![*node],
        Con::ScopeBegin { .. } => vec![],
        Con::ScopeEnd { .. } => vec![],
        Con::BindLocal { node, .. } => vec![*node],
        Con::GenBind { value, .. } => vec![*value],
        Con::RecBegin { anchor, .. } | Con::RecEnd { anchor, .. } => vec![*anchor],
        Con::Fail { node, .. } => vec![*node],
    }
}

// ---------- 公共结果 ----------

#[derive(Clone, Debug, Serialize)]
pub struct TypeView {
    pub repr: String,
    pub kind: String,
    pub cid: Option<u32>,
    pub node: Option<u32>,
    pub children: Vec<TypeView>,
    pub field: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ErrorView {
    pub index: usize,
    pub kind: String,
    pub cid: u32,
    pub message: String,
    pub span: [usize; 2],
    pub expected: Option<String>,
    pub actual: Option<String>,
    pub missing: Vec<String>,
    pub extra: Vec<String>,
    pub core: Vec<u32>,
    pub candidates: Vec<Vec<u32>>,
}

#[derive(Clone, Debug, Serialize)]
pub struct StepView {
    pub index: usize,
    pub kind: String,
    pub cid: u32,
    pub ok: bool,
    pub title: String,
    pub detail: String,
    pub left: Option<String>,
    pub right: Option<String>,
    pub result: Option<String>,
    pub span: [usize; 2],
}

#[derive(Clone, Debug, Serialize)]
pub struct ConstraintView {
    pub id: u32,
    pub kind: String,
    pub label: String,
    pub node: u32,
    pub key: String,
    pub toggleable: bool,
    pub span: [usize; 2],
}

#[derive(Clone, Debug, Serialize)]
pub struct AnalysisResult {
    pub parse_errors: Vec<ParseError>,
    pub nodes: Vec<NodeInfo>,
    pub node_types: Vec<Option<TypeView>>,
    pub root_type: Option<TypeView>,
    pub root_type_str: String,
    pub constraints: Vec<ConstraintView>,
    pub steps: Vec<StepView>,
    pub errors: Vec<ErrorView>,
    pub ok: bool,
}

/// 把求解器中的类型构建成带来源的视图：每个片段指向“引入它的约束”和节点。
fn build_type_view(t: &RType, solver: &Solver) -> TypeView {
    let (kind, repr, cid, node, children, field) = match t {
        RType::Int => ("int".to_string(), "int".into(), None, None, vec![], None),
        RType::Bool => ("bool".to_string(), "bool".into(), None, None, vec![], None),
        RType::Unit => ("unit".to_string(), "unit".into(), None, None, vec![], None),
        RType::Var(tv) => {
            let root = solver.find_t_pub(*tv);
            let o = solver.origin_of(root);
            ("var".into(), solver.pub_show(t), Some(o.introduced.0), Some(o.node.0), vec![], None)
        }
        RType::Arrow(a, b) => {
            let mut ch = vec![build_type_view(a, solver)];
            ch.push(build_type_view(b, solver));
            let s = solver.pub_show(t);
            ("arrow".into(), s, None, None, ch, None)
        }
        RType::Record(row) => {
            let mut ch = vec![];
            let fs: Vec<(&String, &RType)> = match row {
                Row::Closed(fs) | Row::Open(fs, _) => fs.iter().collect(),
            };
            for (k, v) in fs {
                let mut tv = build_type_view(v, solver);
                tv.field = Some(k.clone());
                ch.push(tv);
            }
            ("record".into(), solver.pub_show(t), None, None, ch, None)
        }
        RType::Variant(row) => {
            let mut ch = vec![];
            let fs: Vec<(&String, &RType)> = match row {
                Row::Closed(fs) | Row::Open(fs, _) => fs.iter().collect(),
            };
            for (k, v) in fs {
                let mut tv = build_type_view(v, solver);
                tv.field = Some(k.clone());
                ch.push(tv);
            }
            ("variant".into(), solver.pub_show(t), None, None, ch, None)
        }
    };
    TypeView { repr, kind, cid, node, children, field }
}

/// 完整分析（含禁用集合），返回结果与原始对象（供核心搜索复用）。
pub struct Analysis {
    pub result: AnalysisResult,
    pub solver: Solver,
    pub cons: Vec<Constraint>,
    pub node_tvs: Vec<Option<TV>>,
    pub node_rvs: Vec<Option<RV>>,
    pub root_nid: NodeId,
}

pub fn analyze_source(src: &str, disabled: &BTreeSet<CId>) -> Analysis {
    let program = parse(src);
    analyze_program(&program, disabled)
}

pub fn analyze_program(program: &Program, disabled: &BTreeSet<CId>) -> Analysis {
    let node_count = program.nodes.len();
    // 结构指纹（表达式节点）
    let mut fps: BTreeMap<NodeId, u64> = BTreeMap::new();
    fill_fps(&program.root, &mut fps);
    // 合成节点（lambda 参数、match 载荷）不在树里：单独计算稳定指纹。
    fill_synthetic_fps(&program, &mut fps);

    let mut gcx = GenCx { cons: vec![], fps: fps.clone(), scope: vec![], cid_seq: 0 };
    gcx.gen(&program.root);
    let cons = gcx.cons;

    let mut solver = Solver::new(node_count);
    for ni in &program.nodes { solver.set_span(ni.id, ni.span); }
    for d in disabled { solver.disabled.insert(*d); }
    let mut nt = Solver::node_table(node_count);
    solver.run_constraints(&cons, &mut nt);

    // 结果类型
    let root_nid = program.root.nid();
    let mut node_types = vec![None; node_count];
    for (i, slot) in nt.map.iter().enumerate() {
        if let Some(tv) = slot {
            let frozen = solver.freeze_t(*tv, &mut Default::default());
            let view = build_type_view(&frozen, &solver);
            node_types[i] = Some(view);
        }
    }
    let (root_type, root_str) = match nt.map.get(root_nid.0 as usize).copied().flatten() {
        Some(tv) => {
            let f = solver.freeze_t(tv, &mut Default::default());
            let s = solver.show(f.clone());
            (Some(build_type_view(&f, &solver)), s)
        }
        None => (None, String::new()),
    };

    // 约束视图
    let cviews: Vec<ConstraintView> = cons.iter().map(|c| ConstraintView {
        id: c.id.0, kind: c.kind.clone(), label: c.label.clone(), node: c.node.0,
        key: format!("{:032x}", c.key), toggleable: c.toggleable,
        span: [c.span.start, c.span.end],
    }).collect();

    // 步骤视图
    let step_views: Vec<StepView> = solver.steps.iter().enumerate().map(|(i, st)| {
        let (kind, cid, ok, title, detail, l, r, res, span):
            (&str, u32, bool, String, String, Option<String>, Option<String>, Option<String>, [usize;2]) = match st {
            Step::Unify { cid, left, right, result, ok, detail } =>
                ("unify", cid.0, *ok, format!("统一 {} ~ {}", left, right), detail.clone(),
                 Some(left.clone()), Some(right.clone()), Some(result.clone()), span_of_cid(&cons, *cid)),
            Step::Instantiate { cid, name, scheme, instance } =>
                ("instantiate", cid.0, true, format!("实例化 {} : {}", name, scheme),
                 "多态使用：复制方案中的绑定变量".into(), Some(scheme.clone()), None, Some(instance.clone()),
                 span_of_cid(&cons, *cid)),
            Step::Generalize { cid, name, scheme, vars, rvars, restricted } =>
                ("generalize", cid.0, true, format!("泛化 {} => {}", name, scheme),
                 if *restricted { "value restriction：非值绑定，保持单态".into() }
                 else { format!("泛化 {} 个类型变量、{} 个行变量", vars, rvars) },
                 Some(scheme.clone()), None, None, span_of_cid(&cons, *cid)),
            Step::RecBind { cid, name, ty } =>
                ("rec_bind", cid.0, true, format!("递归锚点 {} : {}", name, ty),
                 "为递归绑定建立单态类型".into(), Some(ty.clone()), None, None, span_of_cid(&cons, *cid)),
            Step::Scope { cid, open, label } =>
                ("scope", cid.0, true,
                 if *open { format!("进入作用域 {}", label) } else { "离开作用域".into() },
                 "词法层级 / 泛化边界".into(), None, None, None, span_of_cid(&cons, *cid)),
            Step::Fail { cid, message } =>
                ("fail", cid.0, false, message.clone(), "显式失败约束".into(), None, None, None,
                 span_of_cid(&cons, *cid)),
            Step::Skip { cid, label } =>
                ("skip", cid.0, true, format!("已禁用：{}", label), "用户临时禁用".into(),
                 None, None, None, span_of_cid(&cons, *cid)),
        };
        StepView { index: i, kind: kind.into(), cid, ok, title, detail, left: l, right: r,
            result: res, span }
    }).collect();

    // 错误与核心
    let mut error_views = vec![];
    // 去重：同 cid+message 只保留一个（求解可能产生重复报告）
    let mut seen_sig = BTreeSet::new();
    let mut err_list: Vec<TypeError> = vec![];
    for e in &solver.errors {
        let sig = (e.cid, e.message.clone());
        if seen_sig.insert(sig) { err_list.push(e.clone()); }
    }
    for (idx, e) in err_list.iter().enumerate() {
        let span = cons.iter().find(|c| c.id == e.cid)
            .map(|c| [c.span.start, c.span.end])
            .unwrap_or([0, 0]);
        let (core, candidates) = find_min_cores(&program, &cons, disabled, e, &err_list);
        error_views.push(ErrorView {
            index: idx,
            kind: format!("{:?}", e.kind).to_lowercase(),
            cid: e.cid.0,
            message: e.message.clone(),
            span,
            expected: { let mut s = solver.clone(); e.expected.as_ref().map(|t| s.show(t.clone())) },
            actual: { let mut s = solver.clone(); e.actual.as_ref().map(|t| s.show(t.clone())) },
            missing: e.missing.clone(),
            extra: e.extra.clone(),
            core: core.iter().map(|c| c.0).collect(),
            candidates: candidates.iter().map(|v| v.iter().map(|c| c.0).collect()).collect(),
        });
    }

    let parse_errors = program.errors.clone();
    let ok = parse_errors.is_empty() && error_views.is_empty();
    let result = AnalysisResult {
        parse_errors,
        nodes: program.nodes.clone(),
        node_types,
        root_type,
        root_type_str: root_str,
        constraints: cviews,
        steps: step_views,
        errors: error_views,
        ok,
    };
    Analysis {
        result, solver, cons,
        node_tvs: nt.map.iter().map(|o| *o).collect(),
        node_rvs: nt.rows.iter().map(|o| *o).collect(),
        root_nid,
    }
}

fn span_of_cid(cons: &[Constraint], cid: CId) -> [usize; 2] {
    cons.iter().find(|c| c.id == cid).map(|c| [c.span.start, c.span.end]).unwrap_or([0, 0])
}

fn fill_fps(e: &Expr, out: &mut BTreeMap<NodeId, u64>) {
    out.insert(e.nid(), fingerprint(e));
    let mut kids = vec![];
    e.children(&mut kids);
    for k in kids { fill_fps(k, out); }
}

fn fill_synthetic_fps(p: &Program, out: &mut BTreeMap<NodeId, u64>) {
    fn walk(e: &Expr, out: &mut BTreeMap<NodeId, u64>) {
        if let Expr::Lam { param_nid, param, body, .. } = e {
            if !out.contains_key(param_nid) {
                let mut f = Fnv::new();
                f.byte(kind_byte(NodeKind::Var));
                f.str(&format!("lam-param:{}", param));
                // 与函数体结构相关，避免不同 lambda 的参数撞指纹
                f.u(fingerprint(body));
                out.insert(*param_nid, f.lo ^ f.hi);
            }
        }
        if let Expr::Match { arms, scrutinee, .. } = e {
            for arm in arms {
                if !out.contains_key(&arm.payload_nid) {
                    let mut f = Fnv::new();
                    f.byte(kind_byte(arm.payload.as_ref().map(|_| NodeKind::Var).unwrap_or(NodeKind::Unit)));
                    f.str(&format!("arm-payload:{}:{}", arm.label,
                                   arm.payload.clone().unwrap_or_default()));
                    f.u(fingerprint(scrutinee));
                    out.insert(arm.payload_nid, f.lo ^ f.hi);
                }
            }
        }
        let mut kids = vec![];
        e.children(&mut kids);
        for k in kids { walk(k, out); }
    }
    walk(&p.root, out);
}

// ---------- 最小不可满足核心 ----------

fn error_signature(e: &TypeError) -> (u8, CId, Vec<String>, Vec<String>, Option<String>) {
    let k = match e.kind {
        TypeErrorKind::TypeMismatch => 1,
        TypeErrorKind::InfiniteType => 2,
        TypeErrorKind::RowMismatch => 3,
        TypeErrorKind::UnboundVariable => 4,
        TypeErrorKind::DuplicateField => 5,
        TypeErrorKind::ArityShape => 6,
    };
    let mut m = e.missing.clone(); m.sort();
    let mut x = e.extra.clone(); x.sort();
    (k, e.cid, m, x, e.var_name.clone())
}

fn toggle_active(c: &Constraint, user_disabled: &BTreeSet<CId>) -> bool {
    c.toggleable && !user_disabled.contains(&c.id)
}

fn run_with_active(program: &Program, cons: &[Constraint],
                   active: &BTreeSet<CId>, user_disabled: &BTreeSet<CId>) -> Vec<TypeError> {
    let node_count = program.nodes.len();
    let mut solver = Solver::new(node_count);
    for ni in &program.nodes { solver.set_span(ni.id, ni.span); }
    for c in cons {
        if user_disabled.contains(&c.id) { solver.disabled.insert(c.id); continue; }
        if c.toggleable && !active.contains(&c.id) { solver.disabled.insert(c.id); }
    }
    let mut nt = Solver::node_table(node_count);
    solver.run_constraints(cons, &mut nt);
    let mut out = vec![];
    let mut seen = BTreeSet::new();
    for e in solver.errors {
        let sig = (error_signature(&e).0, error_signature(&e).1);
        if seen.insert(sig) { out.push(e); }
    }
    out
}

/// 一条约束触及哪些节点（数据流意义）。
fn touched_nodes(c: &Constraint) -> Vec<NodeId> { con_nodes(&c.con) }

/// 反向可达：从失败约束的节点集合出发，沿约束顺序反向收集真正参与推导的约束。
fn relevant_constraints(cons: &[Constraint], fail_cid: CId) -> Vec<CId> {
    let mut involved: BTreeSet<NodeId> = BTreeSet::new();
    let mut relevant: BTreeSet<CId> = BTreeSet::new();
    let fail_idx = cons.iter().position(|c| c.id == fail_cid);
    let mut idx = match fail_idx { Some(i) => i as isize, None => return vec![] };
    // 先把失败约束本身纳入
    if let Some(fc) = cons.get(idx as usize) {
        for n in touched_nodes(fc) { involved.insert(n); }
        relevant.insert(fc.id);
    }
    idx -= 1;
    while idx >= 0 {
        let c = &cons[idx as usize];
        let ts = touched_nodes(c);
        let hits = ts.iter().any(|n| involved.contains(n));
        if hits {
            // 绑定/作用域等非切换约束也参与数据流，保证可达性闭合；
            // 枚举核心时再只保留 toggleable。
            relevant.insert(c.id);
            for n in ts { involved.insert(n); }
        }
        idx -= 1;
    }
    let mut v: Vec<CId> = relevant.into_iter().collect();
    v.sort();
    v
}

fn find_min_cores(
    program: &Program,
    cons: &[Constraint],
    user_disabled: &BTreeSet<CId>,
    target: &TypeError,
    _all_errors: &[TypeError],
) -> (Vec<CId>, Vec<Vec<CId>>) {
    // 对单条约束即可造成的错误（未绑定变量、重复字段），核心就是它自己。
    let direct = matches!(target.kind,
        TypeErrorKind::UnboundVariable | TypeErrorKind::DuplicateField | TypeErrorKind::ArityShape);
    if direct {
        return (vec![target.cid], vec![vec![target.cid]]);
    }

    let mut relevant: Vec<CId> = relevant_constraints(cons, target.cid)
        .into_iter()
        .filter(|c| cons.iter().any(|x| x.id == *c && !user_disabled.contains(&x.id)))
        .collect();
    relevant.sort();

    // 非切换约束（scope/bind/generalize 标记）始终执行；
    // active 只描述保留的“可切换”约束。
    let mut base_active: BTreeSet<CId> = cons.iter()
        .filter(|c| !c.toggleable && !user_disabled.contains(&c.id))
        .map(|c| c.id)
        .collect();

    let causes_target = |extra: &BTreeSet<CId>| -> bool {
        let mut active = base_active.clone();
        active.extend(extra.iter().copied());
        run_with_active(program, cons, &active, user_disabled)
            .iter().any(|e| error_signature(e) == error_signature(target))
    };

    // 全集必须触发；若反向可达遗漏（理论上不会），退回所有 toggleable。
    let full_toggle: BTreeSet<CId> = toggle_relevant.iter().copied().collect();
    if !causes_target(&full_toggle) {
        let all: BTreeSet<CId> = cons.iter()
            .filter(|c| toggle_active(c, user_disabled)).map(|c| c.id).collect();
        let mut v: Vec<CId> = all.into_iter().collect();
        v.sort();
        return (v.clone(), vec![v]);
    }

    let cap = 16usize;
    if relevant.len() <= cap {
        let n = relevant.len();
        for size in 1..=n {
            if std::env::var("CORE_DEBUG").is_ok() && size <= 5 {
                let mut dbg: Vec<Vec<usize>> = vec![];
                gen_combos(n, size, 0, &mut vec![], &mut dbg);
                let hits = dbg.iter().filter(|combo| {
                    let set: BTreeSet<CId> = combo.iter().map(|&i| relevant[i]).collect();
                    causes_target(&set)
                }).count();
                eprintln!("[core] size {} hit {}/{}", size, hits, dbg.len());
            }
            let mut combos: Vec<Vec<usize>> = vec![];
            gen_combos(n, size, 0, &mut vec![], &mut combos);
            let mut found: Vec<Vec<CId>> = vec![];
            for combo in combos {
                let set: BTreeSet<CId> = combo.iter().map(|&i| relevant[i]).collect();
                if causes_target(&set) {
                    found.push(set.into_iter().collect());
                }
            }
            if !found.is_empty() {
                found.sort();
                let primary = found[0].clone();
                return (primary, found);
            }
        }
    }

    // 大规模兜底：贪心收缩
    let mut trial: BTreeSet<CId> = toggle_relevant.iter().copied().collect();
    for c in &toggle_relevant {
        let mut t = trial.clone();
        t.remove(c);
        if causes_target(&t) { trial = t; }
    }
    let mut v: Vec<CId> = trial.into_iter().collect();
    v.sort();
    (v.clone(), vec![v])
}

fn gen_combos(n: usize, k: usize, start: usize, cur: &mut Vec<usize>, out: &mut Vec<Vec<usize>>) {
    if cur.len() == k { out.push(cur.clone()); return; }
    for i in start..n {
        cur.push(i);
        gen_combos(n, k, i + 1, cur, out);
        cur.pop();
    }
}
