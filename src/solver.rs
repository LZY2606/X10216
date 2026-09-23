// 约束求解器：执行事件流，记录逐步统一步骤，收集结构化错误，
// 计算保持不可满足的极小（包含极小）错误核心并保留多个候选。
use crate::constraints::Event;
use crate::types::{RowFlavor, RowTail, RowType, Type, UnionFind, VarOrigin};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};
use std::cell::Cell;
thread_local! { static UD: Cell<u32> = const { Cell::new(0) }; }

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ErrorKind {
    Mismatch { expected: String, actual: String },
    Occurs { var: String, in_type: String },
    Arity { expected: String, actual: String },
    /// row 字段问题：缺字段与多余字段分开
    MissingFields { fields: Vec<String>, flavor: RowFlavor },
    ExtraFields { fields: Vec<String>, flavor: RowFlavor },
}

#[derive(Debug, Clone, Serialize)]
pub struct UnificationError {
    pub kind: ErrorKind,
    pub message: String,
    /// 错误涉及的源码范围（多个约束共同造成 -> 多个节点）
    pub nids: Vec<u32>,
    /// 导致该错误的约束 key 集合（后续做最小化）
    pub involved: Vec<String>,
    /// 直接触发的约束
    pub trigger: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct CoreCandidate {
    pub members: Vec<String>,
    pub size: usize,
    /// 触发该核心时首个失败的约束
    pub trigger: String,
    pub message: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct ErrorReport {
    pub error: UnificationError,
    /// 全部同样最小的候选核心
    pub cores: Vec<CoreCandidate>,
    pub primary_core: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct UnifyLog {
    pub a: String,
    pub b: String,
    pub root: String,
    pub detail: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct StepRecord {
    pub index: usize,
    pub event_key: String,
    pub event_cid: Option<String>,
    pub kind: String,
    pub title: String,
    pub detail: String,
    pub nid: Option<u32>,
    pub unifications: Vec<UnifyLog>,
    pub error: Option<String>,
    /// 执行到这一步后关键类型的快照（变量 -> 渲染类型）
    pub snapshot: BTreeMap<String, String>,
    /// 到这一步仍“活跃”的统一约束 key（用于增量：re-solved 判定）
    pub live_constraints: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct NodeTypeInfo {
    pub nid: u32,
    pub rendered: String,
    /// 类型各部分来源：渲染片段 -> 表达式 nid（最终类型可追溯）
    pub parts: Vec<TypePart>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TypePart {
    pub text: String,
    pub nid: u32,
}

#[derive(Debug, Clone)]
struct Snapshot {
    uf: UnionFind,
    /// 根变量 -> 结构项（仅在根上有效）
    terms: BTreeMap<String, Type>,
    /// 变量被并入时的证据链：根变量历史 -> (并入变量, 约束key, nid)
    evidence: BTreeMap<String, Vec<Evidence>>,
    schemes: BTreeMap<String, (Vec<String>, Type)>,
}

#[derive(Debug, Clone)]
struct Evidence {
    merged: String,
    ckey: String,
    nid: u32,
}

#[derive(Debug, Clone)]
pub struct Solver {
    uf: UnionFind,
    terms: BTreeMap<String, Type>,
    evidence: BTreeMap<String, Vec<Evidence>>,
    schemes: BTreeMap<String, (Vec<String>, Type)>,
    pub steps: Vec<StepRecord>,
    pub errors: Vec<ErrorReport>,
    /// 禁用的约束 key
    disabled: BTreeSet<String>,
    /// 每个统一约束是否成功执行（含被跳过）
    constraint_status: BTreeMap<String, ConstraintStatus>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ConstraintStatus {
    Solved,
    Failed,
    Disabled,
}

#[derive(Debug, Clone, Serialize)]
pub struct SolveResult {
    pub steps: Vec<StepRecord>,
    pub errors: Vec<ErrorReport>,
    pub node_types: Vec<NodeTypeInfo>,
    pub statuses: Vec<ConstraintStatusItem>,
    pub ok: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstraintStatusItem {
    pub key: String,
    pub cid: String,
    pub status: ConstraintStatus,
}

impl Solver {
    pub fn new() -> Self {
        Solver {
            uf: UnionFind::new(),
            terms: BTreeMap::new(),
            evidence: BTreeMap::new(),
            schemes: BTreeMap::new(),
            steps: Vec::new(),
            errors: Vec::new(),
            disabled: BTreeSet::new(),
            constraint_status: BTreeMap::new(),
        }
    }

    pub fn solve(events: &[Event], disabled: &BTreeSet<String>) -> SolveResult {
        let mut s = Solver::new();
        s.disabled = disabled.clone();
        s.run(events);
        s.build_result(events)
    }

    fn snapshot(&self) -> Snapshot {
        Snapshot {
            uf: self.uf.clone(),
            terms: self.terms.clone(),
            evidence: self.evidence.clone(),
            schemes: self.schemes.clone(),
        }
    }
    fn restore(&mut self, snap: Snapshot) {
        self.uf = snap.uf;
        self.terms = snap.terms;
        self.evidence = snap.evidence;
        self.schemes = snap.schemes;
    }

    fn node_type_vars(events: &[Event]) -> BTreeMap<u32, String> {
        // 每个表达式节点对应的结果变量（在 Declare purpose 中带特定角色）
        let mut map = BTreeMap::new();
        for e in events {
            if let Event::Declare { var, nid, purpose, .. } = e {
                if purpose == "app_result"
                    || purpose == "field_result"
                    || purpose == "match_result"
                    || purpose == "top"
                    || purpose == "param"
                    || purpose == "rec_self"
                    || purpose == "let_carry"
                    || purpose.starts_with("lit_")
                    || purpose == "unbound"
                {
                    map.entry(*nid).or_insert_with(|| var.clone());
                }
            }
        }
        map
    }

    fn run(&mut self, events: &[Event]) {
        for (idx, ev) in events.iter().enumerate() {
            match ev {
                Event::Declare { var, nid, origin, .. } => {
                    self.uf.make(var, *nid, *origin, 0);
                    self.step_declare(idx, ev);
                }
                Event::Define { var, term, .. } => {
                    self.do_define(var, term.clone());
                    self.step_simple(idx, ev, "定义", format!("{} := {}", var, term.render()));
                }
                Event::Instantiate { scheme_var, copies, result, .. } => {
                    for (q, t) in copies {
                        let fv = if let Type::Var(f) = t { f.clone() } else { continue };
                        let _ = q;
                        // 副本变量已在 Declare 中建立；记录替换方案（无额外结构）
                        self.uf.make(&fv, ev.nid(), VarOrigin::Inst, 0);
                    }
                    self.step_simple(
                        idx,
                        ev,
                        "实例化",
                        format!("{scheme_var} 的实例类型 {}", result.render()),
                    );
                }
                Event::Generalize { target, vars, is_value, reason, .. } => {
                    let quant = if *is_value {
                        self.compute_quantified(target, vars)
                    } else {
                        vec![]
                    };
                    let body = self.resolved_of_var(target);
                    self.schemes.insert(target.clone(), (quant.clone(), body.clone()));
                    // 绑定名 -> 方案：key 用 target，后续 Instantiate 在事件流中已生成副本，
                    // 这里只需让 body 解析正常即可，无需被引用（实例化时生成新变量结构）。
                    let title = if *is_value { "泛化" } else { "泛化被拒绝" };
                    let detail = if *is_value {
                        format!(
                            "量化 {} 个变量：{}；方案 ∀{}. {}",
                            quant.len(),
                            quant.join(" "),
                            if quant.is_empty() { "".into() } else { quant.join(" ") + ". " },
                            body.render()
                        )
                    } else {
                        format!("{reason}；{target} 保持单态，类型 {}", body.render())
                    };
                    self.step_simple(idx, ev, title, detail);
                }
                Event::Equate { key, cid, left, right, .. } => {
                    if self.disabled.contains(key) {
                        self.constraint_status
                            .insert(key.clone(), ConstraintStatus::Disabled);
                        self.step_simple(
                            idx,
                            ev,
                            "约束已禁用",
                            format!("{cid} 被用户临时禁用，跳过"),
                        );
                        continue;
                    }
                    let snap = self.snapshot();
                    let mut logs = Vec::new();
                    UD.with(|c| c.set(0));
                    match self.unify(left, right, key, ev.nid(), &mut logs) {
                        Ok(()) => {
                            self.constraint_status
                                .insert(key.clone(), ConstraintStatus::Solved);
                            self.step_equate(idx, ev, None, logs);
                        }
                        Err(kind) => {
                            // 回滚该约束造成的部分合并，保证后续错误可独立收集
                            self.restore(snap);
                            self.constraint_status
                                .insert(key.clone(), ConstraintStatus::Failed);
                            let involved = self.collect_involved(left, right, key);
                            let nids = involved_nids(events, &involved);
                            let message = describe_error(&kind, ev);
                            let err = UnificationError {
                                kind: kind.clone(),
                                message: message.clone(),
                                nids,
                                involved: involved.clone(),
                                trigger: key.clone(),
                            };
                            let cores = self.minimize_cores(events, key, &err);
                            let primary = cores
                                .first()
                                .map(|c| c.members.clone())
                                .unwrap_or_else(|| vec![key.clone()]);
                            self.errors.push(ErrorReport {
                                error: err,
                                cores,
                                primary_core: primary,
                            });
                            self.step_equate(idx, ev, Some(message), logs);
                        }
                    }
                }
            }
        }
    }

    fn do_define(&mut self, var: &str, term: Type) {
        let root = self.uf.root_of(var);
        self.terms.insert(root, term);
    }

    // 计算可泛化变量：候选中与 target 同根或在其结构中可达、
    // 且未逃逸到外层（level 较浅）的变量。事件流模式下所有候选都在
    // 当前 let 层声明，这里再依据并查集连通性确认可达。
    fn compute_quantified(&mut self, target: &str, candidates: &[String]) -> Vec<String> {
        let root = self.uf.root_of(target);
        let mut reachable = BTreeSet::new();
        let body = self.deep_resolve(&Type::var(root.clone()));
        let mut stack = vec![body];
        while let Some(t) = stack.pop() {
            match t {
                Type::Var(v) => {
                    if reachable.insert(v.clone()) {
                        if let Some(term) = self.terms.get(&v).cloned() {
                            stack.push(term);
                        }
                    }
                }
                Type::Arrow(a, b) => {
                    stack.push(*a);
                    stack.push(*b);
                }
                Type::Row(r) => {
                    for (_, ty) in r.fields {
                        stack.push(ty);
                    }
                }
                Type::Con { .. } => {}
            }
        }
        let cand: BTreeSet<&str> = candidates.iter().map(|s| s.as_str()).collect();
        reachable
            .into_iter()
            .filter(|v| cand.contains(v.as_str()) || self.root_of_owned(v) == root && v != &root)
            .collect()
    }

    fn root_of_owned(&self, v: &str) -> String {
        self.uf.root_of(v)
    }
}

impl Solver {
    fn resolved_of_var(&self, v: &str) -> Type {
        let r = self.uf.root_of(v);
        self.terms
            .get(&r)
            .cloned()
            .map(|t| self.deep_resolve(&t))
            .unwrap_or_else(|| Type::var(r))
    }

    // 把结构项解析为具体类型（变量跟到根并展开根项），带 occurs 保护。
    fn deep_resolve(&self, t: &Type) -> Type {
        self.deep_resolve_seen(t, &mut BTreeSet::new())
    }
    fn deep_resolve_seen(&self, t: &Type, seen: &mut BTreeSet<String>) -> Type {
        if seen.len() > 60 { eprintln!("RESOLVE-LOOP at {} seen={}", t.render(), seen.len()); std::process::exit(3);}
        match t {
            Type::Var(v) => {
                let r = self.uf.root_of(v);
                let first = seen.insert(r.clone());
                if first {
                    if let Some(term) = self.terms.get(&r).cloned() {
                        eprintln!("resolve {} -> term {}", r, term.render());
                        let resolved = self.deep_resolve_seen(&term, seen);
                        return match &resolved {
                            // 解析结果又回到自己（自指），呈现为变量以截断
                            Type::Var(rr) if rr == &r => Type::var(r),
                            _ => resolved,
                        };
                    }
                }
                Type::var(r)
            }
            Type::Con { .. } => t.clone(),
            Type::Arrow(a, b) => Type::arrow(
                self.deep_resolve_seen(a, seen),
                self.deep_resolve_seen(b, seen),
            ),
            Type::Row(r) => {
                let fields = r
                    .fields
                    .iter()
                    .map(|(l, ty)| (l.clone(), self.deep_resolve_seen(ty, seen)))
                    .collect();
                let tail = match &r.tail {
                    RowTail::Empty => RowTail::Empty,
                    RowTail::Open { var } => RowTail::Open {
                        var: self.uf.root_of(var),
                    },
                };
                Type::Row(RowType { flavor: r.flavor, fields, tail })
            }
        }
    }

    fn term_of(&self, root: &str) -> Option<Type> {
        self.terms.get(root).cloned()
    }

    // 统一入口。成功返回时所有合并已落盘；失败时调用方负责回滚。
    fn unify(
        &mut self,
        a: &Type,
        b: &Type,
        ckey: &str,
        nid: u32,
        logs: &mut Vec<UnifyLog>,
    ) -> Result<(), ErrorKind> {
        UD.with(|c| { let n=c.get()+1; c.set(n); if n>50 { eprintln!("UDEPTH {} {} ~~ {}", n, a.render(), b.render()); std::process::exit(4);} });
        let ar = self.uf.root_of_var_type(a);
        let br = self.uf.root_of_var_type(b);
        match (&ar, &br) {
            (Type::Var(x), Type::Var(y)) if x == y => Ok(()),
            (Type::Var(x), _) => self.bind_var(x.clone(), br.clone(), ckey, nid, logs),
            (_, Type::Var(y)) => self.bind_var(y.clone(), ar.clone(), ckey, nid, logs),
            (Type::Con { name: n1 }, Type::Con { name: n2 }) => {
                if n1 == n2 {
                    Ok(())
                } else {
                    Err(ErrorKind::Mismatch {
                        expected: n1.clone(),
                        actual: n2.clone(),
                    })
                }
            }
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                self.unify(a1, b1, ckey, nid, logs)?;
                self.unify(a2, b2, ckey, nid, logs)
            }
            (Type::Row(r1), Type::Row(r2)) => {
                if r1.flavor != r2.flavor {
                    return Err(ErrorKind::Mismatch {
                        expected: flavor_name(r1.flavor).into(),
                        actual: flavor_name(r2.flavor).into(),
                    });
                }
                self.unify_rows(r1, r2, ckey, nid, logs)
            }
            _ => Err(ErrorKind::Mismatch {
                expected: type_head_name(&ar),
                actual: type_head_name(&br),
            }),
        }
    }

    fn bind_var(
        &mut self,
        var: String,
        term: Type,
        ckey: &str,
        nid: u32,
        logs: &mut Vec<UnifyLog>,
    ) -> Result<(), ErrorKind> {
        let root = self.uf.root_of(&var);
        match &term {
            Type::Var(other) => {
                let other_root = self.uf.root_of(other);
                if other_root == root {
                    return Ok(());
                }
                let t1 = self.terms.remove(&root);
                let t2 = self.terms.remove(&other_root);
                let merged = self.uf.union(&root, &other_root);
                self.push_evidence(merged.clone(), root.clone(), ckey, nid);
                self.push_evidence(merged.clone(), other_root.clone(), ckey, nid);
                logs.push(UnifyLog {
                    a: root,
                    b: other_root,
                    root: merged.clone(),
                    detail: "变量合并".into(),
                });
                // 先把可能存在的结构项挂到合并根上，避免以裸变量递归
                let (a, b) = match (t1, t2) {
                    (Some(a), Some(b)) => (Some(a), Some(b)),
                    (Some(t), None) => {
                        self.terms.insert(merged.clone(), t);
                        (None, None)
                    }
                    (None, Some(t)) => {
                        self.terms.insert(merged.clone(), t);
                        (None, None)
                    }
                    (None, None) => (None, None),
                };
                if let (Some(a), Some(b)) = (a, b) {
                    // 合并根先持有 a，再把 b 作为结构与之统一
                    self.terms.insert(merged.clone(), a);
                    let root_ty = Type::var(merged);
                    self.unify(&root_ty, &b, ckey, nid, logs)
                } else {
                    Ok(())
                }
            }
            structure => {
                // 根已有结构项：保留为占位项，与新结构做结构-结构统一
                if let Some(existing) = self.terms.get(&root).cloned() {
                    self.unify_terms(&existing, structure, ckey, nid, logs)
                } else {
                    // occurs-check 在插入前进行，避免自指环
                    if occurs_in(&root, structure, &self.uf) {
                        return Err(ErrorKind::Occurs {
                            var: root.clone(),
                            in_type: structure.render(),
                        });
                    }
                    self.terms.insert(root.clone(), structure.clone());
                    logs.push(UnifyLog {
                        a: root.clone(),
                        b: structure.render(),
                        root,
                        detail: "变量绑定到结构".into(),
                    });
                    Ok(())
                }
            }
        }
    }

    fn unify_terms(
        &mut self,
        a: &Type,
        b: &Type,
        ckey: &str,
        nid: u32,
        logs: &mut Vec<UnifyLog>,
    ) -> Result<(), ErrorKind> {
        match (a, b) {
            (Type::Var(x), _) => self.unify(&Type::var(x.clone()), &b.clone(), ckey, nid, logs),
            (_, Type::Var(y)) => { let ty = Type::var(y.clone()); self.unify(&a.clone(), &ty, ckey, nid, logs) },
            (Type::Con { name: n1 }, Type::Con { name: n2 }) => {
                if n1 == n2 { Ok(()) } else {
                    Err(ErrorKind::Mismatch { expected: n1.clone(), actual: n2.clone() })
                }
            }
            (Type::Arrow(a1, a2), Type::Arrow(b1, b2)) => {
                self.unify_terms(a1, b1, ckey, nid, logs)?;
                self.unify_terms(a2, b2, ckey, nid, logs)
            }
            (Type::Row(r1), Type::Row(r2)) if r1.flavor == r2.flavor => {
                self.unify_rows_terms(r1, r2, ckey, nid, logs)
            }
            _ => Err(ErrorKind::Mismatch {
                expected: type_head_name(a),
                actual: type_head_name(b),
            }),
        }
    }

    fn unify_rows_terms(
        &mut self,
        r1: &RowType,
        r2: &RowType,
        ckey: &str,
        nid: u32,
        logs: &mut Vec<UnifyLog>,
    ) -> Result<(), ErrorKind> {
        let m1: BTreeMap<&str, &Type> = r1.fields.iter().map(|(l, t)| (l.as_str(), t)).collect();
        let m2: BTreeMap<&str, &Type> = r2.fields.iter().map(|(l, t)| (l.as_str(), t)).collect();
        for (l, t1) in &m1 {
            if let Some(t2) = m2.get(l) {
                self.unify_terms(t1, t2, ckey, nid, logs)?;
            }
        }
        let only1: Vec<String> = r1
            .fields
            .iter()
            .map(|(l, _)| l.clone())
            .filter(|l| !m2.contains_key(l.as_str()))
            .collect();
        let only2: Vec<String> = r2
            .fields
            .iter()
            .map(|(l, _)| l.clone())
            .filter(|l| !m1.contains_key(l.as_str()))
            .collect();
        let missing = if !matches!(r2.tail, RowTail::Open { .. }) { only1 } else { vec![] };
        let extra = if !matches!(r1.tail, RowTail::Open { .. }) { only2 } else { vec![] };
        if !missing.is_empty() {
            return Err(ErrorKind::MissingFields { fields: missing, flavor: r1.flavor });
        }
        if !extra.is_empty() {
            return Err(ErrorKind::ExtraFields { fields: extra, flavor: r1.flavor });
        }
        if let (RowTail::Open { var: v1 }, RowTail::Open { var: v2 }) = (&r1.tail, &r2.tail) {
            self.unify(&Type::var(v1.clone()), &Type::var(v2.clone()), ckey, nid, logs)?;
        }
        Ok(())
    }

    fn push_evidence(&mut self, root: String, merged: String, ckey: &str, nid: u32) {
        self.evidence
            .entry(root)
            .or_default()
            .push(Evidence { merged, ckey: ckey.to_string(), nid });
    }

    // 行统一：把“左侧有、右侧无”与“右侧有、左侧无”分开。
    // 仅当一侧封闭且其尾无法吸收对方标签时才报错：
    // - left 封闭、right 多出 label => ExtraFields（右是多余方）
    // - right 封闭、left 多出 label => MissingFields（左侧要求，右侧缺失）
    fn unify_rows(
        &mut self,
        r1: &RowType,
        r2: &RowType,
        ckey: &str,
        nid: u32,
        logs: &mut Vec<UnifyLog>,
    ) -> Result<(), ErrorKind> {
        let m1: BTreeMap<&str, &Type> =
            r1.fields.iter().map(|(l, t)| (l.as_str(), t)).collect();
        let m2: BTreeMap<&str, &Type> =
            r2.fields.iter().map(|(l, t)| (l.as_str(), t)).collect();

        // 公共字段逐个统一
        for (l, t1) in &m1 {
            if let Some(t2) = m2.get(l) {
                self.unify(t1, t2, ckey, nid, logs)?;
            }
        }

        let only1: Vec<String> =
            r1.fields.iter().map(|(l, _)| l.clone()).filter(|l| !m2.contains_key(l.as_str())).collect();
        let only2: Vec<String> = r2
            .fields
            .iter()
            .map(|(l, _)| l.clone())
            .filter(|l| !m1.contains_key(l.as_str()))
            .collect();

        let tail1_open = matches!(r1.tail, RowTail::Open { .. });
        let tail2_open = matches!(r2.tail, RowTail::Open { .. });

        // 缺字段：右侧封闭，但左侧要求 only1
        let missing: Vec<String> = if !tail2_open {
            only1.clone()
        } else {
            vec![]
        };
        // 多字段：左侧封闭，但右侧带 only2
        let extra: Vec<String> = if !tail1_open {
            only2.clone()
        } else {
            vec![]
        };

        if !missing.is_empty() || !extra.is_empty() {
            // 若二者同时发生，先报缺字段（调用方最小化会暴露核心），
            // 但把两类都挂到错误结构上供 UI 展示。
            if !missing.is_empty() {
                return Err(ErrorKind::MissingFields {
                    fields: missing,
                    flavor: r1.flavor,
                });
            }
            return Err(ErrorKind::ExtraFields { fields: extra, flavor: r1.flavor });
        }

        // 行尾处理（行多态）：
        // - 开放-开放：尾变量合并（共享剩余字段集合）。
        // - 开放-封闭：封闭侧没有更多字段，因此尾变量代表“另一侧独有的字段”，
        //   保持为未解析行变量即可（绝不能把它绑定成空封闭行，否则会把
        //   本应报的缺字段错误吞掉）。
        // - 封闭-封闭：字段集合已在上面全部对齐。
        if let (RowTail::Open { var: v1 }, RowTail::Open { var: v2 }) =
            (&r1.tail, &r2.tail)
        {
            self.unify(&Type::var(v1.clone()), &Type::var(v2.clone()), ckey, nid, logs)?;
        }
        Ok(())
    }
}

impl UnionFind {
    fn root_of_var_type(&self, t: &Type) -> Type {
        self.resolve_names(t)
    }
}

fn flavor_name(f: RowFlavor) -> &'static str {
    match f {
        RowFlavor::Record => "记录(record)",
        RowFlavor::Variant => "变体(variant)",
    }
}

fn type_head_name(t: &Type) -> String {
    match t {
        Type::Var(_) => "类型变量".into(),
        Type::Con { name } => name.clone(),
        Type::Arrow(_, _) => "函数类型".into(),
        Type::Row(r) => flavor_name(r.flavor).to_string(),
    }
}

fn occurs_in(target: &str, t: &Type, uf: &UnionFind) -> bool {
    match t {
        Type::Var(v) => uf.root_of(v) == target,
        Type::Con { .. } => false,
        Type::Arrow(a, b) => occurs_in(target, a, uf) || occurs_in(target, b, uf),
        Type::Row(r) => {
            r.fields.iter().any(|(_, ty)| occurs_in(target, ty, uf))
                || match &r.tail {
                    RowTail::Open { var } => uf.root_of(var) == target,
                    RowTail::Empty => false,
                }
        }
    }
}

fn describe_error(kind: &ErrorKind, ev: &Event) -> String {
    let (lr, rr) = match ev {
        Event::Equate { left_role, right_role, detail, .. } => {
            (left_role.as_str(), right_role.as_str())
        }
        _ => ("", ""),
    };
    match kind {
        ErrorKind::Mismatch { expected, actual } => format!(
            "无法统一：{lr} 需要 {expected}，但 {rr} 得到 {actual}"
        ),
        ErrorKind::Occurs { var, in_type } => format!(
            "出现无限类型（occurs-check 失败）：?{var} 出现在 {in_type} 中；{lr} 与 {rr}"
        ),
        ErrorKind::Arity { expected, actual } => {
            format!("参数数量不一致：期望 {expected}，实际 {actual}")
        }
        ErrorKind::MissingFields { fields, .. } => format!(
            "缺少字段 {}：{rr} 没有提供 {lr} 要求的这些字段",
            fields.join(", ")
        ),
        ErrorKind::ExtraFields { fields, .. } => format!(
            "多余字段 {}：{rr} 出现了 {lr} 未声明的这些字段",
            fields.join(", ")
        ),
    }
}

impl Solver {
    // 通过证据链收集 trigger 涉及的全部约束（“许多约束共同造成”）。
    // 初始集合即 trigger 本身；证据链提供“相关”视图，真正最小化
    // 在 minimize_cores 中通过删除试验完成。
    fn collect_involved(&self, left: &Type, right: &Type, trigger: &str) -> Vec<String> {
        let mut set = BTreeSet::new();
        set.insert(trigger.to_string());
        for t in [left, right] {
            let mut vars = Vec::new();
            collect_type_vars(t, &mut vars);
            for v in vars {
                let root = self.uf.root_of(&v);
                if let Some(es) = self.evidence.get(&root) {
                    for e in es {
                        set.insert(e.ckey.clone());
                    }
                }
            }
        }
        set.into_iter().collect()
    }

    // 在“仅启用给定约束子集”的假设下重新求解，返回 trigger 处是否仍失败
    // 且失败信息的种类一致。
    fn trial(&self, events: &[Event], enabled: &BTreeSet<String>, trigger: &str) -> Option<UnificationError> {
        let disabled: BTreeSet<String> = events
            .iter()
            .filter_map(|e| match e {
                Event::Equate { key, .. } if !enabled.contains(key) => Some(key.clone()),
                _ => None,
            })
            .collect();
        let result = Solver::solve(events, &disabled);
        result
            .errors
            .into_iter()
            .map(|r| r.error)
            .find(|e| e.trigger == trigger)
    }

    /// 计算包含极小核心：保证删除任何一条约束后，trigger 不再失败。
    /// 用两种删除顺序（正序/逆序）得到两个候选，保留大小最小者；
    /// 大小相同则都保留。
    fn minimize_cores(
        &self,
        events: &[Event],
        trigger: &str,
        err: &UnificationError,
    ) -> Vec<CoreCandidate> {
        // 起点：当前全部启用的统一约束
        let all_eqs: Vec<String> = events
            .iter()
            .filter_map(|e| match e {
                Event::Equate { key, .. } => Some(key.clone()),
                _ => None,
            })
            .collect();
        // 限定在 involved 超集上以降低规模；若该超集已无法触发，则回退全集
        let involved: BTreeSet<String> = err.involved.iter().cloned().collect();
        let start = if self
            .trial(events, &involved, trigger)
            .as_ref()
            .map(|e| e.kind == err.kind)
            .unwrap_or(false)
        {
            involved
        } else {
            all_eqs.iter().cloned().collect()
        };

        let mut candidates: Vec<CoreCandidate> = Vec::new();
        for order in [0usize, 1] {
            let mut core = start.clone();
            let mut keys: Vec<String> = core.iter().cloned().collect();
            if order == 1 {
                keys.reverse();
            }
            for k in keys {
                if k == trigger {
                    continue;
                }
                let mut trial_set = core.clone();
                trial_set.remove(&k);
                let still = self
                    .trial(events, &trial_set, trigger)
                    .map(|e| e.kind == err.kind)
                    .unwrap_or(false);
                if !still {
                    // k 是必要的
                } else {
                    core.remove(&k);
                }
            }
            let members: Vec<String> = core.iter().cloned().collect();
            if !candidates.iter().any(|c| c.members == members) {
                let message = err.message.clone();
                candidates.push(CoreCandidate {
                    size: members.len(),
                    members,
                    trigger: trigger.to_string(),
                    message,
                });
            }
        }
        let min = candidates.iter().map(|c| c.size).min().unwrap_or(1);
        candidates.retain(|c| c.size == min);
        candidates
    }
}

fn collect_type_vars(t: &Type, out: &mut Vec<String>) {
    match t {
        Type::Var(v) => out.push(v.clone()),
        Type::Con { .. } => {}
        Type::Arrow(a, b) => {
            collect_type_vars(a, out);
            collect_type_vars(b, out);
        }
        Type::Row(r) => {
            for (_, ty) in &r.fields {
                collect_type_vars(ty, out);
            }
            if let RowTail::Open { var } = &r.tail {
                out.push(var.clone());
            }
        }
    }
}

fn involved_nids(events: &[Event], keys: &[String]) -> Vec<u32> {
    let set: BTreeSet<&str> = keys.iter().map(|s| s.as_str()).collect();
    let mut out = BTreeSet::new();
    for e in events {
        if let Event::Equate { key, nid, .. } = e {
            if set.contains(key.as_str()) {
                out.insert(*nid);
            }
        }
    }
    out.into_iter().collect()
}

// ---------------- 步骤记录与结果 ----------------

impl Solver {
    fn step_simple(&mut self, idx: usize, ev: &Event, title: &str, detail: String) {
        let snap = self.type_snapshot();
        let live = self.live_constraints();
        self.steps.push(StepRecord {
            index: idx,
            event_key: ev.key().to_string(),
            event_cid: cid_of(ev),
            kind: event_kind_name(ev),
            title: title.to_string(),
            detail,
            nid: Some(ev.nid()),
            unifications: vec![],
            error: None,
            snapshot: snap,
            live_constraints: live,
        });
    }

    fn step_declare(&mut self, idx: usize, ev: &Event) {
        let (var, purpose) = match ev {
            Event::Declare { var, purpose, .. } => (var.clone(), purpose.clone()),
            _ => return,
        };
        let snap = self.type_snapshot();
        let live = self.live_constraints();
        self.steps.push(StepRecord {
            index: idx,
            event_key: ev.key().to_string(),
            event_cid: None,
            kind: "declare".into(),
            title: "新类型变量".into(),
            detail: format!("?{var}（用途：{purpose}）"),
            nid: Some(ev.nid()),
            unifications: vec![],
            error: None,
            snapshot: snap,
            live_constraints: live,
        });
    }

    fn step_equate(
        &mut self,
        idx: usize,
        ev: &Event,
        error: Option<String>,
        unifications: Vec<UnifyLog>,
    ) {
        let (detail, nid) = match ev {
            Event::Equate { detail, nid, .. } => (detail.clone(), *nid),
            _ => return,
        };
        let title = if error.is_some() { "统一失败" } else { "统一" }.to_string();
        let snap = self.type_snapshot();
        let live = self.live_constraints();
        self.steps.push(StepRecord {
            index: idx,
            event_key: ev.key().to_string(),
            event_cid: cid_of(ev),
            kind: "equate".into(),
            title,
            detail,
            nid: Some(nid),
            unifications,
            error,
            snapshot: snap,
            live_constraints: live,
        });
    }

    fn type_snapshot(&self) -> BTreeMap<String, String> {
        let mut out = BTreeMap::new();
        for (root, _) in &self.terms {
            let t = self.deep_resolve(&Type::var(root.clone()));
            if !matches!(t, Type::Var(_)) {
                out.insert(root.clone(), t.render());
            }
        }
        out
    }

    fn live_constraints(&self) -> Vec<String> {
        self.constraint_status
            .iter()
            .filter(|(_, s)| **s == ConstraintStatus::Solved)
            .map(|(k, _)| k.clone())
            .collect()
    }

    fn build_result(self, events: &[Event]) -> SolveResult {
        let ok = self.errors.is_empty();
        let solver = self;
        let node_vars = Solver::node_type_vars(events);
        let mut node_types = Vec::new();
        for (nid, var) in &node_vars {
            let t = solver.resolved_of_var(var);
            let parts = annotate_parts(&t, &solver, *nid);
            node_types.push(NodeTypeInfo { nid: *nid, rendered: t.render(), parts });
        }
        let statuses = events
            .iter()
            .filter_map(|e| match e {
                Event::Equate { key, cid, .. } => solver.constraint_status.get(key).map(|st| {
                    ConstraintStatusItem { key: key.clone(), cid: cid.clone(), status: st.clone() }
                }),
                _ => None,
            })
            .collect();
        SolveResult { steps: solver.steps, errors: solver.errors, node_types, statuses, ok }
    }
}


fn cid_of(ev: &Event) -> Option<String> {
    match ev {
        Event::Equate { cid, .. } => Some(cid.clone()),
        _ => None,
    }
}

fn event_kind_name(ev: &Event) -> String {
    match ev {
        Event::Declare { .. } => "declare".into(),
        Event::Define { .. } => "define".into(),
        Event::Instantiate { .. } => "instantiate".into(),
        Event::Generalize { is_value, .. } => {
            if *is_value { "generalize".into() } else { "generalize_rejected".into() }
        }
        Event::Equate { .. } => "equate".into(),
    }
}

// 给最终类型的每个叶子/结构片段标注来源 nid：
// 变量根的 origin_nid 即为产生它的表达式。
fn annotate_parts(t: &Type, solver: &Solver, fallback: u32) -> Vec<TypePart> {
    let mut parts = Vec::new();
    match t {
        Type::Var(v) => {
            let nid = solver.uf.root_nid(v);
            parts.push(TypePart { text: t.render(), nid });
        }
        Type::Con { name } => parts.push(TypePart { text: name.clone(), nid: fallback }),
        Type::Arrow(a, b) => {
            parts.extend(annotate_parts(a, solver, fallback));
            parts.extend(annotate_parts(b, solver, fallback));
        }
        Type::Row(r) => {
            for (l, ty) in &r.fields {
                parts.push(TypePart { text: l.clone(), nid: fallback });
                parts.extend(annotate_parts(ty, solver, fallback));
            }
        }
    }
    parts
}

pub fn collect_vars_public(t: &crate::types::Type, out: &mut Vec<String>) {
    collect_type_vars(t, out);
}
