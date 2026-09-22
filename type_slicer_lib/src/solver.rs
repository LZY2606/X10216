//! Constraint solver: executes actions, performs unification with occurs
//! check and row polymorphism, records step-by-step traces, and computes
//! minimal unsat cores.

use crate::model::*;
use crate::syntax::*;
use crate::types::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UnifyEvent {
    pub left: String,
    pub right: String,
    pub relation: String,
    pub ok: bool,
    pub detail: String,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum StepStatus {
    Ok,
    Failed,
    Noop,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StepRecord {
    pub action_id: String,
    pub index: usize,
    pub title: String,
    pub status: StepStatus,
    pub events: Vec<UnifyEvent>,
    pub error_index: Option<usize>,
    pub summary: String,
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct TypeErrorData {
    pub kind: String,
    pub message: String,
    pub left: String,
    pub right: String,
    pub span: Option<Span>,
    pub action_id: String,
    pub node: Option<NodeId>,
    pub left_origins: BTreeSet<u64>,
    pub right_origins: BTreeSet<u64>,
    pub missing: Vec<String>,
    pub extra: Vec<String>,
    pub occurs_var: String,
    pub occurs_in: String,
    pub cores: Vec<Vec<String>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MetaInfo {
    pub key: String,
    pub id: MetaId,
    pub kind: TVarKind,
    pub level: u32,
    pub label: String,
    pub origin: NodeId,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SchemeInfo {
    pub key: String,
    pub role: String,
    pub is_value: bool,
    pub quantifiers: Vec<u32>,
    pub quantifier_kinds: Vec<TVarKind>,
    pub rendered: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SolveResult {
    pub steps: Vec<StepRecord>,
    pub errors: Vec<TypeErrorData>,
    pub root_type: TypeView,
    pub node_types: Vec<(u64, TypeView)>,
    pub metas: Vec<MetaInfo>,
    pub schemes: Vec<SchemeInfo>,
}

// ---------------------------------------------------------------------------
// Internal state
// ---------------------------------------------------------------------------

struct MetaCell {
    id: MetaId,
    kind: TVarKind,
    level: u32,
    label: String,
    origin: NodeId,
}

#[derive(Clone)]
struct ResolvedScheme {
    quantifiers: Vec<u32>,
    quantifier_kinds: Vec<TVarKind>,
    body: Term,
    is_value: bool,
    role: String,
}

pub struct Executor<'a> {
    prog: &'a ConstraintProgram,
    disabled: BTreeSet<String>,
    cells: HashMap<String, MetaCell>,
    next_meta: MetaId,
    link: HashMap<MetaId, MetaId>,
    binds: HashMap<MetaId, Term>,
    row_binds: HashMap<MetaId, Row>,
    meta_origins: HashMap<MetaId, BTreeSet<NodeId>>,
    level: u32,
    schemes: HashMap<String, ResolvedScheme>,
    next_q: u32,
    steps: Vec<StepRecord>,
    errors: Vec<TypeErrorData>,
}

impl<'a> Executor<'a> {
    pub(crate) fn new(prog: &'a ConstraintProgram, disabled: BTreeSet<String>) -> Self {
        Executor {
            prog,
            disabled,
            cells: HashMap::new(),
            next_meta: 1,
            link: HashMap::new(),
            binds: HashMap::new(),
            row_binds: HashMap::new(),
            meta_origins: HashMap::new(),
            level: 0,
            schemes: HashMap::new(),
            next_q: 0,
            steps: Vec::new(),
            errors: Vec::new(),
        }
    }

    // -- meta allocation / union find -------------------------------------

    fn register(&mut self, key: &str, kind: TVarKind, level: u32, label: String, origin: NodeId) -> MetaId {
        if let Some(c) = self.cells.get(key) {
            return c.id;
        }
        let id = self.next_meta;
        self.next_meta += 1;
        self.cells.insert(
            key.to_string(),
            MetaCell { id, kind, level, label, origin },
        );
        let mut s = BTreeSet::new();
        s.insert(origin);
        self.meta_origins.insert(id, s);
        id
    }

    fn ensure_term(&mut self, t: &STerm) -> Term {
        match t {
            STerm::V(k) => {
                if !self.cells.contains_key(&k.0) {
                    self.register(&k.0, TVarKind::Star, self.level, "隐式变量".into(), NodeId(0));
                }
                Term::Var(self.cells[&k.0].id)
            }
            STerm::Q(i) => Term::QVar(*i),
            STerm::C(c) => Term::Con(*c),
            STerm::Arrow(a, b) => Term::Fun(
                Box::new(self.ensure_term(a)),
                Box::new(self.ensure_term(b)),
            ),
            STerm::Rec(r) => Term::Record(self.ensure_row(r)),
            STerm::Variant(r) => Term::Variant(self.ensure_row(r)),
        }
    }

    fn ensure_row(&mut self, r: &SRow) -> Row {
        Row {
            fields: r.fields.iter().map(|(l, t)| (l.clone(), self.ensure_term(t))).collect(),
            end: match &r.end {
                SRowEnd::Closed => RowEnd::Closed,
                SRowEnd::Open(t) => {
                    if let STerm::V(k) = t.as_ref() {
                        if !self.cells.contains_key(&k.0) {
                            self.register(&k.0, TVarKind::Row, self.level, "隐式行尾".into(), NodeId(0));
                        }
                        RowEnd::Open(Box::new(Term::Var(self.cells[&k.0].id)))
                    } else {
                        RowEnd::Open(Box::new(self.ensure_term(t)))
                    }
                }
            },
        }
    }

    fn find(&mut self, x: MetaId) -> MetaId {
        let mut root = x;
        while let Some(p) = self.link.get(&root).copied() {
            root = p;
        }
        if root != x {
            self.link.insert(x, root);
        }
        root
    }

    fn resolve(&mut self, t: &Term) -> Term {
        match t {
            Term::Var(v) => {
                let r = self.find(*v);
                if let Some(bt) = self.binds.get(&r).cloned() {
                    self.resolve(&bt)
                } else {
                    Term::Var(r)
                }
            }
            Term::QVar(_) | Term::Con(_) => t.clone(),
            Term::Fun(a, b) => Term::Fun(Box::new(self.resolve(a)), Box::new(self.resolve(b))),
            Term::Record(r) => Term::Record(self.resolve_row(r)),
            Term::Variant(r) => Term::Variant(self.resolve_row(r)),
        }
    }

    fn resolve_row(&mut self, r: &Row) -> Row {
        let mut out: Vec<(String, Term)> =
            r.fields.iter().map(|(l, t)| (l.clone(), self.resolve(t))).collect();
        out.sort_by(|a, b| a.0.cmp(&b.0));
        let end = match &r.end {
            RowEnd::Closed => RowEnd::Closed,
            RowEnd::Open(t) => {
                let rt = self.resolve(t);
                if let Term::Var(v) = rt {
                    let rv = self.find(v);
                    if let Some(row) = self.row_binds.get(&rv).cloned() {
                        let mut merged = self.resolve_row(&row);
                        let mut all = out;
                        all.append(&mut merged.fields);
                        all.sort_by(|a, b| a.0.cmp(&b.0));
                        return match merged.end {
                            RowEnd::Closed => Row { fields: all, end: RowEnd::Closed },
                            RowEnd::Open(tt) => Row { fields: all, end: RowEnd::Open(tt) },
                        };
                    }
                    RowEnd::Open(Box::new(Term::Var(v)))
                } else {
                    RowEnd::Open(Box::new(rt))
                }
            }
        };
        Row { fields: out, end }
    }

    fn origins_of(&self, t: &Term) -> BTreeSet<NodeId> {
        let mut out = BTreeSet::new();
        fn go(s: &Executor, t: &Term, out: &mut BTreeSet<NodeId>) {
            match t {
                Term::Var(v) => {
                    if let Some(set) = s.meta_origins.get(v) {
                        out.extend(set.iter().copied());
                    }
                }
                Term::QVar(_) | Term::Con(_) => {}
                Term::Fun(a, b) => {
                    go(s, a, out);
                    go(s, b, out);
                }
                Term::Record(r) | Term::Variant(r) => {
                    for (_, f) in &r.fields {
                        go(s, f, out);
                    }
                    if let RowEnd::Open(tail) = &r.end {
                        go(s, tail, out);
                    }
                }
            }
        }
        go(self, t, &mut out);
        out
    }
}

// ---------------------------------------------------------------------------
// Unification
// ---------------------------------------------------------------------------

pub struct UnifyOutcome {
    pub ok: bool,
    pub detail: String,
    pub error: Option<TypeErrorData>,
}

impl<'a> Executor<'a> {
    fn occurs(&mut self, v: MetaId, t: &Term) -> bool {
        let t = self.resolve(t);
        match t {
            Term::Var(w) => self.find(w) == self.find(v),
            Term::QVar(_) | Term::Con(_) => false,
            Term::Fun(a, b) => self.occurs(v, &a) || self.occurs(v, &b),
            Term::Record(r) | Term::Variant(r) => {
                r.fields.iter().any(|(_, f)| self.occurs(v, f))
                    || match &r.end {
                        RowEnd::Closed => false,
                        RowEnd::Open(tail) => self.occurs(v, tail),
                    }
            }
        }
    }

    fn min_level_of(&mut self, t: &Term) -> u32 {
        let t = self.resolve(t);
        match t {
            Term::Var(v) => {
                let id = self.find(v);
                self.cell_level(id)
            }
            Term::QVar(_) | Term::Con(_) => u32::MAX,
            Term::Fun(a, b) => self.min_level_of(&a).min(self.min_level_of(&b)),
            Term::Record(r) | Term::Variant(r) => {
                let mut m = u32::MAX;
                for (_, f) in &r.fields {
                    m = m.min(self.min_level_of(f));
                }
                if let RowEnd::Open(tail) = &r.end {
                    m = m.min(self.min_level_of(tail));
                }
                m
            }
        }
    }

    fn cell_level(&self, id: MetaId) -> u32 {
        self.cells.values().find(|c| c.id == id).map(|c| c.level).unwrap_or(0)
    }

    fn set_level(&mut self, id: MetaId, level: u32) {
        for c in self.cells.values_mut() {
            if c.id == id && c.level > level {
                c.level = level;
            }
        }
    }

    fn merge_origins(&mut self, keep: MetaId, drop: MetaId) {
        if keep == drop {
            return;
        }
        let extra = self.meta_origins.get(&drop).cloned().unwrap_or_default();
        self.meta_origins.entry(keep).or_default().extend(extra);
    }

    fn bind_var(&mut self, v: MetaId, t: Term) {
        let rv = self.find(v);
        let rt = self.resolve(&t);
        if let Term::Var(w) = &rt {
            let rw = self.find(*w);
            if rw == rv {
                return;
            }
            let lv = self.cell_level(rv);
            let lw = self.cell_level(rw);
            let (keep, drop) = if lv <= lw { (rv, rw) } else { (rw, rv) };
            self.link.insert(drop, keep);
            let lvl = lv.min(lw);
            self.set_level(keep, lvl);
            self.merge_origins(keep, drop);
            return;
        }
        let lvl = self.min_level_of(&rt).min(self.cell_level(rv));
        self.set_level(rv, lvl);
        self.binds.insert(rv, rt);
    }

    /// Unify two terms. `path` describes the structural position for errors.
    fn unify_term(&mut self, a: &Term, b: &Term, path: &str) -> UnifyOutcome {
        let ra = self.resolve(a);
        let rb = self.resolve(b);
        match (&ra, &rb) {
            (Term::Var(v), Term::Var(w)) => {
                self.bind_var(*v, Term::Var(*w));
                UnifyOutcome { ok: true, detail: format!("合并变量 ?{} 与 ?{}", self.find(*v), self.find(*w)), error: None }
            }
            (Term::Var(v), other) | (other, Term::Var(v)) => {
                let v = *v;
                let other = other.clone();
                if self.occurs(v, &other) {
                    let rv = self.find(v);
                    let rendered = render(&other);
                    let msg = format!(
                        "occurs check 失败：变量 ?{rv} 出现在 {rendered} 中，将产生无限类型"
                    );
                    return UnifyOutcome {
                        ok: false,
                        detail: msg.clone(),
                        error: Some(TypeErrorData {
                            kind: "occurs".into(),
                            message: msg,
                            occurs_var: format!("?{}", self.find(v)),
                            occurs_in: render(&other),
                            ..Default::default()
                        }),
                    };
                }
                let rv = self.find(v);
                let kind = self.cells.values().find(|c| c.id == rv).map(|c| c.kind).unwrap_or(TVarKind::Star);
                if kind == TVarKind::Row {
                    // A row variable can only be bound to a row term.
                    match &other {
                        Term::Record(r) | Term::Variant(r) => {
                            self.row_binds.insert(rv, r.clone());
                            return UnifyOutcome { ok: true, detail: format!("行变量 ?{rv} := 行"), error: None };
                        }
                        _ => {
                            let msg = format!("行变量 ?{} 不能绑定到非行类型 {}", self.find(v), render(&other));
                            return UnifyOutcome {
                                ok: false,
                                detail: msg.clone(),
                                error: Some(TypeErrorData {
                                    kind: "row_kind".into(),
                                    message: msg,
                                    left: format!("?{rv}"),
                                    right: render(&other),
                                    ..Default::default()
                                }),
                            };
                        }
                    }
                }
                self.bind_var(v, other.clone());
                UnifyOutcome { ok: true, detail: format!("绑定 ?{rv} := {}", render(&other)), error: None }
            }
            (Term::Con(c1), Term::Con(c2)) => {
                if c1 == c2 {
                    UnifyOutcome { ok: true, detail: format!("相同构造子 {}", c1.name()), error: None }
                } else {
                    let msg = format!("类型构造子冲突：{} 与 {}", c1.name(), c2.name());
                    UnifyOutcome {
                        ok: false,
                        detail: msg.clone(),
                        error: Some(TypeErrorData {
                            kind: "ctor".into(),
                            message: msg,
                            left: c1.name().into(),
                            right: c2.name().into(),
                            ..Default::default()
                        }),
                    }
                }
            }
            (Term::Fun(a1, r1), Term::Fun(a2, r2)) => {
                let o1 = self.unify_term(a1, a2, &format!("{path}.参数"));
                if !o1.ok {
                    return o1;
                }
                self.unify_term(r1, r2, &format!("{path}.结果"))
            }
            (Term::Record(r1), Term::Record(r2)) => self.unify_rows(r1, r2, "record", path),
            (Term::Variant(r1), Term::Variant(r2)) => self.unify_rows(r1, r2, "variant", path),
            (Term::QVar(i), Term::QVar(j)) if i == j => {
                UnifyOutcome { ok: true, detail: "相同量化变量".into(), error: None }
            }
            _ => {
                let msg = format!(
                    "类型形状冲突：{} 与 {}",
                    shape_name(&ra),
                    shape_name(&rb)
                );
                UnifyOutcome {
                    ok: false,
                    detail: msg.clone(),
                    error: Some(TypeErrorData {
                        kind: "shape".into(),
                        message: msg,
                        left: render(&ra),
                        right: render(&rb),
                        ..Default::default()
                    }),
                }
            }
        }
    }

    fn unify_rows(&mut self, r1: &Row, r2: &Row, flavor: &str, path: &str) -> UnifyOutcome {
        let r1 = self.resolve_row(r1);
        let r2 = self.resolve_row(r2);
        let m1: BTreeMap<String, Term> = r1.fields.iter().cloned().collect();
        let m2: BTreeMap<String, Term> = r2.fields.iter().cloned().collect();

        for (label, t1) in &m1 {
            if let Some(t2) = m2.get(label) {
                let o = self.unify_term(t1, t2, &format!("{path}.{label}"));
                if !o.ok {
                    return o;
                }
            }
        }
        let only1: Vec<(String, Term)> =
            m1.iter().filter(|(l, _)| !m2.contains_key(*l)).map(|(l, t)| (l.clone(), t.clone())).collect();
        let only2: Vec<(String, Term)> =
            m2.iter().filter(|(l, _)| !m1.contains_key(*l)).map(|(l, t)| (l.clone(), t.clone())).collect();

        match (&r1.end, &r2.end) {
            (RowEnd::Closed, RowEnd::Closed) => {
                if only1.is_empty() && only2.is_empty() {
                    UnifyOutcome { ok: true, detail: "封闭行字段完全一致".into(), error: None }
                } else {
                    let missing = only1.iter().map(|(l, _)| l.clone()).collect::<Vec<_>>();
                    let extra = only2.iter().map(|(l, _)| l.clone()).collect::<Vec<_>>();
                    {
                    let msg = format!(
                        "{flavor} 行不匹配：缺少字段 [{}]，多余字段 [{}]",
                        missing.join(", "),
                        extra.join(", ")
                    );
                    UnifyOutcome {
                        ok: false,
                        detail: msg.clone(),
                        error: Some(TypeErrorData {
                            kind: format!("{flavor}_row"),
                            message: msg,
                            left: render(&Term::Record(r1.clone())),
                            right: render(&Term::Record(r2.clone())),
                            missing,
                            extra,
                            ..Default::default()
                        }),
                    }
                    }
                }
            }
            (RowEnd::Open(t1), RowEnd::Closed) => {
                if !only1.is_empty() {
                    return row_shape_error(&r1, &r2, flavor, &only1, &only2);
                }
                self.bind_row_tail(t1, Row::closed(only2))
            }
            (RowEnd::Closed, RowEnd::Open(t2)) => {
                if !only2.is_empty() {
                    return row_shape_error(&r1, &r2, flavor, &only2, &only1);
                }
                self.bind_row_tail(t2, Row::closed(only1))
            }
            (RowEnd::Open(t1), RowEnd::Open(t2)) => {
                let fresh = self.fresh_row_var();
                let o1 = self.bind_row_tail(t1, Row::open(only2.clone(), Term::Var(fresh)));
                if !o1.ok {
                    return o1;
                }
                self.bind_row_tail(t2, Row::open(only1.clone(), Term::Var(fresh)))
            }
        }
    }

    fn fresh_row_var(&mut self) -> MetaId {
        let key = format!("row!{}", self.next_meta);
        self.register(&key, TVarKind::Row, self.level, "行统一产生的新鲜行尾".into(), NodeId(0))
    }

    fn bind_row_tail(&mut self, tail: &Term, row: Row) -> UnifyOutcome {
        let rt = self.resolve(tail);
        match rt {
            Term::Var(v) => {
                let rv = self.find(v);
                if let Some(existing) = self.row_binds.get(&rv).cloned() {
                    // Tail already bound: unify the rows.
                    return self.unify_rows(&existing, &row, "record", "行尾");
                }
                // occurs check for row variable
                let mut fv = Vec::new();
                for (_, t) in &row.fields {
                    free_vars(t, &mut fv);
                }
                if let RowEnd::Open(t) = &row.end {
                    free_vars(t, &mut fv);
                }
                let rv2 = rv;
                if fv.iter().any(|f| self.find(*f) == rv2) {
                    let msg = format!("occurs check 失败：行变量 ?{rv2} 出现在自身行中");
                    return UnifyOutcome {
                        ok: false,
                        detail: msg.clone(),
                        error: Some(TypeErrorData {
                            kind: "occurs".into(),
                            message: msg,
                            occurs_var: format!("?{rv2}"),
                            occurs_in: "行".into(),
                            ..Default::default()
                        }),
                    };
                }
                self.row_binds.insert(rv, row);
                UnifyOutcome { ok: true, detail: format!("行尾 ?{rv} := 行"), error: None }
            }
            other => {
                let msg = format!("行尾不是行变量，无法扩展：{}", render(&other));
                UnifyOutcome {
                    ok: false,
                    detail: msg.clone(),
                    error: Some(TypeErrorData {
                        kind: "row_tail".into(),
                        message: msg,
                        left: render(&other),
                        ..Default::default()
                    }),
                }
            }
        }
    }
}

fn row_shape_error(
    r1: &Row,
    r2: &Row,
    flavor: &str,
    missing: &[(String, Term)],
    extra: &[(String, Term)],
) -> UnifyOutcome {
    let missing_names: Vec<String> = missing.iter().map(|(l, _)| l.clone()).collect();
    let extra_names: Vec<String> = extra.iter().map(|(l, _)| l.clone()).collect();
    let msg = format!(
        "{flavor} 行不匹配：缺少字段 [{}]，多余字段 [{}]",
        missing_names.join(", "),
        extra_names.join(", ")
    );
    UnifyOutcome {
        ok: false,
        detail: msg.clone(),
        error: Some(TypeErrorData {
            kind: format!("{flavor}_row"),
            message: msg,
            left: render(&Term::Record(r1.clone())),
            right: render(&Term::Record(r2.clone())),
            missing: missing_names,
            extra: extra_names,
            ..Default::default()
        }),
    }
}

fn shape_name(t: &Term) -> String {
    match t {
        Term::Var(v) => format!("变量 ?{v}"),
        Term::QVar(i) => format!("量化变量 {}", qname(*i)),
        Term::Con(c) => format!("基础类型 {}", c.name()),
        Term::Fun(_, _) => "函数类型".into(),
        Term::Record(_) => "记录类型".into(),
        Term::Variant(_) => "变体类型".into(),
    }
}

// ---------------------------------------------------------------------------
// Action execution
// ---------------------------------------------------------------------------

impl<'a> Executor<'a> {
    fn record_step(&mut self, action: &Action, index: usize, status: StepStatus, events: Vec<UnifyEvent>, error_index: Option<usize>, summary: String) {
        self.steps.push(StepRecord {
            action_id: action.id.0.clone(),
            index,
            title: action.title.clone(),
            status,
            events,
            error_index,
            summary,
        });
    }

    fn run_action(&mut self, action: &Action, index: usize) {
        if self.disabled.contains(&action.id.0) {
            self.record_step(action, index, StepStatus::Noop, vec![], None, "已禁用，跳过".into());
            return;
        }
        match &action.kind {
            ActionKind::Intro { key, var_kind, level, label } => {
                let kind = if var_kind == "row" { TVarKind::Row } else { TVarKind::Star };
                let id = self.register(&key.0, kind, *level, label.clone(), action.node);
                self.record_step(
                    action,
                    index,
                    StepStatus::Ok,
                    vec![],
                    None,
                    format!("引入 ?{id}（{label}，level {level}）"),
                );
            }
            ActionKind::EnterScope { level } => {
                self.level = *level;
                self.record_step(action, index, StepStatus::Ok, vec![], None, format!("level := {level}"));
            }
            ActionKind::ExitScope => {
                self.level = self.level.saturating_sub(1);
                self.record_step(action, index, StepStatus::Ok, vec![], None, format!("level := {}", self.level));
            }
            ActionKind::Error { message } => {
                let mut data = TypeErrorData {
                    kind: "unbound".into(),
                    message: message.clone(),
                    action_id: action.id.0.clone(),
                    node: Some(action.node),
                    span: Some(action.span),
                    ..Default::default()
                };
                data.left_origins.insert(action.node.0);
                let eidx = self.errors.len();
                self.errors.push(data);
                self.record_step(action, index, StepStatus::Failed, vec![], Some(eidx), message.clone());
            }
            ActionKind::Unify { left, right, relation } => {
                let lt = self.ensure_term(left);
                let rt = self.ensure_term(right);
                let ltxt = render(&self.resolve(&lt));
                let rtxt = render(&self.resolve(&rt));
                let outcome = self.unify_term(&lt, &rt, relation);
                let event = UnifyEvent {
                    left: ltxt.clone(),
                    right: rtxt.clone(),
                    relation: relation.clone(),
                    ok: outcome.ok,
                    detail: outcome.detail.clone(),
                };
                if outcome.ok {
                    self.record_step(action, index, StepStatus::Ok, vec![event], None, outcome.detail);
                } else {
                    let mut data = outcome.error.unwrap_or_default();
                    data.action_id = action.id.0.clone();
                    data.node = Some(action.node);
                    data.span = Some(action.span);
                    data.left = ltxt;
                    data.right = rtxt;
                    data.left_origins = self.origins_of(&lt).iter().map(|n| n.0).collect();
                    data.right_origins = self.origins_of(&rt).iter().map(|n| n.0).collect();
                    let eidx = self.errors.len();
                    self.errors.push(data);
                    self.record_step(action, index, StepStatus::Failed, vec![event], Some(eidx), "统一失败".into());
                }
            }
            ActionKind::Prim { op, operands, result, result_con } => {
                let mut events = Vec::new();
                let mut failed: Option<TypeErrorData> = None;
                for (i, operand) in operands.iter().enumerate() {
                    let t = Term::Var(self.register(&operand.0, TVarKind::Star, self.level, "操作数".into(), action.node));
                    let ltxt = render(&self.resolve(&t));
                    let outcome = self.unify_term(&t, &Term::Con(TCon::Int), "运算数必须为 int");
                    events.push(UnifyEvent {
                        left: ltxt,
                        right: "int".into(),
                        relation: format!("{op} 的第 {} 个操作数", i + 1),
                        ok: outcome.ok,
                        detail: outcome.detail.clone(),
                    });
                    if !outcome.ok && failed.is_none() {
                        failed = outcome.error;
                    }
                }
                let rt = Term::Var(self.register(&result.0, TVarKind::Star, self.level, "结果".into(), action.node));
                let con = if result_con == "bool" { TCon::Bool } else { TCon::Int };
                let ltxt = render(&self.resolve(&rt));
                let outcome = self.unify_term(&rt, &Term::Con(con), "运算结果类型");
                events.push(UnifyEvent {
                    left: ltxt,
                    right: con.name().into(),
                    relation: format!("{op} 的结果"),
                    ok: outcome.ok,
                    detail: outcome.detail.clone(),
                });
                if !outcome.ok && failed.is_none() {
                    failed = outcome.error;
                }
                if let Some(mut data) = failed {
                    data.action_id = action.id.0.clone();
                    data.node = Some(action.node);
                    data.span = Some(action.span);
                    let eidx = self.errors.len();
                    self.errors.push(data);
                    self.record_step(action, index, StepStatus::Failed, events, Some(eidx), "内建运算约束失败".into());
                } else {
                    self.record_step(action, index, StepStatus::Ok, events, None, format!("{op} : int -> int -> {result_con}"));
                }
            }
            ActionKind::Match { scrut, result, arms, tail } => {
                let mut events = Vec::new();
                let mut failed: Option<TypeErrorData> = None;
                let scrut_t = Term::Var(self.register(&scrut.0, TVarKind::Star, self.level, "判别式".into(), action.node));
                let result_t = Term::Var(self.register(&result.0, TVarKind::Star, self.level, "match 结果".into(), action.node));
                let tail_t = Term::Var(self.register(&tail.0, TVarKind::Row, self.level, "未覆盖标签".into(), action.node));

                let mut row_fields: Vec<(String, Term)> = Vec::new();
                for arm in arms {
                    if arm.label == "_" {
                        continue;
                    }
                    let payload = match &arm.payload {
                        Some(p) => Term::Var(self.register(&p.0, TVarKind::Star, self.level, "分支绑定".into(), action.node)),
                        None => Term::Con(TCon::Unit),
                    };
                    row_fields.push((arm.label.clone(), payload));
                }
                let row = Row::open(row_fields, tail_t.clone());
                let variant = Term::Variant(row);
                let ltxt = render(&self.resolve(&scrut_t));
                let outcome = self.unify_term(&scrut_t, &variant, "判别式必须是覆盖所有分支标签的变体");
                events.push(UnifyEvent {
                    left: ltxt,
                    right: render(&variant),
                    relation: "match 判别式 vs 分支标签行".into(),
                    ok: outcome.ok,
                    detail: outcome.detail.clone(),
                });
                if !outcome.ok && failed.is_none() {
                    failed = outcome.error;
                }
                for arm in arms {
                    let body_t = Term::Var(self.register(&arm.body.0, TVarKind::Star, self.level, "分支体".into(), action.node));
                    let ltxt = render(&self.resolve(&result_t));
                    let rtxt = render(&self.resolve(&body_t));
                    let outcome = self.unify_term(&result_t, &body_t, "各分支结果一致");
                    events.push(UnifyEvent {
                        left: ltxt,
                        right: rtxt,
                        relation: format!("分支 `{} => 结果", arm.label),
                        ok: outcome.ok,
                        detail: outcome.detail.clone(),
                    });
                    if !outcome.ok && failed.is_none() {
                        failed = outcome.error;
                    }
                }
                if let Some(mut data) = failed {
                    data.action_id = action.id.0.clone();
                    data.node = Some(action.node);
                    data.span = Some(action.span);
                    let eidx = self.errors.len();
                    self.errors.push(data);
                    self.record_step(action, index, StepStatus::Failed, events, Some(eidx), "match 约束失败".into());
                } else {
                    self.record_step(action, index, StepStatus::Ok, events, None, "match 分支统一完成".into());
                }
            }
            ActionKind::Generalize { scheme, result, at_level, is_value, role } => {
                let root = Term::Var(self.register(&result.0, TVarKind::Star, self.level, role.clone(), action.node));
                let (body, qs, qkinds) = self.generalize(&root, *at_level, *is_value);
                let resolved = ResolvedScheme {
                    quantifiers: qs,
                    quantifier_kinds: qkinds,
                    body,
                    is_value: *is_value,
                    role: role.clone(),
                };
                let rendered = render_scheme(&Scheme {
                    quantifiers: resolved.quantifiers.clone(),
                    body: resolved.body.clone(),
                });
                self.schemes.insert(scheme.0.clone(), resolved);
                let note = if *is_value {
                    format!("{role} : {rendered}")
                } else {
                    format!("{role} : {rendered}（value restriction：无量化变量）")
                };
                self.record_step(action, index, StepStatus::Ok, vec![], None, note);
            }
            ActionKind::Instantiate { scheme, result } => {
                let resolved_scheme = match scheme {
                    SchemeRef::Local(sk) => self.schemes.get(&sk.0).cloned(),
                    SchemeRef::Builtin(name) => Some(builtin_scheme(name)),
                };
                match resolved_scheme {
                    Some(rs) => {
                        let mut map: HashMap<u32, Term> = HashMap::new();
                        for (i, q) in rs.quantifiers.iter().enumerate() {
                            let kind = rs.quantifier_kinds.get(i).copied().unwrap_or(TVarKind::Star);
                            let key = format!("inst:{}:{}", action.id.0, q);
                            let id = self.register(&key, kind, self.level, format!("实例化 {}", qname(*q)), action.node);
                            map.insert(*q, Term::Var(id));
                        }
                        let body = subst_qvars(&rs.body, &map);
                        let rt = Term::Var(self.register(&result.0, TVarKind::Star, self.level, "实例化结果".into(), action.node));
                        let ltxt = render(&self.resolve(&rt));
                        let rtxt = render(&self.resolve(&body));
                        let outcome = self.unify_term(&rt, &body, "实例化结果等于展开后的方案体");
                        let event = UnifyEvent {
                            left: ltxt,
                            right: rtxt,
                            relation: "实例化".into(),
                            ok: outcome.ok,
                            detail: outcome.detail.clone(),
                        };
                        if outcome.ok {
                            self.record_step(action, index, StepStatus::Ok, vec![event], None, outcome.detail);
                        } else {
                            let mut data = outcome.error.unwrap_or_default();
                            data.action_id = action.id.0.clone();
                            data.node = Some(action.node);
                            data.span = Some(action.span);
                            let eidx = self.errors.len();
                            self.errors.push(data);
                            self.record_step(action, index, StepStatus::Failed, vec![event], Some(eidx), "实例化失败".into());
                        }
                    }
                    None => {
                        self.record_step(action, index, StepStatus::Noop, vec![], None, "方案缺失（上游失败），跳过".into());
                    }
                }
            }
        }
    }

    /// Collect metas reachable from `root` at level >= `at_level`, freezing
    /// them into QVars. With the value restriction nothing is quantified.
    fn generalize(&mut self, root: &Term, at_level: u32, is_value: bool) -> (Term, Vec<u32>, Vec<TVarKind>) {
        let mut quantifiable: Vec<MetaId> = Vec::new();
        let mut seen: BTreeSet<MetaId> = BTreeSet::new();
        let mut stack = vec![root.clone()];
        while let Some(t) = stack.pop() {
            let t = self.resolve(&t);
            match &t {
                Term::Var(v) => {
                    let r = self.find(*v);
                    if seen.insert(r) {
                        let level = self.cell_level(r);
                        if level >= at_level {
                            quantifiable.push(r);
                        } else {
                            // Do not traverse through lower-level variables:
                            // anything behind them belongs to outer scopes.
                        }
                    }
                }
                Term::QVar(_) | Term::Con(_) => {}
                Term::Fun(a, b) => {
                    stack.push((**a).clone());
                    stack.push((**b).clone());
                }
                Term::Record(r) | Term::Variant(r) => {
                    for (_, f) in &r.fields {
                        stack.push(f.clone());
                    }
                    if let RowEnd::Open(tail) = &r.end {
                        stack.push((**tail).clone());
                    }
                }
            }
        }
        quantifiable.sort();
        let mut qs = Vec::new();
        let mut qkinds = Vec::new();
        let mut map: HashMap<MetaId, u32> = HashMap::new();
        if is_value {
            for m in &quantifiable {
                let q = self.next_q;
                self.next_q += 1;
                map.insert(*m, q);
                qs.push(q);
                let kind = self.cells.values().find(|c| c.id == *m).map(|c| c.kind).unwrap_or(TVarKind::Star);
                qkinds.push(kind);
            }
        }
        let body = freeze(root, self, &map);
        (body, qs, qkinds)
    }
}

fn subst_qvars(t: &Term, map: &HashMap<u32, Term>) -> Term {
    match t {
        Term::QVar(i) => map.get(i).cloned().unwrap_or_else(|| t.clone()),
        Term::Var(_) | Term::Con(_) => t.clone(),
        Term::Fun(a, b) => Term::Fun(Box::new(subst_qvars(a, map)), Box::new(subst_qvars(b, map))),
        Term::Record(r) => Term::Record(Row {
            fields: r.fields.iter().map(|(l, f)| (l.clone(), subst_qvars(f, map))).collect(),
            end: match &r.end {
                RowEnd::Closed => RowEnd::Closed,
                RowEnd::Open(tail) => RowEnd::Open(Box::new(subst_qvars(tail, map))),
            },
        }),
        Term::Variant(r) => Term::Variant(Row {
            fields: r.fields.iter().map(|(l, f)| (l.clone(), subst_qvars(f, map))).collect(),
            end: match &r.end {
                RowEnd::Closed => RowEnd::Closed,
                RowEnd::Open(tail) => RowEnd::Open(Box::new(subst_qvars(tail, map))),
            },
        }),
    }
}

fn freeze(t: &Term, exec: &Executor, map: &HashMap<MetaId, u32>) -> Term {
    let t = exec.resolve_shallow(t);
    match t {
        Term::Var(v) => {
            let r = exec.find_const(v);
            match map.get(&r) {
                Some(q) => Term::QVar(*q),
                None => Term::Var(r),
            }
        }
        Term::QVar(_) | Term::Con(_) => t,
        Term::Fun(a, b) => Term::Fun(
            Box::new(freeze(&a, exec, map)),
            Box::new(freeze(&b, exec, map)),
        ),
        Term::Record(r) => Term::Record(freeze_row(&r, exec, map)),
        Term::Variant(r) => Term::Variant(freeze_row(&r, exec, map)),
    }
}

fn freeze_row(r: &Row, exec: &Executor, map: &HashMap<MetaId, u32>) -> Row {
    Row {
        fields: r.fields.iter().map(|(l, f)| (l.clone(), freeze(f, exec, map))).collect(),
        end: match &r.end {
            RowEnd::Closed => RowEnd::Closed,
            RowEnd::Open(t) => RowEnd::Open(Box::new(freeze(t, exec, map))),
        },
    }
}

fn builtin_scheme(name: &str) -> ResolvedScheme {
    match name {
        "print" => ResolvedScheme {
            quantifiers: vec![9000],
            quantifier_kinds: vec![TVarKind::Star],
            body: Term::Fun(Box::new(Term::QVar(9000)), Box::new(Term::Con(TCon::Unit))),
            is_value: true,
            role: "print".into(),
        },
        _ => ResolvedScheme {
            quantifiers: vec![],
            quantifier_kinds: vec![],
            body: Term::Con(TCon::Unit),
            is_value: true,
            role: name.into(),
        },
    }
}

// ---------------------------------------------------------------------------
// Immutable resolution, views, top-level run
// ---------------------------------------------------------------------------

impl<'a> Executor<'a> {
    fn find_const(&self, x: MetaId) -> MetaId {
        let mut root = x;
        while let Some(p) = self.link.get(&root).copied() {
            root = p;
        }
        root
    }

    fn resolve_shallow(&self, t: &Term) -> Term {
        match t {
            Term::Var(v) => {
                let r = self.find_const(*v);
                if let Some(bt) = self.binds.get(&r) {
                    self.resolve_shallow(bt)
                } else if let Some(row) = self.row_binds.get(&r) {
                    // Present the expanded row through a synthetic variable node
                    // is not possible immutably; callers of freeze only need the
                    // row shape via resolve_row-like expansion:
                    let row = self.resolve_shallow_row(row);
                    Term::Record(row)
                } else {
                    Term::Var(r)
                }
            }
            Term::QVar(_) | Term::Con(_) => t.clone(),
            Term::Fun(a, b) => Term::Fun(
                Box::new(self.resolve_shallow(a)),
                Box::new(self.resolve_shallow(b)),
            ),
            Term::Record(r) => Term::Record(self.resolve_shallow_row(r)),
            Term::Variant(r) => Term::Variant(self.resolve_shallow_row(r)),
        }
    }

    fn resolve_shallow_row(&self, r: &Row) -> Row {
        let mut out: Vec<(String, Term)> = Vec::new();
        for (l, t) in &r.fields {
            out.push((l.clone(), self.resolve_shallow(t)));
        }
        let end = match &r.end {
            RowEnd::Closed => RowEnd::Closed,
            RowEnd::Open(t) => {
                let rt = self.resolve_shallow(t);
                if let Term::Var(v) = rt {
                    if let Some(row) = self.row_binds.get(&self.find_const(v)) {
                        let mut extra = self.resolve_shallow_row(row);
                        out.append(&mut extra.fields);
                        out.sort_by(|a, b| a.0.cmp(&b.0));
                        // merge duplicate labels deterministically
                        out.dedup_by(|a, b| a.0 == b.0);
                        return match extra.end {
                            RowEnd::Closed => Row { fields: out, end: RowEnd::Closed },
                            RowEnd::Open(tt) => Row { fields: out, end: RowEnd::Open(tt) },
                        };
                    }
                    RowEnd::Open(Box::new(Term::Var(v)))
                } else {
                    RowEnd::Open(Box::new(rt))
                }
            }
        };
        out.sort_by(|a, b| a.0.cmp(&b.0));
        Row { fields: out, end }
    }

    fn build_view(&self, t: &Term) -> TypeView {
        let t = self.resolve_shallow(t);
        let origins: BTreeSet<u64> = self.origins_of(&t).iter().map(|n| n.0).collect();
        let node = match t {
            Term::Var(v) => {
                let r = self.find_const(v);
                let cell = self.cells.values().find(|c| c.id == r);
                ViewNode::Var {
                    id: r,
                    level: cell.map(|c| c.level).unwrap_or(0),
                    kind: cell.map(|c| c.kind).unwrap_or(TVarKind::Star),
                    label: cell.map(|c| c.label.clone()).unwrap_or_else(|| "?".into()),
                }
            }
            Term::QVar(i) => ViewNode::QVar { index: i },
            Term::Con(c) => ViewNode::Con { name: c.name().into() },
            Term::Fun(a, b) => ViewNode::Fun {
                param: Box::new(self.build_view(&a)),
                result: Box::new(self.build_view(&b)),
            },
            Term::Record(r) => {
                let fields = r.fields.iter().map(|(l, f)| (l.clone(), self.build_view(f))).collect();
                let tail = match r.end {
                    RowEnd::Closed => None,
                    RowEnd::Open(t) => Some(Box::new(self.build_view(&t))),
                };
                ViewNode::Record { fields, tail }
            }
            Term::Variant(r) => {
                let fields = r.fields.iter().map(|(l, f)| (l.clone(), self.build_view(f))).collect();
                let tail = match r.end {
                    RowEnd::Closed => None,
                    RowEnd::Open(t) => Some(Box::new(self.build_view(&t))),
                };
                ViewNode::Variant { fields, tail }
            }
        };
        TypeView { origins, node }
    }

    pub fn run_all(&mut self) {
        for (index, action) in self.prog.actions.iter().enumerate() {
            self.run_action(action, index);
        }
    }

    pub(crate) fn failed_action_ids(&self) -> Vec<String> {
        self.errors.iter().map(|e| e.action_id.clone()).collect()
    }
}

pub fn solve(prog: &ConstraintProgram, disabled: &BTreeSet<String>) -> SolveResult {
    let mut exec = Executor::new(prog, disabled.clone());
    exec.run_all();
    let errors = exec.errors.clone();
    let core_lists = crate::cores::compute_cores(prog, &errors);
    let mut errors = errors;
    for (i, cores) in core_lists.into_iter().enumerate() {
        errors[i].cores = cores;
    }

    let root_key = prog.root_meta.0.clone();
    let root_term = if let Some(c) = exec.cells.get(&root_key) {
        Term::Var(c.id)
    } else {
        Term::Var(0)
    };
    let root_view = exec.build_view(&root_term);

    let mut node_types = Vec::new();
    let mut seen_nodes = BTreeSet::new();
    for (node, key) in &prog.node_types {
        if seen_nodes.insert(node.0) {
            if let Some(c) = exec.cells.get(&key.0) {
                node_types.push((node.0, exec.build_view(&Term::Var(c.id))));
            }
        }
    }

    let mut metas: Vec<MetaInfo> = exec
        .cells
        .values()
        .map(|c| MetaInfo {
            key: "?".into(),
            id: c.id,
            kind: c.kind,
            level: c.level,
            label: c.label.clone(),
            origin: c.origin,
        })
        .collect();
    for (k, c) in &exec.cells {
        if let Some(m) = metas.iter_mut().find(|m| m.id == c.id) {
            m.key = k.clone();
        }
    }
    metas.sort_by_key(|m| m.id);

    let schemes = exec
        .schemes
        .iter()
        .map(|(k, s)| SchemeInfo {
            key: k.clone(),
            role: s.role.clone(),
            is_value: s.is_value,
            quantifiers: s.quantifiers.clone(),
            quantifier_kinds: s.quantifier_kinds.clone(),
            rendered: render_scheme(&Scheme {
                quantifiers: s.quantifiers.clone(),
                body: s.body.clone(),
            }),
        })
        .collect();

    SolveResult {
        steps: exec.steps,
        errors,
        root_type: root_view,
        node_types,
        metas,
        schemes,
    }
}
