use crate::infer_types::*;
use crate::syntax::*;
use crate::types::*;
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone)]
struct Binding {
    name: Rc<str>,
    scheme: Scheme,
    rank: u32,
    anchor: Rc<str>,
}

#[derive(Clone, Copy)]
struct AmbientVar { root: Nid, rank: u32 }

#[derive(Clone, Default)]
pub struct PreviousResult {
    pub anchors: HashSet<Rc<str>>,
    pub constraints: HashMap<Rc<str>, ConstraintCacheRecord>,
}

#[derive(Clone)]
pub struct ConstraintCacheRecord {
    pub cache_key: Rc<str>,
    pub anchor: Rc<str>,
    pub role: Rc<str>,
}

pub struct AnalysisInput<'a> {
    pub expr: &'a PExpr,
    pub disabled: &'a HashSet<Rc<str>>,
    pub previous: Option<&'a PreviousResult>,
}

pub struct Analysis {
    pub table: TypeTable,
    pub result: Nid,
    pub constraints: Vec<Constraint>,
    pub steps: RefCell<Vec<StepJson>>,
    pub errors: Vec<TypeError>,
    pub notices: Vec<Notice>,
    pub expr_types: HashMap<Rc<str>, (Nid, String)>,
    pub ast_status: HashMap<Rc<str>, &'static str>,
    pub constraint_status: HashMap<Rc<str>, &'static str>,
    pub stats: ReuseStats,
    disabled: HashSet<Rc<str>>,
    previous: Option<PreviousResult>,
    env: Vec<Binding>,
    ambient: Vec<AmbientVar>,
    ambient_counter: u32,
    rank: u32,
    counter: usize,
}

pub fn analyze(expr: &PExpr, disabled: &HashSet<Rc<str>>, previous: Option<&PreviousResult>) -> Analysis {
    let mut a = Analysis {
        table: TypeTable::new(),
        result: 0,
        constraints: Vec::new(),
        steps: RefCell::new(Vec::new()),
        errors: Vec::new(),
        notices: Vec::new(),
        expr_types: HashMap::new(),
        ast_status: HashMap::new(),
        constraint_status: HashMap::new(),
        stats: ReuseStats::default(),
        disabled: disabled.clone(),
        previous: previous.cloned(),
        env: Vec::new(),
        ambient: Vec::new(),
        ambient_counter: 0,
        rank: 1,
        counter: 0,
    };
    if let Some(prev) = previous {
        mark_ast(expr, &prev.anchors, &mut a.ast_status, &mut a.stats);
    }
    a.result = a.gen(expr, Rc::from("root"));
    let ambient = a.ambient.iter().map(|x| x.root).collect::<Vec<_>>();
    for q in a.table.generalize(a.result, 1) {
        if !ambient.contains(&q) { a.table.set_rank(q, u32::MAX); }
    }
    let rendered2: Vec<_> = a.expr_types.iter().map(|(anchor,(nid,_))| (anchor.clone(),*nid,a.table.render(*nid))).collect();
    for (anchor,nid,rendered) in rendered2 { a.expr_types.insert(anchor,(nid,rendered)); }
    let mut final_steps = a.steps.borrow_mut();
    let final_index = final_steps.len();
    final_steps.push(StepJson {
        index: final_index,
        kind: "final".into(),
        constraint_id: None,
        title: "最终类型".into(),
        detail: format!("程序类型：{}", a.table.render(a.result)),
        span: Some([expr.span().start, expr.span().end]),
        active_anchor: Some("root".into()),
        before_left: None,
        before_right: None,
        after: a.table.snapshot(&[a.result]).0,
        after_roots: vec![0],
        expr_types: a.expr_type_json(),
        errors: a.error_json(),
    });
    drop(final_steps);
    a
}

fn mark_ast(e: &PExpr, prev: &HashSet<Rc<str>>, status: &mut HashMap<Rc<str>, &'static str>, stats: &mut ReuseStats) {
    let mut walk = |_: &PExpr| {};
    let _ = &mut walk;
    fn go(e: &PExpr, parent: &str, prev: &HashSet<Rc<str>>, status: &mut HashMap<Rc<str>, &'static str>, stats: &mut ReuseStats) {
        let anchor = if parent == "root" { Rc::from("root") } else { Rc::from(format!("{parent}/{}", child_key(e))) };
        if prev.contains(&anchor) { status.insert(anchor.clone(), "reused"); stats.ast_reused += 1; }
        else { status.insert(anchor.clone(), "added"); stats.ast_added += 1; }
        match &**e {
            Expr::Lam { body, .. } => go(body, &anchor, prev, status, stats),
            Expr::App { fun, arg, .. } => { go(fun,&anchor,prev,status,stats); go(arg,&anchor,prev,status,stats); }
            Expr::Bin { left,right,.. } => { go(left,&anchor,prev,status,stats); go(right,&anchor,prev,status,stats); }
            Expr::If { cond,then_branch,else_branch,.. } => { go(cond,&anchor,prev,status,stats); go(then_branch,&anchor,prev,status,stats); go(else_branch,&anchor,prev,status,stats); }
            Expr::Let { value,body,.. } => { go(value,&anchor,prev,status,stats); go(body,&anchor,prev,status,stats); }
            Expr::Record { fields,.. } => for (_,x) in fields { go(x,&anchor,prev,status,stats); },
            Expr::Field { record,.. } => go(record,&anchor,prev,status,stats),
            Expr::Variant { payload,.. } => go(payload,&anchor,prev,status,stats),
            Expr::Match { scrutinee,arms,fallback,.. } => {
                go(scrutinee,&anchor,prev,status,stats);
                for (i,m) in arms.iter().enumerate() { go(&m.body, &format!("{anchor}/arm{i}"), prev, status, stats); }
                if let Some(x)=fallback { go(x,&anchor,prev,status,stats); }
            }
            _ => {}
        }
    }
    go(e, "root", prev, status, stats);
    stats.ast_invalidated = prev.len().saturating_sub(stats.ast_reused);
}

fn child_key(e: &Expr) -> String { e.kind().to_string() }

impl Analysis {
    fn gen(&mut self, e: &PExpr, anchor: Rc<str>) -> Nid {
        let n = match &**e {
            Expr::Int { .. } => self.table.con("Int"),
            Expr::Bool { .. } => self.table.con("Bool"),
            Expr::Str { .. } => self.table.con("String"),
            Expr::Var { name, span, .. } => self.gen_var(name, *span, &anchor),
            Expr::Lam { param, param_span, body, .. } => self.gen_lam(e, param, *param_span, body, &anchor),
            Expr::App { fun, arg, .. } => {
                let ft = self.gen_child(fun, &anchor, "fun");
                let at = self.gen_child(arg, &anchor, "arg");
                let rt = self.table.var(self.rank);
                let ft2 = self.table.arr(at, rt);
                self.unify_constraint(ft, ft2, e.span(), &anchor, "app", "应用：函数类型必须是 参数 -> 结果");
                rt
            }
            Expr::Bin { op,left,right,.. } => self.gen_bin(*op,left,right,e.span(),&anchor),
            Expr::If { cond,then_branch,else_branch,.. } => {
                let ct = self.gen_child(cond,&anchor,"cond");
                let boolt = self.table.con("Bool");
                self.unify_constraint(ct,boolt,e.span(),&anchor,"if_cond","条件必须是 Bool");
                let tt = self.gen_child(then_branch,&anchor,"then");
                let et = self.gen_child(else_branch,&anchor,"else");
                self.unify_constraint(tt,et,e.span(),&anchor,"if_branches","两个分支类型必须一致");
                tt
            }
            Expr::Let { recursive,name,name_span,value,body,.. } => self.gen_let(e,*recursive,name,*name_span,value,body,&anchor),
            Expr::Record { fields,.. } => self.gen_record(fields,e.span(),&anchor),
            Expr::Field { record,name,name_span: _,.. } => {
                let rt = self.gen_child(record,&anchor,"record");
                let field = self.table.var(self.rank);
                let tail = self.table.var(self.rank);
                let row = self.table.extend_row(RowKind::Record,name.clone(),field,tail);
                let rec_t = self.table.rec(row);
                self.unify_constraint(rt,rec_t,e.span(),&anchor,"field_access",&format!("投影字段 {name}：记录必须包含该字段"));
                field
            }
            Expr::Variant { tag,payload,.. } => {
                let pt = self.gen_child(payload,&anchor,"payload");
                let tail = self.table.var(self.rank);
                let row = self.table.extend_row(RowKind::Variant,tag.clone(),pt,tail);
                self.table.variant(row)
            }
            Expr::Match { scrutinee,arms,fallback,.. } => self.gen_match(scrutinee,arms,fallback.as_ref(),e.span(),&anchor),
        };
        let rendered = self.table.render(n);
        self.expr_types.insert(anchor.clone(),(n,rendered));
        n
    }

    fn gen_child(&mut self, e: &PExpr, parent: &str, child: &str) -> Nid {
        let anchor = Rc::from(format!("{parent}/{child}@{}", e_kind_id(e)));
        self.gen(e, anchor)
    }

    fn gen_var(&mut self, name: &str, span: Span, anchor: &str) -> Nid {
        if let Some(b) = self.env.iter().rev().find(|b| b.name.as_ref() == name).cloned() {
            let copied = self.instantiate(&b.scheme);
            self.step_simple("instantiate", span, Some(anchor), &format!("变量 {name}：多态实例化"), &format!("方案 {} 实例化", self.render_scheme(&b.scheme)), vec![copied]);
            copied
        } else {
            let v = self.table.var(self.rank);
            self.ambient.push(AmbientVar { root: v, rank: self.rank });
            self.step_simple("ambient", span, Some(anchor), &format!("自由变量 {name}"), "未绑定名称，保留为外部单态变量", vec![v]);
            v
        }
    }

    fn gen_lam(&mut self, _e: &PExpr, param: &str, _param_span: Span, body: &PExpr, anchor: &str) -> Nid {
        self.rank += 1;
        let pt = self.table.var(self.rank);
        self.env.push(Binding { name: Rc::from(param), scheme: Scheme { quantified: vec![], root: pt }, rank: self.rank, anchor: Rc::from(format!("{anchor}/param")) });
        let bt = self.gen_child(body, anchor, "body");
        let pr = self.env.pop().unwrap().rank;
        self.rank = pr - 1;
        self.table.arr(pt, bt)
    }

    fn gen_bin(&mut self, op: BinOp, left: &PExpr, right: &PExpr, span: Span, anchor: &str) -> Nid {
        let lt = self.gen_child(left,anchor,"left");
        let rt = self.gen_child(right,anchor,"right");
        let int_t = self.table.con("Int");
        self.unify_constraint(lt,int_t,span,anchor,"bin_left",&format!("{} 左侧必须是 Int",op.as_str()));
        self.unify_constraint(rt,int_t,span,anchor,"bin_right",&format!("{} 右侧必须是 Int",op.as_str()));
        match op { BinOp::Eq | BinOp::Lt => self.table.con("Bool"), _ => self.table.con("Int") }
    }

    fn gen_let(&mut self, e: &PExpr, recursive: bool, name: &str, name_span: Span, value: &PExpr, body: &PExpr, anchor: &str) -> Nid {
        self.rank += 1;
        let let_rank = self.rank;
        let recursive_var = if recursive {
            let v = self.table.var(let_rank);
            self.env.push(Binding { name: Rc::from(name), scheme: Scheme { quantified: vec![], root: v }, rank: let_rank, anchor: Rc::from(format!("{anchor}/binding")) });
            Some(v)
        } else { None };
        let vt = self.gen_child(value,anchor,"value");
        if let Some(rv) = recursive_var {
            self.unify_constraint(rv,vt,name_span,anchor,"rec_binding",&format!("递归绑定 {name}：右侧必须匹配递归变量"));
        }
        let expansive = !is_syntactic_value(value);
        let ambient_before = self.ambient_fingerprint(let_rank);
        let quantified = if expansive {
            self.notices.push(Notice {
                kind: "value_restriction".into(),
                message: format!("{name} 的定义不是语法值，按 value restriction 不泛化剩余多态变量"),
                span: [name_span.start,name_span.end],
                anchor: anchor.to_string(),
                name: name.to_string(),
                inferred: self.table.render(vt),
                generalized: self.table.render(vt),
            });
            self.add_value_restriction(name_span, anchor, vt, e);
            vec![]
        } else {
            let qs = self.table.generalize(vt, let_rank - 1);
            for q in &qs { self.table.set_rank(*q, u32::MAX); }
            qs
        };
        let scheme = Scheme { quantified: quantified.iter().map(|x| self.table.find(*x)).collect(), root: self.table.find(vt) };
        self.notices.last_mut();
        let generalized = self.render_scheme(&scheme);
        if let Some(n) = self.notices.last_mut() { n.generalized = generalized; }
        if recursive { self.env.pop(); }
        self.env.push(Binding { name: Rc::from(name), scheme: scheme.clone(), rank: let_rank, anchor: Rc::from(format!("{anchor}/binding")) });
        let step_roots = std::iter::once(vt).chain(quantified.iter().copied()).collect();
        self.step_simple(if quantified.is_empty() {"monomorphize"} else {"generalize"}, e.span(), Some(anchor), &format!("绑定 {name}：{}", if quantified.is_empty() {"保持单态"} else {"泛化"}),
            &format!("scope={let_rank}; genv={ambient_before}; {} {}", self.render_scheme(&Binding{name:Rc::from(name),scheme:scheme.clone(),rank:let_rank,anchor:Rc::from("")}.scheme), if expansive {"value-restricted"} else {""}), step_roots);
        let bt = self.gen_child(body,anchor,"body");
        self.env.pop();
        self.rank -= 1;
        bt
    }

    fn gen_record(&mut self, fields: &[(Rc<str>, PExpr)], span: Span, anchor: &str) -> Nid {
        let mut map = BTreeMap::new();
        for (i,(name,fe)) in fields.iter().enumerate() {
            let t = self.gen_child(fe, anchor, &format!("field{i}"));
            map.insert(name.clone(), t);
        }
        let row = self.table.closed_row(RowKind::Record, map);
        self.step_simple("record", span, Some(anchor), "构造封闭 record", "字段以排序后的行表示", vec![row]);
        self.table.rec(row)
    }

    fn gen_match(&mut self, scrutinee: &PExpr, arms: &[MatchArm], fallback: Option<&PExpr>, span: Span, anchor: &str) -> Nid {
        let st = self.gen_child(scrutinee,anchor,"scrutinee");
        let result = self.table.var(self.rank);
        let mut row_tail: Option<Nid> = Some(self.table.var(self.rank));
        for (i,arm) in arms.iter().enumerate().rev() {
            let payload = self.table.var(self.rank);
            let tail = row_tail.take().unwrap_or_else(|| self.table.var(self.rank));
            let row = self.table.extend_row(RowKind::Variant,arm.tag.clone(),payload,tail);
            row_tail = Some(row);
            self.rank += 1;
            self.env.push(Binding { name: arm.binding.clone(), scheme: Scheme{quantified:vec![],root:payload}, rank:self.rank, anchor: Rc::from(format!("{anchor}/arm{i}/binding")) });
            let bt = self.gen_child(&arm.body,anchor,&format!("arm{i}"));
            self.env.pop();
            self.rank -= 1;
            self.unify_constraint(bt,result,span,anchor,&format!("arm{i}_result"),&format!("#{} 分支结果必须一致",arm.tag));
            let variant_t = self.table.variant(row);
            let mut sst = st;
            self.unify_constraint(sst,variant_t,span,anchor,&format!("arm{i}_tag"),&format!("匹配分支 #{} 约束被检查值",arm.tag));
            sst = self.table.find(st);
            let _ = &mut sst;
        }
        if let Some(fb) = fallback {
            let ft = self.gen_child(fb,anchor,"fallback");
            self.unify_constraint(ft,result,span,anchor,"fallback_result","fallback 分支结果必须一致");
        } else {
            if let Some(tail) = row_tail {
                let empty = self.table.empty_row(RowKind::Variant);
                self.unify_constraint(tail,empty,span,anchor,"match_exhaustive","无 fallback 时 variant 行必须封闭");
            }
        }
        result
    }

    fn ambient_fingerprint(&self, rank: u32) -> String {
        let mut roots: BTreeSet<u32> = self.ambient.iter().filter(|v| v.rank <= rank).map(|v| self.table.find(v.root)).collect();
        roots.extend(self.env.iter().flat_map(|b| {
            let mut s = self.table.free_vars(&[b.scheme.root]).into_iter().collect::<Vec<_>>();
            s.sort(); s
        }));
        roots.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")
    }

    fn instantiate(&mut self, scheme: &Scheme) -> Nid {
        let mut mapping: HashMap<Nid,Nid> = HashMap::new();
        for q in &scheme.quantified {
            let fresh = self.table.var(self.rank);
            mapping.insert(*q, fresh);
        }
        self.copy_node(scheme.root, &mapping)
    }

    fn copy_node(&mut self, x: Nid, mapping: &HashMap<Nid,Nid>) -> Nid {
        let root = self.table.find(x);
        if let Some(n) = mapping.get(&root) { return *n; }
        match self.table.d(root).clone() {
            D::Var => root,
            D::Con(n) => self.table.con(&n),
            D::Arr(a,b) => { let a=self.copy_node(a,mapping); let b=self.copy_node(b,mapping); self.table.arr(a,b) }
            D::Rec(r) => { let r=self.copy_node(r,mapping); self.table.rec(r) }
            D::Variant(r) => { let r=self.copy_node(r,mapping); self.table.variant(r) }
            D::Row(ext, tv) => {
                let value=self.copy_node(ext.value,mapping);
                let tail=self.copy_node(ext.tail,mapping);
                let tv=tv.map(|tv| self.copy_node(tv,mapping));
                self.table.make(D::Row(RowExt{kind:ext.kind,label:ext.label,value,tail},tv), 0)
            }
        }
    }

    fn unify_constraint(&mut self, left: Nid, right: Nid, span: Span, anchor: &str, role: &str, label: &str) {
        let id: Rc<str> = Rc::from(format!("{anchor}::{role}"));
        let cache_key: Rc<str> = Rc::from(format!("{id}|scope={}|genv={}", self.rank, self.ambient_fingerprint(self.rank)));
        let left_initial = self.snapshot_ref(left);
        let right_initial = self.snapshot_ref(right);
        let c = Constraint { id: id.clone(), anchor: Rc::from(anchor), role: Rc::from(role), cache_key: cache_key.clone(), span, label: label.to_string(), left, right, left_initial: left_initial.clone(), right_initial: right_initial.clone() };
        self.constraints.push(c);
        if let Some(prev) = self.take_previous_constraint(&id, &cache_key) {
            self.constraint_status.insert(id.clone(), prev);
        } else {
            let status = if self.previous_any(&id) { "resolved_again" } else { "added" };
            self.constraint_status.insert(id.clone(), status);
        }
        let disabled = self.disabled.contains(&id);
        let before_errors = self.errors.len();
        let before = self.current_snapshot(left,right);
        if !disabled {
            if let Err(kind) = unify(&mut self.table, left, right, &id) {
                self.errors.push(TypeError { kind, span, constraint_id: id.clone(), anchor: Rc::from(anchor), left, right });
            }
        }
        let errs = self.errors[before_errors..].iter().map(|e| self.error_to_json(e)).collect();
        let mut steps = self.steps.borrow_mut();
        let step_index = steps.len();
        steps.push(StepJson {
            index: step_index,
            kind: if disabled {"unify_skipped".into()} else {"unify".into()},
            constraint_id: Some(id.to_string()),
            title: if disabled { format!("已禁用：{label}") } else { label.to_string() },
            detail: if disabled { "约束存在但本次求解临时禁用；不执行 union。".into() } else { format!("统一 {} 与 {}", self.table.render(left), self.table.render(right)) },
            span: Some([span.start,span.end]),
            active_anchor: Some(anchor.to_string()),
            before_left: Some(SnapshotRefJson{nodes:left_initial.nodes,root:left_initial.root}),
            before_right: Some(SnapshotRefJson{nodes:right_initial.nodes,root:right_initial.root}),
            after: before.0,
            after_roots: before.1,
            expr_types: self.expr_type_json(),
            errors: errs,
        });
    }

    fn current_snapshot(&self, left: Nid, right: Nid) -> (Vec<TypeJson>, Vec<u32>) {
        let (nodes, map) = self.table.snapshot(&[left,right]);
        (nodes, vec![map[&self.table.find(left)], map[&self.table.find(right)]])
    }
    fn snapshot_ref(&self, x: Nid) -> SnapshotRef { let (nodes,map)=self.table.snapshot(&[x]); SnapshotRef{nodes,root:map[&self.table.find(x)]} }

    fn take_previous_constraint(&mut self, id: &str, key: &str) -> Option<&'static str> {
        let hit = self.previous.as_ref().and_then(|p| p.constraints.get(id)).map(|r| r.cache_key.as_ref() == key).unwrap_or(false);
        if hit {
            self.stats.constraint_reused += 1;
            self.stats.cache_hits += 1;
            Some("reused")
        } else {
            None
        }
    }
    fn previous_any(&mut self, id: &str) -> bool {
        let existed = self.previous.as_ref().map(|p| p.constraints.contains_key(id)).unwrap_or(false);
        if existed { self.stats.constraint_invalidated += 1; self.stats.cache_misses += 1; }
        else { self.stats.constraint_added += 1; }
        existed
    }

    fn add_value_restriction(&mut self, span: Span, anchor: &str, vt: Nid, e: &PExpr) {
        self.errors.push(TypeError {
            kind: TypeErrorKind::ValueRestriction,
            span,
            constraint_id: Rc::from(format!("{anchor}::value_restriction")),
            anchor: Rc::from(anchor),
            left: vt,
            right: vt,
        });
        let _ = e;
    }

    fn step_simple(&mut self, kind: &str, span: Span, anchor: Option<&str>, title: &str, detail: &str, roots: Vec<Nid>) {
        let (after,map) = self.table.snapshot(&roots);
        let roots = roots.iter().map(|x| map[&self.table.find(*x)]).collect();
        let mut steps = self.steps.borrow_mut();
        let step_index = steps.len();
        steps.push(StepJson {
            index: step_index, kind: kind.into(), constraint_id: None, title: title.into(), detail: detail.into(),
            span: Some([span.start,span.end]), active_anchor: anchor.map(|x|x.to_string()), before_left:None, before_right:None,
            after, after_roots: roots, expr_types: self.expr_type_json(), errors: self.error_json(),
        });
    }

    fn render_scheme(&self, s: &Scheme) -> String {
        if s.quantified.is_empty() { self.table.render(s.root) }
        else { let names: Vec<String> = s.quantified.iter().map(|x| format!("t{x}")).collect(); format!("forall {}. {}", names.join(" "), self.table.render(s.root)) }
    }

    pub fn error_to_json(&self, e: &TypeError) -> TypeErrorJson {
        let mut j = TypeErrorJson {
            id: format!("err:{}", e.constraint_id),
            kind: match e.kind { TypeErrorKind::Mismatch => "mismatch", TypeErrorKind::Occurs(_) => "occurs", TypeErrorKind::RowMismatch(_) => "row_mismatch", TypeErrorKind::ValueRestriction => "value_restriction", TypeErrorKind::Arity => "arity", TypeErrorKind::NotRecord => "not_record", TypeErrorKind::NotVariant => "not_variant" }.into(),
            message: String::new(),
            span: [e.span.start,e.span.end],
            constraint_id: e.constraint_id.to_string(),
            anchor: e.anchor.to_string(),
            left: self.table.render(e.left),
            right: self.table.render(e.right),
            occurs_var: None,
            value_restriction: false,
            missing_fields: vec![], extra_fields: vec![], field_conflicts: vec![],
        };
        match &e.kind {
            TypeErrorKind::Mismatch => j.message = format!("无法把 {} 与 {} 统一：类型构造不一致", j.left, j.right),
            TypeErrorKind::Occurs(v) => { j.occurs_var = Some(self.table.render(*v)); j.message = format!("occurs-check 拒绝无限类型：{} 出现在 {} 中", self.table.render(*v), self.table.render(e.right)); }
            TypeErrorKind::RowMismatch(d) => {
                j.missing_fields = d.missing.iter().map(|(l,t)| format!("{l}: {}", self.table.render(*t))).collect();
                j.extra_fields = d.extra.iter().map(|(l,t)| format!("{l}: {}", self.table.render(*t))).collect();
                j.field_conflicts = d.conflicts.iter().map(|(l,a,b)| FieldConflictJson{field:l.to_string(),left:self.table.render(*a),right:self.table.render(*b)}).collect();
                j.message = format!("行类型不兼容；缺字段 [{}]，多字段 [{}]", j.missing_fields.join(", "), j.extra_fields.join(", "));
            }
            TypeErrorKind::ValueRestriction => { j.value_restriction = true; j.message = "value restriction：右侧不是语法值，未泛化的多态不能安全共享".into(); }
            TypeErrorKind::Arity => j.message = "函数应用参数数量不一致".into(),
            TypeErrorKind::NotRecord => j.message = "只有 record 可以做字段投影".into(),
            TypeErrorKind::NotVariant => j.message = "只有 variant 可以做标签匹配".into(),
        }
        j
    }

    fn error_json(&self) -> Vec<TypeErrorJson> { self.errors.iter().map(|e| self.error_to_json(e)).collect() }
    pub fn expr_type_json(&self) -> Vec<ExprTypeJson> {
        let mut v: Vec<_> = self.expr_types.iter().map(|(a,(n,t))| ExprTypeJson{anchor:a.to_string(),typ:t.clone(),root:*n}).collect();
        v.sort_by(|a,b| a.anchor.cmp(&b.anchor)); v
    }
}

fn e_kind_id(e: &PExpr) -> String { format!("{}@{}", e.kind(), e.span().start) }

pub fn is_syntactic_value(e: &PExpr) -> bool {
    match &**e {
        Expr::Int {..} | Expr::Bool{..} | Expr::Str{..} | Expr::Var{..} | Expr::Lam{..} => true,
        Expr::Record{fields,..} => fields.iter().all(|(_,e)| is_syntactic_value(e)),
        Expr::Variant{payload,..} => is_syntactic_value(payload),
        _ => false,
    }
}

fn occurs(table: &TypeTable, v: Nid, x: Nid) -> bool { table.free_vars(&[x]).contains(&table.find(v)) }

fn bind(table: &mut TypeTable, a: Nid, b: Nid) {
    let a = table.find(a); let b = table.find(b);
    if a == b { return; }
    table.link(a,b);
}

pub fn unify(table: &mut TypeTable, left: Nid, right: Nid, _cid: &str) -> Result<(), TypeErrorKind> {
    let a = table.find(left);
    let b = table.find(right);
    if a == b { return Ok(()); }
    match (table.nodes[a as usize].d.clone(), table.nodes[b as usize].d.clone()) {
        (D::Var,_) => { if occurs(table,a,b) { return Err(TypeErrorKind::Occurs(a)); } bind(table,a,b); Ok(()) }
        (_,D::Var) => { if occurs(table,b,a) { return Err(TypeErrorKind::Occurs(b)); } bind(table,b,a); Ok(()) }
        (D::Con(x),D::Con(y)) if x == y => Ok(()),
        (D::Arr(a1,a2),D::Arr(b1,b2)) => { unify(table,a1,b1,_cid)?; unify(table,a2,b2,_cid) }
        (D::Rec(ra),D::Rec(rb)) => unify_rows(table,RowKind::Record,ra,rb),
        (D::Variant(ra),D::Variant(rb)) => unify_rows(table,RowKind::Variant,ra,rb),
        _ => Err(TypeErrorKind::Mismatch),
    }
}

#[derive(Clone)]
struct NormRow { fields: BTreeMap<Rc<str>, Nid>, tail: Option<Nid> }

fn norm_row(table: &TypeTable, x: Nid, kind: RowKind) -> NormRow {
    let (fs,tail) = table.normalize_row(x);
    let mut fields = BTreeMap::new();
    for (l,v) in fs { fields.insert(l,v); }
    if let Some(t) = tail {
        if let D::Row(ext,_) = table.d(t) {
            if ext.kind == kind && !ext.label.is_empty() { let n = norm_row(table,t,kind); return NormRow{fields:n.fields.into_iter().chain(fields).collect(),tail:n.tail}; }
        }
    }
    NormRow{fields,tail}
}

fn unify_rows(table: &mut TypeTable, kind: RowKind, left: Nid, right: Nid) -> Result<(), TypeErrorKind> {
    let l = norm_row(table,left,kind);
    let r = norm_row(table,right,kind);
    let labels: BTreeSet<Rc<str>> = l.fields.keys().chain(r.fields.keys()).cloned().collect();
    let mut missing = Vec::new();
    let mut extra = Vec::new();
    let mut conflicts = Vec::new();
    for label in &labels {
        match (l.fields.get(label), r.fields.get(label)) {
            (Some(x),Some(y)) => { if table.find(*x) != table.find(*y) { conflicts.push((label.clone(),*x,*y)); } }
            (Some(x),None) => extra.push((label.clone(),*x)),
            (None,Some(y)) => missing.push((label.clone(),*y)),
            (None,None) => {}
        }
    }
    match (l.tail,r.tail) {
        (None,None) => {
            if !missing.is_empty() || !extra.is_empty() || !conflicts.is_empty() {
                return Err(TypeErrorKind::RowMismatch(RowMismatchDetail{missing,extra,conflicts}));
            }
            Ok(())
        }
        (Some(t),None) => {
            if !extra.is_empty() { return Err(TypeErrorKind::RowMismatch(RowMismatchDetail{missing,extra,conflicts})); }
            let empty = table.empty_row(kind);
            for c in &conflicts { let _=c; }
            for (label,val) in r.fields.iter().rev() {
                let old = table.find(t);
                let nf = table.extend_row(kind,label.clone(),table.find(*val),old);
                bind(table,t,nf);
            }
            unify(table,t,empty,"")?;
            for (_,x,y) in conflicts { unify(table,x,y,"")?; }
            for (_,y) in missing { let _=y; }
            Ok(())
        }
        (None,Some(t)) => {
            if !missing.is_empty() { return Err(TypeErrorKind::RowMismatch(RowMismatchDetail{missing,extra,conflicts})); }
            let empty = table.empty_row(kind);
            for (label,val) in l.fields.iter().rev() {
                let old = table.find(t);
                let nf = table.extend_row(kind,label.clone(),table.find(*val),old);
                bind(table,t,nf);
            }
            unify(table,t,empty,"")?;
            for (_,x,y) in conflicts { unify(table,x,y,"")?; }
            Ok(())
        }
        (Some(a),Some(b)) => {
            let common_l: BTreeSet<_> = l.fields.keys().cloned().collect();
            let common_r: BTreeSet<_> = r.fields.keys().cloned().collect();
            let only_l: Vec<_> = l.fields.iter().filter(|(k,_)| !common_r.contains(*k)).map(|(k,v)|(k.clone(),*v)).collect();
            let only_r: Vec<_> = r.fields.iter().filter(|(k,_)| !common_l.contains(*k)).map(|(k,v)|(k.clone(),*v)).collect();
            let new_tail_b = table.var(table.rank(b));
            let mut new_b = new_tail_b;
            for (label,val) in only_l.iter().rev() { new_b = table.extend_row(kind,label.clone(),table.find(*val),new_b); }
            bind(table,b,new_b);
            let new_tail_a = table.var(table.rank(a));
            let mut new_a = new_tail_a;
            for (label,val) in only_r.iter().rev() { new_a = table.extend_row(kind,label.clone(),table.find(*val),new_a); }
            bind(table,a,new_a);
            unify(table,new_tail_a,new_tail_b,"")?;
            for (_,x,y) in conflicts { unify(table,x,y,"")?; }
            let _ = (missing,extra);
            Ok(())
        }
    }
}
