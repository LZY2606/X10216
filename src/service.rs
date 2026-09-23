use crate::infer::{analyze, Analysis, PreviousResult};
use crate::infer::ConstraintCacheRecord;
use crate::infer_types::*;
use crate::parser::Parser;
use crate::syntax::*;
use crate::types::Nid;
use serde::Serialize;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone, Debug, Serialize)]
pub struct AnalysisReport {
    pub ok: bool,
    pub result_type: String,
    pub ast: Vec<AstNodeJson>,
    pub constraints: Vec<ConstraintJson>,
    pub steps: Vec<StepJson>,
    pub errors: Vec<TypeErrorJson>,
    pub notices: Vec<Notice>,
    pub expr_types: Vec<ExprTypeJson>,
    pub cores: Vec<CoreJson>,
    pub graph: GraphJson,
    pub stats: ReuseStats,
    pub parse_error: Option<ParseErrorJson>,
}

#[derive(Clone, Debug, Serialize)]
pub struct ParseErrorJson { pub message: String, span: [u32;2] }

#[derive(Clone, Debug, Serialize)]
pub struct AstNodeJson {
    pub anchor: String,
    pub parent: Option<String>,
    pub kind: String,
    pub label: String,
    pub span: [u32;2],
    pub reuse: String,
    pub typ: Option<String>,
    pub children: Vec<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct CoreJson {
    pub id: usize,
    pub constraints: Vec<String>,
    pub errors: Vec<String>,
    pub spans: Vec<[u32;2]>,
    pub minimal: bool,
    pub message: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct GraphJson {
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GraphNode {
    pub id: String,
    pub group: String,
    pub label: String,
    pub span: Option<[u32;2]>,
    pub typ: Option<String>,
    pub status: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GraphEdge {
    pub source: String,
    pub target: String,
    pub label: String,
}

pub fn build_previous(report: &AnalysisReport) -> PreviousResult {
    let anchors = report.ast.iter().map(|x| Rc::from(x.anchor.as_str())).collect();
    let constraints = report.constraints.iter().map(|c| (
        Rc::from(c.id.as_str()),
        ConstraintCacheRecord { cache_key: Rc::from(c.cache_key.as_str()), anchor: Rc::from(c.anchor.as_str()), role: Rc::from(c.role.as_str()) },
    )).collect();
    PreviousResult { anchors, constraints }
}

pub fn analyze_source(source: &str, disabled: &HashSet<Rc<str>>, previous: Option<&PreviousResult>) -> AnalysisReport {
    let parsed = Parser::parse(source);
    match parsed {
        Err(e) => AnalysisReport {
            ok: false, result_type: String::new(), ast: vec![], constraints: vec![], steps: vec![], errors: vec![],
            notices: vec![], expr_types: vec![], cores: vec![], graph: GraphJson{nodes:vec![],edges:vec![]},
            stats: ReuseStats::default(), parse_error: Some(ParseErrorJson{message:e.message.clone(),span:[e.span.start,e.span.end]}),
        },
        Ok(expr) => {
            let a = analyze(&expr, disabled, previous);
            report_from_analysis(&expr, &a, disabled)
        }
    }
}

pub fn report_from_analysis(expr: &PExpr, a: &Analysis, disabled: &HashSet<Rc<str>>) -> AnalysisReport {
    let mut ast = Vec::new();
    ast_nodes(expr, "root", None, a, &mut ast);
    let constraints = a.constraints.iter().map(|c| constraint_json(c,a,disabled)).collect();
    let errors = a.errors.iter().map(|e| a.error_to_json_public(e)).collect();
    let cores = if disabled.is_empty() { find_cores(expr) } else { Vec::new() };
    let graph = graph(expr,a,disabled);
    AnalysisReport {
        ok: a.errors.is_empty(),
        result_type: a.table.render(a.result),
        ast,
        constraints,
        steps: a.steps.borrow().clone(),
        errors,
        notices: a.notices.clone(),
        expr_types: a.expr_type_json_public(),
        cores,
        graph,
        stats: clone_stats(&a.stats),
        parse_error: None,
    }
}

fn clone_stats(s: &ReuseStats) -> ReuseStats { ReuseStats { ast_reused:s.ast_reused, ast_added:s.ast_added, ast_invalidated:s.ast_invalidated, constraint_reused:s.constraint_reused, constraint_added:s.constraint_added, constraint_invalidated:s.constraint_invalidated, cache_hits:s.cache_hits, cache_misses:s.cache_misses } }

fn constraint_json(c: &Constraint, _a: &Analysis, disabled: &HashSet<Rc<str>>) -> ConstraintJson {
    ConstraintJson {
        id: c.id.to_string(), anchor: c.anchor.to_string(), role: c.role.to_string(), cache_key: c.cache_key.to_string(),
        label: c.label.clone(), span: [c.span.start,c.span.end],
        left: SnapshotRefJson{nodes:c.left_initial.nodes.clone(),root:c.left_initial.root},
        right: SnapshotRefJson{nodes:c.right_initial.nodes.clone(),root:c.right_initial.root},
        disabled: disabled.contains(&c.id),
    }
}

fn ast_nodes(e: &PExpr, anchor: &str, parent: Option<String>, a: &Analysis, out: &mut Vec<AstNodeJson>) {
    let span = e.span();
    let children = ast_children(e,anchor);
    let typ = a.expr_types.get(&Rc::from(anchor)).map(|(_,s)|s.clone());
    let reuse = a.ast_status.get(&Rc::from(anchor)).copied().unwrap_or("current").to_string();
    out.push(AstNodeJson { anchor: anchor.into(), parent, kind: e.kind().into(), label: ast_label(e), span:[span.start,span.end], reuse, typ, children: children.iter().map(|(a,_)|a.clone()).collect() });
    for (child_anchor, child) in children { ast_nodes(child,&child_anchor,Some(anchor.into()),a,out); }
}

fn ast_children<'a>(e: &'a PExpr, parent: &str) -> Vec<(String,&'a PExpr)> {
    let mut out = Vec::new();
    let p = parent;
    match &**e {
        Expr::Lam{body,..} => out.push((format!("{p}/body@{}", body_kind(body)),body)),
        Expr::App{fun,arg,..} => { out.push((format!("{p}/fun@{}", body_kind(fun)),fun)); out.push((format!("{p}/arg@{}", body_kind(arg)),arg)); }
        Expr::Bin{left,right,..} => { out.push((format!("{p}/left@{}", body_kind(left)),left)); out.push((format!("{p}/right@{}", body_kind(right)),right)); }
        Expr::If{cond,then_branch,else_branch,..} => { out.push((format!("{p}/cond@{}", body_kind(cond)),cond)); out.push((format!("{p}/then@{}", body_kind(then_branch)),then_branch)); out.push((format!("{p}/else@{}", body_kind(else_branch)),else_branch)); }
        Expr::Let{value,body,..} => { out.push((format!("{p}/value@{}", body_kind(value)),value)); out.push((format!("{p}/body@{}", body_kind(body)),body)); }
        Expr::Record{fields,..} => for (i,(_,x)) in fields.iter().enumerate() { out.push((format!("{p}/field{i}@{}", body_kind(x)),x)); },
        Expr::Field{record,..} => out.push((format!("{p}/record@{}", body_kind(record)),record)),
        Expr::Variant{payload,..} => out.push((format!("{p}/payload@{}", body_kind(payload)),payload)),
        Expr::Match{scrutinee,arms,fallback,..} => {
            out.push((format!("{p}/scrutinee@{}", body_kind(scrutinee)),scrutinee));
            for (i,m) in arms.iter().enumerate() { out.push((format!("{p}/arm{i}@{}", body_kind(&m.body)),&m.body)); }
            if let Some(x)=fallback { out.push((format!("{p}/fallback@{}", body_kind(x)),x)); }
        }
        _ => {}
    }
    out
}
fn body_kind(e: &PExpr) -> String { format!("{}@{}",e.kind(),e.span().start) }
fn ast_label(e: &PExpr) -> String {
    match &**e {
        Expr::Int{value,..}=>value.to_string(), Expr::Bool{value,..}=>value.to_string(), Expr::Str{value,..}=>format!("\"{value}\""),
        Expr::Var{name,..}=>name.to_string(), Expr::Lam{param,..}=>format!("fn {param}"), Expr::App{..}=>"应用".into(),
        Expr::Bin{op,..}=>op.as_str().into(), Expr::If{..}=>"if".into(), Expr::Let{name,recursive,..}=>format!("{}{name}",if *recursive {"let rec "} else {"let "}),
        Expr::Record{fields,..}=>fields.iter().map(|(n,_)|n.to_string()).collect::<Vec<_>>().join(", "),
        Expr::Field{name,..}=>format!(".{name}"), Expr::Variant{tag,..}=>format!("#{tag}"), Expr::Match{..}=>"match".into(),
    }
}

fn find_cores(expr: &PExpr) -> Vec<CoreJson> {
    let base = analyze(expr, &HashSet::new(), None);
    let base_errors: Vec<Rc<str>> = base.errors.iter().map(|e| e.constraint_id.clone()).collect();
    let initial: HashSet<Rc<str>> = base_errors.iter().cloned().collect();
    if initial.is_empty() { return Vec::new(); }
    let mut answers = Vec::new();
    enumerate_cores(expr, initial, &mut answers, 0, 8);
    answers.sort_by_key(|c| (c.len(), c.iter().cloned().collect::<Vec<_>>().join(",")));
    answers.dedup();
    answers.into_iter().enumerate().map(|(i,set)| core_json(i+1,set,&base)).collect()
}

fn enumerate_cores(expr: &PExpr, candidates: HashSet<Rc<str>>, out: &mut Vec<HashSet<Rc<str>>>, depth: usize, limit: usize) {
    if depth > 12 || out.len() >= limit { return; }
    if out.iter().any(|old| old.is_subset(&candidates)) { return; }
    if !unsat_with_disabled(expr, &candidates) { return; }
    let mut set = candidates.clone();
    let items: Vec<Rc<str>> = set.iter().cloned().collect();
    for c in items {
        if out.len() >= limit { return; }
        set.remove(&c);
        if !unsat_with_disabled(expr,&set) { set.insert(c); }
    }
    if !out.contains(&set) {
        out.push(set.clone());
    }
    let all: Vec<Rc<str>> = analyze(expr,&HashSet::new(),None).constraints.iter().map(|c|c.id.clone()).collect();
    for c in all {
        if out.len() >= limit { return; }
        if candidates.contains(&c) || out.iter().any(|old| old.contains(&c)) { continue; }
        let mut branch = candidates.clone();
        branch.insert(c);
        enumerate_cores(expr,branch,out,depth+1,limit);
    }
}

fn unsat_with_disabled(expr: &PExpr, disabled: &HashSet<Rc<str>>) -> bool {
    let a = analyze(expr,disabled,None);
    !a.errors.is_empty()
}

fn core_json(index: usize, set: HashSet<Rc<str>>, base: &Analysis) -> CoreJson {
    let mut constraints: Vec<String> = set.iter().map(|x|x.to_string()).collect();
    constraints.sort();
    let errs: HashSet<Rc<str>> = set.iter().cloned().collect();
    let mut errors: Vec<String> = base.errors.iter().filter(|e| errs.contains(&e.constraint_id)).map(|e| e.constraint_id.to_string()).collect();
    errors.sort(); errors.dedup();
    let mut spans: Vec<[u32;2]> = base.constraints.iter().filter(|c| errs.contains(&c.id)).map(|c|[c.span.start,c.span.end]).collect();
    spans.sort(); spans.dedup();
    CoreJson { id:index, constraints, errors, spans, minimal:true, message:"禁用核心中的任意一条都会让这一组冲突解除；该集合本身仍不可满足。".into() }
}

fn graph(expr: &PExpr, a: &Analysis, disabled: &HashSet<Rc<str>>) -> GraphJson {
    let mut nodes = Vec::new();
    let mut edges = Vec::new();
    for n in &a.ast_status { let _=n; }
    let mut ast = Vec::new();
    ast_nodes(expr,"root",None,a,&mut ast);
    for n in ast {
        nodes.push(GraphNode{id:format!("ast:{}",n.anchor),group:"ast".into(),label:n.label,span:Some(n.span),typ:n.typ,status:Some(n.reuse)});
    }
    for c in &a.constraints {
        let status = a.constraint_status.get(&c.id).copied().unwrap_or("current");
        nodes.push(GraphNode{id:format!("c:{}",c.id),group:"constraint".into(),label:c.label.clone(),span:Some([c.span.start,c.span.end]),typ:None,status:Some(if disabled.contains(&c.id) {"disabled".to_string()} else {status.to_string()})});
        edges.push(GraphEdge{source:format!("ast:{}",c.anchor),target:format!("c:{}",c.id),label:c.role.to_string()});
    }
    GraphJson{nodes,edges}
}

impl Analysis {
    pub fn error_to_json_public(&self, e: &crate::infer_types::TypeError) -> TypeErrorJson { self.error_to_json(e) }
    pub fn expr_type_json_public(&self) -> Vec<ExprTypeJson> { self.expr_type_json() }
}

#[allow(dead_code)]
fn nid(x:Nid)->u32{x}
#[allow(dead_code)]
fn bmap() -> BTreeMap<String,HashMap<u32,()>> { BTreeMap::new() }
