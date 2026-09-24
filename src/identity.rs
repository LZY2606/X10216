use crate::syntax::*;
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug, Clone, Default)]
pub struct Identity {
    pub nodes: HashMap<Span, NodeInfo>,
    pub spans_by_id: HashMap<String, Span>,
}

impl Identity {
    pub fn id_at(&self, span: Span) -> String {
        self.nodes.get(&span).map(|n| n.id.clone()).unwrap_or_else(|| format!("span:{}:{}", span.start, span.end))
    }
    pub fn node(&self, span: Span) -> NodeInfo {
        self.nodes.get(&span).cloned().unwrap_or(NodeInfo {
            id: format!("span:{}:{}", span.start, span.end),
            kind: "synthetic".into(),
            label: String::new(),
            span,
        })
    }
}

fn hstring(parts: &[String]) -> u64 {
    let mut h = 0xcbf29ce484222325u64;
    for p in parts {
        for b in p.bytes() { h ^= b as u64; h = h.wrapping_mul(0x100000001b3); }
        h ^= 0xff; h = h.wrapping_mul(0x100000001b3);
    }
    h
}

fn with_name(ctx: &[String], name: &str) -> Vec<String> {
    let mut c = ctx.to_vec();
    c.push(name.to_string());
    c
}

fn var_hash(name: &str, global: &HashSet<String>, local: &[String]) -> String {
    if global.contains(name) {
        format!("G:{name}")
    } else if let Some(i) = local.iter().rposition(|x| x == name) {
        format!("L:{}", local.len() - i)
    } else {
        format!("F:{name}")
    }
}

fn alpha_expr(e: &Expr, global: &HashSet<String>, local: &[String]) -> u64 {
    let parts = match e {
        Expr::Lit(n) => vec!["lit".into(), format!("{:?}", n.value)],
        Expr::Var(n) => vec!["var".into(), var_hash(&n.name, global, local)],
        Expr::Lam(n) => {
            let ctx = with_name(local, &n.param);
            vec!["lam".into(), alpha_expr(&n.body, global, &ctx).to_string()]
        }
        Expr::App(n) => vec!["app".into(), alpha_expr(&n.fun, global, local).to_string(), alpha_expr(&n.arg, global, local).to_string()],
        Expr::Let { node } => {
            let rhs_ctx = if node.rec { with_name(local, &node.name) } else { local.to_vec() };
            let body_ctx = with_name(local, &node.name);
            vec![if node.rec { "letrec" } else { "let" }.into(), alpha_expr(&node.rhs, global, &rhs_ctx).to_string(), alpha_expr(&node.body, global, &body_ctx).to_string()]
        }
        Expr::Record(n) => {
            let mut v = vec!["record".into()];
            for f in &n.fields { v.push(f.label.clone()); v.push(alpha_expr(&f.value, global, local).to_string()); }
            v
        }
        Expr::Field(n) => vec!["field".into(), n.label.clone(), alpha_expr(&n.record, global, local).to_string()],
        Expr::Extend(n) => {
            let mut v = vec!["extend".into(), alpha_expr(&n.base, global, local).to_string()];
            for f in &n.fields { v.push(f.label.clone()); v.push(alpha_expr(&f.value, global, local).to_string()); }
            v
        }
        Expr::Tag(n) => {
            let mut v = vec!["tag".into(), n.label.clone()];
            if let Some(p) = &n.payload { v.push(alpha_expr(p, global, local).to_string()); }
            v
        }
        Expr::Match(n) => {
            let mut v = vec!["match".into(), alpha_expr(&n.scrutinee, global, local).to_string()];
            for arm in &n.arms {
                let mut ctx = local.to_vec();
                v.push(pattern_hash(&arm.pattern.pattern));
                collect_pattern_names(&arm.pattern.pattern, &mut ctx);
                v.push(alpha_expr(&arm.body, global, &ctx).to_string());
            }
            v
        }
    };
    hstring(&parts)
}

fn pattern_hash(p: &Pattern) -> String {
    match p {
        Pattern::Wildcard => "_".into(),
        Pattern::Name(_) => "name".into(),
        Pattern::Tag(l, p) => format!("tag:{l}:{}", p.as_ref().map(|x| pattern_hash(x)).unwrap_or_default()),
    }
}

fn collect_pattern_names(p: &Pattern, out: &mut Vec<String>) {
    match p {
        Pattern::Name(n) => out.push(n.clone()),
        Pattern::Tag(_, p) => if let Some(p) = p { collect_pattern_names(p, out) },
        Pattern::Wildcard => {}
    }
}

pub fn assign(program: &Program) -> Identity {
    let globals: HashSet<String> = program.decls.iter().map(|d| d.name.clone()).collect();
    let mut identity = Identity::default();
    let mut decl_counter: BTreeMap<u64, usize> = BTreeMap::new();
    for (di, decl) in program.decls.iter().enumerate() {
        let ctx = if decl.rec { vec![decl.name.clone()] } else { Vec::new() };
        let ah = alpha_expr(&decl.rhs, &globals, &ctx);
        let ord = decl_counter.entry(ah).or_insert(0);
        let decl_id = if *ord == 0 { format!("decl/{ah:016x}") } else { format!("decl/{ah:016x}#{di}") };
        *ord += 1;
        identity.nodes.insert(decl.name_span, NodeInfo { id: format!("{decl_id}/binder"), kind: "binder".into(), label: decl.name.clone(), span: decl.name_span });
        identity.spans_by_id.insert(format!("{decl_id}/binder"), decl.name_span);
        assign_expr(&decl.rhs, &decl_id, "rhs", &globals, &ctx, &mut identity);
    }
    if let Some(result) = &program.result {
        assign_expr(result, "root", "result", &globals, &[], &mut identity);
    }
    identity
}

fn assign_expr(e: &Expr, parent: &str, role: &str, globals: &HashSet<String>, local: &[String], id: &mut Identity) {
    let ah = alpha_expr(e, globals, local);
    let node_id = format!("{parent}/{role}/{ah:016x}");
    let (kind, label) = kind_label(e);
    id.nodes.insert(e.span(), NodeInfo { id: node_id.clone(), kind: kind.into(), label, span: e.span() });
    id.spans_by_id.insert(node_id.clone(), e.span());
    match e {
        Expr::Lit(_) | Expr::Var(_) | Expr::Tag(TagNode { payload: None, .. }) => {}
        Expr::Lam(n) => {
            id.nodes.insert(n.var_span, NodeInfo { id: format!("{node_id}/param"), kind: "binder".into(), label: n.param.clone(), span: n.var_span });
            id.spans_by_id.insert(format!("{node_id}/param"), n.var_span);
            assign_expr(&n.body, &node_id, "body", globals, &with_name(local, &n.param), id);
        }
        Expr::App(n) => {
            assign_expr(&n.fun, &node_id, "fun", globals, local, id);
            assign_expr(&n.arg, &node_id, "arg", globals, local, id);
        }
        Expr::Let { node } => {
            id.nodes.insert(node.name_span, NodeInfo { id: format!("{node_id}/binder"), kind: "binder".into(), label: node.name.clone(), span: node.name_span });
            id.spans_by_id.insert(format!("{node_id}/binder"), node.name_span);
            let rhs_ctx = if node.rec { with_name(local, &node.name) } else { local.to_vec() };
            assign_expr(&node.rhs, &node_id, "rhs", globals, &rhs_ctx, id);
            assign_expr(&node.body, &node_id, "body", globals, &with_name(local, &node.name), id);
        }
        Expr::Record(n) => for f in &n.fields { assign_expr(&f.value, &node_id, &format!("field:{}", f.label), globals, local, id); },
        Expr::Field(n) => assign_expr(&n.record, &node_id, "record", globals, local, id),
        Expr::Extend(n) => {
            assign_expr(&n.base, &node_id, "base", globals, local, id);
            for f in &n.fields { assign_expr(&f.value, &node_id, &format!("field:{}", f.label), globals, local, id); }
        }
        Expr::Tag(n) => if let Some(p) = &n.payload { assign_expr(p, &node_id, "payload", globals, local, id); },
        Expr::Match(n) => {
            assign_expr(&n.scrutinee, &node_id, "scrutinee", globals, local, id);
            for (i, arm) in n.arms.iter().enumerate() {
                let mut ctx = local.to_vec();
                let pid = format!("{node_id}/arm{i}/pattern");
                id.nodes.insert(arm.pattern.span, NodeInfo { id: pid.clone(), kind: "pattern".into(), label: pattern_text(&arm.pattern.pattern), span: arm.pattern.span });
                id.spans_by_id.insert(pid, arm.pattern.span);
                collect_pattern_names(&arm.pattern.pattern, &mut ctx);
                assign_expr(&arm.body, &node_id, &format!("arm{i}/body"), globals, &ctx, id);
            }
        }
    }
}

fn pattern_text(p: &Pattern) -> String {
    match p { Pattern::Wildcard => "_".into(), Pattern::Name(n) => n.clone(), Pattern::Tag(l, p) => format!("{l}({})", p.as_ref().map(pattern_text).unwrap_or_default()) }
}

fn kind_label(e: &Expr) -> (&'static str, String) {
    match e {
        Expr::Lit(n) => ("literal", format!("{:?}", n.value)),
        Expr::Var(n) => ("variable", n.name.clone()),
        Expr::Lam(_) => ("lambda", "λ".into()),
        Expr::App(_) => ("application", "app".into()),
        Expr::Let { node } => ("let", node.name.clone()),
        Expr::Record(n) => ("record", format!("{{ {} }}", n.fields.iter().map(|f| f.label.clone()).collect::<Vec<_>>().join(", "))),
        Expr::Field(n) => ("field-access", format!(".{}", n.label)),
        Expr::Extend(n) => ("record-extend", format!("{{+ {} }}", n.fields.iter().map(|f| f.label.clone()).collect::<Vec<_>>().join(", "))),
        Expr::Tag(n) => ("variant", n.label.clone()),
        Expr::Match(_) => ("match", "match".into()),
    }
}
