use crate::ast::*;
use crate::constraint::*;
use crate::types::*;
use std::collections::HashMap;

#[derive(Clone)]
struct Entry {
    binder: NodeId,
    scheme: Option<Scheme>,
    /// For recursive bindings before generalization: the fresh ty.
    pending: Option<Ty>,
}

pub struct Generator<'a> {
    prog_nodes: HashMap<NodeId, NodeInfo>,
    origins: Vec<(MetaVar, MetaOrigin)>,
    cons: Vec<Constraint>,
    bindings: Vec<BindingInfo>,
    unbound: Vec<Unbound>,
    env: Vec<(String, Entry)>,
    /// Metavars that must not be generalized (free in the typing environment).
    ambient: Vec<MetaVar>,
    scope: ScopeId,
    genv: Vec<(String, NodeId)>,
    source: &'a str,
}

const EQ_TAGS: &[&str] = &[
    "app-arrow",
    "app-arg",
    "if-then",
    "if-else",
    "let-body",
    "field-record",
    "field-present",
    "field-type",
    "variant-tag",
    "match-scrut",
    "match-arm-payload",
    "match-arm-result",
];

impl<'a> Generator<'a> {
    pub fn new(prog: &'a Program) -> Self {
        let nodes = prog.nodes.iter().map(|n| (n.id, n.clone())).collect();
        Generator {
            prog_nodes: nodes,
            origins: Vec::new(),
            cons: Vec::new(),
            bindings: Vec::new(),
            unbound: Vec::new(),
            env: Vec::new(),
            ambient: Vec::new(),
            scope: ScopeId(seed_str("root-scope")),
            genv: Vec::new(),
            source: &prog.source,
        }
    }

    pub fn generate(mut self, prog: &Program) -> GenResult {
        let root_ty = self.gen_expr(&prog.root);
        self.cons.sort_by_key(|c| (c.scope, c.tag.clone(), c.cid));
        self.cons.dedup_by_key(|c| c.cid);
        GenResult {
            constraints: self.cons,
            bindings: self.bindings,
            root_ty,
            unbound: self.unbound,
            origins: self.origins,
        }
    }

    fn node_info(&self, id: NodeId) -> Option<&NodeInfo> {
        self.prog_nodes.get(&id)
    }

    fn fresh(&mut self, origin: MetaOrigin) -> MetaVar {
        // Deterministic index from the structural origin.
        let idx = self.origins.len() as u32;
        let key = origin_key(origin, idx);
        let mv = MetaVar(key);
        if !self.origins.iter().any(|(m, _)| m == &mv) {
            self.origins.push((mv, origin));
        }
        mv
    }

    fn eq(&mut self, tag: &str, origin: NodeId, span: Span, left: Ty, right: Ty) {
        debug_assert!(EQ_TAGS.contains(&tag), "unknown eq tag {}", tag);
        let cid = Cid(fnv1a(origin.0, tag.as_bytes()));
        self.cons.push(Constraint {
            cid,
            origin,
            span,
            tag: tag.to_string(),
            kind: ConstraintKind::Eq { left, right },
            scope: self.scope,
            genv: self.genv.clone(),
        });
    }

    fn gen_constraint(&mut self, tag: &str, origin: NodeId, kind: ConstraintKind, span: Span) {
        let cid = Cid(fnv1a(fnv1a(origin.0, b"meta-con"), tag.as_bytes()));
        self.cons.push(Constraint {
            cid,
            origin,
            span,
            tag: tag.to_string(),
            kind,
            scope: self.scope,
            genv: self.genv.clone(),
        });
    }

    fn lookup(&self, name: &str) -> Option<&Entry> {
        self.env.iter().rev().find(|(n, _)| n == name).map(|(_, e)| e)
    }

    fn push_scope(&mut self, key: &str) -> ScopeId {
        ScopeId(fnv1a(self.scope.0, key.as_bytes()))
    }

    fn is_value(e: &Expr) -> bool {
        matches!(
            e,
            Expr::Lit(_)
                | Expr::Var(_)
                | Expr::Fun { .. }
                | Expr::Record(_)
                | Expr::Variant { .. }
        )
    }
}
