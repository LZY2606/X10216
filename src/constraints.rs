//! Constraint generation + solving driver.
//!
//! Following classic algorithm W, equations are generated in source order and
//! solved eagerly, but every equation is also recorded. Re-solving any subset
//! of recorded equations on a fresh solver reproduces the same result, which is
//! what makes unsat-core extraction and constraint disabling sound.

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use serde::Serialize;

use crate::ast::{Ast, Expr, NodeData, NodeId, Pat};
use crate::parser::BUILTINS;
use crate::solver::{Solver, UnifyError};
use crate::types::{Origin, RcStr, Row, Span, TyKind, TypeArena, TypeId};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub struct Cid(pub u32);

#[derive(Clone, Debug)]
pub struct Constraint {
    pub id: Cid,
    pub kind: ConstraintKind,
    pub origin_node: Option<u32>,
    pub span: Option<Span>,
    pub fingerprint: u64,
}

#[derive(Clone, Debug)]
pub enum ConstraintKind {
    /// Two types must be equal.
    Eq(TypeId, TypeId),
    /// Generalize `target` from `value` at this let node.
    Generalize {
        let_node: u32,
        value_node: u32,
        binder: Option<u32>,
        expansive: bool,
    },
    /// Instantiate the scheme of `binder` for occurrence `node`.
    Instantiate { node: u32, binder: Option<u32> },
}

#[derive(Clone, Debug, Serialize)]
pub struct ConstraintJson {
    pub id: u32,
    pub kind: String,
    pub node: Option<u32>,
    pub span: Option<Span>,
    pub fingerprint: String,
    pub detail: String,
}

pub struct Scheme {
    pub node: TypeId,
    pub quantifiers: Vec<u32>,
    pub gen_level: u32,
}

struct Binding {
    ty: TypeId,
    scheme: Option<Rc<Scheme>>,
    /// Union-find vars that were generalized (for display).
    span: Option<Span>,
}

pub struct Generator<'a> {
    pub ast: &'a Ast,
    pub arena: &'a TypeArena,
    pub solver: &'a Solver,
    pub constraints: RefCell<Vec<Constraint>>,
    env: RefCell<Vec<Vec<(RcStr, Binding)>>>,
    level: u32,
    pub gen_steps: RefCell<Vec<GenStep>>,
}

#[derive(Clone, Debug, Serialize)]
pub struct GenStep {
    pub index: u32,
    pub kind: String,
    pub node: Option<u32>,
    pub summary: String,
}

impl<'a> Generator<'a> {
    pub fn new(ast: &'a Ast, arena: &'a TypeArena, solver: &'a Solver) -> Self {
        let mut g = Generator {
            ast, arena, solver,
            constraints: RefCell::new(Vec::new()),
            env: RefCell::new(vec![Vec::new()]),
            level: 0,
            gen_steps: RefCell::new(Vec::new()),
        };
        g.install_builtins();
        g
    }

    fn install_builtins(&mut self) {
        let int = self.arena.con("Int");
        let bool = self.arena.con("Bool");
        let string = self.arena.con("String");
        let mut add = |name: &str, ty: TypeId| {
            self.env.borrow_mut()[0].push((Rc::from(name), Binding {
                ty, scheme: None, span: None,
            }));
        };
        let bin = |a: TypeId, b: TypeId, c: TypeId| -> TypeId {
            self.arena.alloc(TyKind::Arr(a, self.arena.alloc(TyKind::Arr(b, c),
                Origin::Builtin { name: "->".into() }, None)),
                Origin::Builtin { name: "->".into() }, None)
        };
        add("+", bin(int, int, int));
        add("-", bin(int, int, int));
        add("*", bin(int, int, int));
        add("/", bin(int, int, int));
        add("<", bin(int, int, bool));
        add("&&", bin(bool, bool, bool));
        add("||", bin(bool, bool, bool));
        add("!", self.arena.alloc(TyKind::Arr(bool, bool),
            Origin::Builtin { name: "!".into() }, None));
        add("print_int", self.arena.alloc(TyKind::Arr(int, int),
            Origin::Builtin { name: "print_int".into() }, None));
        add("print_str", self.arena.alloc(TyKind::Arr(string, string),
            Origin::Builtin { name: "print_str".into() }, None));
        add("int_to_str", self.arena.alloc(TyKind::Arr(int, string),
            Origin::Builtin { name: "int_to_str".into() }, None));
        add("string_concat", bin(string, string, string));
    }

    fn push_scope(&self) {
        self.env.borrow_mut().push(Vec::new());
    }
    fn pop_scope(&self) {
        self.env.borrow_mut().pop();
    }
    fn bind(&self, name: RcStr, ty: TypeId, scheme: Option<Rc<Scheme>>,
            span: Option<Span>) {
        self.env.borrow_mut().last_mut().unwrap()
            .push((name, Binding { ty, scheme, span }));
    }
    fn lookup(&self, name: &str) -> Option<Binding> {
        for s in self.env.borrow().iter().rev() {
            for (n, b) in s.iter().rev() {
                if n.as_ref() == name {
                    return Some(b.clone());
                }
            }
        }
        None
    }

    fn fresh(&self, origin: Origin, span: Option<Span>) -> TypeId {
        self.arena.fresh_var(origin, span)
    }

    fn add_eq(&self, a: TypeId, b: TypeId, node: NodeId) -> Result<Cid, UnifyError> {
        let id = Cid(self.constraints.borrow().len() as u32);
        let span = self.ast.span_of(node);
        let fp = self.constraint_fingerprint(
            &ConstraintKind::Eq(a, b), node);
        self.constraints.borrow_mut().push(Constraint {
            id,
            kind: ConstraintKind::Eq(a, b),
            origin_node: Some(node.0),
            span: Some(span),
            fingerprint: fp,
        });
        self.solver.unify(self.arena, a, b, id.0).inspect_err(|_| {})?;
        Ok(id)
    }

    /// Collect free unification variables of a type node (bounded depth).
    fn free_vars(&self, id: TypeId) -> BTreeSet<u32> {
        let mut out = BTreeSet::new();
        self.collect_free(id, &mut out, 0);
        out
    }

    fn collect_free(&self, id: TypeId, out: &mut BTreeSet<u32>, depth: u32) {
        if depth > 96 { return; }
        let r = self.solver.resolve(self.arena, id);
        match self.arena.view(r, |n| n.kind.clone()) {
            TyKind::Var(v) => {
                let root = self.solver.find_pub(v);
                out.insert(root);
            }
            TyKind::Con(_) => {}
            TyKind::Arr(a, b) => {
                self.collect_free(a, out, depth + 1);
                self.collect_free(b, out, depth + 1);
            }
            TyKind::Record(row) | TyKind::Variant(row) => {
                for (_, t) in &row.fields {
                    self.collect_free(*t, out, depth + 1);
                }
            }
        }
    }

    fn env_free(&self, until: usize) -> BTreeSet<u32> {
        let mut out = BTreeSet::new();
        let scopes = self.env.borrow();
        for s in &scopes[..until] {
            for (_, b) in s {
                if let Some(sch) = &b.scheme {
                    out.extend(sch.quantifiers.iter().copied());
                }
                for v in self.free_vars(b.ty) {
                    out.insert(v);
                }
            }
        }
        out
    }

    fn is_expansive(&self, id: NodeId) -> bool {
        // Value restriction: only "syntactic values" generalize.
        match &self.ast.nodes[id.0 as usize].data {
            NodeData::E(Expr::Int(_) | Expr::Bool(_) | Expr::Str(_)
                | Expr::Unit | Expr::RecordLit(_) | Expr::Variant { .. }
                | Expr::Lam { .. }) => false,
            NodeData::E(Expr::ResolvedVar { .. }) => false,
            NodeData::E(Expr::Extend { base, fields }) => {
                base.map(|b| self.is_expansive(b)).unwrap_or(false)
                    || fields.iter().any(|(_, n)| self.is_expansive(*n))
            }
            _ => true,
        }
    }

    pub fn gen(&mut self, id: NodeId) -> Result<TypeId, UnifyError> {
        let span = self.ast.span_of(id);
        let node = id.0;
        let ty = match self.ast.nodes[id.0 as usize].data.clone() {
            NodeData::E(Expr::Int(_)) => self.arena.alloc(TyKind::Con("Int".into()),
                Origin::Const { node }, Some(span)),
            NodeData::E(Expr::Bool(_)) => self.arena.alloc(TyKind::Con("Bool".into()),
                Origin::Const { node }, Some(span)),
            NodeData::E(Expr::Str(_)) => self.arena.alloc(TyKind::Con("String".into()),
                Origin::Const { node }, Some(span)),
            NodeData::E(Expr::Unit) => self.arena.alloc(TyKind::Con("Unit".into()),
                Origin::Const { node }, Some(span)),
            NodeData::E(Expr::ResolvedVar { name, binder }) => {
                self.instantiate_var(id, name, binder, span)?
            }
            NodeData::E(Expr::Var(_)) => {
                self.fresh(Origin::Fresh { node, label: "unresolved".into() }, Some(span))
            }
            NodeData::E(Expr::Lam { param, body }) => {
                let pt = self.fresh(Origin::Node { node: param.0 },
                    Some(self.ast.span_of(param)));
                self.push_scope();
                self.bind_pattern(param, pt)?;
                let bt = self.gen(body)?;
                self.pop_scope();
                let label: RcStr = "arrow".into();
                self.arena.alloc(TyKind::Arr(pt, bt),
                    Origin::Fresh { node, label }, Some(span))
            }
            NodeData::E(Expr::App { fun, arg }) => {
                let ft = self.gen(fun)?;
                let at = self.gen(arg)?;
                let rt = self.fresh(Origin::Fresh { node, label: "result".into() },
                    Some(span));
                let dt = self.arena.alloc(TyKind::Arr(at, rt),
                    Origin::Fresh { node, label: "dom/cod".into() }, Some(span));
                self.add_eq(ft, dt, fun)?;
                rt
            }
            NodeData::E(Expr::If { cond, then_b, else_b }) => {
                let ct = self.gen(cond)?;
                let tt = self.gen(then_b)?;
                let et = self.gen(else_b)?;
                let bool_ty = self.arena.con("Bool");
                self.add_eq(ct, bool_ty, cond)?;
                self.add_eq(tt, et, id)?;
                tt
            }
            NodeData::E(Expr::Seq(xs)) => {
                let mut last = self.arena.con("Unit");
                for x in xs {
                    last = self.gen(x)?;
                }
                last
            }
            NodeData::E(Expr::Let { pat, value, body, rec_kw }) => {
                self.gen_let(id, pat, value, body, rec_kw)?
            }
            NodeData::E(Expr::RecordLit(fields)) => self.gen_record(id, &fields, None, true)?,
            NodeData::E(Expr::Record(fields)) => self.gen_record(id, &fields, None, true)?,
            NodeData::E(Expr::Extend { base, fields }) =>
                self.gen_record(id, &fields, base, base.is_none())?,
            NodeData::E(Expr::Field { record, name }) => {
                let rt = self.gen(record)?;
                let ft = self.fresh(Origin::Fresh {
                    node, label: Rc::from(format!("field {name}")) }, Some(span));
                let tail = self.solver.new_var_pub(self.level);
                let row_ty = self.arena.row(
                    vec![(name, ft)], Some(tail), false,
                    Origin::Node { node }, Some(span));
                self.add_eq(rt, row_ty, id)?;
                ft
            }
            NodeData::E(Expr::Variant { label, payload }) => {
                let pt = self.gen(payload)?;
                let tail = self.solver.new_var_pub(self.level);
                self.arena.row(vec![(label, pt)], Some(tail), true,
                    Origin::Node { node }, Some(span))
            }
            NodeData::E(Expr::Match { scrut, arms }) => {
                self.gen_match(id, scrut, arms)?
            }
            _ => self.fresh(Origin::Fresh { node, label: "unsupported".into() }, Some(span)),
        };
        Ok(ty)
    }

    fn instantiate_var(&self, node: NodeId, name: RcStr, binder: Option<NodeId>,
            span: Span) -> Result<TypeId, UnifyError> {
        let b = match self.lookup(&name) {
            Some(b) => b,
            None => {
                return Ok(self.fresh(Origin::VarRef {
                    node: node.0, binder: binder.map(|b| b.0).unwrap_or(u32::MAX),
                }, Some(span)));
            }
        };
        let cid = Cid(self.constraints.borrow().len() as u32);
        let inst_kind = ConstraintKind::Instantiate {
            node: node.0, binder: binder.map(|b| b.0),
        };
        let fp = self.constraint_fingerprint(&inst_kind, node);
        self.constraints.borrow_mut().push(Constraint {
            id: cid, kind: inst_kind,
            origin_node: Some(node.0), span: Some(span), fingerprint: fp,
        });
        let result = if let Some(sch) = &b.scheme {
            // rename quantified vars to fresh ones
            let mut mapping: BTreeMap<u32, TypeId> = BTreeMap::new();
            for q in &sch.quantifiers {
                let nt = self.fresh(Origin::Inst {
                    node: node.0, scheme: sch.gen_level,
                }, Some(span));
                mapping.insert(*q, nt);
            }
            self.rename(sch.node, &mut mapping, 0)
        } else {
            b.ty
        };
        self.gen_steps.borrow_mut().push(GenStep {
            index: self.gen_steps.borrow().len() as u32,
            kind: "instantiate".into(),
            node: Some(node.0),
            summary: if b.scheme.is_some() {
                format!("instantiate polymorphic '{}'", name)
            } else {
                format!("use monomorphic '{}'", name)
            },
        });
        Ok(result)
    }

    /// Deep-copy a scheme node, replacing quantified roots with fresh vars.
    fn rename(&self, id: TypeId, mapping: &mut BTreeMap<u32, TypeId>,
            depth: u32) -> TypeId {
        let node = self.arena.view(id, |n| n.kind.clone());
        let origin = self.arena.view(id, |n| n.origin.clone());
        let sp = self.arena.view(id, |n| n.span);
        match node {
            TyKind::Var(v) => {
                let root = self.solver.find_pub(v);
                if let Some(t) = mapping.get(&root) {
                    *t
                } else {
                    // Free (monomorphic/ambient) variable: keep identity so all
                    // instantiations share the same union-find variable.
                    self.arena.alloc(TyKind::Var(root), origin, sp)
                }
            }
            TyKind::Con(c) => self.arena.alloc(TyKind::Con(c), origin, sp),
            TyKind::Arr(a, b) => {
                let a2 = self.rename(a, mapping, depth + 1);
                let b2 = self.rename(b, mapping, depth + 1);
                self.arena.alloc(TyKind::Arr(a2, b2), origin, sp)
            }
            TyKind::Record(r) => {
                let fs: Vec<_> = r.fields.into_iter()
                    .map(|(n, t)| (n, self.rename(t, mapping, depth + 1)))
                    .collect();
                self.arena.row(fs, None, false, origin, sp)
            }
            TyKind::Variant(r) => {
                let fs: Vec<_> = r.fields.into_iter()
                    .map(|(n, t)| (n, self.rename(t, mapping, depth + 1)))
                    .collect();
                self.arena.row(fs, None, true, origin, sp)
            }
        }
    }

    fn gen_let(&mut self, id: NodeId, pat: NodeId, value: NodeId, body: NodeId,
            rec_kw: bool) -> Result<TypeId, UnifyError> {
        let span = self.ast.span_of(id);
        self.push_scope();
        self.level += 1;
        // For recursive lets, pre-bind monomorphic fresh types.
        let mut pre_types: BTreeMap<u32, TypeId> = BTreeMap::new();
        if rec_kw {
            let binders = collect_binders(self.ast, pat);
            for b in binders {
                let t = self.fresh(Origin::Fresh { node: b, label: "rec".into() },
                    Some(self.ast.span_of(NodeId(b))));
                pre_types.insert(b, t);
                let name = pat_name(self.ast, NodeId(b));
                self.bind(Rc::from(name.as_str()), t, None,
                    Some(self.ast.span_of(NodeId(b))));
            }
        }
        let vt = self.gen(value)?;
        let expansive = self.is_expansive(value);

        let cid = Cid(self.constraints.borrow().len() as u32);
        let binder_id = pat_binder(pat);
        let kind = ConstraintKind::Generalize {
            let_node: id.0, value_node: value.0,
            binder: binder_id, expansive,
        };
        let fp = self.constraint_fingerprint(&kind, id);
        self.constraints.borrow_mut().push(Constraint {
            id: cid, kind, origin_node: Some(id.0),
            span: Some(span), fingerprint: fp,
        });

        let scope_count = self.env.borrow().len();
        let envfv = self.env_free(scope_count - 1);
        let mut quant: Vec<u32> = Vec::new();
        if !expansive {
            for v in self.free_vars(vt) {
                if !envfv.contains(&v)
                    && self.solver.level_pub(v) >= self.level
                {
                    quant.push(v);
                }
            }
        }
        let scheme = if quant.is_empty() {
            None
        } else {
            Some(Rc::new(Scheme {
                node: self.solver.resolve(self.arena, vt),
                quantifiers: quant.clone(),
                gen_level: self.level,
            }))
        };
        self.gen_steps.borrow_mut().push(GenStep {
            index: self.gen_steps.borrow().len() as u32,
            kind: if expansive { "generalize:blocked".into() }
                  else { "generalize".into() },
            node: Some(id.0),
            summary: if expansive {
                format!("value restriction: {} stays monomorphic",
                    pat_name(self.ast, pat))
            } else {
                format!("generalize {} over {} variable(s)",
                    pat_name(self.ast, pat), quant.len())
            },
        });

        // bind pattern names (additive for rec; already bound monomorphically)
        self.bind_pattern_with(pat, vt, scheme.clone())?;
        let bt = self.gen(body)?;
        self.level -= 1;
        self.pop_scope();
        Ok(bt)
    }

    fn bind_pattern(&self, pat: NodeId, ty: TypeId) -> Result<(), UnifyError> {
        self.bind_pattern_with(pat, ty, None)
    }

    fn bind_pattern_with(&self, pat: NodeId, ty: TypeId,
            scheme: Option<Rc<Scheme>>) -> Result<(), UnifyError> {
        match self.ast.nodes[pat.0 as usize].data.clone() {
            NodeData::P(Pat::PVar(n)) if n.as_ref() != "_" => {
                self.bind(n, ty, scheme, Some(self.ast.span_of(pat)));
            }
            NodeData::P(Pat::PWild) | NodeData::P(Pat::PVar(_)) => {}
            NodeData::P(Pat::PRecord(fields)) => {
                for (name, sub) in fields {
                    let ft = self.fresh(Origin::Node { node: sub.0 },
                        Some(self.ast.span_of(sub)));
                    let tail = self.solver.new_var_pub(self.level);
                    let row_ty = self.arena.row(vec![(name, ft)], Some(tail),
                        false, Origin::Node { node: pat.0 },
                        Some(self.ast.span_of(pat)));
                    self.add_eq(ty, row_ty, pat)?;
                    self.bind_pattern_with(sub, ft, None)?;
                }
            }
            NodeData::P(Pat::PVariant { label, payload }) => {
                let pt = self.fresh(Origin::Node { node: payload.0 },
                    Some(self.ast.span_of(payload)));
                let tail = self.solver.new_var_pub(self.level);
                let vty = self.arena.row(vec![(label, pt)], Some(tail), true,
                    Origin::Node { node: pat.0 }, Some(self.ast.span_of(pat)));
                self.add_eq(ty, vty, pat)?;
                self.bind_pattern_with(payload, pt, None)?;
            }
            NodeData::Bind { .. } | NodeData::E(_) => {}
        }
        Ok(())
    }

    fn gen_record(&mut self, id: NodeId,
            fields: &[(RcStr, NodeId)], base: Option<NodeId>,
            closed: bool) -> Result<TypeId, UnifyError> {
        let span = self.ast.span_of(id);
        let mut ftys: Vec<(RcStr, TypeId)> = Vec::new();
        for (name, e) in fields {
            let t = self.gen(*e)?;
            ftys.push((name.clone(), t));
        }
        // If extending a base expression, the tail starts as its row.
        let tail = if let Some(b) = base {
            let bt = self.gen(b)?;
            // introduce a fresh tail var forced equal to the base row so that
            // the base's other fields survive
            let tv = self.solver.new_var_pub(self.level);
            let open_tail = self.arena.alloc(TyKind::Var(tv),
                Origin::Node { node: b.0 }, Some(self.ast.span_of(b)));
            self.add_eq(bt, open_tail, b)?;
            Some(tv)
        } else if closed {
            None
        } else {
            Some(self.solver.new_var_pub(self.level))
        };
        Ok(self.arena.row(ftys, tail, false,
            Origin::Node { node: id.0 }, Some(span)))
    }

    fn gen_match(&mut self, id: NodeId, scrut: NodeId,
            arms: Vec<(NodeId, NodeId)>) -> Result<TypeId, UnifyError> {
        let span = self.ast.span_of(id);
        let st = self.gen(scrut)?;
        let result = self.fresh(Origin::Fresh { node: id.0, label: "match".into() },
            Some(span));
        let mut first = true;
        for (p, body) in arms {
            self.push_scope();
            let pt = self.pattern_type(p)?;
            self.add_eq(st, pt, p)?;
            self.bind_pattern(p, pt)?;
            let bt = self.gen(body)?;
            self.pop_scope();
            if first {
                self.add_eq(result, bt, body)?;
                first = false;
            } else {
                self.add_eq(result, bt, body)?;
            }
        }
        Ok(result)
    }

    fn pattern_type(&mut self, pat: NodeId) -> Result<TypeId, UnifyError> {
        Ok(match self.ast.nodes[pat.0 as usize].data.clone() {
            NodeData::P(Pat::PVar(_)) | NodeData::P(Pat::PWild) => {
                self.fresh(Origin::Node { node: pat.0 },
                    Some(self.ast.span_of(pat)))
            }
            NodeData::P(Pat::PRecord(fields)) => {
                let mut fs = Vec::new();
                for (name, sub) in fields {
                    fs.push((name, self.pattern_type(sub)?));
                }
                self.arena.row(fs, None, false, Origin::Node { node: pat.0 },
                    Some(self.ast.span_of(pat)))
            }
            NodeData::P(Pat::PVariant { label, payload }) => {
                let pt = self.pattern_type(payload)?;
                let tail = self.solver.new_var_pub(self.level);
                self.arena.row(vec![(label, pt)], Some(tail), true,
                    Origin::Node { node: pat.0 }, Some(self.ast.span_of(pat)))
            }
            _ => self.fresh(Origin::Node { node: pat.0 }, None),
        })
    }

    // ---- stable fingerprints --------------------------------------------
    /// Canonical child position path of a node within the AST. Variable
    /// references contribute the binder path they resolve to, so renaming a
    /// binder cannot shift identities elsewhere.
    fn canonical_path(&self, target: NodeId) -> String {
        build_path(self.ast, self.ast.root, target.0).unwrap_or_else(|| format!("n{}", target.0))
    }

    fn constraint_fingerprint(&self, kind: &ConstraintKind, node: NodeId) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut h = SimpleHasher(0xcbf29ce484222325);
        let path = self.canonical_path(node);
        match kind {
            ConstraintKind::Eq(_, _) => {
                0u8.hash(&mut h);
                path.hash(&mut h);
                // distinguish multiple equations attached to one node by
                // including the equation ordinal local to the node.
                let local = self.constraints.borrow().iter()
                    .filter(|c| c.origin_node == Some(node.0)).count() as u32;
                local.hash(&mut h);
            }
            ConstraintKind::Generalize { value_node, expansive, .. } => {
                1u8.hash(&mut h);
                path.hash(&mut h);
                value_node.hash(&mut h);
                expansive.hash(&mut h);
            }
            ConstraintKind::Instantiate { binder, .. } => {
                2u8.hash(&mut h);
                path.hash(&mut h);
                binder.hash(&mut h);
            }
        }
        h.0
    }
}

/// FNV-1a 64 hasher.
struct SimpleHasher(u64);
impl std::hash::Hasher for SimpleHasher {
    fn finish(&self) -> u64 { self.0 }
    fn write(&mut self, bytes: &[u8]) {
        for b in bytes {
            self.0 ^= *b as u64;
            self.0 = self.0.wrapping_mul(0x100000001b3);
        }
    }
}

// ---- AST helpers ----------------------------------------------------------

fn build_path(ast: &Ast, root: NodeId, target: u32) -> Option<String> {
    fn go(ast: &Ast, id: NodeId, target: u32, prefix: &str) -> Option<String> {
        if id.0 == target {
            return Some(prefix.to_string());
        }
        let kids = labeled_children(ast, id);
        for (i, (label, child)) in kids.into_iter().enumerate() {
            let mut p = String::from(prefix);
            p.push('/');
            p.push_str(label);
            p.push('#');
            use std::fmt::Write as _;
            let _ = write!(p, "{}", i);
            if let Some(r) = go(ast, child, target, &p) {
                return Some(r);
            }
        }
        None
    }
    go(ast, root, target, "")
}

/// Stable child ordering with semantic labels.
fn labeled_children(ast: &Ast, id: NodeId) -> Vec<(&'static str, NodeId)> {
    let mut out = Vec::new();
    match &ast.nodes[id.0 as usize].data {
        NodeData::E(e) => match e {
            Expr::Lam { param, body } => {
                out.push(("param", *param));
                out.push(("body", *body));
            }
            Expr::App { fun, arg } => {
                out.push(("fun", *fun));
                out.push(("arg", *arg));
            }
            Expr::Let { pat, value, body, .. } => {
                out.push(("pat", *pat));
                out.push(("value", *value));
                out.push(("body", *body));
            }
            Expr::If { cond, then_b, else_b } => {
                out.push(("cond", *cond));
                out.push(("then", *then_b));
                out.push(("else", *else_b));
            }
            Expr::Field { record, .. } => out.push(("record", *record)),
            Expr::Record(xs) | Expr::RecordLit(xs) | Expr::Extend { fields: xs, .. } => {
                for (_, n) in xs { out.push(("field", *n)); }
                if let Expr::Extend { base: Some(b), .. } = e {
                    out.push(("base", *b));
                }
            }
            Expr::Variant { payload, .. } => out.push(("payload", *payload)),
            Expr::Match { scrut, arms } => {
                out.push(("scrut", *scrut));
                for (p, b) in arms {
                    out.push(("armpat", *p));
                    out.push(("armbody", *b));
                }
            }
            Expr::Seq(xs) => for n in xs { out.push(("seq", *n)); },
            _ => {}
        },
        NodeData::P(Pat::PRecord(fs)) => {
            for (_, n) in fs { out.push(("pfield", *n)); }
        }
        NodeData::P(Pat::PVariant { payload, .. }) => out.push(("ppayload", *payload)),
        _ => {}
    }
    out
}

fn collect_binders(ast: &Ast, pat: NodeId) -> Vec<u32> {
    let mut out = Vec::new();
    fn go(ast: &Ast, p: NodeId, out: &mut Vec<u32>) {
        match &ast.nodes[p.0 as usize].data {
            NodeData::P(Pat::PVar(n)) if n.as_ref() != "_" => out.push(p.0),
            NodeData::P(Pat::PRecord(fs)) =>
                for (_, x) in fs { go(ast, *x, out); },
            NodeData::P(Pat::PVariant { payload, .. }) => go(ast, *payload, out),
            _ => {}
        }
    }
    go(ast, pat, &mut out);
    out
}

fn pat_binder(pat: NodeId) -> Option<u32> {
    Some(pat.0)
}

pub fn pat_name(ast: &Ast, pat: NodeId) -> String {
    match &ast.nodes[pat.0 as usize].data {
        NodeData::P(Pat::PVar(n)) => n.to_string(),
        NodeData::P(Pat::PWild) => "_".into(),
        NodeData::P(Pat::PRecord(_)) => "{record pattern}".into(),
        NodeData::P(Pat::PVariant { label, .. }) => format!("`{label}"),
        _ => "?".into(),
    }
}

/// Scope hash: ordered binding names + binder paths visible at a let node.
pub fn scope_hash(ast: &Ast) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = SimpleHasher(0x9e3779b97f4a7c15);
    for b in BUILTINS {
        b.hash(&mut h);
    }
    // Gather binders in source order with their canonical paths.
    for (i, n) in ast.nodes.iter().enumerate() {
        if matches!(n.data, NodeData::P(Pat::PVar(_))) {
            build_path(ast, ast.root, i as u32).hash(&mut h);
        }
    }
    h.finish()
}
