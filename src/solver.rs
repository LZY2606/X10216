use crate::constraints::*;
use crate::span::Span;
use crate::types::{Kind, RTy, Row, Scheme};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StepDetail {
    NewVar { id: u32, kind: String },
    Bind { con: String, out: u32 },
    Lookup { name: String, poly: bool },
    Instantiate { name: String, copies: BTreeMap<u32, u32> },
    Generalize { name: String, quantified: Vec<u32> },
    UnifyStart { left: String, right: String },
    UnifyOk { left: String, right: String, merged: u32 },
    UnifyFail { message: String },
    ScopeEnter { scope: String },
    ScopeExit { scope: String },
}

#[derive(Clone, Debug, Serialize)]
pub struct Step {
    pub index: usize,
    pub node_path: String,
    pub span: Span,
    pub atom_key: Option<String>,
    pub detail: StepDetail,
}

#[derive(Clone, Debug, Serialize)]
pub struct FieldError {
    pub label: String, // "missing" | "extra"
    pub fields: Vec<String>,
    pub side: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct TypeError {
    pub code: String,
    pub message: String,
    pub span: Span,
    pub atom_key: Option<String>,
    pub occurs_var: Option<u32>,
    pub expected: Option<String>,
    pub found: Option<String>,
    pub row_fields: Vec<FieldError>,
}

#[derive(Clone, Debug, Serialize)]
pub struct SolvedType {
    pub node_path: String,
    pub ty: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct CoreReport {
    pub error_index: usize,
    pub code: String,
    pub span: Span,
    pub minimal_cores: Vec<Vec<String>>,
}

#[derive(Clone, Debug, Serialize)]
pub struct SolveReport {
    pub steps: Vec<Step>,
    pub errors: Vec<TypeError>,
    pub types: Vec<SolvedType>,
    pub root_ty: String,
    pub ok: bool,
    pub cores: Vec<CoreReport>,
}

#[derive(Clone)]
struct Node {
    parent: u32,
    rank: u32,
    kind: Kind,
    desc: Option<Head>,
}

#[derive(Clone)]
enum Head {
    Con(String),
    Fun(u32, u32),
    Row(RowHead),
}

#[derive(Clone)]
struct RowHead {
    variant: bool,
    fields: BTreeMap<String, u32>,
    tail: Option<u32>,
}

struct UF {
    nodes: Vec<Node>,
}

impl UF {
    fn new() -> Self {
        UF { nodes: Vec::new() }
    }
    fn ensure(&mut self, id: u32, kind: Kind, level: u32) {
        while (self.nodes.len() as u32) <= id {
            let i = self.nodes.len() as u32;
            self.nodes.push(Node { parent: i, rank: level, kind, desc: None });
        }
    }
    fn find(&mut self, id: u32) -> u32 {
        let p = self.nodes[id as usize].parent;
        if p == id {
            return id;
        }
        let root = self.find(p);
        self.nodes[id as usize].parent = root;
        root
    }
    fn kind(&self, id: u32) -> Kind {
        self.nodes[id as usize].kind
    }
    fn rank(&self, id: u32) -> u32 {
        self.nodes[id as usize].rank
    }
    fn set_rank(&mut self, id: u32, r: u32) {
        let cur = self.nodes[id as usize].rank;
        if r < cur {
            self.nodes[id as usize].rank = r;
        }
    }
    fn desc(&self, id: u32) -> Option<&Head> {
        self.nodes[id as usize].desc.as_ref()
    }
    fn take_desc(&mut self, id: u32) -> Option<Head> {
        self.nodes[id as usize].desc.take()
    }
    fn set_desc(&mut self, id: u32, h: Head) {
        self.nodes[id as usize].desc = Some(h);
    }
}

pub struct Solver<'a> {
    cp: &'a ConstraintProgram,
    uf: UF,
    level: u32,
    env: Vec<(String, Binding)>,
    steps: Vec<Step>,
    errors: Vec<TypeError>,
    disabled: BTreeSet<String>,
    atom_ok: BTreeMap<String, bool>,
    fresh_hi: u32,
    core_mode: bool,
}

#[derive(Clone)]
enum Binding {
    Mono(u32),
    Scheme(Scheme),
}

pub fn solve(cp: &ConstraintProgram, disabled: &BTreeSet<String>, core_mode: bool) -> SolveReport {
    let mut s = Solver {
        cp,
        uf: UF::new(),
        level: 0,
        env: Vec::new(),
        steps: Vec::new(),
        errors: Vec::new(),
        disabled: disabled.clone(),
        atom_ok: BTreeMap::new(),
        fresh_hi: 1_000_000,
        core_mode,
    };
    let root = s.solve_node(cp.root, 0);
    let root_ty = s.prune(root);
    let root_str = s.render(&root_ty);
    let mut types = Vec::new();
    for n in &cp.nodes {
        if let Some(out) = node_out(n) {
            let t = s.prune(out);
            let rendered = s.render(&t);
            types.push(SolvedType { node_path: n.node_path.clone(), ty: rendered });
        }
    }
    let ok = s.errors.is_empty();
    let cores = if core_mode {
        let errors = s.errors.clone();
        compute_cores(cp, disabled, &errors)
    } else {
        Vec::new()
    };
    SolveReport { steps: s.steps, errors: s.errors, types, root_ty: root_str, ok, cores }
}

fn node_out(n: &CNode) -> Option<u32> {
    match &n.c {
        C::Leaf { ty } => ty.var_id().map(|x| x.0),
        C::Lit { out, .. }
        | C::VarRef { out, .. }
        | C::Abs { out, .. }
        | C::App { out, .. }
        | C::Let { out, .. }
        | C::RecordLit { out, .. }
        | C::FieldAccess { out, .. }
        | C::VariantNew { out, .. }
        | C::Match { out, .. }
        | C::Annot { out, .. } => leaf_var(&cp.nodes[out.0 as usize]),
        C::Eq { .. } => None,
    }
}

fn leaf_var(n: &CNode) -> Option<u32> {
    if let C::Leaf { ty } = &n.c {
        ty.var_id().map(|x| x.0)
    } else {
        None
    }
}

impl<'a> Solver<'a> {
    fn out_var(&self, leaf: CId) -> u32 {
        if let C::Leaf { ty } = &self.cp.nodes[leaf.0 as usize].c {
            ty.var_id().expect("out leaf must be a var").0
        } else {
            panic!("expected leaf")
        }
    }

    fn log(&mut self, node_path: String, span: Span, atom_key: Option<String>, detail: StepDetail) {
        if self.core_mode {
            return;
        }
        let index = self.steps.len();
        self.steps.push(Step { index, node_path, span, atom_key, detail });
    }

    fn new_star(&mut self) -> u32 {
        let id = self.fresh_hi;
        self.fresh_hi += 1;
        self.uf.ensure(id, Kind::Star, self.level);
        id
    }
    fn new_row(&mut self) -> u32 {
        let id = self.fresh_hi;
        self.fresh_hi += 1;
        self.uf.ensure(id, Kind::Row, self.level);
        id
    }
    fn alloc_rty(&mut self, r: &RTy) {
        match r {
            RTy::V(id, k) => self.uf.ensure(*id, *k, self.level),
            _ => {
                r.vars().iter().for_each(|(id, k)| self.uf.ensure(*id, *k, self.level));
            }
        }
    }

    fn solve_node(&mut self, id: CId, ambient: u32) -> u32 {
        let n = self.cp.nodes[id.0 as usize].clone();
        let path = n.node_path.clone();
        let span = n.span;
        match &n.c {
            C::Leaf { ty } => ty.var_id().map(|x| x.0).unwrap_or_else(|| self.as_repr(ty)),
            C::Lit { out, con } => {
                let v = self.out_var(*out);
                self.uf.ensure(v, Kind::Star, ambient);
                self.install_con(v, con);
                self.log(path, span, None, StepDetail::Bind { con: con.clone(), out: v });
                v
            }
            C::VarRef { out, name } => {
                let v = self.out_var(*out);
                self.uf.ensure(v, Kind::Star, ambient);
                match self.lookup(name) {
                    Binding::Mono(r) => {
                        self.log(path, span, None, StepDetail::Lookup { name: name.clone(), poly: false });
                        self.unify_vars(v, r, &path, span, None);
                    }
                    Binding::Scheme(_) => {
                        let copied = self.instantiate_into(name, v);
                        self.log(path, span, None, StepDetail::Instantiate { name: name.clone(), copies: copied });
                    }
                }
                v
            }
            C::Abs { param_out, body, out, .. } => {
                let param = self.out_var(*param_out);
                let ret = self.solve_node(*body, ambient);
                let outv = self.out_var(*out);
                self.uf.ensure(outv, Kind::Star, ambient);
                self.install_fun(outv, param, ret);
                outv
            }
            C::App { fun, args, out } => {
                let f = self.solve_node(*fun, ambient);
                let mut arg_vs = Vec::new();
                for a in args {
                    arg_vs.push(self.solve_node(*a, ambient));
                }
                let mut cur = f;
                for (i, av) in arg_vs.into_iter().enumerate() {
                    let res = self.new_star();
                    let funty = RTy::Fun(
                        Box::new(RTy::V(av, Kind::Star)),
                        Box::new(RTy::V(res, Kind::Star)),
                    );
                    self.unify_struct(cur, &funty, &path, span, Some(app_atom_key(&path, i)));
                    cur = res;
                }
                let outv = self.out_var(*out);
                self.uf.ensure(outv, Kind::Star, ambient);
                self.unify_vars(outv, cur, &path, span, Some(format!("{}#appres", path)));
                outv
            }
            C::Let { rec, name, generalize, def, body, out } => {
                let outv = self.out_var(*out);
                self.uf.ensure(outv, Kind::Star, ambient);
                if *rec {
                    let fv = self.new_star();
                    self.env.push((name.clone(), Binding::Mono(fv)));
                    self.level += 1;
                    let dv = self.solve_node(*def, self.level);
                    self.level -= 1;
                    self.unify_vars(fv, dv, &path, span, Some(format!("{}#rec", path)));
                    // recursive bindings stay monomorphic
                    let bv = self.solve_node(*body, ambient);
                    self.env.pop();
                    self.unify_vars(outv, bv, &path, span, Some(format!("{}#letres", path)));
                } else {
                    self.level += 1;
                    let dv = self.solve_node(*def, self.level);
                    if *generalize {
                        let gen = self.generalize(dv, ambient);
                        self.log(
                            path.clone(),
                            span,
                            None,
                            StepDetail::Generalize { name: name.clone(), quantified: gen.binds.clone() },
                        );
                        self.env.push((name.clone(), Binding::Scheme(gen)));
                    } else {
                        self.log(
                            path.clone(),
                            span,
                            None,
                            StepDetail::Generalize { name: name.clone(), quantified: vec![] },
                        );
                        self.env.push((name.clone(), Binding::Mono(dv)));
                    }
                    self.level -= 1;
                    let bv = self.solve_node(*body, ambient);
                    self.env.pop();
                    self.unify_vars(outv, bv, &path, span, Some(format!("{}#letres", path)));
                }
                outv
            }
            C::RecordLit { field_constraints, out, tail, closed } => {
                self.uf.ensure(*tail, Kind::Row, ambient);
                let mut fields = BTreeMap::new();
                for (fname, fc) in field_constraints {
                    let fv = self.solve_node(*fc, ambient);
                    fields.insert(fname.clone(), fv);
                }
                let outv = self.out_var(*out);
                self.uf.ensure(outv, Kind::Star, ambient);
                let row = if *closed {
                    RowHead { variant: false, fields, tail: None }
                } else {
                    RowHead { variant: false, fields, tail: Some(*tail) }
                };
                self.uf.set_desc(outv, Head::Row(row));
                outv
            }
            C::FieldAccess { recv, out, field, tail } => {
                let rv = self.solve_node(*recv, ambient);
                let outv = self.out_var(*out);
                self.uf.ensure(outv, Kind::Star, ambient);
                self.uf.ensure(*tail, Kind::Row, ambient);
                let mut fields = BTreeMap::new();
                fields.insert(field.clone(), outv);
                let head = RowHead { variant: false, fields, tail: Some(*tail) };
                let hv = self.new_star();
                self.uf.set_desc(hv, Head::Row(head));
                self.unify_vars(rv, hv, &path, span, Some(format!("{}#field", path)));
                outv
            }
            C::VariantNew { tag, payload, out, tail } => {
                let outv = self.out_var(*out);
                self.uf.ensure(outv, Kind::Star, ambient);
                self.uf.ensure(*tail, Kind::Row, ambient);
                let mut fields = BTreeMap::new();
                match payload {
                    Some(pc) => {
                        let pv = self.solve_node(*pc, ambient);
                        fields.insert(tag.clone(), pv);
                    }
                    None => {
                        let unit = self.new_star();
                        self.install_con(unit, "unit");
                        fields.insert(tag.clone(), unit);
                    }
                }
                let head = RowHead { variant: true, fields, tail: Some(*tail) };
                self.uf.set_desc(outv, Head::Row(head));
                outv
            }
            C::Match { scrut, arms, out } => {
                let sv = self.solve_node(*scrut, ambient);
                let outv = self.out_var(*out);
                self.uf.ensure(outv, Kind::Star, ambient);
                let mut result_vars = Vec::new();
                let mut first_scrut_row: Option<u32> = None;
                for (i, (pat, abody)) in arms.iter().enumerate() {
                    let rowv = self.new_star();
                    if first_scrut_row.is_none() {
                        first_scrut_row = Some(rowv);
                    } else {
                        self.unify_vars(sv, rowv, &path, span, Some(format!("{}#armrow{}", path, i)));
                    }
                    self.bind_pat(pat, rowv, ambient, &path, span, i as u32);
                    let bv = self.solve_node(*abody, ambient);
                    result_vars.push(bv);
                }
                if let Some(rv) = first_scrut_row {
                    self.unify_vars(sv, rv, &path, span, Some(format!("{}#scrut", path)));
                }
                for (i, bv) in result_vars.into_iter().enumerate() {
                    self.unify_vars(outv, bv, &path, span, Some(format!("{}#armres{}", path, i)));
                }
                outv
            }
            C::Annot { expr, out } => {
                let ev = self.solve_node(*expr, ambient);
                let outv = self.out_var(*out);
                self.alloc_rty(&self.cp.nodes[out.0 as usize].leaf_ty());
                let rv = self.as_repr(&self.cp.nodes[out.0 as usize].leaf_ty());
                self.unify_vars(ev, rv, &path, span, Some(format!("{}#annot", path)));
                outv
            }
            C::Eq { .. } => panic!("Eq nodes are not generated"),
        }
    }

    fn bind_pat(
        &mut self,
        pat: &MatchPatC,
        rowv: u32,
        ambient: u32,
        path: &str,
        span: Span,
        arm: u32,
    ) {
        match pat {
            MatchPatC::Wild | MatchPatC::Var(_) => {
                // whole scrutinee binds to var / ignored; unify rowv (a star row holder) later
                if let MatchPatC::Var(name) = pat {
                    self.env.push((name.clone(), Binding::Mono(rowv)));
                }
            }
            MatchPatC::Tag { tag, payload } => {
                let tail = self.new_row();
                let payload_v = if let Some(inner) = payload {
                    let pv = self.new_star();
                    self.bind_pat_inner(inner, pv, ambient);
                    Some(pv)
                } else {
                    let u = self.new_star();
                    self.install_con(u, "unit");
                    Some(u)
                };
                let mut fields = BTreeMap::new();
                fields.insert(tag.clone(), payload_v.unwrap());
                let holder = self.new_star();
                self.uf.set_desc(holder, Head::Row(RowHead { variant: true, fields, tail: Some(tail) }));
                self.unify_vars(rowv, holder, path, span, Some(format!("{}#pat{}", path, arm)));
            }
        }
    }

    fn bind_pat_inner(&mut self, pat: &MatchPatC, v: u32, _ambient: u32) {
        match pat {
            MatchPatC::Wild => {}
            MatchPatC::Var(name) => {
                self.env.push((name.clone(), Binding::Mono(v)));
            }
            MatchPatC::Tag { tag, payload } => {
                let tail = self.new_row();
                let pv = if let Some(inner) = payload {
                    let child = self.new_star();
                    self.bind_pat_inner(inner, child, _ambient);
                    child
                } else {
                    let u = self.new_star();
                    self.install_con(u, "unit");
                    u
                };
                let mut fields = BTreeMap::new();
                fields.insert(tag.clone(), pv);
                self.uf.set_desc(v, Head::Row(RowHead { variant: true, fields, tail: Some(tail) }));
            }
        }
    }

    fn lookup(&self, name: &str) -> Binding {
        for (n, b) in self.env.iter().rev() {
            if n == name {
                return b.clone();
            }
        }
        panic!("unbound variable {} (should be caught earlier)", name);
    }
}
