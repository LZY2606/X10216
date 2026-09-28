use crate::ast::*;
use crate::span::Span;
use crate::types::{Kind, RTy, Row};
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CId(pub u32);

/// A single primitive equality atom between two constraint-level types.
#[derive(Clone, Debug)]
pub struct Atom {
    pub id: CId,
    /// Stable key survives edits; renaming-bound variables do not shift it.
    pub key: String,
    pub left: RTy,
    pub right: RTy,
    pub origin: Span,
    pub label: String,
    /// scope signature at emission (node path chain)
    pub scope: String,
    /// generalization environment signature at emission
    pub genv: String,
}

#[derive(Clone, Debug)]
pub enum C {
    Leaf {
        ty: RTy,
    },
    Lit {
        out: CId,
        con: String,
    },
    VarRef {
        out: CId,
        name: String,
    },
    Abs {
        param_out: CId,
        body: CId,
        out: CId,
        param: String,
    },
    App {
        fun: CId,
        args: Vec<CId>,
        out: CId,
    },
    Let {
        rec: bool,
        name: String,
        generalize: bool,
        def: CId,
        body: CId,
        out: CId,
    },
    RecordLit {
        field_constraints: Vec<(String, CId)>,
        out: CId,
        tail: u32,
        closed: bool,
    },
    FieldAccess {
        recv: CId,
        out: CId,
        field: String,
        tail: u32,
    },
    VariantNew {
        tag: String,
        payload: Option<CId>,
        out: CId,
        tail: u32,
    },
    Match {
        scrut: CId,
        arms: Vec<(MatchPatC, CId)>,
        out: CId,
    },
    Annot {
        expr: CId,
        out: CId,
    },
    Eq {
        a: CId,
        b: CId,
        atoms: Vec<CId>,
    },
}

#[derive(Clone, Debug)]
pub enum MatchPatC {
    Wild,
    Var(String),
    Tag { tag: String, payload: Option<Box<MatchPatC>> },
}

#[derive(Clone, Debug)]
pub struct CNode {
    pub id: CId,
    pub kind: String,
    pub node_path: String,
    pub span: Span,
    pub c: C,
}

pub struct ConstraintProgram {
    pub nodes: Vec<CNode>,
    pub root: CId,
    pub atoms: Vec<Atom>,
}

struct G {
    nodes: Vec<CNode>,
    atoms: Vec<Atom>,
    counter: u32,
    scope_stack: Vec<String>,
    genv_stack: Vec<String>,
}

impl G {
    fn vstar(&mut self) -> RTy {
        RTy::fresh_star(&mut self.counter)
    }
    fn vrow(&mut self) -> RTy {
        RTy::fresh_row(&mut self.counter)
    }
    fn scope(&self) -> String {
        self.scope_stack.join(">>")
    }
    fn genv(&self) -> String {
        self.genv_stack.join("|")
    }
    fn alloc(&mut self, c: C, node_path: String, span: Span) -> CId {
        let id = CId(self.nodes.len() as u32);
        let kind = match c {
            C::Leaf { .. } => "leaf",
            C::Lit { .. } => "lit",
            C::VarRef { .. } => "var",
            C::Abs { .. } => "abs",
            C::App { .. } => "app",
            C::Let { .. } => "let",
            C::RecordLit { .. } => "record",
            C::FieldAccess { .. } => "field",
            C::VariantNew { .. } => "variant",
            C::Match { .. } => "match",
            C::Annot { .. } => "annot",
            C::Eq { .. } => "eq",
        };
        self.nodes.push(CNode { id, kind: kind.into(), node_path, span, c });
        id
    }
    fn emit_atom(&mut self, key: String, left: RTy, right: RTy, origin: Span, label: String) -> CId {
        let id = CId(self.atoms.len() as u32);
        self.atoms.push(Atom {
            id,
            key,
            left,
            right,
            origin,
            label,
            scope: self.scope(),
            genv: self.genv(),
        });
        id
    }
}

fn is_value(prog: &Program, id: NodeId) -> bool {
    match &prog.node(id).expr {
        Expr::Int(_) | Expr::Bool(_) | Expr::Fun { .. } | Expr::Variant { payload: None, .. } => true,
        Expr::Variant { payload: Some(p), .. } => is_value(prog, *p),
        Expr::Record { fields } => fields.iter().all(|(_, n)| is_value(prog, *n)),
        Expr::Annot { expr, .. } => is_value(prog, *expr),
        _ => false,
    }
}

pub fn generate(prog: &Program) -> ConstraintProgram {
    let mut g = G {
        nodes: Vec::new(),
        atoms: Vec::new(),
        counter: 0,
        scope_stack: Vec::new(),
        genv_stack: Vec::new(),
    };
    let root = gen_expr(&mut g, prog, prog.root);
    ConstraintProgram { nodes: g.nodes, root, atoms: g.atoms }
}

fn path_of(prog: &Program, id: NodeId) -> String {
    prog.node(id).path.key()
}

fn gen_expr(g: &mut G, prog: &Program, id: NodeId) -> CId {
    let node = prog.node(id);
    let pth = node.path.key();
    let span = node.span;
    match &node.expr {
        Expr::Int(_) => {
            let out = g.vstar();
            g.alloc(C::Lit { out: leaf(g, out), con: "int".into() }, pth, span)
        }
        Expr::Bool(_) => {
            let out = g.vstar();
            g.alloc(C::Lit { out: leaf(g, out), con: "bool".into() }, pth, span)
        }
        Expr::Var(name) => {
            let out = g.vstar();
            g.alloc(C::VarRef { out: leaf(g, out), name: name.clone() }, pth, span)
        }
        Expr::Fun { param, body } => {
            g.scope_stack.push(format!("abs@{}", pth));
            let param_ty = g.vstar();
            let body_c = gen_expr(g, prog, *body);
            g.scope_stack.pop();
            let out = g.vstar();
            g.alloc(
                C::Abs { param_out: leaf(g, param_ty), body: body_c, out: leaf(g, out), param: param.clone() },
                pth,
                span,
            )
        }
        Expr::App { fun, args } => {
            let fun_c = gen_expr(g, prog, *fun);
            let arg_cs = args.iter().map(|a| gen_expr(g, prog, *a)).collect();
            let out = g.vstar();
            g.alloc(C::App { fun: fun_c, args: arg_cs, out: leaf(g, out) }, pth, span)
        }
        Expr::Let { rec, name, def, body } => {
            let val_form = is_value(prog, *def);
            g.genv_stack.push(format!("{}:{}", name, if *rec { "rec" } else if val_form { "gen" } else { "mono" }));
            g.scope_stack.push(format!("let@{}", pth));
            let def_c = gen_expr(g, prog, *def);
            let body_c = gen_expr(g, prog, *body);
            g.scope_stack.pop();
            g.genv_stack.pop();
            let out = g.vstar();
            g.alloc(
                C::Let {
                    rec: *rec,
                    name: name.clone(),
                    generalize: val_form && !rec,
                    def: def_c,
                    body: body_c,
                    out: leaf(g, out),
                },
                pth,
                span,
            )
        }
        Expr::Record { fields } => {
            let mut fcs = Vec::new();
            for (name, fnode) in fields {
                let fc = gen_expr(g, prog, *fnode);
                fcs.push((name.clone(), fc));
            }
            let tail = fresh_row_id(&mut g.counter);
            let out = g.vstar();
            g.alloc(C::RecordLit { field_constraints: fcs, out: leaf(g, out), tail, closed: true }, pth, span)
        }
        Expr::Field { recv, field } => {
            let recv_c = gen_expr(g, prog, *recv);
            let tail = fresh_row_id(&mut g.counter);
            let out = g.vstar();
            g.alloc(C::FieldAccess { recv: recv_c, out: leaf(g, out), field: field.clone(), tail }, pth, span)
        }
        Expr::Variant { tag, payload } => {
            let pc = payload.as_ref().map(|p| gen_expr(g, prog, *p));
            let tail = fresh_row_id(&mut g.counter);
            let out = g.vstar();
            g.alloc(C::VariantNew { tag: tag.clone(), payload: pc, out: leaf(g, out), tail }, pth, span)
        }
        Expr::Match { scrut, arms } => {
            let scrut_c = gen_expr(g, prog, *scrut);
            let mut acs = Vec::new();
            for (pat, abody) in arms {
                let pc = convert_pat(g, prog, pat);
                let bc = gen_expr(g, prog, *abody);
                acs.push((pc, bc));
            }
            let out = g.vstar();
            g.alloc(C::Match { scrut: scrut_c, arms: acs, out: leaf(g, out) }, pth, span)
        }
        Expr::Annot { expr, ty } => {
            let ec = gen_expr(g, prog, *expr);
            build_annot(g, ec, ty, pth, span)
        }
    }
}

fn fresh_row_id(counter: &mut u32) -> u32 {
    let v = *counter;
    *counter += 1;
    v
}

fn leaf(g: &mut G, ty: RTy) -> CId {
    g.alloc(C::Leaf { ty }, String::new(), Span::new(0, 0))
}

fn convert_pat(g: &mut G, prog: &Program, pat: &Pat) -> MatchPatC {
    let _ = prog;
    let _ = g;
    match pat {
        Pat::Wild => MatchPatC::Wild,
        Pat::Var(n) => MatchPatC::Var(n.clone()),
        Pat::Tag { tag, payload } => MatchPatC::Tag {
            tag: tag.clone(),
            payload: payload.as_ref().map(|p| Box::new(convert_pat(g, prog, p))),
        },
    }
}

fn build_annot(g: &mut G, expr: CId, ty: &TypeAst, pth: String, span: Span) -> CId {
    let rty = annot_type(g, ty);
    let out = leaf(g, rty);
    g.alloc(C::Annot { expr, out }, pth, span)
}

fn annot_type(g: &mut G, ty: &TypeAst) -> RTy {
    match ty {
        TypeAst::TName(n) => RTy::Con(n.clone()),
        TypeAst::TFun(a, b) => RTy::Fun(Box::new(annot_type(g, a)), Box::new(annot_type(g, b))),
        TypeAst::TRowVar(_) => g.vrow(),
        TypeAst::TRecord(fields, closed) => {
            let mut fs = BTreeMap::new();
            for (name, t) in fields {
                fs.insert(name.clone(), annot_type(g, t));
            }
            let tail = if *closed { None } else { Some(fresh_row_id(&mut g.counter)) };
            RTy::Record(Row { fields: fs, tail })
        }
        TypeAst::TVariant(cases, closed) => {
            let mut fs = BTreeMap::new();
            for (tag, payload) in cases {
                let v = match payload {
                    Some(t) => annot_type(g, t),
                    None => RTy::Con("unit".into()),
                };
                fs.insert(tag.clone(), v);
            }
            let tail = if *closed { None } else { Some(fresh_row_id(&mut g.counter)) };
            RTy::Variant(Row { fields: fs, tail })
        }
    }
}

#[allow(dead_code)]
fn kind_of(_r: &RTy) -> Kind {
    Kind::Star
}
