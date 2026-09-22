//! Constraint generation for the tiny HM + rows language.
//!
//! Each AST node owns a frame of solver variables and emits equations plus
//! scope events. Equations carry a structural key built from the binding
//! environment (anchor-based), which stays stable under binder renaming but
//! changes when the generalization environment changes.

use std::collections::HashMap;
use serde::Serialize;
use crate::lang::ast::*;
use super::constraint::*;
use super::types::*;

#[derive(Clone, Debug)]
pub enum Entry {
    LetPoly { scope: u32 },
    RecMono { init: u32 },
    LamMono { node: u32, idx: u32 },
    Builtin(String),
}

#[derive(Clone, Debug)]
pub enum ProgramEvent {
    EnterScope(u32),
    Generalize { scope: u32, init: u32, value: bool, rec: bool },
    ExitScope(u32),
    Eq(CId),
}

#[derive(Clone, Debug, Serialize)]
pub struct GenOutput {
    pub constraints: ConstraintSystem,
    pub node_ty: HashMap<u32, TyId>,
    pub program: Vec<ProgramEvent>,
}

#[derive(Clone, Debug)]
pub struct GenError {
    pub message: String,
    pub node: u32,
    pub span: (u32, u32),
}

#[derive(Clone, Debug)]
enum T {
    V(u32, u32),
    Con(String),
    Arrow(Box<T>, Box<T>),
    Record(Vec<(String, T)>, bool),
    Variant(Vec<(String, T)>, bool),
    OpenRow,
    SchemeScope(u32),
    Builtin(String),
}

#[derive(Clone, Debug)]
struct RecEq {
    kind: EqKind,
    lhs: T,
    rhs: T,
    node: u32,
    site: u32,
    label: String,
    key: String,
}

#[derive(Clone, Debug)]
struct Frame {
    vars: Vec<Option<String>>,
}

#[derive(Clone, Debug, Default)]
struct ScopeRec {
    kind: ScopeKind,
    binder_anchor: Option<String>,
    binder_name: String,
    node: Option<u32>,
    cids: Vec<CId>,
    children: Vec<u32>,
    instances: Vec<CId>,
}

struct Gen<'a> {
    nodes: &'a [Node],
    env: Vec<(String, Entry)>,
    frames: Vec<Frame>,
    eqs: Vec<RecEq>,
    scopes: Vec<ScopeRec>,
    events: Vec<ProgramEvent>,
    scope_chain: Vec<u32>,
    mains: HashMap<u32, (u32, u32)>,
}

impl<'a> Gen<'a> {
    fn new(nodes: &'a [Node]) -> Self {
        Gen {
            nodes,
            env: super::builtins::initial_entries(),
            frames: (0..nodes.len()).map(|_| Frame { vars: vec![] }).collect(),
            eqs: Vec::new(),
            scopes: vec![ScopeRec {
                kind: ScopeKind::Root,
                binder_name: "<root>".into(),
                ..Default::default()
            }],
            events: Vec::new(),
            scope_chain: vec![0],
            mains: HashMap::new(),
        }
    }

    fn cur_scope(&self) -> u32 {
        *self.scope_chain.last().unwrap()
    }

    fn fresh(&mut self, frame: u32, name: Option<String>) -> T {
        let f = &mut self.frames[frame as usize];
        let i = f.vars.len() as u32;
        f.vars.push(name);
        T::V(frame, i)
    }

    fn gen_err(&self, message: String, node: u32) -> GenError {
        let s = self.nodes[node as usize].span;
        GenError { message, node, span: (s.lo, s.hi) }
    }

    fn emit(&mut self, e: RecEq) {
        let id = self.eqs.len() as CId;
        self.events.push(ProgramEvent::Eq(id));
        self.scopes[self.cur_scope() as usize].cids.push(id);
        self.eqs.push(e);
    }

    fn lookup(&self, name: &str) -> Option<&Entry> {
        self.env.iter().rev().find(|(n, _)| n == name).map(|(_, e)| e)
    }

    /// Structural fingerprint of the binding environment. Identities are
    /// anchor based, so renaming a binder does not perturb downstream keys,
    /// while a changed generalization environment changes every key.
    fn env_fingerprint(&self) -> String {
        let mut parts: Vec<String> = self
            .env
            .iter()
            .map(|(name, e)| {
                let v = match e {
                    Entry::LetPoly { scope } => format!("p{}", scope),
                    Entry::RecMono { init } => format!("r{}", self.nodes[*init as usize].anchor),
                    Entry::LamMono { node, idx } => format!("l{}#{}", self.nodes[*node as usize].anchor, idx),
                    Entry::Builtin(b) => format!("b{}", b),
                };
                format!("{}={}", name, v)
            })
            .collect();
        parts.sort();
        parts.join("|")
    }

    fn key(&self, node: u32, tag: &str) -> String {
        format!("{}::{}/{}", self.env_fingerprint(), self.nodes[node as usize].anchor, tag)
    }

    fn gen(&mut self, id: NodeId) -> Result<T, GenError> {
        let n = id.0;
        let expr = self.nodes[n as usize].expr.clone();
        let result = self.gen_expr(n, expr)?;
        if let T::V(f, i) = &result {
            self.mains.insert(n, (*f, *i));
        }
        Ok(result)
    }
}

impl<'a> Gen<'a> {
    fn gen_expr(&mut self, n: u32, expr: Expr) -> Result<T, GenError> {
        Ok(match expr {
            Expr::Int(v) => {
                let t = self.fresh(n, None);
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: t.clone(),
                    rhs: T::Con("Int".into()),
                    node: n,
                    site: n,
                    label: format!("integer literal {}", v),
                    key: self.key(n, "lit-int"),
                });
                t
            }
            Expr::Bool(b) => {
                let t = self.fresh(n, None);
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: t.clone(),
                    rhs: T::Con("Bool".into()),
                    node: n,
                    site: n,
                    label: format!("boolean literal {}", b),
                    key: self.key(n, "lit-bool"),
                });
                t
            }
            Expr::Unit => {
                let t = self.fresh(n, None);
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: t.clone(),
                    rhs: T::Con("()".into()),
                    node: n,
                    site: n,
                    label: "unit literal".into(),
                    key: self.key(n, "lit-unit"),
                });
                t
            }
            Expr::Var(name) => self.gen_var(n, &name)?,
            Expr::Lam { param, param_span: _, body } => {
                let t = self.fresh(n, None);
                let dom = self.fresh(n, Some(param.clone()));
                self.env.push((param, Entry::LamMono { node: n, idx: 0 }));
                let cod = self.gen(body)?;
                self.env.pop();
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: t.clone(),
                    rhs: T::Arrow(Box::new(dom), Box::new(cod)),
                    node: n,
                    site: n,
                    label: "function abstraction".into(),
                    key: self.key(n, "lam"),
                });
                t
            }
            Expr::App { fun, arg } => {
                let ft = self.gen(fun)?;
                let at = self.gen(arg)?;
                let t = self.fresh(n, None);
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: ft,
                    rhs: T::Arrow(Box::new(at), Box::new(t.clone())),
                    node: n,
                    site: n,
                    label: "function application".into(),
                    key: self.key(n, "app"),
                });
                t
            }
            Expr::Bin { op, lhs, rhs } => {
                let a = self.gen(lhs)?;
                let b = self.gen(rhs)?;
                let t = self.fresh(n, None);
                let (lhs_ty, rhs_ty, res_ty, tag): (T, T, T, &str) = match op.as_str() {
                    "==" => (T::Con("Int".into()), T::Con("Int".into()), T::Con("Bool".into()), "eq"),
                    "+" | "-" | "*" => (T::Con("Int".into()), T::Con("Int".into()), T::Con("Int".into()), "arith"),
                    _ => return Err(self.gen_err(format!("unknown operator {}", op), n)),
                };
                self.emit(RecEq { kind: EqKind::Expr, lhs: a, rhs: lhs_ty, node: n, site: self.node_id(lhs), label: format!("{} left operand", op), key: self.key(n, &format!("bin-{}-l", tag)) });
                self.emit(RecEq { kind: EqKind::Expr, lhs: b, rhs: rhs_ty, node: n, site: self.node_id(rhs), label: format!("{} right operand", op), key: self.key(n, &format!("bin-{}-r", tag)) });
                self.emit(RecEq { kind: EqKind::Expr, lhs: t.clone(), rhs: res_ty, node: n, site: n, label: format!("{} result", op), key: self.key(n, &format!("bin-{}-res", tag)) });
                t
            }
            Expr::Let { rec, name, name_span: _, init, body } => {
                self.gen_let(n, rec, &name, init, body)?
            }
            Expr::Record(fields) => self.gen_record(n, fields)?,
            Expr::Field { obj, name } => {
                let ot = self.gen(obj)?;
                let t = self.fresh(n, None);
                let row = T::Record(vec![(name.clone(), t.clone())], false);
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: ot,
                    rhs: row,
                    node: n,
                    site: self.node_id(obj),
                    label: format!("field access .{}", name),
                    key: self.key(n, &format!("field-{}", name)),
                });
                t
            }
            Expr::Variant { tag, payload } => {
                let t = self.fresh(n, None);
                let inner = if let Some(p) = payload { self.gen(p)? } else { T::Con("()".into()) };
                let row = T::Variant(vec![(tag.clone(), inner)], false);
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: t.clone(),
                    rhs: row,
                    node: n,
                    site: n,
                    label: format!("variant injection {}", tag),
                    key: self.key(n, &format!("inj-{}", tag)),
                });
                t
            }
            Expr::Match { scrut, arms } => self.gen_match(n, scrut, arms)?,
            Expr::Ite { cond, then_b, else_b } => {
                let c = self.gen(cond)?;
                let a = self.gen(then_b)?;
                let b = self.gen(else_b)?;
                self.emit(RecEq { kind: EqKind::Expr, lhs: c, rhs: T::Con("Bool".into()), node: n, site: self.node_id(cond), label: "if condition".into(), key: self.key(n, "ite-cond") });
                self.emit(RecEq { kind: EqKind::Expr, lhs: a.clone(), rhs: b, node: n, site: n, label: "if branches agree".into(), key: self.key(n, "ite-branches") });
                a
            }
        })
    }

    fn node_id(&self, id: NodeId) -> u32 {
        id.0
    }

    fn gen_var(&mut self, n: u32, name: &str) -> Result<T, GenError> {
        let t = self.fresh(n, None);
        match self.lookup(name).cloned() {
            Some(Entry::Builtin(b)) => self.emit(RecEq {
                kind: EqKind::Builtin,
                lhs: t.clone(),
                rhs: T::Builtin(b.clone()),
                node: n,
                site: n,
                label: format!("builtin {}", b),
                key: self.key(n, &format!("use-builtin-{}", b)),
            }),
            Some(Entry::LetPoly { scope }) => {
                self.emit(RecEq {
                    kind: EqKind::Instantiate,
                    lhs: t.clone(),
                    rhs: T::SchemeScope(scope),
                    node: n,
                    site: n,
                    label: format!("instantiate {}", name),
                    key: self.key(n, &format!("use-poly-{}", scope)),
                });
                let sid = (self.eqs.len() - 1) as CId;
                self.scopes[scope as usize].instances.push(sid);
            }
            Some(Entry::RecMono { init }) => {
                let init_t = {
                    let (f, i) = self.mains[&init];
                    T::V(f, i)
                };
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: t.clone(),
                    rhs: init_t,
                    node: n,
                    site: n,
                    label: format!("recursive use of {}", name),
                    key: self.key(n, "use-rec"),
                });
            }
            Some(Entry::LamMono { node: lam, idx }) => {
                let (f, i) = self.mains[&lam];
                let pv = T::V(f, i + 1 + idx);
                self.emit(RecEq {
                    kind: EqKind::Expr,
                    lhs: t.clone(),
                    rhs: pv,
                    node: n,
                    site: n,
                    label: format!("parameter {}", name),
                    key: self.key(n, &format!("use-lam-{}", idx)),
                });
            }
            None => return Err(self.gen_err(format!("unbound variable `{}`", name), n)),
        }
        Ok(t)
    }
}

impl<'a> Gen<'a> {
    fn gen_let(&mut self, n: u32, rec: bool, name: &str, init: NodeId, body: NodeId) -> Result<T, GenError> {
        let init_node = self.nodes[init.0 as usize].clone();
        let is_value = is_syntactic_value(&init_node.expr);
        let scope_id = self.scopes.len() as u32;
        self.scopes.push(ScopeRec {
            kind: if rec { ScopeKind::Rec } else { ScopeKind::Let },
            binder_anchor: Some(init_node.anchor.clone()),
            binder_name: name.into(),
            node: Some(n),
            cids: Vec::new(),
            children: Vec::new(),
            instances: Vec::new(),
        });
        let parent = self.cur_scope();
        self.scopes[parent as usize].children.push(scope_id);
        self.scope_chain.push(scope_id);
        self.events.push(ProgramEvent::EnterScope(scope_id));

        if rec {
            // binder visible while generating init; instantiation shares init main var
            self.env.push((name.to_string(), Entry::RecMono { init: init.0 }));
            let it = self.gen(init)?;
            self.emit(RecEq {
                kind: EqKind::Expr,
                lhs: T::V(match &it { T::V(f, _) => *f, _ => unreachable!() }, match &it { T::V(_, i) => *i, _ => unreachable!() }),
                rhs: it,
                node: n,
                site: init.0,
                label: "recursive self type".into(),
                key: self.key(n, "rec-self"),
            });
        } else {
            self.gen(init)?;
        }

        self.events.push(ProgramEvent::Generalize { scope: scope_id, init: init.0, value: is_value, rec });

        if rec {
            // replace RecMono with LetPoly so body uses generalised (possibly monomorphic) scheme
            self.env.pop();
        }
        self.env.push((name.to_string(), Entry::LetPoly { scope: scope_id }));

        let bt = self.gen(body)?;

        self.env.pop();
        self.events.push(ProgramEvent::ExitScope(scope_id));
        self.scope_chain.pop();
        Ok(bt)
    }

    fn gen_record(&mut self, n: u32, fields: Vec<Field>) -> Result<T, GenError> {
        let mut out = Vec::new();
        for f in fields {
            let ft = self.gen(f.value)?;
            out.push((f.name, ft));
        }
        let t = self.fresh(n, None);
        self.emit(RecEq {
            kind: EqKind::Expr,
            lhs: t.clone(),
            rhs: T::Record(out, true),
            node: n,
            site: n,
            label: "record literal".into(),
            key: self.key(n, "record"),
        });
        t
    }

    fn gen_match(&mut self, n: u32, scrut: NodeId, arms: Vec<Arm>) -> Result<T, GenError> {
        let st = self.gen(scrut)?;
        let result = self.fresh(n, None);
        let mut row_fields = Vec::new();
        let mut saw_wild = false;
        for (ai, arm) in arms.into_iter().enumerate() {
            let (tag, payload_var) = match &arm.pat {
                ArmPat::Wild => {
                    saw_wild = true;
                    continue;
                }
                ArmPat::Tag { tag, param } => {
                    let pv = if let Some(pname) = param {
                        let pv = self.fresh(n, Some(pname.clone()));
                        let idx = match &pv { T::V(_, i) => *i, _ => unreachable!() };
                        self.env.push((pname.clone(), Entry::LamMono { node: n, idx: idx - 1 }));
                        Some((pv, pname.clone()))
                    } else {
                        None
                    };
                    (tag.clone(), pv)
                }
            };
            let payload_ty = if let Some((pv, _)) = &payload_var { pv.clone() } else { T::Con("()".into()) };
            row_fields.push((tag.clone(), payload_ty));
            let bt = self.gen(arm.body)?;
            if let Some((_, pname)) = &payload_var {
                self.env.retain(|(nm, _)| nm != pname);
            }
            self.emit(RecEq {
                kind: EqKind::Expr,
                lhs: result.clone(),
                rhs: bt,
                node: n,
                site: self.node_id(arm.body),
                label: format!("match arm {} result", ai),
                key: self.key(n, &format!("arm-{}-res", ai)),
            });
        }
        let closed = !saw_wild;
        self.emit(RecEq {
            kind: EqKind::Expr,
            lhs: st,
            rhs: T::Variant(row_fields, closed),
            node: n,
            site: self.node_id(scrut),
            label: if closed { "exhaustive match".into() } else { "match with default".into() },
            key: self.key(n, if closed { "match-closed" } else { "match-open" }),
        });
        Ok(result)
    }
}

/// HM value restriction: only generalize over syntactic values.
pub fn is_syntactic_value(e: &Expr) -> bool {
    matches!(
        e,
        Expr::Int(_)
            | Expr::Bool(_)
            | Expr::Unit
            | Expr::Lam { .. }
            | Expr::Variant { payload: None, .. }
    ) || matches!(e, Expr::Variant { payload: Some(p), .. } if is_value_id_placeholder())
        || matches!(e, Expr::Record(fs) if fs.is_empty())
}

fn is_value_id_placeholder() -> bool {
    false
}
