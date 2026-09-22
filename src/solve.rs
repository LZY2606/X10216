//! 求解器:按序处理约束,记录每一步;统一带 occurs check 与行多态,
//! 缺字段与多余字段分开报告;出错记录后继续,以暴露多个错误。
use crate::constrain::{Constraint, ConstraintKind, Origin};
use crate::syntax::Span;
use crate::types::{
    display_scheme, display_ty, fv_scheme, fv_ty, occurs, prov_chain, ConId, Prov, Row, Scheme,
    Subst, Ty, VarId,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashSet};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ErrKind {
    Occurs,
    Mismatch,
    Row {
        missing: Vec<String>,
        extra: Vec<String>,
        variant: bool,
    },
    UnboundVar {
        name: String,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TypeError {
    pub kind: ErrKind,
    pub con: Option<ConId>,
    pub span: Span,
    pub message: String,
    pub lhs: Option<String>,
    pub rhs: Option<String>,
    /// 两侧类型的来源链:每个部分来自哪个表达式/约束
    pub lhs_prov: Vec<Prov>,
    pub rhs_prov: Vec<Prov>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TraceStep {
    pub n: usize,
    pub con: Option<ConId>,
    pub action: String,
    pub detail: String,
    pub subst_size: usize,
    pub reused: bool,
}

pub struct Solver {
    pub subst: Subst,
    pub env: Vec<(String, Scheme)>,
    pub trace: Vec<TraceStep>,
    pub errors: Vec<TypeError>,
    pub next_var: VarId,
    pub disabled: HashSet<ConId>,
    /// 核心提取时:仅启用集合内的 Eq 约束
    pub eq_filter: Option<HashSet<ConId>>,
    cur: Option<(ConId, Origin)>,
}

impl Solver {
    pub fn new(next_var: VarId) -> Self {
        Solver {
            subst: Subst::default(),
            env: Vec::new(),
            trace: Vec::new(),
            errors: Vec::new(),
            next_var,
            disabled: HashSet::new(),
            eq_filter: None,
            cur: None,
        }
    }

    fn step(&mut self, action: &str, detail: String) {
        let n = self.trace.len();
        self.trace.push(TraceStep {
            n,
            con: self.cur.as_ref().map(|(c, _)| *c),
            action: action.to_string(),
            detail,
            subst_size: self.subst.size(),
            reused: false,
        });
    }

    fn fresh(&mut self) -> VarId {
        let v = self.next_var;
        self.next_var += 1;
        v
    }

    pub fn run(&mut self, constraints: &[Constraint]) {
        for c in constraints {
            if self.disabled.contains(&c.id) {
                self.cur = Some((c.id, c.origin.clone()));
                self.step("跳过", "约束被用户禁用".into());
                continue;
            }
            self.cur = Some((c.id, c.origin.clone()));
            match &c.kind {
                ConstraintKind::Eq { lhs, rhs } => {
                    if let Some(f) = &self.eq_filter {
                        if !f.contains(&c.id) {
                            continue;
                        }
                    }
                    let la = display_ty(&self.subst.resolve(lhs));
                    let lb = display_ty(&self.subst.resolve(rhs));
                    self.step("统一", format!("{} ~ {}", la, lb));
                    if let Err(e) = self.unify(lhs, rhs) {
                        self.errors.push(e);
                    }
                }
                ConstraintKind::Instantiate { name, target } => {
                    self.instantiate(name, target);
                }
                ConstraintKind::Generalize { name, ty } => {
                    let t = self.subst.resolve(ty);
                    let mut env_fv = BTreeSet::new();
                    for (_, s) in &self.env {
                        let fs = fv_scheme(&Scheme {
                            vars: s.vars.clone(),
                            ty: self.subst.resolve(&s.ty),
                        });
                        env_fv.extend(fs);
                    }
                    let mut tfv = BTreeSet::new();
                    fv_ty(&t, &mut tfv);
                    let vars: Vec<VarId> = tfv.difference(&env_fv).copied().collect();
                    let scheme = Scheme { vars, ty: t };
                    self.step(
                        "泛化",
                        format!("{} := {}", name, display_scheme(&scheme)),
                    );
                    self.env.push((name.clone(), scheme));
                }
                ConstraintKind::BindMono { name, ty, why } => {
                    let t = self.subst.resolve(ty);
                    self.step(
                        "单态绑定",
                        format!("{} := {} ({})", name, display_ty(&t), why),
                    );
                    self.env.push((
                        name.clone(),
                        Scheme {
                            vars: vec![],
                            ty: t,
                        },
                    ));
                }
                ConstraintKind::Pop => {
                    self.env.pop();
                    self.step("作用域", "弹出绑定".into());
                }
                ConstraintKind::Unbound { name } => {
                    let span = self.cur.as_ref().map(|(_, o)| o.span).unwrap_or_default();
                    self.step("错误", format!("未绑定变量 {}", name));
                    self.errors.push(TypeError {
                        kind: ErrKind::UnboundVar { name: name.clone() },
                        con: Some(c.id),
                        span,
                        message: format!("未绑定变量:{} 不在作用域内", name),
                        lhs: None,
                        rhs: None,
                        lhs_prov: vec![],
                        rhs_prov: vec![],
                    });
                }
            }
        }
        self.cur = None;
    }

    fn instantiate(&mut self, name: &str, target: &Ty) {
        let found = self.env.iter().rev().find(|(n, _)| n == name);
        match found {
            None => {
                let span = self.cur.as_ref().map(|(_, o)| o.span).unwrap_or_default();
                let con = self.cur.as_ref().map(|(c, _)| *c);
                self.step("错误", format!("未绑定变量 {}", name));
                self.errors.push(TypeError {
                    kind: ErrKind::UnboundVar {
                        name: name.to_string(),
                    },
                    con,
                    span,
                    message: format!("未绑定变量:{}", name),
                    lhs: None,
                    rhs: None,
                    lhs_prov: vec![],
                    rhs_prov: vec![],
                });
            }
            Some((_, scheme)) => {
                let scheme = scheme.clone();
                if scheme.vars.is_empty() {
                    self.step(
                        "实例化",
                        format!("{} 为单态 {}", name, display_ty(&self.subst.resolve(&scheme.ty))),
                    );
                    let ty = scheme.ty.clone();
                    if let Err(e) = self.unify(target, &ty) {
                        self.errors.push(e);
                    }
                } else {
                    let mut map: BTreeMap<VarId, VarId> = BTreeMap::new();
                    for v in &scheme.vars {
                        map.insert(*v, self.fresh());
                    }
                    let body = rename(&scheme.ty, &map);
                    self.step(
                        "实例化",
                        format!(
                            "∀{} . {}  ⇒  {}",
                            scheme.vars.len(),
                            display_scheme(&scheme),
                            display_ty(&body)
                        ),
                    );
                    if let Err(e) = self.unify(target, &body) {
                        self.errors.push(e);
                    }
                }
            }
        }
    }

    pub fn unify(&mut self, a: &Ty, b: &Ty) -> Result<(), TypeError> {
        let a = self.subst.resolve(a);
        let b = self.subst.resolve(b);
        match (&a, &b) {
            (Ty::Var(x, _), Ty::Var(y, _)) if x == y => Ok(()),
            (Ty::Var(x, _), t) => self.bind_var(*x, t),
            (t, Ty::Var(x, _)) => self.bind_var(*x, t),
            (Ty::Int(_), Ty::Int(_)) => Ok(()),
            (Ty::Bool(_), Ty::Bool(_)) => Ok(()),
            (Ty::Fun(a1, b1, _), Ty::Fun(a2, b2, _)) => {
                self.unify(a1, a2)?;
                self.unify(b1, b2)
            }
            (Ty::Record(r1, _), Ty::Record(r2, _)) => {
                let (r1, r2) = (r1.clone(), r2.clone());
                self.unify_rows(&r1, &r2, false, &a, &b)
            }
            (Ty::Variant(r1, _), Ty::Variant(r2, _)) => {
                let (r1, r2) = (r1.clone(), r2.clone());
                self.unify_rows(&r1, &r2, true, &a, &b)
            }
            _ => Err(self.err(
                ErrKind::Mismatch,
                &a,
                &b,
                format!(
                    "类型不匹配:{} 与 {} 无法统一",
                    display_ty(&a),
                    display_ty(&b)
                ),
            )),
        }
    }

    fn bind_var(&mut self, x: VarId, t: &Ty) -> Result<(), TypeError> {
        if occurs(x, t, &self.subst) {
            let vty = Ty::Var(x, Prov::default());
            return Err(self.err(
                ErrKind::Occurs,
                &vty,
                t,
                format!(
                    "无限类型:变量 '{}' 出现在 {} 中(occurs check 失败)",
                    display_ty(&vty),
                    display_ty(t)
                ),
            ));
        }
        self.subst.tys.insert(x, t.clone());
        self.step("绑定", format!("'{} := {}", x, display_ty(t)));
        Ok(())
    }

    fn bind_row(&mut self, v: VarId, row: Row, whole: &Ty) -> Result<(), TypeError> {
        // occurs:行变量不得出现在将绑定的行里
        let mut set = BTreeSet::new();
        crate::types::fv_row(&row, &mut set);
        if set.contains(&v) {
            let vty = Ty::Var(v, Prov::default());
            return Err(self.err(
                ErrKind::Occurs,
                &vty,
                whole,
                "无限行类型:行变量出现在自身展开中(occurs check 失败)".to_string(),
            ));
        }
        self.subst.rows.insert(v, row);
        self.step("行绑定", format!("行变量 '{} 已扩展", v));
        Ok(())
    }

    fn unify_rows(
        &mut self,
        r1: &Row,
        r2: &Row,
        variant: bool,
        whole_a: &Ty,
        whole_b: &Ty,
    ) -> Result<(), TypeError> {
        let r1 = self.subst.resolve_row(r1);
        let r2 = self.subst.resolve_row(r2);
        let keys: BTreeSet<String> = r1.fields.keys().chain(r2.fields.keys()).cloned().collect();
        for k in &keys {
            if let (Some(t1), Some(t2)) = (r1.fields.get(k), r2.fields.get(k)) {
                self.unify(t1, t2)?;
            }
        }
        let rest1: BTreeMap<String, Ty> = r1
            .fields
            .iter()
            .filter(|(k, _)| !r2.fields.contains_key(*k))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        let rest2: BTreeMap<String, Ty> = r2
            .fields
            .iter()
            .filter(|(k, _)| !r1.fields.contains_key(*k))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        let row_err = |missing: Vec<String>, extra: Vec<String>| -> TypeError {
            let kind_name = if variant { "variant" } else { "record" };
            let mut parts = Vec::new();
            if !missing.is_empty() {
                parts.push(format!("缺少字段 {}", missing.join(", ")));
            }
            if !extra.is_empty() {
                parts.push(format!("多余字段 {}", extra.join(", ")));
            }
            self.err(
                ErrKind::Row {
                    missing,
                    extra,
                    variant,
                },
                whole_a,
                whole_b,
                format!("行不匹配({}):{}", kind_name, parts.join(";")),
            )
        };
        match (r1.tail, r2.tail) {
            (None, None) => {
                if rest1.is_empty() && rest2.is_empty() {
                    Ok(())
                } else {
                    Err(row_err(
                        rest2.keys().cloned().collect(),
                        rest1.keys().cloned().collect(),
                    ))
                }
            }
            (Some(v), None) => {
                if !rest1.is_empty() {
                    return Err(row_err(vec![], rest1.keys().cloned().collect()));
                }
                self.bind_row(
                    v,
                    Row {
                        fields: rest2,
                        tail: None,
                        prov: r2.prov,
                    },
                    whole_b,
                )
            }
            (None, Some(v)) => {
                if !rest2.is_empty() {
                    return Err(row_err(rest2.keys().cloned().collect(), vec![]));
                }
                self.bind_row(
                    v,
                    Row {
                        fields: rest1,
                        tail: None,
                        prov: r1.prov,
                    },
                    whole_a,
                )
            }
            (Some(v1), Some(v2)) => {
                if v1 == v2 {
                    if rest1.is_empty() && rest2.is_empty() {
                        return Ok(());
                    }
                    return Err(row_err(
                        rest2.keys().cloned().collect(),
                        rest1.keys().cloned().collect(),
                    ));
                }
                let m = self.fresh();
                let p = r1.prov;
                self.bind_row(
                    v1,
                    Row {
                        fields: rest2,
                        tail: Some(m),
                        prov: p,
                    },
                    whole_b,
                )?;
                self.bind_row(
                    v2,
                    Row {
                        fields: rest1,
                        tail: Some(m),
                        prov: p,
                    },
                    whole_a,
                )
            }
        }
    }

    fn err(&self, kind: ErrKind, a: &Ty, b: &Ty, message: String) -> TypeError {
        let (con, span) = self
            .cur
            .as_ref()
            .map(|(c, o)| (Some(*c), o.span))
            .unwrap_or((None, Span::default()));
        TypeError {
            kind,
            con,
            span,
            message,
            lhs: Some(display_ty(a)),
            rhs: Some(display_ty(b)),
            lhs_prov: prov_chain(a),
            rhs_prov: prov_chain(b),
        }
    }
}

fn rename(t: &Ty, map: &BTreeMap<VarId, VarId>) -> Ty {
    match t {
        Ty::Var(v, p) => Ty::Var(*map.get(v).unwrap_or(v), *p),
        Ty::Int(p) => Ty::Int(*p),
        Ty::Bool(p) => Ty::Bool(*p),
        Ty::Fun(a, b, p) => Ty::Fun(Box::new(rename(a, map)), Box::new(rename(b, map)), *p),
        Ty::Record(r, p) => Ty::Record(rename_row(r, map), *p),
        Ty::Variant(r, p) => Ty::Variant(rename_row(r, map), *p),
    }
}

fn rename_row(r: &Row, map: &BTreeMap<VarId, VarId>) -> Row {
    Row {
        fields: r.fields.iter().map(|(k, v)| (k.clone(), rename(v, map))).collect(),
        tail: r.tail.map(|v| *map.get(&v).unwrap_or(&v)),
        prov: r.prov,
    }
}
