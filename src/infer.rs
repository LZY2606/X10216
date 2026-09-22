use crate::syntax::*;
use crate::types::*;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "camelCase")]
pub enum StepSpec {
    Introduce { id: String, node: String, span: Span, variable: Vid, role: String },
    Instantiate { id: String, node: String, span: Span, source: String, scheme_id: Option<String>, result: Ty, monomorphic: bool },
    Generalize { id: String, node: String, span: Span, name: String, value: Ty, scheme: Scheme },
    Equal(Constraint),
    EnterScope { id: String, node: String, scope_key: String },
    LeaveScope { id: String, node: String, scope_key: String },
}

#[derive(Clone)]
enum Binding {
    Mono(Ty),
    Scheme(Scheme),
}

#[derive(Default)]
struct Scope {
    bindings: Vec<(String, Binding)>,
}

pub struct Generator<'a> {
    root: &'a ExprNode,
    events: Vec<StepSpec>,
    scopes: Vec<(String, String)>,
    counter: usize,
}

#[derive(Debug, Clone, Serialize)]
pub struct Generated {
    pub root_type_variable: Vid,
    pub steps: Vec<StepSpec>,
    pub node_types: BTreeMap<String, Ty>,
}

pub fn generate(root: &ExprNode) -> Generated {
    let mut g = Generator { root, events: Vec::new(), scopes: Vec::new(), counter: 0 };
    g.enter(root.id.clone(), root.id.clone());
    let root_ty = Ty::var(g.fresh("root")).with(root.id.clone());
    g.emit_intro(root.id.clone(), root.span, var_name(&root_ty), "program".into());
    let mut scopes = vec![Scope::default()];
    let ty = g.expr(root, &mut scopes, "root");
    g.equal(root, "program", root_ty.clone(), ty);
    g.leave(root.id.clone(), root.id.clone());
    Generated { root_type_variable: var_name(&root_ty), steps: g.events, node_types: BTreeMap::new() }
}

impl<'a> Generator<'a> {
    fn fresh(&mut self, why: &str) -> Vid {
        let n = self.counter;
        self.counter += 1;
        format!("?{n}_{}", sanitize(why))
    }
    fn scope_key(&self) -> String {
        self.scopes.iter().map(|(_, key)| key.clone()).collect::<Vec<_>>().join(">")
    }
    fn genv_key(&self) -> String {
        self.scopes.iter().filter(|(node, _)| node.starts_with("root/value") || node.ends_with("/value")).map(|(_, key)| key.clone()).collect::<Vec<_>>().join(">")
    }
    fn enter(&mut self, node: String, key: String) {
        self.scopes.push((node, key));
        self.events.push(StepSpec::EnterScope { id: format!("enter:{}", key), node: self.scopes.last().map(|x| x.0.clone()).unwrap_or_default(), scope_key: key });
    }
    fn leave(&mut self, node: String, key: String) {
        self.scopes.pop();
        self.events.push(StepSpec::LeaveScope { id: format!("leave:{}", key), node, scope_key: key });
    }
    fn emit_intro(&mut self, node: String, span: Span, variable: Vid, role: String) {
        self.events.push(StepSpec::Introduce { id: format!("intro:{node}#{variable}"), node, span, variable, role });
    }
    fn equal(&mut self, node: &ExprNode, role: &str, left: Ty, right: Ty) {
        let scope = self.scope_key();
        let genv = self.genv_key();
        let id = format!("eq:{}:{}", node.id, self.counter);
        self.counter += 1;
        let key = format!("scope={scope}|genv={genv}|role={role}|id={id}");
        self.events.push(StepSpec::Equal(Constraint {
            id: id.clone(), origin: node.id.clone(), span: node.span, left, right,
            role: role.into(), cache_key: key, scope_key: scope, genv_key: genv,
        }));
    }
    fn lookup(&mut self, scopes: &[Scope], node: &ExprNode, name: &str, source: &str) -> Ty {
        for scope in scopes.iter().rev() {
            for (bound_name, binding) in scope.bindings.iter().rev() {
                if bound_name == name {
                    return match binding {
                        Binding::Mono(ty) => {
                            self.events.push(StepSpec::Instantiate {
                                id: format!("inst:{}:{}", node.id, self.counter),
                                node: node.id.clone(), span: node.span, source: source.into(),
                                scheme_id: None, result: ty.clone(), monomorphic: true,
                            });
                            ty.clone()
                        }
                        Binding::Scheme(scheme) => {
                            let copied = self.instantiate(scheme, node);
                            self.events.push(StepSpec::Instantiate {
                                id: format!("inst:{}:{}", node.id, self.counter),
                                node: node.id.clone(), span: node.span, source: source.into(),
                                scheme_id: Some(scheme.id.clone()), result: copied.1.clone(),
                                monomorphic: scheme.monomorphic,
                            });
                            copied.1
                        }
                    };
                }
            }
        }
        Ty::var(self.fresh(&format!("{node.id}:unbound"))).with(node.id.clone())
    }
    fn instantiate(&mut self, scheme: &Scheme, node: &ExprNode) -> (BTreeMap<Vid, Ty>, Ty) {
        let mut mapping = BTreeMap::new();
        if scheme.monomorphic {
            return (mapping, scheme.body.clone());
        }
        for vid in &scheme.quantifiers {
            let fresh = Ty::var(self.fresh(&format!("instantiate:{node.id}"))).with(node.id.clone());
            mapping.insert(vid.clone(), fresh);
        }
        let body = substitute(&scheme.body, &mapping);
        (mapping, body)
    }
    fn expr(&mut self, node: &ExprNode, scopes: &mut Vec<Scope>, kind: &str) -> Ty {
        let result = match &node.node {
            Expr::Int(_) => Ty::int().with(node.id.clone()),
            Expr::Bool(_) => Ty::bool().with(node.id.clone()),
            Expr::Var { name, .. } => self.lookup(scopes, node, name, kind),
            Expr::Fun { param, body } => {
                let param_ty = Ty::var(self.fresh(&format!("{}:param:{param}", node.id))).with(node.id.clone());
                self.emit_intro(node.id.clone(), node.span, var_name(&param_ty), format!("parameter {param}"));
                self.enter(format!("{}/body", node.id), format!("{}:fun", node.id));
                let mut scope = Scope::default();
                scope.bindings.push((param.clone(), Binding::Mono(param_ty.clone())));
                scopes.push(scope);
                let body_ty = self.expr(body, scopes, "function-body");
                scopes.pop();
                self.leave(format!("{}/body", node.id), format!("{}:fun", node.id));
                Ty::fun(param_ty, body_ty).with(node.id.clone())
            }
            Expr::App { fun, arg } => {
                let f = self.expr(fun, scopes, "operator");
                let a = self.expr(arg, scopes, "argument");
                let result = Ty::var(self.fresh(&format!("{}:result", node.id))).with(node.id.clone());
                self.emit_intro(node.id.clone(), node.span, var_name(&result), "application result".into());
                self.equal(fun, "application-operator", f, Ty::fun(a, result.clone()));
                result.with(node.id.clone())
            }
            Expr::Let { name, rec, value, body } => {
                self.enter(format!("{}/value", node.id), format!("{}:let-value:{}", node.id, rec));
                if *rec {
                    let scheme_ty = Ty::var(self.fresh(&format!("{}:recursive:{name}", node.id))).with(value.id.clone());
                    self.emit_intro(value.id.clone(), value.span, var_name(&scheme_ty), format!("recursive binding {name}"));
                    let mut scope = Scope::default();
                    scope.bindings.push((name.clone(), Binding::Mono(scheme_ty.clone())));
                    scopes.push(scope);
                    let inferred = self.expr(value, scopes, "recursive-value");
                    scopes.pop();
                    self.equal(value, "recursive-binding", scheme_ty.clone(), inferred);
                    let scheme = Scheme { id: format!("scheme:{}", node.id), quantifiers: Vec::new(), body: scheme_ty, monomorphic: true, value_restricted: false };
                    self.events.push(StepSpec::Generalize { id: format!("gen:{}", node.id), node: node.id.clone(), span: node.span, name: name.clone(), value: scheme.body.clone(), scheme: scheme.clone() });
                    let mut use_scope = Scope::default();
                    use_scope.bindings.push((name.clone(), Binding::Scheme(scheme)));
                    scopes.push(use_scope);
                } else {
                    let value_ty = self.expr(value, scopes, "let-value");
                    let is_value = is_value_expression(value);
                    let (quantifiers, generalized) = if is_value { generalize(value_ty, 0, &Default::default()) } else { (Vec::new(), value_ty.clone()) };
                    let body_for_scheme = if is_value { generalized } else { value_ty.clone() };
                    let scheme = Scheme { id: format!("scheme:{}", node.id), quantifiers: if is_value { quantifiers } else { Vec::new() }, body: body_for_scheme, monomorphic: !is_value, value_restricted: !is_value };
                    self.events.push(StepSpec::Generalize { id: format!("gen:{}", node.id), node: node.id.clone(), span: node.span, name: name.clone(), value: value_ty, scheme: scheme.clone() });
                    let mut use_scope = Scope::default();
                    use_scope.bindings.push((name.clone(), Binding::Scheme(scheme)));
                    scopes.push(use_scope);
                }
                let body_ty = self.expr(body, scopes, "let-body");
                scopes.pop();
                self.leave(format!("{}/value", node.id), format!("{}:let-value:{}", node.id, rec));
                body_ty.with(node.id.clone())
            }
            Expr::Record { fields } => {
                let mut map = BTreeMap::new();
                for f in fields {
                    let ty = self.expr(&f.value, scopes, &format!("record-field:{}", f.label));
                    map.insert(f.label.clone(), ty);
                }
                Ty::Record { row: Row::closed(map), proof: Proof::from([node.id.clone()]) }
            }
            Expr::Field { record, label } => {
                let recv = self.expr(record, scopes, "field-record");
                let field = Ty::var(self.fresh(&format!("{}:field:{label}", node.id))).with(node.id.clone());
                self.emit_intro(node.id.clone(), node.span, var_name(&field), format!("field {label}"));
                let mut fields = BTreeMap::new();
                fields.insert(label.clone(), field.clone());
                let extension = self.fresh(&format!("{}:row-rest", node.id));
                self.equal(record, &format!("field-access:{label}"), recv, Ty::Record { row: Row::open(fields, extension), proof: Proof::new() });
                field.with(node.id.clone())
            }
            Expr::Extend { label, value, rest } => {
                let field_ty = self.expr(value, scopes, "extended-field");
                let rest_ty = match rest {
                    Some(expr) => self.expr(expr, scopes, "record-rest"),
                    None => {
                        let ext = self.fresh(&format!("{}:empty-rest", node.id));
                        Ty::Record { row: Row::open(BTreeMap::new(), ext), proof: Proof::new() }
                    }
                };
                let mut fields = BTreeMap::new();
                fields.insert(label.clone(), field_ty);
                let ext = self.fresh(&format!("{}:extended-rest", node.id));
                let result = Ty::Record { row: Row::open(fields, ext), proof: Proof::from([node.id.clone()]) };
                self.equal(node, &format!("record-extension:{label}"), rest_ty, result.clone());
                result
            }
            Expr::Variant { label, payload } => {
                let payload_ty = match payload {
                    Some(expr) => self.expr(expr, scopes, "variant-payload"),
                    None => Ty::unit().with(node.id.clone()),
                };
                let mut fields = BTreeMap::new();
                fields.insert(label.clone(), payload_ty);
                let ext = self.fresh(&format!("{}:variant-rest", node.id));
                Ty::Variant { row: Row::open(fields, ext), proof: Proof::from([node.id.clone()]) }
            }
            Expr::Match { scrutinee, arms } => {
                let scrut_ty = self.expr(scrutinee, scopes, "match-scrutinee");
                let result = Ty::var(self.fresh(&format!("{}:match-result", node.id))).with(node.id.clone());
                self.emit_intro(node.id.clone(), node.span, var_name(&result), "match result".into());
                for arm in arms {
                    self.enter(arm.body.id.clone(), format!("{}:arm", arm.pattern.id));
                    let mut arm_scope = Scope::default();
                    let mut pat_fields = BTreeMap::new();
                    match &arm.pattern.node {
                        Pattern::Binding(name) => {
                            arm_scope.bindings.push((name.clone(), Binding::Mono(scrut_ty.clone())));
                        }
                        Pattern::Variant { label, binding } => {
                            let payload = match binding {
                                Some(name) => {
                                    let ty = Ty::var(self.fresh(&format!("{}:payload:{name}", arm.pattern.id))).with(arm.pattern.id.clone());
                                    arm_scope.bindings.push((name.clone(), Binding::Mono(ty.clone())));
                                    ty
                                }
                                None => Ty::unit().with(arm.pattern.id.clone()),
                            };
                            pat_fields.insert(label.clone(), payload);
                        }
                    }
                    let ext = self.fresh(&format!("{}:pat-rest", arm.pattern.id));
                    if matches!(arm.pattern.node, Pattern::Variant { .. }) {
                        self.equal(&ExprNode::new(Expr::Bool(false), arm.pattern.span, arm.pattern.id.clone()), &format!("variant-pattern:{}", arm.pattern.id), scrut_ty.clone(), Ty::Variant { row: Row::open(pat_fields, ext), proof: Proof::new() });
                    }
                    scopes.push(arm_scope);
                    let body_ty = self.expr(&arm.body, scopes, "match-arm-body");
                    scopes.pop();
                    self.equal(&arm.body, "match-arm-result", result.clone(), body_ty);
                    self.leave(arm.body.id.clone(), format!("{}:arm", arm.pattern.id));
                }
                result.with(node.id.clone())
            }
            Expr::Annotation { expr, ty } => {
                let inferred = self.expr(expr, scopes, "annotated");
                let encoded = self.annot_type(ty, node);
                self.equal(node, "annotation", inferred, encoded);
                Ty::var(self.fresh(&format!("{}:annotated", node.id))).with(node.id.clone())
            }
        };
        result
    }
    fn annot_type(&mut self, node: &TypeNode, origin: &ExprNode) -> Ty {
        let p = Proof::from([origin.id.clone(), format!("annotation:{}", node.id)]);
        match &node.node {
            Type::Int => Ty::int(),
            Type::Bool => Ty::bool(),
            Type::Var(name) => Ty::var(format!("annot:{name}:{}", origin.id)),
            Type::Fun(a, b) => Ty::fun(self.annot_type(a, origin), self.annot_type(b, origin)),
            Type::Record(row) | Type::Variant(row) => {
                let mut fields = BTreeMap::new();
                for field in &row.fields {
                    fields.insert(field.label.node.clone(), self.annot_type(&field.value, origin));
                }
                let r = match row.extension {
                    Some(ref ext) => Row::open(fields, format!("annotrow:{}:{}", origin.id, ext.node)),
                    None => Row::closed(fields),
                };
                if matches!(node.node, Type::Record(_)) { Ty::Record { row: r, proof: p } } else { Ty::Variant { row: r, proof: p } }
            }
        }
    }
}

pub fn var_name(ty: &Ty) -> Vid {
    match ty { Ty::Var { vid, .. } => vid.clone(), _ => String::new() }
}

pub fn sanitize(s: &str) -> String {
    s.chars().map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' }).take(48).collect()
}

pub fn is_value_expression(e: &ExprNode) -> bool {
    match &e.node {
        Expr::Int(_) | Expr::Bool(_) | Expr::Variant { .. } => true,
        Expr::Fun { .. } => true,
        Expr::Record { fields } => fields.iter().all(|f| is_value_expression(&f.value)),
        Expr::Annotation { expr, .. } => is_value_expression(expr),
        _ => false,
    }
}

pub fn free_vars(ty: &Ty, bound: &BTreeSet<Vid>) -> BTreeSet<Vid> {
    let mut out = BTreeSet::new();
    collect_free(ty, bound, &mut out);
    out
}

fn collect_free(ty: &Ty, bound: &BTreeSet<Vid>, out: &mut BTreeSet<Vid>) {
    match ty {
        Ty::Var { vid, .. } if !bound.contains(vid) => { out.insert(vid.clone()); }
        Ty::Fun { from, to, .. } => { collect_free(from, bound, out); collect_free(to, bound, out); }
        Ty::Record { row, .. } | Ty::Variant { row, .. } => {
            for value in row.fields.values() { collect_free(value, bound, out); }
            if let Some(ext) = &row.extension { if !bound.contains(ext) { out.insert(ext.clone()); } }
        }
        _ => {}
    }
}

pub fn generalize(ty: Ty, _level: u32, _ambient: &BTreeSet<Vid>) -> (Vec<Vid>, Ty) {
    let frees = free_vars(&ty, &BTreeSet::new());
    (frees.into_iter().collect(), ty)
}

pub fn substitute(ty: &Ty, mapping: &BTreeMap<Vid, Ty>) -> Ty {
    match ty {
        Ty::Var { vid, proof } => mapping.get(vid).cloned().unwrap_or_else(|| ty.clone()),
        Ty::Fun { from, to, proof } => Ty::Fun { from: Box::new(substitute(from, mapping)), to: Box::new(substitute(to, mapping)), proof: proof.clone() },
        Ty::Record { row, proof } => Ty::Record { row: substitute_row(row, mapping), proof: proof.clone() },
        Ty::Variant { row, proof } => Ty::Variant { row: substitute_row(row, mapping), proof: proof.clone() },
        _ => ty.clone(),
    }
}

fn substitute_row(row: &Row, mapping: &BTreeMap<Vid, Ty>) -> Row {
    let mut fields = BTreeMap::new();
    for (k, v) in &row.fields { fields.insert(k.clone(), substitute(v, mapping)); }
    let extension = row.extension.clone().and_then(|ext| match mapping.get(&ext) {
        Some(Ty::Var { vid, .. }) => Some(vid.clone()),
        Some(Ty::Record { row: other, .. }) | Some(Ty::Variant { row: other, .. }) => {
            for (k, v) in &other.fields { fields.entry(k.clone()).or_insert_with(|| v.clone()); }
            other.extension.clone()
        }
        _ => Some(ext),
    });
    Row { fields, extension }
}
