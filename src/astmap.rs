use crate::syntax::*;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize)]
pub struct FlatNode {
    pub stable_id: String,
    pub kind: String,
    pub label: String,
    pub span: Span,
    pub children: Vec<String>,
    pub binder: Option<String>,
    pub binder_name: Option<String>,
    pub reference: Option<String>,
    pub depth: Option<usize>,
}

#[derive(Debug, Clone)]
#[derive(serde::Serialize)]
pub struct AstMap {
    pub source_len: usize,
    pub nodes: Vec<FlatNode>,
    pub by_stable: HashMap<String, usize>,
    pub spans: HashMap<String, Span>,
}

impl AstMap {
    pub fn get(&self, stable_id: &str) -> Option<&FlatNode> { self.by_stable.get(stable_id).map(|i| &self.nodes[*i]) }
    pub fn span(&self, stable_id: &str) -> Option<Span> { self.spans.get(stable_id).copied() }
}

#[derive(Debug, Clone, Serialize)]
pub struct NameError {
    pub message: String,
    pub span: Span,
}

#[derive(Clone)]
struct ScopeEntry {
    key: String,
    depth: usize,
}

pub fn flatten(program: &Program, source_len: usize) -> Result<AstMap, NameError> {
    let mut state = BuildState {
        source_len,
        nodes: Vec::new(),
        by_stable: HashMap::new(),
        spans: HashMap::new(),
        scopes: vec![HashMap::new()],
        binder_seq: 0,
    };
    for (index, binding) in program.bindings.iter().enumerate() {
        let path = format!("binding[{index}]");
        state.build_binding(binding, &path)?;
    }
    Ok(AstMap {
        source_len,
        nodes: state.nodes,
        by_stable: state.by_stable,
        spans: state.spans,
    })
}

struct BuildState {
    source_len: usize,
    nodes: Vec<FlatNode>,
    by_stable: HashMap<String, usize>,
    spans: HashMap<String, Span>,
    scopes: Vec<HashMap<String, ScopeEntry>>,
    binder_seq: usize,
}

impl BuildState {
    fn stable_hash(parts: &[String]) -> String {
        let mut hash = 0xcbf29ce484222325u64;
        for part in parts {
            for byte in part.bytes() {
                hash ^= byte as u64;
                hash = hash.wrapping_mul(0x100000001b3);
            }
            hash ^= 0xfe;
            hash = hash.wrapping_mul(0x100000001b3);
        }
        format!("n_{:016x}", hash)
    }

    fn push(&mut self, stable_id: String, kind: String, label: String, span: Span, children: Vec<String>) {
        let index = self.nodes.len();
        self.by_stable.insert(stable_id.clone(), index);
        self.spans.insert(stable_id.clone(), span);
        self.nodes.push(FlatNode {
            stable_id, kind, label, span, children,
            binder: None, binder_name: None, reference: None, depth: None,
        });
    }

    fn update(&mut self, stable_id: &str, update: impl FnOnce(&mut FlatNode)) {
        if let Some(index) = self.by_stable.get(stable_id).copied() {
            update(&mut self.nodes[index]);
        }
    }

    fn enter(&mut self) { self.scopes.push(HashMap::new()); }
    fn leave(&mut self) { self.scopes.pop(); }

    fn declare(&mut self, name: &str, key: String) {
        let depth = self.scopes.len() - 1;
        self.scopes.last_mut().unwrap().insert(name.to_string(), ScopeEntry { key, depth });
    }

    fn lookup(&self, name: &str, span: Span) -> Result<(String, usize), NameError> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Ok((entry.key.clone(), entry.depth));
            }
        }
        Err(NameError { message: format!("未绑定的名字 `{name}`"), span })
    }

    fn fresh_binder_key(&mut self, path: &str) -> String {
        let key = Self::stable_hash(&["binder".into(), path.into(), self.binder_seq.to_string()]);
        self.binder_seq += 1;
        key
    }

    fn build_binding(&mut self, binding: &Binding, path: &str) -> Result<(), NameError> {
        let value_path = format!("{path}.value");
        let key = self.fresh_binder_key(path);
        if binding.rec {
            self.declare(&binding.name, key.clone());
            self.build_expr(&binding.value, &value_path)?;
            self.annotate_binder(&key, &binding.name, binding.name_span, "binding");
        } else {
            self.build_expr(&binding.value, &value_path)?;
            self.declare(&binding.name, key.clone());
            self.annotate_binder(&key, &binding.name, binding.name_span, "binding");
        }
        Ok(())
    }

    fn annotate_binder(&mut self, key: &str, name: &str, span: Span, kind: &str) {
        let stable_id = Self::stable_hash(&[kind.into(), key.to_string()]);
        if !self.by_stable.contains_key(&stable_id) {
            self.push(stable_id.clone(), format!("{kind}_binder"), name.to_string(), span, Vec::new());
        }
        self.update(&stable_id, |node| {
            node.binder = Some(key.to_string());
            node.binder_name = Some(name.to_string());
        });
    }

    fn build_expr(&mut self, expr: &Expr, path: &str) -> Result<String, NameError> {
        Ok(match expr {
            Expr::Lit { value, span } => {
                let label = match value {
                    Lit::Int(v) => v.to_string(), Lit::Bool(v) => v.to_string(),
                    Lit::String(v) => format!("\"{v}\""), Lit::Unit => "()".into(),
                };
                let id = Self::stable_hash(&["lit".into(), path.into(), label.clone()]);
                self.push(id.clone(), "literal".into(), label, *span, Vec::new());
                id
            }
            Expr::Var { name, span } => {
                let (reference, depth) = self.lookup(name, *span)?;
                let id = Self::stable_hash(&["var".into(), path.into(), reference.clone()]);
                self.push(id.clone(), "variable".into(), name.clone(), *span, Vec::new());
                self.update(&id, |node| {
                    node.reference = Some(reference);
                    node.depth = Some(depth);
                });
                id
            }
            Expr::Fun { param, param_span, body, span } => {
                let body_path = format!("{path}.body");
                self.enter();
                let binder = self.fresh_binder_key(path);
                self.declare(param, binder.clone());
                let body_id = self.build_expr(body, &body_path)?;
                self.leave();
                self.annotate_binder(&binder, param, *param_span, "param");
                let id = Self::stable_hash(&["fun".into(), path.into(), binder.clone(), body_id.clone()]);
                self.push(id.clone(), "function".into(), format!("fun {param}"), *span, vec![body_id]);
                self.update(&id, |node| { node.binder = Some(binder); node.binder_name = Some(param.clone()); });
                id
            }
            Expr::App { fun, arg, span } => {
                let fun_id = self.build_expr(fun, &format!("{path}.fun"))?;
                let arg_id = self.build_expr(arg, &format!("{path}.arg"))?;
                let id = Self::stable_hash(&["app".into(), path.into(), fun_id.clone(), arg_id.clone()]);
                self.push(id.clone(), "application".into(), "@".into(), *span, vec![fun_id, arg_id]);
                id
            }
            Expr::Let { rec, name, name_span, value, body, span } => {
                let value_path = format!("{path}.value");
                let body_path = format!("{path}.body");
                let binder = self.fresh_binder_key(path);
                let value_id = if *rec {
                    self.enter();
                    self.declare(name, binder.clone());
                    let result = self.build_expr(value, &value_path)?;
                    self.leave();
                    result
                } else {
                    self.build_expr(value, &value_path)?
                };
                self.enter();
                if !*rec { self.declare(name, binder.clone()); }
                else { self.declare(name, binder.clone()); }
                let body_id = self.build_expr(body, &body_path)?;
                self.leave();
                self.annotate_binder(&binder, name, *name_span, if *rec { "rec_let" } else { "let" });
                let id = Self::stable_hash(&["let".into(), path.into(), binder.clone(), value_id.clone(), body_id.clone()]);
                self.push(id, "let".into(), format!("let {name}"), *span, vec![value_id, body_id]);
                id
            }
            Expr::Record { fields, span } => {
                let mut children = Vec::new();
                let mut key_parts = vec!["record".into(), path.into()];
                for (index, field) in fields.iter().enumerate() {
                    let value_id = self.build_expr(&field.value, &format!("{path}.field[{index}]"))?;
                    key_parts.push(field.name.clone());
                    key_parts.push(value_id);
                    children.push(value_id);
                }
                let id = Self::stable_hash(&key_parts);
                self.push(id, "record".into(), "{ ... }".into(), *span, children);
                id
            }
            Expr::Field { record, name, name_span, span } => {
                let record_id = self.build_expr(record, &format!("{path}.record"))?;
                let id = Self::stable_hash(&["field".into(), path.into(), name.into(), record_id.clone()]);
                self.push(id.clone(), "field_access".into(), format!(".{name}"), *span, vec![record_id]);
                self.update(&id, |node| { node.label = format!(".{name} @ {name_span:?}"); node.label.truncate(40); });
                id
            }
            Expr::Extend { base, fields, span } => {
                let base_id = self.build_expr(base, &format!("{path}.base"))?;
                let mut children = vec![base_id.clone()];
                let mut key_parts = vec!["extend".into(), path.into(), base_id];
                for (index, field) in fields.iter().enumerate() {
                    let value_id = self.build_expr(&field.value, &format!("{path}.field[{index}]"))?;
                    key_parts.push(field.name.clone());
                    key_parts.push(value_id.clone());
                    children.push(value_id);
                }
                let id = Self::stable_hash(&key_parts);
                self.push(id.clone(), "record_extension".into(), "{| ... }".into(), *span, children);
                id
            }
            Expr::Variant { label, payload, span } => {
                let payload_id = if let Some(payload) = payload {
                    Some(self.build_expr(payload, &format!("{path}.payload"))?)
                } else { None };
                let id = Self::stable_hash(&["variant".into(), path.into(), label.into(), payload_id.unwrap_or_default()]);
                self.push(id.clone(), "variant".into(), format!("[{label}]"), *span, Vec::new());
                id
            }
            Expr::Match { scrutinee, arms, span } => {
                let scrutinee_id = self.build_expr(scrutinee, &format!("{path}.scrutinee"))?;
                let mut children = vec![scrutinee_id.clone()];
                let mut key_parts = vec!["match".into(), path.into(), scrutinee_id];
                for (index, arm) in arms.iter().enumerate() {
                    let arm_path = format!("{path}.arm[{index}]");
                    self.enter();
                    let pattern_binder = if let Pattern::Var { name, .. } = &arm.pattern {
                        let binder = self.fresh_binder_key(&arm_path);
                        self.declare(name, binder.clone());
                        Some((binder, name.clone(), arm.pattern.span()))
                    } else if let Pattern::Variant { payload: Some(inner), .. } = arm.pattern {
                        if let Pattern::Var { name, span } = inner.as_ref() {
                            let binder = self.fresh_binder_key(&arm_path);
                            self.declare(name, binder.clone());
                            Some((binder, name.clone(), *span))
                        } else { None }
                    } else { None };
                    let body_id = self.build_expr(&arm.body, &format!("{arm_path}.body"))?;
                    self.leave();
                    if let Some((binder, name, binder_span)) = pattern_binder {
                        self.annotate_binder(&binder, &name, binder_span, "match");
                    }
                    key_parts.push(pattern_key(&arm.pattern));
                    key_parts.push(body_id.clone());
                    children.push(body_id);
                }
                let id = Self::stable_hash(&key_parts);
                self.push(id.clone(), "match".into(), "match".into(), *span, children);
                id
            }
        })
    }
}

fn pattern_key(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Wildcard(_) => "_".into(),
        Pattern::Var { name, .. } => format!("var:{name}"),
        Pattern::Variant { label, payload, .. } => {
            format!("variant:{label}:{}", payload.as_ref().map(|p| pattern_key(p.as_ref())).unwrap_or_default())
        }
    }
}
