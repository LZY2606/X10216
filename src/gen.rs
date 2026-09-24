use crate::constraints::*;
use crate::identity::Identity;
use crate::syntax::*;
use crate::types::*;
use std::collections::BTreeMap;

#[derive(Clone)]
struct Entry {
    name: String,
    binder_id: String,
    scheme: Option<Scheme>,
    monomorphic: Option<Ty>,
}

pub struct Generator<'a> {
    identity: &'a Identity,
    gen: &'a mut VarGen,
    events: Vec<Event>,
    scopes: Vec<Vec<Entry>>,
    event_seed: BTreeMap<String, usize>,
}

impl<'a> Generator<'a> {
    pub fn run(program: &Program, identity: &'a Identity, gen: &'a mut VarGen) -> Vec<Event> {
        let mut g = Generator { identity, gen, events: Vec::new(), scopes: vec![vec![]], event_seed: BTreeMap::new() };
        for decl in &program.decls {
            let value_form = is_value_form(&decl.rhs);
            let rhs_ty = g.fresh(decl.rhs.span(), TraceRole::Expr);
            g.emit(EventKind::BeginLet { rec: decl.rec, name: decl.name.clone(), rhs_ty: rhs_ty.clone(), value_restriction: !value_form }, decl.span, &format!("{}/binder", identity.id_at(decl.name_span)), None);
            if decl.rec {
                g.push(Entry { name: decl.name.clone(), binder_id: format!("{}/binder", identity.id_at(decl.name_span)), scheme: None, monomorphic: Some(rhs_ty.clone()) });
            }
            let inferred = g.expr(&decl.rhs);
            g.assert(inferred, rhs_ty.clone(), AssertReason::LetRhs, decl.rhs.span());
            let generalized = g.generalize(&decl.name, &rhs_ty, value_form, decl.rhs.span(), &format!("{}/binder", identity.id_at(decl.name_span)));
            if decl.rec { g.pop(&decl.name); }
            g.push(Entry { name: decl.name.clone(), binder_id: format!("{}/binder", identity.id_at(decl.name_span)), scheme: generalized, monomorphic: None });
            g.emit(EventKind::EndLet { name: decl.name.clone() }, decl.span, &format!("{}/binder", identity.id_at(decl.name_span)), None);
        }
        if let Some(result) = &program.result { g.expr(result); }
        g.events
    }

    fn fresh(&mut self, span: Span, role: TraceRole) -> Ty {
        self.gen.fresh(0, span, role)
    }
    fn node_id(&self, span: Span) -> String { self.identity.id_at(span) }
    fn push(&mut self, entry: Entry) { self.scopes.last_mut().unwrap().push(entry); }
    fn pop(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(pos) = scope.iter().rposition(|e| e.name == name) { scope.remove(pos); }
        }
    }
    fn lookup(&self, name: &str) -> Option<&Entry> {
        for scope in self.scopes.iter().rev() {
            if let Some(e) = scope.iter().rev().find(|e| e.name == name) { return Some(e); }
        }
        None
    }
