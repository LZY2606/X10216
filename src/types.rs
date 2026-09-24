//! Type representation with provenance. Types are interned: a [`TypeId`] is a
//! unique integer assigned by [`TypeArena`]. Every node remembers the source
//! range that produced it, so the final type can be traced back to the
//! expression.

use std::cell::{Cell, RefCell};
use std::fmt::Write as _;
use std::rc::Rc;

use serde::Serialize;

pub type RcStr = Rc<str>;

/// Byte range into the source file.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start: start as u32, end: end as u32 }
    }
}

/// What produced a type node: an expression or a constraint step.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", content = "detail")]
pub enum Origin {
    /// Created fresh for an AST node, e.g. the result type of an application.
    Fresh { node: u32, label: RcStr },
    /// A literal constant.
    Const { node: u32 },
    /// A variable occurrence: node is the occurrence, binder is the binding
    /// node it resolves to (stable under renaming).
    VarRef { node: u32, binder: u32 },
    /// A type scheme instantiation; `scheme` identifies the generalized let.
    Inst { node: u32, scheme: u32 },
    /// Introduced by a unification step.
    Solved { step: u32 },
    /// A builtin operator / ambient binding.
    Builtin { name: RcStr },
    /// A structural constructor node (record/variant/arrow...).
    Node { node: u32 },
}

#[derive(Clone, Debug)]
pub enum TyKind {
    /// Unification variable.
    Var(u32),
    Con(RcStr),
    Arr(TypeId, TypeId),
    /// Record row type.
    Record(Row),
    /// Variant row type.
    Variant(Row),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

#[derive(Clone, Debug)]
pub struct TyNode {
    pub kind: TyKind,
    pub origin: Origin,
    pub span: Option<Span>,
}

#[derive(Clone, Debug)]
pub struct Row {
    /// Kept sorted by field name so structurally equal rows share layout.
    pub fields: Vec<(RcStr, TypeId)>,
    /// None => closed row. Some(var) => open, extends `var`.
    pub tail: Option<u32>,
    pub origin: Origin,
    pub span: Option<Span>,
}

pub struct TypeArena {
    nodes: RefCell<Vec<TyNode>>,
    var_count: Cell<u32>,
}

impl TypeArena {
    pub fn new() -> Self {
        TypeArena { nodes: RefCell::new(Vec::new()), var_count: Cell::new(0) }
    }

    pub fn fresh_var(&self, origin: Origin, span: Option<Span>) -> TypeId {
        let v = self.var_count.get();
        self.var_count.set(v + 1);
        self.alloc(TyKind::Var(v), origin, span)
    }

    pub fn alloc(&self, kind: TyKind, origin: Origin, span: Option<Span>) -> TypeId {
        let mut nodes = self.nodes.borrow_mut();
        let id = nodes.len() as u32;
        nodes.push(TyNode { kind, origin, span });
        TypeId(id)
    }

    pub fn with<R>(&self, f: impl FnOnce(&TyNode) -> R) -> R {
        let nodes = self.nodes.borrow();
        // We return by value through the closure so the borrow is released.
        // Callers needing structural data should use [`read`].
        f(&nodes[0])
    }

    /// Access a node's data by copying the information the caller asks for.
    /// Most code uses [`view`] which gives a borrowed snapshot.
    pub fn view<R>(&self, id: TypeId, f: impl FnOnce(&TyNode) -> R) -> R {
        let nodes = self.nodes.borrow();
        f(&nodes[id.0 as usize])
    }

    pub fn len(&self) -> usize {
        self.nodes.borrow().len()
    }

    pub fn con(&self, name: &str) -> TypeId {
        self.alloc(
            TyKind::Con(Rc::from(name)),
            Origin::Builtin { name: Rc::from(name) },
            None,
        )
    }

    /// Build a record/variant, sorting fields by name.
    pub fn row(&self, mut fields: Vec<(RcStr, TypeId)>, tail: Option<u32>, variant: bool,
             origin: Origin, span: Option<Span>) -> TypeId {
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        let row = Row { fields, tail, origin, span };
        let kind = if variant { TyKind::Variant(row) } else { TyKind::Record(row) };
        self.alloc(kind, origin, span)
    }
}

impl Row {
    pub fn find(&self, name: &str) -> Option<usize> {
        self.fields.binary_search_by(|(n, _)| n.as_ref().cmp(name)).ok()
    }
    pub fn get(&self, name: &str) -> Option<TypeId> {
        self.find(name).map(|i| self.fields[i].1)
    }
}

/// Render a type for display, resolving union-find variables lazily via a
/// closure (the solver's resolved view).
pub fn render<F: Fn(u32) -> Option<TypeId>>(id: TypeId, arena: &TypeArena, resolve: &F) -> String {
    let mut out = String::new();
    render_ty(id, arena, resolve, &mut out, 0);
    out
}

fn render_ty<F: Fn(u32) -> Option<TypeId>>(id: TypeId, arena: &TypeArena, resolve: &F,
        out: &mut String, depth: u32) {
    let snapshot = arena.view(id, |n| n.kind.clone());
    match snapshot {
        TyKind::Var(v) => match resolve(v) {
            Some(t) if t != id => render_ty(t, arena, resolve, out, depth),
            _ => { let _ = write!(out, "?{v}"); }
        },
        TyKind::Con(n) => out.push_str(&n),
        TyKind::Arr(a, b) => {
            if depth > 0 { out.push('('); }
            render_ty(a, arena, resolve, out, depth + 1);
            out.push_str(" -> ");
            render_ty(b, arena, resolve, out, 0);
            if depth > 0 { out.push(')'); }
        }
        TyKind::Record(r) | TyKind::Variant(r) => {
            let variant = matches!(snapshot, TyKind::Variant(_));
            out.push_str(if variant { "[|" } else { "{" });
            for (i, (name, ty)) in r.fields.iter().enumerate() {
                if i > 0 { out.push_str(", "); }
                let _ = write!(out, "{name}: ");
                render_ty(*ty, arena, resolve, out, 0);
            }
            if let Some(t) = r.tail {
                if !r.fields.is_empty() { out.push_str(", "); }
                match resolve(t) {
                    Some(t2) => render_ty(t2, arena, resolve, out, 0),
                    None => { let _ = write!(out, "?{t}"); }
                }
            }
            out.push_str(if variant { "|]" } else { "}" });
        }
    }
}
