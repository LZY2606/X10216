use crate::ast::NodeId;
use std::collections::{BTreeMap, BTreeSet};

pub type TVar = u32;
pub type RVar = u32;
pub type TypeNodeId = u64;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub enum Type {
    Var(TVar),
    Con(String),
    App(Box<Type>, Box<Type>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub struct Row {
    pub fields: BTreeMap<String, Type>,
    pub tail: RowTail,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub enum RowTail {
    Empty,
    Open(RVar),
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Scheme {
    pub params: Vec<Param>,
    pub body: Type,
    pub generalized: bool,
    pub value_restricted: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, serde::Serialize)]
pub enum Param {
    Type(TVar),
    Row(RVar),
}

#[derive(Clone, Debug, Default, serde::Serialize)]
pub struct Subst {
    pub types: BTreeMap<TVar, Type>,
    pub rows: BTreeMap<RVar, Row>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct TypeNode {
    pub id: TypeNodeId,
    pub term: TypeTerm,
    pub origins: BTreeSet<NodeId>,
    pub alias: Option<TVar>,
    pub row_var: Option<RVar>,
    pub label: String,
}

#[derive(Clone, Debug, serde::Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TypeTerm {
    Variable { name: String, level: u32 },
    Constructor { name: String },
    Application { function: TypeNodeId, argument: TypeNodeId },
    Record { fields: BTreeMap<String, TypeNodeId>, tail: Option<RVar>, closed: bool },
    Variant { fields: BTreeMap<String, TypeNodeId>, tail: Option<RVar>, closed: bool },
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Provenance {
    pub nodes: BTreeSet<NodeId>,
    pub label: String,
}

pub struct TypeFactory {
    nodes: BTreeMap<TypeNodeId, TypeNode>,
    next_node: TypeNodeId,
    next_type_var: TVar,
    next_row_var: RVar,
}

impl Type {
    pub fn arrow(from: Type, to: Type) -> Self {
        Type::App(Box::new(Type::App(Box::new(Type::Con("->".into())), Box::new(from))), Box::new(to))
    }

    pub fn record(fields: BTreeMap<String, Type>, closed: bool) -> Self {
        Type::Record(Row { fields, tail: if closed { RowTail::Empty } else { RowTail::Open(0) } })
    }

    pub fn variant(fields: BTreeMap<String, Type>, closed: bool) -> Self {
        Type::Variant(Row { fields, tail: if closed { RowTail::Empty } else { RowTail::Open(0) } })
    }

    pub fn occurs(&self, var: TVar) -> bool {
        match self {
            Type::Var(other) => *other == var,
            Type::Con(_) => false,
            Type::App(function, argument) => function.occurs(var) || argument.occurs(var),
            Type::Record(row) | Type::Variant(row) => row.fields.values().any(|ty| ty.occurs(var))
                || matches!(row.tail, RowTail::Open(rv) if row_occurs_var(rv, var)),
        }
    }

    pub fn row_occurs(&self, target: RVar) -> bool {
        match self {
            Type::Record(row) | Type::Variant(row) => {
                matches!(&row.tail, RowTail::Open(rv) if *rv == target)
                    || row.fields.values().any(|ty| ty.row_occurs(target))
            }
            Type::App(function, argument) => function.row_occurs(target) || argument.row_occurs(target),
            _ => false,
        }
    }

    pub fn type_vars(&self, output: &mut BTreeSet<TVar>) {
        match self {
            Type::Var(var) => { output.insert(*var); }
            Type::Con(_) => {}
            Type::App(left, right) => { left.type_vars(output); right.type_vars(output); }
            Type::Record(row) | Type::Variant(row) => {
                for value in row.fields.values() { value.type_vars(output); }
            }
        }
    }

    pub fn row_vars(&self, output: &mut BTreeSet<RVar>) {
        match self {
            Type::Record(row) | Type::Variant(row) => {
                if let RowTail::Open(var) = &row.tail { output.insert(*var); }
                for value in row.fields.values() { value.row_vars(output); }
            }
            Type::App(left, right) => { left.row_vars(output); right.row_vars(output); }
            Type::Var(_) | Type::Con(_) => {}
        }
    }
}

fn row_occurs_var(_rv: RVar, _var: TVar) -> bool { false }

impl Row {
    pub fn open(tail: RVar) -> Self { Row { fields: BTreeMap::new(), tail: RowTail::Open(tail) } }
}

impl Subst {
    pub fn apply_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(var) => match self.types.get(var) {
                Some(found) => self.apply_type(found),
                None => ty.clone(),
            },
            Type::Con(_) => ty.clone(),
            Type::App(left, right) => Type::App(Box::new(self.apply_type(left)), Box::new(self.apply_type(right))),
            Type::Record(row) | Type::Variant(row) => {
                let kind = if matches!(ty, Type::Record(_)) { 'r' } else { 'v' };
                let mapped = self.apply_row(row);
                if kind == 'r' { Type::Record(mapped) } else { Type::Variant(mapped) }
            }
        }
    }

    pub fn apply_row(&self, row: &Row) -> Row {
        let mut fields = BTreeMap::new();
        for (name, ty) in &row.fields {
            fields.insert(name.clone(), self.apply_type(ty));
        }
        match &row.tail {
            RowTail::Empty => Row { fields, tail: RowTail::Empty },
            RowTail::Open(var) => match self.rows.get(var) {
                Some(mapped) => {
                    let mut merged = self.apply_row(mapped);
                    for (name, ty) in fields {
                        merged.fields.insert(name, ty);
                    }
                    merged
                }
                None => Row { fields, tail: RowTail::Open(*var) },
            },
        }
    }

    pub fn bind_type(&mut self, var: TVar, ty: Type) {
        self.types.insert(var, ty);
    }

    pub fn bind_row(&mut self, rvar: RVar, row: Row) {
        self.rows.insert(rvar, row);
    }

    pub fn free_type_vars(&self, ty: &Type, output: &mut BTreeSet<TVar>) {
        self.apply_type(ty).type_vars(output);
    }
}

impl Default for TypeFactory {
    fn default() -> Self { Self::new() }
}

impl TypeFactory {
    pub fn new() -> Self {
        Self { nodes: BTreeMap::new(), next_node: 1, next_type_var: 1, next_row_var: 1 }
    }

    pub fn fresh_type_var(&mut self, origins: BTreeSet<NodeId>, label: String, level: u32) -> (Type, TypeNodeId) {
        let var = self.next_type_var;
        self.next_type_var += 1;
        let id = self.next_node;
        self.next_node += 1;
        self.nodes.insert(id, TypeNode {
            id,
            term: TypeTerm::Variable { name: var_name(var), level },
            origins,
            alias: Some(var),
            row_var: None,
            label,
        });
        (Type::Var(var), id)
    }

    pub fn fresh_row_var(&mut self, origins: BTreeSet<NodeId>, label: String) -> (RVar, TypeNodeId) {
        let var = self.next_row_var;
        self.next_row_var += 1;
        let id = self.next_node;
        self.next_node += 1;
        self.nodes.insert(id, TypeNode {
            id,
            term: TypeTerm::Variable { name: row_name(var), level: 0 },
            origins,
            alias: None,
            row_var: Some(var),
            label,
        });
        (var, id)
    }

    pub fn constructor(&mut self, name: &str, origins: BTreeSet<NodeId>) -> (Type, TypeNodeId) {
        let id = self.next_node;
        self.next_node += 1;
        self.nodes.insert(id, TypeNode {
            id,
            term: TypeTerm::Constructor { name: name.into() },
            origins,
            alias: None,
            row_var: None,
            label: name.into(),
        });
        (Type::Con(name.into()), id)
    }

    pub fn application(&mut self, function: Type, argument: Type, origins: BTreeSet<NodeId>, function_node: TypeNodeId, argument_node: TypeNodeId) -> (Type, TypeNodeId) {
        let ty = Type::App(Box::new(function), Box::new(argument));
        let id = self.next_node;
        self.next_node += 1;
        self.nodes.insert(id, TypeNode {
            id,
            term: TypeTerm::Application { function: function_node, argument: argument_node },
            origins,
            alias: None,
            row_var: None,
            label: "application".into(),
        });
        (ty, id)
    }

    pub fn row_type(&mut self, kind: RowKind, fields: Vec<(String, Type, TypeNodeId)>, tail: Option<RVar>, origins: BTreeSet<NodeId>) -> (Type, TypeNodeId) {
        let mut field_types = BTreeMap::new();
        let mut field_nodes = BTreeMap::new();
        for (name, ty, node) in fields {
            field_types.insert(name.clone(), ty);
            field_nodes.insert(name, node);
        }
        let row = Row { fields: field_types, tail: tail.map_or(RowTail::Empty, RowTail::Open) };
        let id = self.next_node;
        self.next_node += 1;
        let term = match kind {
            RowKind::Record => TypeTerm::Record { fields: field_nodes, tail, closed: tail.is_none() },
            RowKind::Variant => TypeTerm::Variant { fields: field_nodes, tail, closed: tail.is_none() },
        };
        self.nodes.insert(id, TypeNode {
            id, term, origins, alias: None, row_var: None,
            label: if matches!(kind, RowKind::Record) { "record" } else { "variant" }.into(),
        });
        let ty = if matches!(kind, RowKind::Record) { Type::Record(row) } else { Type::Variant(row) };
        (ty, id)
    }

    pub fn merge_origins(&mut self, left: TypeNodeId, right: TypeNodeId) {
        let right_origins = self.nodes.get(&right).map(|node| node.origins.clone()).unwrap_or_default();
        if let Some(node) = self.nodes.get_mut(&left) {
            for origin in right_origins { node.origins.insert(origin); }
        }
    }

    pub fn set_alias(&mut self, node: TypeNodeId, var: TVar) {
        if let Some(found) = self.nodes.get_mut(&node) { found.alias = Some(var); }
    }

    pub fn set_row_alias(&mut self, node: TypeNodeId, var: RVar) {
        if let Some(found) = self.nodes.get_mut(&node) { found.row_var = Some(var); }
    }

    pub fn nodes(&self) -> &BTreeMap<TypeNodeId, TypeNode> { &self.nodes }

    pub fn node_count(&self) -> TypeNodeId { self.next_node - 1 }
}

#[derive(Clone, Copy, Debug)]
pub enum RowKind { Record, Variant }

pub fn var_name(var: TVar) -> String {
    let mut value = var;
    let mut chars = Vec::new();
    loop {
        chars.push(b'a' + ((value - 1) % 26) as u8);
        value = (value - 1) / 26;
        if value == 0 { break; }
    }
    chars.reverse();
    String::from_utf8(chars).unwrap_or_else(|_| format!("t{var}"))
}

pub fn row_name(var: RVar) -> String {
    format!("ρ{}", var)
}

pub fn display(ty: &Type) -> String {
    match ty {
        Type::Var(var) => var_name(*var),
        Type::Con(name) => name.clone(),
        Type::App(outer, right) => {
            if let Type::App(arrow, left) = outer.as_ref() {
                if matches!(arrow.as_ref(), Type::Con(name) if name == "->") {
                    return format!("{} -> {}", display(left), display(right));
                }
            }
            format!("{} {}", display(outer), display(right))
        }
        Type::Record(row) | Type::Variant(row) => {
            let open = matches!(row.tail, RowTail::Open(_));
            let body = row.fields.iter().map(|(name, ty)| format!("{name}: {}", display(ty))).collect::<Vec<_>>().join(", ");
            if matches!(ty, Type::Record(_)) {
                format!("{{ {}{} }}", body, if open { ", .." } else { "" })
            } else {
                format!("[ {}{} ]", body, if open { ", .." } else { "" })
            }
        }
    }
}
