use crate::constraints::Command;
use crate::syntax::Span;
use crate::types::*;
use crate::constraints::type_name;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TypeError {
    Mismatch { message: String, left: String, right: String, span: Option<Span>, constraints: Vec<String> },
    Occurs { variable: String, typ: String, span: Option<Span>, constraints: Vec<String> },
    MissingFields { fields: Vec<String>, extra_fields: Vec<String>, span: Option<Span>, constraints: Vec<String> },
    ExtraFields { fields: Vec<String>, missing_fields: Vec<String>, span: Option<Span>, constraints: Vec<String> },
    DuplicateField { field: String, span: Option<Span>, constraints: Vec<String> },
}

impl TypeError {
    pub fn message(&self) -> &str {
        match self {
            TypeError::Mismatch { message, .. } => message,
            TypeError::Occurs { variable, typ, .. } => "",
            TypeError::MissingFields { fields, .. } => "",
            TypeError::ExtraFields { fields, .. } => "",
            TypeError::DuplicateField { field, .. } => "",
        }
    }
    pub fn span(&self) -> Option<Span> {
        match self {
            TypeError::Mismatch { span, .. } | TypeError::Occurs { span, .. } | TypeError::MissingFields { span, .. }
            | TypeError::ExtraFields { span, .. } | TypeError::DuplicateField { span, .. } => *span,
        }
    }
    pub fn constraints(&self) -> &[String] {
        match self {
            TypeError::Mismatch { constraints, .. } | TypeError::Occurs { constraints, .. } | TypeError::MissingFields { constraints, .. }
            | TypeError::ExtraFields { constraints, .. } | TypeError::DuplicateField { constraints, .. } => constraints,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Step {
    pub index: usize,
    pub command: String,
    pub kind: String,
    pub action: String,
    pub detail: String,
    pub substitutions: BTreeMap<String, String>,
    pub status: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct SolveResult {
    pub ok: bool,
    pub steps: Vec<Step>,
    pub error: Option<TypeError>,
    pub final_type: Option<Type>,
    pub schemes: BTreeMap<String, Scheme>,
    pub cores: Vec<Vec<String>>,
    pub disabled: BTreeSet<String>,
}

struct Node {
    parent: FlexId,
    rank: u8,
    kind: FlexKind,
    level: u32,
    binding: Option<Type>,
}

pub struct Solver {
    nodes: BTreeMap<FlexId, Node>,
    steps: Vec<Step>,
    schemes: BTreeMap<String, Scheme>,
}

#[derive(Clone)]
struct Change {
    id: FlexId,
    parent: FlexId,
    rank: u8,
    level: u32,
    binding: Option<Type>,
    existed: bool,
}

pub fn solve(commands: &[Command], disabled: &BTreeSet<String>) -> SolveResult {
    let mut solver = Solver { nodes: BTreeMap::new(), steps: Vec::new(), schemes: BTreeMap::new() };
    let mut last_type = None;
    let mut error = None;

    for (index, command) in commands.iter().enumerate() {
        if disabled.contains(command.id()) {
            solver.steps.push(Step {
                index, command: command.id().into(), kind: command_kind(command), action: "disable".into(),
                detail: "用户临时禁用该约束，求解时跳过".into(), substitutions: BTreeMap::new(), status: "disabled".to_string(),
            });
            continue;
        }
        let snapshot = solver.snapshot();
        let outcome = match command {
            Command::Eq { left, right, .. } => solver.unify(left, right).map(|_| "统一成功".to_string()),
            Command::Instantiate { output, .. } => Ok(format!("实例化为 {}", type_name(output))),
            Command::Generalize { name, typ, expansive, .. } => {
                let resolved = solver.resolve(typ);
                let quantifiers = if *expansive { Vec::new() } else { solver.generalize(&resolved) };
                solver.schemes.insert(name.clone(), Scheme { quantifiers, typ: resolved.clone() });
                last_type = Some(resolved.clone());
                Ok(if *expansive { format!("value restriction：`{name}` 不泛化") } else { format!("泛化 `{name}`") })
            }
        };
        match outcome {
            Ok(detail) => solver.record_success(index, command, detail),
            Err(err) => {
                solver.rollback(snapshot);
                let mut err = err;
                err.push_constraint(command.id().to_string());
                
                solver.steps.push(Step {
                    index, command: command.id().into(), kind: command_kind(command), action: "error".into(),
                    detail: error_brief(&err), substitutions: solver.display_subs(), status: "error".to_string(),
                });
                error = Some(err);
                break;
            }
        }
    }

    let active_eq: Vec<&Command> = commands.iter().filter(|c| matches!(c, Command::Eq {..}) && !disabled.contains(c.id())).collect();
    let cores = if error.is_some() { minimal_cores(&active_eq) } else { Vec::new() };
    SolveResult {
        ok: error.is_none(),
        steps: solver.steps,
        error,
        final_type: last_type,
        schemes: solver.schemes,
        cores,
        disabled: disabled.clone(),
    }
}

impl Solver {
    fn ensure(&mut self, id: FlexId, level: u32) {
        self.nodes.entry(id).or_insert_with(|| Node { parent: id, rank: 0, kind: id.kind, level, binding: None });
    }

    fn snapshot(&self) -> Vec<Change> {
        self.nodes.iter().map(|(id, node)| Change {
            id: *id, parent: node.parent, rank: node.rank, level: node.level, binding: node.binding.clone(), existed: true,
        }).collect()
    }

    fn rollback(&mut self, snapshot: Vec<Change>) {
        let current: BTreeSet<FlexId> = self.nodes.keys().copied().collect();
        let old: BTreeSet<FlexId> = snapshot.iter().map(|c| c.id).collect();
        for removed in current.difference(&old) { self.nodes.remove(removed); }
        for change in snapshot {
            self.nodes.insert(change.id, Node {
                parent: change.parent, rank: change.rank, kind: change.id.kind, level: change.level, binding: change.binding,
            });
        }
    }

    fn find(&mut self, id: FlexId) -> FlexId {
        self.ensure(id, 0);
        let parent = self.nodes[&id].parent;
        if parent == id { return id; }
        let root = self.find(parent);
        self.nodes.get_mut(&id).unwrap().parent = root;
        root
    }

    fn introduce(&mut self, typ: &Type) {
        match &typ.kind {
            TypeKind::Var(id) => { self.ensure(FlexId { kind: FlexKind::Type, id: *id }, 0); }
            TypeKind::Arrow(a, b) => { self.introduce(a); self.introduce(b); }
            TypeKind::Record(row) | TypeKind::Variant(row) => self.introduce_row(row),
            TypeKind::Prim(_) => {}
        }
    }

    fn introduce_row(&mut self, row: &Row) {
        match row {
            Row::Fields { entries, tail } => {
                for (typ, _) in entries.values() { self.introduce(typ); }
                if let Some((id, _)) = tail { self.ensure(FlexId { kind: FlexKind::Row, id: *id }, 0); }
            }
            Row::Flex(id, _) => self.ensure(FlexId { kind: FlexKind::Row, id: *id }, 0),
        }
    }

    fn resolve(&mut self, typ: &Type) -> Type {
        let mut result = typ.clone();
        result.kind = match result.kind {
            TypeKind::Var(id) => {
                let root = self.find(FlexId { kind: FlexKind::Type, id });
                if let Some(bound) = self.nodes[&root].binding.clone() {
                    return self.resolve(&bound);
                }
                TypeKind::Var(root.id)
            }
            TypeKind::Arrow(a, b) => TypeKind::Arrow(Box::new(self.resolve(&a)), Box::new(self.resolve(&b))),
            TypeKind::Record(row) => TypeKind::Record(self.resolve_row(&row)),
            TypeKind::Variant(row) => TypeKind::Variant(self.resolve_row(&row)),
            other => other,
        };
        result
    }

    fn resolve_row(&mut self, row: &Row) -> Row {
        match row {
            Row::Flex(id, prov) => {
                let root = self.find(FlexId { kind: FlexKind::Row, id: *id });
                if let Some(TypeKind::Record(bound)) = self.nodes[&root].binding.clone().map(|t| t.kind) {
                    return self.resolve_row(&bound);
                }
                Row::Flex(root.id, prov.clone())
            }
            Row::Fields { entries, tail } => Row::Fields {
                entries: entries.iter().map(|(name, (typ, prov))| (name.clone(), (self.resolve(typ), prov.clone()))).collect(),
                tail: tail.as_ref().map(|(id, prov)| (self.find(FlexId { kind: FlexKind::Row, id }).id, prov.clone())),
            },
        }
    }

    fn occurs_type(&mut self, var: FlexId, typ: &Type) -> bool {
        match &typ.kind {
            TypeKind::Var(id) => self.find(FlexId { kind: FlexKind::Type, id: *id }) == var,
            TypeKind::Prim(_) => false,
            TypeKind::Arrow(a, b) => self.occurs_type(var, a) || self.occurs_type(var, b),
            TypeKind::Record(row) | TypeKind::Variant(row) => self.occurs_row(var, row),
        }
    }

    fn occurs_row(&mut self, var: FlexId, row: &Row) -> bool {
        match row {
            Row::Flex(id, _) => var.kind == FlexKind::Row && self.find(FlexId { kind: FlexKind::Row, id: *id }) == var,
            Row::Fields { entries, tail } => {
                entries.values().any(|(typ, _)| self.occurs_type(var, typ))
                    || tail.as_ref().map(|(id, _)| var.kind == FlexKind::Row && self.find(FlexId { kind: FlexKind::Row, id }) == var).unwrap_or(false)
            }
        }
    }

    fn bind_var(&mut self, var: FlexId, typ: Type) {
        self.introduce(&typ);
        let root = self.find(var);
        self.nodes.get_mut(&root).unwrap().binding = Some(typ);
    }

    fn union_vars(&mut self, left: FlexId, right: FlexId) {
        self.ensure(left, 0); self.ensure(right, 0);
        let l = self.find(left);
        let r = self.find(right);
        if l == r { return; }
        let (from, to) = if self.nodes[&l].rank < self.nodes[&r].rank { (l, r) } else { (r, l) };
        if self.nodes[&from].rank == self.nodes[&to].rank { self.nodes.get_mut(&to).unwrap().rank += 1; }
        let binding = self.nodes[&from].binding.clone().or_else(|| self.nodes[&to].binding.clone());
        self.nodes.get_mut(&from).unwrap().parent = to;
        if binding.is_some() { self.nodes.get_mut(&to).unwrap().binding = binding; }
    }

    fn unify(&mut self, left: &Type, right: &Type) -> Result<(), TypeError> {
        self.introduce(left); self.introduce(right);
        let left = self.resolve(left);
        let right = self.resolve(right);
        match (&left.kind, &right.kind) {
            (TypeKind::Var(a), TypeKind::Var(b)) => {
                self.union_vars(FlexId { kind: FlexKind::Type, id: *a }, FlexId { kind: FlexKind::Type, id: *b });
                Ok(())
            }
            (TypeKind::Var(id), typ) | (typ, TypeKind::Var(id)) => {
                let var = FlexId { kind: FlexKind::Type, id: *id };
                let other = Type::new(typ.clone(), merge_prov(&left.prov, &right.prov));
                if self.occurs_type(var, &other) {
                    return Err(TypeError::Occurs {
                        variable: type_name(&left), typ: type_name(&right),
                        span: choose_span(&left, &right), constraints: constraint_ids(&left, &right),
                    });
                }
                self.bind_var(var, other);
                Ok(())
            }
            (TypeKind::Prim(a), TypeKind::Prim(b)) if a == b => Ok(()),
            (TypeKind::Arrow(a1, b1), TypeKind::Arrow(a2, b2)) => {
                self.unify(a1, a2)?;
                self.unify(b1, b2)
            }
            (TypeKind::Record(r1), TypeKind::Record(r2)) => self.unify_rows(false, r1, r2, &left.prov, &right.prov),
            (TypeKind::Variant(r1), TypeKind::Variant(r2)) => self.unify_rows(true, r1, r2, &left.prov, &right.prov),
            _ => Err(TypeError::Mismatch {
                message: format!("无法把 `{}` 与 `{}` 统一：类型构造器不同", type_name(&left), type_name(&right)),
                left: type_name(&left), right: type_name(&right),
                span: choose_span(&left, &right), constraints: constraint_ids(&left, &right),
            }),
        }
    }

    fn unify_rows(&mut self, variant: bool, left: &Row, right: &Row, pl: &Provenance, pr: &Provenance) -> Result<(), TypeError> {
        let left = self.resolve_row(left);
        let right = self.resolve_row(right);
        match (left.clone(), right.clone()) {
            (Row::Flex(id, _), other) | (other, Row::Flex(id, _)) => {
                let var = FlexId { kind: FlexKind::Row, id };
                let wrapped = if variant { Type::variant(other.clone(), merge_prov(pl, pr)) } else { Type::record(other.clone(), merge_prov(pl, pr)) };
                if self.occurs_row(var, &other) {
                    return Err(TypeError::Occurs {
                        variable: format!("?r{}", id.0), typ: crate::constraints::type_name(&wrapped),
                        span: pl.spans.first().copied().or_else(|| pr.spans.first().copied()),
                        constraints: pl.constraints.iter().chain(pr.constraints.iter()).cloned().collect(),
                    });
                }
                self.bind_var(var, wrapped);
                Ok(())
            }
            (Row::Fields { entries: e1, tail: t1 }, Row::Fields { entries: e2, tail: t2 }) => {
                let keys1: BTreeSet<String> = e1.keys().cloned().collect();
                let keys2: BTreeSet<String> = e2.keys().cloned().collect();
                let common: Vec<String> = keys1.intersection(&keys2).cloned().collect();
                for key in &common {
                    self.unify(&e1[key].0, &e2[key].0)?;
                }
                if let (Some((a, _)), Some((b, _))) = (&t1, &t2) {
                    self.union_vars(FlexId { kind: FlexKind::Row, id: a }, FlexId { kind: FlexKind::Row, id: b });
                }
                let only_left: Vec<String> = keys1.difference(&keys2).cloned().collect();
                let only_right: Vec<String> = keys2.difference(&keys1).cloned().collect();
                let closed_left = t1.is_none();
                let closed_right = t2.is_none();
                let noun = if variant { "标签" } else { "字段" };
                if closed_right && !only_left.is_empty() {
                    return Err(TypeError::ExtraFields {
                        fields: only_left, missing_fields: only_right,
                        span: pl.spans.first().copied().or_else(|| pr.spans.first().copied()),
                        constraints: row_constraints(&e1, &e2),
                    }.with_message(&format!("多余{}", noun)));
                }
                if closed_left && !only_right.is_empty() {
                    return Err(TypeError::MissingFields {
                        fields: only_right, extra_fields: only_left,
                        span: pl.spans.first().copied().or_else(|| pr.spans.first().copied()),
                        constraints: row_constraints(&e1, &e2),
                    }.with_message(&format!("缺少{}", noun)));
                }
                Ok(())
            }
        }
    }

    fn generalize(&self, typ: &Type) -> Vec<FlexId> {
        let mut found = BTreeSet::new();
        collect(typ, &mut found);
        found.into_iter().collect()
    }

    fn record_success(&mut self, index: usize, command: &Command, detail: String) {
        self.steps.push(Step {
            index, command: command.id().into(), kind: command_kind(command), action: "apply".into(),
            detail, substitutions: self.display_subs(), status: "ok".to_string(),
        });
    }

    fn display_subs(&self) -> BTreeMap<String, String> {
        let mut map = BTreeMap::new();
        for (id, node) in &self.nodes {
            if let Some(bound) = &node.binding {
                let name = match id.kind { FlexKind::Type => format!("?t{}", id.id.0), FlexKind::Row => format!("?r{}", id.id.0) };
                map.insert(name, crate::constraints::type_name(bound));
            }
        }
        map
    }
}

fn collect(typ: &Type, found: &mut BTreeSet<FlexId>) {
    match &typ.kind {
        TypeKind::Var(id) => { found.insert(FlexId { kind: FlexKind::Type, id: *id }); }
        TypeKind::Prim(_) => {}
        TypeKind::Arrow(a, b) => { collect(a, found); collect(b, found); }
        TypeKind::Record(row) | TypeKind::Variant(row) => collect_row(row, found),
    }
}

fn collect_row(row: &Row, found: &mut BTreeSet<FlexId>) {
    match row {
        Row::Fields { entries, tail } => {
            for (typ, _) in entries.values() { collect(typ, found); }
            if let Some((id, _)) = tail { found.insert(FlexId { kind: FlexKind::Row, id: *id }); }
        }
        Row::Flex(id, _) => { found.insert(FlexId { kind: FlexKind::Row, id: *id }); }
    }
}

fn merge_prov(a: &Provenance, b: &Provenance) -> Provenance {
    let mut spans = a.spans.clone();
    spans.extend(b.spans.iter().copied());
    spans.dedup();
    let mut constraints = a.constraints.clone();
    constraints.extend(b.constraints.iter().cloned());
    constraints.dedup();
    Provenance { expression: a.expression.or(b.expression), spans, constraints, reason: a.reason.clone() }
}

fn choose_span(left: &Type, right: &Type) -> Option<Span> {
    right.prov.spans.first().copied().or_else(|| left.prov.spans.first().copied())
}

fn constraint_ids(left: &Type, right: &Type) -> Vec<String> {
    let mut ids = left.prov.constraints.clone();
    ids.extend(right.prov.constraints.iter().cloned());
    ids.dedup();
    ids
}

fn row_constraints(e1: &BTreeMap<String, (Type, Provenance)>, e2: &BTreeMap<String, (Type, Provenance)>) -> Vec<String> {
    let mut ids = Vec::new();
    for (_, prov) in e1.values() { ids.extend(prov.1.constraints.iter().cloned()); }
    for (_, prov) in e2.values() { ids.extend(prov.1.constraints.iter().cloned()); }
    ids.dedup();
    ids
}

fn command_kind(command: &Command) -> String {
    match command {
        Command::Eq {..} => "equality",
        Command::Instantiate {..} => "instantiation",
        Command::Generalize {..} => "generalization",
    }.into()
}

fn error_brief(error: &TypeError) -> String {
    match error {
        TypeError::Mismatch { message, .. } => message.clone(),
        TypeError::Occurs { variable, typ, .. } => format!("occurs check：{variable} 已出现在 {typ} 中"),
        TypeError::MissingFields { fields, .. } => format!("缺少：{}", fields.join(", ")),
        TypeError::ExtraFields { fields, .. } => format!("多余：{}", fields.join(", ")),
        TypeError::DuplicateField { field, .. } => format!("重复字段 `{field}`"),
    }
}

impl TypeError {
    fn with_message(self, prefix: &str) -> Self {
        let fields_text = match &self {
            TypeError::ExtraFields { fields, .. } | TypeError::MissingFields { fields, .. } => fields.join("、"),
            _ => String::new(),
        };
        let message = format!("{prefix}：`{fields_text}`；缺字段与多字段已分别列出");
        match self {
            TypeError::Mismatch { left, right, span, constraints, .. } => TypeError::Mismatch { message, left, right, span, constraints },
            TypeError::Occurs { variable, typ, span, constraints } => TypeError::Mismatch { message, left: variable, right: typ, span, constraints },
            TypeError::MissingFields { fields, extra_fields, span, constraints } => TypeError::MissingFields { fields, extra_fields, span, constraints },
            TypeError::ExtraFields { fields, missing_fields, span, constraints } => TypeError::ExtraFields { fields, missing_fields, span, constraints },
            TypeError::DuplicateField { field, span, constraints } => TypeError::DuplicateField { field, span, constraints },
        }
    }
}

fn run_eq_subset(commands: &[&Command]) -> bool {
    let mut solver = Solver { nodes: BTreeMap::new(), steps: Vec::new(), schemes: BTreeMap::new() };
    for command in commands {
        if let Command::Eq { left, right, .. } = command {
            if solver.unify(left, right).is_err() { return false; }
        }
    }
    true
}

fn deletion_mus(mut commands: Vec<&Command>) -> Vec<String> {
    let mut index = 0;
    while index < commands.len() {
        let candidate: Vec<&Command> = commands.iter().enumerate().filter_map(|(i, command)| (i != index).then_some(*command)).collect();
        if !run_eq_subset(&candidate) {
            commands.remove(index);
        } else {
            index += 1;
        }
    }
    commands.iter().map(|command| command.id().to_string()).collect()
}

fn enumerate_size(commands: &[&Command], size: usize, start: usize, chosen: &mut Vec<usize>, cores: &mut Vec<Vec<String>>) {
    if chosen.len() == size {
        let subset: Vec<&Command> = chosen.iter().map(|index| commands[*index]).collect();
        if !run_eq_subset(&subset) {
            let mut members: Vec<String> = subset.iter().map(|command| command.id().to_string()).collect();
            members.sort();
            if !cores.contains(&members) { cores.push(members); }
        }
        return;
    }
    for index in start..commands.len() {
        chosen.push(index);
        enumerate_size(commands, size, index + 1, chosen, cores);
        chosen.pop();
    }
}

fn minimal_cores(commands: &[&Command]) -> Vec<Vec<String>> {
    if commands.len() <= 14 {
        for size in 1..=commands.len() {
            let mut cores = Vec::new();
            enumerate_size(commands, size, 0, &mut Vec::new(), &mut cores);
            if !cores.is_empty() { return cores; }
        }
    }
    vec![deletion_mus(commands.to_vec())]
}
