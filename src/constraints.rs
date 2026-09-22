use crate::astmap::{flatten, AstMap};
use crate::parser::parse;
use crate::syntax::*;
use crate::types::*;
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Command {
    Eq { id: String, left: Type, right: Type, scope: String, gen_env: String },
    Instantiate { id: String, name: String, reference: String, output: Type, scope: String, gen_env: String },
    Generalize { id: String, binder: String, name: String, typ: Type, expansive: bool, scope: String, gen_env: String },
}

impl Command {
    pub fn id(&self) -> &str {
        match self {
            Command::Eq { id, .. } | Command::Instantiate { id, .. } | Command::Generalize { id, .. } => id,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ConstraintView {
    pub id: String,
    pub kind: String,
    pub label: String,
    pub node: Option<String>,
    pub span: Option<Span>,
    pub scope: String,
    pub gen_env: String,
    pub status: String,
}

#[derive(Clone)]
enum Slot {
    Scheme(Scheme),
    Mono(Type),
}

#[derive(Clone)]
struct GenEntry {
    reference: String,
    name: String,
}

pub struct AnalysisInput<'a> {
    pub source: &'a str,
    pub disabled: BTreeSet<String>,
    pub previous: Option<PreviousAnalysis>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct PreviousAnalysis {
    pub command_ids: BTreeSet<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Analysis {
    pub source: String,
    pub ast: AstMap,
    pub commands: Vec<Command>,
    pub views: Vec<ConstraintView>,
    pub reused: BTreeSet<String>,
    pub invalidated: BTreeSet<String>,
    pub resolved: BTreeSet<String>,
}

struct Generator<'a> {
    source_len: usize,
    program: &'a Program,
    ast: AstMap,
    env: Vec<BTreeMap<String, (Slot, GenEntry)>>,
    commands: Vec<Command>,
    command_keys: BTreeSet<String>,
    type_counter: u32,
    row_counter: u32,
    scope_parts: Vec<String>,
    gen_stack: Vec<String>,
}

pub fn analyze(source: &str) -> Result<Analysis, AnalysisError> {
    analyze_with(AnalysisInput { source, disabled: BTreeSet::new(), previous: None })
}

pub fn analyze_with(input: AnalysisInput) -> Result<Analysis, AnalysisError> {
    let program = parse(input.source).map_err(AnalysisError::Parse)?;
    let ast = flatten(&program, input.source.len()).map_err(AnalysisError::Name)?;
    let mut gen = Generator {
        source_len: input.source.len(),
        program: &program,
        ast,
        env: vec![BTreeMap::new()],
        commands: Vec::new(),
        command_keys: BTreeSet::new(),
        type_counter: 0,
        row_counter: 0,
        scope_parts: vec!["root".into()],
        gen_stack: Vec::new(),
    };
    gen.install_builtins();
    let mut final_types = BTreeMap::new();
    for (index, binding) in gen.program.bindings.clone().iter().enumerate() {
        gen.scope_parts.push(format!("binding[{index}]"));
        let typ = gen.gen_binding(binding)?;
        let expansive = !non_expansive(&binding.value);
        gen.generalize_binding(binding, typ, expansive);
        final_types.insert(binding.name.clone(), (binding.span, expansive));
        gen.scope_parts.pop();
    }
    let current: BTreeSet<String> = gen.commands.iter().map(|c| c.id().to_string()).collect();
    let previous = input.previous.unwrap_or_default().command_ids;
    let reused = previous.intersection(&current).cloned().collect();
    let invalidated = previous.difference(&current).cloned().collect();
    let resolved = current.difference(&previous).cloned().collect();
    let views = gen.views(input.disabled);
    Ok(Analysis {
        source: input.source.to_string(),
        ast: gen.ast,
        commands: gen.commands,
        views,
        reused, invalidated, resolved,
    })
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", content = "detail", rename_all = "snake_case")]
pub enum AnalysisError {
    Parse(crate::parser::ParseError),
    Name(crate::astmap::NameError),
}

impl<'a> Generator<'a> {
    fn install_builtins(&mut self) {
        let int = Type::prim("int", Provenance::builtin("内建整数类型"));
        let bool = Type::prim("bool", Provenance::builtin("内建布尔类型"));
        let mono = |name: &str, typ: Type| {
            (Slot::Scheme(Scheme { quantifiers: Vec::new(), typ }), GenEntry { reference: format!("builtin:{name}"), name: name.into() })
        };
        let mut add = |name: &str, args: Vec<Type>, result: Type| {
            let mut typ = result;
            for arg in args.into_iter().rev() {
                typ = Type::arrow(arg, typ, Provenance::builtin("内建函数类型"));
            }
            self.env[0].insert(name.to_string(), mono(name, typ));
        };
        add("__add", vec![int.clone(), int.clone()], int.clone());
        add("__sub", vec![int.clone(), int.clone()], int.clone());
        add("__mul", vec![int.clone(), int.clone()], int.clone());
        add("__div", vec![int.clone(), int.clone()], int.clone());
        add("__eq", vec![int.clone(), int.clone()], bool.clone());
        add("__ne", vec![int.clone(), int.clone()], bool.clone());
        add("__lt", vec![int.clone(), int.clone()], bool.clone());
        add("__gt", vec![int.clone(), int.clone()], bool.clone());
        add("__le", vec![int.clone(), int.clone()], bool.clone());
        add("__ge", vec![int.clone(), int.clone()], bool.clone());
        self.env[0].insert("not".into(), mono("not", Type::arrow(bool.clone(), bool, Provenance::builtin("内建函数类型"))));
    }

    fn short_hash(parts: &[String]) -> String {
        let mut hash = 0xcbf29ce484222325u64;
        for part in parts {
            for byte in part.bytes() {
                hash ^= byte as u64;
                hash = hash.wrapping_mul(0x100000001b3);
            }
            hash ^= 0xff;
        }
        format!("{:08x}", hash & 0xffff_ffff)
    }

    fn scope_hash(&self) -> String {
        let mut visible: Vec<String> = self.env.iter().flat_map(|scope| {
            scope.iter().map(|(name, (_, entry))| format!("{}@{}", name, entry.reference)).collect::<Vec<_>>()
        }).collect();
        visible.sort();
        visible.dedup();
        Self::short_hash(&[self.scope_parts.join("/"), visible.join(",")])
    }

    fn gen_env_hash(&self) -> String {
        Self::short_hash(&[self.gen_stack.join(">")])
    }

    fn fresh(&mut self, kind: FlexKind, node: &str, role: &str, span: Span, reason: &str) -> Type {
        let id = VarId(match kind {
            FlexKind::Type => { let n = self.type_counter; self.type_counter += 1; n }
            FlexKind::Row => { let n = self.row_counter; self.row_counter += 1; n }
        });
        let stable = format!("flex:{}:{kind:?}:{node}:{role}:{}", id.0, Self::short_hash(&[self.scope_hash(), self.gen_env_hash()]));
        let prov = Provenance::expr(node_num(node), span, reason.to_string());
        match kind {
            FlexKind::Type => Type::var(id, prov),
            FlexKind::Row => Type::record(Row::Flex(id, prov), prov),
        }
    }

    fn fresh_row(&mut self, node: &str, role: &str, span: Span, reason: &str) -> Row {
        if let TypeKind::Record(Row::Flex(id, prov)) = self.fresh(FlexKind::Row, node, role, span, reason).kind {
            Row::Flex(id, prov)
        } else { unreachable!() }
    }

    fn add_command(&mut self, mut command: Command) {
        let id = match &command {
            Command::Eq { id, .. } | Command::Instantiate { id, .. } | Command::Generalize { id, .. } => id.clone(),
        };
        if self.command_keys.insert(id.clone()) { self.commands.push(command); }
    }

    fn eq_command(&mut self, node: &str, role: &str, left: Type, right: Type, reason: &str) {
        let scope = self.scope_hash();
        let gen_env = self.gen_env_hash();
        let id = format!("c:{}", Self::short_hash(&[
            "eq".into(), node.into(), role.into(), type_key(&left), type_key(&right), scope.clone(), gen_env.clone()
        ]));
        let mut l = left; let mut r = right;
        l.prov.constraints.push(id.clone()); r.prov.constraints.push(id.clone());
        self.add_command(Command::Eq { id, left: l, right: r, scope, gen_env });
    }

    fn enter(&mut self) { self.env.push(BTreeMap::new()); }
    fn leave(&mut self) { self.env.pop(); }

    fn declare_mono(&mut self, name: &str, reference: String, typ: Type) {
        self.env.last_mut().unwrap().insert(name.to_string(), (Slot::Mono(typ), GenEntry { reference, name: name.into() }));
    }

    fn declare_scheme(&mut self, name: &str, reference: String, scheme: Scheme) {
        self.env.last_mut().unwrap().insert(name.to_string(), (Slot::Scheme(scheme), GenEntry { reference, name: name.into() }));
    }

    fn lookup(&self, name: &str) -> Option<&(Slot, GenEntry)> {
        for scope in self.env.iter().rev() {
            if let Some(entry) = scope.get(name) { return Some(entry); }
        }
        None
    }

    fn gen_binding(&mut self, binding: &Binding) -> Result<Type, AnalysisError> {
        if binding.rec {
            let node = binder_node(&self.ast, binding.name_span).unwrap_or_else(|| format!("binding:{}", binding.name));
            let span = binding.name_span;
            let typ = self.fresh(FlexKind::Type, &node, "recursive_binding", span, "递归绑定的单态类型");
            self.enter();
            self.declare_mono(&binding.name, node.clone(), typ.clone());
            let actual = self.gen_expr(&binding.value)?;
            self.leave();
            self.eq_command(&node, "recursive_binding", typ, actual, "递归绑定与其定义相等");
            let result = self.fresh(FlexKind::Type, &node, "recursive_result", span, "递归绑定结果");
            self.eq_command(&node, "recursive_result", self.fresh_lookup(&binding.name, span, &node)?, result.clone(), "暴露递归绑定类型");
            Ok(result)
        } else {
            self.gen_expr(&binding.value)
        }
    }

    fn generalize_binding(&mut self, binding: &Binding, typ: Type, expansive: bool) {
        let node = binder_node(&self.ast, binding.name_span).unwrap_or_else(|| format!("binding:{}", binding.name));
        let scope = self.scope_hash();
        let gen_env = self.gen_env_hash();
        let id = format!("g:{}", Self::short_hash(&["generalize".into(), node.clone(), binding.name.clone(), type_key(&typ), expansive.to_string(), scope.clone(), gen_env.clone()]));
        let quantified = if expansive { Vec::new() } else { free_flex(&typ, &BTreeSet::new()) };
        typ.prov.clone().constraints.iter().for_each(|_| {});
        let scheme = Scheme { quantifiers: quantified, typ: typ.clone() };
        if let Some(existing) = self.env.last_mut().unwrap().get_mut(&binding.name) {
            existing.0 = Slot::Scheme(scheme.clone());
        } else {
            self.declare_scheme(&binding.name, node.clone(), scheme);
        }
        self.add_command(Command::Generalize {
            id, binder: node, name: binding.name.clone(), typ, expansive, scope, gen_env,
        });
    }

    fn fresh_lookup(&mut self, name: &str, span: Span, node: &str) -> Result<Type, AnalysisError> {
        let (slot, entry) = self.lookup(name).ok_or_else(|| AnalysisError::Name(crate::astmap::NameError {
            message: format!("未绑定的名字 `{name}`"), span,
        }))?.clone();
        match slot {
            Slot::Mono(typ) => Ok(typ),
            Slot::Scheme(scheme) => {
                let substitutions = instantiate_scheme(&scheme);
                let output = substitute_type(&scheme.typ, &substitutions);
                let scope = self.scope_hash();
                let gen_env = self.gen_env_hash();
                let id = format!("i:{}", Self::short_hash(&[
                    "instantiate".into(), node.into(), entry.reference.clone(), type_key(&output), scope.clone(), gen_env.clone()
                ]));
                self.add_command(Command::Instantiate {
                    id, name: name.to_string(), reference: entry.reference, output: output.clone(), scope, gen_env,
                });
                Ok(output)
            }
        }
    }

    fn gen_expr(&mut self, expr: &Expr) -> Result<Type, AnalysisError> {
        Ok(match expr {
            Expr::Lit { value, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "literal".into());
                let name = match value { Lit::Int(_) => "int", Lit::Bool(_) => "bool", Lit::String(_) => "string", Lit::Unit => "unit" };
                Type::prim(name, Provenance::expr(node_num(&node), *span, "字面量引入的基础类型"))
            }
            Expr::Var { name, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| name.clone());
                self.fresh_lookup(name, *span, &node)?
            }
            Expr::Fun { param, param_span, body, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "function".into());
                let param_node = nearest_at(&self.ast, *param_span).unwrap_or_else(|| node.clone());
                let param_type = self.fresh(FlexKind::Type, &param_node, "parameter", *param_span, "函数参数类型");
                self.enter();
                self.declare_mono(param, param_node.clone(), param_type.clone());
                let body_type = self.gen_expr(body)?;
                self.leave();
                Type::arrow(param_type, body_type, Provenance::expr(node_num(&node), *span, "函数构造器引入的箭头类型"))
            }
            Expr::App { fun, arg, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "application".into());
                let fun_type = self.gen_expr(fun)?;
                let arg_type = self.gen_expr(arg)?;
                let result = self.fresh(FlexKind::Type, &node, "result", *span, "函数应用结果类型");
                let expected = Type::arrow(arg_type, result.clone(), Provenance::expr(node_num(&node), *span, "应用点期待的函数类型"));
                self.eq_command(&node, "application", fun_type, expected, "被应用表达式必须是函数");
                result
            }
            Expr::Let { rec, name, name_span, value, body, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "let".into());
                if *rec {
                    let mono = self.fresh(FlexKind::Type, &node, "recursive_let", *name_span, "递归 let 的单态类型");
                    self.enter();
                    self.declare_mono(name, node.clone(), mono.clone());
                    let actual = self.gen_expr(value)?;
                    self.leave();
                    self.eq_command(&node, "recursive_let", mono, actual, "递归 let 与其定义相等");
                    self.enter();
                    self.declare_mono(name, node.clone(), self.fresh_lookup(name, *span, &node)?);
                    let result = self.gen_expr(body)?;
                    self.leave();
                    result
                } else {
                    let value_type = self.gen_expr(value)?;
                    let expansive = !non_expansive(value);
                    self.gen_stack.push(node.clone());
                    let scope = self.scope_hash();
                    let gen_env = self.gen_env_hash();
                    let id = format!("g:{}", Self::short_hash(&["generalize".into(), node.clone(), name.clone(), type_key(&value_type), expansive.to_string(), scope.clone(), gen_env.clone()]));
                    let quantifiers = if expansive { Vec::new() } else { free_flex(&value_type, &BTreeSet::new()) };
                    self.enter();
                    self.declare_scheme(name, node.clone(), Scheme { quantifiers, typ: value_type.clone() });
                    let result = self.gen_expr(body)?;
                    self.leave();
                    self.gen_stack.pop();
                    self.add_command(Command::Generalize { id, binder: node, name: name.clone(), typ: value_type, expansive, scope, gen_env });
                    result
                }
            }
            Expr::Record { fields, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "record".into());
                let mut map = BTreeMap::new();
                for field in fields {
                    let typ = self.gen_expr(&field.value)?;
                    map.insert(field.name.clone(), (typ, Provenance::expr(node_num(&node), field.name_span, "记录字段")));
                }
                Type::record(Row::Fields { entries: map, tail: None }, Provenance::expr(node_num(&node), *span, "记录字面量引入的行"))
            }
            Expr::Field { record, name, name_span, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "field".into());
                let record_type = self.gen_expr(record)?;
                let result = self.fresh(FlexKind::Type, &node, "field_result", *span, "字段访问结果");
                let tail = self.fresh_row(&node, &format!("tail_after_{name}"), *span, "字段访问后的剩余行");
                let tail_var = if let Row::Flex(id, _) = tail { id } else { unreachable!() };
                let mut entries = BTreeMap::new();
                entries.insert(name.clone(), (result.clone(), Provenance::expr(node_num(&node), *name_span, "被访问字段")));
                let expected = Type::record(
                    Row::Fields { entries, tail: Some((tail_var, Provenance::expr(node_num(&node), *span, "其余字段"))) },
                    Provenance::expr(node_num(&node), *span, "字段访问要求的记录类型"),
                );
                self.eq_command(&node, &format!("field_{name}"), record_type, expected, "记录必须包含被访问字段");
                result
            }
            Expr::Extend { base, fields, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "extend".into());
                let base_type = self.gen_expr(base)?;
                let mut map = BTreeMap::new();
                for field in fields {
                    map.insert(field.name.clone(), (self.gen_expr(&field.value)?, Provenance::expr(node_num(&node), field.name_span, "扩展字段")));
                }
                let base_tail = self.fresh_row(&node, "extension_base_tail", *span, "被扩展记录的原有行");
                let base_tail_var = if let Row::Flex(id, _) = base_tail { id } else { unreachable!() };
                let expected_base = Type::record(
                    Row::Fields { entries: map.clone(), tail: Some((base_tail_var, Provenance::expr(node_num(&node), *span, "被扩展记录与新字段一致"))) },
                    Provenance::expr(node_num(&node), *span, "记录扩展基类型"),
                );
                self.eq_command(&node, "record_extension_base", base_type, expected_base, "扩展表达式的基值必须是记录");
                Type::record(
                    Row::Fields { entries: map, tail: Some((base_tail_var, Provenance::expr(node_num(&node), *span, "扩展后的剩余字段"))) },
                    Provenance::expr(node_num(&node), *span, "记录扩展类型"),
                )
            }
            Expr::Variant { label, payload, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "variant".into());
                let payload_type = if let Some(payload) = payload { Some(Box::new(self.gen_expr(payload)?)) } else { None };
                let mut map = BTreeMap::new();
                let payload_type = payload_type.map(|t| *t).unwrap_or_else(|| Type::prim("unit", Provenance::expr(node_num(&node), *span, "无载荷变体使用 unit")));
                map.insert(label.clone(), (payload_type, Provenance::expr(node_num(&node), *span, "变体构造标签")));
                Type::variant(Row::Fields { entries: map, tail: None }, Provenance::expr(node_num(&node), *span, "变体构造引入的开放行"))
            }
            Expr::Match { scrutinee, arms, span } => {
                let node = nearest_before(&self.ast, span.start).unwrap_or_else(|| "match".into());
                let scrutinee_type = self.gen_expr(scrutinee)?;
                let result = self.fresh(FlexKind::Type, &node, "match_result", *span, "match 分支结果");
                for (index, arm) in arms.iter().enumerate() {
                    let (tag, payload_binding, payload_span) = pattern_info(&arm.pattern);
                    let payload_type = self.fresh(FlexKind::Type, &node, &format!("arm_{index}_payload"), payload_span, "变体分支载荷类型");
                    let mut map = BTreeMap::new();
                    map.insert(tag.clone(), (payload_type.clone(), Provenance::expr(node_num(&node), payload_span, "match 分支标签")));
                    let expected_variant = Type::variant(Row::Fields { entries: map, tail: None }, Provenance::expr(node_num(&node), arm.span, "分支期待的变体类型"));
                    self.enter();
                    if let Some((name, span)) = payload_binding {
                        self.declare_mono(&name, format!("arm:{index}"), payload_type);
                    }
                    let body_type = self.gen_expr(&arm.body)?;
                    self.leave();
                    self.eq_command(&node, &format!("arm_{index}_variant"), scrutinee_type.clone(), expected_variant, "分支标签必须属于被匹配变体");
                    self.eq_command(&node, &format!("arm_{index}_result"), body_type, result.clone(), "所有分支结果类型一致");
                }
                result
            }
        })
    }

    fn views(&self, disabled: BTreeSet<String>) -> Vec<ConstraintView> {
        self.commands.iter().map(|command| {
            let id = command.id().to_string();
            let (kind, label, node, span, scope, gen_env) = match command {
                Command::Eq { left, right, scope, gen_env, .. } => {
                    let node = right.prov.expression.map(node_id_string).or_else(|| left.prov.expression.map(node_id_string));
                    let span = right.prov.spans.first().or_else(|| left.prov.spans.first()).copied();
                    ("equality", format!("{} ≡ {}", type_name(left), type_name(right)), node, span, scope.clone(), gen_env.clone())
                }
                Command::Instantiate { name, output, scope, gen_env, .. } => {
                    ("instantiation", format!("实例化 {name} : {}", type_name(output)), None, output.prov.spans.first().copied(), scope.clone(), gen_env.clone())
                }
                Command::Generalize { name, typ, expansive, scope, gen_env, .. } => {
                    ("generalization", format!("{} `{name}` : {}", if *expansive { "受限泛化" } else { "泛化" }, type_name(typ)), Some(name.clone()), typ.prov.spans.first().copied(), scope.clone(), gen_env.clone())
                }
            };
            let status = if disabled.contains(&id) { "disabled".into() } else { "active".into() };
            ConstraintView { id, kind: kind.into(), label, node, span, scope, gen_env, status }
        }).collect()
    }
}

fn node_num(stable: &str) -> usize {
    stable.strip_prefix("n_").and_then(|v| u64::from_str_radix(v, 16).ok()).unwrap_or(0) as usize
}

fn node_id_string(value: usize) -> String { format!("n_{:016x}", value as u64) }

fn nearest_before(ast: &AstMap, offset: usize) -> Option<String> {
    ast.nodes.iter().filter(|node| node.span.start <= offset).max_by_key(|node| (node.span.start, node.span.end)).map(|node| node.stable_id.clone())
}

fn nearest_at(ast: &AstMap, span: Span) -> Option<String> {
    ast.nodes.iter().find(|node| node.span == span).map(|node| node.stable_id.clone())
}

fn binder_node(ast: &AstMap, span: Span) -> Option<String> {
    ast.nodes.iter().filter(|node| node.binder.is_some() && node.span == span).map(|node| node.binder.clone().unwrap_or_else(|| node.stable_id.clone())).next()
}

pub fn non_expansive(expr: &Expr) -> bool {
    match expr {
        Expr::Lit { .. } | Expr::Var { .. } | Expr::Fun { .. } => true,
        Expr::Record { fields, .. } => fields.iter().all(|field| non_expansive(&field.value)),
        Expr::Extend { base, fields, .. } => non_expansive(base) && fields.iter().all(|field| non_expansive(&field.value)),
        Expr::Variant { payload, .. } => payload.as_ref().map(|p| non_expansive(p.as_ref())).unwrap_or(true),
        Expr::Field { record, .. } => non_expansive(record),
        Expr::App { .. } | Expr::Match { .. } => false,
        Expr::Let { value, body, .. } => non_expansive(value) && non_expansive(body),
    }
}

fn pattern_info(pattern: &Pattern) -> (String, Option<(String, Span)>, Span) {
    match pattern {
        Pattern::Wildcard(span) => ("_".into(), None, *span),
        Pattern::Var { name, span } => ("__any__".into(), Some((name.clone(), *span)), *span),
        Pattern::Variant { label, payload, span } => match payload.as_deref() {
            Some(Pattern::Var { name, span: payload_span }) => (label.clone(), Some((name.clone(), *payload_span)), *span),
            Some(Pattern::Wildcard(payload_span)) => (label.clone(), None, *payload_span),
            _ => (label.clone(), None, *span),
        },
    }
}

pub fn free_flex(typ: &Type, bound: &BTreeSet<FlexId>) -> Vec<FlexId> {
    let mut found = BTreeSet::new();
    collect_free(typ, bound, &mut found);
    found.into_iter().collect()
}

fn collect_free(typ: &Type, bound: &BTreeSet<FlexId>, found: &mut BTreeSet<FlexId>) {
    match &typ.kind {
        TypeKind::Var(id) => { let f = FlexId { kind: FlexKind::Type, id: *id }; if !bound.contains(&f) { found.insert(f); } }
        TypeKind::Prim(_) => {}
        TypeKind::Arrow(a, b) => { collect_free(a, bound, found); collect_free(b, bound, found); }
        TypeKind::Record(row) | TypeKind::Variant(row) => collect_row(row, bound, found),
    }
}

fn collect_row(row: &Row, bound: &BTreeSet<FlexId>, found: &mut BTreeSet<FlexId>) {
    match row {
        Row::Fields { entries, tail } => {
            for (typ, _) in entries.values() { collect_free(typ, bound, found); }
            if let Some((id, _)) = tail { let f = FlexId { kind: FlexKind::Row, id: *id }; if !bound.contains(&f) { found.insert(f); } }
        }
        Row::Flex(id, _) => { let f = FlexId { kind: FlexKind::Row, id: *id }; if !bound.contains(&f) { found.insert(f); } }
    }
}

fn instantiate_scheme(scheme: &Scheme) -> Vec<(FlexId, FlexId)> {
    scheme.quantifiers.iter().enumerate().map(|(index, old)| (*old, FlexId { kind: old.kind, id: VarId(100_000 + index as u32) })).collect()
}

fn substitute_type(typ: &Type, substitutions: &[(FlexId, FlexId)]) -> Type {
    substitute(typ, &substitutions.iter().copied().collect())
}

fn substitute(typ: &Type, map: &BTreeMap<FlexId, FlexId>) -> Type {
    let mut result = typ.clone();
    result.kind = match result.kind {
        TypeKind::Var(id) => match map.get(&FlexId { kind: FlexKind::Type, id }) {
            Some(FlexId { id: new, .. }) => TypeKind::Var(*new),
            None => TypeKind::Var(id),
        },
        TypeKind::Arrow(a, b) => TypeKind::Arrow(Box::new(substitute(&a, map)), Box::new(substitute(&b, map))),
        TypeKind::Record(row) => TypeKind::Record(substitute_row(&row, map)),
        TypeKind::Variant(row) => TypeKind::Variant(substitute_row(&row, map)),
        other => other,
    };
    result
}

fn substitute_row(row: &Row, map: &BTreeMap<FlexId, FlexId>) -> Row {
    match row {
        Row::Fields { entries, tail } => Row::Fields {
            entries: entries.iter().map(|(name, (typ, prov))| (name.clone(), (substitute(typ, map), prov.clone()))).collect(),
            tail: tail.as_ref().map(|(id, prov)| (map.get(&FlexId { kind: FlexKind::Row, id }).copied().unwrap_or(FlexId { kind: FlexKind::Row, id: *id }).id, prov.clone())),
        },
        Row::Flex(id, prov) => Row::Flex(map.get(&FlexId { kind: FlexKind::Row, id: *id }).copied().unwrap_or(FlexId { kind: FlexKind::Row, id: *id }).id, prov.clone()),
    }
}

fn type_key(typ: &Type) -> String { type_name(typ) }

pub fn type_name(typ: &Type) -> String {
    match &typ.kind {
        TypeKind::Var(VarId(id)) => format!("?t{id}"),
        TypeKind::Prim(name) => name.clone(),
        TypeKind::Arrow(a, b) => format!("({} -> {})", type_name(a), type_name(b)),
        TypeKind::Record(row) => format!("{{{}}}", row_name(row)),
        TypeKind::Variant(row) => format!("[{}]", row_name(row)),
    }
}

fn row_name(row: &Row) -> String {
    match row {
        Row::Fields { entries, tail } => {
            let mut text: Vec<String> = entries.iter().map(|(name, (typ, _))| format!("{name}: {}", type_name(typ))).collect();
            if let Some((VarId(id), _)) = tail { text.push(format!("?r{id}")); }
            text.join(", ")
        }
        Row::Flex(VarId(id), _) => format!("?r{id}"),
    }
}
