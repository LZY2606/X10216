//! Generate constraint chunks from a parsed program.
//!
//! Each AST node owns a [`Chunk`] made of [`Segment`]s: flat op runs
//! interleaved with references to child chunks. Flattening reproduces the
//! exact solver order, interleaving let-scoping around child nodes. Chunks
//! are cached under (identity, shape, lexical scope, generalisation
//! environment). Type variables carry symbolic `(owner, role)` keys so
//! renaming an unrelated binder never shifts their identity.

use std::collections::{HashMap, HashSet};

use crate::ast::{hash_str, hash_u64, Arm, Expr, ExprNode, NodeId, Pattern, Program};
use crate::constraints::{unify_id, CacheKey, Chunk, Op, Reason, Segment};
use crate::types::{Row, Type, TV};

#[derive(Clone)]
struct Entry {
    name: String,
    binder: NodeId,
    generalized: bool,
    /// For monomorphic binders (parameters, rec bindings, non-generalised
    /// lets), the shared variable that occurrences are linked to.
    var: Option<TV>,
}

pub struct Generator {
    level: u32,
    next_tv: u32,
    tv_keys: HashMap<(NodeId, String), TV>,
    var_info: Vec<(u32, NodeId, String)>,
    env: Vec<Entry>,
    binders: HashSet<(NodeId, String)>,
    cache: HashMap<CacheKey, Chunk>,
    by_node: HashMap<NodeId, Chunk>,
    reused: HashSet<NodeId>,
}

impl Generator {
    fn new(cache: HashMap<CacheKey, Chunk>) -> Self {
        Generator {
            level: 1,
            next_tv: 1,
            tv_keys: HashMap::new(),
            var_info: Vec::new(),
            env: Vec::new(),
            binders: HashSet::new(),
            cache,
            by_node: HashMap::new(),
            reused: HashSet::new(),
        }
    }

    fn intern(&mut self, owner: NodeId, role: &str) -> TV {
        let key = (owner, role.to_string());
        if let Some(tv) = self.tv_keys.get(&key) {
            return *tv;
        }
        let tv = TV(self.next_tv);
        self.next_tv += 1;
        self.tv_keys.insert(key, tv);
        self.var_info.push((tv.0, owner, role.to_string()));
        tv
    }

    fn scope_hash(&self) -> u64 {
        let mut h = 0xcbf29ce484222325u64;
        for e in &self.env {
            h = hash_str(h, &e.name);
            h = hash_u64(h, e.binder.0);
            h = hash_u64(h, e.generalized as u64);
            h = hash_u64(h, e.var.map(|v| v.0 as u64 + 1).unwrap_or(0));
        }
        h
    }

    fn genenv_hash(&self) -> u64 {
        let mut h = hash_u64(0xcbf29ce484222325u64, self.level as u64);
        for e in &self.env {
            h = hash_u64(h, e.binder.0);
            h = hash_str(h, &e.name);
            h = hash_u64(h, e.var.map(|v| v.0 as u64 + 1).unwrap_or(0xffff_ffff));
        }
        h
    }

    fn shape_hash(node_id: NodeId, children: &[NodeId]) -> u64 {
        let mut h = hash_u64(0x517cc1b727220a95, node_id.0);
        for c in children {
            h = hash_u64(h, c.0);
        }
        h
    }

    fn child_ids(e: &Expr) -> Vec<NodeId> {
        let mut out = Vec::new();
        match e {
            Expr::Fun { body, .. } => out.push(body.id),
            Expr::App { fun, arg } => {
                out.push(fun.id);
                out.push(arg.id);
            }
            Expr::Let { value, body, .. } => {
                out.push(value.id);
                out.push(body.id);
            }
            Expr::If { cond, then_e, else_e } => {
                out.push(cond.id);
                out.push(then_e.id);
                out.push(else_e.id);
            }
            Expr::BinOp { left, right, .. } => {
                out.push(left.id);
                out.push(right.id);
            }
            Expr::Record { fields, .. } => {
                for (_, f) in fields {
                    out.push(f.id);
                }
            }
            Expr::Field { object, .. } => out.push(object.id),
            Expr::Tag { payload, .. } => {
                if let Some(p) = payload {
                    out.push(p.id);
                }
            }
            Expr::Match { scrut, arms } => {
                out.push(scrut.id);
                for a in arms {
                    out.push(a.body.id);
                }
            }
            _ => {}
        }
        out
    }

    fn key_for(&self, node: &ExprNode) -> CacheKey {
        let children = Self::child_ids(&node.expr);
        CacheKey {
            node: node.id,
            shape: Self::shape_hash(node.id, &children),
            scope: self.scope_hash(),
            gen_env: self.genenv_hash(),
        }
    }

    /// Register a built/reused chunk: ensure symbolic vars exist, remap local
    /// variable numbers onto current globals, and store it in `by_node`.
    fn adopt(&mut self, chunk: &Chunk) -> TV {
        let mut local_to_global: HashMap<u32, u32> = HashMap::new();
        for ((owner, role), local) in chunk.var_defs.iter().zip(chunk.vars.iter()) {
            let gtv = self.intern(*owner, role);
            local_to_global.insert(*local, gtv.0);
        }
        let result_global = self.intern(chunk.node, "result").0;
        local_to_global.insert(chunk.result, result_global);

        let segments = chunk
            .segments
            .iter()
            .map(|seg| match seg {
                Segment::Ops(ops) => Segment::Ops(
                    ops.iter().cloned().map(|o| remap_op(o, &local_to_global)).collect(),
                ),
                Segment::Child(c) => Segment::Child(*c),
            })
            .collect();

        let adopted = Chunk {
            node: chunk.node,
            result: result_global,
            vars: chunk.vars.iter().map(|v| local_to_global[v]).collect(),
            var_defs: chunk.var_defs.clone(),
            segments,
            children: chunk.children.clone(),
        };
        self.by_node.insert(adopted.node, adopted);
        TV(result_global)
    }

    /// Build (or reuse) a node, storing its chunk; return its result var.
    fn gen_node(&mut self, node: &ExprNode) -> TV {
        let key = self.key_for(node);
        if let Some(cached) = self.cache.get(&key).cloned() {
            let result = self.adopt(&cached);
            self.reused.insert(node.id);
            return result;
        }

        let before = self.var_info.len();
        let mut segments: Vec<Segment> = Vec::new();
        let result = self.gen_expr(node, &mut segments);

        let mut var_defs = Vec::new();
        let mut vars = Vec::new();
        for (tv, owner, role) in &self.var_info[before..] {
            var_defs.push((*owner, role.clone()));
            vars.push(*tv);
        }

        // Lead with Fresh ops for every variable declared by this chunk, so
        // the solver sees them under the level active when the chunk begins.
        let mut all = Vec::with_capacity(segments.len() + 1);
        let fresh: Vec<Op> = vars
            .iter()
            .zip(var_defs.iter())
            .map(|(tv, (owner, role))| Op::Fresh {
                var: *tv,
                owner: *owner,
                role: role.clone(),
            })
            .collect();
        all.push(Segment::Ops(fresh));
        all.extend(segments);

        let chunk = Chunk {
            node: node.id,
            result: result.0,
            vars: vars.clone(),
            var_defs,
            segments: all,
            children: Self::child_ids(&node.expr),
        };
        self.cache.insert(key, chunk.clone());
        self.adopt(&chunk)
    }
}

fn is_value(e: &Expr) -> bool {
    matches!(e, Expr::Lit(_) | Expr::Var(_) | Expr::Fun { .. } | Expr::Record { .. } | Expr::Tag { .. })
}

fn ops(segments: &mut Vec<Segment>, v: Vec<Op>) {
    segments.push(Segment::Ops(v));
}
fn child(segments: &mut Vec<Segment>, id: NodeId) {
    segments.push(Segment::Child(id));
}

impl Generator {
    fn var_of(&mut self, owner: NodeId, role: &str) -> TV {
        self.intern(owner, role)
    }

    fn unify_op(&self, expected: Type, actual: Type, reason: Reason, span_owner: Option<NodeId>) -> Op {
        let id = unify_id(&expected, &actual, &reason);
        Op::Unify { id, expected, actual, reason, span_owner }
    }

    fn gen_expr(&mut self, node: &ExprNode, segs: &mut Vec<Segment>) -> TV {
        let n = node.id;
        match &node.expr {
            Expr::Lit(lit) => {
                let result = self.var_of(n, "result");
                let ty = match lit {
                    crate::ast::Literal::Int(_) => Type::con("int"),
                    crate::ast::Literal::Bool(_) => Type::con("bool"),
                    crate::ast::Literal::Str(_) => Type::con("string"),
                };
                ops(segs, vec![self.unify_op(ty, Type::Var(result), Reason::Literal(n), Some(n))]);
                result
            }
            Expr::Var(name) => {
                let result = self.var_of(n, "result");
                if let Some(entry) = self.env.iter().rev().find(|e| &e.name == name).cloned() {
                    if let Some(tv) = entry.var {
                        ops(
                            segs,
                            vec![self.unify_op(
                                Type::Var(tv),
                                Type::Var(result),
                                Reason::VarOccur { use_node: n, binder_node: Some(entry.binder) },
                                Some(n),
                            )],
                        );
                    } else {
                        ops(
                            segs,
                            vec![Op::Instantiate {
                                binder: entry.binder,
                                use_node: n,
                                name: name.clone(),
                                target: result.0,
                            }],
                        );
                    }
                } else {
                    ops(segs, vec![Op::Note { text: format!("unbound variable '{name}'"), owner: Some(n) }]);
                }
                result
            }
            Expr::Fun { params, body } => {
                let result = self.var_of(n, "result");
                let mut arg_types = Vec::new();
                let saved = self.env.len();
                for (i, p) in params.iter().enumerate() {
                    let ptv = self.var_of(n, &format!("param:{}", p.name));
                    let binder = NodeId(n.0 ^ (0x9e37_79b9_7f4a_7c15u64.wrapping_mul(i as u64 + 1)));
                    self.env.push(Entry {
                        name: p.name.clone(),
                        binder,
                        generalized: false,
                        var: Some(ptv),
                    });
                    arg_types.push(Type::Var(ptv));
                }
                let body_tv = self.gen_node(body);
                self.env.truncate(saved);
                child(segs, body.id);
                let mut arrow = Type::Var(body_tv);
                for at in arg_types.into_iter().rev() {
                    arrow = Type::arrow(at, arrow);
                }
                ops(segs, vec![self.unify_op(arrow, Type::Var(result), Reason::FunResult(n), Some(n))]);
                result
            }
            Expr::App { fun, arg } => {
                let result = self.var_of(n, "result");
                let ret = self.var_of(n, "app-result");
                let fun_tv = self.gen_node(fun);
                let arg_tv = self.gen_node(arg);
                child(segs, fun.id);
                child(segs, arg.id);
                ops(
                    segs,
                    vec![
                        self.unify_op(
                            Type::arrow(Type::Var(arg_tv), Type::Var(ret)),
                            Type::Var(fun_tv),
                            Reason::AppArg(n),
                            Some(arg.id),
                        ),
                        self.unify_op(Type::Var(ret), Type::Var(result), Reason::AppResult(n), Some(n)),
                    ],
                );
                result
            }
            Expr::BinOp { op, left, right, op_span: _ } => {
                let result = self.var_of(n, "result");
                let eqv = self.var_of(n, "eq");
                let lt = self.gen_node(left);
                let rt = self.gen_node(right);
                child(segs, left.id);
                child(segs, right.id);
                let (operand_l, operand_r, ret) = if op == "&&" || op == "||" {
                    (Type::con("bool"), Type::con("bool"), Type::con("bool"))
                } else if op == "==" || op == "!=" {
                    (Type::Var(eqv), Type::Var(eqv), Type::con("bool"))
                } else {
                    (Type::con("int"), Type::con("int"), Type::con("int"))
                };
                ops(
                    segs,
                    vec![
                        self.unify_op(operand_l, Type::Var(lt), Reason::BinOpLhs(n), Some(left.id)),
                        self.unify_op(operand_r, Type::Var(rt), Reason::BinOpRhs(n), Some(right.id)),
                        self.unify_op(ret, Type::Var(result), Reason::BinOpResult(n), Some(n)),
                    ],
                );
                result
            }
            Expr::If { cond, then_e, else_e } => {
                let result = self.var_of(n, "result");
                let ct = self.gen_node(cond);
                let tt = self.gen_node(then_e);
                let et = self.gen_node(else_e);
                child(segs, cond.id);
                child(segs, then_e.id);
                child(segs, else_e.id);
                ops(
                    segs,
                    vec![
                        self.unify_op(Type::con("bool"), Type::Var(ct), Reason::IfCond(n), Some(cond.id)),
                        self.unify_op(Type::Var(tt), Type::Var(et), Reason::IfBranches(n), Some(else_e.id)),
                        self.unify_op(Type::Var(tt), Type::Var(result), Reason::IfBranches(n), Some(n)),
                    ],
                );
                result
            }
            Expr::Field { object, label } => {
                let result = self.var_of(n, "result");
                let tail = self.var_of(n, "row-tail");
                let otv = self.gen_node(object);
                child(segs, object.id);
                let row = Type::record(vec![(label.as_str(), Type::Var(result))], Some(tail));
                ops(
                    segs,
                    vec![self.unify_op(
                        row,
                        Type::Var(otv),
                        Reason::FieldProject(n, label.clone()),
                        Some(n),
                    )],
                );
                result
            }
            Expr::Tag { label, payload } => {
                let result = self.var_of(n, "result");
                let payload_tv = if let Some(p) = payload {
                    let tv = self.gen_node(p);
                    child(segs, p.id);
                    tv
                } else {
                    self.var_of(n, "unit")
                };
                let vty = Type::variant(vec![(label.as_str(), Type::Var(payload_tv))], None);
                ops(
                    segs,
                    vec![self.unify_op(vty, Type::Var(result), Reason::VariantTag(n, label.clone()), Some(n))],
                );
                result
            }
            Expr::Record { fields, open: _ } => {
                let result = self.var_of(n, "result");
                // record literals are closed rows: all fields known
                let tail: Option<TV> = None;
                let mut fs: Vec<(String, Type)> = Vec::new();
                for (label, fexpr) in fields {
                    let ftv = self.gen_node(fexpr);
                    child(segs, fexpr.id);
                    fs.push((label.clone(), Type::Var(ftv)));
                }
                let row = Type::Record(Row { fields: fs, tail });
                ops(
                    segs,
                    vec![self.unify_op(row, Type::Var(result), Reason::Synthetic("record literal".into()), Some(n))],
                );
                result
            }
            Expr::Match { scrut, arms } => {
                let result = self.var_of(n, "result");
                let stv = self.gen_node(scrut);
                child(segs, scrut.id);
                for (i, arm) in arms.iter().enumerate() {
                    self.gen_arm(n, i, arm, stv, result, segs);
                }
                // Coverage constraint: the scrutinee (closed injected row)
                // must only carry tags the arms handle. We build an open
                // expected row containing every tag payload variable and
                // unify expected(open) with actual(closed): actual-only tags
                // are absorbed harmlessly for *records* but here we use the
                // dedicated `MatchCoverage` reason so the solver enforces
                // exhaustiveness and reports unexpected tags precisely.
                self.emit_match_coverage(n, arms, stv, segs);
                result
            }
            Expr::Let { rec, name, value, body } => {
                let result = self.var_of(n, "result");
                self.binders.insert((n, name.clone()));

                if *rec {
                    let btv = self.var_of(n, "bind");
                    ops(segs, vec![Op::BeginLet { node: n }]);
                    self.env.push(Entry { name: name.clone(), binder: n, generalized: false, var: Some(btv) });
                    let vt = self.gen_node(value);
                    child(segs, value.id);
                    self.env.pop();
                    ops(
                        segs,
                        vec![
                            self.unify_op(Type::Var(btv), Type::Var(vt), Reason::LetBind(n), Some(value.id)),
                            Op::Generalize {
                                node: n,
                                name: name.clone(),
                                root: Type::Var(btv),
                                generalized: false,
                                rec: true,
                            },
                        ],
                    );
                    self.env.push(Entry { name: name.clone(), binder: n, generalized: false, var: Some(btv) });
                    let body_tv = self.gen_node(body);
                    child(segs, body.id);
                    self.env.pop();
                    ops(segs, vec![self.unify_op(Type::Var(body_tv), Type::Var(result), Reason::LetBind(n), Some(n))]);
                } else {
                    ops(segs, vec![Op::BeginLet { node: n }]);
                    let vt = self.gen_node(value);
                    child(segs, value.id);
                    let generalized = is_value(&value.expr);
                    ops(
                        segs,
                        vec![Op::Generalize {
                            node: n,
                            name: name.clone(),
                            root: Type::Var(vt),
                            generalized,
                            rec: false,
                        }],
                    );
                    self.env.push(Entry { name: name.clone(), binder: n, generalized, var: None });
                    let body_tv = self.gen_node(body);
                    child(segs, body.id);
                    self.env.pop();
                    ops(segs, vec![self.unify_op(Type::Var(body_tv), Type::Var(result), Reason::LetBind(n), Some(n))]);
                }
                result
            }
        }
    }
}

impl Generator {
    fn gen_arm(&mut self, match_node: NodeId, i: usize, arm: &Arm, scrut: TV, result: TV, segs: &mut Vec<Segment>) {
        let arm_binder = NodeId(match_node.0 ^ 0xA11CE ^ (0x9e37_79b9_7f4a_7c15u64.wrapping_mul(i as u64 + 1)));
        let body_tv = match &arm.pat {
            Pattern::Wild => {
                let b = self.gen_node(&arm.body);
                child(segs, arm.body.id);
                b
            }
            Pattern::Name(name) => {
                let btv = self.var_of(match_node, &format!("arm{i}-bind"));
                ops(
                    segs,
                    vec![self.unify_op(
                        Type::Var(btv),
                        Type::Var(scrut),
                        Reason::Synthetic(format!("arm {i} binder")),
                        Some(arm.body.id),
                    )],
                );
                self.env.push(Entry {
                    name: name.clone(),
                    binder: arm_binder,
                    generalized: false,
                    var: Some(btv),
                });
                let b = self.gen_node(&arm.body);
                child(segs, arm.body.id);
                self.env.pop();
                b
            }
            Pattern::Tag(tag, binding, _tag_span) => {
                let payload_tv = self.var_of(match_node, &format!("arm{i}-payload"));
                // The tag payload is linked to the scrutinee row when the
                // coverage constraint unifies below; record the association
                // via a synthetic note-free op on the coverage row itself.
                let _ = (tag, scrut);
                if let Some(bname) = binding {
                    self.env.push(Entry {
                        name: bname.clone(),
                        binder: arm_binder,
                        generalized: false,
                        var: Some(payload_tv),
                    });
                    let b = self.gen_node(&arm.body);
                    child(segs, arm.body.id);
                    self.env.pop();
                    b
                } else {
                    let b = self.gen_node(&arm.body);
                    child(segs, arm.body.id);
                    b
                }
            }
        };
        ops(
            segs,
            vec![self.unify_op(
                Type::Var(body_tv),
                Type::Var(result),
                Reason::MatchArmResult(match_node, i),
                Some(arm.body.id),
            )],
        );
    }

    /// Pre-order flattening of chunks into ops/chunk-membership tracking.
    fn flatten(
        chunks_by_node: &HashMap<NodeId, Chunk>,
        root: NodeId,
    ) -> (Vec<Op>, Vec<(u64, NodeId)>) {
        let mut flat: Vec<Op> = Vec::new();
        // membership: unify constraint id -> owning node
        let mut membership: Vec<(u64, NodeId)> = Vec::new();
        let mut visiting: Vec<NodeId> = Vec::new();
        fn walk(
            cid: NodeId,
            chunks: &HashMap<NodeId, Chunk>,
            flat: &mut Vec<Op>,
            membership: &mut Vec<(u64, NodeId)>,
            visiting: &mut Vec<NodeId>,
        ) {
            if visiting.contains(&cid) {
                return;
            }
            visiting.push(cid);
            let chunk = chunks.get(&cid).expect("chunk exists");
            for seg in &chunk.segments {
                match seg {
                    Segment::Ops(ops_list) => {
                        for op in ops_list {
                            if let Op::Unify { id, .. } = op {
                                membership.push((*id, cid));
                            }
                            flat.push(op.clone());
                        }
                    }
                    Segment::Child(c) => walk(*c, chunks, flat, membership, visiting),
                }
            }
        }
        walk(root, chunks_by_node, &mut flat, &mut membership, &mut visiting);
        (flat, membership)
    }
}

fn remap_tv(tv: TV, m: &HashMap<u32, u32>) -> TV {
    TV(*m.get(&tv.0).unwrap_or(&tv.0))
}

fn remap_type(t: &Type, m: &HashMap<u32, u32>) -> Type {
    match t {
        Type::Var(v) => Type::Var(remap_tv(*v, m)),
        Type::Con(n, ts) => Type::Con(n.clone(), ts.iter().map(|x| remap_type(x, m)).collect()),
        Type::Arrow(a, b) => Type::Arrow(Box::new(remap_type(a, m)), Box::new(remap_type(b, m))),
        Type::Record(r) | Type::Variant(r) => {
            let nr = Row {
                fields: r.fields.iter().map(|(l, t)| (l.clone(), remap_type(t, m))).collect(),
                tail: r.tail.map(|v| remap_tv(v, m)),
            };
            if matches!(t, Type::Record(_)) {
                Type::Record(nr)
            } else {
                Type::Variant(nr)
            }
        }
    }
}

fn remap_op(op: Op, m: &HashMap<u32, u32>) -> Op {
    match op {
        Op::Fresh { var, owner, role } => {
            Op::Fresh { var: *m.get(&var).unwrap_or(&var), owner, role }
        }
        Op::Unify { id, expected, actual, reason, span_owner } => Op::Unify {
            id,
            expected: remap_type(&expected, m),
            actual: remap_type(&actual, m),
            reason,
            span_owner,
        },
        Op::BeginLet { node } => Op::BeginLet { node },
        Op::Generalize { node, name, root, generalized, rec } => Op::Generalize {
            node,
            name,
            root: remap_type(&root, m),
            generalized,
            rec,
        },
        Op::Instantiate { binder, use_node, name, target } => Op::Instantiate {
            binder,
            use_node,
            name,
            target: *m.get(&target).unwrap_or(&target),
        },
        Op::Note { text, owner } => Op::Note { text, owner },
    }
}

/// Output of generation: the flattened op stream plus structural information.
pub struct GeneratedProgram {
    pub flat_ops: Vec<Op>,
    pub membership: HashMap<u64, NodeId>,
    pub chunks: Vec<Chunk>,
    pub reused: HashSet<NodeId>,
    pub root_result: u32,
    pub var_info: Vec<(u32, NodeId, String)>,
    pub keys: Vec<CacheKey>,
}

/// Generate everything. Takes (and returns) the persistent cache so callers
/// can reuse it across edits.
pub fn generate(
    program: &Program,
    cache: HashMap<CacheKey, Chunk>,
) -> (GeneratedProgram, HashMap<CacheKey, Chunk>) {
    let mut g = Generator::new(cache);
    let root_result = if let Some(root) = program.root.as_ref() {
        g.gen_node(root).0
    } else {
        0
    };

    let keys: Vec<CacheKey> = g.cache.keys().cloned().collect();
    let cache_out = g.cache;
    let mut chunks: Vec<Chunk> = g.by_node.values().cloned().collect();
    chunks.sort_by_key(|c| c.node.0);
    let reused = g.reused;
    let var_info = g.var_info;

    let mut membership = HashMap::new();
    let mut flat_ops = Vec::new();
    if let Some(root) = program.root.as_ref() {
        let (flat, mem) = Generator::flatten(&by_node_from_chunks(&chunks), root.id);
        flat_ops = flat;
        membership = mem.into_iter().collect();
    }

    (
        GeneratedProgram {
            flat_ops,
            membership,
            chunks,
            reused,
            root_result,
            var_info,
            keys,
        },
        cache_out,
    )
}

fn by_node_from_chunks(chunks: &[Chunk]) -> HashMap<NodeId, Chunk> {
    chunks.iter().map(|c| (c.node, c.clone())).collect()
}

/// Public remap used by the solver during scheme instantiation.
pub fn remap_type_pub(t: &Type, m: &HashMap<u32, u32>) -> Type {
    remap_type(t, m)
}

impl Generator {
    /// Emit one unify constraint whose expected row lists every tag handled by
    /// the arms (open tail, so extra expected tags are allowed) and whose
    /// actual side is the closed scrutinee row. Solver direction makes tags
    /// present in the scrutinee but absent from the arms "extra".
    fn emit_match_coverage(
        &mut self,
        match_node: NodeId,
        arms: &[Arm],
        scrut: TV,
        segs: &mut Vec<Segment>,
    ) {
        // Only meaningful when at least one tag arm exists; wildcard/name arms
        // accept everything.
        let has_total = arms.iter().any(|a| matches!(a.pat, Pattern::Wild | Pattern::Name(_)));
        if has_total {
            return;
        }
        let mut fields: Vec<(String, Type)> = Vec::new();
        let mut seen_tags: HashSet<String> = HashSet::new();
        for (i, arm) in arms.iter().enumerate() {
            if let Pattern::Tag(tag, _, _) = &arm.pat {
                if seen_tags.insert(tag.clone()) {
                    let ptv = self.var_of(match_node, &format!("arm{i}-payload"));
                    fields.push((tag.clone(), Type::Var(ptv)));
                }
            }
        }
        if fields.is_empty() {
            return;
        }
        // open tail on the expected (arm) side
        let tail = self.var_of(match_node, "match-tail");
        let expected = Type::Variant(Row { fields, tail: Some(tail) });
        let op = self.unify_op(
            expected,
            Type::Var(scrut),
            Reason::MatchCoverage(match_node),
            Some(match_node),
        );
        ops(segs, vec![op]);
    }
}
