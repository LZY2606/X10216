// Constraint solver: instantiation, rank-based generalization with the value
// restriction, and unification with occurs check and row mismatch reporting.

use crate::constraints::*;
use crate::term::*;
use crate::types::*;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

#[derive(Clone, Debug)]
pub enum ErrorKind {
    Mismatch,
    Occurs,
    Rigid,
    RowField,
    Arity,
    FieldDuplicate,
}

#[derive(Clone, Debug)]
pub struct TypeError {
    pub id: u32,
    pub kind: ErrorKind,
    pub message: String,
    pub spans: Vec<(usize, usize)>,
    pub cids: Vec<CId>,
    pub missing: Vec<String>,
    pub extra: Vec<String>,
    pub sort: &'static str,
    /// Canonical stable identity of the conflict (sorted cids + kind signature).
    pub signature: String,
}

#[derive(Clone, Debug, Default)]
pub struct CoreSet {
    pub cids: Vec<CId>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct StepRecord {
    pub index: usize,
    pub kind: String,
    pub cid: Option<u32>,
    pub label: String,
    pub detail: serde_json::Value,
    pub ok: bool,
    pub error_id: Option<u32>,
    /// Snapshot after the action.
    pub vars: Vec<VarSnapshot>,
    pub active_errors: Vec<u32>,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct VarSnapshot {
    pub id: u32,
    pub root: u32,
    pub term: TermF,
    pub rank: u32,
    pub origin_expr: Option<u32>,
}

#[derive(Clone, Debug)]
pub struct BindingScheme {
    pub quantified: Vec<NodeIdT>,
    pub body: NodeIdT,
    pub origin_let: CId,
}

#[derive(Clone)]
pub struct SolveConfig {
    pub disabled: HashSet<CId>,
    /// If true, stop after recording the first error per branch attempt
    /// (used only for core probing where we still walk everything but ignore
    /// recorded errors; behavior is controlled via `probe`).
    pub probe: bool,
}

impl Default for SolveConfig {
    fn default() -> Self {
        SolveConfig { disabled: HashSet::new(), probe: false }
    }
}

#[derive(Clone, Debug)]
enum Entry {
    Flex {
        kind: FlexKind,
        rank: u32,
        hint: String,
        origin_expr: Option<u32>,
        rigid_name: Option<String>,
    },
    Link(NodeIdT),
    Concrete(ConcreteDesc),
}

#[derive(Clone, Debug)]
enum ConcreteDesc {
    Con(String),
    Arrow(NodeIdT, NodeIdT),
    Row {
        sort: RowSort,
        fields: Vec<(String, NodeIdT)>,
        tail: Tail,
    },
}

#[derive(Clone, Copy, Debug)]
enum Tail {
    Closed,
    Open(NodeIdT),
}

#[derive(Clone, Default, Debug)]
struct Stats {
    instantiations: u32,
    generalizations: u32,
    unifications: u32,
    errors: u32,
}

pub struct Solver<'a> {
    graph: &'a ConstraintGraph,
    entries: Vec<Option<Entry>>,
    ambient_roots: &'a HashMap<CId, NodeIdT>,
    scheme_insts: &'a HashMap<CId, (Vec<NodeIdT>, NodeIdT)>,
    schemes: HashMap<CId, BindingScheme>,
    config: SolveConfig,
    errors: Vec<TypeError>,
    steps: Vec<StepRecord>,
    stats: Stats,
    /// Per-run fresh node counter continues above the static ids.
    next_dynamic: u32,
    /// Blame cids active during unification.
    blame: Vec<CId>,
    /// Static base descriptors for concrete nodes.
    bases: &'a HashMap<NodeIdT, BaseS>,
    flexes: &'a HashMap<NodeIdT, FlexDesc>,
    /// Forbid re-reporting the same signature during a walk.
    seen_signatures: HashSet<String>,
    /// Only collect steps when true (disabled during core probing).
    record_steps: bool,
}

pub struct Analysis {
    pub errors: Vec<TypeError>,
    pub steps: Vec<StepRecord>,
    pub cores: Vec<Vec<CId>>,
    pub final_vars: Vec<VarSnapshot>,
    pub root_type: Option<TermF>,
    pub stats: (u32, u32, u32, u32),
}

impl<'a> Solver<'a> {
    pub fn analyze(
        graph: &'a ConstraintGraph,
        bases: &'a HashMap<NodeIdT, BaseS>,
        flexes: &'a HashMap<NodeIdT, FlexDesc>,
        ambient: &'a HashMap<CId, NodeIdT>,
        scheme_insts: &'a HashMap<CId, (Vec<NodeIdT>, NodeIdT)>,
        config: &SolveConfig,
    ) -> Analysis {
        let config = config.clone();
        let max_id = graph.type_nodes.iter().map(|t| t.id.0).max().unwrap_or(0) + 1;
        let mut entries = vec![None; max_id as usize];
        for info in &graph.type_nodes {
            if let Some(b) = bases.get(&info.id) {
                entries[info.id.0 as usize] = Some(Entry::Concrete(desc_from_base(b)));
            } else if let Some(rigid) = &info.rigid {
                entries[info.id.0 as usize] = Some(Entry::Flex {
                    kind: FlexKind::Type,
                    rank: u32::MAX / 2,
                    hint: rigid.clone(),
                    origin_expr: info.expr.map(|n| n.0),
                    rigid_name: Some(rigid.clone()),
                });
            } else if let Some(f) = flexes.get(&info.id) {
                entries[info.id.0 as usize] = Some(Entry::Flex {
                    kind: f.kind,
                    rank: f.rank,
                    hint: f.hint.clone(),
                    origin_expr: info.expr.map(|n| n.0),
                    rigid_name: None,
                });
            }
        }
        eprintln!("DBG before box entries_len={}", entries.len());
        let mut solver = Box::new(Solver {
            graph,
            entries,
            ambient_roots: ambient,
            scheme_insts,
            schemes: HashMap::new(),
            config,
            errors: Vec::new(),
            steps: Vec::new(),
            stats: Stats::default(),
            next_dynamic: max_id,
            blame: Vec::new(),
            bases,
            flexes,
            seen_signatures: HashSet::new(),
            record_steps: true,
        });
        eprintln!("DBG before walk");
        solver.walk(graph.root);
        eprintln!("DBG after walk");
        let root_expr_id = graph.ast_nodes.iter().fold(None, |acc: Option<NodeIdT>, n| {
            if n.path == "$" { Some(n.id) } else { acc }
        });
        let root_ty = root_expr_id
            .and_then(|n| graph.expr_type.get(&n).copied())
            .map(|t| solver.snapshot_term(t));
        let final_vars = solver.all_snapshots();
        let mut analysis = Analysis {
            errors: solver.errors,
            steps: solver.steps,
            cores: Vec::new(),
            final_vars,
            root_type: root_ty,
            stats: (solver.stats.instantiations, solver.stats.generalizations,
                    solver.stats.unifications, solver.stats.errors),
        };
        analysis.cores = crate::cores::find_cores(graph, bases, flexes, ambient, scheme_insts, &analysis.errors);
        analysis
    }

    fn enabled(&self, id: CId) -> bool {
        !self.config.disabled.contains(&id)
    }

    fn walk(&mut self, cid: CId) {
        let depth = self.blame.len();
        eprintln!("WALK {} depth~{}", cid.0, depth);
        let node = match self.graph.nodes.iter().find(|n| n.id == cid) {
            Some(n) => n.clone(),
            None => return,
        };
        if !self.enabled(cid) {
            self.log_skip(cid, &node);
            return;
        }
        let kids = node.children.clone();
        match &node.kind {
            CKind::Block => {
                for k in kids {
                    self.walk(k);
                }
            }
            CKind::Marker => {
                for k in kids {
                    self.walk(k);
                }
            }
            CKind::Eq(eq) => {
                self.blame.push(cid);
                let ok = self.unify(eq.left, eq.right);
                self.blame.pop();
                self.log_unify(cid, &eq.label, ok);
                for k in kids {
                    self.walk(k);
                }
            }
            CKind::Inst(inst) => {
                self.do_instantiate(cid, inst.clone());
                for k in kids {
                    self.walk(k);
                }
            }
            CKind::Gen(gen) => self.do_generalize(cid, gen.clone()),
            CKind::EmptyRow(er) => {
                self.blame.push(cid);
                let ok = self.unify_empty_row(er.row, er.sort);
                self.blame.pop();
                self.log_empty(cid, er.sort, ok);
                for k in kids {
                    self.walk(k);
                }
            }
        }
    }
}

fn desc_from_base(b: &BaseS) -> ConcreteDesc {
    match b {
        BaseS::Con(n) => ConcreteDesc::Con(n.clone()),
        BaseS::Arrow(a, c) => ConcreteDesc::Arrow(*a, *c),
        BaseS::Row { sort, fields, tail } => ConcreteDesc::Row {
            sort: match sort {
                RowSortS::Record => RowSort::Record,
                RowSortS::Variant => RowSort::Variant,
            },
            fields: fields.clone(),
            tail: match tail {
                TailS::Closed => Tail::Closed,
                TailS::Open(r) => Tail::Open(*r),
            },
        },
    }
}

impl<'a> Solver<'a> {
    fn do_instantiate(&mut self, cid: CId, inst: InstSpec) {
        let copied = if inst.origin_let == CId(0) {
            self.ambient_roots.get(&cid).copied().map(|root| {
                let mut map = HashMap::new();
                let new_root = self.copy_term(root, &mut map);
                (new_root, map)
            })
        } else if let Some(sch) = self.schemes.get(&inst.origin_let).cloned() {
            let mut map = HashMap::new();
            for q in &sch.quantified {
                let nv = self.new_dynamic_flex(FlexKind::Type, 1, String::new(), None);
                map.insert(*q, nv);
            }
            let new_root = self.copy_term(sch.body, &mut map);
            Some((new_root, map))
        } else {
            // Scheme missing (e.g. disabled gen): fall back to plain copy of body.
            self.scheme_insts.get(&cid).cloned().map(|(_, body)| {
                let mut map = HashMap::new();
                let nr = self.copy_term(body, &mut map);
                (nr, map)
            })
        };
        let (new_root, map) = match copied {
            Some(x) => x,
            None => {
                self.log_simple(cid, "inst", &format!("instantiate {} (no scheme available)", inst.name), false);
                return;
            }
        };
        self.blame.push(cid);
        let ok = self.unify(inst.target, new_root);
        self.blame.pop();
        let mapping: BTreeMap<String, u32> = map
            .iter()
            .map(|(k, v)| (format!("t{}", k.0), v.0))
            .collect();
        self.log_step(
            "inst",
            Some(cid),
            format!("instantiate `{}`", inst.name),
            serde_json::json!({ "mapping": mapping, "unified": ok }),
            ok,
        );
    }

    fn new_dynamic_flex(&mut self, kind: FlexKind, rank: u32, hint: String, origin: Option<u32>) -> NodeIdT {
        let id = NodeIdT(self.next_dynamic);
        self.next_dynamic += 1;
        if (id.0 as usize) >= self.entries.len() {
            self.entries.resize(id.0 as usize + 16, None);
        }
        self.entries[id.0 as usize] = Some(Entry::Flex {
            kind,
            rank,
            hint,
            origin_expr: origin,
            rigid_name: None,
        });
        id
    }

    /// Copy the concrete/flex term graph rooted at `src`, remapping flex nodes
    /// via `mapping`. Rigid annotation nodes are preserved.
    fn copy_term(&mut self, src: NodeIdT, mapping: &mut HashMap<NodeIdT, NodeIdT>) -> NodeIdT {
        let root = self.find(src);
        if let Some(existing) = mapping.get(&root) {
            return *existing;
        }
        match self.entries[root.0 as usize].clone() {
            Some(Entry::Flex { kind, rank, hint, origin_expr, rigid_name }) => {
                if rigid_name.is_some() {
                    return root;
                }
                let nv = self.new_dynamic_flex(kind, rank, hint, origin_expr);
                mapping.insert(root, nv);
                nv
            }
            Some(Entry::Concrete(c)) => {
                let nc = match c {
                    ConcreteDesc::Con(n) => ConcreteDesc::Con(n),
                    ConcreteDesc::Arrow(a, b) => {
                        let na = self.copy_term(a, mapping);
                        let nb = self.copy_term(b, mapping);
                        ConcreteDesc::Arrow(na, nb)
                    }
                    ConcreteDesc::Row { sort, fields, tail } => {
                        let mut nf = Vec::new();
                        for (l, t) in fields {
                            let nt = self.copy_term(t, mapping);
                            nf.push((l, nt));
                        }
                        let nt = match tail {
                            Tail::Closed => Tail::Closed,
                            Tail::Open(r) => Tail::Open(self.copy_row_var(r, mapping)),
                        };
                        ConcreteDesc::Row { sort, fields: nf, tail: nt }
                    }
                };
                let id = NodeIdT(self.next_dynamic);
                self.next_dynamic += 1;
                if (id.0 as usize) >= self.entries.len() {
                    self.entries.resize(id.0 as usize + 16, None);
                }
                self.entries[id.0 as usize] = Some(Entry::Concrete(nc));
                mapping.insert(root, id);
                id
            }
            Some(Entry::Link(_)) => unreachable!("find returned a link"),
            None => root,
        }
    }

    fn copy_row_var(&mut self, r: NodeIdT, mapping: &mut HashMap<NodeIdT, NodeIdT>) -> NodeIdT {
        let root = self.find(r);
        if let Some(e) = mapping.get(&root) {
            return *e;
        }
        match &self.entries[root.0 as usize] {
            Some(Entry::Flex { kind: FlexKind::Row, .. }) => {
                let kind = FlexKind::Row;
                let rank = self.flex_rank(root);
                let nv = self.new_dynamic_flex(kind, rank, String::new(), None);
                mapping.insert(root, nv);
                nv
            }
            Some(Entry::Concrete(ConcreteDesc::Row { tail: Tail::Open(r2), .. })) => {
                self.copy_row_var(*r2, mapping)
            }
            Some(Entry::Concrete(_)) => {
                // Closed / concrete mismatch: copy as a fresh row variable that
                // will later unify and report the real conflict.
                let nv = self.new_dynamic_flex(FlexKind::Row, 1, String::new(), None);
                mapping.insert(root, nv);
                nv
            }
            _ => {
                let nv = self.new_dynamic_flex(FlexKind::Row, 1, String::new(), None);
                mapping.insert(root, nv);
                nv
            }
        }
    }

    fn flex_rank(&self, v: NodeIdT) -> u32 {
        match &self.entries[v.0 as usize] {
            Some(Entry::Flex { rank, .. }) => *rank,
            _ => 1,
        }
    }

    fn do_generalize(&mut self, cid: CId, gen: GenSpec) {
        // The let layout is [rhs_block, gen, body_block]; the binding eq is the
        // last child of rhs_block.
        let let_id = self.parent(cid);
        let mut target: Option<NodeIdT> = None;
        if let Some(lid) = let_id {
            if let Some(let_node) = self.graph.nodes.iter().find(|n| n.id == lid) {
                if let Some(rhs_id) = let_node.children.first() {
                    if let Some(rhs_node) = self.graph.nodes.iter().find(|n| n.id == *rhs_id) {
                        for child in rhs_node.children.iter().rev() {
                            if let Some(c) = self.graph.nodes.iter().find(|n| n.id == *child) {
                                if let CKind::Eq(eq) = &c.kind {
                                    target = Some(eq.left);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
        let target = match target {
            Some(t) => t,
            None => {
                self.log_simple(cid, "gen", "generalize (binding not found)", false);
                return;
            }
        };
        if gen.value_restricted {
            self.log_step(
                "gen",
                Some(cid),
                format!("generalize `{}` skipped (value restriction)", gen.name),
                serde_json::json!({ "value_restricted": true, "quantified": [] }),
                true,
            );
            return;
        }
        if gen.rec {
            self.log_step(
                "gen",
                Some(cid),
                format!("generalize `{}` skipped (recursive binding is monomorphic)", gen.name),
                serde_json::json!({ "rec": true, "quantified": [] }),
                true,
            );
            return;
        }
        let root = self.find(target);
        let rank = self.flex_rank(root).max(1);
        let mut qset: BTreeSet<NodeIdT> = BTreeSet::new();
        self.collect_generalizable(root, rank, &mut qset);
        let quantified: Vec<NodeIdT> = qset.iter().copied().collect();
        if let Some(lid) = let_id {
            self.schemes.insert(
                lid,
                BindingScheme { quantified: quantified.clone(), body: root, origin_let: lid },
            );
        }
        self.stats.generalizations += 1;
        let qnames: Vec<String> = quantified.iter().map(|q| format!("t{}", q.0)).collect();
        self.log_step(
            "gen",
            Some(cid),
            format!("generalize `{}`", gen.name),
            serde_json::json!({ "quantified": qnames, "count": quantified.len() }),
            true,
        );
    }

    fn collect_generalizable(&mut self, v: NodeIdT, rank: u32, out: &mut BTreeSet<NodeIdT>) {
        let root = self.find(v);
        if out.contains(&root) {
            return;
        }
        let desc = match &self.entries[root.0 as usize] {
            Some(Entry::Flex { kind: FlexKind::Type, rank: r, rigid_name: None, .. }) if *r > rank => {
                out.insert(root);
                return;
            }
            Some(Entry::Concrete(c)) => Some(c.clone()),
            _ => None,
        };
        match desc {
            Some(ConcreteDesc::Arrow(a, b)) => {
                self.collect_generalizable(a, rank, out);
                self.collect_generalizable(b, rank, out);
            }
            Some(ConcreteDesc::Row { fields, tail, .. }) => {
                for (_, t) in fields {
                    self.collect_generalizable(t, rank, out);
                }
                if let Tail::Open(r) = tail {
                    let rr = self.find(r);
                    let qualifies = matches!(
                        &self.entries[rr.0 as usize],
                        Some(Entry::Flex { kind: FlexKind::Row, rank: rk, rigid_name: None, .. }) if *rk > rank
                    );
                    if qualifies {
                        out.insert(rr);
                    }
                }
            }
            None => {}
            Some(ConcreteDesc::Con(_)) => {}
        }
    }

    fn parent(&self, cid: CId) -> Option<CId> {
        self.graph.nodes.iter().find(|n| n.id == cid).and_then(|n| n.parent)
    }
}

impl<'a> Solver<'a> {
    fn find(&mut self, v: NodeIdT) -> NodeIdT {
        let mut cur = v;
        loop {
            match &self.entries[cur.0 as usize] {
                Some(Entry::Link(t)) => {
                    let t = *t;
                    cur = t;
                }
                _ => break,
            }
        }
        // Path compression.
        if cur != v {
            let mut x = v;
            while let Some(Entry::Link(t)) = self.entries[x.0 as usize].clone() {
                self.entries[x.0 as usize] = Some(Entry::Link(cur));
                x = t;
                if x == cur {
                    break;
                }
            }
        }
        cur
    }

    fn set_link(&mut self, from: NodeIdT, to: NodeIdT) {
        self.entries[from.0 as usize] = Some(Entry::Link(to));
    }

    fn unify(&mut self, a: NodeIdT, b: NodeIdT) -> bool {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return true;
        }
        let ea = match &self.entries[ra.0 as usize] {
            Some(e) => e.clone(),
            None => return true,
        };
        let eb = match &self.entries[rb.0 as usize] {
            Some(e) => e.clone(),
            None => return true,
        };
        match (ea, eb) {
            (Entry::Flex { kind: ka, rank: ra_r, rigid_name: None, .. }, Entry::Flex { kind: kb, rank: rb_r, rigid_name: None, .. }) => {
                if ka != kb {
                    self.report_mismatch(ra, rb, ErrorKind::Mismatch);
                    return false;
                }
                // Merge lower rank into representative; keep lower rank.
                let target = if ra_r <= rb_r { ra } else { rb };
                let other = if target == ra { rb } else { ra };
                self.set_link(other, target);
                self.stats.unifications += 1;
                true
            }
            (Entry::Flex { rigid_name: Some(n), .. }, other) => {
                if let Entry::Flex { rigid_name: Some(n2), .. } = other {
                    if n == n2 {
                        self.set_link(rb, ra);
                        return true;
                    }
                }
                self.report_rigid(ra, rb, &n);
                false
            }
            (other, Entry::Flex { rigid_name: Some(n), .. }) => {
                let _ = other;
                self.report_rigid(rb, ra, &n);
                false
            }
            (Entry::Flex { .. }, Entry::Concrete(c)) => {
                if self.occurs(ra, &c) {
                    self.report_occurs(ra, rb);
                    return false;
                }
                self.set_link(ra, rb);
                self.stats.unifications += 1;
                true
            }
            (Entry::Concrete(c), Entry::Flex { .. }) => {
                if self.occurs(rb, &c) {
                    self.report_occurs(rb, ra);
                    return false;
                }
                self.set_link(rb, ra);
                self.stats.unifications += 1;
                true
            }
            (Entry::Concrete(ca), Entry::Concrete(cb)) => self.unify_concrete(ra, ca, rb, cb),
            (Entry::Link(_), _) | (_, Entry::Link(_)) => unreachable!(),
        }
    }

    fn occurs(&self, var: NodeIdT, c: &ConcreteDesc) -> bool {
        // DFS over concrete term structure. A node previously visited *as a
        // concrete term* is skipped, but the target flex variable itself is
        // always detected even if it was already seen through a link (that is
        // exactly the self-reference we must reject).
        let mut stack: Vec<NodeIdT> = Vec::new();
        let push_term = |t: NodeIdT, stack: &mut Vec<NodeIdT>| stack.push(t);
        match c {
            ConcreteDesc::Con(_) => return false,
            ConcreteDesc::Arrow(a, b) => {
                stack.push(*a);
                stack.push(*b);
            }
            ConcreteDesc::Row { fields, tail, .. } => {
                for (_, t) in fields {
                    stack.push(*t);
                }
                if let Tail::Open(r) = tail {
                    stack.push(*r);
                }
            }
        }
        let mut visited_concrete: HashSet<NodeIdT> = HashSet::new();
        while let Some(x) = stack.pop() {
            let mut cur = x;
            loop {
                match &self.entries[cur.0 as usize] {
                    Some(Entry::Link(l)) => cur = *l,
                    _ => break,
                }
            }
            if cur == var {
                return true;
            }
            match &self.entries[cur.0 as usize] {
                Some(Entry::Concrete(conc)) => {
                    if !visited_concrete.insert(cur) {
                        continue;
                    }
                    match conc {
                        ConcreteDesc::Arrow(a, b) => {
                            push_term(*a, &mut stack);
                            push_term(*b, &mut stack);
                        }
                        ConcreteDesc::Row { fields, tail, .. } => {
                            for (_, t) in fields {
                                push_term(*t, &mut stack);
                            }
                            if let Tail::Open(r) = tail {
                                push_term(*r, &mut stack);
                            }
                        }
                        ConcreteDesc::Con(_) => {}
                    }
                }
                Some(Entry::Flex { .. }) | None => {}
                Some(Entry::Link(_)) => unreachable!(),
            }
        }
        false
    }

    fn unify_concrete(&mut self, ra: NodeIdT, ca: ConcreteDesc, rb: NodeIdT, cb: ConcreteDesc) -> bool {
        match (ca, cb) {
            (ConcreteDesc::Con(x), ConcreteDesc::Con(y)) => {
                if x == y {
                    self.set_link(rb, ra);
                    self.stats.unifications += 1;
                    true
                } else {
                    self.report_mismatch(ra, rb, ErrorKind::Mismatch);
                    false
                }
            }
            (ConcreteDesc::Arrow(a1, b1), ConcreteDesc::Arrow(a2, b2)) => {
                if !self.unify(a1, a2) {
                    self.report_mismatch(ra, rb, ErrorKind::Arity);
                    return false;
                }
                if !self.unify(b1, b2) {
                    return false;
                }
                self.set_link(rb, ra);
                self.stats.unifications += 1;
                true
            }
            (
                ConcreteDesc::Row { sort: sa, fields: fa, tail: ta },
                ConcreteDesc::Row { sort: sb, fields: fb, tail: tb },
            ) => {
                if sa != sb {
                    self.report_mismatch(ra, rb, ErrorKind::Mismatch);
                    return false;
                }
                self.unify_rows(sa, fa, ta, fb, tb)
            }
            _ => {
                self.report_mismatch(ra, rb, ErrorKind::Mismatch);
                false
            }
        }
    }
}

impl<'a> Solver<'a> {
    fn unify_rows(
        &mut self,
        sort: RowSort,
        fa: Vec<(String, NodeIdT)>,
        ta: Tail,
        fb: Vec<(String, NodeIdT)>,
        tb: Tail,
    ) -> bool {
        // Flatten each side by chasing open flex tails (only through concrete
        // rows); return the residual tail.
        let mut a_fields: Vec<(String, NodeIdT)> = Vec::new();
        let mut a_seen: HashSet<String> = HashSet::new();
        let mut a_dup: Option<String> = None;
        let mut ta_cur = ta;
        for (l, t) in &fa {
            if !a_seen.insert(l.clone()) {
                a_dup = Some(l.clone());
            }
            a_fields.push((l.clone(), *t));
        }
        let ta_res = self.chase_tail(&mut a_fields, &mut a_seen, &mut a_dup, ta_cur);

        let mut b_fields: Vec<(String, NodeIdT)> = Vec::new();
        let mut b_seen: HashSet<String> = HashSet::new();
        let mut b_dup: Option<String> = None;
        for (l, t) in &fb {
            if !b_seen.insert(l.clone()) {
                b_dup = Some(l.clone());
            }
            b_fields.push((l.clone(), *t));
        }
        let tb_res = self.chase_tail(&mut b_fields, &mut b_seen, &mut b_dup, tb);

        if let Some(d) = a_dup.or(b_dup) {
            self.report_duplicate(sort, d);
            return false;
        }

        // Build label maps; sort deterministically.
        let mut am: BTreeMap<String, NodeIdT> = BTreeMap::new();
        for (l, t) in a_fields {
            am.insert(l, t);
        }
        let mut bm: BTreeMap<String, NodeIdT> = BTreeMap::new();
        for (l, t) in b_fields {
            bm.insert(l, t);
        }
        let labels: BTreeSet<String> = am.keys().chain(bm.keys()).cloned().collect();
        let mut missing: Vec<String> = Vec::new();
        let mut extra: Vec<String> = Vec::new();
        let mut all_ok = true;
        // Side A = requirement, side B = concrete actual.
        let a_closed = matches!(ta_res, Tail::Closed);
        let b_closed = matches!(tb_res, Tail::Closed);
        for l in &labels {
            match (am.get(l), bm.get(l)) {
                (Some(x), Some(y)) => {
                    if !self.unify(*x, *y) {
                        all_ok = false;
                    }
                }
                (Some(_), None) => {
                    if b_closed {
                        missing.push(l.clone());
                        all_ok = false;
                    }
                }
                (None, Some(_)) => {
                    if a_closed {
                        extra.push(l.clone());
                        all_ok = false;
                    }
                }
                (None, None) => unreachable!(),
            }
        }
        // Residual tails.
        let tail_ok = match (ta_res, tb_res) {
            (Tail::Closed, Tail::Closed) => true,
            (Tail::Open(r), Tail::Closed) => {
                // Excess labels of B are forbidden: bind r to closed row.
                let row = self.make_empty_concrete(sort);
                self.unify(r, row)
            }
            (Tail::Closed, Tail::Open(r)) => {
                let row = self.make_empty_concrete(sort);
                self.unify(row, r)
            }
            (Tail::Open(x), Tail::Open(y)) => self.unify(x, y),
        };
        if !tail_ok {
            all_ok = false;
        }
        if !missing.is_empty() || !extra.is_empty() {
            self.report_row(sort, missing, extra, a_closed, b_closed);
            return false;
        }
        if !all_ok {
            return false;
        }
        self.stats.unifications += 1;
        true
    }

    fn chase_tail(
        &self,
        fields: &mut Vec<(String, NodeIdT)>,
        seen: &mut HashSet<String>,
        dup: &mut Option<String>,
        tail: Tail,
    ) -> Tail {
        let mut cur = tail;
        loop {
            match cur {
                Tail::Closed | Tail::Open(_) => {}
            }
            match cur {
                Tail::Closed => return Tail::Closed,
                Tail::Open(r) => {
                    let mut root = r;
                    loop {
                        match &self.entries[root.0 as usize] {
                            Some(Entry::Link(l)) => root = *l,
                            _ => break,
                        }
                    }
                    match &self.entries[root.0 as usize] {
                        Some(Entry::Concrete(ConcreteDesc::Row { fields: more, tail: nt, .. })) => {
                            for (l, t) in more {
                                if !seen.insert(l.clone()) {
                                    *dup = Some(l.clone());
                                }
                                fields.push((l.clone(), *t));
                            }
                            cur = *nt;
                        }
                        Some(Entry::Flex { kind: FlexKind::Row, .. }) => return Tail::Open(root),
                        _ => return Tail::Open(root),
                    }
                }
            }
        }
    }

    fn make_empty_concrete(&mut self, sort: RowSort) -> NodeIdT {
        let id = NodeIdT(self.next_dynamic);
        self.next_dynamic += 1;
        if (id.0 as usize) >= self.entries.len() {
            self.entries.resize(id.0 as usize + 16, None);
        }
        self.entries[id.0 as usize] = Some(Entry::Concrete(ConcreteDesc::Row {
            sort,
            fields: Vec::new(),
            tail: Tail::Closed,
        }));
        id
    }

    fn unify_empty_row(&mut self, row: NodeIdT, sort: &'static str) -> bool {
        let root = self.find(row);
        let srt = match sort {
            "variant" => RowSort::Variant,
            _ => RowSort::Record,
        };
        match &self.entries[root.0 as usize] {
            Some(Entry::Flex { kind: FlexKind::Row, .. }) => {
                let empty = self.make_empty_concrete(srt);
                self.unify(root, empty)
            }
            Some(Entry::Concrete(ConcreteDesc::Row { fields, tail, .. })) => {
                let mut flds: Vec<(String, NodeIdT)> = fields.clone();
                let mut seen: HashSet<String> = fields.iter().map(|(l, _)| l.clone()).collect();
                let mut dup = None;
                let res = self.chase_tail(&mut flds, &mut seen, &mut dup, *tail);
                if let Some(d) = dup {
                    self.report_duplicate(srt, d);
                    return false;
                }
                if flds.is_empty() && matches!(res, Tail::Closed) {
                    true
                } else {
                    let labels: Vec<String> = flds.iter().map(|(l, _)| l.clone()).collect();
                    self.report_row(srt, Vec::new(), labels, true, true);
                    false
                }
            }
            _ => true,
        }
    }
}

impl<'a> Solver<'a> {
    fn report_mismatch(&mut self, a: NodeIdT, b: NodeIdT, kind: ErrorKind) {
        let ta = self.snapshot_term(a);
        let tb = self.snapshot_term(b);
        let cids = self.blame_chain();
        let spans = self.blame_spans();
        let kind_name = match kind {
            ErrorKind::Arity => "arrow-arity",
            _ => "mismatch",
        };
        let sig = self.signature(
            match kind { ErrorKind::Arity => "arity", _ => "mismatch" },
            &cids,
            &[&render(&ta), &render(&tb)],
        );
        if self.seen_signatures.insert(sig.clone()) {
            let id = self.errors.len() as u32;
            self.errors.push(TypeError {
                id,
                kind,
                message: format!("无法统一 {} 与 {}", render(&ta), render(&tb)),
                spans,
                cids,
                missing: Vec::new(),
                extra: Vec::new(),
                sort: "type",
                signature: sig,
            });
            self.record_failure(Some(id), kind_name);
        } else {
            self.record_failure(None, kind_name);
        }
    }

    fn report_occurs(&mut self, var: NodeIdT, inside: NodeIdT) {
        let tv = self.snapshot_term(var);
        let ti = self.snapshot_term(inside);
        let cids = self.blame_chain();
        let spans = self.blame_spans();
        let sig = self.signature("occurs", &cids, &[&render(&tv), &render(&ti)]);
        if self.seen_signatures.insert(sig.clone()) {
            let id = self.errors.len() as u32;
            self.errors.push(TypeError {
                id,
                kind: ErrorKind::Occurs,
                message: format!(
                    "无限类型：{} 出现在 {} 内部（occurs-check 失败）",
                    render(&tv),
                    render(&ti)
                ),
                spans,
                cids,
                missing: Vec::new(),
                extra: Vec::new(),
                sort: "type",
                signature: sig,
            });
            self.record_failure(Some(id), "occurs");
        } else {
            self.record_failure(None, "occurs");
        }
    }

    fn report_rigid(&mut self, rigid: NodeIdT, other: NodeIdT, name: &str) {
        let tr = self.snapshot_term(rigid);
        let to = self.snapshot_term(other);
        let cids = self.blame_chain();
        let spans = self.blame_spans();
        let sig = self.signature("rigid", &cids, &[name, &render(&to)]);
        if self.seen_signatures.insert(sig.clone()) {
            let id = self.errors.len() as u32;
            self.errors.push(TypeError {
                id,
                kind: ErrorKind::Rigid,
                message: format!(
                    "无法将具体类型 {} 赋给刚性类型变量 {}（注解无法满足）",
                    render(&to),
                    render(&tr)
                ),
                spans,
                cids,
                missing: Vec::new(),
                extra: Vec::new(),
                sort: "type",
                signature: sig,
            });
            self.record_failure(Some(id), "rigid");
        } else {
            self.record_failure(None, "rigid");
        }
    }

    fn report_duplicate(&mut self, sort: RowSort, label: String) {
        let cids = self.blame_chain();
        let spans = self.blame_spans();
        let sig = self.signature("duplicate", &cids, &[sort.word(), label.as_str()]);
        if self.seen_signatures.insert(sig.clone()) {
            let id = self.errors.len() as u32;
            self.errors.push(TypeError {
                id,
                kind: ErrorKind::FieldDuplicate,
                message: format!("{} 中字段/构造子 `{}` 重复", kind_word(sort), label),
                spans,
                cids,
                missing: Vec::new(),
                extra: vec![label],
                sort: sort.word(),
                signature: sig,
            });
            self.record_failure(Some(id), "duplicate");
        } else {
            self.record_failure(None, "duplicate");
        }
    }

    fn report_row(
        &mut self,
        sort: RowSort,
        missing: Vec<String>,
        extra: Vec<String>,
        _a_closed: bool,
        _b_closed: bool,
    ) {
        let cids = self.blame_chain();
        let spans = self.blame_spans();
        let mut key = Vec::new();
        key.push(sort.word().to_string());
        key.push("M".to_string() + &missing.join(","));
        key.push("E".to_string() + &extra.join(","));
        let extras: Vec<&str> = key.iter().map(|s| s.as_str()).collect();
        let sig = self.signature("row", &cids, &extras);
        if self.seen_signatures.insert(sig.clone()) {
            let mut parts = Vec::new();
            if !missing.is_empty() {
                parts.push(format!("缺少 {} `{}`", kind_word(sort), missing.join("`, `")));
            }
            if !extra.is_empty() {
                parts.push(format!("多出 {} `{}`", kind_word(sort), extra.join("`, `")));
            }
            let id = self.errors.len() as u32;
            self.errors.push(TypeError {
                id,
                kind: ErrorKind::RowField,
                message: format!("{}不匹配：{}", kind_word(sort), parts.join("；")),
                spans,
                cids,
                missing,
                extra,
                sort: sort.word(),
                signature: sig,
            });
            self.record_failure(Some(id), "row");
        } else {
            self.record_failure(None, "row");
        }
    }

    fn signature(&self, tag: &str, cids: &[CId], extra: &[&str]) -> String {
        let mut cs: Vec<u32> = cids.iter().map(|c| c.0).collect();
        cs.sort_unstable();
        cs.dedup();
        format!("{}:{}:{}", tag, cs.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","), extra.join("|"))
    }

    fn blame_chain(&self) -> Vec<CId> {
        let mut v: Vec<CId> = self.blame.clone();
        v.sort();
        v.dedup();
        v
    }

    fn blame_spans(&self) -> Vec<(usize, usize)> {
        let mut spans = Vec::new();
        for cid in &self.blame {
            if let Some(n) = self.graph.nodes.iter().find(|n| n.id == *cid) {
                if n.span.end > n.span.start {
                    spans.push((n.span.start, n.span.end));
                }
            }
        }
        spans.sort();
        spans.dedup();
        spans
    }
}

fn kind_word(s: RowSort) -> &'static str {
    match s {
        RowSort::Record => "字段",
        RowSort::Variant => "分支",
    }
}

impl<'a> Solver<'a> {
    fn record_failure(&mut self, error_id: Option<u32>, _tag: &str) {
        self.stats.errors += 1;
        let active: Vec<u32> = self.errors.iter().map(|e| e.id).collect();
        let snaps = self.all_snapshots();
        if let Some(step) = self.steps.last_mut() {
            step.ok = false;
            step.error_id = error_id.or(step.error_id);
            step.active_errors = active;
            step.vars = snaps;
        }
    }

    fn log_unify(&mut self, cid: CId, label: &str, ok: bool) {
        if !self.record_steps {
            return;
        }
        let vars = self.all_snapshots();
        self.steps.push(StepRecord {
            index: self.steps.len(),
            kind: "unify".into(),
            cid: Some(cid.0),
            label: label.into(),
            detail: serde_json::json!({}),
            ok,
            error_id: self.last_error_id_for(cid),
            vars,
            active_errors: self.errors.iter().map(|e| e.id).collect(),
        });
    }

    fn log_empty(&mut self, cid: CId, sort: &str, ok: bool) {
        if !self.record_steps {
            return;
        }
        self.steps.push(StepRecord {
            index: self.steps.len(),
            kind: "empty_row".into(),
            cid: Some(cid.0),
            label: format!("闭合 {} 行", if sort == "variant" { "variant" } else { "record" }),
            detail: serde_json::json!({}),
            ok,
            error_id: self.last_error_id_for(cid),
            vars: self.all_snapshots(),
            active_errors: self.errors.iter().map(|e| e.id).collect(),
        });
    }

    fn log_skip(&mut self, cid: CId, node: &ConstraintNode) {
        if !self.record_steps {
            return;
        }
        let label = match &node.kind {
            CKind::Eq(e) => format!("已禁用：{}", e.label),
            CKind::Inst(i) => format!("已禁用：实例化 `{}`", i.name),
            CKind::Gen(g) => format!("已禁用：泛化 `{}`", g.name),
            CKind::EmptyRow(_) => "已禁用：闭合行".into(),
            CKind::Block => return,
            CKind::Marker => return,
        };
        self.steps.push(StepRecord {
            index: self.steps.len(),
            kind: "skip".into(),
            cid: Some(cid.0),
            label,
            detail: serde_json::json!({}),
            ok: true,
            error_id: None,
            vars: self.all_snapshots(),
            active_errors: self.errors.iter().map(|e| e.id).collect(),
        });
    }

    fn log_step(
        &mut self,
        kind: &str,
        cid: Option<CId>,
        label: String,
        detail: serde_json::Value,
        ok: bool,
    ) {
        if !self.record_steps {
            return;
        }
        self.steps.push(StepRecord {
            index: self.steps.len(),
            kind: kind.into(),
            cid: cid.map(|c| c.0),
            label,
            detail,
            ok,
            error_id: None,
            vars: self.all_snapshots(),
            active_errors: self.errors.iter().map(|e| e.id).collect(),
        });
    }

    fn log_simple(&mut self, cid: CId, kind: &str, label: &str, ok: bool) {
        self.log_step(kind, Some(cid), label.into(), serde_json::json!({}), ok);
    }

    fn last_error_id_for(&self, cid: CId) -> Option<u32> {
        self.errors
            .iter()
            .rev()
            .find(|e| e.cids.contains(&cid))
            .map(|e| e.id)
    }

    fn all_snapshots(&self) -> Vec<VarSnapshot> {
        let mut out = Vec::new();
        for (idx, entry) in self.entries.iter().enumerate() {
            let id = idx as u32;
            if let Some(e) = entry {
                let term = self.snapshot_term(NodeIdT(id));
                let (rank, origin) = match e {
                    Entry::Flex { rank, origin_expr, .. } => (*rank, *origin_expr),
                    _ => (0, None),
                };
                out.push(VarSnapshot {
                    id,
                    root: self.find_readonly(NodeIdT(id)).0,
                    term,
                    rank,
                    origin_expr: origin,
                });
            }
        }
        out
    }

    fn find_readonly(&self, mut v: NodeIdT) -> NodeIdT {
        loop {
            match &self.entries[v.0 as usize] {
                Some(Entry::Link(t)) => v = *t,
                _ => return v,
            }
        }
    }

    fn snapshot_term(&self, v: NodeIdT) -> TermF {
        let mut names: HashMap<u32, String> = HashMap::new();
        let mut counters: HashMap<char, u32> = HashMap::new();
        let root = self.find_readonly(v);
        // Assign pretty names only to flex vars reachable.
        self.assign_names(root, &mut names, &mut counters);
        self.build_term(root, &names)
    }

    fn assign_names(
        &self,
        v: NodeIdT,
        names: &mut HashMap<u32, String>,
        counters: &mut HashMap<char, u32>,
    ) {
        let root = self.find_readonly(v);
        if names.contains_key(&root.0) {
            return;
        }
        match &self.entries[root.0 as usize] {
            Some(Entry::Flex { kind, hint, .. }) => {
                let name = if !hint.is_empty() {
                    let key = hint.chars().next().unwrap_or('t');
                    let n = counters.entry(key).or_insert(0);
                    let nm = format!("{}{}", key, *n);
                    *n += 1;
                    nm
                } else if *kind == FlexKind::Row {
                    let n = counters.entry('r').or_insert(0);
                    let nm = format!("r{}", *n);
                    *n += 1;
                    nm
                } else {
                    let n = counters.entry('t').or_insert(0);
                    let nm = format!("t{}", *n);
                    *n += 1;
                    nm
                };
                names.insert(root.0, name);
            }
            Some(Entry::Concrete(ConcreteDesc::Arrow(a, b))) => {
                names.insert(root.0, String::new());
                self.assign_names(*a, names, counters);
                self.assign_names(*b, names, counters);
            }
            Some(Entry::Concrete(ConcreteDesc::Row { fields, tail, .. })) => {
                names.insert(root.0, String::new());
                for (_, t) in fields {
                    self.assign_names(*t, names, counters);
                }
                if let Tail::Open(r) = tail {
                    self.assign_names(*r, names, counters);
                }
            }
            Some(Entry::Concrete(ConcreteDesc::Con(_))) => {
                names.insert(root.0, String::new());
            }
            None => {
                names.insert(root.0, format!("?{}", root.0));
            }
            Some(Entry::Link(_)) => unreachable!(),
        }
    }

    fn build_term(&self, v: NodeIdT, names: &HashMap<u32, String>) -> TermF {
        let root = self.find_readonly(v);
        let default_name = names.get(&root.0).cloned().unwrap_or_else(|| format!("v{}", root.0));
        let kind = match &self.entries[root.0 as usize] {
            Some(Entry::Flex { kind, hint, rigid_name, .. }) => match rigid_name {
                Some(n) => TermKind::Rigid(n.clone()),
                None => TermKind::Flex { kind: *kind, hint: hint.clone() },
            },
            Some(Entry::Concrete(ConcreteDesc::Con(n))) => TermKind::Con(n.clone()),
            Some(Entry::Concrete(ConcreteDesc::Arrow(a, b))) => {
                TermKind::Arrow(Box::new(self.build_term(*a, names)), Box::new(self.build_term(*b, names)))
            }
            Some(Entry::Concrete(ConcreteDesc::Row { sort, fields, tail })) => {
                let mut fs: Vec<RowFieldF> = fields
                    .iter()
                    .map(|(l, t)| RowFieldF { label: l.clone(), ty: Box::new(self.build_term(*t, names)) })
                    .collect();
                fs.sort_by(|a, b| a.label.cmp(&b.label));
                let tf = match tail {
                    Tail::Closed => TailF::Closed,
                    Tail::Open(r) => {
                        let rr = self.find_readonly(*r);
                        TailF::Open(rr.0)
                    }
                };
                match sort {
                    RowSort::Record => TermKind::Record(fs, tf),
                    RowSort::Variant => TermKind::Variant(fs, tf),
                }
            }
            Some(Entry::Link(_)) => unreachable!(),
            None => TermKind::Flex { kind: FlexKind::Type, hint: String::new() },
        };
        TermF { root: root.0, kind, name: default_name }
    }
}
