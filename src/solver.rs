//! 约束求解：带 proof 集合的统一、occurs check、行多态、泛化与 value restriction。

use crate::constraint::{Cid, CInfo, Constraint};
use crate::types::*;
use std::collections::{BTreeMap, BTreeSet, HashMap};

/// 行类型种类（记录/变体），用于错误与行合并。
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RowKind {
    Record,
    Variant,
}

impl RowKind {
    pub fn name(self) -> &'static str {
        match self {
            RowKind::Record => "记录",
            RowKind::Variant => "变体",
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeError {
    pub kind: String,
    pub message: String,
    pub detail: String,
    pub span: Option<[u32; 2]>,
    pub nodes: BTreeSet<u64>,
    /// 造成该错误不可满足的约束核心（已最小化前的初始核心）。
    pub core: BTreeSet<Cid>,
}

#[derive(Clone, Debug)]
struct NodeData {
    rank: u32,
    /// union-find 父节点；根节点 parent == 自身。
    parent: u32,
    /// 根节点绑定的结构（未绑定时为 None）。
    term: Option<TypeTerm>,
    proof: Proof,
    origin: Option<Cid>,
    /// 是否为行尾变量（用于 occurs 与渲染）。
    is_row: bool,
}

#[derive(Clone, Debug)]
pub struct LinkEffect {
    pub var: u32,
    pub target: u32,
}

#[derive(Clone, Debug)]
pub struct StepSnapshot {
    pub bindings: Vec<BindingSnapshot>,
    pub pending: Vec<PendingTy>,
}

#[derive(Clone, Debug)]
pub struct BindingSnapshot {
    pub name: String,
    pub poly: bool,
    pub weak: bool,
    pub ty: RTy,
}

#[derive(Clone, Debug)]
pub struct PendingTy {
    pub node: u64,
    pub ty: RTy,
}

#[derive(Clone, Debug)]
pub struct Step {
    pub index: usize,
    pub cid: Cid,
    pub action: String,
    pub detail: String,
    pub effects: Vec<StepEffect>,
    pub error: Option<usize>,
    pub snapshot: StepSnapshot,
}

#[derive(Clone, Debug)]
pub enum StepEffect {
    Link { var: String, to: String },
    Bind { var: String, term: String },
    Instantiate { names: Vec<(String, String)> },
    Generalize {
        name: String,
        quantified: Vec<String>,
        weak: Vec<String>,
    },
    Skip { reason: String },
}

#[derive(Clone)]
struct GenInfo {
    root: u32,
    depth: u32,
    non_exp: bool,
    rec: bool,
    rec_vars: Vec<u32>,
    name: String,
    node: u64,
}

pub struct Solver<'a> {
    nodes: Vec<NodeData>,
    infos: &'a [CInfo],
    disabled: &'a BTreeSet<Cid>,
    origins: HashMap<u32, Cid>,
    /// 按出现顺序命名未绑定变量。
    var_names: HashMap<u32, String>,
    next_var_name: usize,
    row_var_names: HashMap<u32, String>,
    next_row_name: usize,
    /// value_node -> 最新 GenInfo（Inst 使用）。
    gens: HashMap<u64, GenInfo>,
    /// Gen 已计算的方案（value_node -> scheme）。
    schemes: HashMap<u64, Scheme>,
    /// 当前词法可见的绑定（步骤快照用）：name -> 信息。
    live_bindings: Vec<(String, GenInfo)>,
    pub steps: Vec<Step>,
    pub errors: Vec<TypeError>,
    /// 每一步：被跳过（禁用）的约束集合。
    pub skipped: BTreeSet<Cid>,
    current: Option<Cid>,
    /// 当前约束引入的统一证明（除变量自身证明外）。
    cur_proof: Proof,
    top_roots: Vec<(String, u32, bool)>,
    node_ty: &'a [(u64, u32)],
}

impl<'a> Solver<'a> {
    pub fn new(
        infos: &'a [CInfo],
        origins: HashMap<u32, Cid>,
        node_ty: &'a [(u64, u32)],
        disabled: &'a BTreeSet<Cid>,
    ) -> Self {
        let maxv = infos
            .iter()
            .flat_map(|i| collect_vars(&i.cons))
            .chain(origins.keys().copied())
            .max()
            .map(|m| m + 1)
            .unwrap_or(0);
        let nodes = (0..maxv)
            .enumerate()
            .map(|(i, _)| NodeData {
                rank: 0,
                parent: i as u32,
                term: None,
                proof: BTreeSet::new(),
                origin: None,
                is_row: false,
            })
            .collect();
        Solver {
            nodes,
            infos,
            disabled,
            origins,
            var_names: HashMap::new(),
            next_var_name: 0,
            row_var_names: HashMap::new(),
            next_row_name: 0,
            gens: HashMap::new(),
            schemes: HashMap::new(),
            live_bindings: Vec::new(),
            steps: Vec::new(),
            errors: Vec::new(),
            skipped: BTreeSet::new(),
            current: None,
            cur_proof: BTreeSet::new(),
            top_roots: Vec::new(),
            node_ty,
        }
    }

    fn ensure(&mut self, v: u32) {
        while self.nodes.len() <= v as usize {
            let i = self.nodes.len() as u32;
            self.nodes.push(NodeData {
                rank: 0,
                parent: i,
                term: None,
                proof: BTreeSet::new(),
                origin: None,
                is_row: false,
            });
        }
    }

    fn mark_row(&mut self, v: u32) {
        self.ensure(v);
        self.nodes[v as usize].is_row = true;
    }

    fn find(&mut self, v: u32) -> u32 {
        self.ensure(v);
        // 迭代式路径压缩，并把证明合并到根。
        let mut cur = v;
        let mut path = Vec::new();
        loop {
            let parent = self.nodes[cur as usize].parent;
            if parent == cur {
                break;
            }
            path.push(cur);
            cur = parent;
        }
        if !path.is_empty() {
            let root = cur;
            let mut acc: Proof = self.nodes[root as usize].proof.clone();
            for &p in &path {
                acc = merge_proof(&acc, &self.nodes[p as usize].proof);
                if let Some(o) = self.nodes[p as usize].origin.clone() {
                    if self.nodes[root as usize].origin.is_none() {
                        self.nodes[root as usize].origin = Some(o);
                    }
                }
            }
            self.nodes[root as usize].proof = acc;
            for p in path {
                self.nodes[p as usize].parent = root;
            }
        }
        cur
    }

    fn var_proof(&mut self, v: u32) -> Proof {
        let r = self.find(v);
        let mut p = self.nodes[r as usize].proof.clone();
        if let Some(o) = self.origins.get(&r).cloned().or_else(|| {
            // origins 以原始变量为键；find 后可能换根。
            self.nodes[r as usize].origin.clone()
        }) {
            p.insert(o);
        }
        p
    }

    fn set_origin_if_absent(&mut self, v: u32, cid: &Cid) {
        self.ensure(v);
        if self.nodes[v as usize].origin.is_none() && !self.origins.contains_key(&v) {
            self.nodes[v as usize].origin = Some(cid.clone());
        }
    }

    fn current_cid(&self) -> Cid {
        self.current.clone().unwrap_or_default()
    }
}

fn collect_vars(c: &Constraint) -> Vec<u32> {
    let mut out = Vec::new();
    match c {
        Constraint::Con { term, target } => {
            term.walk_vars(&mut out);
            out.push(*target);
        }
        Constraint::Unify { left, right } => {
            out.push(*left);
            out.push(*right);
        }
        Constraint::Inst { scheme, target, .. } => {
            scheme.body.walk_vars(&mut out);
            out.push(*target);
        }
        Constraint::Gen { root, rec_vars, .. } => {
            out.push(*root);
            out.extend(rec_vars.iter().copied());
        }
        Constraint::Error { .. } => {}
    }
    out
}
