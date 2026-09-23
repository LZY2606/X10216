use crate::ast::*;
use crate::types::*;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, PartialOrd, Ord)]
pub struct CId(pub u64);

/// 求解器中登记的行描述符（生成期初值）。
#[derive(Clone, Debug, Serialize, PartialEq)]
pub enum RowInit {
    Closed,
    Open,
    Fields(Vec<(String, TV)>, RV),
}

#[derive(Clone, Debug, Serialize)]
pub enum Side {
    Op,
    Record,
    Variant,
    MatchArm(usize),
    Other,
}

#[derive(Clone, Debug, Serialize)]
pub enum CNode {
    Conj { children: Vec<CId> },
    Eq { a: Ty, b: Ty, pid: Pid, detail: String, side: Side },
    Inst { target: TV, frame: CId, use_pid: Pid },
    LetFrame {
        var_name: String,
        val_pid: Pid,
        val_ty: TV,
        is_value: bool,
        rec: bool,
        binding: CId,
        body: CId,
    },
}

#[derive(Clone, Copy, Debug, Serialize, PartialEq, Eq)]
pub enum TVOrigin {
    Node(Pid),
    Param(Pid),
    Inst(CId),
    Tmp,
}

#[derive(Clone, Copy, Debug, Serialize, PartialEq, Eq)]
pub enum RowOrigin {
    RecordLit(Pid),
    Field(Pid),
    Match(Pid),
    Tag(Pid),
    Inst(CId),
    Tmp,
}

#[derive(Clone, Debug, Serialize)]
pub struct ConRef {
    pub cid: CId,
    pub name: String,
    pub kind: &'static str,
    pub pid: Option<Pid>,
    pub detail: String,
    pub enabled: bool,
}

#[derive(Clone, Debug, Serialize)]
pub struct ScopeMark {
    pub var_name: String,
    pub frame: CId,
    pub bind_index: u32,
}

#[derive(Clone, Debug, Serialize)]
pub struct UnboundVar {
    pub name: String,
    pub pid: Pid,
    pub cid: CId,
}

#[derive(Clone, Debug)]
pub struct GenResult {
    pub root_c: CId,
    pub nodes: HashMap<CId, CNode>,
    pub order: Vec<CId>,
    pub cons: Vec<ConRef>,
    pub n_tv: u32,
    pub n_rv: u32,
    pub tv_origin: Vec<TVOrigin>,
    pub rv_origin: Vec<RowOrigin>,
    pub rows: Vec<RowInit>,
    pub node_ty: HashMap<Pid, TV>,
    pub scope_path: HashMap<CId, Vec<ScopeMark>>,
    pub root_tv: TV,
    pub genv_fp: HashMap<CId, u64>,
    pub frame_bindings: HashMap<CId, (Pid, String, u32)>,
    pub unbound: Vec<UnboundVar>,
}

enum EntryKind {
    Mono(TV),
    Poly(CId, u32),
}

struct EnvEntry {
    name: String,
    frame: CId,
    bind_index: u32,
    kind: EntryKind,
}

pub struct G {
    nodes: HashMap<CId, CNode>,
    order: Vec<CId>,
    cons: Vec<ConRef>,
    n_tv: u32,
    n_rv: u32,
    tv_origin: Vec<TVOrigin>,
    rv_origin: Vec<RowOrigin>,
    rows: Vec<RowInit>,
    node_ty: HashMap<Pid, TV>,
    scope_path: HashMap<CId, Vec<ScopeMark>>,
    genv_fp: HashMap<CId, u64>,
    frame_bindings: HashMap<CId, (Pid, String, u32)>,
    unbound: Vec<UnboundVar>,
    env: Vec<EnvEntry>,
    ast: HashMap<Pid, Node>,
    bind_counter: u32,
    local_seq: HashMap<(Pid, u32), u32>,
}

impl G {
    fn new(ast: HashMap<Pid, Node>) -> Self {
        G {
            nodes: HashMap::new(),
            order: Vec::new(),
            cons: Vec::new(),
            n_tv: 0,
            n_rv: 0,
            tv_origin: Vec::new(),
            rv_origin: Vec::new(),
            rows: Vec::new(),
            node_ty: HashMap::new(),
            scope_path: HashMap::new(),
            genv_fp: HashMap::new(),
            frame_bindings: HashMap::new(),
            unbound: Vec::new(),
            env: Vec::new(),
            ast,
            bind_counter: 0,
            local_seq: HashMap::new(),
        }
    }

    fn fresh_tv(&mut self, o: TVOrigin) -> TV {
        let id = self.n_tv;
        self.n_tv += 1;
        self.tv_origin.push(o);
        TV(id)
    }

    fn fresh_rv(&mut self, o: RowOrigin) -> RV {
        let id = self.n_rv;
        self.n_rv += 1;
        self.rv_origin.push(o);
        RV(id)
    }

    fn mk_row(&mut self, init: RowInit, o: RowOrigin) -> RV {
        let rv = self.fresh_rv(o);
        self.rows.push(init);
        debug_assert_eq!(self.rows.len() as u32 - 1, rv.0);
        rv
    }

    fn seq(&mut self, pid: Pid, role: u32) -> u32 {
        let e = self.local_seq.entry((pid, role)).or_insert(0);
        let v = *e;
        *e += 1;
        v
    }

    /// 确定式约束 id：仅依赖 (pid, 角色, 本地序号)；变量改名不改变它。
    fn cid(&mut self, pid: Pid, role: u32) -> CId {
        let s = self.seq(pid, role);
        let mut x = pid
            .0
            .wrapping_mul(0x9E3779B97F4A7C15)
            .wrapping_add((role as u64) << 40 | (s as u64) << 8 | 0xC7);
        x ^= x >> 33;
        x = x.wrapping_mul(0xFF51AFD7ED558CCD);
        x ^= x >> 33;
        CId(x)
    }

    fn frame_cid(&mut self, pid: Pid) -> CId {
        let mut x = pid.0.wrapping_mul(0xD6E8FEB86659FD93).wrapping_add(0xF1A5);
        x ^= x >> 29;
        x = x.wrapping_mul(0x94D049BB133111EB);
        CId(x)
    }

    fn add_node(&mut self, cid: CId, node: CNode) {
        self.nodes.insert(cid, node);
    }

    fn scope_fingerprint(&self) -> u64 {
        let mut x = 0xcbf29ce484222325u64;
        for e in &self.env {
            // 绑定身份 = let 节点 pid 与绑定序号；不含显示名
            let key = match e.kind {
                EntryKind::Mono(tv) => frame_key(e.frame, e.bind_index) ^ (tv.0 as u64).wrapping_mul(0x9e37),
                EntryKind::Poly(_, _) => frame_key(e.frame, e.bind_index),
            };
            x ^= key;
            x = x.wrapping_mul(0x100000001b3);
        }
        x ^= x >> 31;
        x
    }

    fn record_leaf(&mut self, cid: CId, cr: ConRef) {
        self.genv_fp.insert(cid, self.scope_fingerprint());
        let marks = self
            .env
            .iter()
            .map(|e| ScopeMark { var_name: e.name.clone(), frame: e.frame, bind_index: e.bind_index })
            .collect();
        self.scope_path.insert(cid, marks);
        self.order.push(cid);
        self.cons.push(cr);
    }

    fn lookup(&self, name: &str) -> Option<&EnvEntry> {
        self.env.iter().rev().find(|e| e.name == name)
    }

    fn conj(&mut self, pid: Pid, children: Vec<CId>) -> CId {
        if children.len() == 1 {
            children[0]
        } else {
            let cid = self.cid(pid, 99);
            self.add_node(cid, CNode::Conj { children });
            cid
        }
    }

    fn eqc(&mut self, pid: Pid, role: u32, a: Ty, b: Ty, detail: String, side: Side) -> CId {
        let cid = self.cid(pid, role);
        self.add_node(cid, CNode::Eq { a, b, pid, detail: detail.clone(), side });
        self.record_leaf(
            cid,
            ConRef { cid, name: format!("eq:{}", detail), kind: "eq", pid: Some(pid), detail, enabled: true },
        );
        cid
    }

    fn expr_tv(&mut self, p: Pid) -> TV {
        if let Some(t) = self.node_ty.get(&p) {
            return *t;
        }
        let tv = self.fresh_tv(TVOrigin::Node(p));
        self.node_ty.insert(p, tv);
        tv
    }
}

fn frame_key(frame: CId, bind_index: u32) -> u64 {
    frame.0.wrapping_mul(0x100000001b3).wrapping_add(bind_index as u64)
}

fn lit_ty(l: Lit) -> Ty {
    match l {
        Lit::Int(_) => Ty::Int,
        Lit::Str => Ty::Str,
        Lit::Bool(_) => Ty::Bool,
        Lit::Unit => Ty::Unit,
    }
}

/// 语法值判定（value restriction）。
pub fn is_value(ast: &HashMap<Pid, Node>, p: Pid) -> bool {
    match &ast[&p].expr {
        ExprE::Lit(_) | ExprE::Fun { .. } | ExprE::Tag { .. } => true,
        ExprE::Record { fields } => fields.iter().all(|(_, fp)| is_value(ast, *fp)),
        _ => false,
    }
}

pub fn generate(ast: HashMap<Pid, Node>, root: Pid) -> GenResult {
    let mut g = G::new(ast);
    let root_tv = g.expr_tv(root);
    let inner = gen_expr(&mut g, root);
    let eq = g.eqc(
        root,
        1,
        Ty::Var(root_tv),
        Ty::Var(g.node_ty[&root]),
        "整个程序的类型".into(),
        Side::Other,
    );
    let root_c = g.conj(root, vec![inner, eq]);
    GenResult {
        root_c,
        nodes: g.nodes,
        order: g.order,
        cons: g.cons,
        n_tv: g.n_tv,
        n_rv: g.n_rv,
        tv_origin: g.tv_origin,
        rv_origin: g.rv_origin,
        rows: g.rows,
        node_ty: g.node_ty,
        scope_path: g.scope_path,
        root_tv,
        genv_fp: g.genv_fp,
        frame_bindings: g.frame_bindings,
        unbound: g.unbound,
    }
}
