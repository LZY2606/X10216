// 类型内核：类型变量、行多态的 RowBind、并查集、来源追踪。
// 不依赖求解器逻辑，可被约束生成与求解器共同使用。
use crate::lexer::Span;
use serde::Serialize;
use std::collections::BTreeSet;

pub type TV = u32;
pub type Cid = u32;

/// 一条来源：表达式节点 + 产生它的约束。
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Prov {
    pub node: u32,
    pub cid: Option<u32>,
    pub span: Option<Span>,
}

/// 来源集合：有序去重，便于合并与结构断言。
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct ProvSet(pub Vec<Prov>);

impl ProvSet {
    pub fn new() -> Self {
        ProvSet(Vec::new())
    }
    pub fn single(p: Prov) -> Self {
        ProvSet(vec![p])
    }
    pub fn node(node: u32, span: Option<Span>) -> Self {
        ProvSet(vec![Prov { node, cid: None, span }])
    }
    pub fn merge(&mut self, other: &ProvSet) {
        for p in &other.0 {
            if let Err(i) = self.0.binary_search(p) {
                self.0.insert(i, p.clone());
            }
        }
    }
    pub fn merged(mut self, other: &ProvSet) -> Self {
        self.merge(other);
        self

    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn iter(&self) -> std::slice::Iter<'_, Prov> {
        self.0.iter()
    }
}

/// 行的尾部：封闭（无更多字段）或开放（绑定到一个类型变量）。
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum RowTail {
    Closed,
    Open(TV),
}

/// 行绑定：字段有序存储；unify 时把 front 折叠到 tail 中。
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RowBind {
    pub fields: Vec<(String, Ty)>,
    pub tail: RowTail,
}

/// 类型表示（求解前/中/后共用）。
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "k", content = "v")]
pub enum Ty {
    Var(TV),
    Con { name: String, args: Vec<Ty> },
    Arrow(Box<Ty>, Box<Ty>),
    Record(RowBind),
    Variant(RowBind),
}

impl Ty {
    pub fn int() -> Ty {
        Ty::Con { name: "Int".to_string(), args: vec![] }
    }
    pub fn str() -> Ty {
        Ty::Con { name: "String".to_string(), args: vec![] }
    }
    pub fn bool() -> Ty {
        Ty::Con { name: "Bool".to_string(), args: vec![] }
    }
    pub fn unit() -> Ty {
        Ty::Con { name: "Unit".to_string(), args: vec![] }
    }
    pub fn var(t: TV) -> Ty {
        Ty::Var(t)
    }
    pub fn arrow(a: Ty, b: Ty) -> Ty {
        Ty::Arrow(Box::new(a), Box::new(b))
    }
    pub fn record(fields: Vec<(String, Ty)>, tail: RowTail) -> Ty {
        Ty::Record(RowBind { fields, tail })
    }
    pub fn variant(fields: Vec<(String, Ty)>, tail: RowTail) -> Ty {
        Ty::Variant(RowBind { fields, tail })
    }

    /// 该类型中出现的所有（已解析前的）变量。
    pub fn vars(&self, uf: &UnionFind, out: &mut BTreeSet<TV>) {
        match self {
            Ty::Var(t) => {
                let r = uf.find(*t);
                out.insert(r);
            }
            Ty::Con { args, .. } => {
                for a in args {
                    a.vars(uf, out);
                }
            }
            Ty::Arrow(a, b) => {
                a.vars(uf, out);
                b.vars(uf, out);
            }
            Ty::Record(r) | Ty::Variant(r) => {
                for (_, f) in &r.fields {
                    f.vars(uf, out);
                }
                if let RowTail::Open(v) = r.tail {
                    out.insert(uf.find(v));
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum NodeData {
    Root { rank: u32, level: u32, term: Option<Ty>, prov: ProvSet },
    Child { parent: TV },
}

/// 类型变量并查集：rank（Oller level）、term 绑定与来源。
#[derive(Debug, Clone)]
pub struct UnionFind {
    nodes: Vec<NodeData>,
}

impl UnionFind {
    pub fn new() -> Self {
        UnionFind { nodes: Vec::new() }
    }

    pub fn fresh(&mut self, level: u32, prov: ProvSet) -> TV {
        let t = self.nodes.len() as TV;
        self.nodes.push(NodeData::Root { rank: 0, level, term: None, prov });
        t
    }

    pub fn snapshot(&self) -> usize {
        self.nodes.len()
    }

    pub fn find(&self, mut t: TV) -> TV {
        loop {
            match &self.nodes[t as usize] {
                NodeData::Child { parent } => {
                    if *parent == t {
                        return t;
                    }
                    t = *parent;
                }
                NodeData::Root { .. } => return t,
            }
        }
    }

    /// 路径压缩版 find（求解主路径使用）。
    pub fn findc(&mut self, mut t: TV) -> TV {
        let root = self.find(t);
        while t != root {
            if let NodeData::Child { parent } = &self.nodes[t as usize] {
                let p = *parent;
                self.nodes[t as usize] = NodeData::Child { parent: root };
                t = p;
            } else {
                break;
            }
        }
        root
    }

    fn root(&self, t: TV) -> (&u32, &u32, &Option<Ty>, &ProvSet) {
        match &self.nodes[t as usize] {
            NodeData::Root { rank, level, term, prov } => (rank, level, term, prov),
            NodeData::Child { .. } => panic!("not a root: {}", t),
        }
    }

    fn root_mut(&mut self, t: TV) -> (&mut u32, &mut u32, &mut Option<Ty>, &mut ProvSet) {
        match &mut self.nodes[t as usize] {
            NodeData::Root { rank, level, term, prov } => (rank, level, term, prov),
            NodeData::Child { .. } => panic!("not a root: {}", t),
        }
    }

    pub fn level_of(&self, t: TV) -> u32 {
        *self.root(t).1
    }

    pub fn term_of(&self, t: TV) -> Option<&Ty> {
        self.root(t).2.as_ref()
    }

    pub fn prov_of(&self, t: TV) -> &ProvSet {
        self.root(t).3
    }

    pub fn set_term(&mut self, t: TV, term: Ty, prov: &ProvSet) {
        let (_, _, slot, p) = self.root_mut(t);
        *slot = Some(term);
        p.merge(prov);
    }

    pub fn add_prov(&mut self, t: TV, prov: &ProvSet) {
        self.root_mut(t).3.merge(prov);
    }

    /// 把变量的 level 抬升到不超过 target（level 数值越小越外层）。
    pub fn adjust_level(&mut self, t: TV, target: u32) {
        let l = self.level_of(t);
        if target < l {
            *self.root_mut(t).1 = target;
        }
    }

    /// 并查集合并：返回最终 root。
    pub fn union(&mut self, a: TV, b: TV, prov: &ProvSet) -> TV {
        let ra = self.findc(a);
        let rb = self.findc(b);
        if ra == rb {
            self.add_prov(ra, prov);
            return ra;
        }
        let (rka, la, ta, pa) = match self.nodes[ra as usize].clone() {
            NodeData::Root { rank, level, term, prov } => (rank, level, term, prov),
            _ => unreachable!(),
        };
        let (rkb, lb, tb, pb) = match self.nodes[rb as usize].clone() {
            NodeData::Root { rank, level, term, prov } => (rank, level, term, prov),
            _ => unreachable!(),
        };
        // 绑定了 term 的 root 优先保留（减少重定向 term 的工作）。
        let (winner, loser) = match (&ta, &tb) {
            (Some(_), None) => (ra, rb),
            (None, Some(_)) => (rb, ra),
            _ => {
                if rka >= rkb {
                    (ra, rb)
                } else {
                    (rb, ra)
                }
            }
        };
        let mut merged_prov = ProvSet::new();
        merged_prov.merge(&pa);
        merged_prov.merge(&pb);
        merged_prov.merge(prov);
        let term_w = if winner == ra { ta } else { tb };
        let term_l = if loser == ra { ta } else { tb };
        let level_w = if winner == ra { la } else { lb };
        let level_l = if loser == ra { la } else { lb };
        let rank_w = if winner == ra { rka } else { rkb };
        let rank_l = if loser == ra { rka } else { rkb };
        let new_level = level_w.min(level_l);
        let new_rank = if rank_w == rank_l { rank_w + 1 } else { rank_w };
        let mut new_term = term_w.clone();
        if new_term.is_none() {
            new_term = term_l;
        }
        self.nodes[loser as usize] = NodeData::Child { parent: winner };
        let (rw, lw, tw, pw) = self.root_mut(winner);
        *rw = new_rank;
        *lw = new_level;
        *tw = new_term;
        *pw = merged_prov;
        winner
    }

    /// 回滚到某次 snapshot（仅用于尝试性检查，例如核心枚举时整体克隆求解器）。
    pub fn rollback(&mut self, n: usize) {
        self.nodes.truncate(n);
    }
}
