use crate::types::{Row, RowView, Ty, TyView};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    T,
    R,
}

/// 一个类型/行构造的出处：哪条约束、哪个 AST 节点
#[derive(Debug, Clone, Copy)]
pub struct Origin {
    pub cid: u32,
    pub node: u32,
}

#[derive(Debug, Clone)]
pub enum Head {
    Flex { kind: Kind, origin: Origin },
    Con { name: String, origin: Origin },
    App { name: String, args: Vec<u32>, origin: Origin },
    Record(u32, Origin),
    Variant(u32, Origin),
    RowClosed { fields: Vec<(String, u32)>, tail: Option<u32>, origin: Origin },
}

impl Head {
    pub fn origin(&self) -> Origin {
        match self {
            Head::Flex { origin, .. }
            | Head::Con { origin, .. }
            | Head::App { origin, .. }
            | Head::Record(_, origin)
            | Head::Variant(_, origin)
            | Head::RowClosed { origin, .. } => *origin,
        }
    }
}

pub struct UF {
    pub parent: Vec<u32>,
    pub rank: Vec<u32>,
    pub head: Vec<Option<Head>>,
}

impl UF {
    pub fn new() -> Self {
        UF { parent: Vec::new(), rank: Vec::new(), head: Vec::new() }
    }

    pub fn mk(&mut self, h: Head) -> u32 {
        let n = self.parent.len() as u32;
        self.parent.push(n);
        self.rank.push(0);
        self.head.push(Some(h));
        n
    }

    pub fn fresh_t(&mut self, o: Origin) -> u32 {
        self.mk(Head::Flex { kind: Kind::T, origin: o })
    }
    pub fn fresh_r(&mut self, o: Origin) -> u32 {
        self.mk(Head::Flex { kind: Kind::R, origin: o })
    }

    pub fn find(&mut self, mut n: u32) -> u32 {
        let mut r = n;
        while self.parent[r as usize] != r {
            r = self.parent[r as usize];
        }
        while self.parent[n as usize] != n {
            let p = self.parent[n as usize];
            self.parent[n as usize] = r;
            n = p;
        }
        r
    }

    pub fn root_head(&mut self, n: u32) -> (u32, Head) {
        let r = self.find(n);
        (r, self.head[r as usize].clone().unwrap())
    }

    /// 两个根合一（仅限 Flex/Flex 的秩合并）。返回代表根。
    pub fn union_roots(&mut self, a: u32, b: u32) -> u32 {
        if a == b {
            return a;
        }
        match (self.rank[a as usize], self.rank[b as usize]) {
            (ra, rb) if ra < rb => {
                self.parent[a as usize] = b;
                b
            }
            (ra, rb) if ra > rb => {
                self.parent[b as usize] = a;
                a
            }
            _ => {
                self.parent[b as usize] = a;
                self.rank[a as usize] += 1;
                a
            }
        }
    }

    /// 把 flex 根 a 绑定到结构根 b（b 保留 head）。
    pub fn bind(&mut self, flex_root: u32, struct_root: u32) {
        self.head[flex_root as usize] = None;
        self.parent[flex_root as usize] = struct_root;
    }

    pub fn set_head(&mut self, root: u32, h: Head) {
        self.head[root as usize] = Some(h);
    }

    pub fn kind_of(&mut self, n: u32) -> Kind {
        let (_, h) = self.root_head(n);
        match h {
            Head::Flex { kind, .. } => kind,
            Head::RowClosed { .. } => Kind::R,
            _ => Kind::T,
        }
    }

    /// 由 Ty 结构新建节点；var_map 给出 TVar/Open 行变量到节点的映射。
    pub fn build(&mut self, t: &Ty, o: Origin, var_map: &mut [(u32, u32)]) -> u32 {
        match t {
            Ty::TVar(v) => {
                if let Some((_, n)) = var_map.iter().find(|(tv, _)| tv == v) {
                    *n
                } else {
                    let n = self.fresh_t(o);
                    var_map.push((*v, n));
                    n
                }
            }
            Ty::Con(name) => self.mk(Head::Con { name: name.clone(), origin: o }),
            Ty::App(name, args) => {
                let ns: Vec<u32> = args.iter().map(|a| self.build(a, o, var_map)).collect();
                self.mk(Head::App { name: name.clone(), args: ns, origin: o })
            }
            Ty::Record(r) => {
                let rn = self.build_row(r, o, var_map);
                self.mk(Head::Record(rn, o))
            }
            Ty::Variant(r) => {
                let rn = self.build_row(r, o, var_map);
                self.mk(Head::Variant(rn, o))
            }
        }
    }

    fn build_row(&mut self, r: &Row, o: Origin, var_map: &mut [(u32, u32)]) -> u32 {
        match r {
            Row::Open(v) => {
                if let Some((_, n)) = var_map.iter().find(|(tv, _)| tv == v) {
                    *n
                } else {
                    let n = self.fresh_r(o);
                    var_map.push((*v, n));
                    n
                }
            }
            Row::Closed(fs) => {
                let fields: Vec<(String, u32)> = fs
                    .iter()
                    .map(|(n, t)| (n.clone(), self.build(t, o, var_map)))
                    .collect();
                self.mk(Head::RowClosed { fields, tail: None, origin: o })
            }
        }
    }

    /// 读出节点当前的结构视图（含未解析变量）。seen 用于阻断循环。
    pub fn view(&mut self, n: u32, seen: &mut Vec<u32>) -> TyView {
        let (r, h) = self.root_head(n);
        match h {
            Head::Flex { .. } => TyView::Var(r),
            Head::Con { name, .. } => TyView::Con(name),
            Head::App { name, args, .. } => {
                TyView::App(name, args.iter().map(|a| self.view_safe(*a, seen)).collect())
            }
            Head::Record(rn, _) => TyView::Record(self.view_row(rn, seen)),
            Head::Variant(rn, _) => TyView::Variant(self.view_row(rn, seen)),
            Head::RowClosed { .. } => TyView::Con("<row>".into()),
        }
    }

    fn view_safe(&mut self, n: u32, seen: &mut Vec<u32>) -> TyView {
        let r = self.find(n);
        if seen.contains(&r) {
            return TyView::Con("…".into());
        }
        seen.push(r);
        let t = self.view(n, seen);
        seen.pop();
        t
    }

    pub fn view_row(&mut self, n: u32, seen: &mut Vec<u32>) -> RowView {
        let (r, h) = self.root_head(n);
        match h {
            Head::Flex { .. } => RowView { fields: vec![], tail: Some(r) },
            Head::RowClosed { fields, tail, .. } => {
                let mut fs = Vec::new();
                for (name, tn) in fields {
                    fs.push((name, self.view_safe(tn, seen)));
                }
                match tail {
                    Some(tn) => {
                        let mut more = self.view_row(tn, seen);
                        fs.append(&mut more.fields);
                        RowView { fields: fs, tail: more.tail }
                    }
                    None => RowView { fields: fs, tail: None },
                }
            }
            _ => RowView::closed(vec![]),
        }
    }
}
