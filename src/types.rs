// 类型表示与 union-find。所有未定类型以字符串“变量名”索引（变量名由
// 节点身份+用途派生，因此变量改名不会导致身份漂移）。
use serde::Serialize;
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RowFlavor {
    Record,
    Variant,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RowTail {
    /// 封闭行
    Empty,
    /// 开放行：尾部变量（行多态）
    Open { var: String },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct RowType {
    pub flavor: RowFlavor,
    /// 确定性字段顺序（按 label 排序）
    pub fields: Vec<(String, Type)>,
    pub tail: RowTail,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Type {
    /// 未解析的统一变量
    Var(String),
    Con { name: String },
    Arrow(Box<Type>, Box<Type>),
    Row(RowType),
}

impl Type {
    pub fn var(name: impl Into<String>) -> Self {
        Type::Var(name.into())
    }
    pub fn con(name: impl Into<String>) -> Self {
        Type::Con { name: name.into() }
    }
    pub fn arrow(a: Type, b: Type) -> Self {
        Type::Arrow(Box::new(a), Box::new(b))
    }
    pub fn int() -> Self {
        Type::con("Int")
    }
    pub fn bool() -> Self {
        Type::con("Bool")
    }
    pub fn str() -> Self {
        Type::con("String")
    }
    pub fn record(fields: Vec<(String, Type)>, tail: RowTail) -> Self {
        let mut fields = fields;
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        Type::Row(RowType { flavor: RowFlavor::Record, fields, tail })
    }
    pub fn variant(fields: Vec<(String, Option<Type>)>, tail: RowTail) -> Self {
        let mut fields: Vec<(String, Type)> = fields
            .into_iter()
            .map(|(l, t)| (l, t.unwrap_or_else(|| Type::con("unit"))))
            .collect();
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        Type::Row(RowType { flavor: RowFlavor::Variant, fields, tail })
    }
    pub fn unit() -> Self {
        Type::con("unit")
    }
    pub fn is_var(&self) -> bool {
        matches!(self, Type::Var(_))
    }
    pub fn label(&self) -> &'static str {
        match self {
            Type::Var(_) => "var",
            Type::Con { .. } => "con",
            Type::Arrow(_, _) => "arrow",
            Type::Row(r) => match r.flavor {
                RowFlavor::Record => "record",
                RowFlavor::Variant => "variant",
            },
        }
    }
    pub fn render(&self) -> String {
        match self {
            Type::Var(v) => format!("?{v}"),
            Type::Con { name } => name.clone(),
            Type::Arrow(a, b) => {
                let a_s = if matches!(a.as_ref(), Type::Arrow(_, _)) {
                    format!("({})", a.render())
                } else {
                    a.render()
                };
                format!("{a_s} -> {}", b.render())
            }
            Type::Row(r) => {
                let open = match &r.tail {
                    RowTail::Empty => String::new(),
                    RowTail::Open { var } => format!(" | ?{var}"),
                };
                match r.flavor {
                    RowFlavor::Record => {
                        let body: Vec<String> =
                            r.fields.iter().map(|(l, t)| format!("{l}: {}", t.render())).collect();
                        format!("{{{}{}}}", body.join(", "), open)
                    }
                    RowFlavor::Variant => {
                        let body: Vec<String> = r
                            .fields
                            .iter()
                            .map(|(l, t)| {
                                if t == &Type::unit() {
                                    l.clone()
                                } else {
                                    format!("{l} {}", t.render())
                                }
                            })
                            .collect();
                        format!("[{}]", body.join(" | "))
                    }
                }
            }
        }
    }
}

// ---------------- Union-Find ----------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum VarOrigin {
    /// 通用的推导变量
    Infer,
    /// 行尾变量
    RowTail,
    /// 实例化多态方案产生的副本
    Inst,
}

#[derive(Debug, Clone)]
pub struct UfNode {
    pub parent: Option<String>,
    pub rank: u32,
    /// 来源表达式节点
    pub origin_nid: u32,
    pub origin_kind: VarOrigin,
    /// 泛化用：该变量在哪个 let 层级被引入（None = 全局层）
    pub level: u32,
}

#[derive(Debug, Clone)]
pub struct UnionFind {
    pub nodes: BTreeMap<String, UfNode>,
}

impl UnionFind {
    pub fn new() -> Self {
        UnionFind { nodes: BTreeMap::new() }
    }

    pub fn make(&mut self, name: &str, nid: u32, kind: VarOrigin, level: u32) {
        self.nodes.entry(name.to_string()).or_insert(UfNode {
            parent: None,
            rank: 0,
            origin_nid: nid,
            origin_kind: kind,
            level,
        });
    }

    pub fn contains(&self, name: &str) -> bool {
        self.nodes.contains_key(name)
    }

    pub fn root_of(&self, name: &str) -> String {
        let mut cur = name.to_string();
        while let Some(p) = self.nodes.get(&cur).and_then(|n| n.parent.clone()) {
            cur = p;
        }
        cur
    }

    pub fn root_nid(&self, name: &str) -> u32 {
        let r = self.root_of(name);
        self.nodes[&r].origin_nid
    }

    // 返回合并后根名。调用方应先确认两根不同；这里对同根做严格防护，
    // 绝不写入自环。
    pub fn union(&mut self, a: &str, b: &str) -> String {
        let ra = self.root_of(a);
        let rb = self.root_of(b);
        if ra == rb {
            return ra;
        }
        let (ka, kb) = (self.nodes[&ra].rank, self.nodes[&rb].rank);
        if ka < kb {
            self.nodes.get_mut(&ra).unwrap().parent = Some(rb.clone());
            rb
        } else if ka > kb {
            self.nodes.get_mut(&rb).unwrap().parent = Some(ra.clone());
            ra
        } else {
            // 取一个与 rb 不同的新根：让 ra 挂 rb 也可，但 rank 相等时
            // 固定把 rb 并入 ra，并提升 ra 的 rank。
            self.nodes.get_mut(&rb).unwrap().parent = Some(ra.clone());
            self.nodes.get_mut(&ra).unwrap().rank += 1;
            ra
        }
    }

    /// 解析一个类型中的所有变量到根变量名（不替换为具体类型，
    /// 具体类型通过 root_term 表关联）。
    pub fn resolve_names(&self, t: &Type) -> Type {
        match t {
            Type::Var(v) => Type::Var(self.root_of(v)),
            Type::Con { .. } => t.clone(),
            Type::Arrow(a, b) => {
                Type::arrow(self.resolve_names(a), self.resolve_names(b))
            }
            Type::Row(r) => {
                let fields = r
                    .fields
                    .iter()
                    .map(|(l, ty)| (l.clone(), self.resolve_names(ty)))
                    .collect();
                let tail = match &r.tail {
                    RowTail::Empty => RowTail::Empty,
                    RowTail::Open { var } => RowTail::Open { var: self.root_of(var) },
                };
                Type::Row(RowType { flavor: r.flavor, fields, tail })
            }
        }
    }
}
