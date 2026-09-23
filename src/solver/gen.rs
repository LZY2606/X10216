//! 从 AST 生成类型节点与约束（同时承担作用域与泛化环境指纹）。

use crate::ast::*;
use crate::constraint::{make_cid, Constraint};
use crate::solver::arena::{Arena, Origin};
use crate::span::Span;
use crate::types::{Content, Kind, RowKind, TyId, TyNode};
use std::collections::HashMap;
use twox_hash::XxHash64;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug)]
struct EnvEntry {
    /// 绑定位置（用于展示方案来源）。
    scheme_span: Span,
    /// 泛化槽位数；>0 表示多态。
    generics: u32,
    /// 环境指纹贡献：绑定点 nid + 槽位数。
    fp: u64,
}

pub struct Gen<'a> {
    pub arena: Arena,
    pub cs: Vec<Constraint>,
    env: Vec<HashMap<String, EnvEntry>>,
    /// 环境指纹：各作用域名字绑定的叠加哈希。
    env_fp: u64,
    level: u32,
    src: &'a str,
    /// 表达式节点 -> 推导出的 TyId。
    pub expr_ty: HashMap<u64, TyId>,
    /// 记录/变体标签来源（label -> span），供错误定位。
    pub label_spans: HashMap<u64, Span>,
    scope_idx: u32,
}

impl<'a> Gen<'a> {
    pub fn new(src: &'a str) -> Self {
        Gen {
            arena: Arena::new(),
            cs: Vec::new(),
            env: vec![HashMap::new()],
            env_fp: 0x1234_5678,
            level: 1,
            src,
            expr_ty: HashMap::new(),
            label_spans: HashMap::new(),
            scope_idx: 0,
        }
    }

    fn push_scope(&mut self) {
        self.env.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        let m = self.env.pop().unwrap();
        // 退出作用域时从指纹移除贡献
        for (_, e) in m {
            let mut h = XxHash64::with_seed(0x5C0PE ^ e.fp);
            h.write_u64(self.env_fp);
            self.env_fp = h.finish();
        }
    }
    fn bind_name(&mut self, name: &str, e: EnvEntry) {
        let mut h = XxHash64::with_seed(0xB1ND ^ e.fp);
        h.write(name.as_bytes());
        h.write_u64(self.env_fp);
        self.env_fp = h.finish();
        self.env.last_mut().unwrap().insert(name.to_string(), e);
    }
    fn lookup(&self, name: &str) -> Option<&EnvEntry> {
        for m in self.env.iter().rev() {
            if let Some(e) = m.get(name) {
                return Some(e);
            }
        }
        None
    }

    fn flex(&mut self, nid: NodeId, span: Span, note: &str) -> TyId {
        let o = Origin::new(note.to_string()).with_nid(nid.0).with_span(span);
        self.arena.fresh(Kind::Type, self.level, o)
    }
    fn flex_row(&mut self, nid: NodeId, span: Span, note: &str) -> TyId {
        let o = Origin::new(note.to_string()).with_nid(nid.0).with_span(span);
        self.arena.fresh(Kind::Row, self.level, o)
    }

    fn mk(&mut self, node: TyNode, nid: NodeId, span: Span, note: &str) -> TyId {
        let o = Origin::new(note.to_string()).with_nid(nid.0).with_span(span);
        self.arena.make(node, o)
    }

    fn env_fp_now(&self) -> u64 {
        self.env_fp
    }
}
