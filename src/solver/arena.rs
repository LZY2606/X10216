use crate::types::{Content, Kind, TyId, TyNode};
use std::collections::HashMap;

#[derive(Clone)]
pub struct Arena {
    pub nodes: Vec<TyNode>,
    pub link: Vec<Option<u32>>,
    /// 节点来源（生成点 / 求解中派生点）。
    pub origin: Vec<Origin>,
    /// 统一后并入代表节点的其它节点 id（用于来源追溯）。
    pub merged: HashMap<u32, Vec<u32>>,
}

#[derive(Clone, Debug)]
pub struct Origin {
    pub note: String,
    pub nid: Option<u64>,
    pub span: Option<crate::span::Span>,
    pub cid: Option<u64>,
}

impl Origin {
    pub fn new(note: impl Into<String>) -> Self {
        Origin {
            note: note.into(),
            nid: None,
            span: None,
            cid: None,
        }
    }
    pub fn with_nid(mut self, nid: u64) -> Self {
        self.nid = Some(nid);
        self
    }
    pub fn with_span(mut self, span: crate::span::Span) -> Self {
        self.span = Some(span);
        self
    }
    pub fn with_cid(mut self, cid: u64) -> Self {
        self.cid = Some(cid);
        self
    }
}

impl Arena {
    pub fn new() -> Self {
        Arena {
            nodes: Vec::new(),
            link: Vec::new(),
            origin: Vec::new(),
            merged: HashMap::new(),
        }
    }

    pub fn fresh(&mut self, kind: Kind, level: u32, origin: Origin) -> TyId {
        let id = self.nodes.len() as u32;
        self.nodes.push(TyNode {
            content: Content::Flex {
                level,
                generic: None,
            },
            kind,
        });
        self.link.push(None);
        self.origin.push(origin);
        TyId(id)
    }

    pub fn make(&mut self, node: TyNode, origin: Origin) -> TyId {
        let id = self.nodes.len() as u32;
        self.nodes.push(node);
        self.link.push(None);
        self.origin.push(origin);
        TyId(id)
    }

    pub fn find(&mut self, t: TyId) -> TyId {
        let TyId(mut x) = t;
        let mut root = x;
        while let Some(p) = self.link[root as usize] {
            root = p;
        }
        while let Some(p) = self.link[x as usize] {
            self.link[x as usize] = Some(root);
            x = p;
        }
        TyId(root)
    }

    pub fn find_imm(&self, t: TyId) -> TyId {
        let TyId(mut x) = t;
        while let Some(p) = self.link[x as usize] {
            x = p;
        }
        TyId(x)
    }

    pub fn kind_of(&mut self, t: TyId) -> Kind {
        let r = self.find(t);
        self.nodes[r.0 as usize].kind
    }

    pub fn content(&self, t: TyId) -> &Content {
        &self.nodes[t.0 as usize].content
    }

    /// 把 a 链接到 b（b 必须是代表节点）。
    pub fn link_to(&mut self, a: TyId, b: TyId) {
        debug_assert_eq!(self.find_imm(b), b);
        self.link[a.0 as usize] = Some(b.0);
        self.merged.entry(b.0).or_default().push(a.0);
    }
}
