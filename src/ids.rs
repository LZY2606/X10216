//! Deterministic, rename-stable identifiers.

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize)]
#[serde(transparent)]
pub struct NodeId(pub u64);

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize)]
#[serde(transparent)]
pub struct BindingId(pub u64);

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize)]
#[serde(transparent)]
pub struct CId(pub u32);

/// Fresh type / row variable ids, allocated per analysis run.
#[derive(Default, Clone, Copy)]
pub struct VarGen(pub u32);

impl VarGen {
    pub fn fresh_t(&mut self) -> super::types::TId {
        self.0 += 1;
        super::types::TId(self.0)
    }
    pub fn fresh_r(&mut self) -> super::types::RId {
        self.0 += 1;
        super::types::RId(self.0)
    }
}

impl std::fmt::Debug for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "n{:016x}", self.0)
    }
}
impl std::fmt::Debug for BindingId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "b{:016x}", self.0)
    }
}
impl std::fmt::Debug for CId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "c{}", self.0)
    }
}
impl std::fmt::Display for CId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "c{}", self.0)
    }
}

/// FNV-1a 64 bit hasher used to make identities content/structure based.
#[derive(Clone, Copy)]
pub struct Hasher(pub u64);

const OFFSET: u64 = 0xcbf29ce484222325;
const PRIME: u64 = 0x100000001b3;

impl Hasher {
    pub fn new() -> Self {
        Hasher(OFFSET)
    }
    pub fn with(tag: u8) -> Self {
        let mut h = Hasher(OFFSET);
        h.byte(tag);
        h
    }
    #[inline]
    pub fn byte(&mut self, b: u8) {
        self.0 ^= b as u64;
        self.0 = self.0.wrapping_mul(PRIME);
    }
    pub fn bytes(&mut self, bs: &[u8]) {
        for &b in bs {
            self.byte(b);
        }
    }
    pub fn str(&mut self, s: &str) {
        self.bytes(s.as_bytes());
        self.byte(0);
    }
    pub fn u64(&mut self, x: u64) {
        self.bytes(&x.to_le_bytes());
    }
    pub fn bool(&mut self, b: bool) {
        self.byte(b as u8);
    }
    pub fn finish(self) -> u64 {
        self.0
    }
}
