// Stable, rename-insensitive structural identities.
// Keys are hash chains over: node kind/discriminator, child roles, the enclosing
// scope chain (binder keys), and the generalization environment active at a let.

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeKey(pub String);
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BinderKey(pub String);

// Deterministic FNV-1a 64-bit, chaining tagged pieces.
#[derive(Clone, Copy)]
pub struct Chain(u64);

const OFFSET: u64 = 0xcbf29ce484222325;
const PRIME: u64 = 0x100000001b3;

impl Chain {
    pub fn new() -> Self { Chain(OFFSET) }
    pub fn byte(&mut self, b: u8) -> &mut Self {
        self.0 ^= b as u64;
        self.0 = self.0.wrapping_mul(PRIME);
        self
    }
    pub fn str(&mut self, s: &str) -> &mut Self {
        for b in s.as_bytes() { self.byte(*b); }
        self.byte(0xff);
        self
    }
    pub fn u64(&mut self, n: u64) -> &mut Self {
        let mut x = n;
        for _ in 0..8 { self.byte((x & 0xff) as u8); x >>= 8; }
        self
    }
    pub fn tag(&mut self, tag: &str) -> &mut Self {
        self.byte(0x02);
        self.str(tag)
    }
    pub fn child(&mut self, role: &str, sub: u64) -> &mut Self {
        self.byte(0x03);
        self.str(role);
        self.u64(sub);
        self
    }
    pub fn get(&self) -> u64 { self.0 }
    pub fn key(&self) -> String { format!("{:016x}", self.0) }
}
