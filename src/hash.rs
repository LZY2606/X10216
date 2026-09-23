//! 64 位 FNV-1a 内容哈希。
//!
//! 解析节点与约束的“身份”只依赖结构内容（子节点哈希、节点种类、
//! 字面量、绑定名、作用域哈希、泛化环境哈希），不依赖自增下标，
//! 因此变量改名时无关代码的身份保持稳定。

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct H64(pub u64);

impl H64 {
    pub fn new() -> Self {
        H64(0xcbf2_9ce4_8422_2325)
    }
    #[inline]
    pub fn byte(mut self, b: u8) -> Self {
        self.0 ^= b as u64;
        self.0 = self.0.wrapping_mul(0x0000_0100_0000_01b3);
        self
    }
    pub fn bytes(mut self, bs: &[u8]) -> Self {
        for &b in bs {
            self = self.byte(b);
        }
        self
    }
    pub fn str(self, s: &str) -> Self {
        self.bytes(s.as_bytes())
    }
    pub fn u64(mut self, x: u64) -> Self {
        self = self.byte(b'N');
        for i in 0..8 {
            self = self.byte(((x >> (8 * i)) & 0xff) as u8);
        }
        self
    }
    pub fn tag(self, t: u8) -> Self {
        self.byte(b'T').byte(t)
    }
    pub fn mix(self, h: H64) -> Self {
        self.u64(h.0)
    }
    pub fn get(self) -> u64 {
        self.0
    }
}

impl Default for H64 {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for H64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:016x}", self.0)
    }
}

impl std::fmt::Display for H64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:016x}", self.0)
    }
}

impl serde::Serialize for H64 {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&format!("{:016x}", self.0))
    }
}
