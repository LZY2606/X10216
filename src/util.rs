pub fn fnv1a(data: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for &b in data {
        h ^= b as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

pub fn short(h: u64) -> String {
    format!("{:x}", h & 0x00ff_ffff_ffff)
}

pub fn hash_str(parts: &[&str]) -> u64 {
    let mut buf = Vec::new();
    for p in parts {
        buf.extend_from_slice(&(p.len() as u64).to_le_bytes());
        buf.extend_from_slice(p.as_bytes());
        buf.push(0xff);
    }
    fnv1a(&buf)
}
