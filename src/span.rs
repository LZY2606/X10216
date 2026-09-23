#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
    pub fn merge(self, other: Span) -> Span {
        Span {
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
        }
    }
    pub fn line_col(&self, src: &str) -> ((usize, usize), (usize, usize)) {
        let conv = |pos: usize| {
            let prefix = &src[..pos.min(src.len())];
            let line = prefix.bytes().filter(|b| *b == b'\n').count() + 1;
            let col = match prefix.rfind('\n') {
                Some(i) => pos - i - 1,
                None => pos,
            } + 1;
            (line, col)
        };
        (conv(self.lo), conv(self.hi))
    }
}
