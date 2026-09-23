use serde::Serialize;
use std::collections::BTreeSet;

/// Semantic, content-stable type. Variable identities are *string keys*
/// (e.g. "v:<nodeId>"), independent of union-find ids, so a constraint
/// keeps the same identity across edits and across solver runs.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "k", content = "v")]
pub enum SType {
    V(String),
    Con(String),
    Fun(Box<SType>, Box<SType>),
    Record(Vec<SField>, SRow),
    Variant(Vec<SField>, SRow),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct SField {
    pub label: String,
    pub ty: SType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(tag = "k", content = "v")]
pub enum SRow {
    Closed,
    Open(String),
}

impl SType {
    pub fn fun(a: SType, b: SType) -> SType { SType::Fun(Box::new(a), Box::new(b)) }
    pub fn rec(fields: Vec<SField>, row: SRow) -> SType { SType::Record(fields, row) }
    pub fn variant(fields: Vec<SField>, row: SRow) -> SType { SType::Variant(fields, row) }

    pub fn free_vars(&self) -> BTreeSet<String> {
        let mut out = BTreeSet::new();
        self.collect(&mut out);
        out
    }
    fn collect(&self, out: &mut BTreeSet<String>) {
        match self {
            SType::V(k) => { out.insert(k.clone()); }
            SType::Con(_) => {}
            SType::Fun(a, b) => { a.collect(out); b.collect(out); }
            SType::Record(fs, r) | SType::Variant(fs, r) => {
                for f in fs { f.ty.collect(out); }
                if let SRow::Open(k) = r { out.insert(k.clone()); }
            }
        }
    }

    /// Deep copy, replacing keys in `mapping`.
    pub fn with_mapping(&self, mapping: &std::collections::HashMap<String, String>) -> SType {
        match self {
            SType::V(k) => mapping.get(k).cloned().map(SType::V).unwrap_or_else(|| self.clone()),
            SType::Con(_) => self.clone(),
            SType::Fun(a, b) => SType::fun(a.with_mapping(mapping), b.with_mapping(mapping)),
            SType::Record(fs, r) => SType::Record(
                fs.iter().map(|f| SField { label: f.label.clone(), ty: f.ty.with_mapping(mapping) }).collect(),
                map_row(r, mapping),
            ),
            SType::Variant(fs, r) => SType::Variant(
                fs.iter().map(|f| SField { label: f.label.clone(), ty: f.ty.with_mapping(mapping) }).collect(),
                map_row(r, mapping),
            ),
        }
    }

    pub fn render(&self) -> String {
        let mut s = String::new();
        self.write(&mut s, false);
        s
    }
    fn write(&self, s: &mut String, paren: bool) {
        match self {
            SType::V(k) => s.push_str(short_key(k)),
            SType::Con(n) => s.push_str(n),
            SType::Fun(a, b) => {
                if paren { s.push('('); }
                a.write(s, true);
                s.push_str(" -> ");
                b.write(s, false);
                if paren { s.push(')'); }
            }
            SType::Record(fs, r) => {
                s.push('{');
                for (i, f) in fs.iter().enumerate() {
                    if i > 0 { s.push_str(", "); }
                    s.push_str(&f.label);
                    s.push_str(": ");
                    f.ty.write(s, false);
                }
                match r {
                    SRow::Closed => {}
                    SRow::Open(k) => {
                        if !fs.is_empty() { s.push_str(", "); }
                        s.push_str("..");
                        s.push_str(short_key(k));
                    }
                }
                s.push('}');
            }
            SType::Variant(fs, r) => {
                s.push('<');
                for (i, f) in fs.iter().enumerate() {
                    if i > 0 { s.push_str(", "); }
                    s.push_str(&f.label);
                    if matches!(f.ty, SType::Con(_)) && f.ty == SType::Con("unit".into()) {
                        // nullary tag: no payload
                    } else {
                        s.push('(');
                        f.ty.write(s, false);
                        s.push(')');
                    }
                }
                if let SRow::Open(k) = r {
                    if !fs.is_empty() { s.push_str(", "); }
                    s.push_str("..");
                    s.push_str(short_key(k));
                }
                s.push('>');
            }
        }
    }
}

fn map_row(r: &SRow, mapping: &std::collections::HashMap<String, String>) -> SRow {
    match r {
        SRow::Closed => SRow::Closed,
        SRow::Open(k) => SRow::Open(mapping.get(k).cloned().unwrap_or_else(|| k.clone())),
    }
}

pub fn short_key(k: &str) -> &str {
    k.rsplit(':').next().unwrap_or(k)
}
