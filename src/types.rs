use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Star,
    Row,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RTy {
    V(u32, Kind),
    Con(String),
    Fun(Box<RTy>, Box<RTy>),
    Record(Row),
    Variant(Row),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Row {
    pub fields: BTreeMap<String, RTy>,
    /// None means closed; Some(v) is a row tail variable.
    pub tail: Option<u32>,
}

impl Row {
    pub fn open(fields: BTreeMap<String, RTy>, tail: u32) -> Self {
        Row { fields, tail: Some(tail) }
    }
    pub fn closed(fields: BTreeMap<String, RTy>) -> Self {
        Row { fields, tail: None }
    }
    pub fn empty_closed() -> Self {
        Row { fields: BTreeMap::new(), tail: None }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme {
    pub binds: Vec<u32>,
    pub body: RTy,
}

impl RTy {
    pub fn fresh_star(counter: &mut u32) -> RTy {
        let v = *counter;
        *counter += 1;
        RTy::V(v, Kind::Star)
    }
    pub fn fresh_row(counter: &mut u32) -> RTy {
        let v = *counter;
        *counter += 1;
        RTy::V(v, Kind::Row)
    }
    pub fn var_id(&self) -> Option<(u32, Kind)> {
        match self {
            RTy::V(id, k) => Some((*id, *k)),
            _ => None,
        }
    }
    pub fn map_vars<F: FnMut(u32, Kind) -> RTy>(&self, f: &mut F) -> RTy {
        match self {
            RTy::V(id, k) => f(*id, *k),
            RTy::Con(c) => RTy::Con(c.clone()),
            RTy::Fun(a, b) => RTy::Fun(Box::new(a.map_vars(f)), Box::new(b.map_vars(f))),
            RTy::Record(r) => RTy::Record(map_row(r, f)),
            RTy::Variant(r) => RTy::Variant(map_row(r, f)),
        }
    }
    pub fn vars(&self) -> Vec<(u32, Kind)> {
        let mut out = Vec::new();
        let _ = self.map_vars(&mut |id, k| {
            out.push((id, k));
            RTy::V(id, k)
        });
        out
    }
}

fn map_row<F: FnMut(u32, Kind) -> RTy>(r: &Row, f: &mut F) -> Row {
    let mut fields = BTreeMap::new();
    for (k, v) in &r.fields {
        fields.insert(k.clone(), v.map_vars(f));
    }
    let tail = r.tail.map(|t| match f(t, Kind::Row) {
        RTy::V(id, Kind::Row) => id,
        _ => panic!("row var remap must yield a row var"),
    });
    Row { fields, tail }
}
