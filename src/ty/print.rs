//! Human printing and provenance-annotated JSON views of types.

use super::ty::*;
use super::unify;
use serde_json::{json, Value};
use std::collections::HashMap;

pub fn print(e: &Engine, t: &Ty) -> String {
    let mut names: HashMap<u32, String> = HashMap::new();
    let mut rnames: HashMap<u32, String> = HashMap::new();
    let greek = ["a","b","c","d","e","f","g","h"];
    let mut ti = 0usize;
    let mut ri = 0usize;
    fn atom(e: &Engine, t: &Ty) -> String {
        match e.resolve(t) {
            Ty::Con(c) => c.name().to_string(),
            other => format!("({})", go(e, &other)),
        }
    }
    fn go(e: &Engine, t: &Ty) -> String {
        match e.resolve(t) {
            Ty::Con(c) => c.name().to_string(),
            Ty::Var(v) => format!("t{}", v),
            Ty::Arrow(a, b) => format!("{} -> {}", atom(e, &a), go(e, &b)),
            Ty::Row(kind, fs, tail) => {
                let (l, r) = kind.brace();
                let mut parts: Vec<String> = fs.iter().map(|f| match kind {
                    RowKind::Record => format!("{}: {}", f.name, go(e, &f.ty)),
                    RowKind::Variant => match e.resolve(&f.ty) {
                        Ty::Con(Con::Unit) => f.name.clone(),
                        other => format!("{}({})", f.name, go(e, &other)),
                    },
                }).collect();
                if let Tail::Open(rv) = tail { parts.push(format!("..r{}", rv)); }
                format!("{}{}{}", l, parts.join(", "), r)
            }
        }
    }
    let _ = (&mut names, &mut rnames, &mut ti, &mut ri, greek);
    go(e, t)
}

/// JSON tree of the final type with provenance node ids on every part.
pub fn view(e: &Engine, t: &Ty) -> Value {
    fn provs_of(e: &Engine, v: VarId) -> Vec<Value> {
        e.prov_t(v).tags.iter().filter_map(|t| match t {
            ProvTag::Node(n) | ProvTag::Binder(n) => Some(json!(n)),
            ProvTag::Unknown => None,
        }).collect()
    }
    fn row_tail(e: &Engine, acc: &mut Vec<Value>, tail: Tail, kind: RowKind) -> Option<Value> {
        match tail {
            Tail::Closed => None,
            Tail::Open(rv) => match e.rvar(rv) {
                RVar::Free { .. } => Some(json!({ "kind": "rowvar", "name": format!("r{}", rv), "prov": e.prov_r(rv).first_node() })),
                RVar::Bound(entry) => {
                    for f in entry.fields {
                        acc.push(field(e, &f, kind));
                    }
                    row_tail(e, acc, entry.tail, kind)
                }
            },
        }
    }
    fn field(e: &Engine, f: &Field, kind: RowKind) -> Value {
        match (kind, e.resolve(&f.ty)) {
            (RowKind::Variant, Ty::Con(Con::Unit)) => json!({ "name": f.name, "unit": true, "value": Value::Null }),
            _ => json!({ "name": f.name, "value": walk(e, &f.ty) }),
        }
    }
    fn walk(e: &Engine, t: &Ty) -> Value {
        let r = e.resolve(t);
        let root_prov: Vec<Value> = match t {
            Ty::Var(v) => provs_of(e, *v),
            _ => vec![],
        };
        match r {
            Ty::Var(v) => json!({
                "kind": "var", "name": format!("t{}", v),
                "prov": provs_of(e, v),
            }),
            Ty::Con(c) => json!({ "kind": "con", "name": c.name(), "prov": root_prov }),
            Ty::Arrow(a, b) => json!({
                "kind": "arrow", "from": walk(e, &a), "to": walk(e, &b), "prov": root_prov,
            }),
            Ty::Row(kind, mut fs, tail) => {
                let (l, rr) = kind.brace();
                let mut fields: Vec<Value> = fs.drain(..).map(|f| field(e, &f, kind)).collect();
                let rowvar = row_tail(e, &mut fields, tail, kind);
                json!({
                    "kind": match kind { RowKind::Record => "record", RowKind::Variant => "variant" },
                    "open": l, "close": rr,
                    "fields": fields, "rowvar": rowvar, "prov": root_prov,
                })
            }
        }
    }
    walk(e, t)
}

/// Collect a human-readable explanation chain for a unification error.
pub fn explain_error(err: &unify::UErr) -> Value {
    use unify::UErr::*;
    fn one(e: &unify::UErr) -> Value {
        match e {
            Occurs { var, kind, .. } => json!({
                "code": "occurs",
                "message": format!("出现检查失败：变量 t{} 出现在自身的右侧，构成无限类型（{}）", var, kind_label(*kind)),
            }),
            RowOccurs { rvar, kind } => json!({
                "code": "row_occurs",
                "message": format!("行变量 r{} 无限递归（{}）", rvar, kind_label(*kind)),
            }),
            ConMismatch { a, b } => json!({
                "code": "con_mismatch",
                "message": format!("基本类型不一致：{} 与 {}", a.name(), b.name()),
            }),
            ConArrow { con } | ArrowCon { con } => json!({
                "code": "con_arrow",
                "message": format!("把基本类型 {} 与函数类型混在一起", con.name()),
            }),
            ConRow { con, kind } | RowCon { con, kind } => json!({
                "code": "con_row",
                "message": format!("把基本类型 {} 与 {}混在一起", con.name(), kind_label(*kind)),
            }),
            ArrowRow { kind } | RowArrow { kind } => json!({
                "code": "arrow_row",
                "message": format!("把函数类型与 {}混在一起", kind_label(*kind)),
            }),
            RecordVariant { .. } => json!({
                "code": "record_variant",
                "message": "记录类型与变体（标签和类型）不能统一",
            }),
            RowClosed { kind, missing, extra } => {
                let mut msg = format!("封闭{}不兼容：", kind_label(*kind));
                if !missing.is_empty() {
                    msg.push_str(&format!("缺少字段 [{}]", missing.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>().join(", ")));
                }
                if !extra.is_empty() {
                    if !missing.is_empty() { msg.push_str("；"); }
                    msg.push_str(&format!("多余字段 [{}]", extra.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>().join(", ")));
                }
                json!({
                    "code": "row_closed",
                    "message": msg,
                    "missing": missing.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>(),
                    "extra": extra.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>(),
                })
            }
            FieldMismatch { kind, field, err } => {
                let mut v = one(err);
                v["message"] = json!(format!("字段 {} 的类型冲突：{}", field, v["message"].as_str().unwrap_or("")));
                v["field"] = json!(field);
                let _ = kind;
                v
            }
        }
    }
    one(err)
}

fn kind_label(k: RowKind) -> &'static str {
    match k { RowKind::Record => "记录", RowKind::Variant => "变体" }
}
