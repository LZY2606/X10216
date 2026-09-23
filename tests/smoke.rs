use type_slicing_bench::analysis;
use std::collections::BTreeSet;

fn run(src: &str) -> analysis::AnalysisView {
    analysis::analyze(src, None, &BTreeSet::new())
}

#[test]
fn smoke_let_poly() {
    let v = run("let id = fn x -> x in { a = id 1, b = id true }");
    if let Some(e) = &v.parse_error { panic!("parse: {}", e.message); }
    for er in &v.result.errors {
        panic!("unexpected error: {}", er.error.message);
    }
    assert!(v.result.ok);
}

#[test]
fn smoke_occurs() {
    let v = run("fn x -> x x");
    for er in &v.result.errors {
        println!("ERR: {} kind={:?}", er.error.message, er.error.kind);
    }
    assert!(!v.result.ok);
}

#[test]
fn smoke_value_restriction() {
    let h = "let helper = fn f -> f in let r = helper (fn x -> x) in { a = r 1, b = r true }";
    let v = run(h);
    for er in &v.result.errors {
        println!("ERR: {} kind={:?}", er.error.message, er.error.kind);
    }
    assert!(!v.result.ok, "value restriction should prevent polymorphic use");
}

#[test]
fn smoke_row_missing_extra() {
    let missing = run("let get = fn r -> r.a in get { b = 1 }");
    assert!(!missing.result.ok);
    let extra = run("let get = fn r -> r.a in get { a = 1, b = 2 }");
    for er in &extra.result.errors {
        println!("EXTRA ERR: {} kind={:?}", er.error.message, er.error.kind);
    }
}
