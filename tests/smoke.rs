use std::collections::BTreeSet;
use type_slicer_lib::pipeline::analyze;

fn infer(src: &str) -> type_slicer_lib::solver::SolveResult {
    let a = analyze(src, None, &BTreeSet::new());
    assert!(a.parse_error.is_none(), "parse error: {:?}", a.parse_error);
    a.result.unwrap()
}

#[test]
fn let_polymorphism() {
    let r = infer("let id = fun x -> x in { a = id 1, b = id true }");
    assert!(r.errors.is_empty(), "{:?}", r.errors);
    let root = serde_json::to_string(&r.root_type).unwrap();
    assert!(root.contains("\"a\""));
}

#[test]
fn infinite_type_occurs() {
    let r = infer("let f = fun x -> x x in f");
    assert!(r.errors.iter().any(|e| e.kind == "occurs"), "{:?}", r.errors.iter().map(|e| &e.kind).collect::<Vec<_>>());
}

#[test]
fn value_restriction() {
    let r = infer("let g = id 1 in { a = g, b = g true }");
    // g must be monomorphic: second use conflicts int vs bool
    assert!(!r.errors.is_empty());
}

#[test]
fn row_extension_missing_field() {
    let r = infer("let p = fun r -> r.x in p { y = 1 }");
    let e = r.errors.iter().find(|e| e.kind == "record_row").expect("record_row error");
    assert!(e.missing.iter().any(|m| m == "x"));
    assert!(e.extra.iter().any(|m| m == "y"));
}

#[test]
fn row_extension_polymorphic() {
    // project must be polymorphic in the remaining row
    let r = infer("let getx = fun r -> r.x in { a = getx { x = 1, y = 2 }, b = getx { x = true } }");
    assert!(r.errors.is_empty(), "{:?}", r.errors);
}

#[test]
fn recursive_bind() {
    let src = "let rec loop = fun n -> loop n in loop";
    let r = infer(src);
    // loop is generalized (lambda value): 'a -> 'b type, application fine
    assert!(r.errors.is_empty(), "{:?}", r.errors);
    let s = r.schemes.iter().find(|s| s.role == "loop").unwrap();
    assert!(s.is_value);
}

#[test]
fn shadowing() {
    let r = infer("let x = 1 in let x = true in x");
    assert!(r.errors.is_empty(), "{:?}", r.errors);
}

#[test]
fn multiple_errors_with_cores() {
    let r = infer("{ a = 1 + true, b = true + 2 }");
    assert!(r.errors.len() >= 2, "{:?}", r.errors.len());
    for e in &r.errors {
        assert!(!e.cores.is_empty());
        assert!(!e.cores[0].is_empty());
    }
}
