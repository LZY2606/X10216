use type_slice_bench::*;

fn analyze(src: &str) -> AnalysisResponse {
    match build_response(src, &[]) {
        Ok(b) => b.response,
        Err(e) => panic!("parse error: {} {:?}", e.message, e.span),
    }
}

#[test]
fn let_polymorphism_infers_id() {
    let r = analyze("let id = fun x -> x in { a = id 1, b = id true }");
    assert!(r.ok, "errors: {:?}", r.errors.iter().map(|e| &e.message).collect::<Vec<_>>());
}
