use type_slicing_bench::syntax::parse;
fn main() {
    let src = std::env::args().nth(1).unwrap_or_else(|| {
        "let id = fun x -> x in id id".into()
    });
    let ast = match parse(&src) {
        Ok(a) => a,
        Err(e) => { eprintln!("PARSE ERROR: {}", e); std::process::exit(1); }
    };
    for n in &ast.nodes {
        let label = n.label.clone().map(|l| format!(" label={}", l)).unwrap_or_default();
        let name = n.name.clone().map(|l| format!(" name={}", l)).unwrap_or_default();
        println!("#{} {:?} span={}..{} path=[{}]{}{}", n.id, n.kind, n.span.lo, n.span.hi, n.path, name, label);
    }
}
