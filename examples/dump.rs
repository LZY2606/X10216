use std::collections::BTreeSet;
use type_slicing_bench::analyze::analyze_source;
fn main() {
    let src = "let x = `A(1) in match x with | `A(n) => n | `B => 0";
    let a = analyze_source(src, &BTreeSet::new());
    for c in &a.result.constraints {
        println!("cid={:2} node={:2} {} | {}", c.id, c.node, c.kind, c.label);
    }
    for s in &a.result.steps {
        println!("step {:2} cid={:2} {} | {} | ok={}", s.index, s.cid, s.kind, s.title, s.ok);
    }
}
