use std::collections::BTreeSet;
use type_slicing_bench::analyze::analyze_source;
fn main() {
    for src in ["fun f -> f f", "(fun r -> r.Z) { X = 1 }"] {
        let a = analyze_source(src, &BTreeSet::new());
        println!("== {}", src);
        for c in &a.result.constraints {
            println!("cid={:2} node={:2} {:8} | {}", c.id, c.node, c.kind, c.label);
        }
        for e in &a.result.errors {
            println!("ERR cid={} core={:?} cands={:?}", e.cid, e.core, e.candidates);
        }
    }
}
