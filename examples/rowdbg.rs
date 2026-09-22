use std::collections::HashSet;
use type_slicer::analysis::{self, IncrementalCache};
fn main() {
    for src in [
        "let get = fun r -> r.x in get { x = 1, y = 2 }",
        "(fun r -> r.x) { x = 1, y = 2 }",
        "let r = { x = 1, y = 2 } in r.x",
    ] {
        let mut cache = IncrementalCache::default();
        let res = analysis::analyze(src, &mut cache, &HashSet::new(), None, None);
        println!("SRC: {src}\n  type: {}", res.final_type_string);
        for e in &res.errors { println!("  ERR {} {} cores={:?}", e.kind, e.message, e.cores); }
    }
}
