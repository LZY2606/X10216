use std::collections::HashSet;
use type_slicer::analysis::{self, IncrementalCache};
fn main() {
    let src = "(fun r -> r.x) { x = 1, y = 2 }";
    let mut cache = IncrementalCache::default();
    let res = analysis::analyze(src, &mut cache, &HashSet::new(), None, None);
    for st in &res.steps {
        if st.kind.contains("unify") {
            println!("{:6} {}", st.kind, st.summary);
        }
    }
    for e in &res.errors { println!("ERR {}", e.detail); }
}
