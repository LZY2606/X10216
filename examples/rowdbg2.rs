use type_slicer::parser;
use type_slicer::gen::{self};
use type_slicer::analysis::IncrementalCache;
fn main() {
    let src = "(fun r -> r.x) { x = 1, y = 2 }";
    let program = parser::parse(src);
    let cache = std::collections::HashMap::new();
    let (g, _) = gen::generate(&program, cache);
    for op in &g.flat_ops {
        println!("{:?}", op);
    }
}
