use type_slicing_bench::{analysis, syntax::Parser, constraints::Generator};
use std::collections::BTreeSet;

fn main() {
    let src = std::env::args().nth(1).unwrap_or_else(|| "fn x -> x x".into());
    let o = Parser::parse_program(&src, 0).unwrap();
    let evs = Generator::run(&o.root);
    for e in &evs {
        println!("{:?}", e);
    }
    println!("--- solve with depth guard ---");
    let v = analysis::analyze(&src, None, &BTreeSet::new());
    println!("errors {}", v.result.errors.len());
    for s in &v.result.steps {
        println!("step {} {} {}", s.index, s.kind, s.error.clone().unwrap_or_default());
        for u in &s.unifications {
            println!("   {} ~ {} -> {}", u.a, u.b, u.root);
        }
    }
}
