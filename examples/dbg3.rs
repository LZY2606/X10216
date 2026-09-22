fn main() {
    println!("start");
    match type_slice_bench::parser::parse("fun x -> x x") {
        Ok(e) => {
            println!("parsed {:?}", e.span());
            let gen = type_slice_bench::generate::Generator::new("fun x -> x x");
            let g = gen.generate(&e);
            println!("generated {} constraints", g.graph.nodes.len());
            let cfg = type_slice_bench::solver::SolveConfig::default();
            let a = type_slice_bench::solver::Solver::analyze(
                &g.graph, &g.bases, &g.flexes, &g.ambient_inst_roots, &g.scheme_insts, &cfg);
            println!("solved errors={}", a.errors.len());
        }
        Err(e) => println!("parse err {}", e.msg),
    }
}
