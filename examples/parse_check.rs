fn main() {
    let p = type_slicing_bench::parser::parse_pub("match x with | `A(n) => n");
    println!("errors: {:?}", p.errors);
    fn dump(e: &type_slicing_bench::ast::Expr, d: usize) {
        println!("{}{:?}", " ".repeat(d*2), e.kind());
        use type_slicing_bench::ast::Expr::*;
        if let Match { arms, .. } = e {
            for a in arms { println!("    arm {} payload={:?}", a.label, a.payload); }
        }
        let mut ks = vec![];
        e.children(&mut ks);
        for k in ks { dump(k, d+1); }
    }
    dump(&p.root, 0);
}
