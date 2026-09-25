use type_slicing_bench::syntax::parse;
use type_slicing_bench::infer::{Engine, render_type_compact};

fn main() {
    let src = "let id = fun x -> x\nlet a = id 1\nlet b = id true";
    let ast = parse(src);
    for e in &ast.errors { eprintln!("parse: {}", e.message); }
    let eng = Engine::new(&ast).run();
    for bt in &eng.binding_types {
        println!("{} :: {}", bt.name, render_type_compact(&bt.ty));
    }
    for e in &eng.errors {
        println!("ERR {}: {:?}", e.kind, e.message);
    }
}
