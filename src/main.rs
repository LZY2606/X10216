use type_slicer::infer::{Inferer, Step};
use type_slicer::syntax::parse;
use type_slicer::typing::{show_ty, Uf};

fn run(src: &str) {
    println!("=== {src}");
    let e = match parse(src) { Ok(e)=>e, Err((s,m))=>{println!("parse {s:?} {m}");return;}};
    let uf = Uf::new();
    let mut inf = Inferer::new(&uf);
    let (ty, errs) = inf.infer_program(&e);
    println!("type = {}", show_ty(&ty));
    for c in &errs { println!("  ERR: {}", c.message()); }
    let _ = Step::Record { at:0,node:String::new(),fields:vec![],span:type_slicer::syntax::Span{lo:0,hi:0}};
}
fn main() {
    run("let id = fun x | x in id 3");
    run("let id = fun x | x in id id");
    run("fun f | f 3");
    run("let r = { a = 1, b = 2 } in r.a");
    run("let r = { a = 1 } in r.b");
    run("fun x | x x");
    run("let k = fun y | fun z | y in k");
    run("let rec fact = fun n | n in fact");
    run("let v = (fun x | x) 1 in v");
    run("fun r | r.a + r.b");
    run("#Some 3 match < |#Some x: x|#None: 0>");
    run("fun f | f { a = 1 }");
    run("let g = fun r | r.a in g { a = 1, b = 2 }");
}
