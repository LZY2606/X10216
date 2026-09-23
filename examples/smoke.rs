use std::collections::BTreeSet;
use type_slicing_bench::analyze::analyze_source;

fn main() {
    let cases = [
        ("identity", "let id = fun x -> x in id 1"),
        ("id_bool", "let id = fun x -> x in if id true then 1 else 2"),
        ("occurs", "fun f -> f f"),
        ("vr", "let r = (fun x -> x) 1 in r"),
        ("record", "let p = { X = 1, Y = true } in p.X"),
        ("row_extra", "(fun r -> r.X) { X = 1, Y = 2 }"),
        ("row_miss", "(fun r -> r.Z) { X = 1 }"),
        ("variant", "let x = `A(1) in match x with | `A(n) => n | `B => 0"),
        ("rec", "let rec fact = fun n -> n in fact 5"),
        ("multi_err", "1 + true ; false + 2"),
        ("shadow", "let x = 1 in let x = true in x"),
    ];
    for (name, src) in cases {
        let a = analyze_source(src, &BTreeSet::new());
        println!("=== {} ===", name);
        println!("src: {}", src);
        println!("root: {}", a.result.root_type_str);
        for e in &a.result.errors {
            println!("ERR[{}] cid={} core={:?} : {}", e.kind, e.cid, e.core, e.message);
        }
    }
}
