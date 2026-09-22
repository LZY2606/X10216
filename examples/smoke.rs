use std::collections::HashSet;
use type_slicer::analysis::{self, IncrementalCache};

fn main() {
    let cases = [
        ("let id = fun x -> x in (id 1) + 1", "poly-int"),
        ("fun f -> f f", "infinite"),
        ("let r = { a = 1 } in r.b", "missing-field"),
        ("let get = fun r -> r.x in get { x = 1, y = 2 }", "row-poly"),
        ("(1 + true)", "mismatch"),
        ("let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1) in fact 5", "fact"),
        ("let v = Some(1) in match v with | Some n -> n | None -> 0", "variant-match"),
        ("let r = (fun x -> x) 1 in r == true", "value-restriction"),
        ("let x = 1 in let x = true in x", "shadow"),
    ];
    for (src, tag) in cases {
        let mut cache = IncrementalCache::default();
        let r = analysis::analyze(src, &mut cache, &HashSet::new(), None, None);
        let errs: Vec<String> = r.errors.iter().map(|e| format!("{}: {}", e.kind, e.message)).collect();
        println!("== {tag}");
        println!("   type: {}", r.final_type_string);
        println!("   ops: {}, constraints: {}, steps: {}", r.total_ops, r.constraint_nodes.len(), r.steps.len());
        if errs.is_empty() {
            println!("   errors: none");
        } else {
            for e in &errs {
                println!("   error: {e}");
            }
            for e in &r.errors {
                println!("     cores: {:?}", e.cores);
            }
        }
    }
}
