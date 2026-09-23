use type_slice_bench::service::analyze_source;
use std::collections::HashSet;
fn r(s:&str){ let a=analyze_source(s,&HashSet::new(),None); println!("SRC {s}\n TYPE {}\n OK {} PARSE {:#?} ERR {:#?}\n NOTICES {:#?}", a.result_type,a.ok,a.parse_error,a.errors.iter().map(|e|(e.kind.clone(),e.message.clone(),e.missing_fields.clone(),e.extra_fields.clone())).collect::<Vec<_>>(),a.notices.iter().map(|n|n.message.clone()).collect::<Vec<_>>()); }
#[test] fn smoke(){
r("let get = fn p => p.x in get({x=1,y=true})");
r("let f = fn p => p.x in ({x=1}.y)");
r("let f = fn p => p.x in f({x=1}) + f({x=true})");
r("let id = fn x => x in id(1)");
r("let bad = id(1) in bad + true");
r("let rec loop = fn x => loop(x) in loop(1)");
r("match #A(1) { #A(x) => x }");
r("match #B(1) { #A(x) => x; _ => 2 }");
}
