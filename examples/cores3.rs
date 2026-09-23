use std::collections::BTreeSet;
use type_slicing_bench::analyze::analyze_source;
use type_slicing_bench::solve::CId;
fn main() {
    let src = "fun f -> f f";
    // 只禁用 cid5（第二个 arrow），其余全开（包括 scope/bind）
    let dis = BTreeSet::from([CId(5)]);
    let a = analyze_source(src, &dis);
    println!("disabling cid5 -> {} errors", a.result.errors.len());
    for e in &a.result.errors { println!("  {} cid={}", e.kind, e.cid); }
}
