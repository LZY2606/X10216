use std::collections::BTreeSet;
use type_slicing_bench::analyze::analyze_source;
use type_slicing_bench::parser::parse_pub;
use type_slicing_bench::solve::CId;
fn main() {
    let src = "fun f -> f f";
    let prog = parse_pub(src);
    let full = analyze_source(src, &BTreeSet::new());
    let target = &full.result.errors[0];
    println!("target kind={} cid={} miss={:?} extra={:?} var={:?}",
        target.kind, target.cid, target.missing, target.extra, "(n/a)");
    // 直接用公开 API：禁用 5
    for keep in [vec![1u32,2,3,4], vec![1,2,3,4,5]] {
        let dis: BTreeSet<CId> = (0u32..7).filter(|i| !keep.contains(i)).map(CId).collect();
        let a = analyze_source(src, &dis);
        for e in &a.result.errors {
            println!("keep={:?} -> kind={} cid={}", keep, e.kind, e.cid);
        }
        if a.result.errors.is_empty() { println!("keep={:?} -> no errors", keep); }
    }
    let _ = prog;
}
