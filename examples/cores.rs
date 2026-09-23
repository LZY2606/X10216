use std::collections::BTreeSet;
use type_slicing_bench::analyze::analyze_source;
fn main() { dbg2();
    let src_dummy = ""; let _ = src_dummy;
    let src = "fun f -> f f";
    for set in [
        vec![1u32,2,4,5], vec![1,3,4,5], vec![2,3,4,5], vec![1,2,3,4],
        vec![2,4,5], vec![1,2,4], vec![3,4,5],
    ] {
        let dis: BTreeSet<_> = (0u32..7).filter(|i| !set.contains(i)).map(type_slicing_bench::solve::CId).collect();
        let a = analyze_source(src, &dis);
        println!("active {:?} => errs: {:?}", set,
            a.result.errors.iter().map(|e| (e.kind.clone(), e.cid)).collect::<Vec<_>>());
    }
}
#[allow(dead_code)]
fn dbg2(){
    let src = "fun f -> f f";
    let dis: BTreeSet<_> = vec![0u32,5,6].into_iter().map(type_slicing_bench::solve::CId).collect();
    let a = analyze_source(src, &dis);
    for e in &a.result.errors { println!("{:?} cid={} msg={}", e.kind, e.cid, e.message); }
}
