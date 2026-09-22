use type_slice_bench::*;
fn main() {
    let b = build_response("1 true", &[]).unwrap();
    let r = &b.response;
    println!("ok={} errors={}", r.ok, r.errors.len());
    println!("stats {:?}", (r.stats.instantiations, r.stats.generalizations, r.stats.unifications, r.stats.errors));
    println!("steps={}", r.steps.len());
    for s in &r.steps {
        if !s.ok || s.kind=="unify" {
            println!("step {} cid={:?} ok={} label={}", s.kind, s.cid, s.ok, s.label);
        }
    }
    println!("type_nodes={}", r.type_nodes.len());
    for t in &r.type_nodes { println!("  node {} concrete={} expr={:?}", t.id, t.concrete, t.expr); }
    println!("constraints:");
    for c in &r.ast {
        println!("  c{} kind={} {:?} span={:?}", c.id, c.kind, c.eq.as_ref().map(|e|(e.left,e.right,e.label.clone())), c.span);
    }
}
