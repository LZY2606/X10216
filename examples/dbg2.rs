use type_slice_bench::*;
fn main() {
    let b = build_response("fun x -> x x", &[]).unwrap();
    println!("errors={}", b.response.errors.len());
    for e in &b.response.errors { println!("{} {}", e.kind, e.message); }
}
