use type_slice_bench::web;

#[tokio::main]
async fn main() {
    let addr = std::env::args()
        .position(|a| a == "--addr")
        .and_then(|i| std::env::args().nth(i + 1))
        .unwrap_or_else(|| "127.0.0.1:5276".to_string());
    web::serve(addr, "type-slice-bench.sqlite").await;
}
