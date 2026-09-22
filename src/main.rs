use type_slice::server::serve;

#[tokio::main]
async fn main() {
    let addr = std::env::args()
        .position(|arg| arg == "--addr")
        .and_then(|index| std::env::args().nth(index + 1))
        .unwrap_or_else(|| "127.0.0.1:5276".to_string());
    serve(&addr, "type_slice.sqlite").await;
}
