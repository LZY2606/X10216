mod syntax;
mod types;
mod constraints;
mod solver;
mod identity;
mod analysis;
mod incremental;
mod storage;
mod web;

use std::net::SocketAddr;
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut addr: SocketAddr = "127.0.0.1:5276".parse()?;
    let mut db_path = "type_slicing_bench.sqlite".to_string();
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--addr" | "-a" => {
                if let Some(v) = args.next() {
                    addr = v.parse()?;
                }
            }
            "--db" => {
                if let Some(v) = args.next() {
                    db_path = v;
                }
            }
            "--help" | "-h" => {
                println!("类型切片台\n用法: type-slicing-bench [--addr 127.0.0.1:5276] [--db PATH]");
                return Ok(());
            }
            other => {
                // 也允许直接传 addr
                if let Ok(parsed) = other.parse::<SocketAddr>() {
                    addr = parsed;
                } else {
                    eprintln!("未知参数: {other}");
                }
            }
        }
    }

    let store = Arc::new(storage::Store::open(&db_path)?);
    let app = web::router(store);
    let listener = tokio::net::TcpListener::bind(addr).await?;
    println!("类型切片台已启动：http://{addr}");
    axum::serve(listener, app).await?;
    Ok(())
}
