use std::net::SocketAddr;
use std::sync::Arc;
use type_slicing_bench::{storage::Db, web};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut addr: SocketAddr = "127.0.0.1:5276".parse().unwrap();
    let mut db_path = String::from("type_slicing.db");
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--addr" | "-a" => {
                if let Some(v) = args.next() { addr = v.parse()?; }
            }
            "--db" => {
                if let Some(v) = args.next() { db_path = v; }
            }
            other => eprintln!("忽略未知参数 {}", other),
        }
    }

    let db = Arc::new(Db::open(&db_path)?);
    let app = web::router(db);
    let listener = tokio::net::TcpListener::bind(addr).await?;
    println!("类型切片台已启动：http://{}", listener.local_addr()?);
    axum::serve(listener, app).await?;
    Ok(())
}
