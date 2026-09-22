mod astmap;
mod constraints;
mod parser;
mod service;
mod solver;
mod storage;
mod syntax;
mod types;
mod web;

use std::sync::Arc;

#[tokio::main]
async fn main() {
    let addr = std::env::args().nth(1).unwrap_or_else(|| "--addr".into());
    let value = std::env::args().nth(2).unwrap_or_else(|| "127.0.0.1:5276".into());
    let addr = if addr == "--addr" { value } else { addr };
    let store = Arc::new(storage::Store::open("type_slicer.sqlite").expect("打开 SQLite 数据库"));
    let listener = tokio::net::TcpListener::bind(&addr).await.expect("绑定监听地址");
    println!("类型切片台: http://{addr}");
    axum::serve(listener, web::router(store)).await.expect("启动 HTTP 服务");
}
