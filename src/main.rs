//! 类型切片台 binary: serves the inference workbench over HTTP.

use std::net::SocketAddr;
use std::sync::OnceLock;

use type_slicer::store::Store;
use type_slicer::web;

fn usage() -> ! {
    eprintln!("usage: type_slicer --addr 127.0.0.1:5276 [--db path.sqlite]");
    std::process::exit(2);
}

static DB_PATH: OnceLock<String> = OnceLock::new();

#[tokio::main]
async fn main() {
    let mut args = std::env::args().skip(1);
    let mut addr: SocketAddr = "127.0.0.1:5276".parse().unwrap();
    let mut db = "type_slicer.sqlite".to_string();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--addr" => {
                let v = args.next().unwrap_or_else(|| usage());
                addr = v.parse().unwrap_or_else(|_| usage());
            }
            "--db" => {
                db = args.next().unwrap_or_else(|| usage());
            }
            "-h" | "--help" => usage(),
            other => {
                eprintln!("unknown argument: {other}");
                usage();
            }
        }
    }
    let _ = DB_PATH.set(db.clone());

    let mut store = Store::open(&db).expect("open sqlite store");
    store.seed_demos().expect("seed demos");

    println!("类型切片台 listening on http://{addr}  (db: {db})");
    web::serve(store, addr).await;
}
