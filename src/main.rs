use anyhow::Result;
use std::net::SocketAddr;

#[tokio::main]
async fn main() -> Result<()> {
    let mut addr: SocketAddr = "127.0.0.1:3000".parse()?;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--addr" => addr = args.next().expect("missing address").parse()?,
            other => eprintln!("ignoring unknown argument: {other}"),
        }
    }
    let pool = type_slicer::db::Database::open("type-slicer.sqlite")?;
    let listener = tokio::net::TcpListener::bind(addr).await?;
    println!("类型切片台 listening on http://{addr}");
    type_slicer::web::serve(listener, pool).await?;
    Ok(())
}
