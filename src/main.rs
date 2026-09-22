use clap::Parser;
use type_slicer_lib::server::app_from_path;

#[derive(Parser, Debug)]
#[command(name = "type-slicer", about = "类型切片台：HM 推断过程可视化")]
struct Args {
    /// 监听地址
    #[arg(long, default_value = "127.0.0.1:5276")]
    addr: String,
    /// SQLite 数据库路径
    #[arg(long, default_value = "type-slicer.db")]
    db: String,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();
    let app = app_from_path(&args.db);
    let listener = tokio::net::TcpListener::bind(&args.addr)
        .await
        .unwrap_or_else(|e| panic!("无法监听 {}：{e}", args.addr));
    println!("类型切片台已启动：http://{}", args.addr);
    axum::serve(listener, app).await.expect("服务器异常退出");
}
