use type_slicer::analysis::analyze;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // 占位：稍后接入 Axum
    let _ = analyze("let id = fun x -> x\nid 3");
    println!("type-slicer bootstrap");
    Ok(())
}
