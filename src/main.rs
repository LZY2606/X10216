use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::Html,
    routing::{get, post},
    Json, Router,
};
use serde::Deserialize;
use std::path::PathBuf;
use std::sync::Arc;
use type_slicer::analyze::{self, Analysis};
use type_slicer::store::Store;

struct AppState {
    store: Store,
}

type ApiResult<T> = Result<Json<T>, (StatusCode, String)>;

fn err(msg: String) -> (StatusCode, String) {
    (StatusCode::BAD_REQUEST, msg)
}

async fn index() -> Html<&'static str> {
    Html(include_str!("../static/index.html"))
}

async fn create_session(State(st): State<Arc<AppState>>) -> ApiResult<serde_json::Value> {
    let id = st.store.create_session().map_err(err)?;
    Ok(Json(serde_json::json!({ "session_id": id })))
}

#[derive(Deserialize)]
struct AnalyzeReq {
    source: String,
}

async fn analyze_version(
    State(st): State<Arc<AppState>>,
    Path(session_id): Path<i64>,
    Json(req): Json<AnalyzeReq>,
) -> ApiResult<serde_json::Value> {
    let analysis = analyze::analyze(&req.source).map_err(err)?;
    let vid = st
        .store
        .add_version(session_id, &req.source, &analysis)
        .map_err(err)?;
    Ok(Json(serde_json::json!({
        "version_id": vid,
        "analysis": analysis,
    })))
}

async fn get_session(
    State(st): State<Arc<AppState>>,
    Path(session_id): Path<i64>,
) -> ApiResult<serde_json::Value> {
    let versions = st.store.list_versions(session_id).map_err(err)?;
    Ok(Json(serde_json::json!({ "versions": versions })))
}

async fn get_version(
    State(st): State<Arc<AppState>>,
    Path(version_id): Path<i64>,
) -> ApiResult<serde_json::Value> {
    let (source, analysis) = st.store.get_version(version_id).map_err(err)?;
    Ok(Json(serde_json::json!({ "source": source, "analysis": analysis })))
}

#[derive(Deserialize)]
struct DiffQuery {
    against: i64,
}

async fn diff_versions(
    State(st): State<Arc<AppState>>,
    Path(version_id): Path<i64>,
    Query(q): Query<DiffQuery>,
) -> ApiResult<serde_json::Value> {
    let (_, a) = st.store.get_version(version_id).map_err(err)?;
    let (_, b) = st.store.get_version(q.against).map_err(err)?;
    let d = analyze::diff(&a, &b);
    Ok(Json(serde_json::to_value(d).unwrap()))
}

#[derive(Deserialize)]
struct DisableReq {
    constraint_id: String,
}

async fn disable_constraint(
    State(st): State<Arc<AppState>>,
    Path(version_id): Path<i64>,
    Json(req): Json<DisableReq>,
) -> ApiResult<serde_json::Value> {
    let (source, _) = st.store.get_version(version_id).map_err(err)?;
    let r = analyze::analyze_with_disabled(&source, &req.constraint_id).map_err(err)?;
    Ok(Json(serde_json::to_value(r).unwrap()))
}

#[derive(Deserialize)]
struct StepsQuery {
    from: Option<usize>,
    to: Option<usize>,
}

async fn get_steps(
    State(st): State<Arc<AppState>>,
    Path(version_id): Path<i64>,
    Query(q): Query<StepsQuery>,
) -> ApiResult<serde_json::Value> {
    let (_, analysis): (String, Analysis) = st.store.get_version(version_id).map_err(err)?;
    let from = q.from.unwrap_or(0);
    let to = q.to.unwrap_or(analysis.steps.len());
    let steps: Vec<_> = analysis
        .steps
        .iter()
        .filter(|s| s.index >= from && s.index < to)
        .collect();
    Ok(Json(serde_json::json!({ "steps": steps, "total": analysis.steps.len() })))
}

#[tokio::main]
async fn main() {
    let mut addr = "127.0.0.1:5276".to_string();
    let mut db = "type-slicer.db".to_string();
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--addr" => addr = args.next().unwrap_or(addr),
            "--db" => db = args.next().unwrap_or(db),
            _ => {}
        }
    }
    let store = Store::open(&PathBuf::from(&db)).expect("open sqlite");
    let state = Arc::new(AppState { store });

    let app = Router::new()
        .route("/", get(index))
        .route("/api/sessions", post(create_session))
        .route("/api/sessions/{id}", get(get_session))
        .route("/api/sessions/{id}/analyze", post(analyze_version))
        .route("/api/versions/{id}", get(get_version))
        .route("/api/versions/{id}/diff", get(diff_versions))
        .route("/api/versions/{id}/disable", post(disable_constraint))
        .route("/api/versions/{id}/steps", get(get_steps))
        .with_state(state);

    let listener = tokio::net::TcpListener::bind(&addr).await.expect("bind");
    println!("类型切片台 listening on http://{}", addr);
    axum::serve(listener, app).await.unwrap();
}
