//! Axum HTTP API and embedded single-page UI.

use crate::pipeline::{analyze, Analysis};
use crate::storage::{open, Store, VersionRow};
use axum::{
    extract::Path,
    http::StatusCode,
    response::{Html, IntoResponse},
    routing::{get, post},
    Extension, Json, Router,
};
use serde_json::{json, Value};
use std::collections::{BTreeSet, HashMap};
use std::sync::{Arc, Mutex};

pub const DEMO_SOURCE: &str = r#"-- 类型切片台演示：let 多态、函数、record、variant、递归
let id = fun x -> x in
let apply = fun f -> fun x -> f x in
let inc = fun n -> n + 1 in
let twice = fun f -> fun x -> f (f x) in

let make = fun n -> { age = twice inc n, tag = `Ok("ready") } in

let rec is_adult = fun p ->
  match p.tag with
  | `Ok msg -> p.age < 100
  | `Err _ -> false
  in

is_adult (make 18)
"#;

#[derive(Clone)]
pub struct AppState {
    pub store: Arc<Store>,
    cache: Arc<Mutex<HashMap<String, Arc<Analysis>>>>,
}

fn cache_key(source: &str, disabled: &[String]) -> String {
    format!("{:x}:{}", source.len(), disabled.join(","))
}

fn run_analysis(
    state: &AppState,
    source: &str,
    prev_source: Option<&str>,
    disabled: &BTreeSet<String>,
) -> Arc<Analysis> {
    let key = cache_key(source, &disabled.iter().cloned().collect::<Vec<_>>());
    if let Some(a) = state.cache.lock().unwrap().get(&key) {
        return a.clone();
    }
    let prev_program = prev_source.and_then(|src| {
        analyze(src, None, &BTreeSet::new()).program
    });
    let a = Arc::new(analyze(source, prev_program.as_ref(), disabled));
    state.cache.lock().unwrap().insert(key, a.clone());
    a
}

async fn index() -> impl IntoResponse {
    Html(include_str!("../../web/index.html"))
}

async fn list_sessions(Extension(st): Extension<AppState>) -> axum::response::Response {
    match st.store.list_sessions() {
        Ok(rows) => {
            let list: Vec<Value> = rows
                .into_iter()
                .map(|(id, title)| json!({ "id": id, "title": title }))
                .collect();
            Json(json!({ "sessions": list })).into_response()
        }
        Err(e) => internal(e),
    }
}

async fn create_session(Extension(st): Extension<AppState>, Json(body): Json<Value>) -> impl IntoResponse {
    let title = body.get("title").and_then(|v| v.as_str()).unwrap_or("未命名会话");
    match st.store.create_session(title) {
        Ok(id) => (StatusCode::OK, Json(json!({ "id": id }))).into_response(),
        Err(e) => internal(e),
    }
}

async fn list_versions(Extension(st): Extension<AppState>, Path(sid): Path<i64>) -> axum::response::Response {
    match st.store.list_versions(sid) {
        Ok(rows) => {
            let list: Vec<Value> = rows.iter().map(version_meta).collect();
            Json(json!({ "session": sid, "versions": list })).into_response()
        }
        Err(e) => internal(e),
    }
}

fn version_meta(v: &VersionRow) -> Value {
    json!({
        "id": v.id,
        "label": v.label,
        "created_at": v.created_at,
        "cursor": v.step_cursor,
        "disabled": serde_json::from_str::<Value>(&v.disabled).unwrap_or(json!([])),
    })
}

async fn add_version(
    Extension(st): Extension<AppState>,
    Path(sid): Path<i64>,
    Json(body): Json<Value>,
) -> impl IntoResponse {
    let source = body.get("source").and_then(|v| v.as_str()).unwrap_or("");
    let label = body.get("label").and_then(|v| v.as_str()).unwrap_or("新版本");
    match st.store.add_version(sid, source, label) {
        Ok(id) => (StatusCode::OK, Json(json!({ "id": id }))).into_response(),
        Err(e) => internal(e),
    }
}

async fn analyze_version(Extension(st): Extension<AppState>, Path(vid): Path<i64>) -> impl IntoResponse {
    let row = match st.store.get_version(vid) {
        Ok(Some(r)) => r,
        Ok(None) => return (StatusCode::NOT_FOUND, Json(json!({"error":"版本不存在"}))).into_response(),
        Err(e) => return internal(e),
    };
    let disabled_list: Vec<String> =
        serde_json::from_str(&row.disabled).unwrap_or_default();
    let disabled: BTreeSet<String> = disabled_list.into_iter().collect();

    let prev_source = {
        let conn = st.store.conn.lock().unwrap();
        conn.query_row(
            "SELECT source FROM versions WHERE id < ?1 ORDER BY id DESC LIMIT 1",
            rusqlite::params![vid],
            |r| r.get::<_, String>(0),
        )
        .ok()
    };

    let analysis = run_analysis(&st, &row.source, prev_source.as_deref(), &disabled);
    Json(json!({
        "version": version_meta(&row),
        "source": row.source,
        "analysis": &*analysis,
    }))
    .into_response()
}

async fn set_disabled(
    Extension(st): Extension<AppState>,
    Path(vid): Path<i64>,
    Json(body): Json<Value>,
) -> impl IntoResponse {
    let arr = body.get("disabled").and_then(|v| v.as_array()).cloned().unwrap_or_default();
    let list: Vec<String> = arr.iter().filter_map(|v| v.as_str().map(String::from)).collect();
    let json_text = serde_json::to_string(&list).unwrap_or_else(|_| "[]".into());
    match st.store.set_disabled(vid, &json_text) {
        Ok(()) => Json(json!({"ok": true})).into_response(),
        Err(e) => internal(e),
    }
}

async fn set_cursor(
    Extension(st): Extension<AppState>,
    Path(vid): Path<i64>,
    Json(body): Json<Value>,
) -> impl IntoResponse {
    let cursor = body.get("cursor").and_then(|v| v.as_i64()).unwrap_or(0);
    match st.store.set_cursor(vid, cursor) {
        Ok(()) => Json(json!({"ok": true})).into_response(),
        Err(e) => internal(e),
    }
}

async fn scratch(Json(body): Json<Value>) -> impl IntoResponse {
    let source = body.get("source").and_then(|v| v.as_str()).unwrap_or("");
    let prev_source = body.get("prev").and_then(|v| v.as_str());
    let disabled: BTreeSet<String> = body
        .get("disabled")
        .and_then(|v| v.as_array())
        .map(|a| a.iter().filter_map(|x| x.as_str().map(String::from)).collect())
        .unwrap_or_default();
    let prev_program = prev_source.and_then(|src| analyze(src, None, &BTreeSet::new()).program);
    let analysis = analyze(source, prev_program.as_ref(), &disabled);
    Json(json!({ "analysis": analysis }))
}

fn internal(e: rusqlite::Error) -> axum::response::Response {
    (StatusCode::INTERNAL_SERVER_ERROR, Json(json!({ "error": e.to_string() }))).into_response()
}

pub fn build_app(store: Store) -> Router {
    let state = AppState {
        store: Arc::new(store),
        cache: Arc::new(Mutex::new(HashMap::new())),
    };
    Router::new()
        .route("/", get(index))
        .route("/index.html", get(index))
        .route("/api/sessions", get(list_sessions).post(create_session))
        .route("/api/sessions/:sid/versions", get(list_versions).post(add_version))
        .route("/api/versions/:vid/analyze", get(analyze_version))
        .route("/api/versions/:vid/disabled", post(set_disabled))
        .route("/api/versions/:vid/cursor", post(set_cursor))
        .route("/api/scratch", post(scratch))
        .layer(Extension(state))
}

pub fn app_from_path(db_path: &str) -> Router {
    let store = open(db_path).expect("打开数据库失败");
    store.ensure_demo(DEMO_SOURCE).expect("初始化演示会话失败");
    build_app(store)
}
