//! Axum web layer and embedded static UI.

pub mod demos;

use std::collections::HashSet;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};

use axum::{
    extract::{Path, State},
    http::{header, StatusCode},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
    Json, Router,
};
use rust_embed::RustEmbed;
use serde::Deserialize;
use serde_json::json;

use crate::analysis::{self, IncrementalCache};
use crate::store::{Session, Store};

#[derive(RustEmbed)]
#[folder = "static/"]
struct Asset;

#[derive(Default)]
struct Caches {
    /// one incremental cache per session id; each version labeled A/B.
    a: std::collections::HashMap<String, IncrementalCache>,
    b: std::collections::HashMap<String, IncrementalCache>,
    /// constraint-id sets of the other version for diffing.
    prev_a: std::collections::HashMap<String, HashSet<u64>>,
    prev_b: std::collections::HashMap<String, HashSet<u64>>,
}

#[derive(Clone)]
struct AppState {
    store: Arc<Mutex<Store>>,
    caches: Arc<Mutex<Caches>>,
}

#[derive(Deserialize)]
struct AnalyzeReq {
    source: String,
    #[serde(default)]
    disabled: Vec<u64>,
    #[serde(default)]
    cursor: Option<usize>,
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    diff: bool,
}

#[derive(Deserialize)]
struct SaveReq {
    #[serde(default)]
    id: Option<String>,
    title: String,
    source_a: String,
    source_b: String,
    active: String,
    #[serde(default)]
    disabled: Vec<u64>,
    #[serde(default)]
    cursor: Option<usize>,
}

fn cache_map<'a>(c: &'a mut Caches, version: &str) -> &'a mut std::collections::HashMap<String, IncrementalCache> {
    if version == "B" {
        &mut c.b
    } else {
        &mut c.a
    }
}

fn prev_map<'a>(c: &'a mut Caches, version: &str) -> &'a mut std::collections::HashMap<String, HashSet<u64>> {
    if version == "B" {
        &mut c.prev_b
    } else {
        &mut c.prev_a
    }
}

async fn index() -> impl IntoResponse {
    match Asset::get("index.html") {
        Some(f) => Html(String::from_utf8_lossy(f.data.as_ref()).to_string()),
        None => Html("<h1>类型切片台</h1><p>UI missing</p>".into()),
    }
}

async fn static_path(Path(path): Path<String>) -> Response {
    let path = path.trim_start_matches('/');
    match Asset::get(path) {
        Some(f) => {
            let mime = mime_for(path);
            Response::builder()
                .header(header::CONTENT_TYPE, mime)
                .body(axum::body::Body::from(f.data.into_owned()))
                .unwrap()
        }
        None => (StatusCode::NOT_FOUND, "not found").into_response(),
    }
}

fn mime_for(path: &str) -> &'static str {
    if path.ends_with(".js") {
        "application/javascript"
    } else if path.ends_with(".css") {
        "text/css"
    } else if path.ends_with(".svg") {
        "image/svg+xml"
    } else {
        "text/plain"
    }
}

async fn api_analyze(State(st): State<AppState>, Json(req): Json<AnalyzeReq>) -> Json<serde_json::Value> {
    let version = req.version.clone().unwrap_or_else(|| "A".into());
    let session_id = "scratch".to_string();
    let disabled: HashSet<u64> = req.disabled.iter().copied().collect();

    let mut caches = st.caches.lock().unwrap();

    // previous constraint ids (before this edit) for diffing
    let previous = if req.diff {
        prev_map(&mut caches, &version).get(&session_id).cloned()
    } else {
        None
    };

    let cache = cache_map(&mut caches, &version)
        .entry(session_id.clone())
        .or_default();
    let result = analysis::analyze(&req.source, cache, &disabled, req.cursor, previous.as_ref());
    let current: HashSet<u64> = result.constraint_nodes.iter().map(|c| c.id).collect();
    prev_map(&mut caches, &version).insert(session_id, current);

    let parse_issues = analysis::parse_issues(&req.source);
    Json(json!({ "result": result, "parse_issues": parse_issues }))
}

async fn api_sessions(State(st): State<AppState>) -> Json<serde_json::Value> {
    let store = st.store.lock().unwrap();
    let sessions = store.list().unwrap_or_default();
    Json(json!({ "sessions": sessions }))
}

async fn api_get_session(State(st): State<AppState>, Path(id): Path<String>) -> Json<serde_json::Value> {
    let store = st.store.lock().unwrap();
    match store.get(&id).unwrap_or(None) {
        Some(s) => Json(json!({ "session": s })),
        None => Json(json!({ "error": "not found" })),
    }
}

#[derive(Deserialize)]
struct SessionStepReq {
    #[serde(default)]
    disabled: Vec<u64>,
    #[serde(default)]
    cursor: Option<usize>,
    #[serde(default)]
    version: Option<String>,
}

async fn api_session_analyze(
    State(st): State<AppState>,
    Path(id): Path<String>,
    Json(req): Json<SessionStepReq>,
) -> Json<serde_json::Value> {
    let session = {
        let store = st.store.lock().unwrap();
        store.get(&id).unwrap_or(None)
    };
    let Some(mut session) = session else {
        return Json(json!({ "error": "session not found" }));
    };
    session.disabled = req.disabled;
    session.cursor = req.cursor;
    session.active = req.version.unwrap_or(session.active.clone());
    {
        let mut store = st.store.lock().unwrap();
        let _ = store.upsert(&session);
    }

    let source = if session.active == "B" { &session.source_b } else { &session.source_a };
    let disabled: HashSet<u64> = session.disabled.iter().copied().collect();

    let mut caches = st.caches.lock().unwrap();
    let other_version = if session.active == "B" { "A" } else { "B" };
    let other = prev_map(&mut caches, other_version).get(&id).cloned();
    let cache = cache_map(&mut caches, &session.active)
        .entry(id.clone())
        .or_default();
    let result = analysis::analyze(source, cache, &disabled, session.cursor, other.as_ref());
    let current: HashSet<u64> = result.constraint_nodes.iter().map(|c| c.id).collect();
    prev_map(&mut caches, &session.active).insert(id.clone(), current);

    Json(json!({ "result": result, "session": session }))
}

async fn api_save(State(st): State<AppState>, Json(req): Json<SaveReq>) -> Json<serde_json::Value> {
    let id = req.id.filter(|s| !s.is_empty()).unwrap_or_else(|| {
        use std::time::{SystemTime, UNIX_EPOCH};
        let nanos = SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_nanos()).unwrap_or(0);
        format!("s-{nanos:x}")
    });
    let session = Session {
        id: id.clone(),
        title: req.title,
        source_a: req.source_a,
        source_b: req.source_b,
        active: if req.active == "B" { "B".into() } else { "A".into() },
        disabled: req.disabled,
        cursor: req.cursor,
        created_at: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs() as i64)
            .unwrap_or(0),
    };
    let mut store = st.store.lock().unwrap();
    match store.upsert(&session) {
        Ok(()) => Json(json!({ "ok": true, "id": id, "session": session })),
        Err(e) => Json(json!({ "ok": false, "error": e.to_string() })),
    }
}

async fn api_delete(State(st): State<AppState>, Path(id): Path<String>) -> Json<serde_json::Value> {
    let mut store = st.store.lock().unwrap();
    match store.delete(&id) {
        Ok(_) => Json(json!({ "ok": true })),
        Err(e) => Json(json!({ "ok": false, "error": e.to_string() })),
    }
}

pub fn build_router(store: Store) -> Router {
    let state = AppState {
        store: Arc::new(Mutex::new(store)),
        caches: Arc::new(Mutex::new(Caches::default())),
    };
    Router::new()
        .route("/", get(index))
        .route("/static/{path}", get(static_path))
        .route("/api/analyze", post(api_analyze))
        .route("/api/sessions", get(api_sessions))
        .route("/api/sessions/{id}", get(api_get_session).post(api_save_any))
        .route("/api/sessions/{id}/analyze", post(api_session_analyze))
        .route("/api/save", post(api_save))
        .route("/api/sessions/{id}/delete", post(api_delete))
        .with_state(state)
}

async fn api_save_any() -> impl IntoResponse {
    (StatusCode::METHOD_NOT_ALLOWED, "use /api/save").into_response()
}

pub async fn serve(store: Store, addr: SocketAddr) {
    let app = build_router(store);
    let listener = tokio::net::TcpListener::bind(addr).await.expect("bind");
    axum::serve(listener, app).await.expect("server");
}
