use crate::db::Store;
use crate::engine::{analyze_with_previous, AnalysisOptions, PreviousVersion};
use axum::body::Body;
use axum::extract::State;
use axum::http::{header, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::Json;
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};

#[derive(Clone)]
struct AppState {
    store: Arc<Mutex<Store>>,
}

#[derive(Deserialize, Default)]
struct AnalyzeRequest {
    source: String,
    #[serde(default)]
    disabled_constraints: std::collections::BTreeSet<String>,
    until_step: Option<usize>,
    previous: Option<PreviousVersion>,
}

#[derive(Serialize, Deserialize)]
struct SessionState {
    left: String,
    right: String,
    disabled: std::collections::BTreeSet<String>,
    step: Option<usize>,
}

pub async fn serve(addr: &str, database_path: &str) {
    let store = Store::open(database_path).expect("open SQLite session store");
    let state = AppState { store: Arc::new(Mutex::new(store)) };
    let app = axum::Router::new()
        .route("/", get(index))
        .route("/api/analyze", post(analyze))
        .route("/api/sessions", get(list_sessions).post(save_session))
        .route("/api/sessions/:id", get(load_session))
        .with_state(state);
    let listener = tokio::net::TcpListener::bind(addr).await.expect("bind address");
    axum::serve(listener, app).await.expect("run server");
}

async fn index() -> Response {
    let html = include_str!("../static/index.html");
    Response::builder().header(header::CONTENT_TYPE, "text/html; charset=utf-8").body(Body::from(html)).unwrap()
}

async fn analyze(Json(request): Json<AnalyzeRequest>) -> Json<crate::engine::Analysis> {
    let options = AnalysisOptions { disabled_constraints: request.disabled_constraints, until_step: request.until_step };
    Json(analyze_with_previous(&request.source, options, request.previous.as_ref()))
}

async fn list_sessions(State(state): State<AppState>) -> Response {
    let store = state.store.lock().unwrap();
    match store.list() {
        Ok(items) => Json(serde_json::to_value(items).unwrap()).into_response(),
        Err(error) => (StatusCode::INTERNAL_SERVER_ERROR, error).into_response(),
    }
}

#[derive(Deserialize)]
struct SaveRequest {
    id: String,
    title: String,
    state: SessionState,
}

async fn save_session(State(state): State<AppState>, Json(request): Json<SaveRequest>) -> Response {
    let value = serde_json::to_value(request.state).unwrap();
    match state.store.lock().unwrap().save(&request.id, &request.title, &value) {
        Ok(()) => Json(serde_json::json!({"ok":true})).into_response(),
        Err(error) => (StatusCode::INTERNAL_SERVER_ERROR, error).into_response(),
    }
}

async fn load_session(State(state): State<AppState>, axum::extract::Path(id): axum::extract::Path<String>) -> Response {
    match state.store.lock().unwrap().load(&id) {
        Ok(Some(value)) => Json(value).into_response(),
        Ok(None) => (StatusCode::NOT_FOUND, "session not found").into_response(),
        Err(error) => (StatusCode::INTERNAL_SERVER_ERROR, error).into_response(),
    }
}
