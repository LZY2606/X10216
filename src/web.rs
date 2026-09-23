//! HTTP 层：静态单页 + 分析/差异/会话 API。
use crate::analyze::{analyze_source, AnalysisResult};
use crate::incremental::{diff, DiffResult};
use crate::parser::parse_pub;
use crate::solve::CId;
use crate::storage::{now_secs, Db, Session};
use axum::{extract::State, http::header, response::IntoResponse, routing::{get, post}, Json, Router};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::sync::Arc;

#[derive(Clone)]
pub struct AppState {
    pub db: Arc<Db>,
}

pub fn router(db: Arc<Db>) -> Router {
    Router::new()
        .route("/", get(index))
        .route("/api/analyze", post(api_analyze))
        .route("/api/diff", post(api_diff))
        .route("/api/sessions", get(list_sessions).post(save_session))
        .route("/api/sessions/{id}", get(get_session))
        .with_state(AppState { db })
}

async fn index() -> impl IntoResponse {
    (
        [(header::CONTENT_TYPE, "text/html; charset=utf-8")],
        crate::web_ui::INDEX_HTML,
    )
}

#[derive(Deserialize)]
struct AnalyzeReq {
    source: String,
    #[serde(default)]
    disabled: Vec<u32>,
}

#[derive(Serialize)]
struct AnalyzeResp {
    #[serde(flatten)]
    result: AnalysisResult,
}

async fn api_analyze(Json(req): Json<AnalyzeReq>) -> Json<serde_json::Value> {
    let dis: BTreeSet<CId> = req.disabled.into_iter().map(CId).collect();
    let a = analyze_source(&req.source, &dis);
    serde_json::to_value(&a.result).map(Json).unwrap_or(Json(serde_json::json!({"error":"序列化失败"})))
}

#[derive(Deserialize)]
struct DiffReq {
    source_a: String,
    source_b: String,
    #[serde(default)]
    disabled: Vec<u32>,
}

#[derive(Serialize)]
struct DiffResp {
    diff: DiffResult,
    result_b: AnalysisResult,
}

async fn api_diff(Json(req): Json<DiffReq>) -> Json<serde_json::Value> {
    let dis: BTreeSet<CId> = req.disabled.into_iter().map(CId).collect();
    let pa = parse_pub(&req.source_a);
    let pb = parse_pub(&req.source_b);
    let old = analyze_source(&req.source_a, &dis);
    let d = diff(&old, &pa, &pb, &dis);
    let new = analyze_source(&req.source_b, &dis);
    let resp = DiffResp { diff: d, result_b: new.result };
    serde_json::to_value(&resp).map(Json).unwrap_or(Json(serde_json::json!({"error":"序列化失败"})))
}

async fn list_sessions(State(st): State<AppState>) -> Json<serde_json::Value> {
    let items = st.db.list().unwrap_or_default().into_iter()
        .map(|(id, title, ts)| serde_json::json!({"id":id,"title":title,"updated_at":ts}))
        .collect::<Vec<_>>();
    Json(serde_json::json!({ "sessions": items }))
}

#[derive(Deserialize)]
struct SaveReq {
    #[serde(default)]
    id: Option<String>,
    title: String,
    source_a: String,
    source_b: String,
    #[serde(default)]
    disabled_a: Vec<u32>,
    #[serde(default)]
    current_step: i64,
}

async fn save_session(State(st): State<AppState>, Json(req): Json<SaveReq>) -> Json<serde_json::Value> {
    let now = now_secs();
    let id = req.id.filter(|s| !s.is_empty()).unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
    let existing = st.db.get(&id).ok().flatten();
    let created = existing.as_ref().map(|e| e.created_at).unwrap_or(now);
    let s = Session {
        id: id.clone(),
        title: req.title,
        source_a: req.source_a,
        source_b: req.source_b,
        disabled_a: req.disabled_a,
        current_step: req.current_step,
        created_at: created,
        updated_at: now,
    };
    match st.db.upsert(&s) {
        Ok(()) => Json(serde_json::json!({"id": id})),
        Err(e) => Json(serde_json::json!({"error": e.to_string()})),
    }
}

async fn get_session(State(st): State<AppState>, axum::extract::Path(id): axum::extract::Path<String>)
    -> Json<serde_json::Value> {
    match st.db.get(&id) {
        Ok(Some(s)) => Json(serde_json::to_value(s).unwrap_or(serde_json::json!({"error":"序列化失败"}))),
        Ok(None) => Json(serde_json::json!({"error":"未找到会话"})),
        Err(e) => Json(serde_json::json!({"error": e.to_string()})),
    }
}
