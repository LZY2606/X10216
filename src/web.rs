// Axum web 层：API + 内嵌单页前端。
use crate::analysis::{self, AnalysisView, PriorContext};
use crate::incremental;
use crate::solver;
use crate::storage::{Store, VersionRow};
use axum::{
    extract::State,
    http::StatusCode,
    response::{Html, IntoResponse, Json},
    routing::{get, post},
    Router,
};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::sync::Arc;

#[derive(Clone)]
pub struct AppState {
    pub store: Arc<Store>,
}

pub fn router(store: Arc<Store>) -> Router {
    Router::new()
        .route("/", get(index))
        .route("/api/health", get(health))
        .route("/api/sessions", get(list_sessions).post(create_session))
        .route(
            "/api/sessions/{id}/versions/{side}",
            get(get_version).put(save_version),
        )
        .route("/api/sessions/{id}/analyze/{side}", post(analyze_side))
        .route("/api/sessions/{id}/diff", get(diff))
        .with_state(AppState { store })
}

async fn index() -> Html<&'static str> {
    Html(include_str!("../assets/index.html"))
}

async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({ "ok": true, "name": "类型切片台" }))
}

#[derive(Serialize)]
struct SessionOut {
    id: String,
    title: String,
}

async fn list_sessions(State(st): State<AppState>) -> Result<Json<Vec<SessionRow>>, ApiError> {
    let rows = st.store.list_sessions().map_err(internal)?;
    Ok(Json(rows))
}

use crate::storage::SessionRow;

#[derive(Deserialize)]
struct CreateSessionReq {
    id: String,
    #[serde(default)]
    title: Option<String>,
}

async fn create_session(
    State(st): State<AppState>,
    Json(req): Json<CreateSessionReq>,
) -> Result<Json<serde_json::Value>, ApiError> {
    st.store
        .create_session(&req.id, req.title.as_deref().unwrap_or("类型切片台会话"))
        .map_err(internal)?;
    Ok(Json(serde_json::json!({ "id": req.id })))
}

struct Paths {
    id: String,
    side: String,
}

async fn get_version(
    State(st): State<AppState>,
    axum::extract::Path((id, side)): axum::extract::Path<(String, String)>,
) -> Result<Json<VersionRow>, ApiError> {
    let row = st.store.get_version(&id, &side).map_err(internal)?;
    Ok(Json(row))
}

#[derive(Deserialize)]
struct SaveVersionReq {
    source: String,
    #[serde(default)]
    disabled: Vec<String>,
    #[serde(default)]
    cursor: i64,
}

async fn save_version(
    State(st): State<AppState>,
    axum::extract::Path((id, side)): axum::extract::Path<(String, String)>,
    Json(req): Json<SaveVersionReq>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let row = VersionRow {
        session_id: id,
        side,
        source: req.source,
        disabled: req.disabled,
        cursor: req.cursor,
        updated_at: 0,
    };
    st.store.save_version(&row).map_err(internal)?;
    Ok(Json(serde_json::json!({ "saved": true })))
}

#[derive(Deserialize)]
struct AnalyzeReq {
    #[serde(default)]
    source: Option<String>,
    #[serde(default)]
    disabled: Option<Vec<String>>,
    #[serde(default)]
    cursor: Option<i64>,
}

fn compute_view(source: &str, prior: Option<&PriorContext>, disabled: &BTreeSet<String>) -> AnalysisView {
    analysis::analyze(source, prior, disabled)
}

async fn analyze_side(
    State(st): State<AppState>,
    axum::extract::Path((id, side)): axum::extract::Path<(String, String)>,
    Json(req): Json<AnalyzeReq>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let saved = st.store.get_version(&id, &side).map_err(internal)?;
    let source = req.source.unwrap_or(saved.source.clone());
    let disabled: BTreeSet<String> = req
        .disabled
        .map(|d| d.into_iter().collect())
        .unwrap_or_else(|| saved.disabled.iter().cloned().collect());
    let cursor = req.cursor.unwrap_or(saved.cursor);

    // 同会话另一版本作为“旧版本”上下文
    let other_side = if side == "A" { "B" } else { "A" };
    let other = st.store.get_version(&id, other_side).ok();
    let prior = build_prior(other.as_ref());

    let view = compute_view(&source, prior.as_ref(), &disabled);

    // 保存（含游标），实现“重启后从任一步继续”
    let _ = st.store.save_version(&VersionRow {
        session_id: id.clone(),
        side: side.clone(),
        source: source.clone(),
        disabled: disabled.iter().cloned().collect(),
        cursor,
        updated_at: 0,
    });

    Ok(Json(serde_json::json!({
        "side": side,
        "cursor": cursor,
        "total_steps": view.result.steps.len(),
        "view": view,
    })))
}

fn build_prior(other: Option<&VersionRow>) -> Option<PriorContext> {
    let row = other?;
    let parsed = crate::syntax::Parser::parse_program(&row.source, 0).ok()?;
    Some(PriorContext { old_tree: Some(parsed.root), old_max: parsed.max_id })
}

async fn diff(
    State(st): State<AppState>,
    axum::extract::Path(id): axum::extract::Path<String>,
) -> Result<Json<serde_json::Value>, ApiError> {
    let a = st.store.get_version(&id, "A").map_err(internal)?;
    let b = st.store.get_version(&id, "B").map_err(internal)?;
    let da: BTreeSet<String> = a.disabled.iter().cloned().collect();
    let db: BTreeSet<String> = b.disabled.iter().cloned().collect();

    // 新版本（B）以 A 为旧版本上下文，保证身份/约束复用判定真实发生
    let prior_a = PriorContext {
        old_tree: crate::syntax::Parser::parse_program(&a.source, 0).ok().map(|p| p.root),
        old_max: crate::syntax::Parser::parse_program(&a.source, 0)
            .map(|p| p.max_id)
            .unwrap_or(0),
    };
    let va = analysis::analyze(&a.source, None, &da);
    let vb = analysis::analyze(&b.source, Some(&prior_a), &db);
    let d = incremental::diff_versions(&va, &vb);
    Ok(Json(serde_json::json!({ "diff": d, "a": va, "b": vb })))
}

struct ApiError {
    status: StatusCode,
    message: String,
}
impl IntoResponse for ApiError {
    fn into_response(self) -> axum::response::Response {
        (self.status, Json(serde_json::json!({ "error": self.message }))).into_response()
    }
}
fn internal(e: impl std::fmt::Display) -> ApiError {
    ApiError { status: StatusCode::INTERNAL_SERVER_ERROR, message: e.to_string() }
}

// 让 solver 在 web 层可见（部分端点可能直接调用）
#[allow(dead_code)]
fn _force_use(_: solver::ConstraintStatus) {}
