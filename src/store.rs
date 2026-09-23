//! SQLite persistence for demo sessions. A session holds a chain of source
//! versions; each version stores its full analysis so a saved session can be
//! resumed from any step after a restart.

use crate::analyze::Analysis;
use rusqlite::{params, Connection};
use std::path::Path;
use std::sync::Mutex;

pub struct Store {
    conn: Mutex<Connection>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct VersionInfo {
    pub id: i64,
    pub session_id: i64,
    pub source: String,
    pub created: String,
}

impl Store {
    pub fn open(path: &Path) -> Result<Store, String> {
        let conn = Connection::open(path).map_err(|e| e.to_string())?;
        conn.execute_batch(
            "CREATE TABLE IF NOT EXISTS sessions(
                id INTEGER PRIMARY KEY,
                created TEXT NOT NULL DEFAULT (datetime('now'))
            );
            CREATE TABLE IF NOT EXISTS versions(
                id INTEGER PRIMARY KEY,
                session_id INTEGER NOT NULL REFERENCES sessions(id),
                source TEXT NOT NULL,
                analysis TEXT NOT NULL,
                created TEXT NOT NULL DEFAULT (datetime('now'))
            );",
        )
        .map_err(|e| e.to_string())?;
        Ok(Store { conn: Mutex::new(conn) })
    }

    pub fn create_session(&self) -> Result<i64, String> {
        let conn = self.conn.lock().unwrap();
        conn.execute("INSERT INTO sessions DEFAULT VALUES", [])
            .map_err(|e| e.to_string())?;
        Ok(conn.last_insert_rowid())
    }

    pub fn add_version(&self, session_id: i64, source: &str, analysis: &Analysis) -> Result<i64, String> {
        let json = serde_json::to_string(analysis).map_err(|e| e.to_string())?;
        let conn = self.conn.lock().unwrap();
        conn.execute(
            "INSERT INTO versions(session_id, source, analysis) VALUES(?1, ?2, ?3)",
            params![session_id, source, json],
        )
        .map_err(|e| e.to_string())?;
        Ok(conn.last_insert_rowid())
    }

    pub fn list_versions(&self, session_id: i64) -> Result<Vec<VersionInfo>, String> {
        let conn = self.conn.lock().unwrap();
        let mut stmt = conn
            .prepare("SELECT id, session_id, source, created FROM versions WHERE session_id=?1 ORDER BY id")
            .map_err(|e| e.to_string())?;
        let rows = stmt
            .query_map(params![session_id], |r| {
                Ok(VersionInfo {
                    id: r.get(0)?,
                    session_id: r.get(1)?,
                    source: r.get(2)?,
                    created: r.get(3)?,
                })
            })
            .map_err(|e| e.to_string())?;
        Ok(rows.filter_map(|r| r.ok()).collect())
    }

    pub fn get_version(&self, version_id: i64) -> Result<(String, Analysis), String> {
        let conn = self.conn.lock().unwrap();
        let (source, json): (String, String) = conn
            .query_row(
                "SELECT source, analysis FROM versions WHERE id=?1",
                params![version_id],
                |r| Ok((r.get(0)?, r.get(1)?)),
            )
            .map_err(|e| e.to_string())?;
        let analysis: Analysis = serde_json::from_str(&json).map_err(|e| e.to_string())?;
        Ok((source, analysis))
    }
}
