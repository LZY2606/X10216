//! SQLite persistence for demo sessions: versions, disabled-constraint
//! selections and the current step cursor survive restarts.

use rusqlite::{params, Connection};
use std::sync::Mutex;

pub struct Store {
    pub conn: Mutex<Connection>,
}

pub struct VersionRow {
    pub id: i64,
    pub source: String,
    pub label: String,
    pub disabled: String,
    pub step_cursor: i64,
    pub created_at: String,
}

pub fn open(path: &str) -> rusqlite::Result<Store> {
    let conn = Connection::open(path)?;
    conn.pragma_update(None, "journal_mode", "WAL")?;
    conn.execute_batch(
        r#"
        CREATE TABLE IF NOT EXISTS sessions (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            title TEXT NOT NULL,
            created_at TEXT NOT NULL DEFAULT (datetime('now'))
        );
        CREATE TABLE IF NOT EXISTS versions (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            session_id INTEGER NOT NULL REFERENCES sessions(id) ON DELETE CASCADE,
            source TEXT NOT NULL,
            label TEXT NOT NULL,
            disabled_json TEXT NOT NULL DEFAULT '[]',
            step_cursor INTEGER NOT NULL DEFAULT 0,
            created_at TEXT NOT NULL DEFAULT (datetime('now'))
        );
        "#,
    )?;
    Ok(Store { conn: Mutex::new(conn) })
}

impl Store {
    pub fn create_session(&self, title: &str) -> rusqlite::Result<i64> {
        let c = self.conn.lock().unwrap();
        c.execute("INSERT INTO sessions (title) VALUES (?1)", params![title])?;
        Ok(c.last_insert_rowid())
    }

    pub fn ensure_demo(&self, source: &str) -> rusqlite::Result<i64> {
        let c = self.conn.lock().unwrap();
        let exists: i64 = c
            .query_row("SELECT COUNT(*) FROM sessions WHERE title = '类型切片台·演示'", [], |r| r.get(0))
            .unwrap_or(0);
        drop(c);
        if exists > 0 {
            let c = self.conn.lock().unwrap();
            c.query_row(
                "SELECT id FROM sessions WHERE title = '类型切片台·演示' ORDER BY id LIMIT 1",
                [],
                |r| r.get(0),
            )
        } else {
            let sid = self.create_session("类型切片台·演示")?;
            self.add_version(sid, source, "v1 初始版本")?;
            Ok(sid)
        }
    }

    pub fn add_version(&self, session: i64, source: &str, label: &str) -> rusqlite::Result<i64> {
        let c = self.conn.lock().unwrap();
        c.execute(
            "INSERT INTO versions (session_id, source, label) VALUES (?1, ?2, ?3)",
            params![session, source, label],
        )?;
        Ok(c.last_insert_rowid())
    }

    pub fn list_sessions(&self) -> rusqlite::Result<Vec<(i64, String)>> {
        let c = self.conn.lock().unwrap();
        let mut stmt = c.prepare("SELECT id, title FROM sessions ORDER BY id DESC")?;
        let rows = stmt
            .query_map([], |r| Ok((r.get::<_, i64>(0)?, r.get::<_, String>(1)?)))?
            .collect::<rusqlite::Result<Vec<_>>>()?;
        Ok(rows)
    }

    pub fn list_versions(&self, session: i64) -> rusqlite::Result<Vec<VersionRow>> {
        let c = self.conn.lock().unwrap();
        let mut stmt = c.prepare(
            "SELECT id, source, label, disabled_json, step_cursor, created_at \
             FROM versions WHERE session_id = ?1 ORDER BY id",
        )?;
        let rows = stmt
            .query_map(params![session], |r| {
                Ok(VersionRow {
                    id: r.get(0)?,
                    source: r.get(1)?,
                    label: r.get(2)?,
                    disabled: r.get(3)?,
                    step_cursor: r.get(4)?,
                    created_at: r.get(5)?,
                })
            })?
            .collect::<rusqlite::Result<Vec<_>>>()?;
        Ok(rows)
    }

    pub fn get_version(&self, version: i64) -> rusqlite::Result<Option<VersionRow>> {
        let c = self.conn.lock().unwrap();
        let mut stmt = c.prepare(
            "SELECT id, source, label, disabled_json, step_cursor, created_at FROM versions WHERE id = ?1",
        )?;
        let mut rows = stmt.query_map(params![version], |r| {
            Ok(VersionRow {
                id: r.get(0)?,
                source: r.get(1)?,
                label: r.get(2)?,
                disabled: r.get(3)?,
                step_cursor: r.get(4)?,
                created_at: r.get(5)?,
            })
        })?;
        rows.next().transpose()
    }

    pub fn set_disabled(&self, version: i64, disabled_json: &str) -> rusqlite::Result<()> {
        let c = self.conn.lock().unwrap();
        c.execute("UPDATE versions SET disabled_json = ?1 WHERE id = ?2", params![disabled_json, version])?;
        Ok(())
    }

    pub fn set_cursor(&self, version: i64, cursor: i64) -> rusqlite::Result<()> {
        let c = self.conn.lock().unwrap();
        c.execute("UPDATE versions SET step_cursor = ?1 WHERE id = ?2", params![cursor, version])?;
        Ok(())
    }
}
