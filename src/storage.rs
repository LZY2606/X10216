// SQLite 持久化：会话、两个编辑版本、禁用约束、步进游标。
use rusqlite::{params, Connection};
use serde::{Deserialize, Serialize};
use std::sync::Mutex;

pub struct Store {
    pub conn: Mutex<Connection>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionRow {
    pub id: String,
    pub title: String,
    pub created_at: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionRow {
    pub session_id: String,
    pub side: String, // "A" | "B"
    pub source: String,
    pub disabled: Vec<String>,
    pub cursor: i64,
    pub updated_at: i64,
}

impl Store {
    pub fn open(path: &str) -> rusqlite::Result<Store> {
        let conn = Connection::open(path)?;
        conn.pragma_update(None, "foreign_keys", 1)?;
        conn.execute_batch(
            r#"
            CREATE TABLE IF NOT EXISTS sessions (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                created_at INTEGER NOT NULL
            );
            CREATE TABLE IF NOT EXISTS versions (
                session_id TEXT NOT NULL,
                side TEXT NOT NULL CHECK(side IN ('A','B')),
                source TEXT NOT NULL,
                disabled TEXT NOT NULL DEFAULT '[]',
                cursor INTEGER NOT NULL DEFAULT -1,
                updated_at INTEGER NOT NULL,
                PRIMARY KEY(session_id, side),
                FOREIGN KEY(session_id) REFERENCES sessions(id) ON DELETE CASCADE
            );
            "#,
        )?;
        Ok(Store { conn: Mutex::new(conn) })
    }

    /// 内存数据库（测试与临时演示）
    pub fn in_memory() -> rusqlite::Result<Store> {
        let conn = Connection::open_in_memory()?;
        conn.pragma_update(None, "foreign_keys", 1)?;
        conn.execute_batch(
            r#"
            CREATE TABLE sessions (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                created_at INTEGER NOT NULL
            );
            CREATE TABLE versions (
                session_id TEXT NOT NULL,
                side TEXT NOT NULL CHECK(side IN ('A','B')),
                source TEXT NOT NULL,
                disabled TEXT NOT NULL DEFAULT '[]',
                cursor INTEGER NOT NULL DEFAULT -1,
                updated_at INTEGER NOT NULL,
                PRIMARY KEY(session_id, side),
                FOREIGN KEY(session_id) REFERENCES sessions(id) ON DELETE CASCADE
            );
            "#,
        )?;
        Ok(Store { conn: Mutex::new(conn) })
    }

    pub fn create_session(&self, id: &str, title: &str) -> rusqlite::Result<()> {
        let now = now_ts();
        let c = self.conn.lock().unwrap();
        c.execute(
            "INSERT OR IGNORE INTO sessions(id,title,created_at) VALUES (?1,?2,?3)",
            params![id, title, now],
        )?;
        for side in ["A", "B"] {
            c.execute(
                "INSERT OR IGNORE INTO versions(session_id,side,source,disabled,cursor,updated_at)
                 VALUES (?1,?2,'','[]',-1,?3)",
                params![id, side, now],
            )?;
        }
        Ok(())
    }

    pub fn list_sessions(&self) -> rusqlite::Result<Vec<SessionRow>> {
        let c = self.conn.lock().unwrap();
        let mut stmt = c.prepare(
            "SELECT id,title,created_at FROM sessions ORDER BY created_at DESC, id ASC",
        )?;
        let rows = stmt
            .query_map([], |r| {
                Ok(SessionRow {
                    id: r.get(0)?,
                    title: r.get(1)?,
                    created_at: r.get(2)?,
                })
            })?
            .collect::<rusqlite::Result<Vec<_>>>()?;
        Ok(rows)
    }

    pub fn get_version(&self, session_id: &str, side: &str) -> rusqlite::Result<VersionRow> {
        let c = self.conn.lock().unwrap();
        c.query_row(
            "SELECT session_id,side,source,disabled,cursor,updated_at
             FROM versions WHERE session_id=?1 AND side=?2",
            params![session_id, side],
            |r| {
                let disabled_json: String = r.get(3)?;
                let disabled: Vec<String> =
                    serde_json::from_str(&disabled_json).unwrap_or_default();
                Ok(VersionRow {
                    session_id: r.get(0)?,
                    side: r.get(1)?,
                    source: r.get(2)?,
                    disabled,
                    cursor: r.get(4)?,
                    updated_at: r.get(5)?,
                })
            },
        )
    }

    pub fn save_version(&self, row: &VersionRow) -> rusqlite::Result<()> {
        let now = now_ts();
        let disabled_json = serde_json::to_string(&row.disabled).unwrap_or_else(|_| "[]".into());
        let c = self.conn.lock().unwrap();
        c.execute(
            "INSERT INTO sessions(id,title,created_at) VALUES (?1,'类型切片台会话',?2)
             ON CONFLICT(id) DO NOTHING",
            params![row.session_id, now],
        )?;
        c.execute(
            "INSERT INTO versions(session_id,side,source,disabled,cursor,updated_at)
             VALUES (?1,?2,?3,?4,?5,?6)
             ON CONFLICT(session_id,side) DO UPDATE SET
               source=excluded.source,
               disabled=excluded.disabled,
               cursor=excluded.cursor,
               updated_at=excluded.updated_at",
            params![row.session_id, row.side, row.source, disabled_json, row.cursor, now],
        )?;
        Ok(())
    }
}

fn now_ts() -> i64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}
