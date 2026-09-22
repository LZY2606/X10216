//! SQLite persistence for demo sessions. A session stores both editor
//! versions, the disabled-constraint set and the step cursor, so a saved
//! demonstration resumes at exactly any step after a restart.

use rusqlite::{params, Connection};
use serde::{Deserialize, Serialize};

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct Session {
    pub id: String,
    pub title: String,
    pub source_a: String,
    pub source_b: String,
    pub active: String, // "A" | "B"
    pub disabled: Vec<u64>,
    pub cursor: Option<usize>,
    pub created_at: i64,
}

pub struct Store {
    pub conn: Connection,
}

impl Store {
    pub fn open(path: &str) -> rusqlite::Result<Store> {
        let conn = Connection::open(path)?;
        conn.execute_batch(
            r#"
            CREATE TABLE IF NOT EXISTS sessions (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                source_a TEXT NOT NULL,
                source_b TEXT NOT NULL,
                active TEXT NOT NULL,
                disabled TEXT NOT NULL,
                cursor INTEGER,
                created_at INTEGER NOT NULL
            );
            "#,
        )?;
        Ok(Store { conn })
    }

    pub fn upsert(&mut self, s: &Session) -> rusqlite::Result<()> {
        self.conn.execute(
            "INSERT INTO sessions (id, title, source_a, source_b, active, disabled, cursor, created_at)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)
             ON CONFLICT(id) DO UPDATE SET
                title=excluded.title,
                source_a=excluded.source_a,
                source_b=excluded.source_b,
                active=excluded.active,
                disabled=excluded.disabled,
                cursor=excluded.cursor,
                created_at=excluded.created_at",
            params![
                s.id,
                s.title,
                s.source_a,
                s.source_b,
                s.active,
                serde_json::to_string(&s.disabled).unwrap_or_else(|_| "[]".into()),
                s.cursor.map(|c| c as i64),
                s.created_at,
            ],
        )?;
        Ok(())
    }

    pub fn get(&self, id: &str) -> rusqlite::Result<Option<Session>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, title, source_a, source_b, active, disabled, cursor, created_at
             FROM sessions WHERE id = ?1",
        )?;
        let mut rows = stmt.query(params![id])?;
        if let Some(row) = rows.next()? {
            let disabled_json: String = row.get(5)?;
            let cursor: Option<i64> = row.get(6)?;
            Ok(Some(Session {
                id: row.get(0)?,
                title: row.get(1)?,
                source_a: row.get(2)?,
                source_b: row.get(3)?,
                active: row.get(4)?,
                disabled: serde_json::from_str(&disabled_json).unwrap_or_default(),
                cursor: cursor.map(|c| c as usize),
                created_at: row.get(7)?,
            }))
        } else {
            Ok(None)
        }
    }

    pub fn list(&self) -> rusqlite::Result<Vec<Session>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, title, source_a, source_b, active, disabled, cursor, created_at
             FROM sessions ORDER BY created_at DESC, id ASC",
        )?;
        let mut rows = stmt.query([])?;
        let mut out = Vec::new();
        while let Some(row) = rows.next()? {
            let disabled_json: String = row.get(5)?;
            let cursor: Option<i64> = row.get(6)?;
            out.push(Session {
                id: row.get(0)?,
                title: row.get(1)?,
                source_a: row.get(2)?,
                source_b: row.get(3)?,
                active: row.get(4)?,
                disabled: serde_json::from_str(&disabled_json).unwrap_or_default(),
                cursor: cursor.map(|c| c as usize),
                created_at: row.get(7)?,
            });
        }
        Ok(out)
    }

    pub fn delete(&mut self, id: &str) -> rusqlite::Result<()> {
        self.conn.execute("DELETE FROM sessions WHERE id = ?1", params![id])?;
        Ok(())
    }

    /// Seed demo sessions on first start.
    pub fn seed_demos(&mut self) -> rusqlite::Result<()> {
        if self.list()?.is_empty() {
            for (id, title, a, b) in crate::web::demos::DEMOS {
                self.upsert(&Session {
                    id: id.to_string(),
                    title: title.to_string(),
                    source_a: a.to_string(),
                    source_b: b.to_string(),
                    active: "A".into(),
                    disabled: Vec::new(),
                    cursor: None,
                    created_at: 0,
                })?;
            }
        }
        Ok(())
    }
}
