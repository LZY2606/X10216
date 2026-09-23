//! SQLite 会话持久化：保存两份源码、禁用约束、当前步骤与会话标题，
//! 使演示在重启后可从任一步继续。
use rusqlite::Connection;
use serde::{Deserialize, Serialize};
use std::sync::Mutex;

pub struct Db(pub Mutex<Connection>);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Session {
    pub id: String,
    pub title: String,
    pub source_a: String,
    pub source_b: String,
    /// 针对 A 版本禁用的约束 id。
    pub disabled_a: Vec<u32>,
    pub current_step: i64,
    pub created_at: i64,
    pub updated_at: i64,
}

impl Db {
    pub fn open(path: &str) -> rusqlite::Result<Db> {
        let conn = Connection::open(path)?;
        conn.execute_batch(
            "CREATE TABLE IF NOT EXISTS sessions (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                source_a TEXT NOT NULL,
                source_b TEXT NOT NULL,
                disabled_a TEXT NOT NULL DEFAULT '[]',
                current_step INTEGER NOT NULL DEFAULT 0,
                created_at INTEGER NOT NULL,
                updated_at INTEGER NOT NULL
            );",
        )?;
        Ok(Db(Mutex::new(conn)))
    }

    pub fn upsert(&self, s: &Session) -> rusqlite::Result<()> {
        let c = self.0.lock().unwrap();
        c.execute(
            "INSERT INTO sessions (id,title,source_a,source_b,disabled_a,current_step,created_at,updated_at)
             VALUES (?1,?2,?3,?4,?5,?6,?7,?8)
             ON CONFLICT(id) DO UPDATE SET
               title=excluded.title, source_a=excluded.source_a, source_b=excluded.source_b,
               disabled_a=excluded.disabled_a, current_step=excluded.current_step,
               updated_at=excluded.updated_at",
            rusqlite::params![
                s.id, s.title, s.source_a, s.source_b,
                serde_json::to_string(&s.disabled_a).unwrap_or_else(|_| "[]".into()),
                s.current_step, s.created_at, s.updated_at,
            ],
        )?;
        Ok(())
    }

    pub fn get(&self, id: &str) -> rusqlite::Result<Option<Session>> {
        let c = self.0.lock().unwrap();
        let mut stmt = c.prepare(
            "SELECT id,title,source_a,source_b,disabled_a,current_step,created_at,updated_at
             FROM sessions WHERE id=?1")?;
        let mut rows = stmt.query(rusqlite::params![id])?;
        if let Some(r) = rows.next()? {
            let da: String = r.get(4)?;
            Ok(Some(Session {
                id: r.get(0)?, title: r.get(1)?, source_a: r.get(2)?, source_b: r.get(3)?,
                disabled_a: serde_json::from_str(&da).unwrap_or_default(),
                current_step: r.get(5)?, created_at: r.get(6)?, updated_at: r.get(7)?,
            }))
        } else { Ok(None) }
    }

    pub fn list(&self) -> rusqlite::Result<Vec<(String, String, i64)>> {
        let c = self.0.lock().unwrap();
        let mut stmt = c.prepare("SELECT id,title,updated_at FROM sessions ORDER BY updated_at DESC LIMIT 50")?;
        let mut rows = stmt.query([])?;
        let mut out = vec![];
        while let Some(r) = rows.next()? {
            out.push((r.get(0)?, r.get(1)?, r.get(2)?));
        }
        Ok(out)
    }
}

pub fn now_secs() -> i64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_secs() as i64).unwrap_or(0)
}
