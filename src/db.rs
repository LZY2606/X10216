//! SQLite persistence for demo sessions.

use rusqlite::Connection;
use std::sync::Mutex;

pub struct Database {
    pub conn: Mutex<Connection>,
}

impl Database {
    pub fn open(path: &str) -> rusqlite::Result<Self> {
        let conn = Connection::open(path)?;
        let db = Database { conn: Mutex::new(conn) };
        db.init()?;
        Ok(db)
    }

    fn init(&self) -> rusqlite::Result<()> {
        let c = self.conn.lock().unwrap();
        c.execute_batch(
            r#"
            CREATE TABLE IF NOT EXISTS sessions (
                id TEXT PRIMARY KEY,
                title TEXT NOT NULL,
                created_at INTEGER NOT NULL,
                updated_at INTEGER NOT NULL
            );
            CREATE TABLE IF NOT EXISTS versions (
                session_id TEXT NOT NULL,
                slot TEXT NOT NULL,
                source TEXT NOT NULL,
                updated_at INTEGER NOT NULL,
                PRIMARY KEY (session_id, slot)
            );
            CREATE TABLE IF NOT EXISTS runs (
                session_id TEXT NOT NULL,
                slot TEXT NOT NULL,
                run_id INTEGER PRIMARY KEY AUTOINCREMENT,
                disabled TEXT NOT NULL,
                analysis_json TEXT NOT NULL,
                created_at INTEGER NOT NULL
            );
            CREATE TABLE IF NOT EXISTS cursors (
                session_id TEXT PRIMARY KEY,
                slot TEXT NOT NULL,
                step_index INTEGER NOT NULL,
                disabled TEXT NOT NULL
            );
            "#,
        )?;
        Ok(())
    }
}
