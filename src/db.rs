use rusqlite::Connection;
use serde_json::Value;

pub struct Store {
    connection: Connection,
}

impl Store {
    pub fn open(path: &str) -> Result<Self, String> {
        let connection = Connection::open(path).map_err(|error| error.to_string())?;
        connection.execute_batch(
            "create table if not exists sessions (
                id text primary key,
                title text not null,
                created_at integer not null,
                state text not null
            );",
        ).map_err(|error| error.to_string())?;
        Ok(Self { connection })
    }

    pub fn save(&mut self, id: &str, title: &str, state: &Value) -> Result<(), String> {
        self.connection.execute(
            "insert into sessions(id, title, created_at, state) values (?1, ?2, unixepoch(), ?3)
             on conflict(id) do update set title=excluded.title, state=excluded.state",
            rusqlite::params![id, title, state.to_string()],
        ).map_err(|error| error.to_string())?;
        Ok(())
    }

    pub fn load(&self, id: &str) -> Result<Option<Value>, String> {
        let mut statement = self.connection.prepare("select state from sessions where id = ?1").map_err(|error| error.to_string())?;
        let mut rows = statement.query(rusqlite::params![id]).map_err(|error| error.to_string())?;
        if let Some(row) = rows.next().map_err(|error| error.to_string())? {
            let text: String = row.get(0).map_err(|error| error.to_string())?;
            Ok(Some(serde_json::from_str(&text).map_err(|error| error.to_string())?))
        } else {
            Ok(None)
        }
    }

    pub fn list(&self) -> Result<Vec<(String, String)>, String> {
        let mut statement = self.connection.prepare("select id, title from sessions order by created_at desc").map_err(|error| error.to_string())?;
        let rows = statement.query_map([], |row| Ok((row.get(0)?, row.get(1)?))).map_err(|error| error.to_string())?;
        rows.collect::<Result<_, _>>().map_err(|error| error.to_string())
    }
}
