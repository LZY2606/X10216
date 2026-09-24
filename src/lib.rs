//! 类型切片台：Hindley-Milner 风格语言的解析、约束生成、统一与增量分析。

pub mod syntax;
pub mod types;
pub mod infer;
pub mod diff;
pub mod persist;
pub mod server;

pub use syntax::{NodeId, Span};
