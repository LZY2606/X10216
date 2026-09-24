//! Type Slice Bench — a step-by-step Hindley-Milner type inference bench.

pub mod syntax;
pub mod types;
pub mod constraints;
pub mod infer;
pub mod core;
pub mod incremental;
pub mod db;
pub mod web;

pub use infer::{analyze, AnalyzeOptions, Analysis};
pub use incremental::diff_analyses;
pub use db::Database;
