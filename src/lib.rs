//! 类型切片台 — Hindley-Milner 风格语言的推断切片引擎。

pub mod ast;
pub mod types;
pub mod constraints;
pub mod gen;
pub mod solver;
pub mod core;
pub mod incremental;
pub mod analysis;
pub mod db;
pub mod server;
pub mod demos;

pub use ast::{parse, Program, Span};
pub use gen::{generate, GenResult};
pub use solver::{solve_with, SolveOpts, SolveResult};
pub use incremental::{diff_versions, incremental_solve, VersionDiff};
