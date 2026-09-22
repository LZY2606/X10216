//! 类型切片台 (Type Slicer): Hindley-Milner inference workbench.
//!
//! Modules:
//! - [`types`]: type representation and rows
//! - [`ast`]: syntax tree with stable content-addressed node ids
//! - [`parser`]: lexer + recursive-descent parser
//! - [`constraints`]: constraint operations
//! - [`gen`]: constraint generation with scope-aware incremental cache
//! - [`solver`]: union-find unification, ranks, occurs check, row errors
//! - [`cores`]: minimal unsat cores
//! - [`analysis`]: end-to-end pipeline and JSON view models
//! - [`store`]: SQLite persistence of demo sessions

pub mod analysis;
pub mod ast;
pub mod constraints;
pub mod cores;
pub mod gen;
pub mod parser;
pub mod solver;
pub mod store;
pub mod types;
pub mod web;
