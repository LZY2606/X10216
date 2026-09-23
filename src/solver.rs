//! 约束生成、union-find 统一、层级泛化与求解编排。

pub mod arena;
pub mod gen;
pub mod unify;
pub mod solve;
pub mod cores;
pub mod view;

pub use arena::Arena;
pub use solve::{SolveResult, AnalysisResult, analyze, analyze_with_options, SolveOptions};
pub use view::{Provenance, TyViewTree};
