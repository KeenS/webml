extern crate nom;

pub mod ty;
pub mod ast;
mod typing;
mod hir;

pub use typing::{TyEnv, TypeError};
