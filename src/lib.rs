#[macro_use]
extern crate nom;

mod parser;
pub mod ty;
pub mod ast;
mod typing;
mod hir;

pub use typing::{TyEnv, TypeError};
pub use parser::parse;
