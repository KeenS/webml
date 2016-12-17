#[macro_use]
extern crate nom;

pub mod util;
pub mod prim;
mod parser;
pub mod pass;
pub mod ast;
pub mod hir;
pub mod mir;

pub use prim::TypeError;
pub use pass::{Pass, Chain};
pub use parser::parse;
pub use ast::typing::TyEnv;
