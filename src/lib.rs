#[macro_use]
extern crate nom;
extern crate petgraph;
#[macro_use]
extern crate web_assembler;

#[macro_use]
pub mod util;
pub mod prim;
mod parser;
pub mod pass;
pub mod ast;
pub mod hir;
pub mod mir;
pub mod lir;
pub mod backend;
pub mod id;

pub use pass::{Chain, Pass};
pub use parser::parse;
pub use ast::TypeError;

static BUILTIN_FUNCTIONS: &[&str] = &[
    "print" // "+", "-", "*", "div", "/", "mod",
// "=", "<>", ">", ">=", "<", "<="
];
