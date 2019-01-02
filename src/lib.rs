#[macro_use]
extern crate nom;
extern crate petgraph;
#[macro_use]
extern crate web_assembler;

#[macro_use]
pub mod util;
pub mod ast;
pub mod backend;
pub mod hir;
pub mod id;
pub mod lir;
pub mod mir;
mod parser;
pub mod pass;
pub mod prim;

pub use crate::ast::TypeError;
pub use crate::parser::parse;
pub use crate::pass::{Chain, Pass};

static BUILTIN_FUNCTIONS: &[&str] = &[
    "print", // "+", "-", "*", "div", "/", "mod",
             // "=", "<>", ">", ">=", "<", "<="
];
