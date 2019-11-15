#[macro_use]
pub mod util;
pub mod ast;
pub mod backend;
mod config;
pub mod hir;
pub mod id;
pub mod lir;
pub mod mir;
mod parser;
pub mod pass;
pub mod prim;

pub use crate::ast::TypeError;
pub use crate::config::Config;
pub use crate::parser::parse;
pub use crate::pass::{Chain, Pass};

static BUILTIN_FUNCTIONS: &[&str] = &[
    "print", // "+", "-", "*", "div", "/", "mod",
            // "=", "<>", ">", ">=", "<", "<="
];

pub fn compile_str<'a>(input: &'a str, config: &Config) -> Result<Vec<u8>, TypeError<'a>> {
    use crate::pass::{ConvError, DebugPass, PrintablePass};
    use wasm::Dump;

    let id = id::Id::new();

    let mut passes = compile_pass![
       parse: ConvError::new(parse),
       rename: ast::Rename::new(id.clone()),
       typing: ast::Typing::new(),
       case_check: ast::CaseCheck::new(),
       ast_to_hir: hir::AST2HIR::new(id.clone()),
       find_buildin: hir::FindBuiltin::new(),
       flattening_expresion: hir::FlatExpr::new(id.clone()),
       flattening_let: hir::FlatLet::new(),
       unnest_functions: hir::UnnestFunc::new(id.clone()),
       closure_conversion: hir::ForceClosure::new(),
       hir_to_mir: mir::HIR2MIR::new(id.clone()),
       unalias: mir::UnAlias::new(),
       block_arrange: mir::BlockArrange::new(),
       mir_to_lir: DebugPass(lir::MIR2LIR::new()),
       backend: backend::LIR2WASM::new(),
    ];

    let module: wasm::Module = passes.trans(input, config)?;

    let mut code = Vec::new();
    module.dump(&mut code);
    Ok(code)
}
