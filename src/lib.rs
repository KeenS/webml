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
mod unification_pool;

pub use crate::ast::TypeError;
pub use crate::config::Config;
pub use crate::parser::Parser;
pub use crate::pass::{Chain, Pass};
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

pub fn compile_string(input: String, config: &Config) -> Result<Vec<u8>, TypeError> {
    use crate::pass::PrintablePass;
    use wasm::Dump;

    let id = id::Id::new();

    let mut passes = compile_pass![
       parse: parser::Parser::default(),
       desugar: ast::Desugar::new(id.clone()),
       rename: ast::Rename::new(id.clone()),
       var_to_constructor: ast::VarToConstructor::new(id.clone()),
       collect_langitems: ast::CollectLangItems::default(),
       typing: ast::Typer::default(),
       resolve_overload: ast::ResolveOverload::default(),
       case_simplify: ast::CaseSimplify::new(id.clone()),
       ast_to_hir: hir::AST2HIR::new(id.clone()),
       constructor_to_enum: hir::ConstructorToEnum::default(),
       simplify: hir::Simplify::new(id.clone()),
       flattening_expression: hir::FlatExpr::new(id.clone()),
       flattening_let: hir::FlatLet::default(),
       unnest_functions: hir::UnnestFunc::new(id.clone()),
       simplify2: hir::Simplify::new(id.clone()),
       force_closure: hir::ForceClosure::default(),
       hir_to_mir: mir::HIR2MIR::new(id),
       unalias: mir::UnAlias::default(),
       block_arrange: mir::BlockArrange::default(),
       mir_to_lir: lir::MIR2LIR::default(),
       backend: backend::LIR2WASM::default(),
    ];

    let module: backend::Output = passes.trans(input, config)?;

    let mut code = Vec::new();
    module.0.dump(&mut code);
    Ok(code)
}

#[cfg(target_arch = "wasm32")]
#[cfg_attr(target_arch = "wasm32", wasm_bindgen)]
pub fn compile_string_all(input: String) -> Result<Vec<u8>, JsValue> {
    let mut prelude = include_str!("../ml_src/prelude.sml").to_string();
    prelude.push_str(&input);

    let config = Config::default();
    compile_string(prelude, &config).map_err(|e| format!("Compile failed: {}", e).into())
}
