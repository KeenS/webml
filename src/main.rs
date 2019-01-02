extern crate web_assembler as wasm;
#[macro_use]
extern crate webml;
use crate::wasm::Dump;
use std::env;
use std::fs::File;
use std::io::{BufReader, Read, Write};
#[allow(unused_imports)]
use webml::pass::{ConvError, DebugPass, PPPass};
use webml::*;

fn main() {
    let filename = env::args()
        .nth(1)
        .unwrap_or("ml_example/example1.sml".to_string());

    let input = {
        let file =
            File::open(filename.clone()).expect(&format!("input file {} doesn't exist", filename));
        let mut br = BufReader::new(file);
        let mut s = String::new();
        br.read_to_string(&mut s).unwrap();
        s
    };

    let id = id::Id::new();

    let mut passes = compile_pass![
        ConvError::new(parse),
        ast::TyEnv::new(),
        ast::CaseCheck::new(),
        hir::AST2HIR,
        hir::Rename::new(id.clone()),
        hir::FindBuiltin::new(),
        hir::FlatExpr::new(id.clone()),
        hir::FlatLet::new(),
        hir::UnnestFunc::new(id.clone()),
        hir::ForceClosure::new(),
        mir::HIR2MIR::new(id.clone()),
        mir::UnAlias::new(),
        mir::BlockArrange::new(),
        !lir::MIR2LIR::new(),
        backend::LIR2WASM::new(),
    ];

    let module: Result<wasm::Module, TypeError> = passes.trans(&input);

    let module = module.unwrap();
    let mut code = Vec::new();
    module.dump(&mut code);
    let mut out = File::create("out.wasm").unwrap();
    out.write(&code).unwrap();
}
