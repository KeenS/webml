extern crate web_assembler as wasm;
#[macro_use]
extern crate webml;
use std::env;
use std::fs;
use wasm::Dump;
#[allow(unused_imports)]
use webml::pass::{ConvError, DebugPass, PPPass};
use webml::*;

fn compile_str(input: &str) -> Result<Vec<u8>, TypeError> {
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

    let module: wasm::Module = passes.trans(input)?;

    let mut code = Vec::new();
    module.dump(&mut code);
    Ok(code)
}

fn main() {
    let filename = env::args()
        .nth(1)
        .unwrap_or("ml_example/example1.sml".to_string());

    let input = fs::read_to_string(filename).expect("failed to load file");
    let code = compile_str(&input).unwrap();
    fs::write("out.wasm", &code).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_compile_pass {
        ($filename: expr) => {
            let input = fs::read_to_string($filename).unwrap();
            compile_str(&input).expect(&format!("failed to compile {}", $filename));
        };
    }

    #[test]
    fn examples_compile_pass() {
        use walkdir::WalkDir;
        println!("called");
        for entry in WalkDir::new("ml_example")
            .into_iter()
            .filter(|e| e.as_ref().map(|e| e.file_type().is_file()).unwrap_or(false))
        {
            let path = entry.unwrap().into_path();
            println!("path: {}", path.to_str().unwrap());
            assert_compile_pass!(path.to_str().unwrap());
        }
    }
}
