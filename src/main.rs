extern crate web_assembler as wasm;
#[macro_use]
extern crate webml;
use clap::{app_from_crate, crate_authors, crate_description, crate_name, crate_version, Arg};
use std::collections::HashSet;
use std::env;
use std::fs;
use wasm::Dump;
use webml::pass::{ConvError, PrintablePass};
use webml::*;

fn compile_str<'a>(input: &'a str, config: &Config) -> Result<Vec<u8>, TypeError<'a>> {
    let id = id::Id::new();

    let mut passes = compile_pass![
       parse: ConvError::new(parse),
       typing: ast::Typing::new(),
       case_check: ast::CaseCheck::new(),
       ast_to_hir: hir::AST2HIR,
       rename: hir::Rename::new(id.clone()),
       find_buildin: hir::FindBuiltin::new(),
       flattening_expresion: hir::FlatExpr::new(id.clone()),
       flattening_let: hir::FlatLet::new(),
       unnest_functions: hir::UnnestFunc::new(id.clone()),
       closure_conversion: hir::ForceClosure::new(),
       hir_to_mir: mir::HIR2MIR::new(id.clone()),
       unalias: mir::UnAlias::new(),
       block_arrange: mir::BlockArrange::new(),
       mir_to_lir: lir::MIR2LIR::new(),
       backend: backend::LIR2WASM::new(),
    ];

    let module: wasm::Module = passes.trans(input, config)?;

    let mut code = Vec::new();
    module.dump(&mut code);
    Ok(code)
}

fn main() {
    let matches = app_from_crate!()
        .arg(
            Arg::with_name("PRINT_IR")
                .long("print-ir")
                .help("print the output of IR")
                .value_name("IR")
                .takes_value(true)
                .multiple(true),
        )
        .arg(
            Arg::with_name("INPUT")
                .help("file to compile")
                .required(true),
        )
        .get_matches();

    let filename = matches
        .value_of("INPUT")
        .unwrap_or("ml_example/example1.sml");
    let pretty_print_ir = matches
        .values_of("PRINT_IR")
        .into_iter()
        .flat_map(|vs| vs)
        .map(|s| s.to_string())
        .collect::<HashSet<String>>();

    let config = Config {
        pretty_print_ir,
        ..Default::default()
    };

    let input = fs::read_to_string(filename).expect("failed to load file");
    let code = compile_str(&input, &config).unwrap();
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
        for entry in WalkDir::new("ml_example")
            .into_iter()
            .filter(|e| e.as_ref().map(|e| e.file_type().is_file()).unwrap_or(false))
        {
            let path = entry.unwrap().into_path();
            assert_compile_pass!(path.to_str().unwrap());
        }
    }
}
