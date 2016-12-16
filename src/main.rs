#[macro_use]
extern crate webml;
use webml::*;
use webml::pass::{DebugPass, PPPass};

fn main() {
    let input = b"val x = 1
val y=false
val z = y
val b = 1 + 2 * 3 + 4
val c = (1 + 2) * 3 + 4
val d = fn x => fn y => x + y
val e = if true then b else c
val f = let val d = 1 in d + c end
val g = d 1 2
val a = x + 2
";

    let mut passes = compile_pass![
        parse,
        TyEnv::new(),
        !hir::AST2HIR,
    ];

    passes.trans(input).unwrap();

}
