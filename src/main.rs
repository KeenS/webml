#[macro_use]
extern crate webml;
use webml::*;

fn main() {

    let mut passes = compile_pass![
        parse,
        TyEnv::new(),
    ];
    let input = b"val x = 1
val y=false
val z = y
val b = 1 + 2 * 3 + 4
val c = (1 + 2) * 3 + 4
val d = fun x => fun y => x + y
val e = if true then b else c
val f = let val d = 1 in d + c end
val g = d 1 2
val a = x + 2
";
    let ret = passes.trans(input).unwrap();
    for ast in ret {
        println!("{:?}", ast);
    }

}
