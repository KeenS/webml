#[macro_use]
extern crate webml;
use webml::*;
use webml::pass::{DebugPass, PPPass};

fn main() {
    let input1 = b"val x = 1
val y=false
val z = y
val b = 1 + 2 * 3 + 4
val c = (1 + 2) * 3 + 4
val d = fn x => fn y => x + y
val e = if true then b else c
val f = let val d = 1 in d + c end
val g = d 1 2
val h = let val y = 1 in fn x => x + y end
val a = x + 2
";

    let input2 = b"
val a = let
  val b = let
    val c = 1
    val d = 2
  in
    c + d * 3 + 4
  end
  val e = if let val f = true in f end
          then let val g = true in g end
          else let val h = false in h end
in
 (let
    val i = fn x => fn y => x + y
  in
    i
  end) (let
    val j = b + b
  in
   j
  end) 2
end
val x = 1

";

    let input3 = b"val f = fn x => fn y => x + y
val x = 1";

    let mut passes = compile_pass![
        parse,
        TyEnv::new(),
        hir::AST2HIR,
        hir::Rename::new(),
        // TODO: val hoisting
        !hir::ClosureConv::new(),
        hir::FlatExpr::new(),
        hir::FlatLet::new(),
//        !mir::HIR2MIR::new(),
    ];

    passes.trans(input1).unwrap();

}
