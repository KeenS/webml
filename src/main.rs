extern crate webml;
use webml::*;
use webml::ast::*;
use webml::ast::AST::*;
use webml::ast::Expr::*;

fn main() {

    let mut tyenv = TyEnv::new();
    let ast1 = Top(Bind::V(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("x".to_string()),
        expr: Add{ty: ty::TyDefer::empty(),
                  l: Box::new(LitInt(1)),
                  r: Box::new(LitInt(2)),
        }
    }));
    let ast2 = Top(Bind::V(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("y".to_string()),
        expr: If{ty: ty::TyDefer::empty(),
                 cond: Box::new(LitBool(true)),
                  then: Box::new(LitInt(1)),
                  else_: Box::new(LitInt(2)),
        }
    }));

    let ast3 = Top(Bind::V(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("z".to_string()),
        expr: If{ty: ty::TyDefer::empty(),
                 cond: Box::new(LitBool(true)),
                 then: Box::new(Sym(Symbol("x".to_string()))),
                 else_: Box::new(Sym(Symbol("y".to_string()))),
        }
    }));

    let ast4 = Top(Bind::V(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("z".to_string()),
        expr: Fun{ty: ty::TyDefer::empty(),
                  arg: Symbol("a".to_string()),
                  body: Box::new(Add{ty: ty::TyDefer::empty(),
                           l: Box::new(LitInt(1)),
                           r: Box::new(Sym(Symbol("a".to_string()))),
                  }),
        }
    }));


    let mut asts = vec![
        ast1,
        ast2,
        ast3,
        ast4,
    ];
    tyenv.infer(&mut asts).unwrap();
    for ast in asts {
        println!("{:?}", ast);
    }

    let mut asts = parse(b"val x = 1
val y=false
val z = y
val b = 1 + 2 * 3 + 4
val c = (1 + 2) * 3 + 4
val d = fun x => fun y => x + y
val e = if true then b else c
val f = let val d = 1 in d + c end
val a = x + 2
").unwrap();
    println!("{:?}", asts);
    tyenv.infer(&mut asts).unwrap();
    for ast in asts {
        println!("{:?}", ast);
    }

}
