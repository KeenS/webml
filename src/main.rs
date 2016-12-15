extern crate webml;
use webml::*;
use webml::ast::*;
use webml::ast::AST::*;
use webml::ast::Expr::*;

fn main() {

    let mut tyenv = TyEnv::new();
    let ast1 = TopVal(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("x".to_string()),
        expr: Add{ty: ty::TyDefer::empty(),
                  l: Box::new(LitInt(1)),
                  r: Box::new(LitInt(2)),
        }
    });
    let ast2 = TopVal(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("y".to_string()),
        expr: If{ty: ty::TyDefer::empty(),
                 cond: Box::new(LitBool(true)),
                  then: Box::new(LitInt(1)),
                  else_: Box::new(LitInt(2)),
        }
    });

    let ast3 = TopVal(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("z".to_string()),
        expr: If{ty: ty::TyDefer::empty(),
                 cond: Box::new(LitBool(true)),
                 then: Box::new(Sym(Symbol("x".to_string()))),
                 else_: Box::new(Sym(Symbol("y".to_string()))),
        }
    });

    let ast4 = TopVal(Val {
        ty: ty::TyDefer::empty(),
        name: Symbol("z".to_string()),
        expr: Fun{ty: ty::TyDefer::empty(),
                  arg: Symbol("a".to_string()),
                  body: Box::new(Add{ty: ty::TyDefer::empty(),
                           l: Box::new(LitInt(1)),
                           r: Box::new(Sym(Symbol("a".to_string()))),
                  }),
        }
    });


    let mut asts = vec![
        ast1,
        ast2,
        ast3,
        ast4
    ];
    tyenv.infer(&mut asts).unwrap();
    for ast in asts {
        println!("{:?}", ast);
    }

}
