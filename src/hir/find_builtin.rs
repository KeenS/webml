use hir::*;
use pass::Pass;

pub struct FindBuiltin;

impl FindBuiltin {
    fn traverse_hir(&mut self, mut hir: HIR) -> HIR {
        hir.0 = hir.0
            .into_iter()
            .map(|val| self.traverse_val(val))
            .collect();
        hir
    }

    fn traverse_val(&mut self, mut val: Val) -> Val {
        val.expr = self.traverse_expr(val.expr);
        val
    }

    fn traverse_expr(&mut self, mut expr: Expr) -> Expr {
        use hir::Expr::*;
        match expr {
            Binds { mut binds, ret, ty } => {
                binds = binds
                    .into_iter()
                    .map(|mut val| {
                        val.expr = self.traverse_expr(val.expr);
                        val
                    })
                    .collect();
                let ret = self.traverse_expr(*ret);
                Binds {
                    binds: binds,
                    ret: Box::new(ret),
                    ty: ty,
                }
            }
            BinOp { ty, name, l, r } => {
                let l = self.traverse_expr(*l);
                let r = self.traverse_expr(*r);
                BinOp {
                    ty: ty,
                    name: name,
                    l: Box::new(l),
                    r: Box::new(r),
                }
            }
            Fun {
                body,
                param,
                body_ty,
                captures,
            } => {
                let body = self.traverse_expr(*body);
                Fun {
                    body: Box::new(body),
                    param: param,
                    body_ty: body_ty,
                    captures: captures,
                }
            }
            App { fun, arg, ty } => match *fun {
                Expr::Sym { ref name, .. } if name.1 == 0 => {
                    assert!(::BUILTIN_FUNCTIONS.contains(&name.0.as_str()));
                    let fun = match name.0.as_str() {
                        "print" => BIF::Print,
                        name => unreachable!("unknown builtin function found: {}", name),
                    };
                    let arg = self.traverse_expr(*arg);
                    return BuiltinCall {
                        ty: ty.clone(),
                        fun: fun,
                        arg: Box::new(arg),
                    };
                }
                _ => {
                    let arg = self.traverse_expr(*arg);
                    let fun = self.traverse_expr(*fun);

                    App {
                        fun: Box::new(fun),
                        arg: Box::new(arg),
                        ty: ty,
                    }
                }
            },
            If {
                cond,
                then,
                else_,
                ty,
            } => {
                let cond = self.traverse_expr(*cond);
                let then = self.traverse_expr(*then);
                let else_ = self.traverse_expr(*else_);
                If {
                    ty: ty,
                    cond: Box::new(cond),
                    then: Box::new(then),
                    else_: Box::new(else_),
                }
            }
            Tuple { tys, tuple } => {
                let tuple = tuple.into_iter().map(|e| self.traverse_expr(e)).collect();
                Tuple {
                    tys: tys,
                    tuple: tuple,
                }
            }
            BuiltinCall { ty, fun, arg } => {
                let arg = self.traverse_expr(*arg);
                BuiltinCall {
                    ty: ty,
                    fun: fun,
                    arg: Box::new(arg),
                }
            }
            x @ Closure { .. } | x @ Sym { .. } | x @ Lit { .. } => x,
        }
    }
}

// fn traverse_app(&mut self, ty: &mut HTy, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
//     let prim_name;
//     if let Expr::Sym{ref mut name, ..} = **fun {
//         if name.1 == 0 {
//             assert!(::BUILTIN_FUNCTIONS.contains(&name.0.as_str()));
//             prim_name = match name.0.as_str() {
//                 "print" => BIF::Print,
//                 name => unreachable!("unknown builtin function found: {}", name)
//             }
//             // pass through to satisfy borrow checher
//         } else {
//             return
//         }
//     } else {
//         return
//     }
//     *fun = Box::new(Expr::PrimFun {
//         param_ty: arg.ty(),
//         ret_ty: ty.clone(),
//         name: prim_name,

//     });
//     self.traverse_expr(arg);
// }
impl FindBuiltin {
    pub fn new() -> Self {
        FindBuiltin
    }
}

impl<E> Pass<HIR, E> for FindBuiltin {
    type Target = HIR;

    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, E> {
        Ok(self.traverse_hir(hir))
    }
}
