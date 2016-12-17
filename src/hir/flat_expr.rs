use prim::*;
use hir::*;
use pass::Pass;

pub struct FlatExpr {
    id: usize,
}

impl FlatExpr {
    pub fn new() -> Self {
        FlatExpr {
            id: 0
        }
    }

    pub fn gensym(&mut self) -> Symbol {
        let name = format!("#g{:06}", self.id);
        self.id += 1;
        Symbol(name)
    }

    pub fn flat_hir(&mut self, mut hir: HIR) -> HIR {
        hir.0 = hir.0.into_iter().map(|val| self.flat_val(val)).collect();
        hir
    }

    fn flat_val(&mut self, mut val: Val) -> Val {
        val.expr = self.flat_expr(val.expr);
        val
    }

    fn flat_expr(&mut self, expr: Expr) -> Expr {
        use hir::Expr::*;
        match expr {
            Binds{mut binds, mut ret, ty} => {
                binds = binds.into_iter().map(|mut val| {
                    val.expr = self.flat_expr(val.expr);
                    val
                }).collect();
                let ret_ = self.flat_expr(*ret);
                ret = Box::new(ret_);
                Binds {binds: binds, ret: ret, ty: ty}
            },
            Fun{mut body, ty, param} => {
                body = Box::new(self.flat_expr(*body));
                Fun{body: body, ty: ty, param: param}
            }
            App{mut fun, mut arg, ty} => {
                let fun_ = self.flat_expr(*fun);
                let arg_ = self.flat_expr(*arg);
                let funsym = self.gensym();
                let fun_ty = fun_.ty().clone();
                let argsym = self.gensym();
                let arg_ty = arg_.ty().clone();
                fun = Box::new(Sym{ty: fun_ty.clone(), name: funsym.clone()});
                arg = Box::new(Sym{ty: arg_ty.clone(), name: argsym.clone()});
                Binds{
                    ty: ty.clone(),
                    binds: vec![
                        Val{ty: fun_ty, name: funsym, expr: fun_},
                        Val{ty: arg_ty, name: argsym, expr: arg_}
                    ],
                    ret: Box::new(App{fun: fun, arg: arg, ty: ty})
                }
            }
            If {mut cond, mut then, mut else_, ty} => {
                cond = Box::new(self.flat_expr(*cond));
                then = Box::new(self.flat_expr(*then));
                else_ = Box::new(self.flat_expr(*else_));
                If {ty: ty, cond: cond, then: then, else_: else_}
            }
            x @ PrimFun{..} |
            x @ Sym{..} |
            x @ Lit{..} => x

        }
    }
}


impl Pass<HIR> for FlatExpr {
    type Target = HIR;
    type Err = TypeError;

    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.flat_hir(hir))
    }
}
