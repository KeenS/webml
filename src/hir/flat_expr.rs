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
        let name = format!("#g{}", self.id);
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
                let retsym = self.gensym();
                let ret_ = self.flat_expr(*ret);
                let retty = ret_.ty();
                binds.push(Val{ty: retty.clone(), rec: false, name: retsym.clone(), expr: ret_});
                ret = Box::new(Sym{ty: retty, name: retsym.clone()});
                Binds {binds: binds, ret: ret, ty: ty}
            },
            Op{ty, name, mut l, mut r} => {
                let l_ = self.flat_expr(*l);
                let lty = l_.ty().clone();
                let lsym = self.gensym();
                l = Box::new(Sym{ty: lty.clone(), name: lsym.clone()});
                let r_ = self.flat_expr(*r);
                let rsym = self.gensym();
                let rty = r_.ty().clone();
                r = Box::new(Sym{ty: rty.clone(), name: rsym.clone()});
                let retsym = self.gensym();
                Binds{
                    ty: ty.clone(),
                    binds: vec![
                        Val{ty: lty, rec: false, name: lsym, expr: l_},
                        Val{ty: rty, rec: false, name: rsym, expr: r_},
                        Val{ty: ty.clone(), rec: false, name: retsym.clone(), expr: Op{ty: ty.clone(), name: name, l: l, r: r}},
                    ],
                    ret: Box::new(Sym{ty: ty, name:retsym})
                }
            }
            Fun{mut body, param, body_ty, captures} => {
                let body_ = self.flat_expr(*body);
                let bodysym = self.gensym();

                body = Box::new(Binds {
                    ty: body_ty.clone(),
                    binds: vec![
                        Val{ty: body_ty.clone(), rec: false, name: bodysym.clone(), expr: body_}
                    ],
                    ret: Box::new(Sym{ty: body_ty.clone(), name: bodysym})
                });
                Fun{body: body, param: param, body_ty: body_ty, captures: captures}
            }
            App{mut fun, mut arg, ty} => {
                let fun_ = self.flat_expr(*fun);
                let funsym = self.gensym();
                let fun_ty = fun_.ty().clone();
                fun = Box::new(Sym{ty: fun_ty.clone(), name: funsym.clone()});
                let arg_ = self.flat_expr(*arg);
                let argsym = self.gensym();
                let arg_ty = arg_.ty().clone();
                arg = Box::new(Sym{ty: arg_ty.clone(), name: argsym.clone()});
                let retsym = self.gensym();
                Binds{
                    ty: ty.clone(),
                    binds: vec![
                        Val{ty: fun_ty, rec: false, name: funsym, expr: fun_},
                        Val{ty: arg_ty, rec: false, name: argsym, expr: arg_},
                        Val{ty: ty.clone(), rec: false, name: retsym.clone(), expr: App{fun: fun, arg: arg, ty: ty.clone()}},
                    ],
                    ret: Box::new(Sym{name: retsym, ty: ty})
                }
            }
            If {mut cond, mut then, mut else_, ty} => {
                cond = Box::new(self.flat_expr(*cond));
                then = Box::new(self.flat_expr(*then));
                else_ = Box::new(self.flat_expr(*else_));
                If {ty: ty, cond: cond, then: then, else_: else_}
            }
            x @ Closure{..} |
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
