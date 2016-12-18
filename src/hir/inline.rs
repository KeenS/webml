use hir::*;
use prim::*;
use pass::Pass;

pub struct Inline;

impl Inline {
    pub fn new() -> Self {
        Inline
    }

    pub fn inline_hir(&mut self, mut hir: HIR) -> HIR {
        hir.0 = hir.0.into_iter().map(|val| self.inline_val(val)).collect();
        hir
    }

    fn inline_val(&mut self, mut val: Val) -> Val {
        val.expr = self.inline_expr(val.expr);
        val
    }

    fn inline_expr(&mut self, expr: Expr) -> Expr {
        use hir::Expr::*;
        match expr {
            Binds{mut binds, mut ret, ty} => {
                binds = binds.into_iter().map(|mut val| {
                    val.expr = self.inline_expr(val.expr);
                    val
                }).collect();
                let ret_ = self.inline_expr(*ret);
                ret = Box::new(ret_);
                Binds {binds: binds, ret: ret, ty: ty}
            },
            Fun{mut body, ty, param} => {
                body = Box::new(self.inline_expr(*body));
                Fun{body: body, ty: ty, param: param}
            }
            App{fun, arg, ty} => {
                let fun = *fun;
                match fun {
                    Fun{mut body, param, ..} => {
                        body = Box::new(self.inline_expr(*body));
                        let arg_ty = arg.ty().clone();

                        Binds{
                            ty: ty,
                            binds: vec![
                                Val{ty: arg_ty, rec: false, name: param, expr: *arg}
                            ],
                            ret: body
                        }
                    }
                    fun => App{fun: Box::new(fun), arg: arg, ty: ty}
                }
            }
            If {mut cond, mut then, mut else_, ty} => {
                cond = Box::new(self.inline_expr(*cond));
                then = Box::new(self.inline_expr(*then));
                else_ = Box::new(self.inline_expr(*else_));
                If {ty: ty, cond: cond, then: then, else_: else_}
            }
            x @ PrimFun{..} |
            x @ Sym{..} |
            x @ Lit{..} => x

        }
    }
}

impl Pass<HIR> for Inline {
    type Target = HIR;
    type Err = TypeError;

    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.inline_hir(hir))
    }
}
