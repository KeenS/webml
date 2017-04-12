use hir::*;
use pass::Pass;

pub struct ForceClosure;


impl ForceClosure {
    pub fn new() -> Self {
        ForceClosure
    }


    fn conv_hir(&mut self, hir: &mut HIR) {
        for val in hir.0.iter_mut() {
            self.conv_top_val(val)
        }
    }

    fn conv_top_val(&mut self, val: &mut Val) {
        self.conv_expr(&mut val.expr, true);
    }

    fn conv_expr(&mut self, expr: &mut Expr, bound: bool) {
        use hir::Expr::*;
        let assign;
        match *expr {
            Binds {
                ref mut binds,
                ref mut ret,
                ..
            } => {
                for bind in binds.iter_mut() {
                    self.conv_expr(&mut bind.expr, true);
                }
                self.conv_expr(ret, bound);
                return;
            }
            Op {
                ref mut l,
                ref mut r,
                ..
            } => {
                self.conv_expr(l.as_mut(), false);
                self.conv_expr(r, false);
                return;
            }

            Fun { ref mut body, .. } => {
                self.conv_expr(body, false);
                return;
            }
            App {
                ref mut fun,
                ref mut arg,
                ..
            } => {
                self.conv_expr(fun, false);
                self.conv_expr(arg, false);
                return;
            }
            If {
                ref mut cond,
                ref mut then,
                ref mut else_,
                ..
            } => {
                self.conv_expr(cond, bound);
                self.conv_expr(then, bound);
                self.conv_expr(else_, bound);
                return;
            }

            Sym { ref name, ref ty } => {
                if !bound {
                    return;
                }
                match *ty {
                    HTy::Fun(ref arg, ref ret) => {
                        assign = Closure {
                            envs: vec![],
                            param_ty: *arg.clone(),
                            body_ty: *ret.clone(),
                            fname: name.clone(),
                        }
                    }
                    _ => return,
                }
            }
            Closure { .. } | Lit { .. } | PrimFun { .. } => return,

        }
        *expr = assign;
    }
}

impl<E> Pass<HIR, E> for ForceClosure {
    type Target = HIR;

    fn trans(&mut self, mut hir: HIR) -> ::std::result::Result<Self::Target, E> {
        self.conv_hir(&mut hir);
        Ok(hir)
    }
}
