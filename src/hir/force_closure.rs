use std::collections::HashSet;

use hir::*;
use pass::Pass;


pub struct ForceClosure {
    functions: HashSet<Symbol>,
}


impl ForceClosure {
    pub fn new() -> Self {
        ForceClosure { functions: HashSet::new() }
    }


    fn conv_hir(&mut self, hir: &mut HIR) {
        for val in hir.0.iter_mut() {
            self.register_top_val(val)
        }

        for val in hir.0.iter_mut() {
            self.conv_top_val(val)
        }
    }

    fn conv_top_val(&mut self, val: &mut Val) {
        self.conv_expr(&mut val.expr, true);
    }

    fn register_top_val(&mut self, val: &mut Val) {
        let mut bound_name = None;
        if val.rec {
            bound_name = Some(&val.name);
        }
        self.register_functions(&mut val.expr, bound_name);
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
                if !bound || !self.functions.contains(name) {
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

    fn register_functions(&mut self, expr: &mut Expr, bound_name: Option<&Symbol>) {
        use hir::Expr::*;
        match *expr {
            Binds {
                ref mut binds,
                ref mut ret,
                ..
            } => {
                for bind in binds.iter_mut() {
                    let mut bound_name = None;
                    if bind.rec {
                        bound_name = Some(&bind.name);
                    }
                    self.register_functions(&mut bind.expr, bound_name);
                }
                self.register_functions(ret, None);
                return;
            }
            Op {
                ref mut l,
                ref mut r,
                ..
            } => {
                self.register_functions(l.as_mut(), None);
                self.register_functions(r, None);
                return;
            }

            Fun { ref mut body, .. } => {
                match bound_name {
                    Some(name) => {
                        self.functions.insert(name.clone());
                    }
                    None => (),
                };
                self.register_functions(body, None);
                return;
            }
            App {
                ref mut fun,
                ref mut arg,
                ..
            } => {
                self.register_functions(fun, None);
                self.register_functions(arg, None);
                return;
            }
            If {
                ref mut cond,
                ref mut then,
                ref mut else_,
                ..
            } => {
                self.register_functions(cond, None);
                self.register_functions(then, None);
                self.register_functions(else_, None);
                return;
            }

            Sym { .. } | Closure { .. } | Lit { .. } | PrimFun { .. } => return,

        }
    }
}

impl<E> Pass<HIR, E> for ForceClosure {
    type Target = HIR;

    fn trans(&mut self, mut hir: HIR) -> ::std::result::Result<Self::Target, E> {
        self.conv_hir(&mut hir);
        Ok(hir)
    }
}
