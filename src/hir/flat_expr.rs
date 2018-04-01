use prim::*;
use hir::*;
use pass::Pass;
use id::Id;

pub struct FlatExpr {
    id: Id,
}

impl FlatExpr {
    pub fn new(id: Id) -> Self {
        Self { id }
    }

    pub fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#g".into(), id)
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
            Binds { mut binds, ret, ty } => {
                binds = binds
                    .into_iter()
                    .map(|mut val| {
                        val.expr = self.flat_expr(val.expr);
                        val
                    })
                    .collect();
                let (ret, retval) = self.flat_make_val(*ret);
                binds.push(retval);
                Binds {
                    binds: binds,
                    ret: ret,
                    ty: ty,
                }
            }
            BinOp { ty, name, l, r } => {
                let (l, lval) = self.flat_make_val(*l);
                let (r, rval) = self.flat_make_val(*r);
                let (ret, retval) = self.make_val(BinOp {
                    ty: ty.clone(),
                    name: name,
                    l: l,
                    r: r,
                });
                Binds {
                    ty: ty.clone(),
                    binds: vec![lval, rval, retval],
                    ret: ret,
                }
            }
            Fun {
                mut body,
                param,
                body_ty,
                captures,
            } => {
                let (ret, bodyval) = self.flat_make_val(*body);
                body = Box::new(Binds {
                    ty: body_ty.clone(),
                    binds: vec![bodyval],
                    ret: ret,
                });
                Fun {
                    body: body,
                    param: param,
                    body_ty: body_ty,
                    captures: captures,
                }
            }
            BuiltinCall { fun, arg, ty } => {
                let (arg, argval) = self.flat_make_val(*arg);
                let (ret, retval) = self.make_val(BuiltinCall {
                    fun: fun,
                    arg: arg,
                    ty: ty.clone(),
                });
                Binds {
                    ty: ty.clone(),
                    binds: vec![argval, retval],
                    ret: ret,
                }
            }
            App { fun, arg, ty } => {
                let (fun, funval) = self.flat_make_val(*fun);
                let (arg, argval) = self.flat_make_val(*arg);
                let (ret, retval) = self.make_val(App {
                    fun: fun,
                    arg: arg,
                    ty: ty.clone(),
                });
                Binds {
                    ty: ty.clone(),
                    binds: vec![funval, argval, retval],
                    ret: ret,
                }
            }
            If {
                cond,
                then,
                else_,
                ty,
            } => {
                let (cond, condval) = self.flat_make_val(*cond);
                let (then, thenval) = self.flat_make_val(*then);
                let (else_, elseval) = self.flat_make_val(*else_);
                let then = Box::new(Binds {
                    ty: ty.clone(),
                    binds: vec![thenval],
                    ret: then,
                });
                let else_ = Box::new(Binds {
                    ty: ty.clone(),
                    binds: vec![elseval],
                    ret: else_,
                });
                let e = If {
                    ty: ty.clone(),
                    cond: cond,
                    then: then,
                    else_: else_,
                };
                let (ret, retval) = self.make_val(e);
                Binds {
                    ty: ty,
                    binds: vec![condval, retval],
                    ret: ret,
                }
            }
            Tuple { tys, tuple } => {
                let (tuple, mut vals): (Vec<_>, Vec<_>) = tuple
                    .into_iter()
                    .map(|e| {
                        let (t, val) = self.flat_make_val(e);
                        (*t, val)
                    })
                    .unzip();
                let (ret, tupleval) = self.make_val(Tuple {
                    tys: tys.clone(),
                    tuple: tuple,
                });
                vals.push(tupleval);
                Binds {
                    ty: HTy::Tuple(tys.clone()),
                    binds: vals,
                    ret: ret,
                }
            }
            x @ Closure { .. } | x @ Sym { .. } | x @ Lit { .. } => x,
        }
    }
    // because self.make_val(self.flat_expr(expr)) doesn't pass borrow checker, we need this util
    fn flat_make_val(&mut self, expr: Expr) -> (Box<Expr>, Val) {
        let expr = self.flat_expr(expr);
        self.make_val(expr)
    }

    fn make_val(&mut self, expr: Expr) -> (Box<Expr>, Val) {
        let name = self.gensym();
        let ty = expr.ty().clone();
        let val = Val {
            ty: ty.clone(),
            rec: false,
            name: name.clone(),
            expr: expr,
        };
        let sym = Expr::Sym { name: name, ty: ty };
        (Box::new(sym), val)
    }
}

impl<E> Pass<HIR, E> for FlatExpr {
    type Target = HIR;

    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, E> {
        Ok(self.flat_hir(hir))
    }
}
