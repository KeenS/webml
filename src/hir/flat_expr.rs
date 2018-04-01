use prim::*;
use hir::*;
use pass::Pass;
use id::Id;
use hir::util::Transform;

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

    // because self.make_val(self.flat_expr(expr)) doesn't pass borrow checker, we need this util
    fn flat_make_val(&mut self, expr: Expr) -> (Box<Expr>, Val) {
        let expr = self.transform_expr(expr);
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

use hir::Expr::*;

impl Transform for FlatExpr {
    fn transform_binds(&mut self, ty: HTy, mut binds: Vec<Val>, ret: Box<Expr>) -> Expr {
        binds = binds
            .into_iter()
            .map(|mut val| {
                val.expr = self.transform_expr(val.expr);
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

    fn transform_binop(&mut self, ty: HTy, name: Symbol, l: Box<Expr>, r: Box<Expr>) -> Expr {
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

    fn transform_fun(
        &mut self,
        param: (HTy, Symbol),
        body_ty: HTy,
        mut body: Box<Expr>,
        captures: Vec<(HTy, Symbol)>,
    ) -> Expr {
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

    fn transform_closure(
        &mut self,
        envs: Vec<(HTy, Symbol)>,
        param_ty: HTy,
        body_ty: HTy,
        fname: Symbol,
    ) -> Expr {
        Expr::Closure {
            envs,
            param_ty,
            body_ty,
            fname,
        }
    }

    fn transform_builtin_call(&mut self, ty: HTy, fun: BIF, arg: Box<Expr>) -> Expr {
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

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
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

    fn transform_if(
        &mut self,
        ty: HTy,
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    ) -> Expr {
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

    fn transform_tuple(&mut self, tys: Vec<HTy>, tuple: Vec<Expr>) -> Expr {
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
}

impl<E> Pass<HIR, E> for FlatExpr {
    type Target = HIR;

    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, E> {
        Ok(self.transform_hir(hir))
    }
}
