use crate::config::Config;
use crate::hir::util::Transform;
use crate::hir::*;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;

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
        let ty = expr.ty();
        let val = Val {
            ty: ty.clone(),
            rec: false,
            name: name.clone(),
            expr,
        };
        let sym = Expr::Sym { name, ty };
        (Box::new(sym), val)
    }
}

use crate::hir::Expr::*;

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
        Binds { binds, ret, ty }
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
            ret,
        });
        Fun {
            body,
            param,
            body_ty,
            captures,
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

    fn transform_builtin_call(&mut self, ty: HTy, fun: BIF, args: Vec<Expr>) -> Expr {
        let (args, mut vals): (Vec<_>, Vec<_>) = args
            .into_iter()
            .map(|arg| {
                let (arg, argval) = self.flat_make_val(arg);
                (*arg, argval)
            })
            .unzip();
        let (ret, retval) = self.make_val(BuiltinCall {
            fun,
            args,
            ty: ty.clone(),
        });

        vals.push(retval);
        Binds {
            ty,
            binds: vals,
            ret,
        }
    }

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        let (fun, funval) = self.flat_make_val(*fun);
        let (arg, argval) = self.flat_make_val(*arg);
        let (ret, retval) = self.make_val(App {
            fun,
            arg,
            ty: ty.clone(),
        });
        Binds {
            ty,
            binds: vec![funval, argval, retval],
            ret,
        }
    }

    fn transform_case(&mut self, ty: HTy, expr: Box<Expr>, arms: Vec<(Pattern, Expr)>) -> Expr {
        let (expr, exprval) = self.flat_make_val(*expr);
        let arms = {
            let arm_and_val = arms.into_iter().map(|(pat, expr)| {
                let (expr, exprval) = self.flat_make_val(expr);
                ((pat, expr), exprval)
            });
            arm_and_val
                .map(|((pat, arm), armval)| {
                    (
                        pat,
                        Binds {
                            ty: ty.clone(),
                            binds: vec![armval],
                            ret: arm,
                        },
                    )
                })
                .collect()
        };
        let e = Case {
            ty: ty.clone(),
            expr,
            arms,
        };
        let (ret, retval) = self.make_val(e);
        Binds {
            ty,
            binds: vec![exprval, retval],
            ret,
        }
    }

    fn transform_constructor(
        &mut self,
        ty: HTy,
        arg: Option<Box<Expr>>,
        descriminant: u32,
    ) -> Expr {
        if let Some(arg) = arg {
            let (arg, exprval) = self.flat_make_val(*arg);
            let (ret, constval) = self.make_val(Constructor {
                ty: ty.clone(),
                descriminant,
                arg: Some(arg),
            });
            Binds {
                ty,
                binds: vec![exprval, constval],
                ret,
            }
        } else {
            let (ret, constval) = self.make_val(Constructor {
                ty: ty.clone(),
                descriminant,
                arg,
            });
            Binds {
                ty,
                binds: vec![constval],
                ret,
            }
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
            tuple,
        });
        vals.push(tupleval);
        Binds {
            ty: HTy::Tuple(tys),
            binds: vals,
            ret,
        }
    }
}

impl<E> Pass<Context, E> for FlatExpr {
    type Target = Context;

    fn trans(
        &mut self,
        Context(symbol_table, hir): Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        Ok(Context(symbol_table, self.transform_hir(hir)))
    }
}
