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
    fn transform_binds(&mut self, ty: HTy, mut bind: Box<Val>, ret: Box<Expr>) -> Expr {
        bind = Box::new(self.transform_val(*bind));
        let (ret, retval) = self.flat_make_val(*ret);
        Let {
            ty: ty.clone(),
            bind,
            ret: Box::new(Let {
                ty,
                bind: Box::new(retval),
                ret,
            }),
        }
    }

    fn transform_fun(
        &mut self,
        param: (HTy, Symbol),
        body_ty: HTy,
        body: Box<Expr>,
        captures: Vec<(HTy, Symbol)>,
    ) -> Expr {
        let (body, val) = self.flat_make_val(*body);

        Expr::Fun {
            param,
            body_ty: body_ty.clone(),
            captures,
            body: Box::new(Let {
                ty: body_ty,
                bind: Box::new(val),
                ret: body,
            }),
        }
    }

    fn transform_closure(
        &mut self,
        envs: Vec<(HTy, Symbol)>,
        param_ty: HTy,
        body_ty: HTy,
        fname: Symbol,
    ) -> Expr {
        let (expr, val) = self.make_val(Expr::Closure {
            envs,
            param_ty,
            body_ty: body_ty.clone(),
            fname,
        });
        Let {
            ty: body_ty,
            bind: Box::new(val),
            ret: expr,
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

        vals.into_iter().rev().fold(*ret, |ret, bind| Let {
            ty: ty.clone(),
            bind: Box::new(bind),
            ret: Box::new(ret),
        })
    }

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        let (fun, funval) = self.flat_make_val(*fun);
        let (arg, argval) = self.flat_make_val(*arg);
        let (ret, retval) = self.make_val(App {
            fun,
            arg,
            ty: ty.clone(),
        });

        vec![funval, argval, retval]
            .into_iter()
            .rev()
            .fold(*ret, |ret, bind| Let {
                ty: ty.clone(),
                bind: Box::new(bind),
                ret: Box::new(ret),
            })
    }

    fn transform_case(&mut self, ty: HTy, expr: Box<Expr>, arms: Vec<(Pattern, Expr)>) -> Expr {
        let (expr, exprval) = self.flat_make_val(*expr);
        let arms = arms
            .into_iter()
            .map(|(pat, expr)| {
                let (expr, exprval) = self.flat_make_val(expr);
                (
                    pat,
                    Let {
                        ty: ty.clone(),
                        bind: Box::new(exprval),
                        ret: expr,
                    },
                )
            })
            .collect();
        let (case, caseval) = self.make_val(Case {
            ty: ty.clone(),
            expr,
            arms,
        });
        Let {
            ty: ty.clone(),
            bind: Box::new(exprval),
            ret: Box::new(Let {
                ty,
                bind: Box::new(caseval),
                ret: case,
            }),
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
            let (ret, consval) = self.make_val(Constructor {
                ty: ty.clone(),
                descriminant,
                arg: Some(arg),
            });
            Let {
                ty: ty.clone(),
                bind: Box::new(exprval),
                ret: Box::new(Let {
                    ty,
                    bind: Box::new(consval),
                    ret,
                }),
            }
        } else {
            let (ret, bind) = self.make_val(Constructor {
                ty: ty.clone(),
                descriminant,
                arg,
            });
            Let {
                ty,
                bind: Box::new(bind),
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

        let ty = HTy::Tuple(tys);

        vals.into_iter().rev().fold(*ret, |ret, bind| Let {
            ty: ty.clone(),
            bind: Box::new(bind),
            ret: Box::new(ret),
        })
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
