use crate::config::Config;
use crate::hir::util::Transform;
use crate::hir::*;
use crate::pass::Pass;

pub struct FlatLet;

fn take_binds(expr: Expr) -> (Expr, Vec<Val>) {
    use crate::hir::Expr::*;
    match expr {
        Let { bind, ret, .. } => {
            let (expr, mut binds) = take_binds(*ret);
            binds.insert(0, *bind);
            (expr, binds)
        }
        BuiltinCall { args, ty, fun } => {
            let (args, bindss): (_, Vec<_>) = args.into_iter().map(take_binds).unzip();
            let expr = BuiltinCall { fun, args, ty };
            (expr, bindss.into_iter().flat_map(Vec::into_iter).collect())
        }
        ExternCall {
            args,
            ty,
            module,
            fun,
        } => {
            let (args, bindss): (_, Vec<_>) = args.into_iter().map(take_binds).unzip();
            let expr = ExternCall {
                module,
                fun,
                args,
                ty,
            };
            (expr, bindss.into_iter().flat_map(Vec::into_iter).collect())
        }
        App {
            mut fun,
            mut arg,
            ty,
        } => {
            let (f, mut fbinds) = take_binds(*fun);
            let (a, mut abinds) = take_binds(*arg);
            fun = Box::new(f);
            arg = Box::new(a);
            fbinds.append(&mut abinds);
            let expr = App { fun, arg, ty };
            (expr, fbinds)
        }
        Case { mut expr, arms, ty } => {
            let (e, ebinds) = take_binds(*expr);
            expr = Box::new(e);
            let expr = Case { expr, arms, ty };
            (expr, ebinds)
        }
        Tuple { tys, tuple } => {
            let (tuple, bindss): (_, Vec<_>) = tuple.into_iter().map(take_binds).unzip();
            let expr = Tuple { tys, tuple };
            (expr, bindss.into_iter().flat_map(Vec::into_iter).collect())
        }
        Proj { ty, tuple, index } => {
            let (t, binds) = take_binds(*tuple);
            let proj = Proj {
                ty,
                index,
                tuple: Box::new(t),
            };
            (proj, binds)
        }
        Constructor {
            ty,
            descriminant,
            arg,
        } => {
            if let Some(arg) = arg {
                let (arg, binds) = take_binds(*arg);
                (
                    Constructor {
                        ty,
                        descriminant,
                        arg: Some(Box::new(arg)),
                    },
                    binds,
                )
            } else {
                (
                    Constructor {
                        ty,
                        descriminant,
                        arg,
                    },
                    Vec::new(),
                )
            }
        }
        x @ Fun { .. } | x @ Closure { .. } | x @ Sym { .. } | x @ Lit { .. } => (x, Vec::new()),
    }
}

impl Transform for FlatLet {
    fn transform_binds(&mut self, ty: HTy, bind: Box<Val>, ret: Box<Expr>) -> Expr {
        let mut vec = Vec::new();
        let mut val = *bind;
        val.expr = self.transform_expr(val.expr);
        let (expr, mut binds) = take_binds(val.expr);
        val.expr = expr;
        vec.append(&mut binds);
        vec.push(val);

        let ret = self.transform_expr(*ret);
        let (ret, mut binds) = take_binds(ret);
        vec.append(&mut binds);

        vec.into_iter().rev().fold(ret, |ret, bind| Expr::Let {
            bind: Box::new(bind),
            ret: Box::new(ret),
            ty: ty.clone(),
        })
    }
}

impl FlatLet {
    pub fn new() -> Self {
        FlatLet
    }
}

impl<E> Pass<Context, E> for FlatLet {
    type Target = Context;

    fn trans(
        &mut self,
        Context(symbol_table, hir): Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        Ok(Context(symbol_table, self.transform_hir(hir)))
    }
}
