use mir::*;
use super::builder::*;
use prim::*;
use pass::Pass;
use hir;

pub struct HIR2MIR {
    id: usize,
}



fn from(ty: hir::HTy) -> EbbTy {
    use hir::HTy::*;
    match ty {
        Unit => EbbTy::Unit,
        Bool => EbbTy::Bool,
        Int => EbbTy::Int,
        Float => EbbTy::Float,
        Tuple(tys) => EbbTy::Tuple(tys.into_iter().map(from).collect()),
        Fun(arg, ret) => {
            EbbTy::Cls {
                closures: vec![],
                param: Box::new(from(*arg)),
                ret: Box::new(from(*ret)),
            }
        }
    }
}


impl HIR2MIR {
    pub fn new() -> Self {
        HIR2MIR { id: 0 }
    }

    fn genlabel(&mut self, name: &str) -> Symbol {
        let name = format!("{}@{}", name, self.id);
        self.id += 1;
        Symbol(name)
    }

    fn trans_hir(&mut self, hir: hir::HIR) -> MIR {
        // TODO: make anonymous
        let mut mainbuilder = FunctionBuilder::new(Symbol("main".to_string()), EbbTy::Unit);
        let mut mainebuilder = EBBBuilder::new(self.genlabel("entry"), Vec::new());
        let mut funs = Vec::new();

        for val in hir.0.into_iter() {
            mainebuilder = self.trans_val(&mut funs, &mut mainbuilder, mainebuilder, val);
        }
        let ebb = mainebuilder.ret(None, EbbTy::Unit);
        mainbuilder.add_ebb(ebb);
        let main = mainbuilder.build();
        funs.push(main);
        MIR(funs)
    }

    fn trans_val(
        &mut self,
        funs: &mut Vec<Function>,
        fb: &mut FunctionBuilder,
        mut eb: EBBBuilder,
        val: hir::Val,
    ) -> EBBBuilder {
        use hir::Expr::*;
        let hir::Val {
            ty: ty_,
            name,
            expr,
            ..
        } = val;
        match expr {
            Fun {
                body,
                param,
                body_ty,
                captures,
                make_closure,
            } => {
                //                assert_eq!(body_ty, ty_);
                let param = (from(param.0), param.1);
                let mut eb_;
                // assuming all the flags are set
                if make_closure.unwrap() {
                    // make closured function
                    let (tuples, vars): (Vec<_>, Vec<_>) = captures
                        .into_iter()
                        .map(|(ty, var)| (from(ty), var))
                        .unzip();
                    let closure = Symbol("env".to_string());
                    eb_ = EBBBuilder::new(
                        Symbol("entry".to_string()),
                        vec![(EbbTy::Tuple(tuples.clone()), closure.clone()), param],
                    );
                    for (i, (v, ty)) in vars.into_iter().zip(tuples).enumerate() {
                        eb_.proj(v, ty, i as u32, closure.clone());
                    }
                } else {
                    // make pure function
                    eb_ = EBBBuilder::new(Symbol("entry".to_string()), vec![param]);
                }
                let mut fb = FunctionBuilder::new(name, from(body_ty.clone()));
                let ebb = self.trans_expr(&mut fb, eb_, body_ty, *body);
                fb.add_ebb(ebb);
                let function = fb.build();
                funs.push(function);
                eb
            }
            e @ Sym { .. } | e @ Binds { .. } => {
                let (mut eb, var) = self.trans_expr_block(fb, eb, ty_.clone(), e);
                eb.alias(name, from(ty_), var);
                eb
            }
            App { ty, fun, arg } => {
                assert_eq!(ty, ty_);
                let arg = force_symbol(*arg);
                let fun = force_symbol(*fun);
                eb.call(name, from(ty), fun, vec![arg]);
                eb
            }
            If {
                ty,
                cond,
                then,
                else_,
            } => {
                let thenlabel = self.genlabel("then");
                let elselabel = self.genlabel("else");
                let joinlabel = self.genlabel("join");
                let (eb, var) = self.trans_expr_block(fb, eb, hir::HTy::Bool, *cond);
                let ebb = eb.branch(var, thenlabel.clone(), true, elselabel.clone(), true);
                fb.add_ebb(ebb);

                let eb = EBBBuilder::new(thenlabel, Vec::new());
                let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), *then);
                let ebb = eb.jump(joinlabel.clone(), true, vec![var]);
                fb.add_ebb(ebb);

                let eb = EBBBuilder::new(elselabel, Vec::new());
                let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), *else_);
                let ebb = eb.jump(joinlabel.clone(), true, vec![var]);
                fb.add_ebb(ebb);

                let eb = EBBBuilder::new(joinlabel, vec![(from(ty), name)]);
                eb
            }
            Tuple { tys, tuple } => {
                let tys = tys.into_iter().map(from).collect();
                let tuple = tuple.into_iter().map(force_symbol).collect();
                eb.tuple(name, tys, tuple);
                eb
            }
            Op {
                ty,
                name: name_,
                l,
                r,
            } => {
                assert_eq!(ty, ty_);
                let l = force_symbol(*l);
                let r = force_symbol(*r);
                match name_.0.as_ref() {
                    "+" => eb.add(name, from(ty), l, r),
                    "*" => eb.mul(name, from(ty), l, r),
                    _ => panic!("internal error"),
                };
                eb
            }
            Closure {
                envs,
                param_ty,
                body_ty,
                fname,
            } => {
                let envs = envs.into_iter().map(|(ty, var)| (from(ty), var)).collect();
                eb.closure(name, from(param_ty), from(body_ty), fname, envs);
                eb
            }
            PrimFun {
                param_ty,
                ret_ty,
                name: fname,
            } => {
                eb.alias(
                    name,
                    EbbTy::Ebb {
                        params: vec![from(param_ty)],
                        ret: Box::new(from(ret_ty)),
                    },
                    fname,
                );
                eb
            }
            Lit { ty, value } => {
                assert_eq!(ty, ty_);
                eb.lit(name, from(ty), value);
                eb
            }
        }

    }

    fn trans_expr(
        &mut self,
        fb: &mut FunctionBuilder,
        mut eb: EBBBuilder,
        ty_: hir::HTy,
        expr: hir::Expr,
    ) -> EBB {
        use hir::Expr::*;
        match expr {
            Binds { ty, binds, ret } => {
                assert_eq!(ty, ty_);
                let mut funs = Vec::new();
                for val in binds {
                    eb = self.trans_val(&mut funs, fb, eb, val);
                }
                assert_eq!(funs.len(), 0);
                eb.ret(force_symbol(*ret), from(ty))
            }
            Sym { ty, name } => {
                assert_eq!(ty, ty_);
                eb.ret(name, from(ty))
            }
            _ => panic!("internal error"),
        }
    }

    fn trans_expr_block(
        &mut self,
        fb: &mut FunctionBuilder,
        mut eb: EBBBuilder,
        ty_: hir::HTy,
        expr: hir::Expr,
    ) -> (EBBBuilder, Symbol) {
        use hir::Expr::*;
        match expr {
            Binds { ty, binds, ret } => {
                assert_eq!(ty, ty_);
                let mut funs = Vec::new();
                for val in binds {
                    eb = self.trans_val(&mut funs, fb, eb, val)
                }
                assert_eq!(funs.len(), 0);
                (eb, force_symbol(*ret))
            }
            Sym { ty, name } => {
                assert_eq!(ty, ty_);
                (eb, name)
            }
            _ => panic!("internal error"),
        }
    }
}

fn force_symbol(e: hir::Expr) -> Symbol {
    match e {
        hir::Expr::Sym { name, .. } => name,
        e => panic!("not a symbol, {:?}", e),
    }
}


impl<E> Pass<hir::HIR, E> for HIR2MIR {
    type Target = MIR;

    fn trans(&mut self, hir: hir::HIR) -> ::std::result::Result<Self::Target, E> {
        Ok(self.trans_hir(hir))
    }
}
