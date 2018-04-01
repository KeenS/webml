use mir::*;
use super::builder::*;
use prim::*;
use pass::Pass;
use hir;
use std::collections::HashMap;
use id::Id;

pub struct HIR2MIR {
    label: u64,
    id: Id,
    closure_wrapper: HashMap<Symbol, (Symbol, EbbTy, EbbTy)>,
}

fn from(ty: hir::HTy) -> EbbTy {
    use hir::HTy::*;
    match ty {
        Unit => EbbTy::Unit,
        Bool => EbbTy::Bool,
        Int => EbbTy::Int,
        Float => EbbTy::Float,
        Tuple(tys) => EbbTy::Tuple(tys.into_iter().map(from).collect()),
        Fun(arg, ret) => EbbTy::Cls {
            closures: vec![],
            param: Box::new(from(*arg)),
            ret: Box::new(from(*ret)),
        },
    }
}

impl HIR2MIR {
    pub fn new(id: Id) -> Self {
        HIR2MIR {
            id,
            label: 0,
            closure_wrapper: HashMap::new(),
        }
    }

    fn genlabel(&mut self, name: &str) -> Symbol {
        let name = format!("{}", name);
        let label = self.label;
        self.label += 1;
        Symbol(name, label)
    }

    fn wrapper_name(&mut self, mut name: Symbol) -> Symbol {
        name.1 = self.id.next();
        name.0.push_str("_closure_wrapper");
        name
    }

    fn to_make_closure_wrapper(
        &mut self,
        fname: Symbol,
        param_ty: EbbTy,
        body_ty: EbbTy,
    ) -> Symbol {
        // tmp name
        let wrapper_name = self.wrapper_name(fname.clone());
        let wrapper_name = self.closure_wrapper.entry(fname).or_insert((
            wrapper_name,
            param_ty.clone(),
            body_ty.clone(),
        ));
        wrapper_name.0.clone()
    }

    fn trans_hir(&mut self, hir: hir::HIR) -> MIR {
        // TODO: make anonymous
        let mut mainbuilder = FunctionBuilder::new(Symbol::new("sml-main"), EbbTy::Unit);
        let mut mainebuilder = EBBBuilder::new(self.genlabel("entry"), Vec::new());
        let mut funs = Vec::new();

        for val in hir.0.into_iter() {
            mainebuilder = self.trans_val(&mut funs, &mut mainbuilder, mainebuilder, val);
        }
        for (fname, (wrapper_name, param_ty, ret_ty)) in self.closure_wrapper.clone().into_iter() {
            self.make_wrapper(
                &mut funs,
                fname.clone(),
                wrapper_name.clone(),
                param_ty.clone(),
                ret_ty.clone(),
            );
        }

        let ebb = mainebuilder.ret(None, EbbTy::Unit);
        mainbuilder.add_ebb(ebb);
        let main = mainbuilder.build();
        funs.push(main);
        MIR(funs)
    }

    fn make_wrapper(
        &mut self,
        funs: &mut Vec<Function>,
        fname: Symbol,
        wrapper_name: Symbol,
        param_ty: EbbTy,
        ret_ty: EbbTy,
    ) {
        let param = Symbol::new("param");
        let mut fb = FunctionBuilder::new(wrapper_name, ret_ty.clone());
        let mut eb = EBBBuilder::new(
            Symbol::new("entry"),
            vec![
                (EbbTy::Tuple(vec![]), Symbol::new("_")),
                (param_ty, param.clone()),
            ],
        );
        let ret = Symbol::new("ret");
        eb.call(ret.clone(), ret_ty.clone(), fname, vec![param]);
        let ebb = eb.ret(ret, ret_ty);
        fb.add_ebb(ebb);
        let f = fb.build();
        funs.push(f);
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
            } => {
                //                assert_eq!(body_ty, ty_);
                let param = (from(param.0), param.1);
                let mut eb_;
                if !captures.is_empty() {
                    // make closured function
                    let (tuples, vars): (Vec<_>, Vec<_>) = captures
                        .into_iter()
                        .map(|(ty, var)| (from(ty), var))
                        .unzip();
                    let closure = Symbol::new("env");
                    eb_ = EBBBuilder::new(
                        Symbol::new("entry"),
                        vec![(EbbTy::Tuple(tuples.clone()), closure.clone()), param],
                    );
                    for (i, (v, ty)) in vars.into_iter().zip(tuples).enumerate() {
                        eb_.proj(v, ty, i as u32, closure.clone());
                    }
                } else {
                    // make pure function
                    eb_ = EBBBuilder::new(Symbol::new("entry"), vec![param]);
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
            BuiltinCall { ty, fun, arg } => {
                assert_eq!(ty, ty_);
                let arg = force_symbol(*arg);
                eb.builtin_call(name, from(ty), fun, vec![arg]);
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
            BinOp {
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
                    "-" => eb.sub(name, from(ty), l, r),
                    "*" => eb.mul(name, from(ty), l, r),
                    "div" => eb.div_int(name, from(ty), l, r),
                    "/" => eb.div_float(name, from(ty), l, r),
                    "mod" => eb.mod_(name, from(ty), l, r),
                    "=" => eb.eq(name, from(ty), l, r),
                    "<>" => eb.neq(name, from(ty), l, r),
                    ">" => eb.gt(name, from(ty), l, r),
                    ">=" => eb.ge(name, from(ty), l, r),
                    "<" => eb.lt(name, from(ty), l, r),
                    "<=" => eb.le(name, from(ty), l, r),
                    op => panic!("internal error, unknow, op '{}'", op),
                };
                eb
            }
            Closure {
                envs,
                param_ty,
                body_ty,
                mut fname,
            } => {
                let param_ty = from(param_ty);
                let body_ty = from(body_ty);
                if envs.is_empty() {
                    let wrapper_name =
                        self.to_make_closure_wrapper(fname, param_ty.clone(), body_ty.clone());
                    fname = wrapper_name;
                }
                let envs = envs.into_iter().map(|(ty, var)| (from(ty), var)).collect();
                eb.closure(name, param_ty, body_ty, fname, envs);
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
