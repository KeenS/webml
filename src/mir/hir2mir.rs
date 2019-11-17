use super::builder::*;
use crate::config::Config;
use crate::hir;
use crate::id::Id;
use crate::mir::*;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashMap;

pub struct HIR2MIR {
    label: u64,
    id: Id,
    closure_wrapper: HashMap<Symbol, (Symbol, EbbTy, EbbTy)>,
}

fn from(ty: hir::HTy) -> EbbTy {
    use crate::hir::HTy::*;
    match ty {
        Bool => EbbTy::Bool,
        Int => EbbTy::Int,
        Float => EbbTy::Float,
        Tuple(tys) => match tys.len() {
            0 => EbbTy::Unit,
            // TODO: treat 1-tuple as inner type
            _ => EbbTy::Tuple(tys.into_iter().map(from).collect()),
        },
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

    fn gensym(&mut self, name: &str) -> Symbol {
        let name = format!("{}", name);
        let id = self.id.next();
        Symbol(name, id)
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
        use crate::hir::Expr::*;
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
            Case { ty, expr, arms } => {
                let joinlabel = self.genlabel("join");
                let (eb, var) = self.trans_expr_block(fb, eb, expr.ty(), *expr);
                let (default, arms): (Vec<_>, _) = arms
                    .into_iter()
                    .partition(|&(ref pat, _)| pat.is_irrefutable());
                assert!(
                    default.len() <= 1,
                    "default like branch must be at most one"
                );
                let default = default.into_iter().next();
                let default_label = default.as_ref().map(|_| (self.genlabel("default"), true));
                let arms = arms
                    .into_iter()
                    .enumerate()
                    .map(|(n, (pat, expr))| {
                        (
                            pat.match_key(),
                            self.genlabel(&format!("branch_arm_{}", n)),
                            expr,
                        )
                    })
                    .collect::<Vec<_>>();
                let labels = arms
                    .iter()
                    .map(|&(key, ref label, _)| (key, label.clone(), true))
                    .collect::<Vec<_>>();
                // an easy optimization of non branching case
                let ebb;
                if labels.is_empty() && default_label.is_some() {
                    let (label, is_forward) = default_label.clone().unwrap();
                    ebb = eb.jump(label, is_forward, vec![var]);
                } else {
                    ebb = eb.branch(var, labels, default_label.clone());
                }

                fb.add_ebb(ebb);

                for (_key, label, arm) in arms {
                    let eb = EBBBuilder::new(label, Vec::new());
                    let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), arm);
                    let ebb = eb.jump(joinlabel.clone(), true, vec![var]);
                    fb.add_ebb(ebb);
                }
                match (default, default_label) {
                    (Some((pat, arm)), Some((label, _))) => {
                        let eb = match pat {
                            hir::Pattern::Var { name, ty } => {
                                let eb = EBBBuilder::new(label, vec![(from(ty.clone()), name)]);
                                eb
                            }
                            hir::Pattern::Tuple { ty, tuple } => {
                                let ty = from(ty);
                                let var = self.gensym("tuple");
                                let mut eb =
                                    EBBBuilder::new(label, vec![(from(ty.clone()), var.clone())]);
                                for (i, (t, ty)) in tuple.into_iter().zip(tys).enumerate() {
                                    eb.proj(t, from(ty), i as u32, var.clone());
                                }
                                eb
                            }
                            hir::Pattern::Lit { .. } => unreachable!(),
                        };

                        let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), arm);
                        let ebb = eb.jump(joinlabel.clone(), true, vec![var]);
                        fb.add_ebb(ebb);
                    }
                    _ => (),
                }
                let eb = EBBBuilder::new(joinlabel, vec![(from(ty), name)]);
                eb
            }
            Tuple { tys, tuple } => {
                let tys = tys.into_iter().map(from).collect();
                let tuple = tuple.into_iter().map(force_symbol).collect();
                eb.tuple(name, tys, tuple);
                eb
            }
            Proj { ty, index, tuple } => {
                let ty = from(ty);
                let tuple = force_symbol(*tuple);
                eb.proj(name, ty, index, tuple);
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
        use crate::hir::Expr::*;
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
        use crate::hir::Expr::*;
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

    fn trans(&mut self, hir: hir::HIR, _: &Config) -> ::std::result::Result<Self::Target, E> {
        Ok(self.trans_hir(hir))
    }
}
