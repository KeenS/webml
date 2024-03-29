use super::builder::*;
use crate::config::Config;
use crate::hir;
use crate::id::Id;
use crate::mir::*;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashMap;

pub struct HIR2MIR {
    id: Id,
}

impl HIR2MIR {
    pub fn new(id: Id) -> Self {
        HIR2MIR { id }
    }

    fn generate_pass(&mut self, symbol_table: hir::SymbolTable) -> HIR2MIRPass {
        HIR2MIRPass::new(self.id.clone(), symbol_table)
    }
}

struct HIR2MIRPass {
    label: u64,
    id: Id,
    closure_wrapper: HashMap<Symbol, (Symbol, EbbTy, EbbTy)>,
    symbol_table: hir::SymbolTable,
}

impl HIR2MIRPass {
    pub fn new(id: Id, symbol_table: hir::SymbolTable) -> Self {
        HIR2MIRPass {
            id,
            label: 0,
            closure_wrapper: HashMap::new(),
            symbol_table,
        }
    }

    fn genlabel(&mut self, name: &str) -> Symbol {
        let name = name.to_string();
        let label = self.label;
        self.label += 1;
        Symbol(name, label)
    }

    fn gensym(&mut self, name: &str) -> Symbol {
        let name = name.to_string();
        let id = self.id.next();
        Symbol(name, id)
    }

    fn generate_symbol_table(&self) -> SymbolTable {
        let table = self
            .symbol_table
            .types
            .iter()
            .map(|(name, info)| (name.clone(), self.trans_type_info(info)))
            .collect();
        SymbolTable { table }
    }

    fn trans_type_info(&self, info: &hir::TypeInfo) -> EbbTy {
        let union = info
            .constructors
            .iter()
            .map(|(_, arg)| arg)
            .map(|arg| arg.as_ref().map(|ty| self.trans_ty(ty)))
            .map(|arg| arg.unwrap_or(EbbTy::Unit))
            .collect();

        EbbTy::Tuple(vec![EbbTy::Int, EbbTy::Union(union)])
    }

    fn trans_ty(&self, ty: &hir::HTy) -> EbbTy {
        use crate::hir::HTy::*;
        match ty {
            Char => EbbTy::Char,
            Int => EbbTy::Int,
            Real => EbbTy::Float,
            Tuple(tys) => match tys.len() {
                0 => EbbTy::Unit,
                // TODO: treat 1-tuple as inner type
                _ => EbbTy::Tuple(tys.iter().map(|t| self.trans_ty(t)).collect()),
            },
            Fun(arg, ret) => EbbTy::Cls {
                closures: vec![],
                param: Box::new(self.trans_ty(&*arg)),
                ret: Box::new(self.trans_ty(&*ret)),
            },
            Datatype(name) => EbbTy::Variable(name.clone()),
        }
    }

    fn trans_ty_canonical(&self, ty: &hir::HTy) -> EbbTy {
        match self.trans_ty(ty) {
            EbbTy::Variable(name) => {
                self.trans_type_info(self.symbol_table.types.get(&name).unwrap())
            }
            ty => ty,
        }
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
        let wrapper_name =
            self.closure_wrapper
                .entry(fname)
                .or_insert((wrapper_name, param_ty, body_ty));
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
                let param = (self.trans_ty(&param.0), param.1);
                let mut eb_;
                if !captures.is_empty() {
                    // make closured function
                    let (tuples, vars): (Vec<_>, Vec<_>) = captures
                        .into_iter()
                        .map(|(ty, var)| (self.trans_ty(&ty), var))
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
                let mut fb = FunctionBuilder::new(name, self.trans_ty(&body_ty));
                let ebb = self.trans_expr(&mut fb, eb_, body_ty, *body);
                fb.add_ebb(ebb);
                let function = fb.build();
                funs.push(function);
                eb
            }
            e @ Sym { .. } | e @ Let { .. } => {
                let (mut eb, var) = self.trans_expr_block(fb, eb, ty_.clone(), e);
                eb.alias(name, self.trans_ty(&ty_), var);
                eb
            }
            BuiltinCall { ty, fun, args } => {
                assert_eq!(ty, ty_);
                use crate::hir::BIF::*;
                let mut args = args.into_iter().map(force_symbol).collect::<Vec<Symbol>>();
                macro_rules! pop {
                    () => {
                        args.remove(0)
                    };
                }
                match fun {
                    AddInt => eb.add_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    AddReal => eb.add_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    SubInt => eb.sub_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    SubReal => eb.sub_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    MulInt => eb.mul_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    MulReal => eb.mul_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    DivInt => eb.div_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    DivReal => eb.div_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    ModInt => eb.mod_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    EqInt => eb.eq_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    EqReal => eb.eq_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    EqChar => eb.eq_char(name, self.trans_ty(&ty), pop!(), pop!()),
                    NeqInt => eb.neq_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    NeqReal => eb.neq_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    NeqChar => eb.neq_char(name, self.trans_ty(&ty), pop!(), pop!()),
                    GtInt => eb.gt_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    GtReal => eb.gt_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    GtChar => eb.gt_char(name, self.trans_ty(&ty), pop!(), pop!()),
                    GeInt => eb.ge_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    GeReal => eb.ge_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    GeChar => eb.ge_char(name, self.trans_ty(&ty), pop!(), pop!()),
                    LtInt => eb.lt_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    LtReal => eb.lt_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    LtChar => eb.lt_char(name, self.trans_ty(&ty), pop!(), pop!()),
                    LeInt => eb.le_int(name, self.trans_ty(&ty), pop!(), pop!()),
                    LeReal => eb.le_real(name, self.trans_ty(&ty), pop!(), pop!()),
                    LeChar => eb.le_char(name, self.trans_ty(&ty), pop!(), pop!()),
                };
                eb
            }
            ExternCall {
                ty,
                module,
                fun,
                args,
            } => {
                assert_eq!(ty, ty_);
                let args = args.into_iter().map(force_symbol).collect();
                eb.extern_call(name, self.trans_ty(&ty), module, fun, args);
                eb
            }
            App { ty, fun, arg } => {
                assert_eq!(ty, ty_);
                let arg = force_symbol(*arg);
                let fun = force_symbol(*fun);
                eb.call(name, self.trans_ty(&ty), fun, vec![arg]);
                eb
            }
            Case { ty, expr, arms } => {
                let joinlabel = self.genlabel("join");
                let exprty = expr.ty();
                let (mut eb, var) = self.trans_expr_block(fb, eb, exprty.clone(), *expr);

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
                            pat.binds(),
                            self.genlabel(&format!("branch_arm_{}", n)),
                            expr,
                        )
                    })
                    .collect::<Vec<_>>();
                let labels = arms
                    .iter()
                    .map(|&(key, _, ref label, _)| (key, label.clone(), true))
                    .collect::<Vec<_>>();

                let descriminant = self.gensym("descriminant");
                let arg = self.gensym("arg");
                enum MatchTy {
                    Tuple(Vec<EbbTy>),
                    Datatype(Vec<EbbTy>),
                    Int,
                    Char,
                }

                let exprty = match exprty {
                    hir::HTy::Tuple(tys) => {
                        MatchTy::Tuple(tys.into_iter().map(|ty| self.trans_ty(&ty)).collect())
                    }
                    hir::HTy::Datatype(name) => MatchTy::Datatype(
                        self.symbol_table.types[&name]
                            .constructors
                            .iter()
                            .map(|(_, arg)| arg)
                            .map(|ty| {
                                ty.as_ref()
                                    .map(|ty| self.trans_ty(ty))
                                    .unwrap_or(EbbTy::Unit)
                            })
                            .collect(),
                    ),
                    hir::HTy::Int => MatchTy::Int,
                    hir::HTy::Char => MatchTy::Char,
                    ty => unreachable!("{:?}", ty),
                };
                match &exprty {
                    MatchTy::Tuple(_) => {
                        // noop
                    }
                    MatchTy::Datatype(tys) => {
                        eb.proj(descriminant.clone(), EbbTy::Int, 0, var.clone());
                        eb.proj(arg.clone(), EbbTy::Union(tys.clone()), 1, var.clone());
                    }
                    MatchTy::Int => {
                        eb.alias(descriminant.clone(), EbbTy::Int, var.clone());
                    }
                    MatchTy::Char => {
                        eb.alias(descriminant.clone(), EbbTy::Char, var.clone());
                    }
                }
                // an easy optimization of non branching case

                let ebb = if labels.is_empty() && default_label.is_some() {
                    let (label, is_forward) = default_label.clone().unwrap();
                    eb.jump(label, is_forward, vec![var])
                } else {
                    eb.branch(descriminant, labels, default_label.clone())
                };

                fb.add_ebb(ebb);

                for (key, binds, label, arm) in arms {
                    let mut eb = EBBBuilder::new(label, Vec::new());
                    if let MatchTy::Datatype(tys) = &exprty {
                        let vararg = match binds {
                            Some(s) => s,
                            None => self.gensym("vararg"),
                        };
                        let argty = tys[key as usize].clone();
                        eb.select(vararg, argty, key, arg.clone());
                    }
                    let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), arm);
                    let ebb = eb.jump(joinlabel.clone(), true, vec![var]);
                    fb.add_ebb(ebb);
                }

                if let (Some((pat, arm)), Some((label, _))) = (default, default_label) {
                    let eb = match pat {
                        hir::Pattern::Var { name, ty } => {
                            EBBBuilder::new(label, vec![(self.trans_ty(&ty), name)])
                        }
                        hir::Pattern::Tuple { tys, tuple } => {
                            let ty = hir::HTy::Tuple(tys.clone());
                            let var = self.gensym("tuple");
                            let mut eb =
                                EBBBuilder::new(label, vec![(self.trans_ty(&ty), var.clone())]);
                            for (i, (t, ty)) in tuple.into_iter().zip(tys).enumerate() {
                                eb.proj(t, self.trans_ty(&ty), i as u32, var.clone());
                            }
                            eb
                        }
                        hir::Pattern::Constructor { .. }
                        | hir::Pattern::Constant { .. }
                        | hir::Pattern::Char { .. } => unreachable!(),
                    };

                    let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), arm);
                    let ebb = eb.jump(joinlabel.clone(), true, vec![var]);
                    fb.add_ebb(ebb);
                }
                EBBBuilder::new(joinlabel, vec![(self.trans_ty(&ty), name)])
            }
            Tuple { tys, tuple } => {
                let tys = tys.into_iter().map(|ty| self.trans_ty(&ty)).collect();
                let tuple = tuple.into_iter().map(force_symbol).collect();
                eb.tuple(name, tys, tuple);
                eb
            }
            Proj { ty, index, tuple } => {
                let ty = self.trans_ty(&ty);
                let tuple = force_symbol(*tuple);
                eb.proj(name, ty, index, tuple);
                eb
            }
            Closure {
                envs,
                param_ty,
                body_ty,
                mut fname,
            } => {
                let param_ty = self.trans_ty(&param_ty);
                let body_ty = self.trans_ty(&body_ty);
                if envs.is_empty() {
                    let wrapper_name =
                        self.to_make_closure_wrapper(fname, param_ty.clone(), body_ty.clone());
                    fname = wrapper_name;
                }
                let envs = envs
                    .into_iter()
                    .map(|(ty, var)| (self.trans_ty(&ty), var))
                    .collect();
                eb.closure(name, param_ty, body_ty, fname, envs);
                eb
            }
            Constructor {
                ty,
                arg,
                descriminant,
            } => {
                assert_eq!(ty, ty_);
                let ty = match self.trans_ty_canonical(&ty) {
                    EbbTy::Tuple(tys) => tys,
                    ty => unreachable!("{:?}", ty),
                };
                assert_eq!(ty.len(), 2);
                let arg_ty = match &ty[1] {
                    EbbTy::Union(tys) => tys,
                    ty => unreachable!("{:?}", ty),
                };
                let desc_sym = self.gensym("descriminant");
                eb.lit(
                    desc_sym.clone(),
                    EbbTy::Int,
                    Literal::Int(descriminant as i64),
                );
                let arg_sym = self.gensym("arg");
                // FIXME: create union
                match arg {
                    None => {
                        let void_sym = self.gensym("arg");
                        // eb.tuple(void_sym.clone(), vec![], vec![]);
                        eb.lit(void_sym.clone(), EbbTy::Int, Literal::Int(0));
                        eb.union(arg_sym.clone(), arg_ty.clone(), descriminant, void_sym);
                    }
                    Some(arg) => {
                        eb.union(
                            arg_sym.clone(),
                            arg_ty.clone(),
                            descriminant,
                            force_symbol(*arg),
                        );
                    }
                };
                let tuple = vec![desc_sym, arg_sym];
                eb.tuple(name, ty, tuple);
                eb
            }
            Lit { ty, value } => {
                assert_eq!(ty, ty_);
                eb.lit(name, self.trans_ty(&ty), value);
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
            Let { ty, bind, ret } => {
                assert_eq!(ty, ty_);
                let mut funs = Vec::new();
                eb = self.trans_val(&mut funs, fb, eb, *bind);
                assert_eq!(funs.len(), 0);
                self.trans_expr(fb, eb, ty_, *ret)
            }
            Sym { ty, name } => {
                assert_eq!(ty, ty_);
                eb.ret(name, self.trans_ty(&ty))
            }
            e => panic!("internal error: expr should be let or symbol: {:?}", e),
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
            Let { ty, bind, ret } => {
                assert_eq!(ty, ty_);
                let mut funs = Vec::new();
                eb = self.trans_val(&mut funs, fb, eb, *bind);
                assert_eq!(funs.len(), 0);
                self.trans_expr_block(fb, eb, ty_, *ret)
            }
            Sym { ty, name } => {
                assert_eq!(ty, ty_);
                (eb, name)
            }
            e => {
                let tmp = self.gensym("tmp");
                let expr = Let {
                    ty: ty_.clone(),
                    bind: Box::new(hir::Val {
                        ty: ty_.clone(),
                        rec: false,
                        name: tmp.clone(),
                        expr: e,
                    }),
                    ret: Box::new(hir::Expr::Sym {
                        ty: ty_.clone(),
                        name: tmp,
                    }),
                };
                self.trans_expr_block(fb, eb, ty_, expr)
            }
        }
    }
}

fn force_symbol(e: hir::Expr) -> Symbol {
    match e {
        hir::Expr::Sym { name, .. } => name,
        e => panic!("not a symbol, {:?}", e),
    }
}

impl<E> Pass<hir::Context, E> for HIR2MIR {
    type Target = Context;

    fn trans(
        &mut self,
        hir::Context(symbol_table, hir): hir::Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        let mut pass = self.generate_pass(symbol_table);
        let mir = pass.trans_hir(hir);
        let symbol_table = pass.generate_symbol_table();
        Ok(Context(symbol_table, mir))
    }
}
