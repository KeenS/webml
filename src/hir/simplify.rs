use crate::config::Config;
use crate::hir::util::Transform;
use crate::hir::*;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashMap;

pub struct Simplify {
    id: Id,
    aliases: HashMap<Symbol, Symbol>,
}

impl Simplify {
    pub fn new(id: Id) -> Self {
        Self {
            id,
            aliases: HashMap::new(),
        }
    }

    pub fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#g".into(), id)
    }
}

use crate::hir::Expr::*;

impl Transform for Simplify {
    fn transform_binds(&mut self, ty: HTy, binds: Vec<Val>, ret: Box<Expr>) -> Expr {
        let mut aliased = vec![];
        let mut new_binds = vec![];
        for Val {
            ty,
            rec,
            name,
            expr,
        } in binds
        {
            match expr {
                Sym { name: sym_name, .. } => {
                    let sym_name = match self.aliases.get(&sym_name) {
                        Some(alias) => alias.clone(),
                        None => sym_name,
                    };
                    aliased.push(name.clone());
                    self.aliases.insert(name, sym_name);
                }
                _ => new_binds.push(Val {
                    ty,
                    rec,
                    name,
                    expr,
                }),
            }
        }

        let expr;
        if new_binds.is_empty() {
            expr = self.transform_expr(*ret)
        } else {
            expr = Binds {
                ty,
                binds: new_binds
                    .into_iter()
                    .map(|val| self.transform_val(val))
                    .collect(),
                ret: Box::new(self.transform_expr(*ret)),
            }
        }
        for name in aliased {
            self.aliases.remove(&name);
        }

        expr
    }

    fn transform_closure(
        &mut self,
        envs: Vec<(HTy, Symbol)>,
        param_ty: HTy,
        body_ty: HTy,
        fname: Symbol,
    ) -> Expr {
        let fname = match self.aliases.get(&fname) {
            Some(alias) => alias.clone(),
            None => fname,
        };
        Expr::Closure {
            envs,
            param_ty,
            body_ty,
            fname,
        }
    }

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        match *fun {
            Fun {
                param,
                body_ty,
                body,
                captures,
            } => {
                assert!(captures.is_empty());
                let body = Box::new(self.transform_expr(*body));
                let (param_ty, param) = param;
                let ret = Binds {
                    ty: body_ty,
                    binds: vec![Val {
                        ty: param_ty,
                        rec: false,
                        name: param,
                        expr: *arg,
                    }],
                    ret: body,
                };
                self.transform_expr(ret)
            }
            fun => {
                let fun = Box::new(fun);
                App {
                    ty,
                    fun: Box::new(self.transform_expr(*fun)),
                    arg: Box::new(self.transform_expr(*arg)),
                }
            }
        }
    }

    fn transform_case(&mut self, ty: HTy, cond: Box<Expr>, mut arms: Vec<(Pattern, Expr)>) -> Expr {
        if arms.len() == 1 && arms[0].0.is_irrefutable() {
            let (pat, arm) = arms.remove(0);
            let binds = match pat {
                Pattern::Var { name, ty } => vec![Val {
                    ty,
                    name,
                    rec: false,
                    expr: *cond,
                }],
                Pattern::Tuple { tys, tuple } => {
                    let tmp = self.gensym();
                    let tmp_ty = cond.ty();
                    let mut binds = vec![Val {
                        ty: cond.ty(),
                        name: tmp.clone(),
                        rec: false,
                        expr: *cond,
                    }];
                    for (i, (ty, name)) in tys.into_iter().zip(tuple).enumerate() {
                        binds.push(Val {
                            ty: ty.clone(),
                            name,
                            rec: false,
                            expr: Proj {
                                ty,
                                index: i as u32,
                                tuple: Box::new(Sym {
                                    ty: tmp_ty.clone(),
                                    name: tmp.clone(),
                                }),
                            },
                        })
                    }
                    binds
                }
                pat => unreachable!("expected irrefutable pattern, but got {:?}", pat),
            };
            let arm = self.transform_expr(arm);
            let ret = Binds {
                ty,
                binds,
                ret: Box::new(arm),
            };
            self.transform_expr(ret)
        } else {
            Expr::Case {
                ty,
                expr: Box::new(self.transform_expr(*cond)),
                arms: arms
                    .into_iter()
                    .map(|(pat, expr)|
                         // FIXME: pass `pat` to transformer
                         (pat, self.transform_expr(expr)))
                    .collect(),
            }
        }
    }

    fn transform_sym(&mut self, ty: HTy, name: Symbol) -> Expr {
        match self.aliases.get(&name) {
            Some(alias) => Sym {
                ty,
                name: alias.clone(),
            },
            None => Sym { ty, name },
        }
    }
}

impl<E> Pass<(SymbolTable, HIR), E> for Simplify {
    type Target = (SymbolTable, HIR);

    fn trans(
        &mut self,
        (symbol_table, hir): (SymbolTable, HIR),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        Ok((symbol_table, self.transform_hir(hir)))
    }
}
