use crate::config::Config;
use crate::hir::util::Transform;
use crate::hir::*;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::{HashMap, HashSet};

pub struct Simplify {
    id: Id,
    aliases: HashMap<Symbol, Symbol>,
    // TODO: do not copy expr
    normals: HashMap<Symbol, Expr>,
    used_variables: HashSet<Symbol>,
}

impl Simplify {
    pub fn new(id: Id) -> Self {
        Self {
            id,
            aliases: HashMap::new(),
            normals: HashMap::new(),
            used_variables: HashSet::new(),
        }
    }

    pub fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#g".into(), id)
    }

    fn clear(&mut self) {
        self.aliases.clear();
        self.normals.clear();
        self.used_variables.clear();
    }
}

use crate::hir::Expr::*;

impl Transform for Simplify {
    fn transform_binds(&mut self, ty: HTy, bind: Box<Val>, ret: Box<Expr>) -> Expr {
        let mut aliased = vec![];
        let mut new_bind = None;
        match bind.expr {
            Sym { name: sym_name, .. } => {
                let sym_name = match self.aliases.get(&sym_name) {
                    Some(alias) => alias.clone(),
                    None => sym_name,
                };
                aliased.push(bind.name.clone());
                self.aliases.insert(bind.name, sym_name);
            }
            _ => new_bind = Some(*bind),
        }

        let expr = if let Some(new_bind) = new_bind {
            let bind = Box::new(self.transform_val(new_bind));
            if bind.expr.is_normal() {
                self.normals.insert(bind.name.clone(), bind.expr.clone());
            }
            let ret = self.transform_expr(*ret);
            let bind_is_removable =
                bind.expr.is_normal() && !self.used_variables.contains(&bind.name);
            self.normals.remove(&bind.name);
            self.used_variables.remove(&bind.name);
            if bind_is_removable {
                ret
            } else {
                Let {
                    ty,
                    bind,
                    ret: Box::new(ret),
                }
            }
        } else {
            self.transform_expr(*ret)
        };
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
        for (_, name) in &envs {
            self.used_variables.insert(name.clone());
        }
        self.used_variables.insert(fname.clone());

        Expr::Closure {
            envs,
            param_ty,
            body_ty,
            fname,
        }
    }

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        let arg = self.transform_expr(*arg);
        match self.transform_expr(*fun) {
            Fun {
                param,
                body_ty,
                body,
                captures,
            } => {
                assert!(captures.is_empty());
                let (param_ty, param) = param;
                let ret = Let {
                    ty: body_ty,
                    bind: Box::new(Val {
                        ty: param_ty,
                        rec: false,
                        name: param,
                        expr: arg,
                    }),
                    ret: body,
                };
                self.transform_expr(ret)
            }
            fun => App {
                ty,
                fun: Box::new(fun),
                arg: Box::new(arg),
            },
        }
    }

    fn transform_case(&mut self, ty: HTy, cond: Box<Expr>, mut arms: Vec<(Pattern, Expr)>) -> Expr {
        let cond = self.transform_expr(*cond);
        if arms.len() == 1 && arms[0].0.is_irrefutable() {
            let (pat, arm) = arms.remove(0);
            let binds = match pat {
                Pattern::Var { name, ty } => vec![Val {
                    ty,
                    name,
                    rec: false,
                    expr: cond,
                }],
                Pattern::Tuple { tys, tuple } => {
                    let tmp = self.gensym();
                    let tmp_ty = cond.ty();
                    let mut binds = vec![Val {
                        ty: cond.ty(),
                        name: tmp.clone(),
                        rec: false,
                        expr: cond,
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

            let ret = binds.into_iter().rev().fold(arm, |ret, bind| Let {
                ty: ty.clone(),
                bind: Box::new(bind),
                ret: Box::new(ret),
            });

            self.transform_expr(ret)
        } else {
            Expr::Case {
                ty,
                expr: Box::new(cond),
                arms: arms
                    .into_iter()
                    .map(|(pat, expr)|
                         // FIXME: pass `pat` to transformer
                         (pat, self.transform_expr(expr)))
                    .collect(),
            }
        }
    }

    fn transform_proj(&mut self, ty: HTy, index: u32, tuple: Box<Expr>) -> Expr {
        match self.transform_expr(*tuple) {
            Expr::Tuple { tys, mut tuple } => {
                let tuple_var = self.gensym();
                let tuple_ty = HTy::Tuple(tys.clone());
                let nth_var = self.gensym();
                let nth_ty = tys[index as usize].clone();
                let nth_expr = std::mem::replace(
                    &mut tuple[index as usize],
                    Expr::Sym {
                        ty: nth_ty.clone(),
                        name: nth_var.clone(),
                    },
                );
                Expr::Let {
                    ty: ty.clone(),
                    bind: Box::new(Val {
                        ty: nth_ty,
                        rec: false,
                        name: nth_var,
                        expr: nth_expr,
                    }),
                    ret: Box::new(Expr::Let {
                        ty: ty.clone(),
                        bind: Box::new(Val {
                            ty: tuple_ty.clone(),
                            rec: false,
                            name: tuple_var.clone(),
                            expr: Expr::Tuple { tys, tuple },
                        }),
                        ret: Box::new(Expr::Proj {
                            ty,
                            index,
                            tuple: Box::new(Expr::Sym {
                                ty: tuple_ty,
                                name: tuple_var,
                            }),
                        }),
                    }),
                }
            }
            Expr::Sym { ty: tuple_ty, name } if self.normals.contains_key(&name) => {
                match &self.normals[&name] {
                    Expr::Tuple { tuple, .. } => tuple[index as usize].clone(),
                    _ => Expr::Proj {
                        ty,
                        index,
                        tuple: Box::new(Expr::Sym { ty: tuple_ty, name }),
                    },
                }
            }
            tuple => Expr::Proj {
                ty,
                index,
                tuple: Box::new(tuple),
            },
        }
    }

    fn transform_sym(&mut self, ty: HTy, name: Symbol) -> Expr {
        match self.aliases.get(&name) {
            Some(alias) => {
                self.used_variables.insert(alias.clone());
                Sym {
                    ty,
                    name: alias.clone(),
                }
            }
            None => {
                self.used_variables.insert(name.clone());
                Sym { ty, name }
            }
        }
    }
}

impl<E> Pass<Context, E> for Simplify {
    type Target = Context;

    fn trans(
        &mut self,
        Context(symbol_table, hir): Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        let fst = self.transform_hir(hir);
        self.clear();
        let snd = self.transform_hir(fst);
        Ok(Context(symbol_table, snd))
    }
}
