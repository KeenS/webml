use crate::config::Config;
use crate::hir::*;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::{HashMap, HashSet};

pub struct FlatPattern {
    id: Id,
}

type Stack<T> = Vec<T>;

impl FlatPattern {
    pub fn new(id: Id) -> Self {
        Self { id }
    }

    fn gensym(&mut self, name: &str) -> Symbol {
        let name = format!("{}", name);
        let id = self.id.next();
        Symbol(name, id)
    }
}

impl FlatPattern {
    fn simplify_pattern(&self, ty: HTy, cond: (Symbol, HTy), arms: Vec<(Pattern, Expr)>) -> Expr {
        self.simplify(
            ty,
            vec![cond],
            arms.into_iter().map(|(pat, t)| (vec![pat], t)).collect(),
        )
    }

    fn simplify(
        &self,
        ty: HTy,
        cond: Stack<(Symbol, HTy)>,
        clauses: Vec<(Stack<Pattern>, Expr)>,
    ) -> Expr {
        if clauses.len() == 0 {
            self.simplify_empty(ty, cond, clauses)
        } else if clauses[0].0.iter().all(|p| p.is_variable()) {
            self.simplify_variable(ty, cond, clauses)
        } else if clauses[0].0.iter().all(|p| p.is_tuple()) {
            self.simplify_tuple(ty, cond, clauses)
        } else {
            self.simplify_mixture(ty, cond, clauses)
        }
    }

    fn simplify_empty(
        &self,
        ty: HTy,
        cond: Vec<(Symbol, HTy)>,
        clauses: Vec<(Vec<Pattern>, Expr)>,
    ) -> Expr {
        panic!("non-exhausitive pattern");
    }

    fn simplify_variable(
        &mut self,
        ty: HTy,
        cond: Vec<(Symbol, HTy)>,
        mut clauses: Vec<(Stack<Pattern>, Expr)>,
    ) -> Expr {
        let (patterns, ret) = clauses.remove(0);
        let binds = patterns
            .into_iter()
            .map(|p| p.variable())
            .zip(cond.iter().cloned())
            .map(|(name, (var, ty))| Val {
                ty: ty.clone(),
                name,
                expr: Expr::Sym { name: var, ty: ty },
            })
            .collect();
        Expr::Binds { binds, ret }
    }

    fn simplify_tuple(
        &mut self,
        ty: HTy,
        mut cond: Stack<(Symbol, HTy)>,
        clauses: Vec<(Stack<Pattern>, Expr)>,
    ) -> Expr {
        let pos = self.find_tuple(&clauses);
        let (sym, ty) = cond.swap_remove(pos);
        let param_tys: Vec<HTy> = ty.take_tuple();
        let clauses = clauses
            .into_iter()
            .map(|(mut patterns, mut arm)| {
                let tuple = match patterns.swap_remove(pos) {
                    Pattern::Tuple { tuple, .. } => tuple,
                    Pattern::Var { name, ty } => {
                        let pattern = std::iter::repeat_with(|| self.gensym("_"))
                            .zip(param_tys)
                            .map(|(name, ty)| Pattern::Var { name, ty })
                            .collect();
                        arm = Expr::Binds {
                            ty: arm.ty(),
                            binds: vec![Val {
                                name,
                                ty: ty.clone(),
                                expr: Expr::Sym {
                                    name: sym.clone(),
                                    ty,
                                },
                            }],
                            ret: Box::new(arm),
                        };
                        pattern
                    }
                    _ => unreachable!(),
                };
                patterns.extend(tuple.into_iter().rev());
                (patterns, arm)
            })
            .collect();

        let tmp_vars = std::iter::repeat_with(|| self.gensym("v"))
            .zip(param_tys)
            .collect::<Vec<_>>();
        cond.extend(tmp_vars.into_iter().rev());
        self.simplify(ty, cond, clauses)
    }
    fn simplify_mixture(
        &mut self,
        ty: HTy,
        mut cond: Stack<(Symbol, HTy)>,
        clauses: Vec<(Stack<Pattern>, Expr)>,
    ) -> Expr {
        let pos = self.find_constructor(&clauses);

        let (sym, ty) = cond.swap_remove(pos);
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.swap_remove(pos);
                (head, clause)
            })
            .collect::<Vec<_>>();
        let literals = clause_with_heads
            .iter()
            .filter_map(|(head, _)| match head {
                Pattern::Lit { value, ty } => Some((value, ty)),
                _ => None,
            })
            .collect::<HashSet<_>>();
        let mut clauses = literals
            .iter()
            .map(|&(value, ty)| {
                let clauses =
                    self.specialized_patterns(sym.clone(), &ty, value, clause_with_heads.iter());
                // let param_ty = self
                //     .type_db
                //     .find(&ty)
                //     .cloned()
                //     .unwrap()
                //     .adt()
                //     .into_iter()
                //     .find(|c| c.descriminant == descriminant)
                //     .map(|c| c.param)
                //     .unwrap();
                let param_ty = vec![];
                let tmp_var = param_ty.iter().cloned().map(|_| self.gensym("v"));
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_var.iter().cloned().zip(param_ty).rev());
                (
                    Pattern::Lit { value, ty },
                    self.simplify(ty, new_cond, clauses),
                )
            })
            .collect();

        let literals = literals.iter().map(|(lit, _)| lit.clone()).collect();
        if self.is_exhausitive(&ty, literals) {
            Expr::Case {
                ty: hoge,
                expr: Box::new(Expr::Symbol {
                    sym: sym.clone(),
                    ty: hoge,
                }),
                arms: clauses,
            }
        } else {
            let default = self.default_patterns(ty, sym.clone(), cond, clause_with_heads.iter());
            clauses.push((
                Pattern::Var {
                    name: self.gensym("_"),
                    ty: hoge,
                },
                default,
            ));
            Expr::Case {
                ty: hoge,
                expr: Box::new(Expr::Symbol {
                    sym: sym.clone(),
                    ty: hoge,
                }),
                arms: clauses,
            }
        }
    }

    fn find_tuple(&mut self, clauses: &[(Stack<Pattern>, Expr)]) -> usize {
        clauses[0].0.iter().rposition(|p| p.is_tuple()).unwrap()
    }

    fn find_constructor(&mut self, clauses: &[(Stack<Pattern>, Expr)]) -> usize {
        clauses[0]
            .0
            .iter()
            .rposition(|p| p.is_constructor())
            .unwrap()
    }

    fn specialized_patterns<'a, 'b>(
        &'a mut self,
        cond: Symbol,
        type_id: &HTy,
        descriminant: Literal,
        clause_with_heads: impl Iterator<Item = &'b (Pattern, (Stack<Pattern>, Expr))>,
    ) -> Vec<(Stack<Pattern>, Expr)> {
        // let param_ty = self
        //     .type_db
        //     .find(&type_id)
        //     .cloned()
        //     .unwrap()
        //     .adt()
        //     .into_iter()
        //     .find(|c| c.descriminant == descriminant)
        //     .map(|c| c.param)
        //     .unwrap();
        // currently, Int | Bool only
        let param_ty = vec![];
        clause_with_heads
            .filter_map(|(head, clause)| match head {
                Pattern::Lit { value, ty } if *d == descriminant => {
                    // currently, Int | Bool only, so inner pattern is always None
                    Some((None, clause.clone()))
                }
                Pattern::Var { name, ty } => {
                    let pattern = param_ty.as_ref().map(|ty| Pattern::Var {
                        name: self.gensym("_"),
                        ty,
                    });

                    let (pat, arm) = clause.clone();
                    let arm = Expr::Binds {
                        binds: vec![Val {
                            ty: hoge,
                            rec: false,
                            name,
                            expr: Expr::Sym {
                                name: cond.clone(),
                                ty: hoge,
                            },
                        }],
                        ret: Box::new(arm),
                    };
                    Some((pattern, (pat, arm)))
                }
                _ => None,
            })
            .map(|(patterns, (mut pat, arm))| {
                pat.extend(patterns.into_iter().rev());
                (pat, arm)
            })
            .collect()
    }

    fn default_patterns<'a, 'b>(
        &'a mut self,
        ty: HTy,
        sym: Symbol,
        cond: Stack<(Symbol, HTy)>,
        clause_with_heads: impl Iterator<Item = &'b (Pattern, (Stack<Pattern>, Expr))>,
    ) -> Expr {
        let clauses = clause_with_heads
            .filter(|(head, _)| head.is_variable())
            .cloned()
            .map(|(p, (pat, arm))| {
                let (ty, var) = p.variable();
                let arm = Expr::Binds {
                    binds: vec![Val {
                        ty: ty.clone(),
                        name: var,
                        expr: Expr::Sym {
                            name: sym.clone(),
                            ty,
                        },
                        rec: false,
                    }],
                    ty: arm.ty(),
                    ret: Box::new(arm),
                };
                (pat, arm)
            })
            .collect();
        self.simplify(ty, cond, clauses)
    }

    fn is_exhausitive(&self, ty: &HTy, descriminansts: impl IntoIterator<Item = Literal>) -> bool {
        match ty {
            HTy::Int | HTy::Float => false,
            HTy::Bool => {
                descriminansts.into_iter().collect::<HashSet<_>>()
                    == vec![Literal::Bool(true), Literal::Bool(true)]
                        .into_iter()
                        .collect::<HashSet<_>>()
            }
            _ => panic!(
                "internal error: checking exhausitiveness of non ADT types. it must not occure"
            ),
        }

        // self.type_db
        //     .find(&type_id)
        //     .cloned()
        //     .unwrap()
        //     .adt()
        //     .into_iter()
        //     .map(|c| c.descriminant)
        //     .collect::<HashSet<_>>()
        //     == descriminansts.into_iter().collect::<HashSet<_>>()
    }
}
