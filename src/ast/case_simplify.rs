use super::util::Transform;
use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct CaseSimplify {
    id: Id,
    symbol_table: Option<SymbolTable>,
}

#[derive(Debug)]
pub struct WildcardToVariable {
    id: Id,
}

type Stack<T> = Vec<T>;

impl CaseSimplify {
    pub fn new(id: Id) -> Self {
        CaseSimplify {
            symbol_table: None,
            id,
        }
    }
    fn symbol_table(&self) -> &SymbolTable {
        self.symbol_table.as_ref().unwrap()
    }
    fn generate_symbol_table(&mut self) -> SymbolTable {
        self.symbol_table.take().unwrap()
    }

    fn gensym(&mut self, name: &str) -> Symbol {
        let id = self.id.next();
        Symbol(format!("#{}", name), id)
    }

    fn wildcard_to_variable(&mut self, ast: TypedCore) -> TypedCore {
        WildcardToVariable::new(self.id.clone()).transform_ast(ast)
    }

    fn match_compile(
        &mut self,
        cond: Stack<(Type, Symbol)>,
        ty: Type,
        clauses: Vec<(Stack<TypedPattern>, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        // assuming clauses.any(|(patterns, _)| patterns.len() == cond.len())
        if clauses.len() == 0 {
            self.match_compile_empty(cond, ty, clauses)
        } else if clauses[0].0.iter().all(|p| p.is_variable()) {
            self.match_compile_variable(cond, ty, clauses)
        } else if clauses[0].0.iter().any(|p| p.is_tuple()) {
            self.compile_tuple(cond, ty, clauses)
        } else {
            self.match_compile_mixture(cond, ty, clauses)
        }
    }

    fn match_compile_empty(
        &mut self,
        _: Stack<(Type, Symbol)>,
        _: Type,
        _: Vec<(Stack<TypedPattern>, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        panic!("non-exhausitive pattern");
    }

    fn match_compile_variable(
        &mut self,
        cond: Stack<(Type, Symbol)>,
        ty: Type,
        mut clauses: Vec<(Stack<TypedPattern>, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        let (patterns, expr) = clauses.remove(0);
        patterns
            .into_iter()
            .zip(cond.iter().cloned())
            .fold(expr, |acc, (pattern, (cty, name))| Expr::Binds {
                binds: vec![Statement::Val {
                    rec: false,
                    expr: Expr::Symbol { name, ty: cty },
                    // believing pattern is variable
                    pattern,
                }],
                ty: ty.clone(),
                ret: Box::new(acc),
            })
    }

    fn compile_tuple(
        &mut self,
        mut cond: Stack<(Type, Symbol)>,
        ty: Type,
        clauses: Vec<(Stack<TypedPattern>, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        let pos = self.find_tuple(&clauses);

        let (cty, c) = cond.swap_remove(pos);
        let param_tys: Vec<Type> = match cty.clone() {
            Type::Tuple(tuple) => tuple,
            _ => unreachable!(),
        };
        let clauses = clauses
            .into_iter()
            .map(|(mut patterns, mut arm)| {
                let tuple = match patterns.swap_remove(pos) {
                    Pattern::Tuple { tuple, .. } => tuple,
                    var @ Pattern::Variable { .. } => {
                        let pattern = std::iter::repeat_with(|| self.gensym("_"))
                            .zip(param_tys.clone())
                            .map(|(name, ty)| Pattern::Variable { name, ty })
                            .take(param_tys.len())
                            .collect();
                        arm = Expr::Binds {
                            binds: vec![Statement::Val {
                                rec: false,
                                pattern: var,
                                expr: Expr::Symbol {
                                    name: c.clone(),
                                    ty: cty.clone(),
                                },
                            }],
                            ty: arm.ty(),
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
            .take(param_tys.len())
            .collect::<Vec<_>>();
        cond.extend(param_tys.clone().into_iter().zip(tmp_vars.clone()).rev());
        Expr::Case {
            cond: Expr::Symbol {
                name: c,
                ty: cty.clone(),
            }
            .boxed(),
            ty: ty.clone(),
            clauses: vec![(
                Pattern::Tuple {
                    ty: cty,
                    tuple: tmp_vars
                        .into_iter()
                        .zip(param_tys)
                        .map(|(name, ty)| Pattern::Variable { name, ty })
                        .collect(),
                },
                self.match_compile(cond, ty, clauses),
            )],
        }
    }

    fn match_compile_mixture(
        &mut self,
        mut cond: Stack<(Type, Symbol)>,
        ret_ty: Type,
        clauses: Vec<(Stack<TypedPattern>, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        let pos = self.find_constructor(&clauses);

        let (cty, c) = cond.swap_remove(pos);
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.swap_remove(pos);
                (head, clause)
            })
            .collect::<Vec<_>>();
        let type_id = clause_with_heads
            .iter()
            .map(|(head, _)| head.ty())
            .next()
            .unwrap();
        let constructors = clause_with_heads
            .iter()
            .filter_map(|(head, _)| match head {
                Pattern::Constructor { name, ty, arg } => {
                    Some((name.clone(), (ty.clone(), arg.clone())))
                }
                _ => None,
            })
            .collect::<HashMap<_, _>>();
        let constructor_names = constructors.keys().collect::<HashSet<_>>();
        let mut clauses = constructors
            .iter()
            .map(|(name, (ty, arg))| {
                let clauses = self.specialized_patterns(
                    (cty.clone(), c.clone()),
                    &name,
                    &arg,
                    clause_with_heads.iter(),
                );
                let mut new_cond = cond.clone();
                let arg = match arg {
                    Some(arg) => {
                        let argty = arg.ty();
                        let tmp_var = self.gensym("v");
                        new_cond.push((argty.clone(), tmp_var.clone()));
                        Some(Box::new(Pattern::Variable {
                            name: tmp_var,
                            ty: argty,
                        }))
                    }
                    None => None,
                };
                (
                    Pattern::Constructor {
                        name: name.clone(),
                        arg,
                        ty: ty.clone(),
                    },
                    self.match_compile(new_cond, ret_ty.clone(), clauses),
                )
            })
            .collect();

        if self.is_exhausitive(&type_id, constructor_names) {
            Expr::Case {
                cond: Box::new(Expr::Symbol {
                    name: c.clone(),
                    ty: cty,
                }),
                clauses,
                ty: ret_ty,
            }
        } else {
            let default =
                self.default_patterns(c.clone(), cond, ret_ty.clone(), clause_with_heads.iter());
            clauses.push((
                Pattern::Variable {
                    name: self.gensym("_"),
                    ty: cty.clone(),
                },
                default,
            ));
            Expr::Case {
                cond: Box::new(Expr::Symbol { name: c, ty: cty }),
                clauses: clauses,
                ty: ret_ty,
            }
        }
    }

    fn find_tuple(&mut self, clauses: &[(Stack<TypedPattern>, TypedCoreExpr)]) -> usize {
        clauses[0].0.iter().rposition(|p| p.is_tuple()).unwrap()
    }

    fn find_constructor(&mut self, clauses: &[(Stack<TypedPattern>, TypedCoreExpr)]) -> usize {
        clauses[0]
            .0
            .iter()
            .rposition(|p| p.is_constructor())
            .unwrap()
    }

    fn specialized_patterns<'a, 'b>(
        &'a mut self,
        (cty, cond): (Type, Symbol),
        name: &Symbol,
        arg: &Option<Box<TypedPattern>>,
        clause_with_heads: impl Iterator<
            Item = &'b (TypedPattern, (Stack<TypedPattern>, TypedCoreExpr)),
        >,
    ) -> Vec<(Stack<TypedPattern>, TypedCoreExpr)> {
        clause_with_heads
            .filter_map(|(head, clause)| match head {
                Pattern::Constructor {
                    name: name1,
                    arg: arg1,
                    ..
                } if name == name1 => Some((arg1.as_ref().map(|p| *p.clone()), clause.clone())),
                v @ Pattern::Variable { .. } => {
                    let pattern = match arg {
                        Some(arg) => Some(Pattern::Variable {
                            name: self.gensym("_"),
                            ty: arg.ty(),
                        }),
                        None => None,
                    };
                    let (pat, arm) = clause.clone();
                    let arm = Expr::Binds {
                        binds: vec![Statement::Val {
                            rec: false,
                            pattern: v.clone(),
                            expr: Expr::Symbol {
                                name: cond.clone(),
                                ty: cty.clone(),
                            },
                        }],
                        ty: arm.ty(),
                        ret: Box::new(arm),
                    };
                    Some((pattern, (pat, arm)))
                }
                _ => None,
            })
            .map(|(patterns, (mut pat, arm))| {
                pat.extend(patterns.into_iter());
                (pat, arm)
            })
            .collect()
    }

    fn default_patterns<'a, 'b>(
        &'a mut self,
        c: Symbol,
        cond: Stack<(Type, Symbol)>,
        ty: Type,
        clause_with_heads: impl Iterator<
            Item = &'b (TypedPattern, (Stack<TypedPattern>, TypedCoreExpr)),
        >,
    ) -> TypedCoreExpr {
        let clauses = clause_with_heads
            .cloned()
            .filter_map(|(p, (pat, arm))| match p {
                var @ Pattern::Variable { .. } => {
                    let arm = Expr::Binds {
                        binds: vec![Statement::Val {
                            rec: false,
                            expr: Expr::Symbol {
                                name: c.clone(),
                                ty: var.ty(),
                            },
                            pattern: var,
                        }],
                        ty: arm.ty(),
                        ret: Box::new(arm),
                    };
                    Some((pat, arm))
                }
                _ => None,
            })
            .collect();
        self.match_compile(cond, ty, clauses)
    }

    fn is_exhausitive<'a, 'b>(
        &self,
        ty: &'a Type,
        descriminansts: impl IntoIterator<Item = &'b Symbol>,
    ) -> bool {
        use Type::*;
        match ty {
            Real | Variable(_) | Fun(_, _) => panic!("no way to pattern match against this type"),
            Int => false,
            Tuple(_) => {
                // unlikely reachable, but writing incase it reaches.
                true
            }
            Datatype(name) => {
                self.symbol_table()
                    .get_type(name)
                    .unwrap()
                    .constructors
                    .iter()
                    .map(|(name, _)| name)
                    .collect::<HashSet<_>>()
                    == descriminansts.into_iter().collect::<HashSet<_>>()
            }
        }
    }
}

impl Transform<Type> for CaseSimplify {
    fn transform_case(
        &mut self,
        ty: Type,
        cond: Box<TypedCoreExpr>,
        clauses: Vec<(TypedPattern, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        let condsym = self.gensym("cond");
        let condty = cond.ty();
        let clauses = clauses
            .into_iter()
            .map(|(pat, arm)| (vec![pat], self.transform_expr(arm)))
            .collect();
        Expr::Binds {
            ty: ty.clone(),
            binds: vec![Statement::Val {
                pattern: Pattern::Variable {
                    name: condsym.clone(),
                    ty: condty.clone(),
                },
                rec: false,
                expr: *cond,
            }],
            ret: self
                .match_compile(vec![(condty, condsym)], ty, clauses)
                .boxed(),
        }
    }
}

impl WildcardToVariable {
    fn new(id: Id) -> Self {
        Self { id }
    }

    fn gensym(&mut self, name: &str) -> Symbol {
        let id = self.id.next();
        Symbol(format!("#{}", name), id)
    }
}

impl Transform<Type> for WildcardToVariable {
    fn transform_pat_wildcard(&mut self, ty: Type) -> TypedPattern {
        Pattern::Variable {
            ty,
            name: self.gensym("_"),
        }
    }
}

use crate::pass::Pass;
impl<'a> Pass<(SymbolTable, TypedCore), TypeError<'a>> for CaseSimplify {
    type Target = (SymbolTable, TypedCore);

    fn trans<'b>(
        &'b mut self,
        (symbol_table, ast): (SymbolTable, TypedCore),
        _: &Config,
    ) -> Result<'a, Self::Target> {
        self.symbol_table = Some(symbol_table);
        let ast = self.wildcard_to_variable(ast);
        let ast = self.transform_ast(ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
