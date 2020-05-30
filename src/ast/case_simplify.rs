use super::util::Transform;
use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct CaseSimplify {
    id: Id,
}

#[derive(Debug)]
pub struct CaseSimplifyPass {
    symbol_table: SymbolTable,
    id: Id,
}

#[derive(Debug)]
pub struct WildcardToVariable {
    id: Id,
}

type Stack<T> = Vec<T>;

impl CaseSimplify {
    pub fn new(id: Id) -> Self {
        Self { id }
    }

    fn generate_pass(&mut self, symbol_table: SymbolTable) -> CaseSimplifyPass {
        CaseSimplifyPass::new(symbol_table, self.id.clone())
    }
}

impl CaseSimplifyPass {
    fn new(symbol_table: SymbolTable, id: Id) -> Self {
        Self { symbol_table, id }
    }
    fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }
    fn into_inner(self) -> (SymbolTable, Id) {
        (self.symbol_table, self.id)
    }

    fn gensym(&mut self, name: &str) -> Symbol {
        let id = self.id.next();
        Symbol(format!("#{}", name), id)
    }

    fn wildcard_to_variable(&mut self, ast: TypedCore) -> TypedCore {
        WildcardToVariable::new(self.id.clone()).transform_ast(ast)
    }

    fn rename_pattern(&mut self, pattern: &mut TypedPattern) {
        use PatternKind::*;
        match &mut pattern.inner {
            Constructor { arg, .. } => {
                if let Some(arg) = arg {
                    self.rename_pattern(arg)
                }
            }
            Tuple { tuple, .. } => {
                for pat in tuple {
                    self.rename_pattern(pat)
                }
            }
            Variable { name, .. } => name.1 = self.id.next(),
            _ => (),
        }
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
            self.match_compile_tuple(cond, ty, clauses)
        } else if clauses[0].0.iter().any(|p| p.is_constant()) {
            self.match_compile_constant(cond, ty, clauses)
        } else if clauses[0].0.iter().any(|p| p.is_char()) {
            self.match_compile_char(cond, ty, clauses)
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
            .fold(expr, |acc, (pattern, (cty, name))| Expr {
                ty: ty.clone(),
                inner: ExprKind::Binds {
                    binds: vec![Declaration::Val {
                        rec: false,
                        expr: Expr {
                            ty: cty,
                            inner: ExprKind::Symbol { name },
                        },
                        // believing pattern is variable
                        pattern,
                    }],
                    ret: acc.boxed(),
                },
            })
    }

    fn match_compile_tuple(
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
                let removed_pattern = patterns.swap_remove(pos);
                let tuple = match removed_pattern.inner {
                    PatternKind::Tuple { tuple, .. } => tuple,
                    var @ PatternKind::Variable { .. } => {
                        let pattern = std::iter::repeat_with(|| self.gensym("_"))
                            .zip(param_tys.clone())
                            .map(|(name, ty)| Pattern {
                                ty,
                                inner: PatternKind::Variable { name },
                            })
                            .take(param_tys.len())
                            .collect();
                        arm = Expr {
                            ty: arm.ty(),
                            inner: ExprKind::Binds {
                                binds: vec![Declaration::Val {
                                    rec: false,
                                    pattern: Pattern {
                                        ty: removed_pattern.ty,
                                        inner: var,
                                    },
                                    expr: Expr {
                                        ty: cty.clone(),
                                        inner: ExprKind::Symbol { name: c.clone() },
                                    },
                                }],
                                ret: arm.boxed(),
                            },
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
        Expr {
            ty: ty.clone(),
            inner: ExprKind::Case {
                cond: Expr {
                    ty: cty.clone(),
                    inner: ExprKind::Symbol { name: c },
                }
                .boxed(),
                clauses: vec![(
                    Pattern {
                        ty: cty,
                        inner: PatternKind::Tuple {
                            tuple: tmp_vars
                                .into_iter()
                                .zip(param_tys)
                                .map(|(name, ty)| Pattern {
                                    ty,
                                    inner: PatternKind::Variable { name },
                                })
                                .collect(),
                        },
                    },
                    self.match_compile(cond, ty, clauses),
                )],
            },
        }
    }

    fn match_compile_constant(
        &mut self,
        mut cond: Stack<(Type, Symbol)>,
        ret_ty: Type,
        clauses: Vec<(Stack<TypedPattern>, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        let pos = self.find_constant(&clauses);

        let (cty, c) = cond.swap_remove(pos);
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.swap_remove(pos);
                (head, clause)
            })
            .collect::<Vec<_>>();
        let constants = clause_with_heads
            .iter()
            .filter_map(|(head, _)| match head {
                Pattern {
                    ty,
                    inner: PatternKind::Constant { value },
                } => Some((*value, ty.clone())),
                _ => None,
            })
            .collect::<HashMap<_, _>>();
        let mut clauses = constants
            .iter()
            .map(|(value, ty)| {
                let clauses = self.specialized_patterns_for_constant(
                    (cty.clone(), c.clone()),
                    *value,
                    clause_with_heads.iter(),
                );
                (
                    Pattern {
                        ty: ty.clone(),
                        inner: PatternKind::Constant { value: *value },
                    },
                    self.match_compile(cond.clone(), ret_ty.clone(), clauses),
                )
            })
            .collect::<Vec<_>>();

        // no check for exhausitiveness
        let default =
            self.default_patterns(c.clone(), cond, ret_ty.clone(), clause_with_heads.iter());
        clauses.push((
            Pattern {
                ty: cty.clone(),
                inner: PatternKind::Variable {
                    name: self.gensym("_"),
                },
            },
            default,
        ));
        Expr {
            ty: ret_ty,
            inner: ExprKind::Case {
                cond: Expr {
                    ty: cty,
                    inner: ExprKind::Symbol { name: c },
                }
                .boxed(),
                clauses: clauses,
            },
        }
    }

    fn match_compile_char(
        &mut self,
        mut cond: Stack<(Type, Symbol)>,
        ret_ty: Type,
        clauses: Vec<(Stack<TypedPattern>, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        let pos = self.find_char(&clauses);

        let (cty, c) = cond.swap_remove(pos);
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.swap_remove(pos);
                (head, clause)
            })
            .collect::<Vec<_>>();
        let chars = clause_with_heads
            .iter()
            .filter_map(|(head, _)| match head {
                Pattern {
                    ty,
                    inner: PatternKind::Char { value },
                } => Some((*value, ty.clone())),
                _ => None,
            })
            .collect::<HashMap<_, _>>();
        let mut clauses = chars
            .iter()
            .map(|(value, ty)| {
                let clauses = self.specialized_patterns_for_char(
                    (cty.clone(), c.clone()),
                    *value,
                    clause_with_heads.iter(),
                );
                (
                    Pattern {
                        ty: ty.clone(),
                        inner: PatternKind::Char { value: *value },
                    },
                    self.match_compile(cond.clone(), ret_ty.clone(), clauses),
                )
            })
            .collect::<Vec<_>>();

        // no check for exhausitiveness
        let default =
            self.default_patterns(c.clone(), cond, ret_ty.clone(), clause_with_heads.iter());
        clauses.push((
            Pattern {
                ty: cty.clone(),
                inner: PatternKind::Variable {
                    name: self.gensym("_"),
                },
            },
            default,
        ));
        Expr {
            ty: ret_ty,
            inner: ExprKind::Case {
                cond: Expr {
                    ty: cty,
                    inner: ExprKind::Symbol { name: c },
                }
                .boxed(),
                clauses: clauses,
            },
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
                Pattern {
                    ty,
                    inner: PatternKind::Constructor { name, arg },
                } => Some((name.clone(), (ty.clone(), arg.clone()))),
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
                        Some(Box::new(Pattern {
                            ty: argty,
                            inner: PatternKind::Variable { name: tmp_var },
                        }))
                    }
                    None => None,
                };
                (
                    Pattern {
                        ty: ty.clone(),
                        inner: PatternKind::Constructor {
                            name: name.clone(),
                            arg,
                        },
                    },
                    self.match_compile(new_cond, ret_ty.clone(), clauses),
                )
            })
            .collect();

        if self.is_exhausitive(&type_id, constructor_names) {
            Expr {
                ty: ret_ty,
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: cty,
                        inner: ExprKind::Symbol { name: c.clone() },
                    }
                    .boxed(),
                    clauses,
                },
            }
        } else {
            let default =
                self.default_patterns(c.clone(), cond, ret_ty.clone(), clause_with_heads.iter());
            clauses.push((
                Pattern {
                    ty: cty.clone(),
                    inner: PatternKind::Variable {
                        name: self.gensym("_"),
                    },
                },
                default,
            ));
            Expr {
                ty: ret_ty,
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: cty,
                        inner: ExprKind::Symbol { name: c },
                    }
                    .boxed(),
                    clauses: clauses,
                },
            }
        }
    }

    fn find_tuple(&mut self, clauses: &[(Stack<TypedPattern>, TypedCoreExpr)]) -> usize {
        clauses[0].0.iter().rposition(|p| p.is_tuple()).unwrap()
    }

    fn find_constant(&mut self, clauses: &[(Stack<TypedPattern>, TypedCoreExpr)]) -> usize {
        clauses[0].0.iter().rposition(|p| p.is_constant()).unwrap()
    }

    fn find_char(&mut self, clauses: &[(Stack<TypedPattern>, TypedCoreExpr)]) -> usize {
        clauses[0].0.iter().rposition(|p| p.is_char()).unwrap()
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
            .filter_map(|(head, clause)| match &head.inner {
                PatternKind::Constructor {
                    name: name1,
                    arg: arg1,
                } if name == name1 => Some((arg1.as_ref().map(|p| *p.clone()), clause.clone())),
                v @ PatternKind::Variable { .. } => {
                    let pattern = match arg {
                        Some(arg) => Some(Pattern {
                            ty: arg.ty(),
                            inner: PatternKind::Variable {
                                name: self.gensym("_"),
                            },
                        }),
                        None => None,
                    };
                    let (pat, arm) = clause.clone();
                    let arm = Expr {
                        ty: arm.ty(),
                        inner: ExprKind::Binds {
                            binds: vec![Declaration::Val {
                                rec: false,
                                pattern: Pattern {
                                    ty: head.ty.clone(),
                                    inner: v.clone(),
                                },
                                expr: Expr {
                                    ty: cty.clone(),
                                    inner: ExprKind::Symbol { name: cond.clone() },
                                },
                            }],
                            ret: arm.boxed(),
                        },
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

    fn specialized_patterns_for_constant<'a, 'b>(
        &'a mut self,
        (cty, cond): (Type, Symbol),
        value: i64,
        clause_with_heads: impl Iterator<
            Item = &'b (TypedPattern, (Stack<TypedPattern>, TypedCoreExpr)),
        >,
    ) -> Vec<(Stack<TypedPattern>, TypedCoreExpr)> {
        clause_with_heads
            .filter_map(|(head, clause)| match &head.inner {
                PatternKind::Constant { value: value1, .. } if value == *value1 => {
                    Some(clause.clone())
                }
                v @ PatternKind::Variable { .. } => {
                    let (pat, arm) = clause.clone();
                    let arm = Expr {
                        ty: arm.ty(),
                        inner: ExprKind::Binds {
                            binds: vec![Declaration::Val {
                                rec: false,
                                pattern: Pattern {
                                    ty: head.ty.clone(),
                                    inner: v.clone(),
                                },
                                expr: Expr {
                                    ty: cty.clone(),
                                    inner: ExprKind::Symbol { name: cond.clone() },
                                },
                            }],
                            ret: arm.boxed(),
                        },
                    };
                    Some((pat, arm))
                }
                _ => None,
            })
            .collect()
    }

    fn specialized_patterns_for_char<'a, 'b>(
        &'a mut self,
        (cty, cond): (Type, Symbol),
        value: u32,
        clause_with_heads: impl Iterator<
            Item = &'b (TypedPattern, (Stack<TypedPattern>, TypedCoreExpr)),
        >,
    ) -> Vec<(Stack<TypedPattern>, TypedCoreExpr)> {
        clause_with_heads
            .filter_map(|(head, clause)| match &head.inner {
                PatternKind::Char { value: value1, .. } if value == *value1 => Some(clause.clone()),
                v @ PatternKind::Variable { .. } => {
                    let (pat, arm) = clause.clone();
                    let arm = Expr {
                        ty: arm.ty(),
                        inner: ExprKind::Binds {
                            binds: vec![Declaration::Val {
                                rec: false,
                                pattern: Pattern {
                                    ty: head.ty.clone(),
                                    inner: v.clone(),
                                },
                                expr: Expr {
                                    ty: cty.clone(),
                                    inner: ExprKind::Symbol { name: cond.clone() },
                                },
                            }],
                            ret: arm.boxed(),
                        },
                    };
                    Some((pat, arm))
                }
                _ => None,
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
            .filter_map(|(p, (pat, arm))| match &p.inner {
                PatternKind::Variable { .. } => {
                    let arm = Expr {
                        ty: arm.ty(),
                        inner: ExprKind::Binds {
                            binds: vec![Declaration::Val {
                                rec: false,
                                expr: Expr {
                                    ty: p.ty.clone(),
                                    inner: ExprKind::Symbol { name: c.clone() },
                                },
                                pattern: p,
                            }],
                            ret: arm.boxed(),
                        },
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
            Char | Int => false,
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

impl Transform<Type> for CaseSimplifyPass {
    fn transform_val(
        &mut self,
        rec: bool,
        pattern: TypedPattern,
        expr: TypedCoreExpr,
    ) -> TypedCoreDeclaration {
        match pattern {
            // dirty heuristic for simple patterns
            TypedPattern {
                inner: TypedPatternKind::Variable { .. },
                ..
            }
            | TypedPattern {
                inner: TypedPatternKind::Wildcard { .. },
                ..
            } => Declaration::Val {
                rec,
                pattern,
                expr: self.transform_expr(expr),
            },
            pattern => {
                let binds = pattern.binds();
                let ty = Type::Tuple(binds.iter().map(|&(_, ty)| ty.clone()).collect());
                let tuple_pat = binds
                    .into_iter()
                    .map(|(name, ty)| Pattern {
                        ty: ty.clone(),
                        inner: PatternKind::Variable { name: name.clone() },
                    })
                    .collect();
                let tuple_pat = Pattern {
                    ty: ty.clone(),
                    inner: PatternKind::Tuple { tuple: tuple_pat },
                };
                let mut pattern = self.transform_pattern(pattern);
                self.rename_pattern(&mut pattern);
                let binds = pattern.binds();
                let tuple = binds
                    .into_iter()
                    .map(|(name, ty)| Expr {
                        ty: ty.clone(),
                        inner: ExprKind::Symbol { name: name.clone() },
                    })
                    .collect();
                let tuple = Expr {
                    ty: ty.clone(),
                    inner: ExprKind::Tuple { tuple },
                };
                let cond = self.transform_expr(expr);
                Declaration::Val {
                    rec,
                    pattern: tuple_pat,
                    expr: Expr {
                        ty,
                        inner: self.transform_case(cond.boxed(), vec![(pattern, tuple)]),
                    },
                }
            }
        }
    }

    fn transform_case(
        &mut self,
        cond: Box<TypedCoreExpr>,
        clauses: Vec<(TypedPattern, TypedCoreExpr)>,
    ) -> TypedCoreExprKind {
        let condsym = self.gensym("cond");
        let condty = cond.ty();
        let ty = clauses
            .iter()
            .map(|(_, expr)| expr.ty())
            .next()
            .expect("case should have at least 1 clause");
        let clauses = clauses
            .into_iter()
            .map(|(pat, arm)| (vec![pat], self.transform_expr(arm)))
            .collect();
        ExprKind::Binds {
            binds: vec![Declaration::Val {
                pattern: Pattern {
                    ty: condty.clone(),
                    inner: PatternKind::Variable {
                        name: condsym.clone(),
                    },
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
    fn transform_pat_wildcard(&mut self) -> TypedPatternKind {
        PatternKind::Variable {
            name: self.gensym("_"),
        }
    }
}

use crate::pass::Pass;
impl<'a> Pass<TypedCoreContext, TypeError<'a>> for CaseSimplify {
    type Target = TypedCoreContext;

    fn trans<'b>(
        &'b mut self,
        Context(symbol_table, ast): TypedCoreContext,
        _: &Config,
    ) -> Result<'a, Self::Target> {
        let mut pass = self.generate_pass(symbol_table);
        let ast = pass.wildcard_to_variable(ast);
        let ast = pass.transform_ast(ast);
        let (symbol_table, _) = pass.into_inner();
        Ok(Context(symbol_table, ast))
    }
}
