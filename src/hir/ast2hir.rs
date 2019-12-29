use crate::ast;
use crate::config::Config;
use crate::hir::{Expr, HTy, Pattern, Val, HIR};
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;

pub struct AST2HIR {
    id: Id,
    symbol_table: Option<ast::SymbolTable>,
}

impl AST2HIR {
    pub fn new(id: Id) -> Self {
        Self {
            id,
            symbol_table: None,
        }
    }
    fn init(&mut self, symbol_table: ast::SymbolTable) {
        self.symbol_table = Some(symbol_table)
    }

    fn symbol_table(&self) -> &ast::SymbolTable {
        self.symbol_table.as_ref().unwrap()
    }

    pub fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#g".into(), id)
    }

    fn force_tuple(&self, ty: ast::Type) -> Vec<HTy> {
        use crate::ast::Type::*;
        match ty {
            Tuple(tys) => tys.into_iter().map(|ty| self.conv_ty(ty)).collect(),
            _ => panic!(),
        }
    }

    fn conv_ty(&self, ty: ast::Type) -> HTy {
        use crate::ast::Type::*;
        match ty {
            Int => HTy::Int,
            Real => HTy::Real,
            Tuple(tys) => HTy::Tuple(tys.into_iter().map(|ty| self.conv_ty(ty)).collect()),
            Fun(arg, ret) => HTy::fun(self.conv_ty(*arg), self.conv_ty(*ret)),
            Datatype(name) => {
                let constructors = &self
                    .symbol_table()
                    .get_type(&name)
                    .expect("internal error: type not found")
                    .constructors;
                let descriminants = constructors
                    .into_iter()
                    .enumerate()
                    .map(|(i, (_, ty))| (i as u32, ty.as_ref().map(|ty| self.conv_ty(ty.clone()))))
                    .collect();
                HTy::Datatype(descriminants)
            }
            Variable(_) => panic!("polymorphism is not supported yet"),
        }
    }

    fn conv_ast(&mut self, ast: ast::TypedCore) -> HIR {
        HIR(ast
            .0
            .into_iter()
            .flat_map(|stmt| self.conv_statement(stmt))
            .collect())
    }

    fn conv_statement(&mut self, stmt: ast::TypedCoreStatement) -> Vec<Val> {
        match stmt {
            ast::Statement::Datatype { .. } => {
                // ignore
                vec![]
            }
            ast::Statement::Val { rec, pattern, expr } => {
                match pattern {
                    ast::Pattern::Variable { name, ty } => vec![Val {
                        ty: self.conv_ty(ty),
                        rec: false,
                        name: name,
                        expr: self.conv_expr(expr),
                    }],
                    ast::Pattern::Wildcard { ty } => vec![Val {
                        ty: self.conv_ty(ty),
                        rec: false,
                        name: self.gensym(),
                        expr: self.conv_expr(expr),
                    }],

                    // TODO: implement
                    // ```
                    // val 1 = 2
                    // ```
                    //
                    // to
                    //
                    // ```
                    // val _ = case 1 of 2 => ()
                    // ```
                    //
                    // FIXME: raise Match error when not match
                    ast::Pattern::Constant { ty, .. } => vec![Val {
                        ty: self.conv_ty(ty),
                        rec: false,
                        name: self.gensym(),
                        expr: self.conv_expr(expr),
                    }],
                    // when C(p1, p2, p3) binds var1 var2 var3, convert
                    //
                    // ```
                    // val C(p1, p2, p3) = expr
                    // ```
                    //
                    // to
                    //
                    // ```
                    // val tmp = case expr of C(p1, p2, p3)  => (var1, var2, var3)
                    // val var1 = #1 tmp
                    // val var2 = #2 tmp
                    // val var3 = #3 tmp
                    // ```
                    //
                    // FIXME: raise Match error when not match
                    ast::Pattern::Constructor { ty, .. } => vec![Val {
                        ty: self.conv_ty(ty),
                        rec: false,
                        name: self.gensym(),
                        expr: self.conv_expr(expr),
                    }],
                    ast::Pattern::Tuple { .. } => {
                        // when (p1, p2, p3) binds var1 var2 var3, convert
                        //
                        // ```
                        // val (p1, p2, p3) = expr
                        // ```
                        //
                        // to
                        //
                        // ```
                        // val tmp = case expr of (p1, p2, p3)  => (var1, var2, var3)
                        // val var1 = #1 tmp
                        // val var2 = #2 tmp
                        // val var3 = #3 tmp
                        // ```
                        let binds = pattern
                            .binds()
                            .iter()
                            .map(|&(name, ty)| (name.clone(), ty.clone()))
                            .collect::<Vec<_>>();
                        let (case, tuple_ty) = {
                            let (tys, tuple): (Vec<_>, Vec<_>) = pattern
                                .binds()
                                .iter()
                                .map(|&(name, ty)| {
                                    let ty = self.conv_ty(ty.clone());
                                    let expr = Expr::Sym {
                                        ty: ty.clone(),
                                        name: name.clone(),
                                    };
                                    (ty, expr)
                                })
                                .unzip();
                            let tuple_tys = HTy::Tuple(tys.clone());
                            let tuple = Expr::Tuple { tys, tuple };
                            let pattern = self.conv_pat(pattern);
                            // FIXME: this transformation should be done before case_check
                            // assert!(pattern.is_irrefutable());

                            (
                                Expr::Case {
                                    ty: tuple_tys.clone(),
                                    expr: Box::new(self.conv_expr(expr)),
                                    arms: vec![(pattern, tuple)],
                                },
                                tuple_tys,
                            )
                        };

                        let name = self.gensym();
                        let mut ret = vec![Val {
                            ty: tuple_ty.clone(),
                            rec: false,
                            name: name.clone(),
                            expr: case,
                        }];
                        let tuple = Box::new(Expr::Sym { ty: tuple_ty, name });
                        for (index, (var, ty)) in binds.into_iter().enumerate() {
                            let ty = self.conv_ty(ty.clone());
                            ret.push(Val {
                                ty: ty.clone(),
                                rec,
                                name: var.clone(),
                                expr: Expr::Proj {
                                    ty: ty.clone(),
                                    index: index as u32,
                                    tuple: tuple.clone(),
                                },
                            })
                        }
                        ret
                    }
                }
            }
            ast::Statement::D(d) => match d {},
        }
    }

    fn conv_expr(&mut self, expr: ast::TypedCoreExpr) -> Expr {
        use crate::ast::ExprKind as E;
        let ty = expr.ty;
        match expr.inner {
            E::Binds { binds, ret } => Expr::Binds {
                ty: self.conv_ty(ty),
                binds: binds
                    .into_iter()
                    .flat_map(|s| self.conv_statement(s))
                    .collect(),
                ret: Box::new(self.conv_expr(*ret)),
            },
            E::BuiltinCall { fun, args } => Expr::BuiltinCall {
                ty: self.conv_ty(ty),
                fun,
                args: args.into_iter().map(|arg| self.conv_expr(arg)).collect(),
            },
            E::Fn { param, body } => {
                let (param_ty, body_ty) = match ty {
                    ast::Type::Fun(param_ty, body_ty) => (*param_ty, *body_ty),
                    _ => panic!("internal error: functon is not typed as function"),
                };
                Expr::Fun {
                    param: (self.conv_ty(param_ty), param),
                    body_ty: self.conv_ty(body_ty),
                    body: Box::new(self.conv_expr(*body)),
                    captures: Vec::new(),
                }
            }
            E::App { fun, arg } => self
                .conv_expr(*fun)
                .app1(self.conv_ty(ty), self.conv_expr(*arg)),
            E::Case { cond, clauses } => Expr::Case {
                ty: self.conv_ty(ty),
                expr: Box::new(self.conv_expr(*cond)),
                arms: clauses
                    .into_iter()
                    .map(|(pat, expr)| (self.conv_pat(pat), self.conv_expr(expr)))
                    .collect(),
            },
            E::Tuple { tuple } => Expr::Tuple {
                tys: self.force_tuple(ty),
                tuple: tuple.into_iter().map(|e| self.conv_expr(e)).collect(),
            },
            E::Constructor { arg, name } => Expr::Constructor {
                ty: self.conv_ty(ty),
                arg: arg.map(|a| Box::new(self.conv_expr(*a))),
                descriminant: self.conv_constructor_name(&name),
            },
            E::Symbol { name } => Expr::Sym {
                ty: self.conv_ty(ty),
                name,
            },
            E::Literal { value } => Expr::Lit {
                ty: self.conv_ty(ty),
                value,
            },
            E::D(d) => match d {},
        }
    }
    fn conv_pat(&mut self, pat: ast::TypedPattern) -> Pattern {
        match pat {
            ast::Pattern::Constant { value, ty } => Pattern::Constant {
                value,
                ty: self.conv_ty(ty),
            },
            ast::Pattern::Constructor { ty, arg, name } => Pattern::Constructor {
                ty: self.conv_ty(ty),
                arg: arg.map(|pat| match *pat {
                    ast::Pattern::Variable { name, ty } => (self.conv_ty(ty), name),
                    _ => panic!("internal error: pattern"),
                }),
                descriminant: self.conv_constructor_name(&name),
            },
            ast::Pattern::Tuple { tuple, .. } => {
                let (tys, tuple) = tuple
                    .into_iter()
                    .map(|pat| match pat {
                        ast::Pattern::Variable { name, ty } => (self.conv_ty(ty), name),
                        _ => panic!("internal error: pattern"),
                    })
                    .unzip();
                Pattern::Tuple { tuple, tys }
            }
            ast::Pattern::Variable { name, ty } => Pattern::Var {
                name,
                ty: self.conv_ty(ty),
            },
            ast::Pattern::Wildcard { ty } => Pattern::Var {
                name: Symbol::new("_"),
                ty: self.conv_ty(ty),
            },
        }
    }

    fn conv_constructor_name(&mut self, name: &Symbol) -> u32 {
        self.symbol_table().constructor_to_id(name)
    }
}

impl<E> Pass<(ast::SymbolTable, ast::TypedCore), E> for AST2HIR {
    type Target = HIR;

    fn trans(
        &mut self,
        (symbol_table, ast): (ast::SymbolTable, ast::TypedCore),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        self.init(symbol_table);
        Ok(self.conv_ast(ast))
    }
}
