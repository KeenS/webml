use crate::ast;
use crate::config::Config;
use crate::hir::{Expr, HTy, Pattern, Val, HIR};
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;

pub struct AST2HIR {
    id: Id,
}

fn force_into(ty: ast::Type) -> HTy {
    use crate::ast::Type::*;
    match ty {
        Int => HTy::Int,
        Real => HTy::Real,
        Tuple(tys) => HTy::Tuple(tys.into_iter().map(conv_ty).collect()),
        Fun(arg, ret) => HTy::fun(conv_ty(*arg), conv_ty(*ret)),
        Datatype(name) => HTy::Datatype(name),
        Variable(_) => panic!("polymorphism is not supported yet"),
    }
}

fn force_tuple(ty: ast::Type) -> Vec<HTy> {
    use crate::ast::Type::*;
    match ty {
        Tuple(tys) => tys.into_iter().map(conv_ty).collect(),
        _ => panic!(),
    }
}

fn conv_ty(ty: ast::Type) -> HTy {
    force_into(ty)
}

impl AST2HIR {
    pub fn new(id: Id) -> Self {
        Self { id }
    }

    pub fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#g".into(), id)
    }

    fn conv_ast(&mut self, ast: ast::TypedAst) -> HIR {
        HIR(ast
            .0
            .into_iter()
            .flat_map(|stmt| self.conv_statement(stmt))
            .collect())
    }

    fn conv_statement(&mut self, stmt: ast::Statement<ast::Type>) -> Vec<Val> {
        match stmt {
            ast::Statement::Datatype { .. } => {
                // ignore
                vec![]
            }
            ast::Statement::Fun { name, params, expr } => {
                let fun = params
                    .into_iter()
                    .rev()
                    .fold(self.conv_expr(expr), |body, (ty, sym)| Expr::Fun {
                        param: (conv_ty(ty), sym),
                        body_ty: body.ty(),
                        body: Box::new(body),
                        captures: vec![],
                    });
                vec![Val {
                    rec: true,
                    name,
                    ty: fun.ty(),
                    expr: fun,
                }]
            }
            ast::Statement::Val { pattern, expr } => {
                match pattern {
                    ast::Pattern::Variable { name, ty } => vec![Val {
                        ty: conv_ty(ty),
                        rec: false,
                        name: name,
                        expr: self.conv_expr(expr),
                    }],
                    ast::Pattern::Wildcard { ty } => vec![Val {
                        ty: conv_ty(ty),
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
                        ty: conv_ty(ty),
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
                        ty: conv_ty(ty),
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
                                    let ty = conv_ty(ty.clone());
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
                            let ty = conv_ty(ty.clone());
                            ret.push(Val {
                                ty: ty.clone(),
                                rec: false,
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
        }
    }

    fn conv_expr(&mut self, expr: ast::Expr<ast::Type>) -> Expr {
        use crate::ast::Expr as E;
        match expr {
            E::Binds { ty, binds, ret } => Expr::Binds {
                ty: conv_ty(ty),
                binds: binds
                    .into_iter()
                    .flat_map(|s| self.conv_statement(s))
                    .collect(),
                ret: Box::new(self.conv_expr(*ret)),
            },
            E::BinOp { op, ty, l, r } => Expr::BinOp {
                ty: conv_ty(ty),
                name: op,
                l: Box::new(self.conv_expr(*l)),
                r: Box::new(self.conv_expr(*r)),
            },
            E::Fn { ty, param, body } => {
                let (param_ty, body_ty) = match ty {
                    ast::Type::Fun(param_ty, body_ty) => (*param_ty, *body_ty),
                    _ => panic!("internal error: functon is not typed as function"),
                };
                Expr::Fun {
                    param: (conv_ty(param_ty), param),
                    body_ty: conv_ty(body_ty),
                    body: Box::new(self.conv_expr(*body)),
                    captures: Vec::new(),
                }
            }
            E::App { ty, fun, arg } => self.conv_expr(*fun).app1(conv_ty(ty), self.conv_expr(*arg)),
            E::If {
                ty,
                cond,
                then,
                else_,
            } => Expr::Case {
                ty: conv_ty(ty),
                expr: Box::new(self.conv_expr(*cond)),
                arms: vec![
                    (
                        Pattern::Constructor {
                            // FIXME consult it from symbol table
                            name: Symbol::new("true"),
                            ty: HTy::Datatype(Symbol::new("bool")),
                        },
                        self.conv_expr(*then),
                    ),
                    (
                        Pattern::Constructor {
                            // FIXME consult it from symbol table
                            name: Symbol::new("false"),
                            ty: HTy::Datatype(Symbol::new("bool")),
                        },
                        self.conv_expr(*else_),
                    ),
                ],
            },
            E::Case { ty, cond, clauses } => Expr::Case {
                ty: conv_ty(ty),
                expr: Box::new(self.conv_expr(*cond)),
                arms: clauses
                    .into_iter()
                    .map(|(pat, expr)| (self.conv_pat(pat), self.conv_expr(expr)))
                    .collect(),
            },
            E::Tuple { ty, tuple } => Expr::Tuple {
                tys: force_tuple(ty),
                tuple: tuple.into_iter().map(|e| self.conv_expr(e)).collect(),
            },
            E::Constructor { ty, name } => Expr::Constructor {
                ty: conv_ty(ty),
                name,
            },
            E::Symbol { ty, name } => Expr::Sym {
                ty: conv_ty(ty),
                name,
            },
            E::Literal { ty, value } => Expr::Lit {
                ty: conv_ty(ty),
                value,
            },
        }
    }
    fn conv_pat(&mut self, pat: ast::Pattern<ast::Type>) -> Pattern {
        match pat {
            ast::Pattern::Constant { value, ty } => Pattern::Constant {
                value,
                ty: conv_ty(ty),
            },
            ast::Pattern::Constructor { ty, name } => Pattern::Constructor {
                ty: conv_ty(ty),
                name,
            },
            ast::Pattern::Tuple { tuple, .. } => {
                let (tys, tuple) = tuple
                    .into_iter()
                    .map(|(ty, sym)| (conv_ty(ty), sym))
                    .unzip();
                Pattern::Tuple { tuple, tys }
            }
            ast::Pattern::Variable { name, ty } => Pattern::Var {
                name,
                ty: conv_ty(ty),
            },
            ast::Pattern::Wildcard { ty } => Pattern::Var {
                name: Symbol::new("_"),
                ty: conv_ty(ty),
            },
        }
    }
}

impl<E> Pass<ast::TypedAst, E> for AST2HIR {
    type Target = HIR;

    fn trans(&mut self, ast: ast::TypedAst, _: &Config) -> ::std::result::Result<Self::Target, E> {
        Ok(self.conv_ast(ast))
    }
}
