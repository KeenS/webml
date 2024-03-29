use crate::ast;
use crate::config::Config;
use crate::hir::{Context, Expr, HTy, Pattern, SymbolTable, TypeInfo, Val, BIF, HIR};
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;

pub struct AST2HIR {
    id: Id,
}

struct AST2HIRPass {
    symbol_table: ast::SymbolTable,
    id: Id,
}

impl AST2HIR {
    pub fn new(id: Id) -> Self {
        Self { id }
    }

    fn generate_pass(&mut self, symbol_table: ast::SymbolTable) -> AST2HIRPass {
        AST2HIRPass::new(symbol_table, self.id.clone())
    }
}

fn conv_symbol_table(symbol_table: ast::SymbolTable) -> SymbolTable {
    SymbolTable {
        types: symbol_table
            .types
            .into_iter()
            .map(|(k, v)| (k, conv_type_info(v)))
            .collect(),
    }
}

fn conv_type_info(type_info: ast::TypeInfo) -> TypeInfo {
    TypeInfo {
        constructors: type_info
            .constructors
            .into_iter()
            .enumerate()
            .map(|(des, (_, arg))| (des as u32, arg.map(conv_ty)))
            .collect(),
    }
}

fn conv_ty(ty: ast::Type) -> HTy {
    use crate::ast::Type::*;
    match ty {
        Char => HTy::Char,
        Int => HTy::Int,
        Real => HTy::Real,
        Tuple(tys) => HTy::Tuple(tys.into_iter().map(conv_ty).collect()),
        Fun(arg, ret) => HTy::fun(conv_ty(*arg), conv_ty(*ret)),
        Datatype(name) => HTy::Datatype(name),
        Variable(_) => panic!("polymorphism is not supported yet"),
    }
}

impl AST2HIRPass {
    fn new(symbol_table: ast::SymbolTable, id: Id) -> Self {
        Self { symbol_table, id }
    }
    fn symbol_table(&self) -> &ast::SymbolTable {
        &self.symbol_table
    }

    pub fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#g".into(), id)
    }

    fn force_tuple(&self, ty: ast::Type) -> Vec<HTy> {
        use crate::ast::Type::*;
        match ty {
            Tuple(tys) => tys.into_iter().map(conv_ty).collect(),
            _ => panic!(),
        }
    }

    fn conv_ast(&mut self, ast: ast::TypedCore) -> HIR {
        HIR(ast
            .0
            .into_iter()
            .flat_map(|decl| self.conv_statement(decl))
            .collect())
    }

    fn conv_statement(&mut self, decl: ast::TypedCoreDeclaration) -> Vec<Val> {
        match decl {
            ast::Declaration::Datatype { .. } => {
                // ignore
                vec![]
            }
            ast::Declaration::Val { rec, pattern, expr } => {
                let ty = pattern.ty.clone();
                match pattern.inner {
                    ast::PatternKind::Variable { name } => vec![Val {
                        ty: conv_ty(ty),
                        rec,
                        name,
                        expr: self.conv_expr(expr),
                    }],
                    ast::PatternKind::Wildcard {} => vec![Val {
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
                    ast::PatternKind::Constant { .. } => vec![Val {
                        ty: conv_ty(ty),
                        rec: false,
                        name: self.gensym(),
                        expr: self.conv_expr(expr),
                    }],
                    ast::PatternKind::Char { .. } => vec![Val {
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
                    ast::PatternKind::Constructor { .. } => vec![Val {
                        ty: conv_ty(ty),
                        rec: false,
                        name: self.gensym(),
                        expr: self.conv_expr(expr),
                    }],
                    ast::PatternKind::Tuple { .. } => {
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
                    ast::PatternKind::D(d) => match d {},
                }
            }
            ast::Declaration::LangItem { decl, .. } => self.conv_statement(*decl),
            ast::Declaration::Local { binds, body } => {
                let mut binds = binds
                    .into_iter()
                    .flat_map(|b| self.conv_statement(b))
                    .collect::<Vec<_>>();
                let body = body.into_iter().flat_map(|b| self.conv_statement(b));
                binds.extend(body);
                binds
            }
            ast::Declaration::D(d) => match d {},
        }
    }

    fn conv_expr(&mut self, expr: ast::TypedCoreExpr) -> Expr {
        use crate::ast::ExprKind as E;
        let ty = expr.ty;
        match expr.inner {
            E::Binds { binds, ret } => {
                let ty = conv_ty(ty);
                // collecting into vec here to avoid lifetime error
                let binds = binds
                    .into_iter()
                    .flat_map(|s| self.conv_statement(s))
                    .collect::<Vec<_>>();
                binds
                    .into_iter()
                    .rev()
                    .fold(self.conv_expr(*ret), |ret, bind| Expr::Let {
                        ty: ty.clone(),
                        bind: Box::new(bind),
                        ret: Box::new(ret),
                    })
            }
            E::BuiltinCall { fun, args } => Expr::BuiltinCall {
                ty: conv_ty(ty),
                fun: self.conv_bif(fun),
                args: args.into_iter().map(|arg| self.conv_expr(arg)).collect(),
            },
            E::ExternCall {
                module,
                fun,
                args,
                argty: _,
                retty: _,
            } => Expr::ExternCall {
                ty: conv_ty(ty),
                module,
                fun,

                args: args.into_iter().map(|arg| self.conv_expr(arg)).collect(),
            },
            E::Fn { param, body } => {
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
            E::App { fun, arg } => self.conv_expr(*fun).app1(conv_ty(ty), self.conv_expr(*arg)),
            E::Case { cond, clauses } => Expr::Case {
                ty: conv_ty(ty),
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
                ty: conv_ty(ty),
                arg: arg.map(|a| Box::new(self.conv_expr(*a))),
                descriminant: self.conv_constructor_name(&name),
            },
            E::Symbol { name } => Expr::Sym {
                ty: conv_ty(ty),
                name,
            },
            E::Literal { value } => Expr::Lit {
                ty: conv_ty(ty),
                value,
            },
            E::D(d) => match d {},
        }
    }
    fn conv_pat(&mut self, pat: ast::TypedCorePattern) -> Pattern {
        let ty = pat.ty;
        match pat.inner {
            ast::PatternKind::Constant { value } => Pattern::Constant {
                value,
                ty: conv_ty(ty),
            },
            ast::PatternKind::Char { value } => Pattern::Char {
                value,
                ty: conv_ty(ty),
            },
            ast::PatternKind::Constructor { arg, name } => Pattern::Constructor {
                ty: conv_ty(ty),
                arg: arg.map(|pat| match *pat {
                    ast::Pattern {
                        ty,
                        inner: ast::PatternKind::Variable { name },
                        ..
                    } => (conv_ty(ty), name),
                    _ => panic!("internal error: pattern"),
                }),
                descriminant: self.conv_constructor_name(&name),
            },
            ast::PatternKind::Tuple { tuple, .. } => {
                let (tys, tuple) = tuple
                    .into_iter()
                    .map(|pat| match pat {
                        ast::Pattern {
                            ty,
                            inner: ast::PatternKind::Variable { name },
                            ..
                        } => (conv_ty(ty), name),
                        _ => panic!("internal error: pattern"),
                    })
                    .unzip();
                Pattern::Tuple { tuple, tys }
            }
            ast::PatternKind::Variable { name } => Pattern::Var {
                name,
                ty: conv_ty(ty),
            },
            ast::PatternKind::Wildcard {} => Pattern::Var {
                name: Symbol::new("_"),
                ty: conv_ty(ty),
            },
            ast::PatternKind::D(d) => match d {},
        }
    }

    fn conv_bif(&mut self, bif: ast::BIF) -> BIF {
        use ast::BIF::*;
        match bif {
            AddInt => BIF::AddInt,
            AddReal => BIF::AddReal,
            SubInt => BIF::SubInt,
            SubReal => BIF::SubReal,
            MulInt => BIF::MulInt,
            MulReal => BIF::MulReal,
            Div => BIF::DivInt,
            Divf => BIF::DivReal,
            ModInt => BIF::ModInt,
            EqInt => BIF::EqInt,
            EqReal => BIF::EqReal,
            EqChar => BIF::EqChar,
            NeqInt => BIF::NeqInt,
            NeqReal => BIF::NeqReal,
            NeqChar => BIF::NeqChar,
            GtInt => BIF::GtInt,
            GtReal => BIF::GtReal,
            GtChar => BIF::GtChar,
            GeInt => BIF::GeInt,
            GeReal => BIF::GeReal,
            GeChar => BIF::GeChar,
            LtInt => BIF::LtInt,
            LtReal => BIF::LtReal,
            LtChar => BIF::LtChar,
            LeInt => BIF::LeInt,
            LeReal => BIF::LeReal,
            LeChar => BIF::LeChar,
            op @ (Add | Sub | Mul | Mod | Eq | Neq | Gt | Ge | Lt | Le) => {
                panic!("unresolved overload found: {}", op)
            }
        }
    }

    fn conv_constructor_name(&mut self, name: &Symbol) -> u32 {
        self.symbol_table().constructor_to_id(name)
    }
}

impl<E> Pass<ast::TypedCoreContext, E> for AST2HIR {
    type Target = Context;

    fn trans(
        &mut self,
        context: ast::TypedCoreContext,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        let symbol_table = context.symbol_table;
        let ast = context.ast;

        let mut pass = self.generate_pass(symbol_table);
        let ast = pass.conv_ast(ast);
        let symbol_table = conv_symbol_table(pass.symbol_table);
        Ok(Context(symbol_table, ast))
    }
}
