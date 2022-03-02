use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::Symbol;

pub struct Desugar {
    id: Id,
}

impl Desugar {
    pub fn new(id: Id) -> Self {
        Self { id }
    }

    pub fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#arg".into(), id)
    }
}

impl Desugar {
    fn transform_ast(&mut self, ast: UntypedAst) -> UntypedCore {
        AST(ast
            .0
            .into_iter()
            .filter_map(|decl| self.transform_statement(decl))
            .collect())
    }

    fn transform_statement(&mut self, decl: UntypedDeclaration) -> Option<UntypedCoreDeclaration> {
        use Declaration::*;
        match decl {
            Datatype { name, constructors } => Some(self.transform_datatype(name, constructors)),
            Val { rec, pattern, expr } => Some(self.transform_val(rec, pattern, expr)),
            LangItem { name, decl } => self.transform_langitem(name, *decl),
            D(DerivedDeclaration::Fun { name, clauses }) => Some(self.transform_fun(name, clauses)),
            D(DerivedDeclaration::Infix { .. }) => None,
        }
    }

    fn transform_datatype(
        &mut self,
        name: Symbol,
        constructors: Vec<(Symbol, Option<Type>)>,
    ) -> UntypedCoreDeclaration {
        Declaration::Datatype { name, constructors }
    }

    fn transform_val(
        &mut self,
        rec: bool,
        pattern: UntypedPattern,
        expr: UntypedExpr,
    ) -> UntypedCoreDeclaration {
        Declaration::Val {
            rec,
            pattern: self.transform_pattern(pattern),
            expr: self.transform_expr(expr),
        }
    }

    fn transform_fun(
        &mut self,
        name: Symbol,
        clauses: Vec<(Vec<UntypedPattern>, UntypedExpr)>,
    ) -> UntypedCoreDeclaration {
        let arity = clauses[0].0.len();

        let clauses = clauses
            .into_iter()
            .map(|(pats, expr)| {
                (
                    Pattern {
                        ty: Empty {},
                        inner: PatternKind::Tuple { tuple: pats },
                    },
                    self.transform_expr(expr),
                )
            })
            .collect();

        let params = (0..arity).map(|_| self.gensym()).collect::<Vec<_>>();

        let body = Expr {
            ty: Empty {},
            inner: ExprKind::Case {
                cond: Expr {
                    ty: Empty {},
                    inner: ExprKind::Tuple {
                        tuple: params
                            .iter()
                            .cloned()
                            .map(|name| Expr {
                                ty: Empty {},
                                inner: ExprKind::Symbol { name },
                            })
                            .collect(),
                    },
                }
                .boxed(),
                clauses,
            },
        };

        let fun = params.into_iter().rev().fold(body, |body, param| Expr {
            ty: Empty {},
            inner: ExprKind::Fn {
                param,
                body: body.boxed(),
            },
        });
        Declaration::Val {
            rec: true,
            pattern: Pattern {
                ty: Empty {},
                inner: PatternKind::Variable { name: name },
            },
            expr: fun,
        }
    }

    fn transform_langitem(
        &mut self,
        name: LangItem,
        decl: UntypedDeclaration,
    ) -> Option<UntypedCoreDeclaration> {
        if let Some(decl) = self.transform_statement(decl) {
            Some(Declaration::LangItem {
                name,
                decl: Box::new(decl),
            })
        } else {
            None
        }
    }

    fn transform_expr(&mut self, expr: UntypedExpr) -> UntypedCoreExpr {
        use crate::ast::ExprKind::*;
        let inner = match expr.inner {
            Binds { binds, ret } => self.transform_binds(binds, ret),
            BuiltinCall { fun, args } => self.transform_builtincall(fun, args),
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => self.transform_externcall(module, fun, args, argty, retty),
            Fn { param, body } => self.transform_fn(param, body),
            App { fun, arg } => self.transform_app(fun, arg),
            Case { cond, clauses } => self.transform_case(cond, clauses),
            Tuple { tuple } => self.transform_tuple(tuple),
            Constructor { arg, name } => self.transform_constructor(arg, name),
            Symbol { name } => self.transform_symbol(name),
            Literal { value } => self.transform_literal(value),
            D(DerivedExprKind::If { cond, then, else_ }) => self.transform_if(cond, then, else_),
            D(DerivedExprKind::String { value }) => self.transform_string(value),
        };
        UntypedCoreExpr { ty: expr.ty, inner }
    }
    fn transform_binds(
        &mut self,
        binds: Vec<UntypedDeclaration>,
        ret: Box<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        ExprKind::Binds {
            binds: binds
                .into_iter()
                .filter_map(|decl| self.transform_statement(decl))
                .collect(),
            ret: self.transform_expr(*ret).boxed(),
        }
    }

    fn transform_builtincall(&mut self, fun: BIF, args: Vec<UntypedExpr>) -> UntypedCoreExprKind {
        ExprKind::BuiltinCall {
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
        }
    }

    fn transform_externcall(
        &mut self,
        module: String,
        fun: String,
        args: Vec<UntypedExpr>,
        argty: Vec<Type>,
        retty: Type,
    ) -> UntypedCoreExprKind {
        ExprKind::ExternCall {
            module,
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
            argty,
            retty,
        }
    }

    fn transform_fn(&mut self, param: Symbol, body: Box<UntypedExpr>) -> UntypedCoreExprKind {
        ExprKind::Fn {
            param,
            body: self.transform_expr(*body).boxed(),
        }
    }

    fn transform_app(
        &mut self,
        fun: Box<UntypedExpr>,
        arg: Box<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        ExprKind::App {
            fun: self.transform_expr(*fun).boxed(),
            arg: self.transform_expr(*arg).boxed(),
        }
    }

    fn transform_if(
        &mut self,
        cond: Box<UntypedExpr>,
        then: Box<UntypedExpr>,
        else_: Box<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        ExprKind::Case {
            cond: self.transform_expr(*cond).boxed(),
            clauses: vec![
                (
                    Pattern {
                        ty: Empty {},
                        inner: PatternKind::Constructor {
                            arg: None,
                            name: Symbol::new("true"),
                        },
                    },
                    self.transform_expr(*then),
                ),
                (
                    Pattern {
                        ty: Empty {},
                        inner: PatternKind::Constructor {
                            arg: None,
                            name: Symbol::new("false"),
                        },
                    },
                    self.transform_expr(*else_),
                ),
            ],
        }
    }

    fn transform_case(
        &mut self,
        cond: Box<UntypedExpr>,
        clauses: Vec<(UntypedPattern, UntypedExpr)>,
    ) -> UntypedCoreExprKind {
        ExprKind::Case {
            cond: self.transform_expr(*cond).boxed(),
            clauses: clauses
                .into_iter()
                .map(|(p, e)| (self.transform_pattern(p), self.transform_expr(e)))
                .collect(),
        }
    }

    fn transform_tuple(&mut self, tuple: Vec<UntypedExpr>) -> UntypedCoreExprKind {
        ExprKind::Tuple {
            tuple: tuple.into_iter().map(|t| self.transform_expr(t)).collect(),
        }
    }

    fn transform_constructor(
        &mut self,
        arg: Option<Box<UntypedExpr>>,
        name: Symbol,
    ) -> UntypedCoreExprKind {
        ExprKind::Constructor {
            arg: arg.map(|e| self.transform_expr(*e).boxed()),
            name,
        }
    }
    fn transform_symbol(&mut self, name: Symbol) -> UntypedCoreExprKind {
        ExprKind::Symbol { name }
    }

    fn transform_literal(&mut self, value: Literal) -> UntypedCoreExprKind {
        ExprKind::Literal { value }
    }

    fn transform_string(&mut self, value: Vec<u32>) -> UntypedCoreExprKind {
        let empty = Expr {
            ty: Empty {},
            inner: ExprKind::Constructor {
                arg: None,
                name: Symbol::new("Empty"),
            },
        };
        fn cons(s: UntypedCoreExpr, c: u32) -> UntypedCoreExpr {
            let c = Expr {
                ty: Empty {},
                inner: ExprKind::Literal {
                    value: Literal::Char(c),
                },
            };
            let tuple = Expr {
                ty: Empty {},
                inner: ExprKind::Tuple { tuple: vec![c, s] },
            };
            Expr {
                ty: Empty {},
                inner: ExprKind::Constructor {
                    arg: Some(Box::new(tuple)),
                    name: Symbol::new("Char").into(),
                },
            }
        }
        value.into_iter().rfold(empty, cons).inner
    }

    fn transform_pattern(&mut self, pattern: UntypedPattern) -> UntypedPattern {
        pattern
    }
}

impl<E> Pass<UntypedAst, E> for Desugar {
    type Target = UntypedCore;

    fn trans(&mut self, ast: UntypedAst, _: &Config) -> ::std::result::Result<Self::Target, E> {
        let core = self.transform_ast(ast);
        Ok(core)
    }
}
