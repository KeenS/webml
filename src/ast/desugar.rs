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
            Local { binds, body } => Some(self.transform_local(binds, body)),
            D(DerivedDeclaration::Fun { name, clauses }) => Some(self.transform_fun(name, clauses)),
            D(
                DerivedDeclaration::Infix { .. }
                | DerivedDeclaration::Infixr { .. }
                | DerivedDeclaration::Nonfix { .. },
            ) => None,
            D(DerivedDeclaration::Expr { expr }) => Some(self.transform_decl_expr(expr)),
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

        let span = {
            // assuming at least 1 clauses and at least 1 argument
            let start = clauses[0].0[0].span.start;
            let end = clauses.last().unwrap().1.span.end;
            start..end
        };
        let clauses = clauses
            .into_iter()
            .map(|(pats, expr)| {
                let tuple = pats
                    .into_iter()
                    .map(|p| self.transform_pattern(p))
                    .collect();
                (
                    Pattern::new(span.clone(), PatternKind::Tuple { tuple }),
                    self.transform_expr(expr),
                )
            })
            .collect();

        let params = (0..arity).map(|_| self.gensym()).collect::<Vec<_>>();

        let body = Expr::new(
            span.clone(),
            ExprKind::Case {
                cond: Expr::new(
                    span.clone(),
                    ExprKind::Tuple {
                        tuple: params
                            .iter()
                            .cloned()
                            .map(|name| Expr::new(span.clone(), ExprKind::Symbol { name }))
                            .collect(),
                    },
                )
                .boxed(),
                clauses,
            },
        );

        let fun = params.into_iter().rev().fold(body, |body, param| {
            Expr::new(
                span.clone(),
                ExprKind::Fn {
                    param,
                    body: body.boxed(),
                },
            )
        });
        Declaration::Val {
            rec: true,
            pattern: Pattern::new(span, PatternKind::Variable { name }),
            expr: fun,
        }
    }

    fn transform_decl_expr(&mut self, expr: UntypedExpr) -> UntypedCoreDeclaration {
        Declaration::Val {
            rec: false,
            pattern: Pattern::new(
                expr.span.clone(),
                PatternKind::Variable {
                    name: Symbol::new("it"),
                },
            ),
            expr: self.transform_expr(expr),
        }
    }

    fn transform_langitem(
        &mut self,
        name: LangItem,
        decl: UntypedDeclaration,
    ) -> Option<UntypedCoreDeclaration> {
        match self.transform_statement(decl) {
            Some(decl) => Some(Declaration::LangItem {
                name,
                decl: Box::new(decl),
            }),
            None => None,
        }
    }

    fn transform_local(
        &mut self,
        binds: Vec<UntypedDeclaration>,
        body: Vec<UntypedDeclaration>,
    ) -> UntypedCoreDeclaration {
        let binds = binds
            .into_iter()
            .flat_map(|b| self.transform_statement(b))
            .collect();
        let body = body
            .into_iter()
            .flat_map(|b| self.transform_statement(b))
            .collect();
        Declaration::Local { binds, body }
    }

    fn transform_expr(&mut self, expr: UntypedExpr) -> UntypedCoreExpr {
        use crate::ast::ExprKind::*;
        let span = expr.span.clone();
        let inner = match expr.inner {
            Binds { binds, ret } => self.transform_binds(span, binds, ret),
            BuiltinCall { fun, args } => self.transform_builtincall(span, fun, args),
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => self.transform_externcall(span, module, fun, args, argty, retty),
            Fn { param, body } => self.transform_fn(span, param, body),
            App { fun, arg } => self.transform_app(span, fun, arg),
            Case { cond, clauses } => self.transform_case(span, cond, clauses),
            Tuple { tuple } => self.transform_tuple(span, tuple),
            Constructor { arg, name } => self.transform_constructor(span, arg, name),
            Symbol { name } => self.transform_symbol(span, name),
            Literal { value } => self.transform_literal(span, value),
            D(DerivedExprKind::If { cond, then, else_ }) => {
                self.transform_if(span, cond, then, else_)
            }
            D(DerivedExprKind::AndAlso { l, r }) => self.transform_andalso(span, l, r),
            D(DerivedExprKind::OrElse { l, r }) => self.transform_orelse(span, l, r),
            D(DerivedExprKind::Seq { seq }) => self.transform_seq(span, seq),
            D(DerivedExprKind::BindSeq { binds, ret }) => self.transform_bind_seq(span, binds, ret),
            D(DerivedExprKind::String { value }) => self.transform_string(span, value),
            D(DerivedExprKind::Op { name }) => self.transform_op(span, name),
        };
        UntypedCoreExpr {
            ty: expr.ty,
            span: expr.span,
            inner,
        }
    }
    fn transform_binds(
        &mut self,
        _: Span,
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

    fn transform_builtincall(
        &mut self,
        _: Span,
        fun: BIF,
        args: Vec<UntypedExpr>,
    ) -> UntypedCoreExprKind {
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
        _: Span,
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

    fn transform_fn(
        &mut self,
        _: Span,
        param: Symbol,
        body: Box<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        ExprKind::Fn {
            param,
            body: self.transform_expr(*body).boxed(),
        }
    }

    fn transform_app(
        &mut self,
        _: Span,
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
        span: Span,
        cond: Box<UntypedExpr>,
        then: Box<UntypedExpr>,
        else_: Box<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        ExprKind::Case {
            cond: self.transform_expr(*cond).boxed(),
            clauses: vec![
                (
                    Pattern::new(
                        span.clone(),
                        PatternKind::Constructor {
                            arg: None,
                            name: Symbol::new("true"),
                        },
                    ),
                    self.transform_expr(*then),
                ),
                (
                    Pattern::new(
                        span,
                        PatternKind::Constructor {
                            arg: None,
                            name: Symbol::new("false"),
                        },
                    ),
                    self.transform_expr(*else_),
                ),
            ],
        }
    }

    fn transform_andalso(
        &mut self,
        span: Span,
        l: Box<UntypedExpr>,
        r: Box<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        self.transform_if(
            span.clone(),
            l,
            r,
            Box::new(Expr::new(
                span,
                ExprKind::Constructor {
                    arg: None,
                    name: Symbol::new("false"),
                },
            )),
        )
    }

    fn transform_orelse(
        &mut self,
        span: Span,
        l: Box<UntypedExpr>,
        r: Box<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        self.transform_if(
            span.clone(),
            l,
            Box::new(Expr::new(
                span,
                ExprKind::Constructor {
                    arg: None,
                    name: Symbol::new("true"),
                },
            )),
            r,
        )
    }

    fn transform_seq(&mut self, _: Span, mut seq: Vec<UntypedExpr>) -> UntypedCoreExprKind {
        assert!(!seq.is_empty());
        let tail = seq.pop().expect("seq is not empty");
        let tail = self.transform_expr(tail);
        let seq = seq.into_iter().map(|e| self.transform_expr(e));
        seq.rfold(tail, |acc, e| {
            let span = e.span.start..acc.span.end;
            Expr::new(
                span.clone(),
                ExprKind::Case {
                    cond: e.boxed(),
                    clauses: vec![(Pattern::new(span, PatternKind::Wildcard {}), acc)],
                },
            )
        })
        .inner
    }

    fn transform_bind_seq(
        &mut self,
        span: Span,
        binds: Vec<UntypedDeclaration>,
        seq: Vec<UntypedExpr>,
    ) -> UntypedCoreExprKind {
        assert!(!seq.is_empty());
        let seq_start = seq[0].span.start;
        let seq_end = seq[seq.len() - 1].span.end;

        let expr = Expr {
            ty: Empty {},
            span,
            inner: ExprKind::Binds {
                binds,
                ret: Box::new(Expr {
                    ty: Empty {},
                    span: seq_start..seq_end,
                    inner: ExprKind::D(DerivedExprKind::Seq { seq }),
                }),
            },
        };
        self.transform_expr(expr).inner
    }

    fn transform_case(
        &mut self,
        _: Span,
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

    fn transform_tuple(&mut self, _: Span, tuple: Vec<UntypedExpr>) -> UntypedCoreExprKind {
        ExprKind::Tuple {
            tuple: tuple.into_iter().map(|t| self.transform_expr(t)).collect(),
        }
    }

    fn transform_constructor(
        &mut self,
        _: Span,
        arg: Option<Box<UntypedExpr>>,
        name: Symbol,
    ) -> UntypedCoreExprKind {
        ExprKind::Constructor {
            arg: arg.map(|e| self.transform_expr(*e).boxed()),
            name,
        }
    }
    fn transform_symbol(&mut self, _: Span, name: Symbol) -> UntypedCoreExprKind {
        ExprKind::Symbol { name }
    }

    fn transform_literal(&mut self, _: Span, value: Literal) -> UntypedCoreExprKind {
        ExprKind::Literal { value }
    }

    fn transform_string(&mut self, span: Span, value: Vec<u32>) -> UntypedCoreExprKind {
        let empty = Expr::new(
            span.clone(),
            ExprKind::Constructor {
                arg: None,
                name: Symbol::new("Empty"),
            },
        );
        fn cons(span: Span, s: UntypedCoreExpr, c: u32) -> UntypedCoreExpr {
            let c = Expr::new(
                span.clone(),
                ExprKind::Literal {
                    value: Literal::Char(c),
                },
            );
            let tuple = Expr::new(span.clone(), ExprKind::Tuple { tuple: vec![c, s] });
            Expr::new(
                span,
                ExprKind::Constructor {
                    arg: Some(Box::new(tuple)),
                    name: Symbol::new("Char"),
                },
            )
        }
        value
            .into_iter()
            .rfold(empty, |s, c| cons(span.clone(), s, c))
            .inner
    }

    fn transform_op(&mut self, _: Span, name: Symbol) -> UntypedCoreExprKind {
        ExprKind::Symbol { name }
    }

    fn transform_pattern(&mut self, pattern: UntypedPattern) -> UntypedCorePattern {
        use PatternKind::*;
        let span = pattern.span;
        let inner = match pattern.inner {
            Constant { value } => UntypedCorePatternKind::Constant { value },
            Char { value } => UntypedCorePatternKind::Char { value },
            Constructor { name, arg } => {
                let arg = arg.map(|p| Box::new(self.transform_pattern(*p)));
                UntypedCorePatternKind::Constructor { name, arg }
            }
            Tuple { tuple } => {
                let tuple = tuple
                    .into_iter()
                    .map(|p| self.transform_pattern(p))
                    .collect();
                UntypedCorePatternKind::Tuple { tuple }
            }
            Variable { name } => UntypedCorePatternKind::Variable { name },
            Wildcard {} => UntypedCorePatternKind::Wildcard {},
            D(DerivedPatternKind::String { value }) => {
                let empty = UntypedCorePattern::new(
                    span.clone(),
                    PatternKind::Constructor {
                        arg: None,
                        name: Symbol::new("Empty"),
                    },
                );
                fn cons(span: Span, s: UntypedCorePattern, c: u32) -> UntypedCorePattern {
                    let c = Pattern::new(span.clone(), PatternKind::Char { value: c });
                    let tuple =
                        Pattern::new(span.clone(), PatternKind::Tuple { tuple: vec![c, s] });
                    Pattern::new(
                        span,
                        PatternKind::Constructor {
                            arg: Some(Box::new(tuple)),
                            name: Symbol::new("Char"),
                        },
                    )
                }
                value
                    .into_iter()
                    .rfold(empty, |s, c| cons(span.clone(), s, c))
                    .inner
            }
            D(DerivedPatternKind::Op { name }) => UntypedCorePatternKind::Variable { name },
        };
        UntypedCorePattern::new(span, inner)
    }
}

impl<E> Pass<UntypedAst, E> for Desugar {
    type Target = UntypedCore;

    fn trans(&mut self, ast: UntypedAst, _: &Config) -> ::std::result::Result<Self::Target, E> {
        let core = self.transform_ast(ast);
        Ok(core)
    }
}
