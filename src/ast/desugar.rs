use crate::ast::util::Transform;
use crate::ast::*;
use crate::prim::Symbol;

pub struct Desugar;

impl Desugar {
    pub fn desugar_statement(&mut self, stmt: Statement<Type>) -> Statement<Type> {
        self.transform_statement(stmt)
    }

    pub fn desugar_expr(&mut self, expr: Expr<Type>) -> Expr<Type> {
        self.transform_expr(expr)
    }
}

impl Transform<Type> for Desugar {
    fn transform_fun(
        &mut self,
        name: Symbol,
        params: Vec<(Type, Symbol)>,
        expr: Expr<Type>,
    ) -> Statement<Type> {
        let fun = params
            .into_iter()
            .rev()
            .fold(expr, |body, (ty, sym)| Expr::Fn {
                ty: Type::Fun(Box::new(ty), Box::new(body.ty())),
                param: sym,
                body: Box::new(body),
            });
        Statement::Val {
            rec: true,
            pattern: Pattern::Variable {
                name: name,
                ty: fun.ty(),
            },
            expr: fun,
        }
    }

    fn transform_if(
        &mut self,
        ty: Type,
        cond: Box<Expr<Type>>,
        then: Box<Expr<Type>>,
        else_: Box<Expr<Type>>,
    ) -> Expr<Type> {
        Expr::Case {
            ty,
            cond,
            clauses: vec![
                (
                    Pattern::Constructor {
                        ty: Type::Datatype(Symbol::new("bool")),
                        name: Symbol::new("true"),
                    },
                    *then,
                ),
                (
                    Pattern::Constructor {
                        ty: Type::Datatype(Symbol::new("bool")),
                        name: Symbol::new("false"),
                    },
                    *else_,
                ),
            ],
        }
    }
}
