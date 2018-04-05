use ast;
use pass::Pass;
use hir::{Expr, HTy, Pattern, Val, HIR};
use prim::*;

pub struct AST2HIR;

fn force_into(ty: ast::Ty) -> HTy {
    use ast::Ty::*;
    match ty {
        Unit => HTy::Unit,
        Bool => HTy::Bool,
        Int => HTy::Int,
        Float => HTy::Float,
        Tuple(tys) => HTy::Tuple(tys.into_iter().map(conv_ty).collect()),
        Fun(arg, ret) => HTy::fun(conv_ty(arg), conv_ty(ret)),
    }
}

fn force_tuple(ty: ast::Ty) -> Vec<HTy> {
    use ast::Ty::*;
    match ty {
        Tuple(tys) => tys.into_iter().map(conv_ty).collect(),
        _ => panic!(),
    }
}

fn conv_ty(ty: ast::TyDefer) -> HTy {
    force_into(ty.force("internal typing error"))
}

impl AST2HIR {
    fn conv_ast(&self, ast: ast::AST) -> HIR {
        HIR(ast.0.into_iter().map(|val| self.conv_val(val)).collect())
    }

    fn conv_val(&self, val: ast::Val) -> Val {
        Val {
            ty: conv_ty(val.ty),
            rec: val.rec,
            name: val.name,
            expr: self.conv_expr(val.expr),
        }
    }

    fn conv_expr(&self, expr: ast::Expr) -> Expr {
        use ast::Expr as E;
        match expr {
            E::Binds { ty, binds, ret } => Expr::Binds {
                ty: conv_ty(ty),
                binds: binds.into_iter().map(|b| self.conv_val(b)).collect(),
                ret: Box::new(self.conv_expr(*ret)),
            },
            E::BinOp { op, ty, l, r } => Expr::BinOp {
                ty: conv_ty(ty),
                name: op,
                l: Box::new(self.conv_expr(*l)),
                r: Box::new(self.conv_expr(*r)),
            },
            E::Fun {
                param_ty,
                param,
                body_ty,
                body,
            } => Expr::Fun {
                param: (conv_ty(param_ty), param),
                body_ty: conv_ty(body_ty),
                body: Box::new(self.conv_expr(*body)),
                captures: Vec::new(),
            },
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
                        Pattern::Lit {
                            value: Literal::Bool(true),
                        },
                        self.conv_expr(*then),
                    ),
                    (
                        Pattern::Lit {
                            value: Literal::Bool(false),
                        },
                        self.conv_expr(*else_),
                    ),
                ],
            },
            E::Tuple { ty, tuple } => Expr::Tuple {
                tys: force_tuple(ty.force("internal typing error")),
                tuple: tuple.into_iter().map(|e| self.conv_expr(e)).collect(),
            },
            E::Sym { ty, name } => Expr::Sym {
                ty: conv_ty(ty),
                name: name,
            },
            E::Lit { ty, value } => Expr::Lit {
                ty: conv_ty(ty),
                value: value,
            },
        }
    }
}

impl<E> Pass<ast::AST, E> for AST2HIR {
    type Target = HIR;

    fn trans(&mut self, ast: ast::AST) -> ::std::result::Result<Self::Target, E> {
        Ok(self.conv_ast(ast))
    }
}
