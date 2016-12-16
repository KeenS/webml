pub mod flat_let;
pub mod alpha_conv;

pub use self::alpha_conv::AlphaConv;
pub use self::flat_let::FlatLet;

use ast;
use prim::*;
use pass::Pass;

#[derive(Debug)]
pub struct HIR(pub Vec<Val>);

#[derive(Debug)]
pub enum Expr {
    Binds{ty: Ty, binds: Vec<Val>, ret: Box<Expr>},
    PrimFun{ty: Ty, name: Symbol},
    Fun{ty: Ty, param: Symbol, body: Box<Expr>},
    App{ty: Ty, fun: Box<Expr>, arg: Box<Expr>},
    If {ty: Ty, cond: Box<Expr>, then: Box<Expr>, else_: Box<Expr>},
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym{ty: Ty, name: Symbol},
    Lit{ty: Ty, value: Literal},
}

#[derive(Debug)]
pub struct Val{pub ty: Ty, pub name: Symbol, pub expr: Expr}

impl Expr {
    fn add() -> Expr {
        Expr::PrimFun {
            ty: Ty::fun(Ty::Int, Ty::fun(Ty::Int, Ty::Int)),
            name: Symbol("+".to_string())
        }
    }

    fn mul() -> Expr {
        Expr::PrimFun {
            ty: Ty::fun(Ty::Int, Ty::fun(Ty::Int, Ty::Int)),
            name: Symbol("*".to_string())
        }
    }

    fn app1(self, ty: Ty, e: Expr) -> Expr {
        Expr::App {
            ty: ty,
            fun: Box::new(self),
            arg: Box::new(e),
        }
    }
}

pub struct AST2HIR;

impl AST2HIR {
    fn conv_asts(&self, asts: Vec<ast::AST>) -> HIR {
        HIR(asts.into_iter().map(|ast| self.conv_ast(ast)).collect())
    }

    fn conv_ast(&self, ast: ast::AST) -> Val {
        match ast {
            ast::AST::Top(ast::Bind::V(v)) => self.conv_val(v)
        }
    }

    fn conv_val(&self, val: ast::Val) -> Val {
        Val {
            ty: val.ty.force("internal typing error"),
            name: val.name,
            expr: self.conv_expr(val.expr)
        }
    }

    fn conv_expr(&self, expr: ast::Expr) -> Expr {
        use ast::{Expr as E};
        match expr {
            E::Binds{ty, binds, ret} =>
                Expr::Binds {
                    ty: ty.force("internal typing error"),
                    binds: binds.into_iter().map(|b| self.conv_bind(b)).collect(),
                    ret: Box::new(self.conv_expr(*ret)),
                },
            E::Add{ty, l, r} =>
                Expr::add()
                .app1(Ty::fun(Ty::Int, Ty::Int),
                      self.conv_expr(*l))
                .app1(ty.force("internal typing error"), self.conv_expr(*r)),
            E::Mul{ty, l, r} =>
                Expr::mul()
                .app1(Ty::fun(Ty::Int, Ty::Int),
                      self.conv_expr(*l))
                .app1(ty.force("internal typing error"), self.conv_expr(*r)),
            E::Fun{ty, param, body} =>
                Expr::Fun {
                    ty: ty.defined().expect("internal typing error"),
                    param: param,
                    body: Box::new(self.conv_expr(*body)),

                },
            E::App{ty, fun, arg} =>
                self.conv_expr(*fun).app1(ty.force("internal typing error"), self.conv_expr(*arg)),
            E::If {ty, cond, then, else_} =>
                Expr::If {
                    ty: ty.force("internal typing error"),
                    cond: Box::new(self.conv_expr(*cond)),
                    then: Box::new(self.conv_expr(*then)),
                    else_: Box::new(self.conv_expr(*else_)),
                },
            E::Sym{ty, name} =>
                Expr::Sym{ty: ty.force("internal typing error"), name: name},
            E::Lit{ty, value} =>
                Expr::Lit {
                    ty: ty.force("internal typing error"),
                    value: value,
                }
        }
    }

    fn conv_bind(&self, b: ast::Bind) -> Val {
        match b {
            ast::Bind::V(val) =>  self.conv_val(val)
        }
    }
}

impl Pass<Vec<ast::AST>> for AST2HIR {
    type Target = HIR;
    type Err = TypeError;

    fn trans(&mut self, asts: Vec<ast::AST>) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.conv_asts(asts))
    }
}
