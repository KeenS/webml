pub mod pp;
pub mod flat_let;
pub mod alpha_conv;
pub mod closure_conv;
pub mod flat_expr;

pub use self::alpha_conv::AlphaConv;
pub use self::flat_let::FlatLet;
pub use self::flat_expr::FlatExpr;
pub use self::closure_conv::ClosureConv;

use ast;
use prim::*;
use pass::Pass;

#[derive(Debug)]
pub struct HIR(pub Vec<Val>);

#[derive(Debug)]
pub struct Val{pub ty: Ty, pub rec: bool, pub name: Symbol, pub expr: Expr}

#[derive(Debug)]
pub enum Expr {
    Binds{ty: Ty, binds: Vec<Val>, ret: Box<Expr>},
    PrimFun{ty: Ty, name: Symbol},
    Fun{param: (Ty, Symbol), body_ty: Ty, body: Box<Expr>, captures: Vec<(Ty, Symbol)>},
    Closure{envs: Vec<(Ty, Symbol)>, param_ty: Ty, body_ty: Ty, fname: Symbol},
    App{ty: Ty, fun: Box<Expr>, arg: Box<Expr>},
    If {ty: Ty, cond: Box<Expr>, then: Box<Expr>, else_: Box<Expr>},
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym{ty: Ty, name: Symbol},
    Lit{ty: Ty, value: Literal},
}

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

    pub fn ty(&self) -> Ty {
        use hir::Expr::*;

        match self {
            &Closure{ref param_ty, ref body_ty, ..} |
            &Fun{param: (ref param_ty, _), ref body_ty, ..} =>
                Ty::Fun(Box::new(param_ty.clone()),
                        Box::new(body_ty.clone())),
            &Binds{ref ty, ..} |
            &PrimFun{ref ty, ..} |
            &App{ref ty, ..} |
            &If {ref ty, ..} |
            &Sym{ref ty, ..} |
            &Lit{ref ty, ..} => ty.clone()
        }
    }
}

pub struct AST2HIR;

impl AST2HIR {
    fn conv_ast(&self, ast: ast::AST) -> HIR {
        HIR(ast.0.into_iter().map(|val| self.conv_val(val)).collect())
    }

    fn conv_val(&self, val: ast::Val) -> Val {
        Val {
            ty: val.ty.force("internal typing error"),
            rec: val.rec,
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
                    binds: binds.into_iter().map(|b| self.conv_val(b)).collect(),
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
            E::Fun{param_ty, param, body_ty, body} =>
                Expr::Fun {
                    param: (param_ty.defined().expect("internal typing error"), param),
                    body_ty: body_ty.defined().expect("internal typing error"),
                    body: Box::new(self.conv_expr(*body)),
                    captures: Vec::new(),

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
}

impl Pass<ast::AST> for AST2HIR {
    type Target = HIR;
    type Err = TypeError;

    fn trans(&mut self, ast: ast::AST) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.conv_ast(ast))
    }
}
