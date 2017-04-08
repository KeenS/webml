pub mod ast2hir;
pub mod pp;
pub mod flat_let;
pub mod rename;
pub mod unnest_func;
pub mod flat_expr;

pub use self::ast2hir::AST2HIR;
pub use self::rename::Rename;
pub use self::flat_let::FlatLet;
pub use self::flat_expr::FlatExpr;
pub use self::unnest_func::UnnestFunc;

use prim::*;

#[derive(Debug)]
pub struct HIR(pub Vec<Val>);

#[derive(Debug)]
pub struct Val {
    pub ty: Ty,
    pub rec: bool,
    pub name: Symbol,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Expr {
    Binds {
        ty: Ty,
        binds: Vec<Val>,
        ret: Box<Expr>,
    },
    Op {
        ty: Ty,
        name: Symbol,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    PrimFun {
        param_ty: Ty,
        ret_ty: Ty,
        name: Symbol,
    },
    Fun {
        param: (Ty, Symbol),
        body_ty: Ty,
        body: Box<Expr>,
        captures: Vec<(Ty, Symbol)>,
    },
    Closure {
        envs: Vec<(Ty, Symbol)>,
        param_ty: Ty,
        body_ty: Ty,
        fname: Symbol,
    },
    App {
        ty: Ty,
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    If {
        ty: Ty,
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym { ty: Ty, name: Symbol },
    Lit { ty: Ty, value: Literal },
}

impl Expr {
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
            &Closure {
                 ref param_ty,
                 ref body_ty,
                 ..
             } |
            &PrimFun {
                 ref param_ty,
                 ret_ty: ref body_ty,
                 ..
             } |
            &Fun {
                 param: (ref param_ty, _),
                 ref body_ty,
                 ..
             } => Ty::fun(param_ty.clone(), body_ty.clone()),
            &Op { ref ty, .. } |
            &Binds { ref ty, .. } |
            &App { ref ty, .. } |
            &If { ref ty, .. } |
            &Sym { ref ty, .. } |
            &Lit { ref ty, .. } => ty.clone(),
        }
    }
}
