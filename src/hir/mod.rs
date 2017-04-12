pub mod ast2hir;
pub mod pp;
pub mod flat_let;
pub mod rename;
pub mod unnest_func;
pub mod flat_expr;
pub mod force_closure;
pub mod util;

pub use self::ast2hir::AST2HIR;
pub use self::rename::Rename;
pub use self::flat_let::FlatLet;
pub use self::flat_expr::FlatExpr;
pub use self::unnest_func::UnnestFunc;
pub use self::force_closure::ForceClosure;

use prim::*;

#[derive(Debug)]
pub struct HIR(pub Vec<Val>);

#[derive(Debug, Clone)]
pub struct Val {
    pub ty: HTy,
    pub rec: bool,
    pub name: Symbol,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binds {
        ty: HTy,
        binds: Vec<Val>,
        ret: Box<Expr>,
    },
    Op {
        ty: HTy,
        name: Symbol,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    PrimFun {
        param_ty: HTy,
        ret_ty: HTy,
        name: Symbol,
    },
    Fun {
        param: (HTy, Symbol),
        body_ty: HTy,
        body: Box<Expr>,
        captures: Vec<(HTy, Symbol)>,
        make_closure: Option<bool>,
    },
    Closure {
        envs: Vec<(HTy, Symbol)>,
        param_ty: HTy,
        body_ty: HTy,
        fname: Symbol,
    },
    App {
        ty: HTy,
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    If {
        ty: HTy,
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym { ty: HTy, name: Symbol },
    Lit { ty: HTy, value: Literal },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HTy {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Box<HTy>, Box<HTy>),
}


impl Expr {
    fn app1(self, ty: HTy, e: Expr) -> Expr {
        Expr::App {
            ty: ty,
            fun: Box::new(self),
            arg: Box::new(e),
        }
    }

    pub fn ty(&self) -> HTy {
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
             } => HTy::fun(param_ty.clone(), body_ty.clone()),
            &Op { ref ty, .. } |
            &Binds { ref ty, .. } |
            &App { ref ty, .. } |
            &If { ref ty, .. } |
            &Sym { ref ty, .. } |
            &Lit { ref ty, .. } => ty.clone(),
        }
    }
}

impl HTy {
    pub fn fun(arg: HTy, ret: HTy) -> HTy {
        HTy::Fun(Box::new(arg), Box::new(ret))
    }
}
