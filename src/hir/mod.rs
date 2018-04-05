pub mod ast2hir;
pub mod pp;
pub mod flat_let;
pub mod rename;
pub mod unnest_func;
pub mod flat_expr;
pub mod force_closure;
pub mod find_builtin;
pub mod util;

pub use self::ast2hir::AST2HIR;
pub use self::rename::Rename;
pub use self::find_builtin::FindBuiltin;
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
    BinOp {
        ty: HTy,
        name: Symbol,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    BuiltinCall {
        ty: HTy,
        fun: BIF,
        arg: Box<Expr>,
    },
    Fun {
        param: (HTy, Symbol),
        body_ty: HTy,
        body: Box<Expr>,
        captures: Vec<(HTy, Symbol)>,
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
    Case {
        ty: HTy,
        expr: Box<Expr>,
        arms: Vec<(Pattern, Expr)>,
    },
    Tuple {
        tys: Vec<HTy>,
        tuple: Vec<Expr>,
    },
    Sym {
        ty: HTy,
        name: Symbol,
    },
    Lit {
        ty: HTy,
        value: Literal,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Lit { value: Literal },
}

impl Pattern {
    fn symbols_mut(&mut self) -> Vec<&mut Symbol> {
        vec![]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HTy {
    Unit,
    Bool,
    Int,
    Float,
    Tuple(Vec<HTy>),
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
            }
            | &Fun {
                param: (ref param_ty, _),
                ref body_ty,
                ..
            } => HTy::fun(param_ty.clone(), body_ty.clone()),
            &Tuple { ref tys, .. } => HTy::Tuple(tys.clone()),
            &BinOp { ref ty, .. }
            | &Binds { ref ty, .. }
            | &BuiltinCall { ref ty, .. }
            | &App { ref ty, .. }
            | &Case { ref ty, .. }
            | &Sym { ref ty, .. }
            | &Lit { ref ty, .. } => ty.clone(),
        }
    }
}

impl HTy {
    pub fn fun(arg: HTy, ret: HTy) -> HTy {
        HTy::Fun(Box::new(arg), Box::new(ret))
    }
}
