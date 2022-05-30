pub mod ast2hir;
pub mod constructor2enum;
pub mod flat_expr;
pub mod flat_let;
pub mod force_closure;
pub mod pp;
pub mod simplify;
pub mod unnest_func;
pub mod util;

pub use self::ast2hir::AST2HIR;
pub use self::constructor2enum::ConstructorToEnum;
pub use self::flat_expr::FlatExpr;
pub use self::flat_let::FlatLet;
pub use self::force_closure::ForceClosure;
pub use self::simplify::Simplify;
pub use self::unnest_func::UnnestFunc;
use std::collections::HashMap;

use crate::prim::*;

#[derive(Debug)]
pub struct Context(pub SymbolTable, pub HIR);

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
    Let {
        ty: HTy,
        bind: Box<Val>,
        ret: Box<Expr>,
    },
    BuiltinCall {
        ty: HTy,
        fun: BIF,
        args: Vec<Expr>,
    },
    ExternCall {
        ty: HTy,
        module: String,
        fun: String,
        args: Vec<Expr>,
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
    Proj {
        ty: HTy,
        /// 0-origin
        index: u32,
        tuple: Box<Expr>,
    },
    Constructor {
        ty: HTy,
        arg: Option<Box<Expr>>,
        descriminant: u32,
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
    Constant {
        value: i64,
        ty: HTy,
    },
    Char {
        value: u32,
        ty: HTy,
    },
    Constructor {
        descriminant: u32,
        arg: Option<(HTy, Symbol)>,
        ty: HTy,
    },
    Var {
        name: Symbol,
        ty: HTy,
    },
    Tuple {
        tys: Vec<HTy>,
        tuple: Vec<Symbol>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub types: HashMap<Symbol, TypeInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HTy {
    Char,
    Int,
    Real,
    Fun(Box<HTy>, Box<HTy>),
    Tuple(Vec<HTy>),
    Datatype(Symbol),
    // Datatype(Vec<(u32, Option<HTy>)>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    pub constructors: Vec<(u32, Option<HTy>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BIF {
    // specified versions
    AddInt,
    AddReal,
    SubInt,
    SubReal,
    MulInt,
    MulReal,
    DivInt,
    DivReal,
    ModInt,
    EqInt,
    EqReal,
    EqChar,
    NeqInt,
    NeqReal,
    NeqChar,
    GtInt,
    GtReal,
    GtChar,
    GeInt,
    GeReal,
    GeChar,
    LtInt,
    LtReal,
    LtChar,
    LeInt,
    LeReal,
    LeChar,
}
impl Expr {
    fn app1(self, ty: HTy, e: Expr) -> Expr {
        Expr::App {
            ty,
            fun: Box::new(self),
            arg: Box::new(e),
        }
    }

    pub fn is_normal(&self) -> bool {
        use Expr::*;
        match self {
            Let { .. } => false,
            BuiltinCall { .. } => false,
            ExternCall { .. } => false,
            Fun { .. } => true,
            Closure { .. } => true,
            App { .. } => false,
            Case { .. } => false,
            Tuple { tuple, .. } => tuple.iter().all(|e| e.is_normal()),
            Proj { .. } => false,
            Constructor { arg, .. } => arg.as_ref().map(|e| e.is_normal()).unwrap_or(true),
            Sym { .. } => true,
            Lit { .. } => true,
        }
    }

    pub fn ty(&self) -> HTy {
        use crate::hir::Expr::*;

        match self {
            Closure {
                param_ty, body_ty, ..
            }
            | Fun {
                param: (param_ty, _),
                body_ty,
                ..
            } => HTy::fun(param_ty.clone(), body_ty.clone()),
            Tuple { tys, .. } => HTy::Tuple(tys.clone()),
            Proj { ty, .. }
            | Let { ty, .. }
            | BuiltinCall { ty, .. }
            | ExternCall { ty, .. }
            | App { ty, .. }
            | Case { ty, .. }
            | Constructor { ty, .. }
            | Sym { ty, .. }
            | Lit { ty, .. } => ty.clone(),
        }
    }
}

impl Pattern {
    pub fn match_key(&self) -> u32 {
        use self::Pattern::*;
        // FIXME do not panic
        match self {
            Constant { value, .. } => *value as u32,
            Char { value, .. } => *value,
            Tuple { .. } => panic!("bug: non-variant expression does not have keys"),
            Constructor { descriminant, .. } => *descriminant as u32,
            Var { .. } => panic!("bug: default like branch does not have keys"),
        }
    }

    pub fn binds(&self) -> Option<Symbol> {
        use self::Pattern::*;
        // FIXME do not panic
        match self {
            Constant { .. } | Char { .. } => None,
            Tuple { .. } => panic!("bug: non-variant expression does not have keys"),
            Constructor { arg, .. } => arg.as_ref().map(|(_, name)| name.clone()),
            Var { name, .. } => Some(name.clone()),
        }
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Pattern::Var { .. })
    }

    pub fn is_irrefutable(&self) -> bool {
        use self::Pattern::*;
        match *self {
            Constructor { .. } | Constant { .. } | Char { .. } => false,
            Tuple { .. } | Var { .. } => true,
        }
    }
}

impl HTy {
    pub fn fun(arg: HTy, ret: HTy) -> HTy {
        HTy::Fun(Box::new(arg), Box::new(ret))
    }
}
