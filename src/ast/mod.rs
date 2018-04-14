pub mod typing;
pub mod case_check;
mod pp;
mod util;

use nom;

pub use self::typing::TyEnv;
pub use self::case_check::CaseCheck;

use std::ops::Deref;
use std::error::Error;
use std::fmt;
use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

use ast;
use prim::*;

#[derive(Debug, Clone, PartialEq)]
pub struct AST(pub Vec<Val>);

#[derive(Debug, Clone, PartialEq)]
pub struct Val {
    pub ty: TyDefer,
    pub rec: bool,
    pub name: Symbol,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binds {
        ty: TyDefer,
        binds: Vec<Val>,
        ret: Box<Expr>,
    },
    BinOp {
        op: Symbol,
        ty: TyDefer,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    Fun {
        param_ty: TyDefer,
        param: Symbol,
        body_ty: TyDefer,
        body: Box<Expr>,
    },
    App {
        ty: TyDefer,
        fun: Box<Expr>,
        arg: Box<Expr>,
    },
    If {
        ty: TyDefer,
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    Case {
        ty: TyDefer,
        cond: Box<Expr>,
        clauses: Vec<(Pattern, Expr)>,
    },
    Tuple {
        ty: TyDefer,
        tuple: Vec<Expr>,
    },
    Sym {
        ty: TyDefer,
        name: Symbol,
    },
    Lit {
        ty: TyDefer,
        value: Literal,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Lit { value: Literal, ty: TyDefer },
    Var { name: Symbol, ty: TyDefer },
    Wildcard { ty: TyDefer },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Bool,
    Int,
    Float,
    Fun(TyDefer, TyDefer),
    Tuple(Vec<TyDefer>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyDefer(pub Rc<RefCell<Option<Ty>>>);

impl Expr {
    fn ty_defer(&self) -> TyDefer {
        use self::Expr::*;
        match *self {
            Binds { ref ty, .. }
            | BinOp { ref ty, .. }
            | App { ref ty, .. }
            | If { ref ty, .. }
            | Case { ref ty, .. }
            | Tuple { ref ty, .. }
            | Sym { ref ty, .. }
            | Lit { ref ty, .. } => ty.clone(),
            Fun {
                ref param_ty,
                ref body_ty,
                ..
            } => TyDefer::new(Some(Ty::Fun(param_ty.clone(), body_ty.clone()))),
        }
    }
}

impl Pattern {
    fn ty_defer(&self) -> TyDefer {
        use self::Pattern::*;
        match *self {
            Lit { ref ty, .. } | Var { ref ty, .. } | Wildcard { ref ty } => ty.clone(),
        }
    }
    fn binds(&self) -> Vec<(&Symbol, &TyDefer)> {
        use self::Pattern::*;
        match *self {
            Lit { .. } | Wildcard { .. } => vec![],
            Var { ref name, ref ty } => vec![(name, ty)],
        }
    }
}

impl Ty {
    pub fn fun(param: Ty, ret: Ty) -> Ty {
        Ty::Fun(
            TyDefer(Rc::new(RefCell::new(Some(param)))),
            TyDefer(Rc::new(RefCell::new(Some(ret)))),
        )
    }
    pub fn unit() -> Ty {
        Ty::Tuple(Vec::new())
    }
}

impl TyDefer {
    pub fn get_mut(&mut self) -> RefMut<Option<Ty>> {
        self.0.borrow_mut()
    }

    pub fn get(&self) -> Ref<Option<Ty>> {
        self.0.borrow()
    }

    pub fn new(t: Option<Ty>) -> Self {
        TyDefer(Rc::new(RefCell::new(t)))
    }

    pub fn empty() -> Self {
        Self::new(None)
    }

    pub fn defined(&self) -> Option<Ty> {
        self.0.deref().clone().into_inner()
    }

    pub fn force(self, message: &str) -> Ty {
        self.0.deref().clone().into_inner().expect(message)
    }
}

#[derive(Debug)]
pub enum TypeError<'a> {
    MisMatch { expected: Ty, actual: Ty },
    CannotInfer,
    FreeVar,
    NotFunction(ast::Expr),
    ParseError(nom::Err<&'a str>),
}

impl<'a> fmt::Display for TypeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl<'a> Error for TypeError<'a> {
    fn description(&self) -> &str {
        use self::TypeError::*;
        match self {
            &MisMatch { .. } => "type mismatches against expected type",
            &CannotInfer => "cannot infer the type",
            &FreeVar => "free variable is found",
            &NotFunction(_) => "not a function",
            &ParseError(_) => "parse error",
        }
    }
}

impl<'a> From<nom::Err<&'a str>> for TypeError<'a> {
    fn from(e: nom::Err<&'a str>) -> Self {
        // fn conv<'b>(e: nom::Err<&'b [u8]>) -> nom::Err<&'b str> {
        //     use std::str::from_utf8;
        //     use nom::Err::*;
        //     match e {
        //         Code(e) => Code(e),
        //         Node(kind, box_err) => Node(kind, Box::new(conv(*box_err))),
        //         Position(kind, slice) => Position(kind, from_utf8(slice).unwrap()),
        //         NodePosition(kind, slice, box_err) => {
        //             NodePosition(kind, from_utf8(slice).unwrap(), Box::new(conv(*box_err)))
        //         }
        //     }
        // }

        TypeError::ParseError(e)
    }
}

pub type Result<'a, T> = ::std::result::Result<T, TypeError<'a>>;
