use nom;

use std::ops::Deref;
use std::error::Error;
use std::fmt;
use std::io;
use std::cell::{RefCell, Ref, RefMut};
use std::rc::Rc;
use ast;
use util::PP;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Bool,
    Int,
    Float,
    Fun(TyDefer, TyDefer),
}


impl Ty {
    pub fn fun(param: Ty, ret: Ty) -> Ty {
        Ty::Fun(TyDefer(Rc::new(RefCell::new(Some(param)))),
                TyDefer(Rc::new(RefCell::new(Some(ret)))))
    }
}

impl PP for Ty {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use self::Ty::*;
        match *self {
            Unit => write!(w, "()")?,
            Bool => write!(w, "bool")?,
            Int => write!(w, "int")?,
            Float => write!(w, "float")?,
            Fun(ref t1, ref t2) => {
                t1.clone()
                    .force("type not settled in pp")
                    .pp(w, indent)?;
                write!(w, " -> ")?;
                t2.clone()
                    .force("type not settled in pp")
                    .pp(w, indent)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub struct TyDefer(pub Rc<RefCell<Option<Ty>>>);

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


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String);

impl PP for Symbol {
    fn pp(&self, mut w: &mut io::Write, _indent: usize) -> io::Result<()> {
        write!(w, "{}", self.0)?;
        Ok(())
    }
}


#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl PP for Literal {
    fn pp(&self, mut w: &mut io::Write, _indent: usize) -> io::Result<()> {
        use self::Literal::*;
        match self {
            &Int(ref v) => {
                write!(w, "{}", v)?;

            }
            &Float(ref v) => {
                write!(w, "{}", v)?;

            }
            &Bool(ref v) => {
                write!(w, "{}", v)?;
            }
        }
        Ok(())
    }
}
