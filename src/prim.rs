use nom;

use std::ops::{Deref, DerefMut};
use std::error::Error;
use std::fmt;
use std::io;

use ast;
use util::PP;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Bool,
    Int,
    Fun(Box<Ty>, Box<Ty>),
}


impl Ty {
    pub fn fun(param: Ty, ret: Ty) -> Ty {
        Ty::Fun(Box::new(param), Box::new(ret))
    }
}

impl PP for Ty {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()>{
        use self::Ty::*;
        match self {
            &Unit => write!(w, "()")?,
            &Bool => write!(w, "bool")?,
            &Int  => write!(w, "int")?,
            &Fun(ref t1, ref t2) => {
                t1.pp(w, indent)?;
                write!(w, " -> ")?;
                t2.pp(w, indent)?;
            },
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub struct TyDefer(pub Option<Ty>);

impl Deref for TyDefer {
    type Target = Option<Ty>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TyDefer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl TyDefer {
    pub fn empty() -> Self {
        TyDefer(None)
    }

    pub fn defined(&self) -> Option<Ty> {
        self.clone().0
    }

    pub fn force(self, message: &str) -> Ty {
        self.0.expect(message)
    }
}


#[derive(Debug)]
pub enum TypeError {
    MisMatch{expected: Ty, actual: Ty},
    CannotInfer,
    FreeVar,
    NotFunction(ast::Expr),
    ParseError(nom::ErrorKind),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Error for TypeError {
    fn description(&self) -> &str {
        use self::TypeError::*;
        match self {
            &MisMatch{..} => "type mismatches against expected type",
            &CannotInfer => "cannot infer the type",
            &FreeVar => "free variable is found",
            &NotFunction(_) => "not a function",
            &ParseError(_) => "parse error"
        }
    }
}


impl From<nom::ErrorKind> for TypeError {
    fn from(e: nom::ErrorKind) -> TypeError {
        TypeError::ParseError(e)
    }
}

pub type Result<T> = ::std::result::Result<T, TypeError>;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String);

impl PP for Symbol {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()>{
        write!(w, "{}", self.0)?;
        Ok(())
    }
}


#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}

impl PP for Literal {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()>{
        use self::Literal::*;
        match self {
            &Int(ref v) => {
                write!(w, "{}", v)?;

            },
            &Bool(ref v) => {
                write!(w, "{}", v)?;
            }
        }
        Ok(())
    }
}
