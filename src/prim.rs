use crate::util::PP;
use std::io;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String, pub u64);

impl Symbol {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Symbol(s.into(), 0)
    }
}

impl PP for Symbol {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        write!(w, "{}@{}", self.0, self.1)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Real(f64),
    Bool(bool),
}

impl PP for Literal {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        use self::Literal::*;
        match self {
            &Int(ref v) => {
                write!(w, "{}", v)?;
            }
            &Real(ref v) => {
                write!(w, "{}", v)?;
            }
            &Bool(ref v) => {
                write!(w, "{}", v)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum BIF {
    Print,
}

impl PP for BIF {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        use self::BIF::*;
        match self {
            &Print => {
                write!(w, "_print")?;
            }
        }
        Ok(())
    }
}
