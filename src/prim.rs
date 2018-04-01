use std::io;
use util::PP;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String, pub u64);

impl Symbol {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Symbol(s.into(), 0)
    }
}

impl PP for Symbol {
    fn pp(&self, w: &mut io::Write, _indent: usize) -> io::Result<()> {
        write!(w, "{}@{}", self.0, self.1)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl PP for Literal {
    fn pp(&self, w: &mut io::Write, _indent: usize) -> io::Result<()> {
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

#[derive(Debug, Clone)]
pub enum BIF {
    Print,
}

impl PP for BIF {
    fn pp(&self, w: &mut io::Write, _indent: usize) -> io::Result<()> {
        use self::BIF::*;
        match self {
            &Print => {
                write!(w, "_print")?;
            }
        }
        Ok(())
    }
}
