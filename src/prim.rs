use crate::util::PP;
use std::fmt;
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

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}@{}", self.0, self.1)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Real(f64),
    Char(u32),
}

impl PP for Literal {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        use self::Literal::*;
        match self {
            Int(v) => {
                write!(w, "{}", v)?;
            }
            Real(v) => {
                write!(w, "{}", v)?;
            }
            Char(c) => {
                write!(w, r##"#"{}""##, c)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Literal::*;
        match self {
            Int(v) => {
                write!(f, "{}", v)?;
            }
            Real(v) => {
                write!(f, "{}", v)?;
            }
            Char(c) => {
                write!(f, r##"#"{}""##, c)?;
            }
        }
        Ok(())
    }
}
