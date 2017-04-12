use std::io;
use util::PP;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String);

impl Symbol {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Symbol(s.into())
    }
}

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
