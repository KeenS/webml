use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(pub String, pub u64);

impl Symbol {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Symbol(s.into(), 0)
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
