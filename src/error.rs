use std::fmt;

#[derive(Debug, Clone)]
pub enum Error {
    Parser(String),
    Typing(crate::ast::TypeError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::*;
        match self {
            Parser(s) => writeln!(f, "{s}"),
            Typing(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use Error::*;
        match self {
            Parser(_) => None,
            Typing(e) => Some(e),
        }
    }
}
