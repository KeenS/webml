use crate::backend::Output;
use std::fmt;

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:#?}", self.0)?;
        Ok(())
    }
}
