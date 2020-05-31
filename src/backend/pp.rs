use crate::backend::Output;
use crate::util::PP;
use std::fmt;
use std::io;

impl PP for Output {
    fn pp<W: io::Write>(&self, w: &mut W, _: usize) -> io::Result<()> {
        writeln!(w, "{:#?}", self.0)?;
        Ok(())
    }
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:#?}", self.0)?;
        Ok(())
    }
}
