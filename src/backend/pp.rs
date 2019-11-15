use crate::util::PP;
use std::io;
use wasm::Module;

impl PP for Module {
    fn pp<W: io::Write>(&self, w: &mut W, _: usize) -> io::Result<()> {
        writeln!(w, "{:#?}", self)?;
        Ok(())
    }
}
