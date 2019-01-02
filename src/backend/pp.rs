use crate::util::PP;
use std::io;
use wasm::Module;

impl PP for Module {
    fn pp(&self, w: &mut io::Write, _: usize) -> io::Result<()> {
        writeln!(w, "{:#?}", self)?;
        Ok(())
    }
}
