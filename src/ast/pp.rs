use std::io;

use util::PP;
use ast::*;

impl PP for Ty {
    fn pp(&self, w: &mut io::Write, indent: usize) -> io::Result<()> {
        use self::Ty::*;
        match *self {
            Unit => write!(w, "()")?,
            Bool => write!(w, "bool")?,
            Int => write!(w, "int")?,
            Float => write!(w, "float")?,
            Fun(ref t1, ref t2) => {
                t1.clone().force("type not settled in pp").pp(w, indent)?;
                write!(w, " -> ")?;
                t2.clone().force("type not settled in pp").pp(w, indent)?;
            }
            Tuple(ref tys) => {
                write!(w, "(")?;
                for ty in tys.iter() {
                    ty.clone().force("type not settled in pp").pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
        }
        Ok(())
    }
}
