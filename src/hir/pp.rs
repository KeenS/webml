use std::io;

use util::PP;
use hir::*;

impl PP for HIR {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        for bind in &self.0 {
            bind.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}



impl PP for Val {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        let rec = if self.rec { "rec " } else { "" };
        write!(w, "{}val{} ", Self::nspaces(indent), rec)?;
        self.name.pp(w, indent)?;
        write!(w, ": ")?;
        self.ty.pp(w, indent)?;
        write!(w, " = ")?;
        self.expr.pp(w, indent + 4)?;
        Ok(())
    }
}


impl PP for Expr {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use hir::Expr::*;
        match self {
            &Binds { ref binds, ref ret, .. } => {
                let ind = Self::nspaces(indent);
                let nextind = Self::nspaces(indent + 4);
                write!(w, "let\n")?;
                for val in binds {
                    val.pp(w, indent + 4)?;
                    write!(w, "\n")?;
                }
                write!(w, "{}in\n{}", ind, nextind)?;
                ret.pp(w, indent + 4)?;
                write!(w, "\n{}end", ind)?;
            }
            &Op {
                ref name,
                ref l,
                ref r,
                ..
            } => {
                l.pp(w, indent)?;
                write!(w, " ")?;
                name.pp(w, indent)?;
                write!(w, " ")?;
                r.pp(w, indent)?;
            }
            &Fun {
                ref body,
                ref param,
                ref captures,
                ..
            } => {
                write!(w, "fun (")?;
                for &(_, ref cap) in captures {
                    cap.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ") ")?;
                param.1.pp(w, indent)?;
                write!(w, " => ")?;
                body.pp(&mut w, indent + 4)?;
            }
            &Closure {
                ref envs,
                ref fname,
                ..
            } => {
                write!(w, "<closure ")?;
                fname.pp(w, indent)?;
                write!(w, " (")?;
                for &(_, ref var) in envs.iter() {
                    var.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ")>")?;
            }
            &App { ref fun, ref arg, .. } => {
                write!(w, "(")?;
                fun.pp(w, indent)?;
                write!(w, ") ")?;
                arg.pp(w, indent + 4)?;
            }
            &If {
                ref cond,
                ref then,
                ref else_,
                ..
            } => {
                let ind = Self::nspaces(indent);
                write!(w, "if ")?;
                cond.pp(w, indent + 4)?;
                write!(w, "\n{}then ", ind)?;
                then.pp(w, indent + 4)?;
                write!(w, "\n{}else ", ind)?;
                else_.pp(w, indent + 4)?;
            }
            &Tuple { ref tuple, .. } => {
                write!(w, "(")?;
                for t in tuple.iter() {
                    t.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
            &PrimFun { ref name, .. } => {
                name.pp(w, indent)?;
            }
            &Sym { ref name, .. } => {
                name.pp(w, indent)?;
            }
            &Lit { ref value, .. } => {
                value.pp(w, indent)?;
            }
        }
        Ok(())
    }
}

impl PP for HTy {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use hir::HTy::*;
        match *self {
            Unit => write!(w, "()")?,
            Bool => write!(w, "bool")?,
            Int => write!(w, "int")?,
            Float => write!(w, "float")?,
            Tuple(ref tys) => {
                write!(w, "(")?;
                for ty in tys.iter() {
                    ty.pp(w, indent)?;
                    write!(w, " * ")?;
                }
                write!(w, ")")?;
            }
            Fun(ref t1, ref t2) => {
                t1.pp(w, indent)?;
                write!(w, " -> ")?;
                t2.pp(w, indent)?;
            }
        }
        Ok(())
    }
}
