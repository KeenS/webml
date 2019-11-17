use std::io;

use crate::hir::*;
use crate::util::PP;

impl PP for HIR {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for bind in &self.0 {
            bind.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}

impl PP for Val {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
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
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::hir::Expr::*;
        match self {
            Binds { binds, ret, .. } => {
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
            BinOp { name, l, r, .. } => {
                l.pp(w, indent)?;
                write!(w, " ")?;
                name.pp(w, indent)?;
                write!(w, " ")?;
                r.pp(w, indent)?;
            }
            Fun {
                body,
                param,
                captures,
                ..
            } => {
                write!(w, "fun(")?;
                inter_iter! {
                    captures,
                    write!(w, ", ")?,
                    |(_,  cap)| => {
                        cap.pp(w, indent)?
                    }
                }
                write!(w, ") ")?;
                param.1.pp(w, indent)?;
                write!(w, " => ")?;
                body.pp(w, indent + 4)?;
            }
            Closure { envs, fname, .. } => {
                write!(w, "<closure ")?;
                fname.pp(w, indent)?;
                write!(w, " (")?;
                inter_iter! {
                    envs.iter(),
                    write!(w, ", ")?,
                    |(_,  var)| => {
                        var.pp(w, indent)?
                    }
                }
                write!(w, ")>")?;
            }
            App { fun, arg, .. } => {
                write!(w, "(")?;
                fun.pp(w, indent)?;
                write!(w, ") ")?;
                arg.pp(w, indent + 4)?;
            }
            Case { expr, arms, .. } => {
                let ind = Self::nspaces(indent);
                write!(w, "case ")?;
                expr.pp(w, indent + 4)?;
                write!(w, " of")?;
                for (pat, arm) in arms {
                    write!(w, "\n{}", ind)?;
                    pat.pp(w, indent + 4)?;
                    write!(w, " => ")?;
                    arm.pp(w, indent + 4)?;
                }
            }
            Tuple { tuple, .. } => {
                write!(w, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(w, ", ")?,
                    |t| => {
                        t.pp(w, indent)?
                    }
                }
                write!(w, ")")?;
            }
            Proj { index, tuple, .. } => {
                write!(w, "#{} ", index)?;
                tuple.pp(w, indent + 4)?;
            }
            BuiltinCall { fun, arg, .. } => {
                fun.pp(w, indent)?;
                write!(w, " ")?;
                arg.pp(w, indent)?;
            }
            Constructor { name, .. } => {
                name.pp(w, indent)?;
            }
            Sym { name, .. } => {
                name.pp(w, indent)?;
            }
            Lit { value, .. } => {
                value.pp(w, indent)?;
            }
        }
        Ok(())
    }
}

impl PP for Pattern {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        match self {
            Pattern::Lit { value, .. } => value.pp(w, indent),
            Pattern::Constructor { name, .. } => name.pp(w, indent),
            Pattern::Tuple { tuple, .. } => {
                write!(w, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(w, ", ")?,
                    |t| => {
                        t.pp(w, indent)?
                    }
                }
                write!(w, ")")
            }
            Pattern::Var { name, .. } => name.pp(w, indent),
        }
    }
}

impl PP for HTy {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::hir::HTy::*;
        match self {
            Int => write!(w, "int")?,
            Real => write!(w, "real")?,
            Tuple(tys) => {
                write!(w, "(")?;
                inter_iter! {
                    tys.iter(),
                    write!(w, " * ")?,
                    |ty| => {
                        ty.pp(w, indent)?
                    }
                }
                write!(w, ")")?;
            }
            Fun(t1, t2) => {
                t1.pp(w, indent)?;
                write!(w, " -> ")?;
                t2.pp(w, indent)?;
            }
            Datatype(name) => name.pp(w, indent)?,
        }
        Ok(())
    }
}
