use crate::ast::*;
use crate::util::PP;
use std::io;

impl PP for AST {
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
        write!(w, "{}", Self::nspaces(indent))?;
        if self.rec {
            write!(w, "fun ")?;
        } else {
            write!(w, "val ")?;
        };
        self.pattern.pp(w, indent)?;
        // write!(w, ": ")?;
        // self.ty.pp(w, indent)?;
        write!(w, " = ")?;
        self.expr.pp(w, indent + 4)?;
        Ok(())
    }
}

impl PP for Expr {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::ast::Expr::*;
        match self {
            &Binds {
                ref binds, ref ret, ..
            } => {
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
            &BinOp {
                ref op,
                ref l,
                ref r,
                ..
            } => {
                l.pp(w, indent)?;
                write!(w, " ")?;
                op.pp(w, indent)?;
                write!(w, " ")?;
                r.pp(w, indent)?;
            }
            &Fun {
                ref body,
                ref param,
                ..
            } => {
                write!(w, "fn ")?;
                param.pp(w, indent)?;
                write!(w, " => ")?;
                body.pp(w, indent + 4)?;
            }
            &App {
                ref fun, ref arg, ..
            } => {
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

            &Case {
                ref cond,
                ref clauses,
                ..
            } => {
                let ind = Self::nspaces(indent);
                write!(w, "case ")?;
                cond.pp(w, indent + 4)?;
                write!(w, " of")?;
                for &(ref pat, ref arm) in clauses {
                    write!(w, "\n{}", ind)?;
                    pat.pp(w, indent + 4)?;
                    write!(w, " => ")?;
                    arm.pp(w, indent + 4)?;
                }
            }
            &Tuple { ref tuple, .. } => {
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

impl PP for Pattern {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        match self {
            Pattern::Lit { ref value, .. } => value.pp(w, indent),
            Pattern::Tuple { ref tuple, .. } => {
                write!(w, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(w, ", ")?,
                    |&(_, ref sym)| => {
                        sym.pp(w, indent)?
                    }
                }
                write!(w, ")")
            }
            Pattern::Var { ref name, .. } => name.pp(w, indent),
            Pattern::Wildcard { .. } => write!(w, "_"),
        }
    }
}

impl PP for Ty {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use self::Ty::*;
        match *self {
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
