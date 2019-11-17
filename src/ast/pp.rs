use crate::ast::*;
use crate::util::PP;
use std::io;

impl<Ty> PP for AST<Ty> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for bind in &self.0 {
            bind.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}

impl<Ty> PP for Statement<Ty> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use Statement::*;
        match self {
            Fun {
                name, expr, params, ..
            } => {
                write!(w, "{}", Self::nspaces(indent))?;
                write!(w, "fun ")?;
                name.pp(w, indent)?;
                write!(w, " ")?;
                for (_, param) in params {
                    param.pp(w, indent)?;
                    write!(w, " ")?;
                }
                // write!(w, ": ")?;
                // self.ty.pp(w, indent)?;
                write!(w, " = ")?;
                expr.pp(w, indent + 4)?;
                Ok(())
            }
            Val { pattern, expr } => {
                write!(w, "{}", Self::nspaces(indent))?;
                write!(w, "val ")?;
                pattern.pp(w, indent)?;
                // write!(w, ": ")?;
                // self.ty.pp(w, indent)?;
                write!(w, " = ")?;
                expr.pp(w, indent + 4)?;
                Ok(())
            }
        }
    }
}

impl<Ty> PP for Expr<Ty> {
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
            &Fn {
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
            &Symbol { ref name, .. } => {
                name.pp(w, indent)?;
            }
            &Literal { ref value, .. } => {
                value.pp(w, indent)?;
            }
        }
        Ok(())
    }
}

impl<Ty> PP for Pattern<Ty> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        match self {
            Pattern::Literal { ref value, .. } => value.pp(w, indent),
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
            Pattern::Variable { ref name, .. } => name.pp(w, indent),
            Pattern::Wildcard { .. } => write!(w, "_"),
        }
    }
}

impl PP for Type {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use self::Type::*;
        match *self {
            Variable(id) => write!(w, "'{}", id)?,
            Bool => write!(w, "bool")?,
            Int => write!(w, "int")?,
            Real => write!(w, "float")?,
            Fun(ref t1, ref t2) => {
                t1.pp(w, indent)?;
                write!(w, " -> ")?;
                t2.pp(w, indent)?;
            }
            Tuple(ref tys) => {
                write!(w, "(")?;
                for ty in tys.iter() {
                    ty.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
        }
        Ok(())
    }
}
