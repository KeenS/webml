use crate::ast::*;
use crate::util::PP;
use std::io;

impl<Ty: PP, D: PP> PP for (SymbolTable, AST<Ty, D>) {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.1.pp(w, indent)
    }
}

impl<Ty: PP, D: PP> PP for AST<Ty, D> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for bind in &self.0 {
            bind.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}

impl<Ty: PP, D: PP> PP for Statement<Ty, D> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use Statement::*;
        match self {
            Datatype { name, constructors } => {
                write!(w, "datatype ")?;
                name.pp(w, indent)?;
                write!(w, " =")?;
                inter_iter!(constructors, write!(w, " |")?, |(name, param)| =>{
                    write!(w, " ")?;
                    name.pp(w, indent)?;
                    if let Some(param) = param {
                        write!(w, " of ")?;
                        param.pp(w, indent)?;
                    }
                });
                Ok(())
            }
            Fun {
                name, expr, params, ..
            } => {
                write!(w, "{}", Self::nspaces(indent))?;
                write!(w, "fun ")?;
                name.pp(w, indent)?;
                write!(w, " ")?;
                for param in params {
                    param.pp(w, indent)?;
                    write!(w, " ")?;
                }
                // write!(w, ": ")?;
                // self.ty.pp(w, indent)?;
                write!(w, " = ")?;
                expr.pp(w, indent + 4)?;
                Ok(())
            }
            Val { pattern, expr, rec } => {
                write!(w, "{}", Self::nspaces(indent))?;
                write!(w, "val ")?;
                if *rec {
                    write!(w, "rec ")?;
                }
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

impl<Ty: PP, D: PP> PP for Expr<Ty, D> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::ast::Expr::*;
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
            BinOp { op, l, r, .. } => {
                l.pp(w, indent)?;
                write!(w, " ")?;
                op.pp(w, indent)?;
                write!(w, " ")?;
                r.pp(w, indent)?;
            }
            Fn { body, param, .. } => {
                write!(w, "fn ")?;
                param.pp(w, indent)?;
                write!(w, " => ")?;
                body.pp(w, indent + 4)?;
            }
            App { fun, arg, .. } => {
                write!(w, "(")?;
                fun.pp(w, indent)?;
                write!(w, ") ")?;
                arg.pp(w, indent + 4)?;
            }
            Case { cond, clauses, .. } => {
                let ind = Self::nspaces(indent);
                write!(w, "case ")?;
                cond.pp(w, indent + 4)?;
                write!(w, " of")?;
                for (pat, arm) in clauses {
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
            Symbol { name, .. } | Constructor { name, .. } => {
                name.pp(w, indent)?;
            }
            Literal { value, .. } => {
                value.pp(w, indent)?;
            }
            D(d) => {
                d.pp(w, indent)?;
            }
        }
        Ok(())
    }
}

impl<Ty: PP> PP for DerivedExpr<Ty> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use DerivedExpr::*;
        match self {
            If {
                cond, then, else_, ..
            } => {
                let ind = Self::nspaces(indent);
                write!(w, "if ")?;
                cond.pp(w, indent + 4)?;
                write!(w, "\n{}then ", ind)?;
                then.pp(w, indent + 4)?;
                write!(w, "\n{}else ", ind)?;
                else_.pp(w, indent + 4)?;
            }
        }
        Ok(())
    }
}

impl PP for Nothing {
    fn pp<W: io::Write>(&self, _: &mut W, _: usize) -> io::Result<()> {
        match *self {}
    }
}

impl<Ty> PP for Pattern<Ty> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        match self {
            Pattern::Constant { value, .. } => write!(w, "{}", value),
            Pattern::Constructor { name, arg, .. } => {
                name.pp(w, indent)?;
                if let Some(arg) = arg {
                    // TODO: handle cases when its in function args
                    write!(w, " ")?;
                    arg.pp(w, indent)?;
                }

                Ok(())
            }
            Pattern::Tuple { tuple, .. } => {
                write!(w, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(w, ", ")?,
                    |pat| => {
                        pat.pp(w, indent)?
                    }
                }
                write!(w, ")")
            }
            Pattern::Variable { name, .. } => name.pp(w, indent),
            Pattern::Wildcard { .. } => write!(w, "_"),
        }
    }
}

impl PP for Type {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use self::Type::*;
        match self {
            Variable(id) => write!(w, "'{}", id)?,
            Int => write!(w, "int")?,
            Real => write!(w, "float")?,
            Fun(t1, t2) => {
                t1.pp(w, indent)?;
                write!(w, " -> ")?;
                t2.pp(w, indent)?;
            }
            Tuple(tys) => {
                write!(w, "(")?;
                for ty in tys.iter() {
                    ty.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
            Datatype(name) => name.pp(w, indent)?,
        }
        Ok(())
    }
}

impl PP for () {
    fn pp<W: io::Write>(&self, _: &mut W, _: usize) -> io::Result<()> {
        Ok(())
    }
}
