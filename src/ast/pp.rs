use crate::ast::*;
use crate::util::PP;
use std::io;

impl<Ty: PP, DE: PP, DS: PP> PP for (SymbolTable, AST<Ty, DE, DS>) {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.1.pp(w, indent)
    }
}

impl<Ty: PP, DE: PP, DS: PP> PP for AST<Ty, DE, DS> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for bind in &self.0 {
            bind.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}

impl<Ty: PP, DE: PP, DS: PP> PP for Declaration<Ty, DE, DS> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use Declaration::*;
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
            D(d) => d.pp(w, indent),
        }
    }
}

impl<Ty: PP> PP for DerivedDeclaration<Ty> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use DerivedDeclaration::*;
        match self {
            Fun { name, clauses, .. } => {
                write!(w, "{}", Self::nspaces(indent))?;
                write!(w, "fun ")?;
                inter_iter!(
                    clauses,
                    { write!(w, "\n{}  | ", Self::nspaces(indent))? ; name.pp(w, indent)? },
                    |(params, expr)| => {
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
                });
                Ok(())
            }
            Infix { priority, names } => {
                write!(w, "infix")?;
                if let Some(p) = priority {
                    write!(w, " {}", p)?;
                }
                for name in names {
                    write!(w, " ")?;
                    name.pp(w, indent)?;
                }
                Ok(())
            }
        }
    }
}

impl<Ty: PP, DE: PP, DS: PP> PP for Expr<Ty, DE, DS> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::ast::ExprKind::*;
        match &self.inner {
            Binds { binds, ret } => {
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
            BuiltinCall { fun, args } => {
                write!(w, "_builtincall \"")?;
                fun.pp(w, indent)?;
                write!(w, "\"(")?;
                inter_iter! {
                    &args,
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, indent)?
                    }
                }
                write!(w, ")")?;
            }
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => {
                write!(w, "_externcall \"{}\".\"{}\"(", module, fun)?;

                inter_iter! {
                    &args,
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, indent)?
                    }
                };
                write!(w, "): (")?;
                inter_iter! {
                    &argty,
                    write!(w, ", ")?,
                    |ty| => {
                        ty.pp(w, indent)?
                    }
                };
                write!(w, ") -> ")?;
                retty.pp(w, indent)?;
            }
            Fn { body, param } => {
                write!(w, "fn ")?;
                param.pp(w, indent)?;
                write!(w, " => ")?;
                body.pp(w, indent + 4)?;
            }
            App { fun, arg } => {
                write!(w, "(")?;
                fun.pp(w, indent)?;
                write!(w, ") ")?;
                arg.pp(w, indent + 4)?;
            }
            Case { cond, clauses } => {
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
            Tuple { tuple } => {
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
            Symbol { name } => {
                name.pp(w, indent)?;
            }
            Constructor { name, arg } => {
                name.pp(w, indent)?;
                if let Some(arg) = arg {
                    write!(w, " ")?;
                    arg.pp(w, indent)?;
                }
            }
            Literal { value } => {
                value.pp(w, indent)?;
            }
            D(d) => {
                d.pp(w, indent)?;
            }
        }
        Ok(())
    }
}

impl<Ty: PP> PP for DerivedExprKind<Ty> {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use DerivedExprKind::*;
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
        use PatternKind::*;
        match &self.inner {
            Constant { value, .. } => write!(w, "{}", value),
            Char { value } => write!(w, r##"#"{}""##, value),
            Constructor { name, arg, .. } => {
                name.pp(w, indent)?;
                if let Some(arg) = arg {
                    // TODO: handle cases when its in function args
                    write!(w, " ")?;
                    arg.pp(w, indent)?;
                }

                Ok(())
            }
            Tuple { tuple, .. } => {
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
            Variable { name, .. } => name.pp(w, indent),
            Wildcard { .. } => write!(w, "_"),
        }
    }
}

impl PP for Type {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use self::Type::*;
        match self {
            Variable(id) => write!(w, "'{}", id)?,
            Char => write!(w, "char")?,
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
