use crate::hir::*;
use crate::util;
use std::fmt;

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        write!(f, "{:indent$}", self.1, indent = indent)
    }
}

impl fmt::Display for HIR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        for bind in &self.0 {
            writeln!(f, "{:indent$}\n", bind, indent = indent)?;
        }
        Ok(())
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;
        let rec = if self.rec { " rec" } else { "" };
        write!(
            f,
            "{}val{} {}: {} = {:next$}",
            util::nspaces(indent),
            rec,
            self.name,
            self.ty,
            self.expr,
            next = next
        )?;
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::hir::Expr::*;
        // abusing width as indent size
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;
        match self {
            Let { bind, ret, .. } => {
                let ind = util::nspaces(indent);
                writeln!(f, "let {} in", bind)?;
                write!(f, "{ind}{:indent$}", ret, indent = indent)?;
            }
            Fun {
                body,
                param,
                captures,
                ..
            } => {
                let nextind = util::nspaces(next);
                write!(f, "fun(")?;
                inter_iter! {
                    captures,
                    write!(f, ", ")?,
                    |(_,  cap)| => {
                        write!(f, "{:indent$}", cap, indent = indent)?
                    }
                }
                write!(
                    f,
                    ") {:indent$} =>\n{nextind}{:next$}",
                    param.1,
                    body,
                    indent = indent,
                    next = next,
                )?;
            }
            Closure { envs, fname, .. } => {
                write!(f, "<closure {:indent$} (", fname, indent = indent)?;
                inter_iter! {
                    envs.iter(),
                    write!(f, ", ")?,
                    |(_,  var)| => {
                        write!(f, "{}", var)?
                    }
                }
                write!(f, ")>")?;
            }
            App { fun, arg, .. } => {
                write!(
                    f,
                    "({:indent$}) {:next$}",
                    fun,
                    arg,
                    indent = indent,
                    next = next
                )?;
            }
            Case { expr, arms, .. } => {
                let ind = util::nspaces(indent);
                write!(f, "case {:next$} of", expr, next = next)?;
                for (pat, arm) in arms {
                    write!(f, "\n{}{:next$} => {:next$}", ind, pat, arm, next = next)?;
                }
            }
            Tuple { tuple, .. } => {
                write!(f, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(f, ", ")?,
                    |t| => {
                        write!(f, "{:indent$}", t, indent = indent)?;
                    }
                }
                write!(f, ")")?;
            }
            Proj { index, tuple, .. } => {
                write!(f, "#{} {:next$}", index, tuple, next = next)?;
            }
            BuiltinCall { fun, args, .. } => {
                write!(f, "{:indent$}(", fun, indent = indent)?;
                inter_iter! {
                    args.iter(),
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f, "{:indent$}",arg, indent = indent)?
                    }
                };
                write!(f, ")")?;
            }
            ExternCall {
                module, fun, args, ..
            } => {
                write!(f, "\"{}\".\"{}\"(", module, fun)?;
                inter_iter! {
                    args.iter(),
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f, "{:indent$}", arg, indent = indent)?;
                    }
                };
                write!(f, ")")?;
            }
            Constructor {
                descriminant, arg, ..
            } => match arg {
                None => {
                    write!(f, "{}", descriminant)?;
                }
                Some(arg) => {
                    write!(f, "{} {:indent$}", descriminant, arg, indent = indent)?;
                }
            },
            Sym { name, .. } => {
                write!(f, "{:indent$}", name, indent = indent)?;
            }
            Lit { value, .. } => {
                write!(f, "{:indent$}", value, indent = indent)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Constant { value, .. } => write!(f, "{}", value),
            Pattern::Char { value, .. } => write!(f, r##"#"{}""##, value),
            Pattern::Constructor {
                descriminant, arg, ..
            } => match arg {
                None => write!(f, "{}", descriminant),
                Some((_, sym)) => {
                    write!(f, "{}({})", descriminant, sym)?;
                    Ok(())
                }
            },
            Pattern::Tuple { tuple, .. } => {
                write!(f, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(f, ", ")?,
                    |t| => {
                        write!(f, "{}", t)?
                    }
                }
                write!(f, ")")
            }
            Pattern::Var { name, .. } => write!(f, "{}", name),
        }
    }
}

impl fmt::Display for HTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::hir::HTy::*;
        match self {
            Char => write!(f, "char")?,
            Int => write!(f, "int")?,
            Real => write!(f, "real")?,
            Tuple(tys) => {
                write!(f, "(")?;
                inter_iter! {
                    tys.iter(),
                    write!(f, " * ")?,
                    |ty| => {
                        write!(f, "{}", ty)?
                    }
                }
                write!(f, ")")?;
            }
            Fun(t1, t2) => {
                write!(f, "{} -> {}", t1, t2)?;
            }
            Datatype(name) => write!(f, "{}", name)?,
        }
        Ok(())
    }
}

impl fmt::Display for BIF {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BIF::*;
        match self {
            AddInt | AddReal => {
                write!(f, "add")?;
            }
            SubInt | SubReal => {
                write!(f, "sub")?;
            }
            MulInt | MulReal => {
                write!(f, "mul")?;
            }
            DivInt | DivReal => {
                write!(f, "div")?;
            }
            ModInt => {
                write!(f, "mod")?;
            }
            EqInt | EqReal | EqChar => {
                write!(f, "eq")?;
            }
            NeqInt | NeqReal | NeqChar => {
                write!(f, "neq")?;
            }
            GtInt | GtReal | GtChar => {
                write!(f, "gt")?;
            }
            GeInt | GeReal | GeChar => {
                write!(f, "ge")?;
            }
            LtInt | LtReal | LtChar => {
                write!(f, "lt")?;
            }
            LeInt | LeReal | LeChar => {
                write!(f, "le")?;
            }
        }
        Ok(())
    }
}
