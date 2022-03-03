use std::io;

use crate::hir::*;
use crate::util::PP;
use std::fmt;

impl PP for Context {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.1.pp(w, indent)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        write!(f, "{:indent$}", self.1, indent = indent)
    }
}

impl PP for HIR {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for bind in &self.0 {
            bind.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
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

impl PP for Val {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        let rec = if self.rec { " rec " } else { "" };
        write!(w, "{}val{} ", Self::nspaces(indent), rec)?;
        self.name.pp(w, indent)?;
        write!(w, ": ")?;
        self.ty.pp(w, indent)?;
        write!(w, " = ")?;
        self.expr.pp(w, indent + 4)?;
        Ok(())
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;
        let rec = if self.rec { " rec " } else { "" };
        write!(
            f,
            "{}val{} {}: {} = {:next$}",
            Self::nspaces(indent),
            rec,
            self.name,
            self.ty,
            self.expr,
            next = next
        )?;
        Ok(())
    }
}

impl PP for Expr {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::hir::Expr::*;
        match self {
            Let { bind, ret, .. } => {
                let ind = Self::nspaces(indent);
                let nextind = Self::nspaces(indent + 4);
                write!(w, "let ")?;
                bind.pp(w, indent + 4)?;
                write!(w, "{}in\n{}", ind, nextind)?;
                ret.pp(w, indent + 4)?;
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
            BuiltinCall { fun, args, .. } => {
                fun.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter! {args.iter(), write!(w, ", ")?, |arg| => arg.pp(w, indent)?};
                write!(w, ")")?;
            }
            ExternCall {
                module, fun, args, ..
            } => {
                write!(w, "\"{}\".\"{}\"(", module, fun)?;
                inter_iter! {args.iter(), write!(w, ", ")?, |arg| => arg.pp(w, indent)?};
                write!(w, ")")?;
            }
            Constructor {
                descriminant, arg, ..
            } => match arg {
                None => {
                    write!(w, "{}", descriminant)?;
                }
                Some(arg) => {
                    write!(w, "{} ", descriminant)?;
                    arg.pp(w, indent)?;
                }
            },
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::hir::Expr::*;
        // abusing width as indent size
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;
        match self {
            Let { bind, ret, .. } => {
                write!(f, "let {} in\n", bind)?;
                write!(f, "{:next$}", ret, next = next)?;
            }
            Fun {
                body,
                param,
                captures,
                ..
            } => {
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
                    ") {:indent$} => {:next$}",
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
                let ind = Self::nspaces(indent);
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

impl PP for Pattern {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        match self {
            Pattern::Constant { value, .. } => write!(w, "{}", value),
            Pattern::Char { value, .. } => write!(w, r##"#"{}""##, value),
            Pattern::Constructor {
                descriminant, arg, ..
            } => match arg {
                None => write!(w, "{}", descriminant),
                Some((_, sym)) => {
                    write!(w, "{}(", descriminant)?;
                    sym.pp(w, indent)?;
                    write!(w, ")")?;
                    Ok(())
                }
            },
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

impl PP for HTy {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::hir::HTy::*;
        match self {
            Char => write!(w, "char")?,
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
            Datatype(name) => {
                name.pp(w, indent)?;
            }
        }
        Ok(())
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

impl PP for BIF {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        use self::BIF::*;
        match self {
            AddInt | AddReal => {
                write!(w, "add")?;
            }
            SubInt | SubReal => {
                write!(w, "sub")?;
            }
            MulInt | MulReal => {
                write!(w, "mul")?;
            }
            DivInt | DivReal => {
                write!(w, "div")?;
            }
            ModInt => {
                write!(w, "mod")?;
            }
            EqInt | EqReal | EqChar => {
                write!(w, "eq")?;
            }
            NeqInt | NeqReal | NeqChar => {
                write!(w, "neq")?;
            }
            GtInt | GtReal | GtChar => {
                write!(w, "gt")?;
            }
            GeInt | GeReal | GeChar => {
                write!(w, "ge")?;
            }
            LtInt | LtReal | LtChar => {
                write!(w, "lt")?;
            }
            LeInt | LeReal | LeChar => {
                write!(w, "le")?;
            }
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
