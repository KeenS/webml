use crate::ast::*;
use crate::util::nspaces;
use std::fmt;

impl<Ty: fmt::Display, DE: fmt::Display, DS: fmt::Display, DP: fmt::Display> fmt::Display
    for Context<Ty, DE, DS, DP>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        write!(f, "{:indent$}", self.ast, indent = indent)
    }
}

impl<Ty: fmt::Display, DE: fmt::Display, DS: fmt::Display, DP: fmt::Display> fmt::Display
    for AST<Ty, DE, DS, DP>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        for bind in &self.0 {
            writeln!(f, "{:indent$}", bind, indent = indent)?;
        }
        Ok(())
    }
}

impl<Ty: fmt::Display, DE: fmt::Display, DS: fmt::Display, DP: fmt::Display> fmt::Display
    for Declaration<Ty, DE, DS, DP>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Declaration::*;
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;

        match self {
            Datatype { name, constructors } => {
                write!(f, "datatype {:indent$} =", name, indent = indent)?;
                inter_iter!(constructors, write!(f, " |")?, |(name, param)| =>{
                    write!(f, " {:indent$}", name, indent = indent)?;
                    if let Some(param) = param {
                        write!(f, " of {:indent$}", param , indent = indent)?;
                    }
                });
                Ok(())
            }
            Val { pattern, expr, rec } => {
                write!(f, "{}val ", nspaces(indent))?;
                if *rec {
                    write!(f, "rec ")?;
                }
                write!(
                    f,
                    "{:indent$}: {} = {:next$}",
                    pattern,
                    expr.ty,
                    expr,
                    indent = indent,
                    next = next
                )?;
                Ok(())
            }
            LangItem { name, decl } => {
                writeln!(f, "{}__langitem(({}))__", nspaces(indent), name)?;
                write!(f, "{:indent$}", decl, indent = indent)
            }
            Local { binds, body } => {
                let ind = nspaces(indent);
                writeln!(f, "local")?;
                for val in binds {
                    writeln!(f, "{:next$}", val, next = next)?;
                }
                writeln!(f, "{}in", ind)?;
                for val in body {
                    writeln!(f, "{:next$}", val, next = next)?;
                }
                write!(f, "\n{}end", ind)?;
                Ok(())
            }
            D(d) => write!(f, "{:indent$}", d, indent = indent),
        }
    }
}

impl fmt::Display for LangItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LangItem::*;
        match self {
            Bool => write!(f, "bool"),
            String => write!(f, "string"),
            StringEq => write!(f, "stringEq"),
            StringNeq => write!(f, "stringNeq"),
            StringGt => write!(f, "stringGt"),
            StringGe => write!(f, "stringGe"),
            StringLt => write!(f, "stringLt"),
            StringLe => write!(f, "stringLe"),
        }
    }
}

impl<Ty: fmt::Display> fmt::Display for DerivedDeclaration<Ty> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use DerivedDeclaration::*;
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;

        match self {
            Fun { name, clauses, .. } => {
                write!(f, "{}fun", nspaces(indent))?;
                inter_iter!(
                    clauses,
                     write!(f, "\n{}  | {}", nspaces(indent), name)?,
                    |(params, expr)| => {
                    write!(f, "{:indent$} ", name, indent = indent)?;
                    for param in params {
                        write!(f, "{:indent$} ", param, indent = indent)?;
                    }
                    // write!(w, ": ")?;
                    // self.ty.pp(w, indent)?;
                    write!(f, " = {:next$}", expr, next = next)?;
                });
                Ok(())
            }
            Infix { priority, names } => {
                write!(f, "infix")?;
                if let Some(p) = priority {
                    write!(f, " {}", p)?;
                }
                for name in names {
                    write!(f, " {}", name)?;
                }
                Ok(())
            }
            Infixr { priority, names } => {
                write!(f, "infixr")?;
                if let Some(p) = priority {
                    write!(f, " {}", p)?;
                }
                for name in names {
                    write!(f, " {}", name)?;
                }
                Ok(())
            }
            Nonfix { names } => {
                write!(f, "nonfix")?;
                for name in names {
                    write!(f, " {}", name)?;
                }
                Ok(())
            }
            Expr { expr } => {
                write!(f, "{}", expr)
            }
        }
    }
}

impl<Ty: fmt::Display, DE: fmt::Display, DS: fmt::Display, DP: fmt::Display> fmt::Display
    for Expr<Ty, DE, DS, DP>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::ast::ExprKind::*;
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;

        match &self.inner {
            Binds { binds, ret } => {
                let ind = nspaces(indent);
                let nextind = nspaces(next);
                writeln!(f, "let")?;
                for val in binds {
                    writeln!(f, "{:next$}", val, next = next)?;
                }
                writeln!(f, "{}in", ind)?;
                write!(f, "{}{:next$}", nextind, ret, next = next)?;
                write!(f, "\n{}end", ind)?;
            }
            BuiltinCall { fun, args } => {
                write!(f, "_builtincall \"{:indent$}\"(", fun, indent = indent)?;
                inter_iter! {
                    args,
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f,"{:indent$}", arg, indent = indent)?;
                    }
                }
                write!(f, ")")?;
            }
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => {
                write!(f, "_externcall \"{}\".\"{}\"(", module, fun)?;

                inter_iter! {
                    args,
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f,"{:indent$}", arg, indent = indent)?;
                    }
                };
                write!(f, "): (")?;
                inter_iter! {
                    argty,
                    write!(f, ", ")?,
                    |ty| => {
                        write!(f,"{:indent$}", ty, indent = indent)?;
                    }
                };
                write!(f, ") -> {:indent$}", retty, indent = indent)?;
            }
            Fn { body, param } => {
                write!(
                    f,
                    "fn {:indent$} => {:next$}",
                    param,
                    body,
                    indent = indent,
                    next = next
                )?;
            }
            App { fun, arg } => {
                write!(
                    f,
                    "({:indent$}) {:next$}",
                    fun,
                    arg,
                    indent = indent,
                    next = next
                )?;
            }
            Case { cond, clauses } => {
                let ind = nspaces(indent);
                write!(f, "case {:next$} of", cond, next = next)?;
                for (pat, arm) in clauses {
                    write!(f, "\n{}{:next$}=>{:next$}", ind, pat, arm, next = next)?;
                }
            }
            Tuple { tuple } => {
                write!(f, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(f, ", ")?,
                    |t| => {
                        write!(f,"{:indent$}", t, indent = indent)?;
                    }
                }
                write!(f, ")")?;
            }
            Symbol { name } => {
                write!(f, "{:indent$}", name, indent = indent)?;
            }
            Constructor { name, arg } => {
                write!(f, "{:indent$}", name, indent = indent)?;
                if let Some(arg) = arg {
                    write!(f, " {:indent$}", arg, indent = indent)?;
                }
            }
            Literal { value } => {
                write!(f, "{:indent$}", value, indent = indent)?;
            }
            D(d) => {
                write!(f, "{:indent$}", d, indent = indent)?;
            }
        }
        Ok(())
    }
}

impl<Ty: fmt::Display> fmt::Display for DerivedExprKind<Ty> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use DerivedExprKind::*;
        let indent = f.width().unwrap_or(0);
        let next = indent + 4;

        match self {
            If {
                cond, then, else_, ..
            } => {
                let ind = nspaces(indent);
                writeln!(f, "if {:next$}", cond, next = next)?;
                writeln!(f, "{}then {:next$}", ind, then, next = next)?;
                write!(f, "{}else {:next$}", ind, else_, next = next)?;
            }
            AndAlso { l, r } => {
                writeln!(
                    f,
                    "{:indent$} andalso {:next$}",
                    l,
                    r,
                    indent = indent,
                    next = next
                )?;
            }
            OrElse { l, r } => {
                writeln!(
                    f,
                    "{:indent$} orelse {:next$}",
                    l,
                    r,
                    indent = indent,
                    next = next
                )?;
            }
            Seq { seq } => {
                write!(f, "(")?;
                inter_iter! {
                    seq.iter(),
                    write!(f, "; ")?,
                    |e| => {
                        write!(f,"{:indent$}", e, indent = indent)?;
                    }
                }
                write!(f, ")")?;
            }
            BindSeq { binds, ret } => {
                let ind = nspaces(indent);
                let nextind = nspaces(next);
                writeln!(f, "let")?;
                for val in binds {
                    writeln!(f, "{:next$}", val, next = next)?;
                }
                writeln!(f, "{}in\n", ind)?;
                inter_iter! {
                    ret.iter(),
                    write!(f, ";\n")?,
                    |e| => {
                        write!(f,"{}{:indent$}", nextind, e, indent = next)?;
                    }
                }
                write!(f, "\n{}end", ind)?;
            }
            String { value } => {
                write!(f, "{:?}", value)?;
            }
            Op { name } => {
                write!(f, "op {:indent$}", name, indent = indent)?;
            }
            While { cond, body } => {
                let ind = nspaces(indent);
                write!(
                    f,
                    "while {:indent$}\n{}do {:indent$}",
                    cond,
                    ind,
                    body,
                    indent = next
                )?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Nothing {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        match *self {}
    }
}

impl<Ty, DP: fmt::Display> fmt::Display for Pattern<Ty, DP> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PatternKind::*;
        let indent = f.width().unwrap_or(0);

        match &self.inner {
            Constant { value, .. } => write!(f, "{}", value),
            Char { value } => write!(f, r##"#"{}""##, value),
            Constructor { name, arg, .. } => {
                write!(f, "{}", name)?;
                if let Some(arg) = arg {
                    // TODO: handle cases when its in function args
                    write!(f, " {:indent$}", arg, indent = indent)?;
                }

                Ok(())
            }
            Tuple { tuple, .. } => {
                write!(f, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(f, ", ")?,
                    |pat| => {
                        write!(f, "{:indent$}", pat, indent = indent)?;
                    }
                }
                write!(f, ")")
            }
            Variable { name, .. } => write!(f, "{:indent$}", name, indent = indent),
            Wildcard { .. } => write!(f, "_"),
            D(d) => write!(f, "{:indent$}", d, indent = indent),
        }
    }
}

impl fmt::Display for DerivedPatternKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            DerivedPatternKind::String { value } => write!(
                f,
                "{}",
                value
                    .iter()
                    .map(|&u| char::from_u32(u).unwrap())
                    .collect::<String>()
            ),
            DerivedPatternKind::Op { name } => {
                write!(f, "{}", name)
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Type::*;
        match self {
            Variable(id) => write!(f, "'{}", id)?,
            Char => write!(f, "char")?,
            Int => write!(f, "int")?,
            Real => write!(f, "float")?,
            Fun(t1, t2) => {
                write!(f, "{} -> {}", t1, t2)?;
            }
            Tuple(tys) => {
                write!(f, "(")?;
                for ty in tys.iter() {
                    write!(f, "{}", ty)?;
                    write!(f, ", ")?;
                }
                write!(f, ")")?;
            }
            Datatype(name) => write!(f, "{}", name)?,
        }
        Ok(())
    }
}

impl fmt::Display for Empty {
    fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
        Ok(())
    }
}

impl fmt::Display for BIF {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BIF::*;
        match self {
            Add | AddInt | AddReal => {
                write!(f, "add")?;
            }
            Sub | SubInt | SubReal => {
                write!(f, "sub")?;
            }
            Mul | MulInt | MulReal => {
                write!(f, "mul")?;
            }
            Div => {
                write!(f, "div")?;
            }
            Divf => {
                write!(f, "divf")?;
            }
            Mod | ModInt => {
                write!(f, "mod")?;
            }
            Eq | EqInt | EqReal | EqChar => {
                write!(f, "eq")?;
            }
            Neq | NeqInt | NeqReal | NeqChar => {
                write!(f, "neq")?;
            }
            Gt | GtInt | GtReal | GtChar => {
                write!(f, "gt")?;
            }
            Ge | GeInt | GeReal | GeChar => {
                write!(f, "ge")?;
            }
            Lt | LtInt | LtReal | LtChar => {
                write!(f, "lt")?;
            }
            Le | LeInt | LeReal | LeChar => {
                write!(f, "le")?;
            }
        }
        Ok(())
    }
}
