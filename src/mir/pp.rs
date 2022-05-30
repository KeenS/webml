use crate::mir::*;
use crate::util::{nspaces, PP};
use std::fmt;
use std::io;

impl PP for Context {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.1.pp(w, indent)
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.1)
    }
}
impl PP for MIR {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for fun in &self.0 {
            fun.pp(w, indent)?;
        }
        Ok(())
    }
}

impl fmt::Display for MIR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for fun in &self.0 {
            write!(f, "{}", fun)?;
        }
        Ok(())
    }
}
impl PP for Function {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        let indent = indent + 4;
        write!(w, "fun ")?;
        self.name.pp(w, indent)?;
        write!(w, ": (")?;
        inter_iter! {
            self.body[0].params.iter(),
            write!(w, ", ")?,
            |&(ref param_ty, ref param)| => {
                param.pp(w, 0)?;
                write!(w, ": ")?;
                param_ty.pp(w, 0)?;
            }
        }
        write!(w, ") -> ")?;
        self.body_ty.pp(w, 0)?;
        writeln!(w, " = {{")?;
        for ebb in self.body.iter() {
            ebb.pp(w, indent)?;
        }
        writeln!(w, "}}")?;
        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        let indent = indent + 4;
        write!(f, "fun {}: (", self.name)?;
        inter_iter! {
            self.body[0].params.iter(),
            write!(f, ", ")?,
            |&(ref param_ty, ref param)| => {
                write!(f, "{}: {}", param, param_ty)?;
            }
        }
        writeln!(f, ") -> {} = {{", self.body_ty)?;
        for ebb in self.body.iter() {
            write!(f, "{:indent$}", ebb, indent = indent)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
impl PP for EbbTy {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::mir::EbbTy::*;
        let indent = indent + 4;
        match self {
            Unit => write!(w, "()")?,
            Bool => write!(w, "bool")?,
            Char => write!(w, "char")?,
            Int => write!(w, "int")?,
            Float => write!(w, "float")?,
            Tuple(tys) => {
                write!(w, "(")?;
                inter_iter! {
                    tys,
                    write!(w, ", ")?,
                    |t| => t.pp(w, indent)?
                }
                write!(w, ")")?;
            }
            Union(tys) => {
                write!(w, "union {{")?;
                inter_iter! {
                    tys,
                    write!(w, ", ")?,
                    |t| => t.pp(w, indent)?
                }
                write!(w, "}}")?;
            }
            Cls {
                closures,
                param,
                ret,
            } => {
                write!(w, "closure_fn<")?;
                inter_iter! {
                    closures,
                    write!(w, ", ")?,
                    |c| => c.pp(w, indent)?
                }
                write!(w, ">(")?;
                param.pp(w, indent)?;
                write!(w, ") -> ")?;
                ret.pp(w, indent)?;
            }
            Ebb { params, ret } => {
                write!(w, "fn(")?;
                inter_iter! {
                    params,
                    write!(w, ", ")?,
                    |param| => param.pp(w, indent)?
                }
                write!(w, ") -> ")?;
                ret.pp(w, indent)?;
            }
            Variable(name) => {
                name.pp(w, indent)?;
            }
        };
        Ok(())
    }
}

impl fmt::Display for EbbTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::mir::EbbTy::*;
        match self {
            Unit => write!(f, "()")?,
            Bool => write!(f, "bool")?,
            Char => write!(f, "char")?,
            Int => write!(f, "int")?,
            Float => write!(f, "float")?,
            Tuple(tys) => {
                write!(f, "(")?;
                inter_iter! {
                    tys,
                    write!(f, ", ")?,
                    |t| => {
                        write!(f, "{}", t)?;
                    }
                }
                write!(f, ")")?;
            }
            Union(tys) => {
                write!(f, "union {{")?;
                inter_iter! {
                    tys,
                    write!(f, ", ")?,
                    |t| => {
                        write!(f, "{}", t)?;
                    }
                }
                write!(f, "}}")?;
            }
            Cls {
                closures,
                param,
                ret,
            } => {
                write!(f, "closure_fn<")?;
                inter_iter! {
                    closures,
                    write!(f, ", ")?,
                    |c| => {
                        write!(f, "{}", c)?;
                    }
                }
                write!(f, ">({}) -> {}", param, ret)?;
            }
            Ebb { params, ret } => {
                write!(f, "fn(")?;
                inter_iter! {
                    params,
                    write!(f, ", ")?,
                    |param| => {
                        write!(f, "{}", param)?;
                    }
                }
                write!(f, ") -> {}", ret)?;
            }
            Variable(name) => {
                write!(f, "{}", name)?;
            }
        };
        Ok(())
    }
}

impl PP for EBB {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        let space = Self::nspaces(indent);
        write!(w, "{}", space)?;
        self.name.pp(w, indent)?;
        write!(w, "(")?;
        let indent = indent + 4;
        inter_iter! {
            self.params.iter(),
            write!(w, ", ")?,
            |(param_ty, param)| => {
                param.pp(w, indent)?;
                write!(w, ": ")?;
                param_ty.pp(w, indent)?;
            }
        }
        writeln!(w, "):")?;
        for op in self.body.iter() {
            op.pp(w, indent)?;
            writeln!(w)?;
        }
        Ok(())
    }
}

impl fmt::Display for EBB {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        let space = nspaces(indent);
        write!(f, "{}{}(", space, self.name)?;
        let indent = indent + 4;
        inter_iter! {
            self.params.iter(),
            write!(f, ", ")?,
            |(param_ty, param)| => {
                write!(f, "{}: {}", param, param_ty)?;
            }
        }
        writeln!(f, "):")?;
        for op in self.body.iter() {
            writeln!(f, "{:indent$}", op, indent = indent)?;
        }
        Ok(())
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BinOp::*;
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

fn pp_binop<W: io::Write>(
    w: &mut W,
    indent: usize,
    space: &str,
    binop: BinOp,
    var: &Symbol,
    ty: &EbbTy,
    l: &Symbol,
    r: &Symbol,
) -> io::Result<()> {
    write!(w, "{}", space)?;
    var.pp(w, indent)?;
    write!(w, ": ")?;
    ty.pp(w, indent)?;
    write!(w, " := ")?;
    l.pp(w, indent)?;
    write!(w, " {} ", binop)?;
    r.pp(w, indent)?;
    Ok(())
}

impl PP for Op {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::mir::Op::*;
        let space = Self::nspaces(indent);
        let indent = indent + 4;
        match self {
            Lit { var, ty, value } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                value.pp(w, indent)?;
            }
            Alias { var, ty, sym } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                sym.pp(w, indent)?;
            }
            BinOp {
                var,
                binop,
                ty,
                l,
                r,
            } => {
                pp_binop(w, indent, &space, *binop, var, ty, l, r)?;
            }

            Closure {
                var,
                param_ty,
                ret_ty,
                fun,
                env,
            } => {
                write!(w, "{}{}: ", space, var)?;
                (EbbTy::Cls {
                    closures: env.iter().map(|(ty, _)| ty.clone()).collect(),
                    param: Box::new(param_ty.clone()),
                    ret: Box::new(ret_ty.clone()),
                })
                .pp(w, indent)?;
                write!(w, " := ")?;
                fun.pp(w, indent)?;
                write!(w, ".__close(")?;
                inter_iter! {
                    env.iter(),
                    write!(w, ", ")?,
                    |( var_ty,  var)| => {
                        var.pp(w, 0)?;
                        write!(w, ": ")?;
                        var_ty.pp(w, 0)?;
                    }
                }
                write!(w, ")")?;
            }
            ExternCall {
                var,
                ty,
                module,
                fun,
                args,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := \"{}\".\"{}\"", module, fun)?;
                write!(w, "(")?;
                inter_iter! {
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, 0)?
                    }
                }
                write!(w, ")")?;
            }

            Call { var, ty, fun, args } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                fun.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter! {
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, 0)?
                    }
                }
                write!(w, ")")?;
            }
            Tuple { var, tys, tuple } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": (")?;
                inter_iter! {
                    tys.iter(),
                    write!(w, ", ")?,
                    |ty| => {
                        ty.pp(w, indent)?
                    }
                }
                write!(w, ") := ")?;
                write!(w, "(")?;
                inter_iter! {
                    tuple.iter(),
                    write!(w, ", ")?,
                    |var| => var.pp(w, 0)?
                }
                write!(w, ")")?;
            }
            Proj {
                var,
                ty,
                index,
                tuple,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := #{} ", index)?;
                tuple.pp(w, indent)?;
            }
            Union {
                var,
                tys,
                index,
                variant,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": union {{")?;
                inter_iter! {
                    tys.iter(),
                    write!(w, ", ")?,
                    |ty| => {
                        ty.pp(w, indent)?
                    }
                }
                write!(w, "}} := ")?;
                write!(w, "make_union({}, ", index)?;
                variant.pp(w, 0)?;
                write!(w, ")")?;
            }
            Select {
                var,
                ty,
                index,
                union,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := select({}, ", index)?;
                union.pp(w, indent)?;
                write!(w, ")")?;
            }
            Branch {
                cond,
                clauses,
                default,
                ..
            } => {
                write!(w, "{}branch ", space)?;
                cond.pp(w, indent)?;
                writeln!(w, " {{")?;
                {
                    let indent = indent + 4;
                    let space = Self::nspaces(indent);
                    for (val, arm, _) in clauses.iter() {
                        write!(w, "{}{} => ", space, val)?;
                        arm.pp(w, indent)?;
                        writeln!(w, "()")?;
                    }
                    for (arm, _) in default {
                        write!(w, "{}default => ", space)?;
                        arm.pp(w, indent)?;
                        write!(w, "(")?;
                        cond.pp(w, 0)?;
                        writeln!(w, ")")?;
                    }
                }
                writeln!(w, "{}}}", space)?;
            }
            Jump { target, args, .. } => {
                write!(w, "{}", space)?;
                target.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter! {
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, 0)?
                    }
                }
                write!(w, ")")?;
            }
            Ret { value, ty } => {
                match value {
                    Some(v) => {
                        write!(w, "{}ret ", space)?;
                        v.pp(w, indent)?;
                        write!(w, ": ")?
                    }
                    None => write!(w, "{}ret: ", space)?,
                }

                ty.pp(w, indent)?;
            }
        };
        Ok(())
    }
}

fn display_binop(
    f: &mut fmt::Formatter,
    space: &str,
    binop: BinOp,
    var: &Symbol,
    ty: &EbbTy,
    l: &Symbol,
    r: &Symbol,
) -> fmt::Result {
    write!(f, "{}{}: {} := {} {} {}", space, var, ty, l, binop, r)?;
    Ok(())
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::mir::Op::*;
        let indent = f.width().unwrap_or(0);
        let space = nspaces(indent);
        match self {
            Lit { var, ty, value } => {
                write!(f, "{}{}: {}:= {}", space, var, ty, value)?;
            }
            Alias { var, ty, sym } => {
                write!(f, "{}{}:{} := {}", space, var, ty, sym)?;
            }
            BinOp {
                var,
                ty,
                l,
                r,
                binop,
            } => {
                display_binop(f, &space, *binop, var, ty, l, r)?;
            }
            Closure {
                var,
                param_ty,
                ret_ty,
                fun,
                env,
            } => {
                let ty = EbbTy::Cls {
                    closures: env.iter().map(|(ty, _)| ty.clone()).collect(),
                    param: Box::new(param_ty.clone()),
                    ret: Box::new(ret_ty.clone()),
                };
                write!(f, "{}{}: {} := {}.__close(", space, var, ty, fun)?;
                inter_iter! {
                    env.iter(),
                    write!(f, ", ")?,
                    |(var_ty, var)| => {
                        write!(f, "{}: {}", var, var_ty)?;
                    }
                }
                write!(f, ")")?;
            }
            ExternCall {
                var,
                ty,
                module,
                fun,
                args,
            } => {
                write!(
                    f,
                    "{}{} : {} := \"{}\".\"{}\"(",
                    space, var, ty, module, fun
                )?;
                inter_iter! {
                    args.iter(),
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f, "{}", arg)?;
                    }
                }
                write!(f, ")")?;
            }

            Call { var, ty, fun, args } => {
                write!(f, "{}{}: {} := {}(", space, var, ty, fun)?;
                inter_iter! {
                    args.iter(),
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f, "{}", arg)?;
                    }
                }
                write!(f, ")")?;
            }
            Tuple { var, tys, tuple } => {
                write!(f, "{}{}: (", space, var)?;
                inter_iter! {
                    tys.iter(),
                    write!(f, ", ")?,
                    |ty| => {
                        write!(f, "{}", ty)?;
                    }
                }
                write!(f, ") := (")?;
                inter_iter! {
                    tuple.iter(),
                    write!(f, ", ")?,
                    |var| => {
                        write!(f, "{}", var)?;
                    }
                }
                write!(f, ")")?;
            }
            Proj {
                var,
                ty,
                index,
                tuple,
            } => {
                write!(f, "{}{}: {} := #{} {}", space, var, ty, index, tuple)?;
            }
            Union {
                var,
                tys,
                index,
                variant,
            } => {
                write!(f, "{}{}: union{{", space, var)?;
                inter_iter! {
                    tys.iter(),
                    write!(f, ", ")?,
                    |ty| => {
                        write!(f, "{}", ty)?;
                    }
                }
                write!(f, "}} := make_union({}, {})", index, variant)?;
            }
            Select {
                var,
                ty,
                index,
                union,
            } => {
                write!(
                    f,
                    "{}{}: {} := select({}, {})",
                    space, var, ty, index, union
                )?;
            }
            Branch {
                cond,
                clauses,
                default,
                ..
            } => {
                writeln!(f, "{}branch {}{{", space, cond)?;
                {
                    let indent = indent + 4;
                    let space = Self::nspaces(indent);
                    for (val, arm, _) in clauses.iter() {
                        writeln!(f, "{}{} => {}()", space, val, arm)?;
                    }
                    for (arm, _) in default {
                        write!(f, "{}default => {}({}\n)", space, arm, cond)?;
                    }
                }
                writeln!(f, "{}}}", space)?;
            }
            Jump { target, args, .. } => {
                write!(f, "{}{}(", space, target)?;
                inter_iter! {
                    args.iter(),
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f, "{}", arg)?;
                    }
                }
                write!(f, ")")?;
            }
            Ret { value, ty } => match value {
                Some(v) => write!(f, "{}ret {}: {}", space, v, ty)?,
                None => write!(f, "{}ret: {}", space, ty)?,
            },
        };
        Ok(())
    }
}
