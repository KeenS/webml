use crate::mir::*;
use crate::util::nspaces;
use std::fmt;

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.1)
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
                    let space = nspaces(indent);
                    for (val, arm, _) in clauses.iter() {
                        writeln!(f, "{}{} => {}()", space, val, arm)?;
                    }
                    for (arm, _) in default {
                        writeln!(f, "{}default => {}({})", space, arm, cond)?;
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
