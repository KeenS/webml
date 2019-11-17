use std::io;

use crate::mir::*;
use crate::util::PP;

impl PP for MIR {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for fun in &self.0 {
            fun.pp(w, indent)?;
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
        write!(w, " = {{\n")?;
        for ebb in self.body.iter() {
            ebb.pp(w, indent)?;
        }
        write!(w, "}}\n")?;
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
        write!(w, "):\n")?;
        for op in self.body.iter() {
            op.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}

fn pp_binop<W: io::Write>(
    w: &mut W,
    indent: usize,
    space: &str,
    name: &str,
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
    write!(w, " {} ", name)?;
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
            Add { var, ty, l, r } => {
                pp_binop(w, indent, &space, "+", var, ty, l, r)?;
            }
            Sub { var, ty, l, r } => {
                pp_binop(w, indent, &space, "-", var, ty, l, r)?;
            }
            Mul { var, ty, l, r } => {
                pp_binop(w, indent, &space, "*", var, ty, l, r)?;
            }
            DivInt { var, ty, l, r } => {
                pp_binop(w, indent, &space, "div", var, ty, l, r)?;
            }
            DivFloat { var, ty, l, r } => {
                pp_binop(w, indent, &space, "/", var, ty, l, r)?;
            }
            Mod { var, ty, l, r } => {
                pp_binop(w, indent, &space, "mod", var, ty, l, r)?;
            }
            Eq { var, ty, l, r } => {
                pp_binop(w, indent, &space, "=", var, ty, l, r)?;
            }
            Neq { var, ty, l, r } => {
                pp_binop(w, indent, &space, "<>", var, ty, l, r)?;
            }
            Gt { var, ty, l, r } => {
                pp_binop(w, indent, &space, ">", var, ty, l, r)?;
            }
            Ge { var, ty, l, r } => {
                pp_binop(w, indent, &space, ">=", var, ty, l, r)?;
            }
            Lt { var, ty, l, r } => {
                pp_binop(w, indent, &space, "<", var, ty, l, r)?;
            }
            Le { var, ty, l, r } => {
                pp_binop(w, indent, &space, "<=", var, ty, l, r)?;
            }
            Closure {
                var,
                param_ty,
                ret_ty,
                fun,
                env,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
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
            BuiltinCall { var, ty, fun, args } => {
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
                write!(w, " := select {} ", index)?;
                union.pp(w, indent)?;
            }
            Branch {
                cond,
                clauses,
                default,
                ..
            } => {
                write!(w, "{}branch", space)?;
                cond.pp(w, indent)?;
                write!(w, " {{\n")?;
                {
                    let indent = indent + 4;
                    let space = Self::nspaces(indent);
                    for (val, arm, _) in clauses.iter() {
                        write!(w, "{}{} => ", space, val)?;
                        arm.pp(w, indent)?;
                        write!(w, "()\n")?;
                    }
                    for (arm, _) in default {
                        write!(w, "{}default => ", space)?;
                        arm.pp(w, indent)?;
                        write!(w, "(")?;
                        cond.pp(w, 0)?;
                        write!(w, ")\n")?;
                    }
                }
                write!(w, "{}}}\n", space)?;
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
