use std::io;

use util::PP;
use mir::*;

impl PP for MIR {
    fn pp(&self, w: &mut io::Write, indent: usize) -> io::Result<()> {
        for fun in &self.0 {
            fun.pp(w, indent)?;
        }
        Ok(())
    }
}

impl PP for Function {
    fn pp(&self, w: &mut io::Write, indent: usize) -> io::Result<()> {
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
    fn pp(&self, w: &mut io::Write, indent: usize) -> io::Result<()> {
        use mir::EbbTy::*;
        let indent = indent + 4;
        match *self {
            Unit => write!(w, "()")?,
            Bool => write!(w, "bool")?,
            Int => write!(w, "int")?,
            Float => write!(w, "float")?,
            Tuple(ref tys) => {
                write!(w, "(")?;
                inter_iter! {
                    tys,
                    write!(w, ", ")?,
                    |t| => t.pp(w, indent)?
                }
                write!(w, ")")?;
            }
            Cls {
                ref closures,
                ref param,
                ref ret,
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
            Ebb {
                ref params,
                ref ret,
            } => {
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
    fn pp(&self, w: &mut io::Write, indent: usize) -> io::Result<()> {
        let space = Self::nspaces(indent);
        write!(w, "{}", space)?;
        self.name.pp(w, indent)?;
        write!(w, "(")?;
        let indent = indent + 4;
        inter_iter! {
            self.params.iter(),
            write!(w, ", ")?,
            |&(ref param_ty, ref param)| => {
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

fn pp_binop(
    w: &mut io::Write,
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
    fn pp(&self, w: &mut io::Write, indent: usize) -> io::Result<()> {
        use mir::Op::*;
        let space = Self::nspaces(indent);
        let indent = indent + 4;
        match self {
            &Lit {
                ref var,
                ref ty,
                ref value,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                value.pp(w, indent)?;
            }
            &Alias {
                ref var,
                ref ty,
                ref sym,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                sym.pp(w, indent)?;
            }
            &Add {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "+", var, ty, l, r)?;
            }
            &Sub {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "-", var, ty, l, r)?;
            }
            &Mul {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "*", var, ty, l, r)?;
            }
            &DivInt {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "div", var, ty, l, r)?;
            }
            &DivFloat {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "/", var, ty, l, r)?;
            }
            &Mod {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "mod", var, ty, l, r)?;
            }
            &Eq {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "=", var, ty, l, r)?;
            }
            &Neq {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "<>", var, ty, l, r)?;
            }
            &Gt {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, ">", var, ty, l, r)?;
            }
            &Ge {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, ">=", var, ty, l, r)?;
            }
            &Lt {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "<", var, ty, l, r)?;
            }
            &Le {
                ref var,
                ref ty,
                ref l,
                ref r,
            } => {
                pp_binop(w, indent, &space, "<=", var, ty, l, r)?;
            }
            &Closure {
                ref var,
                ref param_ty,
                ref ret_ty,
                ref fun,
                ref env,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                (EbbTy::Cls {
                    closures: env.iter().map(|&(ref ty, _)| ty.clone()).collect(),
                    param: Box::new(param_ty.clone()),
                    ret: Box::new(ret_ty.clone()),
                }).pp(w, indent)?;
                write!(w, " := ")?;
                fun.pp(w, indent)?;
                write!(w, ".__close(")?;
                inter_iter! {
                    env.iter(),
                    write!(w, ", ")?,
                    |&(ref var_ty, ref var)| => {
                        var.pp(w, 0)?;
                        write!(w, ": ")?;
                        var_ty.pp(w, 0)?;
                    }
                }
                write!(w, ")")?;
            }
            &BuiltinCall {
                ref var,
                ref ty,
                ref fun,
                ref args,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                fun.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter!{
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, 0)?
                    }
                }
                write!(w, ")")?;
            }

            &Call {
                ref var,
                ref ty,
                ref fun,
                ref args,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                fun.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter!{
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, 0)?
                    }
                }
                write!(w, ")")?;
            }
            &Tuple {
                ref var,
                ref tys,
                ref tuple,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": (")?;
                inter_iter!{
                    tys.iter(),
                    write!(w, ", ")?,
                    |ty| => {
                        ty.pp(w, indent)?
                    }
                }
                write!(w, ") := ")?;
                write!(w, "(")?;
                inter_iter!{
                    tuple.iter(),
                    write!(w, ", ")?,
                    |var| => var.pp(w, 0)?
                }
                write!(w, ")")?;
            }
            &Proj {
                ref var,
                ref ty,
                ref index,
                ref tuple,
            } => {
                write!(w, "{}", space)?;
                var.pp(w, indent)?;
                write!(w, ": ")?;
                ty.pp(w, indent)?;
                write!(w, " := #{} ", index)?;
                tuple.pp(w, indent)?;
            }
            &Branch {
                ref cond,
                ref clauses,
                ref default,
                ..
            } => {
                write!(w, "{}branch", space)?;
                cond.pp(w, indent)?;
                write!(w, " {{\n")?;
                {
                    let indent = indent + 4;
                    let space = Self::nspaces(indent);
                    for &(ref val, ref arm, _) in clauses.iter() {
                        write!(w, "{}{} => ", space, val)?;
                        arm.pp(w, indent)?;
                        write!(w, "()\n")?;
                    }
                    for &(ref arm, _) in default {
                        write!(w, "{}default => ", space)?;
                        arm.pp(w, indent)?;
                        write!(w, "(")?;
                        cond.pp(w, 0)?;
                        write!(w, ")\n")?;
                    }
                }
                write!(w, "{}}}\n", space)?;
            }
            &Jump {
                ref target,
                ref args,
                ..
            } => {
                write!(w, "{}", space)?;
                target.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter!{
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => {
                        arg.pp(w, 0)?
                    }
                }
                write!(w, ")")?;
            }
            &Ret { ref value, ref ty } => {
                match *value {
                    Some(ref v) => {
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
