use std::io;

use util::PP;
use mir::*;

impl PP for MIR {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        for fun in &self.0 {
            fun.pp(w, indent)?;
        }
        Ok(())
    }
}



impl PP for Function {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        let indent = indent + 4;
        write!(w, "fun {}: (", self.name.0)?;
        for &(ref param_ty, ref param) in self.body[0].params.iter() {
            param.pp(w, 0)?;
            write!(w, ": ")?;
            param_ty.pp(w, 0)?;
            write!(w, ", ")?;
        }
        write!(w, ") -> ")?;
        self.body_ty.pp(w, 0)?;
        write!(w, " = {{\n")?;
//        self.body[0].pp(w, indent)?;
        for ebb in self.body.iter() {
            ebb.pp(w, indent)?;
        }
        write!(w, "}}\n")?;
        Ok(())
    }

}


impl PP for EbbTy  {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use mir::EbbTy::*;
        let indent = indent + 4;
        match self {
            &Unit => write!(w, "()")?,
            &Bool => write!(w, "bool")?,
            &Int => write!(w, "int")?,
            &Cls{ref param, ref ret} => {
                write!(w, "fn(")?;
                param.pp(w, indent)?;
                write!(w, ") -> ")?;
                ret.pp(w, indent)?;
            },
            &Ebb{ref params, ref ret} => {
                write!(w, "fn(")?;
                for param in params {
                    param.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ") -> ")?;
                ret.pp(w, indent)?;
            }
        };
        Ok(())
    }

}

impl PP for EBB  {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        let space = Self::nspaces(indent);
        write!(w, "{}{}(", space, self.name.0)?;
        let indent = indent + 4;
        for &(ref param_ty, ref param) in self.params.iter() {
            param.pp(w, indent)?;
            write!(w, ": ")?;
            param_ty.pp(w, indent)?;
            write!(w, ", ")?;
        }
        write!(w, "):\n")?;
        for op in self.body.iter() {
            op.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}

impl PP for Op  {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use mir::Op::*;
        let space = Self::nspaces(indent);
        let indent = indent + 4;
        match self {
            &Lit{ref var, ref ty, ref value} => {
                write!(w, "{}{}: ", space, var.0)?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                value.pp(w, indent)?;
            },
            &Alias{ref var, ref ty, ref sym} => {
                write!(w, "{}{}: ", space, var.0)?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                sym.pp(w, indent)?;
            },
            &Add{ref var, ref ty, ref l, ref r} => {
                write!(w, "{}{}: ", space, var.0)?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                l.pp(w, indent)?;
                write!(w, " + ")?;
                r.pp(w, indent)?;

            },
            &Mul{ref var, ref ty, ref l, ref r} => {
                write!(w, "{}{}: ", space, var.0)?;
                ty.pp(w, indent)?;
                write!(w, " := ")?;
                l.pp(w, indent)?;
                write!(w, " * ")?;
                r.pp(w, indent)?;

            },
            &Closure{ref var, ref param_ty, ref ret_ty, ref fun, ref env} => {
                write!(w, "{}{}: ", space, var.0)?;
                (EbbTy::Cls{param: Box::new(param_ty.clone()),
                            ret: Box::new(ret_ty.clone())}).pp(w, indent)?;
                write!(w, " := ")?;
                fun.pp(w, indent)?;
                write!(w, ".__close(")?;
                for &(ref var_ty, ref var) in env.iter() {
                    var.pp(w, 0)?;
                    write!(w, ": ")?;
                    var_ty.pp(w, 0)?;
                    write!(w, ", ")?;
                };
                write!(w, ")")?;

            },
            &Call{ref var, ref ty, ref fun, ref args} => {
                write!(w, "{}{}: ", space, var.0)?;
                ty.pp(w, indent)?;
                write!(w, " := {}(", fun.0)?;
                for arg in args.iter() {
                    arg.pp(w, 0)?;
                    write!(w, ", ")?;
                };
                write!(w, ")")?;
            },
            &Branch{ref cond, ref then, ref else_} => {
                write!(w, "{}if {} then {}() else {}()", space, cond.0, then.0, else_.0)?;
            },
            &Jump{ref target, ref args} => {
                write!(w, "{}{}(", space, target.0)?;
                for arg in args.iter() {
                    arg.pp(w, 0)?;
                    write!(w, ", ")?;
                };
                write!(w, ")")?;

            },
            &Ret{ref value, ref ty} => {
                write!(w, "{}ret {}: ", space, value.0)?;
                ty.pp(w, indent)?;
            },
        };
        Ok(())
    }
}
