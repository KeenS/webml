use std::io;

use util::PP;
use lir::*;

impl PP for LIR {
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
        for (i, reg) in self.regs
                .iter()
                .enumerate()
                .take(self.nparams as usize) {
            write!(w, "r{}: ", i)?;
            reg.pp(w, indent)?;
            write!(w, ", ")?;
        }
        write!(w, ") -> ")?;
        self.ret_ty.pp(w, 0)?;
        write!(w, " = {{\n")?;
        for op in self.body.iter() {
            write!(w, "{}", Self::nspaces(indent))?;
            op.pp(w, indent)?;
        }
        write!(w, "}}\n")?;
        Ok(())
    }
}

impl PP for Reg {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        write!(w, "r{:?}", self.1)
    }
}

impl PP for Label {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        write!(w, "{}", (self.0).0)
    }
}

impl PP for Addr {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        write!(w, "[r{}+{}]", (self.0).1, self.1)
    }
}

impl PP for Value {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use lir::Value::*;
        match self {
            &I(ref i) => write!(w, "{}", i),
            &R(ref r) => r.pp(w, indent),
            &F(ref f) => f.pp(w, indent),
        }
    }
}


impl PP for LTy {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use lir::LTy::*;
        let indent = indent + 4;
        match *self {
            Unit => write!(w, "()")?,
            I32 => write!(w, "i32")?,
            I64 => write!(w, "i64")?,
            F32 => write!(w, "f32")?,
            F64 => write!(w, "f64")?,
        }
        Ok(())
    }
}

impl PP for Block {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        write!(w, "{}:\n", (self.name.0).0);
        let indent = indent + 4;
        for op in self.body.iter() {
            write!(w, "{}", Self::nspaces(indent))?;
            op.pp(w, indent)?;
            write!(w, "\n")?;
        }
        Ok(())
    }
}
impl PP for Op {
    fn pp(&self, mut w: &mut io::Write, indent: usize) -> io::Result<()> {
        use lir::Op::*;
        let indent = indent + 4;
        match *self {
            ConstI32(ref reg, ref i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;
            }
            MoveI32(ref r1, ref r2) |
            MoveI64(ref r1, ref r2) |
            MoveF32(ref r1, ref r2) |
            MoveF64(ref r1, ref r2) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
            }
            StoreI32(ref addr, ref v) |
            StoreI64(ref addr, ref v) |
            StoreF32(ref addr, ref v) |
            StoreF64(ref addr, ref v) => {
                addr.pp(w, indent)?;
                write!(w, " <- ")?;
                v.pp(w, indent)?;
            }
            StoreFnPtr(ref addr, ref f) => {
                addr.pp(w, indent)?;
                write!(w, " <- ")?;
                f.pp(w, indent)?;
            }
            LoadI32(ref reg, ref addr) |
            LoadI64(ref reg, ref addr) |
            LoadF32(ref reg, ref addr) |
            LoadF64(ref reg, ref addr) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- ")?;
                addr.pp(w, indent)?;

            }
            JumpIfI32(ref reg, ref label) => {
                write!(w, "jump_if_zero ")?;
                reg.pp(w, indent)?;
                write!(w, " ")?;
                label.pp(w, indent)?;
            }
            ConstI64(ref reg, ref i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;

            }
            AddI64(ref r1, ref r2, ref r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " + ")?;
                r3.pp(w, indent)?;
            }
            MulI64(ref r1, ref r2, ref r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " * ")?;
                r3.pp(w, indent)?;
            }
            ConstF32(ref reg, ref i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;

            }
            AddF32(ref r1, ref r2, ref r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " + ")?;
                r3.pp(w, indent)?;
            }
            MulF32(ref r1, ref r2, ref r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " * ")?;
                r3.pp(w, indent)?;
            }
            ConstF64(ref reg, ref i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;

            }
            AddF64(ref r1, ref r2, ref r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " + ")?;
                r3.pp(w, indent)?;
            }
            MulF64(ref r1, ref r2, ref r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " * ")?;
                r3.pp(w, indent)?;
            }
            HeapAlloc(ref reg, ref value) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- heapalloc(")?;
                value.pp(w, indent)?;
                write!(w, ")")?;
            }
            StackAlloc(ref reg, ref value) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- stackalloc({})", value)?;
            }
            ClosureCall(ref reg, ref name, ref args) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- closure_call ")?;
                name.pp(w, indent)?;
                write!(w, "(")?;
                for arg in args.iter() {
                    arg.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
            FunCall(ref reg, ref name, ref args) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- call ")?;
                name.pp(w, indent)?;
                write!(w, "(")?;
                for arg in args.iter() {
                    arg.pp(w, indent)?;
                    write!(w, ", ")?;
                }
                write!(w, ")")?;
            }
            Jump(ref label) => {
                write!(w, "jump ")?;
                label.pp(w, indent)?;
            }
            Ret(ref reg) => {
                write!(w, "ret ")?;
                reg.as_ref()
                    .map(|r| r.pp(w, indent))
                    .unwrap_or(Ok(()))?;
            }
        };
        Ok(())
    }
}
