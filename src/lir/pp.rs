use crate::lir::*;
use crate::util::PP;
use std::io;

impl<T> PP for (T, LIR) {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.1.pp(w, indent)
    }
}

impl PP for LIR {
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
        self.name.pp(w, 0)?;
        write!(w, ": (")?;
        inter_iter! {
            self.regs.iter().enumerate().take(self.nparams as usize),
            write!(w, ", ")?,
            |(i, reg)| => {
                write!(w, "r{}: ", i)?;
                reg.pp(w, indent)?;
            }
        };
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
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        write!(w, "r{:?}", self.1)
    }
}

impl PP for Label {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.0.pp(w, indent)
    }
}

impl PP for Addr {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        write!(w, "[r{}+{}]", (self.0).1, self.1)
    }
}

impl PP for Value {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::lir::Value::*;
        match self {
            I(i) => write!(w, "{}", i),
            R(r) => r.pp(w, indent),
        }
    }
}

impl PP for LTy {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        use crate::lir::LTy::*;
        match self {
            Unit => write!(w, "()")?,
            I32 => write!(w, "i32")?,
            I64 => write!(w, "i64")?,
            F32 => write!(w, "f32")?,
            F64 => write!(w, "f64")?,
            Ptr => write!(w, "ptr")?,
            FPtr => write!(w, "fptr")?,
        }
        Ok(())
    }
}

impl PP for Block {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.name.pp(w, indent)?;
        write!(w, ":\n")?;
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
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::lir::Op::*;
        let indent = indent + 4;
        match self {
            ConstI32(reg, i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;
            }
            MoveI32(r1, r2) | MoveI64(r1, r2) | MoveF32(r1, r2) | MoveF64(r1, r2) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
            }
            StoreI32(addr, v) | StoreI64(addr, v) | StoreF32(addr, v) | StoreF64(addr, v) => {
                addr.pp(w, indent)?;
                write!(w, " <- ")?;
                v.pp(w, indent)?;
            }
            StoreFnPtr(addr, f) => {
                addr.pp(w, indent)?;
                write!(w, " <- ")?;
                f.pp(w, indent)?;
            }
            LoadI32(reg, addr) | LoadI64(reg, addr) | LoadF32(reg, addr) | LoadF64(reg, addr) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- ")?;
                addr.pp(w, indent)?;
            }
            JumpIfI32(reg, label) => {
                write!(w, "jump_if ")?;
                reg.pp(w, indent)?;
                write!(w, " ")?;
                label.pp(w, indent)?;
            }
            JumpTableI32(reg, labels, default) => {
                write!(w, "jump_table ")?;
                reg.pp(w, indent)?;
                write!(w, " ")?;
                let spaces = Self::nspaces(indent + 4);
                for label in labels {
                    write!(w, "\n{}", spaces)?;
                    label.pp(w, indent)?;
                }
                for label in default {
                    write!(w, "\n{}default ", spaces)?;
                    label.pp(w, indent)?;
                }
            }
            ConstI64(reg, i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;
            }
            AddI32(r1, r2, r3) | AddI64(r1, r2, r3) | AddF32(r1, r2, r3) | AddF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " + ")?;
                r3.pp(w, indent)?;
            }
            SubI32(r1, r2, r3) | SubI64(r1, r2, r3) | SubF32(r1, r2, r3) | SubF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " - ")?;
                r3.pp(w, indent)?;
            }
            MulI32(r1, r2, r3) | MulI64(r1, r2, r3) | MulF32(r1, r2, r3) | MulF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " * ")?;
                r3.pp(w, indent)?;
            }
            DivI32(r1, r2, r3) | DivI64(r1, r2, r3) | DivF32(r1, r2, r3) | DivF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " / ")?;
                r3.pp(w, indent)?;
            }
            ModI32(r1, r2, r3) | ModI64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " mod ")?;
                r3.pp(w, indent)?;
            }
            EqI32(r1, r2, r3) | EqI64(r1, r2, r3) | EqF32(r1, r2, r3) | EqF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " = ")?;
                r3.pp(w, indent)?;
            }
            NeqI32(r1, r2, r3) | NeqI64(r1, r2, r3) | NeqF32(r1, r2, r3) | NeqF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " <> ")?;
                r3.pp(w, indent)?;
            }
            GtI32(r1, r2, r3) | GtI64(r1, r2, r3) | GtF32(r1, r2, r3) | GtF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " > ")?;
                r3.pp(w, indent)?;
            }
            GeI32(r1, r2, r3) | GeI64(r1, r2, r3) | GeF32(r1, r2, r3) | GeF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " >= ")?;
                r3.pp(w, indent)?;
            }
            LtI32(r1, r2, r3) | LtI64(r1, r2, r3) | LtF32(r1, r2, r3) | LtF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " < ")?;
                r3.pp(w, indent)?;
            }
            LeI32(r1, r2, r3) | LeI64(r1, r2, r3) | LeF32(r1, r2, r3) | LeF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " <= ")?;
                r3.pp(w, indent)?;
            }
            ConstF32(reg, i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;
            }
            ConstF64(reg, i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;
            }
            HeapAlloc(reg, value, tys) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- heapalloc(")?;
                value.pp(w, indent)?;
                write!(w, ", ")?;
                write!(w, "{:?}", tys.as_slice())?;
                write!(w, ")")?;
            }
            StackAlloc(reg, value, tys) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- stackalloc({}, {:?})", value, tys)?;
            }
            ClosureCall(reg, name, args) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- closure_call ")?;
                name.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter! {
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => arg.pp(w, indent)?
                };
                write!(w, ")")?;
            }
            FunCall(reg, name, args) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- call ")?;
                name.pp(w, indent)?;
                write!(w, "(")?;
                inter_iter! {
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => arg.pp(w, indent)?
                }
                write!(w, ")")?;
            }
            ExternCall(reg, module, name, args) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- extern call \"{}\" \"{}\"", module, name)?;
                write!(w, "(")?;
                inter_iter! {
                    args.iter(),
                    write!(w, ", ")?,
                    |arg| => arg.pp(w, indent)?
                }
                write!(w, ")")?;
            }
            Jump(label) => {
                write!(w, "jump ")?;
                label.pp(w, indent)?;
            }
            Unreachable => {
                write!(w, "unreachable")?;
            }
            Ret(reg) => {
                write!(w, "ret ")?;
                reg.as_ref().map(|r| r.pp(w, indent)).unwrap_or(Ok(()))?;
            }
        };
        Ok(())
    }
}
