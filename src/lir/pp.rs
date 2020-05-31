use crate::lir::*;
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

impl PP for LIR {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        for fun in &self.0 {
            fun.pp(w, indent)?;
        }
        Ok(())
    }
}

impl fmt::Display for LIR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        for fun in &self.0 {
            write!(f, "{:indent$}", fun, indent = indent)?;
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        let indent = indent + 4;
        write!(f, "fun {}: (", self.name)?;
        inter_iter! {
            self.regs.iter().enumerate().take(self.nparams as usize),
            write!(f, ", ")?,
            |(i, reg)| => {
                write!(f, "r{}: {}", i, reg)?;
            }
        };
        write!(f, ") -> {} = {{\n", self.ret_ty)?;
        for op in self.body.iter() {
            write!(f, "{}{:indent$}", nspaces(indent), op, indent = indent)?;
        }
        write!(f, "}}\n")?;
        Ok(())
    }
}
impl PP for Reg {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        write!(w, "r{:?}", self.1)
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "r{:?}", self.1)
    }
}

impl PP for Label {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        self.0.pp(w, indent)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PP for Addr {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        write!(w, "[r{}+{}]", (self.0).1, self.1)
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[r{}+{}]", (self.0).1, self.1)
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::lir::Value::*;
        match self {
            I(i) => write!(f, "{}", i),
            R(r) => write!(f, "{}", r),
        }
    }
}
impl PP for LTy {
    fn pp<W: io::Write>(&self, w: &mut W, _indent: usize) -> io::Result<()> {
        use crate::lir::LTy::*;
        match self {
            Unit => write!(w, "()")?,
            I32 => write!(w, "i32")?,
            U32 => write!(w, "u32")?,
            I64 => write!(w, "i64")?,
            U64 => write!(w, "u64")?,
            F32 => write!(w, "f32")?,
            F64 => write!(w, "f64")?,
            Ptr => write!(w, "ptr")?,
            FPtr => write!(w, "fptr")?,
        }
        Ok(())
    }
}

impl fmt::Display for LTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::lir::LTy::*;
        match self {
            Unit => write!(f, "()")?,
            I32 => write!(f, "i32")?,
            U32 => write!(f, "u32")?,
            I64 => write!(f, "i64")?,
            U64 => write!(f, "u64")?,
            F32 => write!(f, "f32")?,
            F64 => write!(f, "f64")?,
            Ptr => write!(f, "ptr")?,
            FPtr => write!(f, "fptr")?,
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

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let indent = f.width().unwrap_or(0);
        let indent = indent + 4;
        write!(f, "{}:\n", self.name)?;
        for op in self.body.iter() {
            write!(f, "{}{}\n", nspaces(indent), op)?;
        }
        Ok(())
    }
}

impl PP for Op {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()> {
        use crate::lir::Op::*;
        let indent = indent + 4;
        match self {
            ConstI32(reg, i) | ConstU32(reg, i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;
            }
            MoveI32(r1, r2)
            | MoveU32(r1, r2)
            | MoveI64(r1, r2)
            | MoveU64(r1, r2)
            | MoveF32(r1, r2)
            | MoveF64(r1, r2) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
            }
            StoreI32(addr, v)
            | StoreU32(addr, v)
            | StoreI64(addr, v)
            | StoreU64(addr, v)
            | StoreF32(addr, v)
            | StoreF64(addr, v) => {
                addr.pp(w, indent)?;
                write!(w, " <- ")?;
                v.pp(w, indent)?;
            }
            StoreFnPtr(addr, f) => {
                addr.pp(w, indent)?;
                write!(w, " <- ")?;
                f.pp(w, indent)?;
            }
            LoadI32(reg, addr)
            | LoadU32(reg, addr)
            | LoadI64(reg, addr)
            | LoadU64(reg, addr)
            | LoadF32(reg, addr)
            | LoadF64(reg, addr) => {
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
            ConstI64(reg, i) | ConstU64(reg, i) => {
                reg.pp(w, indent)?;
                write!(w, ": ")?;
                reg.0.pp(w, indent)?;
                write!(w, " <- {}", i)?;
            }
            AddI32(r1, r2, r3)
            | AddU32(r1, r2, r3)
            | AddI64(r1, r2, r3)
            | AddU64(r1, r2, r3)
            | AddF32(r1, r2, r3)
            | AddF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " + ")?;
                r3.pp(w, indent)?;
            }
            SubI32(r1, r2, r3)
            | SubU32(r1, r2, r3)
            | SubI64(r1, r2, r3)
            | SubU64(r1, r2, r3)
            | SubF32(r1, r2, r3)
            | SubF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " - ")?;
                r3.pp(w, indent)?;
            }
            MulI32(r1, r2, r3)
            | MulU32(r1, r2, r3)
            | MulI64(r1, r2, r3)
            | MulU64(r1, r2, r3)
            | MulF32(r1, r2, r3)
            | MulF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " * ")?;
                r3.pp(w, indent)?;
            }
            DivI32(r1, r2, r3)
            | DivU32(r1, r2, r3)
            | DivI64(r1, r2, r3)
            | DivU64(r1, r2, r3)
            | DivF32(r1, r2, r3)
            | DivF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " / ")?;
                r3.pp(w, indent)?;
            }
            ModI32(r1, r2, r3) | ModU32(r1, r2, r3) | ModI64(r1, r2, r3) | ModU64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " mod ")?;
                r3.pp(w, indent)?;
            }
            EqI32(r1, r2, r3)
            | EqU32(r1, r2, r3)
            | EqI64(r1, r2, r3)
            | EqU64(r1, r2, r3)
            | EqF32(r1, r2, r3)
            | EqF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " = ")?;
                r3.pp(w, indent)?;
            }
            NeqI32(r1, r2, r3)
            | NeqU32(r1, r2, r3)
            | NeqI64(r1, r2, r3)
            | NeqU64(r1, r2, r3)
            | NeqF32(r1, r2, r3)
            | NeqF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " <> ")?;
                r3.pp(w, indent)?;
            }
            GtI32(r1, r2, r3)
            | GtU32(r1, r2, r3)
            | GtI64(r1, r2, r3)
            | GtU64(r1, r2, r3)
            | GtF32(r1, r2, r3)
            | GtF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " > ")?;
                r3.pp(w, indent)?;
            }
            GeI32(r1, r2, r3)
            | GeU32(r1, r2, r3)
            | GeI64(r1, r2, r3)
            | GeU64(r1, r2, r3)
            | GeF32(r1, r2, r3)
            | GeF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " >= ")?;
                r3.pp(w, indent)?;
            }
            LtI32(r1, r2, r3)
            | LtU32(r1, r2, r3)
            | LtI64(r1, r2, r3)
            | LtU64(r1, r2, r3)
            | LtF32(r1, r2, r3)
            | LtF64(r1, r2, r3) => {
                r1.pp(w, indent)?;
                write!(w, ": ")?;
                r1.0.pp(w, indent)?;
                write!(w, " <- ")?;
                r2.pp(w, indent)?;
                write!(w, " < ")?;
                r3.pp(w, indent)?;
            }
            LeI32(r1, r2, r3)
            | LeU32(r1, r2, r3)
            | LeI64(r1, r2, r3)
            | LeU64(r1, r2, r3)
            | LeF32(r1, r2, r3)
            | LeF64(r1, r2, r3) => {
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

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::lir::Op::*;
        let indent = f.width().unwrap_or(0);
        let indent = indent + 4;
        match self {
            ConstI32(reg, i) | ConstU32(reg, i) => write!(f, "{}: {} <- {}", reg, reg.0, i)?,
            MoveI32(r1, r2)
            | MoveU32(r1, r2)
            | MoveI64(r1, r2)
            | MoveU64(r1, r2)
            | MoveF32(r1, r2)
            | MoveF64(r1, r2) => write!(f, "{}: {} <- {}", r1, r1.0, r2)?,
            StoreI32(addr, v)
            | StoreU32(addr, v)
            | StoreI64(addr, v)
            | StoreU64(addr, v)
            | StoreF32(addr, v)
            | StoreF64(addr, v) => write!(f, "{} <- {}", addr, v)?,
            StoreFnPtr(addr, fp) => write!(f, "{} <- {}", addr, fp)?,
            LoadI32(reg, addr)
            | LoadU32(reg, addr)
            | LoadI64(reg, addr)
            | LoadU64(reg, addr)
            | LoadF32(reg, addr)
            | LoadF64(reg, addr) => write!(f, "{}: {} <- {}", reg, reg.0, addr)?,
            JumpIfI32(reg, label) => write!(f, "jump_if {} {}", reg, label)?,
            JumpTableI32(reg, labels, default) => {
                write!(f, "jump_table {} ", reg)?;
                let spaces = nspaces(indent + 4);
                for label in labels {
                    write!(f, "\n{}{}", spaces, label)?;
                }
                for label in default {
                    write!(f, "\n{}default {}", spaces, label)?;
                }
            }
            ConstI64(reg, i) | ConstU64(reg, i) => write!(f, "{}: {} <- {}", reg, reg.0, i)?,
            AddI32(r1, r2, r3)
            | AddU32(r1, r2, r3)
            | AddI64(r1, r2, r3)
            | AddU64(r1, r2, r3)
            | AddF32(r1, r2, r3)
            | AddF64(r1, r2, r3) => write!(f, "{}: {} <- {} + {}", r1, r1.0, r2, r3)?,
            SubI32(r1, r2, r3)
            | SubU32(r1, r2, r3)
            | SubI64(r1, r2, r3)
            | SubU64(r1, r2, r3)
            | SubF32(r1, r2, r3)
            | SubF64(r1, r2, r3) => write!(f, "{}: {} <- {} - {}", r1, r1.0, r2, r3)?,
            MulI32(r1, r2, r3)
            | MulU32(r1, r2, r3)
            | MulI64(r1, r2, r3)
            | MulU64(r1, r2, r3)
            | MulF32(r1, r2, r3)
            | MulF64(r1, r2, r3) => write!(f, "{}: {} <- {} * {}", r1, r1.0, r2, r3)?,
            DivI32(r1, r2, r3)
            | DivU32(r1, r2, r3)
            | DivI64(r1, r2, r3)
            | DivU64(r1, r2, r3)
            | DivF32(r1, r2, r3)
            | DivF64(r1, r2, r3) => write!(f, "{}: {} <- {} / {}", r1, r1.0, r2, r3)?,
            ModI32(r1, r2, r3) | ModU32(r1, r2, r3) | ModI64(r1, r2, r3) | ModU64(r1, r2, r3) => {
                write!(f, "{}: {} <- {} mod {}", r1, r1.0, r2, r3)?
            }
            EqI32(r1, r2, r3)
            | EqU32(r1, r2, r3)
            | EqI64(r1, r2, r3)
            | EqU64(r1, r2, r3)
            | EqF32(r1, r2, r3)
            | EqF64(r1, r2, r3) => write!(f, "{}: {} <- {} = {}", r1, r1.0, r2, r3)?,
            NeqI32(r1, r2, r3)
            | NeqU32(r1, r2, r3)
            | NeqI64(r1, r2, r3)
            | NeqU64(r1, r2, r3)
            | NeqF32(r1, r2, r3)
            | NeqF64(r1, r2, r3) => write!(f, "{}: {} <- {} <> {}", r1, r1.0, r2, r3)?,
            GtI32(r1, r2, r3)
            | GtU32(r1, r2, r3)
            | GtI64(r1, r2, r3)
            | GtU64(r1, r2, r3)
            | GtF32(r1, r2, r3)
            | GtF64(r1, r2, r3) => write!(f, "{}: {} <- {} > {}", r1, r1.0, r2, r3)?,
            GeI32(r1, r2, r3)
            | GeU32(r1, r2, r3)
            | GeI64(r1, r2, r3)
            | GeU64(r1, r2, r3)
            | GeF32(r1, r2, r3)
            | GeF64(r1, r2, r3) => write!(f, "{}: {} <- {} >= {}", r1, r1.0, r2, r3)?,
            LtI32(r1, r2, r3)
            | LtU32(r1, r2, r3)
            | LtI64(r1, r2, r3)
            | LtU64(r1, r2, r3)
            | LtF32(r1, r2, r3)
            | LtF64(r1, r2, r3) => write!(f, "{}: {} <- {} < {}", r1, r1.0, r2, r3)?,
            LeI32(r1, r2, r3)
            | LeU32(r1, r2, r3)
            | LeI64(r1, r2, r3)
            | LeU64(r1, r2, r3)
            | LeF32(r1, r2, r3)
            | LeF64(r1, r2, r3) => write!(f, "{}: {} <- {} <= {}", r1, r1.0, r2, r3)?,
            ConstF32(reg, i) => write!(f, "{}: {} <- {}", reg, reg.0, i)?,
            ConstF64(reg, i) => write!(f, "{}: {} <- {}", reg, reg.0, i)?,
            HeapAlloc(reg, value, tys) => write!(
                f,
                "{}: {} <- heapalloc({},{:?})",
                reg,
                reg.0,
                value,
                tys.as_slice()
            )?,
            StackAlloc(reg, value, tys) => {
                write!(f, "{}: {} <- stackalloc({}, {:?})", reg, reg.0, value, tys)?
            }
            ClosureCall(reg, name, args) => {
                write!(f, "{}: {} <- closure_call {}(", reg, reg.0, name)?;
                inter_iter! {
                    args.iter(),
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f, "{}", arg)?;
                    }
                };
                write!(f, ")")?;
            }
            FunCall(reg, name, args) => {
                write!(f, "{}: {} <- call {}(", reg, reg.0, name)?;
                inter_iter! {
                    args.iter(),
                    write!(f, ", ")?,
                    |arg| => {
                        write!(f, "{}", arg)?;
                    }
                }
                write!(f, ")")?;
            }
            ExternCall(reg, module, name, args) => {
                write!(
                    f,
                    "{}: {} <- extern call \"{}\" \"{}\"(",
                    reg, reg.0, module, name
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
            Jump(label) => {
                write!(f, "jump {}", label)?;
            }
            Unreachable => {
                write!(f, "unreachable")?;
            }
            Ret(reg) => {
                write!(f, "ret ")?;
                reg.as_ref().map(|r| write!(f, "{}", r)).unwrap_or(Ok(()))?;
            }
        };
        Ok(())
    }
}
