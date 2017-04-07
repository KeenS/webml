pub mod mir2lir;
pub mod pp;

pub use self::mir2lir::MIR2LIR;
use prim::*;

#[derive(Debug, Clone)]
pub enum LTy {
    I32,
    I64,
    F32,
    F64,
    Unit,
}

impl LTy {
    pub fn size(&self) -> u32 {
        use self::LTy::*;
        match *self {
            I32 | F32 => 4,
            I64 | F64 => 8,
            Unit => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reg(pub LTy, pub u32);
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Label(pub Symbol);

#[derive(Debug, Clone)]
pub struct Addr(pub Reg, pub u32);

#[derive(Debug, Clone)]
pub enum Value {
    // immediate
    I(i32),
    // register
    R(Reg),
    // function pointer
    F(Symbol),
}

pub const PTR: LTy = LTy::I64;

#[derive(Debug, Clone)]
pub struct LIR(pub Vec<Function>);

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    pub nparams: u32,
    pub regs: Vec<LTy>,
    pub ret_ty: LTy,
    pub body: Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub name: Label,
    pub body: Vec<Op>,
}


#[derive(Debug, Clone)]
pub enum Op {
    ConstI32(Reg, u32),
    MoveI32(Reg, Reg),
    StoreI32(Addr, Reg),
    LoadI32(Reg, Addr),
    JumpIfI32(Reg, Label),

    ConstI64(Reg, u64),
    MoveI64(Reg, Reg),
    AddI64(Reg, Reg, Reg),
    MulI64(Reg, Reg, Reg),
    StoreI64(Addr, Reg),
    LoadI64(Reg, Addr),

    ConstF32(Reg, f32),
    MoveF32(Reg, Reg),
    AddF32(Reg, Reg, Reg),
    MulF32(Reg, Reg, Reg),
    StoreF32(Addr, Reg),
    LoadF32(Reg, Addr),

    ConstF64(Reg, f64),
    MoveF64(Reg, Reg),
    AddF64(Reg, Reg, Reg),
    MulF64(Reg, Reg, Reg),
    StoreF64(Addr, Reg),
    LoadF64(Reg, Addr),

    HeapAlloc(Reg, Value),
    StackAlloc(Reg, u32),

    StoreFnPtr(Addr, Symbol),
    FunCall(Reg, Symbol, Vec<Reg>),
    ClosureCall(Reg, Reg, Vec<Reg>),
    Jump(Label),
    Ret(Option<Reg>),
}


impl Block {
    pub fn branches(&self) -> Vec<&Label> {
        use self::Op::*;
        self.body
            .iter()
            .filter_map(|op| match *op {
                            Jump(ref label) => Some(label),
                            JumpIfI32(_, ref label) => Some(label),
                            _ => None,
                        })
            .collect()
    }
}
