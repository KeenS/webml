pub mod mir2lir;
pub mod pp;

pub use self::mir2lir::MIR2LIR;
use prim::*;

#[derive(Debug, Clone)]
pub enum LTy {
    I32,
    I64,
}

impl LTy {
    pub fn size(&self) -> u32 {
        use self::LTy::*;
        match self {
            &I32 => 4,
            &I64 => 8,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reg(pub LTy, pub u32);
#[derive(Debug, Clone)]
pub struct Label(Symbol);

#[derive(Debug, Clone)]
pub struct Addr(Reg, u32);

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
    pub body: Vec<Op>,
}


#[derive(Debug, Clone)]
pub enum Op {
    ConstI32(Reg, u32),
    MoveI32(Reg, Reg),
    StoreI32(Addr, Value),
    LoadI32(Reg, Addr),
    JumpIfI32(Reg, Label),

    ConstI64(Reg, u64),
    MoveI64(Reg, Reg),
    AddI64(Reg, Reg, Reg),
    MulI64(Reg, Reg, Reg),
    StoreI64(Addr, Value),
    LoadI64(Reg, Addr),

    HeapAlloc(Reg, Value),
    StackAlloc(Reg, u32),

    Call(Reg, Value, Vec<Reg>),
    Jump(Label),
    Ret(Reg),
}
