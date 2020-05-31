pub mod mir2lir;
pub mod pp;

pub use self::mir2lir::MIR2LIR;
use crate::prim::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LTy {
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    Unit,
    Ptr,
    FPtr,
}

impl LTy {
    pub fn size(&self) -> u32 {
        use self::LTy::*;
        match *self {
            I32 | U32 | F32 => 4,
            I64 | U64 | F64 => 8,
            // pointer size is 4
            FPtr | Ptr => 4,
            Unit => 0,
        }
    }

    pub fn is_ptr(&self) -> bool {
        use self::LTy::*;
        match *self {
            Ptr | FPtr => true,
            I32 | U32 | I64 | U64 | F32 | F64 | Unit => false,
        }
    }
}

pub type ExternTypes = HashMap<(String, String), (Vec<LTy>, LTy)>;

#[derive(Debug, Clone)]
pub struct Reg(pub LTy, pub u32);
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Label(pub Symbol);

#[derive(Debug, Clone)]
pub struct Addr(
    pub Reg,
    /// offset, in bytes
    pub u32,
);

#[derive(Debug, Clone)]
pub enum Value {
    // immediate
    I(i32),
    // register
    R(Reg),
    // function pointer
    //    F(Symbol),
}

#[derive(Debug, Clone)]
pub struct Context(pub ExternTypes, pub LIR);

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
    AddI32(Reg, Reg, Reg),
    SubI32(Reg, Reg, Reg),
    MulI32(Reg, Reg, Reg),
    DivI32(Reg, Reg, Reg),
    ModI32(Reg, Reg, Reg),
    EqI32(Reg, Reg, Reg),
    NeqI32(Reg, Reg, Reg),
    GtI32(Reg, Reg, Reg),
    GeI32(Reg, Reg, Reg),
    LtI32(Reg, Reg, Reg),
    LeI32(Reg, Reg, Reg),
    StoreI32(Addr, Reg),
    LoadI32(Reg, Addr),

    JumpIfI32(Reg, Label),
    JumpTableI32(Reg, Vec<Label>, Option<Label>),

    ConstU32(Reg, u32),
    MoveU32(Reg, Reg),
    AddU32(Reg, Reg, Reg),
    SubU32(Reg, Reg, Reg),
    MulU32(Reg, Reg, Reg),
    DivU32(Reg, Reg, Reg),
    ModU32(Reg, Reg, Reg),
    EqU32(Reg, Reg, Reg),
    NeqU32(Reg, Reg, Reg),
    GtU32(Reg, Reg, Reg),
    GeU32(Reg, Reg, Reg),
    LtU32(Reg, Reg, Reg),
    LeU32(Reg, Reg, Reg),
    StoreU32(Addr, Reg),
    LoadU32(Reg, Addr),

    ConstI64(Reg, u64),
    MoveI64(Reg, Reg),
    AddI64(Reg, Reg, Reg),
    SubI64(Reg, Reg, Reg),
    MulI64(Reg, Reg, Reg),
    DivI64(Reg, Reg, Reg),
    ModI64(Reg, Reg, Reg),
    EqI64(Reg, Reg, Reg),
    NeqI64(Reg, Reg, Reg),
    GtI64(Reg, Reg, Reg),
    GeI64(Reg, Reg, Reg),
    LtI64(Reg, Reg, Reg),
    LeI64(Reg, Reg, Reg),
    StoreI64(Addr, Reg),
    LoadI64(Reg, Addr),

    ConstU64(Reg, u64),
    MoveU64(Reg, Reg),
    AddU64(Reg, Reg, Reg),
    SubU64(Reg, Reg, Reg),
    MulU64(Reg, Reg, Reg),
    DivU64(Reg, Reg, Reg),
    ModU64(Reg, Reg, Reg),
    EqU64(Reg, Reg, Reg),
    NeqU64(Reg, Reg, Reg),
    GtU64(Reg, Reg, Reg),
    GeU64(Reg, Reg, Reg),
    LtU64(Reg, Reg, Reg),
    LeU64(Reg, Reg, Reg),
    StoreU64(Addr, Reg),
    LoadU64(Reg, Addr),

    ConstF32(Reg, f32),
    MoveF32(Reg, Reg),
    AddF32(Reg, Reg, Reg),
    SubF32(Reg, Reg, Reg),
    MulF32(Reg, Reg, Reg),
    DivF32(Reg, Reg, Reg),
    EqF32(Reg, Reg, Reg),
    NeqF32(Reg, Reg, Reg),
    GtF32(Reg, Reg, Reg),
    GeF32(Reg, Reg, Reg),
    LtF32(Reg, Reg, Reg),
    LeF32(Reg, Reg, Reg),
    StoreF32(Addr, Reg),
    LoadF32(Reg, Addr),

    ConstF64(Reg, f64),
    MoveF64(Reg, Reg),
    AddF64(Reg, Reg, Reg),
    SubF64(Reg, Reg, Reg),
    MulF64(Reg, Reg, Reg),
    DivF64(Reg, Reg, Reg),
    EqF64(Reg, Reg, Reg),
    NeqF64(Reg, Reg, Reg),
    GtF64(Reg, Reg, Reg),
    GeF64(Reg, Reg, Reg),
    LtF64(Reg, Reg, Reg),
    LeF64(Reg, Reg, Reg),
    StoreF64(Addr, Reg),
    LoadF64(Reg, Addr),

    HeapAlloc(Reg, Value, Vec<LTy>),
    StackAlloc(Reg, u32, Vec<LTy>),

    StoreFnPtr(Addr, Symbol),
    ExternCall(Reg, String, String, Vec<Reg>),
    FunCall(Reg, Symbol, Vec<Reg>),
    ClosureCall(Reg, Reg, Vec<Reg>),
    Jump(Label),
    Unreachable,
    Ret(Option<Reg>),
}

impl Block {
    pub fn branches(&self) -> Vec<&Label> {
        use self::Op::*;
        self.body
            .iter()
            .flat_map(|op| match *op {
                Jump(ref label) => vec![label],
                JumpIfI32(_, ref label) => vec![label],
                JumpTableI32(_, ref labels, ref default) => {
                    labels.iter().chain(default.iter()).collect()
                }
                _ => vec![],
            })
            .collect()
    }
}
