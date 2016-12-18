use std::rc::Rc;
use prim::*;

pub struct MIR(Vec<Function>);

pub struct Function {
    name: Symbol,
    entry: Rc<EBB>,
    body: Vec<Rc<EBB>>,
    body_ty: EbbTy,
}

pub struct EBB {
    name: Symbol,
    params: Vec<Symbol>,
    params_ty: Vec<EbbTy>,
    body: Vec<Op>,
}

pub enum Op {
    Lit{var: Symbol, ty: Ty, value: Literal},
    Add{var: Symbol, ty: Ty, l: Symbol, r: Symbol},
    Mul{var: Symbol, ty: Ty, l: Symbol, r: Symbol},
    Call{var: Symbol, ty: Ty, fun: Rc<EBB>, args: Vec<Symbol>},
    Branch {cond: Symbol, then: Rc<EBB>, else_: Rc<EBB>},
    Jump {target: Rc<EBB>},
    Ret {value: Symbol, ty: Ty}
}

pub enum EbbTy {
    Unit,
    Int,
    Bool,
    Ebb{param: Vec<EbbTy>, ret: Box<EbbTy>}
}

struct HIR2MIR;


