pub mod pp;
mod unalias;
mod builder;
mod hir2mir;
mod block_arrange;
pub mod cfg;

pub use self::hir2mir::HIR2MIR;
pub use self::unalias::UnAlias;
pub use self::block_arrange::BlockArrange;
use prim::*;

#[derive(Debug, Clone)]
pub struct MIR(pub Vec<Function>);

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Symbol,
    // pub params: Vec<Symbol>,
    // pub params_ty: Vec<EbbTy>,
    pub body: Vec<EBB>,
    pub body_ty: EbbTy,
}

#[derive(Debug, Clone)]
pub struct EBB {
    pub name: Symbol,
    pub params: Vec<(EbbTy, Symbol)>,
    pub body: Vec<Op>,
}

#[derive(Debug, Clone)]
pub enum Op {
    Lit {
        var: Symbol,
        ty: EbbTy,
        value: Literal,
    },
    Alias { var: Symbol, ty: EbbTy, sym: Symbol },
    Add {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Sub {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Mul {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    DivInt {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    DivFloat {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Mod {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Eq {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Neq {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Gt {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Ge {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Lt {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Le {
        var: Symbol,
        ty: EbbTy,
        l: Symbol,
        r: Symbol,
    },
    Closure {
        var: Symbol,
        param_ty: EbbTy,
        ret_ty: EbbTy,
        fun: Symbol,
        env: Vec<(EbbTy, Symbol)>,
    },
    // TODO: separate closure call and direct call
    Call {
        var: Symbol,
        ty: EbbTy,
        fun: Symbol,
        args: Vec<Symbol>,
    },
    Tuple {
        var: Symbol,
        tys: Vec<EbbTy>,
        tuple: Vec<Symbol>,
    },
    Proj {
        var: Symbol,
        ty: EbbTy,
        index: u32,
        tuple: Symbol,
    },

    Branch {
        cond: Symbol,
        then: Symbol,
        tforward: bool,
        else_: Symbol,
        eforward: bool,
    },
    Jump {
        target: Symbol,
        forward: bool,
        args: Vec<Symbol>,
    },
    Ret { value: Option<Symbol>, ty: EbbTy },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EbbTy {
    Unit,
    Int,
    Float,
    Bool,
    Tuple(Vec<EbbTy>),
    Cls {
        closures: Vec<EbbTy>,
        param: Box<EbbTy>,
        ret: Box<EbbTy>,
    },
    Ebb { params: Vec<EbbTy>, ret: Box<EbbTy> },
}

impl MIR {
    pub fn add(&mut self, f: Function) {
        self.0.push(f)
    }
}
