use crate::mir::*;
use crate::prim::*;

pub struct FunctionBuilder(Function);

impl FunctionBuilder {
    pub fn new(name: Symbol, body_ty: EbbTy) -> Self {
        FunctionBuilder(Function {
            name,
            body_ty,
            body: Vec::new(),
        })
    }

    pub fn add_ebb(&mut self, ebb: EBB) {
        self.0.body.push(ebb);
    }

    pub fn build(self) -> Function {
        self.0
    }
}

pub struct EBBBuilder(EBB);

impl EBBBuilder {
    pub fn new(name: Symbol, params: Vec<(EbbTy, Symbol)>) -> Self {
        EBBBuilder(EBB {
            name,
            params,
            body: Vec::new(),
        })
    }

    fn push(&mut self, op: Op) {
        self.0.body.push(op);
    }

    pub fn lit(&mut self, var: Symbol, ty: EbbTy, value: Literal) -> &mut Self {
        self.push(Op::Lit { var, ty, value });
        self
    }

    pub fn alias(&mut self, var: Symbol, ty: EbbTy, sym: Symbol) -> &mut Self {
        self.push(Op::Alias { var, ty, sym });
        self
    }

    pub fn add(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Add { var, ty, l, r });
        self
    }

    pub fn sub(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Sub { var, ty, l, r });
        self
    }

    pub fn mul(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Mul { var, ty, l, r });
        self
    }

    pub fn div_int(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::DivInt { var, ty, l, r });
        self
    }

    pub fn div_float(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::DivFloat { var, ty, l, r });
        self
    }

    pub fn mod_(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Mod { var, ty, l, r });
        self
    }

    pub fn eq(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Eq { var, ty, l, r });
        self
    }

    pub fn neq(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Neq { var, ty, l, r });
        self
    }

    pub fn gt(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Gt { var, ty, l, r });
        self
    }

    pub fn ge(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Ge { var, ty, l, r });
        self
    }

    pub fn lt(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Lt { var, ty, l, r });
        self
    }

    pub fn le(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Le { var, ty, l, r });
        self
    }

    pub fn closure(
        &mut self,
        var: Symbol,
        param_ty: EbbTy,
        ret_ty: EbbTy,
        fun: Symbol,
        env: Vec<(EbbTy, Symbol)>,
    ) -> &mut Self {
        self.push(Op::Closure {
            var,
            param_ty,
            ret_ty,
            fun,
            env,
        });
        self
    }

    pub fn extern_call(
        &mut self,
        var: Symbol,
        ty: EbbTy,
        module: String,
        fun: String,
        args: Vec<Symbol>,
    ) -> &mut Self {
        self.push(Op::ExternCall {
            var,
            ty,
            module,
            fun,
            args,
        });
        self
    }

    pub fn call(&mut self, var: Symbol, ty: EbbTy, fun: Symbol, args: Vec<Symbol>) -> &mut Self {
        self.push(Op::Call { var, ty, fun, args });
        self
    }

    pub fn tuple(&mut self, var: Symbol, tys: Vec<EbbTy>, tuple: Vec<Symbol>) -> &mut Self {
        self.push(Op::Tuple { var, tys, tuple });
        self
    }

    pub fn proj(&mut self, var: Symbol, ty: EbbTy, index: u32, tuple: Symbol) -> &mut Self {
        self.push(Op::Proj {
            var,
            ty,
            index,
            tuple,
        });
        self
    }

    pub fn union(
        &mut self,
        var: Symbol,
        tys: Vec<EbbTy>,
        index: u32,
        variant: Symbol,
    ) -> &mut Self {
        self.push(Op::Union {
            var,
            tys,
            index,
            variant,
        });
        self
    }

    pub fn select(&mut self, var: Symbol, ty: EbbTy, index: u32, union: Symbol) -> &mut Self {
        self.push(Op::Select {
            var,
            ty,
            index,
            union,
        });
        self
    }

    pub fn branch(
        mut self,
        cond: Symbol,
        clauses: Vec<(u32, Symbol, bool)>,
        default: Option<(Symbol, bool)>,
    ) -> EBB {
        self.push(Op::Branch {
            cond,
            clauses,
            default,
        });
        self.0
    }

    pub fn jump(mut self, target: Symbol, forward: bool, args: Vec<Symbol>) -> EBB {
        self.push(Op::Jump {
            target,
            forward,
            args,
        });
        self.0
    }

    pub fn ret<V: Into<Option<Symbol>>>(mut self, value: V, ty: EbbTy) -> EBB {
        self.push(Op::Ret {
            value: value.into(),
            ty,
        });
        self.0
    }
}
