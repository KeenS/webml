use mir::*;
use prim::*;

pub struct FunctionBuilder(Function);

impl FunctionBuilder {
    pub fn new(name: Symbol, body_ty: EbbTy) -> Self {
        FunctionBuilder(Function {
            name: name,
            body_ty: body_ty,
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
            name: name,
            params: params,
            body: Vec::new(),
        })
    }

    fn push(&mut self, op: Op) {
        self.0.body.push(op);
    }

    pub fn lit(&mut self, var: Symbol, ty: EbbTy, value: Literal) -> &mut Self {
        self.push(Op::Lit {
            var: var,
            ty: ty,
            value: value,
        });
        self
    }

    pub fn alias(&mut self, var: Symbol, ty: EbbTy, sym: Symbol) -> &mut Self {
        self.push(Op::Alias {
            var: var,
            ty: ty,
            sym: sym,
        });
        self
    }

    pub fn add(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Add {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn sub(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Sub {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn mul(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Mul {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn div_int(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::DivInt {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn div_float(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::DivFloat {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn mod_(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Mod {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn eq(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Eq {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn neq(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Neq {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn gt(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Gt {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn ge(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Ge {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn lt(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Lt {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
        self
    }

    pub fn le(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Le {
            var: var,
            ty: ty,
            l: l,
            r: r,
        });
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
            var: var,
            param_ty: param_ty,
            ret_ty: ret_ty,
            fun: fun,
            env: env,
        });
        self
    }

    pub fn builtin_call(
        &mut self,
        var: Symbol,
        ty: EbbTy,
        fun: BIF,
        args: Vec<Symbol>,
    ) -> &mut Self {
        self.push(Op::BuiltinCall {
            var: var,
            ty: ty,
            fun: fun,
            args: args,
        });
        self
    }

    pub fn call(&mut self, var: Symbol, ty: EbbTy, fun: Symbol, args: Vec<Symbol>) -> &mut Self {
        self.push(Op::Call {
            var: var,
            ty: ty,
            fun: fun,
            args: args,
        });
        self
    }

    pub fn tuple(&mut self, var: Symbol, tys: Vec<EbbTy>, tuple: Vec<Symbol>) -> &mut Self {
        self.push(Op::Tuple {
            var: var,
            tys: tys,
            tuple: tuple,
        });
        self
    }

    pub fn proj(&mut self, var: Symbol, ty: EbbTy, index: u32, tuple: Symbol) -> &mut Self {
        self.push(Op::Proj {
            var: var,
            ty: ty,
            index: index,
            tuple: tuple,
        });
        self
    }

    pub fn branch(mut self, cond: Symbol, clauses: Vec<(u64, Symbol, bool)>) -> EBB {
        self.push(Op::Branch {
            cond: cond,
            clauses: clauses,
        });
        self.0
    }

    pub fn jump(mut self, target: Symbol, forward: bool, args: Vec<Symbol>) -> EBB {
        self.push(Op::Jump {
            target: target,
            forward: forward,
            args: args,
        });
        self.0
    }

    pub fn ret<V: Into<Option<Symbol>>>(mut self, value: V, ty: EbbTy) -> EBB {
        self.push(Op::Ret {
            value: value.into(),
            ty: ty,
        });
        self.0
    }
}
