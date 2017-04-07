use mir::*;
use prim::*;

pub struct MIRBuilder(MIR);

impl MIRBuilder {
    pub fn new() -> Self {
        MIRBuilder(MIR(Vec::new()))
    }

    pub fn add_function(&mut self, f: Function) {
        (self.0).0.push(f);
    }

    pub fn build(self) -> MIR {
        self.0
    }
}

pub struct FunctionBuilder(Function);

type EbbId = usize;

impl FunctionBuilder {
    pub fn new(name: Symbol, body_ty: EbbTy) -> Self {
        FunctionBuilder(Function {
                            name: name,
                            body_ty: body_ty,
                            body: Vec::new(),
                        })
    }

    pub fn entry(&mut self) -> &mut EBB {
        &mut self.0.body[0]
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

    pub fn add_param(&mut self, param: (EbbTy, Symbol)) -> &mut Self {
        self.0.params.push(param);
        self
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

    pub fn mul(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Mul {
                      var: var,
                      ty: ty,
                      l: l,
                      r: r,
                  });
        self
    }

    pub fn closure(&mut self,
                   var: Symbol,
                   param_ty: EbbTy,
                   ret_ty: EbbTy,
                   fun: Symbol,
                   env: Vec<(EbbTy, Symbol)>)
                   -> &mut Self {
        self.push(Op::Closure {
                      var: var,
                      param_ty: param_ty,
                      ret_ty: ret_ty,
                      fun: fun,
                      env: env,
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

    pub fn branch(mut self,
                  cond: Symbol,
                  then: Symbol,
                  tforward: bool,
                  else_: Symbol,
                  eforward: bool)
                  -> EBB {
        self.push(Op::Branch {
                      cond: cond,
                      then: then,
                      tforward: tforward,
                      else_: else_,
                      eforward: eforward,
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
