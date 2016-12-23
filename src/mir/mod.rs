pub mod pp;
mod unalias;

pub use self::unalias::UnAlias;
use pass::Pass;
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
    Lit{var: Symbol, ty: EbbTy, value: Literal},
    Alias{var: Symbol, ty: EbbTy, sym: Symbol},
    Add{var: Symbol, ty: EbbTy, l: Symbol, r: Symbol},
    Mul{var: Symbol, ty: EbbTy, l: Symbol, r: Symbol},
    Closure{var: Symbol, param_ty: EbbTy, ret_ty: EbbTy, fun: Symbol, env: Vec<(EbbTy, Symbol)>},
    // TODO: separate closure call and direct call
    Call{var: Symbol, ty: EbbTy, fun: Symbol, args: Vec<Symbol>},
    Branch {cond: Symbol, then: Symbol, else_: Symbol},
    Jump {target: Symbol, args: Vec<Symbol>},
    Ret {value: Symbol, ty: EbbTy}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EbbTy {
    Unit,
    Int,
    Bool,
    Cls{param: Box<EbbTy>, ret: Box<EbbTy>},
    Ebb{params: Vec<EbbTy>, ret: Box<EbbTy>}
}

impl MIR {
    pub fn add(&mut self, f: Function) {
        self.0.push(f)
    }
}

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
        FunctionBuilder(Function{
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
        self.push(Op::Lit{var: var, ty: ty, value: value});
        self
    }

    pub fn alias(&mut self, var: Symbol, ty: EbbTy, sym: Symbol) -> &mut Self {
        self.push(Op::Alias{var: var, ty: ty, sym: sym});
        self
    }

    pub fn add(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Add{var: var, ty: ty, l: l, r: r});
        self
    }

    pub fn mul(&mut self, var: Symbol, ty: EbbTy, l: Symbol, r: Symbol) -> &mut Self {
        self.push(Op::Mul{var: var, ty: ty, l: l, r: r});
        self
    }

    pub fn closure(&mut self, var: Symbol, param_ty: EbbTy, ret_ty: EbbTy, fun: Symbol, env: Vec<(EbbTy, Symbol)>) -> &mut Self {
        self.push(Op::Closure{var: var, param_ty: param_ty, ret_ty: ret_ty, fun: fun, env: env});
        self
    }

    pub fn call(&mut self, var: Symbol, ty: EbbTy, fun: Symbol, args: Vec<Symbol>) -> &mut Self {
        self.push(Op::Call{var: var, ty: ty, fun: fun, args: args});
        self
    }

    pub fn branch(mut self, cond: Symbol, then: Symbol, else_: Symbol) -> EBB {
        self.push(Op::Branch {cond: cond, then: then, else_: else_});
        self.0
    }

    pub fn jump(mut self, target: Symbol, args: Vec<Symbol>) -> EBB {
        self.push(Op::Jump {target: target, args: args});
        self.0
    }

    pub fn ret(mut self, value: Symbol, ty: EbbTy) -> EBB {
        self.push(Op::Ret {value: value, ty: ty});
        self.0
    }
}


pub struct HIR2MIR {
    id: usize,
}


use hir;

impl From<Ty> for EbbTy {
    fn from(ty: Ty) -> Self {
        match ty {
            Ty::Unit => EbbTy::Unit,
            Ty::Bool => EbbTy::Bool,
            Ty::Int => EbbTy::Int,
            Ty::Fun(arg, ret) => EbbTy::Ebb{
                params: vec![EbbTy::from(*arg)],
                ret: Box::new(EbbTy::from(*ret))},
        }
    }
}

impl HIR2MIR {
    pub fn new() -> Self {
        HIR2MIR{id: 0}
    }

    fn genlabel(&mut self, name: &str) -> Symbol {
        let name = format!("{}@{}", name, self.id);
        self.id += 1;
        Symbol(name)
    }

    fn trans_hir(&mut self, hir: hir::HIR) -> MIR {
        // TODO: make anonymous
        let mut mainbuilder = FunctionBuilder::new(Symbol("main".to_string()), EbbTy::Int);
        let mut mainebuilder = EBBBuilder::new(self.genlabel("entry"), Vec::new());
        let mut funs = Vec::new();

        for val in hir.0.into_iter() {
            mainebuilder = self.trans_val(&mut funs, &mut mainbuilder, mainebuilder, val);
        };
        let ebb = mainebuilder.ret(Symbol("exit".to_string()), EbbTy::Int);
        mainbuilder.add_ebb(ebb);
        let main = mainbuilder.build();
        funs.push(main);
        MIR(funs)
    }

    fn trans_val(&mut self, funs: &mut Vec<Function>, fb: &mut FunctionBuilder, mut eb: EBBBuilder, val: hir::Val) -> EBBBuilder {
        use hir::Expr::*;
        let hir::Val {ty: ty_, name, expr, ..} = val;
        match expr {
            Fun{body, param, body_ty, mut captures} => {
//                assert_eq!(body_ty, ty_);
                captures.push(param);
                let captures = captures.into_iter().map(|(ty, var)| (EbbTy::from(ty), var)).collect();
                let eb_ = EBBBuilder::new(Symbol("entry".to_string()), captures);
                let mut fb = FunctionBuilder::new(name, EbbTy::from(body_ty.clone()));
                let ebb = self.trans_expr(&mut fb, eb_, body_ty, *body);
                fb.add_ebb(ebb);
                let function = fb.build();
                funs.push(function);
                eb
            }
            e @ Sym{..} |
            e @ Binds{..} => {
                let (mut eb, var) = self.trans_expr_block(fb, eb, ty_.clone(), e);
                eb.alias(name, EbbTy::from(ty_), var);
                eb
            },
            App{ty, fun, arg} => {
                assert_eq!(ty, ty_);
                let arg = force_symbol(*arg);
                let fun = force_symbol(*fun);
                eb.call(name, EbbTy::from(ty), fun, vec![arg]);
                eb
            },
            If {ty, cond, then, else_} => {
                let thenlabel = self.genlabel("then");
                let elselabel = self.genlabel("else");
                let joinlabel = self.genlabel("join");
                let (eb, var) = self.trans_expr_block(fb, eb, Ty::Bool, *cond);
                let ebb  = eb.branch(var, thenlabel.clone(), elselabel.clone());
                fb.add_ebb(ebb);

                let eb = EBBBuilder::new(thenlabel, Vec::new());
                let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), *then);
                let ebb = eb.jump(joinlabel.clone(), vec![var]);
                fb.add_ebb(ebb);

                let eb = EBBBuilder::new(elselabel, Vec::new());
                let (eb, var) = self.trans_expr_block(fb, eb, ty.clone(), *else_);
                let ebb = eb.jump(joinlabel.clone(), vec![var]);
                fb.add_ebb(ebb);

                let eb = EBBBuilder::new(joinlabel, vec![(EbbTy::from(ty), name)]);
                eb
            }
                ,
            Op{ty, name: name_, l, r} => {
                assert_eq!(ty, ty_);
                let l = force_symbol(*l);
                let r = force_symbol(*r);
                match name_.0.as_ref() {
                    "+" => eb.add(name, EbbTy::from(ty), l, r),
                    "*" => eb.mul(name, EbbTy::from(ty), l, r),
                    _ => panic!("internal error")
                };
                eb
            },
            Closure{envs, param_ty, body_ty, fname} => {
                let envs = envs.into_iter().map(|(ty, var)| (EbbTy::from(ty), var)).collect();
                eb.closure(name, EbbTy::from(param_ty), EbbTy::from(body_ty), fname,  envs);
                eb
            }
            PrimFun{..} =>
                panic!("internal error: primfun"),
            Lit{ty, value} => {
                assert_eq!(ty, ty_);
                eb.lit(name, EbbTy::from(ty), value);
                eb
            }
        }

    }

    fn trans_expr(&mut self, fb: &mut FunctionBuilder, mut eb: EBBBuilder, ty_: Ty, expr: hir::Expr) -> EBB {
        use hir::Expr::*;
        match expr {
            Binds{ty, binds, ret} => {
                assert_eq!(ty, ty_);
                let mut funs = Vec::new();
                for val in binds {
                    eb = self.trans_val(&mut funs, fb, eb, val);
                }
                assert_eq!(funs.len(), 0);
                eb.ret(force_symbol(*ret), EbbTy::from(ty))
            },
            Sym{ty, name} => {
                assert_eq!(ty, ty_);
                eb.ret(name, EbbTy::from(ty))
            },
            _ => panic!("internal error"),
        }
    }

    fn trans_expr_block(&mut self, fb: &mut FunctionBuilder, mut eb: EBBBuilder, ty_: Ty, expr: hir::Expr) -> (EBBBuilder, Symbol) {
        use hir::Expr::*;
        match expr {
            Binds{ty, binds, ret} => {
                assert_eq!(ty, ty_);
                let mut funs = Vec::new();
                for val in binds {
                    eb = self.trans_val(&mut funs, fb, eb, val)
                };
                assert_eq!(funs.len(), 0);
                (eb, force_symbol(*ret))
            },
            Sym{ty, name} => {
                assert_eq!(ty, ty_);
                (eb, name)
            },
            _ => panic!("internal error"),
        }
    }

}

fn force_symbol(e: hir::Expr) -> Symbol {
    match e {
        hir::Expr::Sym{name, ..} => name,
        _ => panic!("not a symbol")
    }
}


impl Pass<hir::HIR> for HIR2MIR {
    type Target = MIR;
    type Err = TypeError;

    fn trans(&mut self, hir: hir::HIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.trans_hir(hir))
    }
}
