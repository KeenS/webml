use std::collections::HashMap;

use pass::Pass;
use prim::*;
use lir::*;
use mir;

pub struct MIR2LIR;

impl MIR2LIR {
    pub fn new() -> Self {
        MIR2LIR
    }

    fn ebbty_to_lty<'a>(&self, ty: &mir::EbbTy) -> LTy {
        use mir::EbbTy::*;
        match *ty {
            Unit => LTy::Unit,
            Int => LTy::I64,
            Float => LTy::F64,
            Bool => LTy::I32,
            Cls { .. } => PTR,
            Ebb { .. } => PTR,

        }
    }

    pub fn trans_mir(&self, mir: mir::MIR) -> LIR {
        LIR(mir.0
                .into_iter()
                .map(|f| self.trans_function(f))
                .collect())
    }

    fn trans_function(&self, f: mir::Function) -> Function {
        use lir::Op::*;
        use lir::Value::*;
        use mir::Op as m;
        let mir::Function {
            name,
            body,
            body_ty,
        } = f;
        let nparams = body[0].params.len() as u32;
        let ret_ty = self.ebbty_to_lty(&body_ty);
        let symbol_table = self.make_symbol_table(body.as_ref());
        let target_table = self.make_target_table(body.as_ref(), &symbol_table);
        macro_rules! reg {
            ($var: expr) => {symbol_table[&$var].clone()}
        }
        let mut blocks = Vec::new();
        for ebb in body.iter() {
            let mut ops = Vec::new();
            for op in ebb.body.iter() {
                match op {
                    &m::Lit { ref var, ref value, .. } => {
                        match value {
                            &Literal::Bool(b) => ops.push(ConstI32(reg!(var), b as u32)),
                            &Literal::Int(i) => ops.push(ConstI64(reg!(var), i as u64)),
                            &Literal::Float(f) => ops.push(ConstF64(reg!(var), f as f64)),
                        }
                    }
                    &m::Alias {
                         ref var,
                         ref ty,
                         ref sym,
                     } => {
                        match ty {
                            &mir::EbbTy::Unit => {
                                // do nothing
                                ()
                            }
                            &mir::EbbTy::Bool => ops.push(MoveI32(reg!(var), reg!(sym))),
                            &mir::EbbTy::Int |
                            &mir::EbbTy::Cls { .. } |
                            &mir::EbbTy::Ebb { .. } => ops.push(MoveI64(reg!(var), reg!(sym))),
                            &mir::EbbTy::Float => ops.push(MoveF64(reg!(var), reg!(sym))),
                        }
                    }
                    &m::Add {
                         ref var,
                         ref ty,
                         ref l,
                         ref r,
                     } => {
                        if ty == &mir::EbbTy::Int {
                            ops.push(AddI64(reg!(var), reg!(l), reg!(r)));
                        } else {
                            assert_eq!(ty, &mir::EbbTy::Float);
                            ops.push(AddF64(reg!(var), reg!(l), reg!(r)));

                        }
                    }
                    &m::Mul {
                         ref var,
                         ref ty,
                         ref l,
                         ref r,
                     } => {
                        if ty == &mir::EbbTy::Int {
                            ops.push(MulI64(reg!(var), reg!(l), reg!(r)));
                        } else {
                            assert_eq!(ty, &mir::EbbTy::Float);
                            ops.push(MulF64(reg!(var), reg!(l), reg!(r)));

                        }
                    }
                    &m::Closure {
                         ref var,
                         ref fun,
                         ref env,
                         ..
                     } => {
                        let reg = reg!(var);
                        let mut size: u32 = PTR.size();
                        size += env.iter()
                            .map(|&(ref ty, _)| self.ebbty_to_lty(ty).size())
                            .sum();
                        ops.push(HeapAlloc(reg.clone(), I(size as i32)));
                        ops.push(StoreFnPtr(Addr(reg.clone(), 0), fun.clone()));
                        let mut acc = PTR.size();
                        for &(ref ty, ref var) in env.iter() {
                            let ty = self.ebbty_to_lty(ty);
                            match ty {
                                LTy::Unit => {
                                    // do nothing
                                    ()
                                }
                                LTy::I32 => ops.push(StoreI32(Addr(reg.clone(), acc), reg!(var))),
                                LTy::I64 => ops.push(StoreI64(Addr(reg.clone(), acc), reg!(var))),
                                LTy::F32 => ops.push(StoreF32(Addr(reg.clone(), acc), reg!(var))),
                                LTy::F64 => ops.push(StoreF64(Addr(reg.clone(), acc), reg!(var))),
                            }
                            acc += ty.size();
                        }
                    }
                    &m::Call {
                         ref var,
                         ref fun,
                         ref args,
                         ..
                     } => {
                        // FIXME: distinguish call function and call closure
                        let args = args.iter().map(|a| reg!(a)).collect();
                        match symbol_table.get(fun) {
                            Some(r) => ops.push(ClosureCall(reg!(var), r.clone(), args)),
                            None => ops.push(FunCall(reg!(var), fun.clone(), args)),
                        }

                    }
                    &m::Branch {
                         ref cond,
                         ref then,
                         ref else_,
                         ..
                     } => {
                        ops.push(JumpIfI32(reg!(cond), Label(then.clone())));
                        ops.push(Jump(Label(else_.clone())))
                    }
                    &m::Jump {
                         ref target,
                         ref args,
                         ..
                     } => {
                        let params = &target_table[target];
                        for (p, a) in params.iter().zip(args) {
                            match p.0 {
                                LTy::Unit => {
                                    // do nothing
                                    ()
                                }
                                LTy::I32 => ops.push(MoveI32(p.clone(), reg!(a))),
                                LTy::I64 => ops.push(MoveI64(p.clone(), reg!(a))),
                                LTy::F32 => ops.push(MoveF32(p.clone(), reg!(a))),
                                LTy::F64 => ops.push(MoveF64(p.clone(), reg!(a))),
                            }
                        }
                        ops.push(Jump(Label(target.clone())))
                    }
                    &m::Ret { ref value, ref ty } => {
                        println!("{:?}: {:?}", value, ty);
                        ops.push(Ret(value.as_ref().map(|v| reg!(v))));
                    }

                }
            }
            blocks.push(Block {
                            name: Label(ebb.name.clone()),
                            body: ops,
                        })
        }

        let mut regs = symbol_table.values().collect::<Vec<_>>();
        regs.sort_by_key(|r| r.1);
        let regs = regs.into_iter()
            .map(|r| r.0.clone())
            .collect::<Vec<_>>();

        Function {
            name: name,
            nparams: nparams,
            regs: regs,
            ret_ty: ret_ty,
            body: blocks,
        }

    }

    fn make_symbol_table<'a>(&self, body: &'a [mir::EBB]) -> HashMap<&'a Symbol, Reg> {
        let mut id = 0;
        let mut table = HashMap::new();
        macro_rules! intern {
            ($ty: expr, $var: expr) => {{
                if table.get(&$var).is_none() {
                    table.insert($var, Reg($ty, id));
                    id += 1;
                };
                ;
            }}
        }

        // allocate function params first
        for &(ref ty, ref param) in &body[0].params {
            intern!(self.ebbty_to_lty(ty), param);
        }

        for ebb in body {
            for &(ref ty, ref param) in &ebb.params {
                intern!(self.ebbty_to_lty(ty), param);
            }

            for op in ebb.body.iter() {
                match op {
                    &mir::Op::Lit { ref var, ref ty, .. } |
                    &mir::Op::Alias { ref var, ref ty, .. } |
                    &mir::Op::Add { ref var, ref ty, .. } |
                    &mir::Op::Mul { ref var, ref ty, .. } |
                    &mir::Op::Call { ref var, ref ty, .. } => {
                        intern!(self.ebbty_to_lty(ty), var);
                    }
                    &mir::Op::Closure { ref var, .. } => {
                        intern!(PTR, var);
                    }
                    _ => (),
                }
            }
        }


        table
    }

    fn make_target_table<'a>(&self,
                             body: &'a [mir::EBB],
                             symbol_table: &HashMap<&'a Symbol, Reg>)
                             -> HashMap<&'a Symbol, Vec<Reg>> {
        let mut tbl = HashMap::new();
        for ebb in body {
            let params = ebb.params
                .iter()
                .map(|&(_, ref param)| symbol_table[param].clone())
                .collect();
            tbl.insert(&ebb.name, params);
        }
        tbl
    }
}

impl Pass<mir::MIR> for MIR2LIR {
    type Target = LIR;
    type Err = TypeError;

    fn trans(&mut self, mir: mir::MIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.trans_mir(mir))
    }
}
