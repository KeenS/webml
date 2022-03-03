use crate::config::Config;
use crate::lir::*;
use crate::mir;
use crate::pass::Pass;
use crate::prim::*;
use log::debug;
use std::collections::HashMap;

pub struct MIR2LIR {}

pub struct MIR2LIRPass {
    extern_types: ExternTypes,
    symbol_table: mir::SymbolTable,
}

impl MIR2LIR {
    pub fn new() -> Self {
        MIR2LIR {}
    }

    fn generate_pass(&mut self, symbol_table: mir::SymbolTable) -> MIR2LIRPass {
        MIR2LIRPass::new(symbol_table)
    }
}

impl MIR2LIRPass {
    fn new(symbol_table: mir::SymbolTable) -> Self {
        Self {
            extern_types: HashMap::new(),
            symbol_table,
        }
    }

    fn ebbty_to_lty<'a>(&self, ty: &mir::EbbTy) -> LTy {
        use crate::mir::EbbTy::*;
        match ty {
            Unit => LTy::Unit,
            Char => LTy::U32,
            Int => LTy::I32,
            Float => LTy::F64,
            Bool => LTy::I32,
            Tuple(_) => LTy::Ptr,
            //FIXME
            Union(_) => LTy::Ptr,
            Cls { .. } => LTy::Ptr,
            Ebb { .. } => LTy::FPtr,
            Variable(name) => self.ebbty_to_lty(self.symbol_table.canonical_value(name).unwrap()),
        }
    }

    pub fn trans_mir(&mut self, mir: mir::MIR) -> LIR {
        LIR(mir.0.into_iter().map(|f| self.trans_function(f)).collect())
    }

    fn trans_function(&mut self, f: mir::Function) -> Function {
        use crate::lir::Op::*;
        use crate::lir::Value::*;
        use crate::mir::Op as m;
        let mir::Function {
            name,
            body,
            body_ty,
        } = f;
        let nparams = body[0].params.len() as u32;
        let ret_ty = self.ebbty_to_lty(&body_ty);
        let mut regs = Vec::new();
        let mut id = 0;
        let mut blocks = Vec::new();
        {
            // limit the scope of new_reg

            let mut new_reg = |ty| {
                let reg = Reg(ty, id);
                regs.push(reg.clone());
                id += 1;
                reg
            };

            let symbol_table = self.make_symbol_table(body.as_ref(), &mut new_reg);
            let target_table = self.make_target_table(body.as_ref(), &symbol_table);
            macro_rules! reg {
                ($var: expr) => {
                    symbol_table
                        .get(&$var)
                        .unwrap_or_else(|| panic!("variable resolution failed: {:?}", &$var))
                        .clone()
                };
            }

            for ebb in body.iter() {
                let mut ops = Vec::new();
                for op in ebb.body.iter() {
                    debug!(target: "mir_to_lir", "op: {:?}", op);
                    match op {
                        &m::Lit {
                            ref var, ref value, ..
                        } => match value {
                            &Literal::Char(c) => ops.push(ConstI32(reg!(var), c as u32)),
                            &Literal::Int(i) => ops.push(ConstI32(reg!(var), i as u32)),
                            &Literal::Real(f) => ops.push(ConstF64(reg!(var), f as f64)),
                        },
                        &m::Alias {
                            ref var,
                            ref ty,
                            ref sym,
                        } => {
                            let ty = self.ebbty_to_lty(ty);
                            match ty {
                                LTy::Unit => {
                                    // do nothing
                                }
                                LTy::I32 => ops.push(MoveI32(reg!(var), reg!(sym))),
                                LTy::U32 => ops.push(MoveU32(reg!(var), reg!(sym))),
                                LTy::I64 => ops.push(MoveI64(reg!(var), reg!(sym))),
                                LTy::U64 => ops.push(MoveU64(reg!(var), reg!(sym))),
                                LTy::F32 => ops.push(MoveF32(reg!(var), reg!(sym))),
                                LTy::F64 => ops.push(MoveF64(reg!(var), reg!(sym))),
                                // assuming ptr size is i32
                                LTy::Ptr => ops.push(MoveI32(reg!(var), reg!(sym))),
                                LTy::FPtr => ops.push(MoveI32(reg!(var), reg!(sym))),
                            }
                        }
                        &m::BinOp {
                            ref var,
                            ref l,
                            ref r,
                            ref binop,
                            ..
                        } => {
                            use mir::BinOp;
                            match binop {
                                BinOp::AddInt => ops.push(AddI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::AddReal => ops.push(AddF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::SubInt => ops.push(SubI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::SubReal => ops.push(SubF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::MulInt => ops.push(MulI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::MulReal => ops.push(MulF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::DivInt => ops.push(DivI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::DivReal => ops.push(DivF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::ModInt => ops.push(ModI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::EqInt => ops.push(EqI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::EqReal => ops.push(EqF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::EqChar => ops.push(EqU32(reg!(var), reg!(l), reg!(r))),
                                BinOp::NeqInt => ops.push(NeqI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::NeqReal => ops.push(NeqF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::NeqChar => ops.push(NeqU32(reg!(var), reg!(l), reg!(r))),
                                BinOp::GtInt => ops.push(GtI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::GtReal => ops.push(GtF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::GtChar => ops.push(GtU32(reg!(var), reg!(l), reg!(r))),
                                BinOp::GeInt => ops.push(GeI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::GeReal => ops.push(GeF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::GeChar => ops.push(GeU32(reg!(var), reg!(l), reg!(r))),
                                BinOp::LtInt => ops.push(LtI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::LtReal => ops.push(LtF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::LtChar => ops.push(LtU32(reg!(var), reg!(l), reg!(r))),
                                BinOp::LeInt => ops.push(LeI32(reg!(var), reg!(l), reg!(r))),
                                BinOp::LeReal => ops.push(LeF64(reg!(var), reg!(l), reg!(r))),
                                BinOp::LeChar => ops.push(LeU32(reg!(var), reg!(l), reg!(r))),
                            }
                        }
                        &m::Tuple {
                            ref var,
                            ref tys,
                            ref tuple,
                        } => {
                            let reg = reg!(var);

                            let tys: Vec<_> = tys.iter().map(|ty| self.ebbty_to_lty(ty)).collect();
                            // currently all the items are aligned to 8
                            let size: u32 = tys.iter().map(|_| 8).sum();
                            // let size: u32 = tys.iter().map(|ty| ty.size()).sum();

                            ops.push(HeapAlloc(reg.clone(), I(size as i32), tys.clone()));

                            let mut acc = 0;
                            for (var, ty) in tuple.iter().zip(tys) {
                                match ty {
                                    LTy::Unit => {
                                        // do nothing
                                    }
                                    LTy::I32 => {
                                        ops.push(StoreI32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::U32 => {
                                        ops.push(StoreU32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::I64 => {
                                        ops.push(StoreI64(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::U64 => {
                                        ops.push(StoreU64(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::F32 => {
                                        ops.push(StoreF32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::F64 => {
                                        ops.push(StoreF64(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::Ptr => {
                                        ops.push(StoreI32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::FPtr => {
                                        ops.push(StoreI32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                }
                                acc += 8;
                            }
                        }
                        &m::Proj {
                            ref var,
                            ref ty,
                            ref index,
                            ref tuple,
                        } => {
                            #[allow(clippy::never_loop)]
                            loop {
                                let ctor = match self.ebbty_to_lty(ty) {
                                    LTy::F32 => LoadF32,
                                    LTy::F64 => LoadF64,
                                    LTy::I32 => LoadI32,
                                    LTy::U32 => LoadU32,
                                    LTy::I64 => LoadI64,
                                    LTy::U64 => LoadU64,
                                    LTy::Ptr => LoadI32,
                                    LTy::FPtr => LoadI32,
                                    LTy::Unit =>
                                    // do nothing
                                    {
                                        break
                                    }
                                };
                                ops.push(ctor(reg!(var), Addr(reg!(tuple), *index * 8)));
                                break;
                            }
                        }

                        &m::Union {
                            ref var,
                            ref tys,
                            ref variant,
                            ref index,
                        } => {
                            let ty = &tys[*index as usize];
                            #[allow(clippy::never_loop)]
                            loop {
                                let ctor = match self.ebbty_to_lty(ty) {
                                    LTy::F32 => MoveF32,
                                    LTy::F64 => MoveF64,
                                    LTy::I32 => MoveI32,
                                    LTy::U32 => MoveU32,
                                    LTy::I64 => MoveI64,
                                    LTy::U64 => MoveU64,
                                    LTy::Ptr => MoveI32,
                                    LTy::FPtr => MoveI32,
                                    LTy::Unit => MoveI32,
                                };
                                ops.push(ctor(reg!(var), reg!(variant)));
                                break;
                            }
                        }
                        &m::Select {
                            ref var,
                            ref ty,
                            ref union,
                            ..
                        } => {
                            #[allow(clippy::never_loop)]
                            loop {
                                let ctor = match self.ebbty_to_lty(ty) {
                                    LTy::F32 => MoveF32,
                                    LTy::F64 => MoveF64,
                                    LTy::I32 => MoveI32,
                                    LTy::U32 => MoveU32,
                                    LTy::I64 => MoveI64,
                                    LTy::U64 => MoveU64,
                                    LTy::Ptr => MoveI32,
                                    LTy::FPtr => MoveI32,
                                    LTy::Unit =>
                                    // do nothing
                                    {
                                        break
                                    }
                                };
                                ops.push(ctor(reg!(var), reg!(union)));
                                break;
                            }
                        }

                        &m::Closure {
                            ref var,
                            ref fun,
                            ref env,
                            ..
                        } => {
                            // closure looks like on memory:
                            //   64      64    ...
                            // +-----------------------
                            // | fptr | arg1 | ...
                            // +-----------------------

                            let reg = reg!(var);
                            let mut size: u32 = LTy::Ptr.size();
                            size += env
                                .iter()
                                .map(|&(ref ty, _)| self.ebbty_to_lty(ty).size())
                                .sum::<u32>();
                            let mut tys = vec![LTy::FPtr];
                            for &(ref ty, _) in env.iter() {
                                tys.push(self.ebbty_to_lty(ty));
                            }
                            ops.push(HeapAlloc(reg.clone(), I(size as i32), tys));
                            // FIXME: explicitly take fun pointer
                            ops.push(StoreFnPtr(Addr(reg.clone(), 0), fun.clone()));
                            let mut acc = LTy::FPtr.size();
                            for &(ref ty, ref var) in env.iter() {
                                let ty = self.ebbty_to_lty(ty);
                                match ty {
                                    LTy::Unit => {
                                        // FIXME: remove unit from closure
                                        // do nothing
                                    }
                                    LTy::I32 => {
                                        ops.push(StoreI32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::U32 => {
                                        ops.push(StoreU32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::I64 => {
                                        ops.push(StoreI64(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::U64 => {
                                        ops.push(StoreU64(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::F32 => {
                                        ops.push(StoreF32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::F64 => {
                                        ops.push(StoreF64(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::Ptr => {
                                        ops.push(StoreI32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                    LTy::FPtr => {
                                        ops.push(StoreI32(Addr(reg.clone(), acc), reg!(var)))
                                    }
                                }
                                acc += 8;
                            }
                        }
                        &m::ExternCall {
                            ref var,
                            ref module,
                            ref fun,
                            ref args,
                            ..
                        } => {
                            let args = args.iter().map(|a| reg!(a)).collect();
                            self.extern_types.insert(
                                (module.to_string(), fun.to_string()),
                                (vec![LTy::I32], LTy::Unit),
                            );
                            ops.push(ExternCall(
                                reg!(var),
                                module.to_string(),
                                fun.to_string(),
                                args,
                            ))
                        }
                        &m::Call {
                            ref var,
                            ref fun,
                            ref args,
                            ..
                        } => {
                            let args = args.iter().map(|a| reg!(a)).collect();
                            match symbol_table.get(fun) {
                                Some(r) => ops.push(ClosureCall(reg!(var), r.clone(), args)),
                                None => ops.push(FunCall(reg!(var), fun.clone(), args)),
                            }
                        }
                        &m::Branch {
                            ref cond,
                            ref clauses,
                            ref default,
                            ..
                        } => {
                            let mut clauses = clauses.clone();
                            clauses.sort_by_key(|&(ref key, _, _)| *key);
                            let default_label = match default.clone() {
                                None => None,
                                Some((label, _)) => {
                                    let params = &target_table[&label];
                                    assert_eq!(params.len(), 1);
                                    let p = &params[0];
                                    match p.0 {
                                        LTy::Unit => {
                                            // do nothing
                                        }
                                        LTy::I32 => ops.push(MoveI32(p.clone(), reg!(cond))),
                                        LTy::U32 => ops.push(MoveU32(p.clone(), reg!(cond))),
                                        LTy::I64 => ops.push(MoveI64(p.clone(), reg!(cond))),
                                        LTy::U64 => ops.push(MoveU64(p.clone(), reg!(cond))),
                                        LTy::F32 => ops.push(MoveF32(p.clone(), reg!(cond))),
                                        LTy::F64 => ops.push(MoveF64(p.clone(), reg!(cond))),
                                        LTy::Ptr => ops.push(MoveI32(p.clone(), reg!(cond))),
                                        LTy::FPtr => ops.push(MoveI32(p.clone(), reg!(cond))),
                                    };
                                    Some(Label(label))
                                }
                            };

                            if !clauses.is_empty()
                                && clauses[0].0 == 0
                                && clauses
                                    .iter()
                                    .enumerate()
                                    .all(|(n, &(ref key, _, _))| n == (*key as usize))
                            {
                                // use jump table
                                ops.push(JumpTableI32(
                                    reg!(cond),
                                    clauses
                                        .into_iter()
                                        .map(|(_, label, _)| Label(label))
                                        .collect(),
                                    default_label,
                                ))
                            } else {
                                let cond = reg!(cond);

                                match cond.0 {
                                    LTy::I32 => {
                                        let boolean = new_reg(LTy::I32);
                                        let constant = new_reg(LTy::I32);
                                        for (key, label, _) in clauses {
                                            ops.push(ConstI32(constant.clone(), key as u32));
                                            ops.push(EqI32(
                                                boolean.clone(),
                                                cond.clone(),
                                                constant.clone(),
                                            ));
                                            ops.push(JumpIfI32(boolean.clone(), Label(label)))
                                        }
                                    }
                                    LTy::U32 => {
                                        let boolean = new_reg(LTy::U32);
                                        let constant = new_reg(LTy::U32);
                                        for (key, label, _) in clauses {
                                            ops.push(ConstU32(constant.clone(), key as u32));
                                            ops.push(EqU32(
                                                boolean.clone(),
                                                cond.clone(),
                                                constant.clone(),
                                            ));
                                            ops.push(JumpIfI32(boolean.clone(), Label(label)))
                                        }
                                    }
                                    _ => panic!("internal error: branching currently supports only 32 bit types"),
                                }
                                if let Some(label) = default_label {
                                    ops.push(Jump(label))
                                }
                            }
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
                                    }
                                    LTy::I32 => ops.push(MoveI32(p.clone(), reg!(a))),
                                    LTy::U32 => ops.push(MoveU32(p.clone(), reg!(a))),
                                    LTy::I64 => ops.push(MoveI64(p.clone(), reg!(a))),
                                    LTy::U64 => ops.push(MoveU64(p.clone(), reg!(a))),
                                    LTy::F32 => ops.push(MoveF32(p.clone(), reg!(a))),
                                    LTy::F64 => ops.push(MoveF64(p.clone(), reg!(a))),
                                    LTy::Ptr => ops.push(MoveI32(p.clone(), reg!(a))),
                                    LTy::FPtr => ops.push(MoveI32(p.clone(), reg!(a))),
                                }
                            }
                            ops.push(Jump(Label(target.clone())))
                        }
                        &m::Ret { ref value, ref ty } => match ty {
                            mir::EbbTy::Unit => ops.push(Ret(None)),
                            _ => ops.push(Ret(value.as_ref().map(|v| reg!(v)))),
                        },
                    }
                }
                blocks.push(Block {
                    name: Label(ebb.name.clone()),
                    body: ops,
                })
            }
        }

        let regs = regs.into_iter().map(|r| r.0).collect::<Vec<_>>();

        Function {
            name,
            nparams,
            regs,
            ret_ty,
            body: blocks,
        }
    }

    fn make_symbol_table<'a, F>(
        &self,
        body: &'a [mir::EBB],
        mut new_reg: F,
    ) -> HashMap<&'a Symbol, Reg>
    where
        F: FnMut(LTy) -> Reg,
    {
        let mut table = HashMap::new();
        macro_rules! intern {
            ($ty: expr, $var: expr) => {{
                if table.get(&$var).is_none() {
                    table.insert($var, new_reg($ty));
                };
            }};
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
                    &mir::Op::Lit {
                        ref var, ref ty, ..
                    }
                    | &mir::Op::Alias {
                        ref var, ref ty, ..
                    }
                    | &mir::Op::BinOp {
                        ref var, ref ty, ..
                    }
                    | &mir::Op::Proj {
                        ref var, ref ty, ..
                    }
                    | &mir::Op::Select {
                        ref var, ref ty, ..
                    }
                    | &mir::Op::ExternCall {
                        ref var, ref ty, ..
                    }
                    | &mir::Op::Call {
                        ref var, ref ty, ..
                    } => {
                        intern!(self.ebbty_to_lty(ty), var);
                    }
                    &mir::Op::Tuple { ref var, .. } | &mir::Op::Closure { ref var, .. } => {
                        intern!(LTy::Ptr, var);
                    }
                    &mir::Op::Union { ref var, .. } => intern!(LTy::Ptr, var),
                    _ => (),
                }
            }
        }

        table
    }

    fn make_target_table<'a>(
        &self,
        body: &'a [mir::EBB],
        symbol_table: &HashMap<&'a Symbol, Reg>,
    ) -> HashMap<&'a Symbol, Vec<Reg>> {
        let mut tbl = HashMap::new();
        for ebb in body {
            let params = ebb
                .params
                .iter()
                .map(|&(_, ref param)| symbol_table[param].clone())
                .collect();
            tbl.insert(&ebb.name, params);
        }
        tbl
    }
}

impl<E> Pass<mir::Context, E> for MIR2LIR {
    type Target = Context;

    fn trans(
        &mut self,
        mir::Context(symbol_table, mir): mir::Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        let mut pass = self.generate_pass(symbol_table);
        let lir = pass.trans_mir(mir);
        let types = pass.extern_types.drain().collect();
        Ok(Context(types, lir))
    }
}
