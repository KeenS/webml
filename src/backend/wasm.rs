extern crate web_assembler as wasm;

use std::collections::{HashSet, HashMap};

use prim::*;
use pass::Pass;
use lir;
use self::wasm::builder::*;
use self::wasm::*;

#[derive(Debug, Clone)]
enum Control<'a> {
    Body(&'a lir::Block),
    Loop(&'a lir::Label),
    LoopEnd(&'a lir::Label),
    Block(&'a lir::Label),
    BlockEnd(&'a lir::Label),
}


fn lty_to_valuetype_opt(t: &lir::LTy) -> Option<ValueType> {
    use lir::LTy::*;
    match *t {
        Unit => None,
        I32 => Some(ValueType::I32),
        I64 => Some(ValueType::I64),
        F32 => Some(ValueType::F32),
        F64 => Some(ValueType::F64),
        FPtr => Some(ValueType::I32),
        Ptr => Some(ValueType::I32),
    }
}

fn lty_to_valuetype(t: &lir::LTy) -> ValueType {
    lty_to_valuetype_opt(t).unwrap_or(ValueType::I32)
}

fn tys_to_ptrbits(tys: &[lir::LTy]) -> u32 {
    let mut bits = 0;
    for ty in tys {
        bits <<= 1;
        if ty.is_ptr() {
            bits |= 1;
        }
    }
    bits
}

pub struct LIR2WASM {
    md: ModuleBuilder,
    alloc_fun: FunctionSpaceIndex,
    print_fun: FunctionSpaceIndex,
    function_table: HashMap<Symbol, u32>
}

impl LIR2WASM {
    pub fn new() -> Self {
        let mut md =  ModuleBuilder::new();

        let alloc_fun_index = md.add_type(funtype!((i32, i32) -> i32));
        let alloc_fun = md.import("webml-rt", "alloc", alloc_fun_index);

        let print_fun_index = md.add_type(funtype!((f64)));
        let print_fun = md.import("js-ffi", "print", print_fun_index);

        LIR2WASM {
            md: md,
            alloc_fun: alloc_fun.into(),
            print_fun: print_fun.into(),
            function_table: HashMap::new(),
        }
    }

    pub fn trans_lir(&mut self, l: lir::LIR) -> Module {
        self.function_table = l.0.iter()
            .enumerate()
            .map(|(i, s)| (s.name.clone(), i as u32))
            .collect();

        for f in l.0 {
            self.trans_function(f);
        }
        let mut ret = ModuleBuilder::new();
        // FIXME:
        ::std::mem::swap(&mut self.md, &mut ret);
        ret.build()
    }

    fn trans_function(&mut self, f: lir::Function) {
        use lir::Value::*;
        let lir::Function { nparams, regs, ret_ty, body, .. } = f;
        let mut tys = regs.iter().map(|reg| lty_to_valuetype(reg)).collect::<Vec<_>>();
        let regtys = tys.split_off(nparams as usize);
        let mut fb = FunctionBuilder::new(FuncType {
            params: tys,
            ret: match ret_ty {
                ty => lty_to_valuetype_opt(&ty),
            },
        });

        let mut locals = fb.new_locals(regtys);

        let fb = fb.code(|mut cb, params| {
            let body = self.alloc_loop_block_break(&body);
            let mut params = params.to_vec();
            params.append(&mut locals);
            let mut scope = Vec::new();

            macro_rules! reg {
                ($reg: expr) => (params[$reg.1 as usize])
            }

            macro_rules! label {
                ($label: expr) => (scope.iter().rev().position(|x| *x == $label).expect("internal error") as u32)
            }

            macro_rules! fun {
                ($funname: expr) => {{
                    let findex = FunctionIndex(self.function_table[$funname]);
                    Into::<FunctionSpaceIndex>::into(findex)
                }}
            }


            for c in body {
                match c {
                    Control::Block(name) => {
                        scope.push(name);
                        cb = cb.block(BlockType(None));
                    },
                    Control::BlockEnd(name) => {
                        let top = scope.pop().unwrap();
                        assert_eq!(name, top);
                        cb = cb.end();
                    }
                    Control::Loop(name) => {
                        scope.push(name);
                        cb = cb.loop_(BlockType(None));
                    },
                    Control::LoopEnd(name) => {
                        let top = scope.pop().unwrap();
                        assert_eq!(name, top);
                        cb = cb.end();
                    },
                    Control::Body(b) => {
                        use lir::Op::*;
                        for op in b.body.iter() {
                            match *op {
                                ConstI32(ref reg, c) => {
                                    cb = cb.constant(c as i32)
                                        .set_local(reg!(reg))
                                },
                                MoveI32(ref reg1, ref reg2) |
                                MoveI64(ref reg1, ref reg2) |
                                MoveF32(ref reg1, ref reg2) |
                                MoveF64(ref reg1, ref reg2) => {
                                    cb = cb.get_local(reg!(reg2))
                                        .set_local(reg!(reg1))
                                },
                                StoreI32(ref addr, ref value) => {
                                    cb = cb
                                        .get_local(reg!(value))
                                        .get_local(reg!(addr.0))
                                        .i32_store(addr.1);
                                },
                                LoadI32(ref reg, ref addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .i32_load(addr.1)
                                        .set_local(reg!(reg));

                                },
                                JumpIfI32(ref reg, ref label) => {
                                    cb = cb
                                        .get_local(reg!(reg))
                                        .br_if(label!(label));
                                },

                                ConstI64(ref reg, c) => {
                                    cb = cb.constant(c as i64)
                                        .set_local(reg!(reg))
                                },
                                AddI64(ref reg1, ref reg2, ref reg3) => {
                                    cb = cb.get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_add()
                                        .set_local(reg!(reg1))
                                },
                                MulI64(ref reg1, ref reg2, ref reg3) => {
                                    cb = cb.get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_mul()
                                        .set_local(reg!(reg1))
                                },
                                LoadI64(ref reg, ref addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .i64_load(addr.1)
                                        .set_local(reg!(reg));
                                },

                                StoreI64(ref addr, ref value) => {
                                    cb = cb
                                        .get_local(reg!(value))
                                        .get_local(reg!(addr.0))
                                        .i64_store(addr.1);
                                },


                                ConstF64(ref reg, c) => {
                                    cb = cb.constant(c)
                                        .set_local(reg!(reg))
                                },
                                AddF64(ref reg1, ref reg2, ref reg3) => {
                                    cb = cb.get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_add()
                                        .set_local(reg!(reg1))
                                },
                                MulF64(ref reg1, ref reg2, ref reg3) => {
                                    cb = cb.get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_mul()
                                        .set_local(reg!(reg1))
                                },
                                StoreF64(ref addr, ref value) => {
                                    cb = cb
                                        .get_local(reg!(value))
                                        .get_local(reg!(addr.0))
                                        .f64_store(addr.1);
                                },

                                LoadF64(ref reg, ref addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .f64_load(addr.1)
                                        .set_local(reg!(reg));
                                },

                                ConstF32(ref reg, c) => {
                                    cb = cb.constant(c)
                                        .set_local(reg!(reg))
                                },
                                AddF32(ref reg1, ref reg2, ref reg3) => {
                                    cb = cb.get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_add()
                                        .set_local(reg!(reg1))
                                },
                                MulF32(ref reg1, ref reg2, ref reg3) => {
                                    cb = cb.get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_mul()
                                        .set_local(reg!(reg1))
                                },

                                StoreF32(ref addr, ref value) => {
                                    cb = cb
                                        .get_local(reg!(value))
                                        .get_local(reg!(addr.0))
                                        .f32_store(addr.1);
                                },
                                LoadF32(ref reg, ref addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .f32_load(addr.1)
                                        .set_local(reg!(reg));
                                },

                                HeapAlloc(ref reg, ref value, ref tys) => {
                                    cb = match *value {
                                        I(i) => cb.constant(i as i64),
                                        R(ref r) => cb.get_local(reg!(r)),
                                    };

                                    cb = cb
                                        .constant(tys_to_ptrbits(tys) as i32)
                                        .call(self.alloc_fun)
                                        .set_local(reg!(reg))
                                },
                                StackAlloc(ref reg, size, ref tys) => {
                                    // allocating to heap, not stack
                                    cb = cb
                                        .constant(size as i32)
                                        .constant(tys_to_ptrbits(tys) as i32)
                                        .call(self.alloc_fun)
                                        .set_local(reg!(reg))
                                },
                                StoreFnPtr(ref addr, ref value) => {
                                    cb = cb
                                        .constant(*fun!(value) as i64)
                                        .get_local(reg!(addr.0))
                                        .i64_store(addr.1);
                                },

                                ClosureCall(ref reg, ref fun, ref args) => {
                                    for arg in args.iter() {
                                        cb = cb.get_local(reg!(arg))
                                    }

                                    cb = cb
                                        .get_local(reg!(fun))
                                        // load ptr to captured env
                                        .i64_load(8);
                                    // load the rest args
                                    for arg in args.iter() {
                                        cb = cb.get_local(reg!(arg))
                                    }

                                    cb =
                                        cb
                                        .get_local(reg!(fun))
                                        // load function
                                        .i64_load(0)
                                        .call_indirect(0, false)
                                        // if ret ty isn't unit
                                        .set_local(reg!(reg));
                                },
                                FunCall(ref reg, ref fun, ref args) => {
                                    for arg in args.iter() {
                                        cb = cb.get_local(reg!(arg))
                                    }
                                    // prim funs
                                    if fun.0 == "print" {
                                        cb = cb.call(self.print_fun)
                                        // the ret ty of print is unit
                                    } else {
                                        cb = cb.call(fun!(fun))
                                        // if ret ty isn't unit
                                            .set_local(reg!(reg));
                                    }

                                },
                                Jump(ref label) => {
                                    cb = cb.br(label!(label));
                                },
                                Ret(ref reg) => {
                                    cb = match *reg {
                                        Some(ref r) => cb.get_local(reg!(r)),
                                        None => cb,
                                    };
                                    cb = cb
                                        .return_()
                                },

                            }
                        }
                    }
                }
            }
            cb
        });
        self.md.start(FunctionIndex(self.function_table[&Symbol("main".into())]));
        self.md.new_function(fb.build());
    }

    /// allocate block and loop scopes for jump -> break transformation.
    /// Forward jump will be block + break, and backword jump will be loop + break in following transformation.
    fn alloc_loop_block_break<'a>(&mut self, v: &'a [lir::Block]) -> Vec<Control<'a>> {
        // 1. calculate minimum coverings of loops, blocks and insert them
        // 2. adjust interleavings (lift down loopends and lift up blocks)

        let ret = v.into_iter().map(Control::Body).collect();
        let ret = self.insert_loop_block(ret);
        let ret = self.adjust_loop_block(ret);
        ret
    }


    fn insert_loop_block<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let v = self.insert_loop(v);
        let v = self.insert_block(v);
        v
    }

    fn insert_loop<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let mut ret = Vec::new();
        let mut loop_targets = HashSet::new();
        let mut labels = HashSet::new();
        for c in v.iter() {
            match *c {
                Control::Body(ref block) => {
                    labels.insert(&block.name);

                    for label in block.branches().into_iter() {
                        if labels.contains(&label) {
                            loop_targets.insert(label);
                        }
                    }
                },
                _ => ()
            }
        }

        let mut closed_loops = HashSet::new();
        // note: iterating reverse order
        for c in v.into_iter().rev() {
            match c {
                Control::Body(block) => {
                    let name = &block.name;

                    // loopend
                    for label in block.branches().into_iter() {
                        if loop_targets.contains(&label) && ! closed_loops.contains(&label) {
                            ret.push(Control::LoopEnd(&label));
                            closed_loops.insert(label);
                        }
                    }
                    // ebb
                    ret.push(Control::Body(block));

                    // loop
                    if loop_targets.contains(name) {
                        ret.push(Control::Loop(name));
                    }
                },
                c => ret.push(c),
            }
        }
        let ret = ret.into_iter().rev().collect();
        ret
    }

    fn insert_block<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let mut ret = Vec::new();
        let mut block_targets = HashSet::new();
        let mut labels = HashSet::new();
        for c in v.iter() {
            match *c {
                Control::Body(ref block) => {
                    labels.insert(&block.name);

                    for label in block.branches().into_iter() {
                        if ! labels.contains(&label) {
                            block_targets.insert(label);
                        }
                    }
                },
                _ => ()
            }
        }

        let mut opened_blocks = HashSet::new();

        for c in v.into_iter() {
            match c {
                Control::Body(block) => {
                    let name = &block.name;

                    // blockend
                    if block_targets.contains(name) {
                        ret.push(Control::BlockEnd(name));
                    }

                    // block
                    for label in block.branches().into_iter() {
                        if block_targets.contains(&label) && ! opened_blocks.contains(&label){
                            ret.push(Control::Block(&label));
                            opened_blocks.insert(label);
                        }
                    }

                    // ebb
                    ret.push(Control::Body(block));
                },
                c => ret.push(c),
            }
        }
        ret
    }

    fn adjust_loop_block<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let v = self.adjust_loop(v);
        let v = self.adjust_block(v);
        v
    }


    fn adjust_loop<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let mut ret = Vec::new();
        let mut scope = Vec::new();
        let mut defers = HashMap::new();
        for c in v.into_iter() {
            match c {
                Control::Loop(name) => {
                    scope.push(name);
                    ret.push(c);
                },
                Control::LoopEnd(name) => {
                    let last_name = scope.pop().unwrap();
                    if name == last_name  {
                        ret.push(c);
                        for d in defers.remove(&name).unwrap() {
                            let ds = self.resolve_defers(d, &mut defers).into_iter().map(Control::LoopEnd);
                            ret.extend(ds);
                        }
                    } else {
                        defers.entry(last_name).or_insert(Vec::new()).push(name);
                        scope.push(last_name);
                    }
                },
                Control::Body(_) |
                Control::Block(_) |
                Control::BlockEnd(_) => ret.push(c),
            }
        }

        ret
    }

    fn adjust_block<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let mut tmp = Vec::new();
        let mut scope = Vec::new();
        let mut defers = HashMap::new();
        for c in v.into_iter().rev() {
            match c {
                Control::BlockEnd(name) | Control::LoopEnd(name) => {
                    scope.push(name);
                    tmp.push(c);
                },
                Control::Block(name) | Control::Loop(name) => {
                    let last_name = scope.pop().unwrap();
                    if name == last_name  {
                        tmp.push(c);
                        for d in defers.remove(&name).unwrap() {
                            let ds = self.resolve_defers(d, &mut defers).into_iter().map(Control::Block);
                            tmp.extend(ds);
                        }
                    } else {
                        // Note: in the arm of Loop, this else clause must not occure, thus assuming safe.
                        defers.entry(last_name).or_insert(Vec::new()).push(name);
                        scope.push(last_name);
                    }
                },
                c => tmp.push(c),
            }
        }

        tmp.into_iter().rev().collect()
    }

    fn resolve_defers<'a>(&mut self, name: &'a lir::Label, defers: &mut HashMap<&'a lir::Label, Vec<&'a lir::Label>>) -> Vec<&'a lir::Label>{
        let mut ret = Vec::new();
        ret.push(name);
        for d in defers.remove(&name).iter().flat_map(|v|v.iter()) {
            ret.extend(self.resolve_defers(d, defers))
        }
        ret
    }

}

impl<E> Pass<lir::LIR, E> for LIR2WASM {
    type Target = Module;

    fn trans(&mut self, lir: lir::LIR) -> ::std::result::Result<Self::Target, E> {
        Ok(self.trans_lir(lir))
    }
}
