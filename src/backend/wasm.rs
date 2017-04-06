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

fn lty_to_valuetype(t: &lir::LTy) -> ValueType {
    use lir::LTy::*;
    match t {
        &I32 => ValueType::I32,
        &I64 => ValueType::I64,

    }
}

pub struct LIR2WASM {
    md: ModuleBuilder,
    alloc_fun: FunctionSpaceIndex,
    function_table: HashMap<Symbol, u32>
}

// may vary over environment
const Pointer: ValueType = ValueType::I64;

impl LIR2WASM {
    pub fn new() -> Self {
        let mut md =  ModuleBuilder::new();
        let fun_index = md.add_type(FuncType{
            params: vec![ValueType::I64],
            ret: Some(ValueType::I64)
        });
        let alloc_fun = md.import("wasm-rt", "alloc", fun_index);

        LIR2WASM {
            md: md,
            alloc_fun: alloc_fun.into(),
            function_table: HashMap::new(),
        }
    }

    pub fn trans_lir(&mut self, l: lir::LIR) -> Module {
        self.function_table = l.0.iter()
            .enumerate()
            .map(|(i, s)| (s.name.clone(), i as u32))
            .collect();

        println!("{:?}", self.function_table);
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
        let lir::Function { name, nparams, regs, ret_ty, body } = f;
        let mut tys = regs.iter().map(|reg| lty_to_valuetype(reg)).collect::<Vec<_>>();
        let regtys = tys.split_off(nparams as usize);
        let mut fb = FunctionBuilder::new(FuncType {
            params: tys,
            ret: match ret_ty {
                ty => Some(lty_to_valuetype(&ty)),
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
                ($label: expr) => (scope.iter().rposition(|x| *x == $label).expect("internal error") as u32)
            }

            macro_rules! fun {
                ($funname: expr) => {{
                    println!("{:?}", $funname);
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
                                MoveI32(ref reg1, ref reg2) => {
                                    cb = cb.get_local(reg!(reg1))
                                        .set_local(reg!(reg2))
                                },
                                StoreI32(ref addr, ref value) => {
                                    cb = match *value {
                                        I(i) => cb.constant(i as i32),
                                        R(ref r) => cb.get_local(reg!(r)),
                                        F(ref s) => cb.constant(*fun!(s) as i32),
                                    };
                                    cb = cb
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
                                    cb = cb.constant(c as i32)
                                        .set_local(reg!(reg))
                                },
                                MoveI64(ref reg1, ref reg2) => {
                                    cb = cb.get_local(reg!(reg1))
                                        .set_local(reg!(reg2))
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
                                StoreI64(ref addr, ref value) => {
                                    cb = match *value {
                                        I(i) => cb.constant(i as i64),
                                        R(ref r) => cb.get_local(reg!(r)),
                                        F(ref s) => cb.constant(*fun!(s) as i32)
                                    };
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .i64_store(addr.1);
                                },
                                LoadI64(ref reg, ref addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .i64_load(addr.1)
                                        .set_local(reg!(reg));
                                },

                                HeapAlloc(ref reg, ref value) => {
                                    cb = match *value {
                                        I(i) => cb.constant(i as i64),
                                        R(ref r) => cb.get_local(reg!(r)),
                                        F(_) => panic!("unsupported operation")
                                    };

                                    cb = cb
                                        .call(self.alloc_fun)
                                        .set_local(reg!(reg))
                                },
                                StackAlloc(ref reg, size) => {
                                    // allocating to heap, not stack
                                    cb = cb
                                        .constant(size as i32)
                                        .call(self.alloc_fun)
                                        .set_local(reg!(reg))
                                },
                                Call(ref reg, ref fun, ref args) => {
                                    for arg in args.iter() {
                                        cb = cb.get_local(reg!(arg))
                                    }
                                    cb = match *fun {
                                        I(_) => panic!("unsupported operation"),
                                        R(ref r) => {
                                            unimplemented!();
                                            cb.call_indirect(*reg!(r), false)
                                        },
                                        F(ref f) => cb.call(fun!(f)),
                                    };

                                    cb = cb
                                        .set_local(reg!(reg));
                                },
                                Jump(ref label) => {
                                    cb = cb.br(label!(label));
                                },
                                Ret(ref reg) => {
                                    cb = cb.get_local(reg!(reg))
                                        .return_()
                                },

                            }
                        }
                    }
                }
            }
            cb
        });
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

impl Pass<lir::LIR> for LIR2WASM {
    type Target = Module;
    type Err = TypeError;

    fn trans(&mut self, lir: lir::LIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.trans_lir(lir))
    }
}
