use crate::config::Config;
use crate::lir;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::{HashMap, HashSet};
use wasm::builder::*;
use wasm::*;

#[derive(Debug, Clone)]
enum Control<'a> {
    Body(&'a lir::Block),
    Loop(&'a lir::Label),
    LoopEnd(&'a lir::Label),
    Block(&'a lir::Label),
    BlockEnd(&'a lir::Label),
}

fn lty_to_valuetype_opt(t: &lir::LTy) -> Option<ValueType> {
    use crate::lir::LTy::*;
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

fn fun_type(f: &lir::Function) -> FuncType {
    let &lir::Function {
        ref nparams,
        ref regs,
        ref ret_ty,
        ..
    } = f;
    let mut tys = regs
        .iter()
        .map(|reg| lty_to_valuetype(reg))
        .collect::<Vec<_>>();
    let _ = tys.split_off(*nparams as usize);
    FuncType {
        params: tys,
        ret: match ret_ty {
            ty => lty_to_valuetype_opt(&ty),
        },
    }
}

pub struct LIR2WASM;

impl LIR2WASM {
    pub fn new() -> Self {
        Self
    }

    fn generate_pass(&mut self, extern_types: lir::ExternTypes) -> LIR2WASMPass {
        let mut md = ModuleBuilder::new();
        let mut extern_functions = HashMap::new();
        let mut function_type_table = HashMap::new();
        for ((module, name), (paramtys, retty)) in extern_types {
            let ftype = FuncType {
                params: paramtys
                    .into_iter()
                    .map(|ty| lty_to_valuetype(&ty))
                    .collect(),
                ret: lty_to_valuetype_opt(&retty),
            };
            let tyind = if !function_type_table.contains_key(&ftype) {
                let tyi = md.add_type(ftype.clone());
                function_type_table.insert(ftype, tyi.clone());
                tyi
            } else {
                function_type_table[&ftype].clone()
            };

            let funind = md.import(module.clone(), name.clone(), tyind);
            let fun = md.function_index_of(funind).unwrap();
            extern_functions.insert((module, name), fun);
        }
        LIR2WASMPass::new(md, extern_functions, function_type_table)
    }
}

struct LIR2WASMPass {
    md: ModuleBuilder,
    init_fun: FunctionSpaceIndex,
    alloc_fun: FunctionSpaceIndex,
    extern_functions: HashMap<(String, String), FunctionSpaceIndex>,
    function_table: HashMap<Symbol, u32>,
    function_type_table: HashMap<FuncType, TypeIndex>,
    dynamic_function_table: HashMap<Symbol, u32>,
    dynamic_function_elements: Vec<FunctionSpaceIndex>,
}

impl LIR2WASMPass {
    fn new(
        mut md: ModuleBuilder,
        extern_functions: HashMap<(String, String), FunctionSpaceIndex>,
        mut function_type_table: HashMap<FuncType, TypeIndex>,
    ) -> Self {
        let init_fun_ty = funtype!(());
        let alloc_fun_ty = funtype!((i32) -> i32);
        let init_fun_ty_index = md.add_type(init_fun_ty.clone());
        let alloc_fun_ty_index = md.add_type(alloc_fun_ty.clone());
        let init_fun = md.import("webml-rt", "init", init_fun_ty_index);
        let init_fun = md.function_index_of(init_fun).unwrap();
        let alloc_fun = md.import("webml-rt", "alloc", alloc_fun_ty_index);
        let alloc_fun = md.function_index_of(alloc_fun).unwrap();

        function_type_table.extend(vec![
            (init_fun_ty, init_fun_ty_index),
            (alloc_fun_ty, alloc_fun_ty_index),
        ]);

        md.import(
            "webml-rt",
            "memory",
            MemoryType {
                limits: ResizableLimits::new(2),
            },
        );

        Self {
            md,
            init_fun,
            alloc_fun,
            extern_functions,
            function_table: HashMap::new(),
            function_type_table,
            dynamic_function_table: HashMap::new(),
            dynamic_function_elements: vec![],
        }
    }

    fn intern_fun(&mut self, fname: &Symbol) -> u32 {
        let index = self.function_index(fname);
        let &mut Self {
            ref mut dynamic_function_table,
            ref mut dynamic_function_elements,
            ..
        } = self;
        *dynamic_function_table
            .entry(fname.clone())
            .or_insert_with(|| {
                dynamic_function_elements.push(index);
                let ret = dynamic_function_elements.len() - 1;
                ret as u32
            })
    }

    pub fn trans_lir(&mut self, l: lir::LIR) -> Module {
        self.function_table =
            l.0.iter()
                .enumerate()
                .map(|(i, s)| (s.name.clone(), i as u32))
                .collect();
        {
            for f in l.0.iter() {
                let ftype = fun_type(f);
                if !self.function_type_table.contains_key(&ftype) {
                    let tyi = self.md.add_type(ftype.clone());
                    self.function_type_table.insert(ftype, tyi);
                }
            }
        }

        let nfunctions = l.0.len();
        for f in l.0 {
            self.trans_function(f);
        }
        let fun_table = self.md.new_table(ElemType::AnyFunc, (nfunctions as u32)..);
        let elems = ElemSegment {
            index: fun_table,
            offset: InitExpr(CodeBuilder::new().constant(0 as i32).end().build()),
            elems: self.dynamic_function_elements.clone(),
        };

        self.md.add_element(elems);
        let main_function = FunctionBuilder::new(funtype!(()))
            .code(|cb, _params| {
                cb.call(self.init_fun)
                    .call(self.function_index(&Symbol::new("sml-main")))
                    .return_()
            })
            .build();
        let main_function = self.md.new_function(main_function);
        self.md.start(main_function);

        let mut ret = ModuleBuilder::new();
        // FIXME:
        ::std::mem::swap(&mut self.md, &mut ret);
        ret.build()
    }

    fn function_index(&self, fname: &Symbol) -> FunctionSpaceIndex {
        let findex = FunctionIndex(self.function_table[fname]);
        Into::<FunctionSpaceIndex>::into(findex)
    }

    fn trans_function(&mut self, f: lir::Function) {
        use crate::lir::Value::*;
        let ftype = fun_type(&f);
        let lir::Function {
            nparams,
            regs,
            body,
            ..
        } = f;
        let mut tys = regs
            .iter()
            .map(|reg| lty_to_valuetype(reg))
            .collect::<Vec<_>>();
        let regtys = tys.split_off(nparams as usize);
        let mut fb = FunctionBuilder::new(ftype.clone());

        let mut locals = fb.new_locals(regtys);

        let fb = fb.code(|mut cb, params| {
            let body = self.alloc_loop_block_break(&body);
            let mut params = params.to_vec();
            params.append(&mut locals);
            let mut scope = Vec::new();

            macro_rules! reg {
                ($reg: expr) => {
                    params[$reg.1 as usize]
                };
            }

            macro_rules! label {
                ($label: expr) => {
                    scope
                        .iter()
                        .rev()
                        .position(|x| x == $label)
                        .unwrap_or_else(|| panic!("internal error: label not found: {:?}", $label))
                        as u32
                };
            }

            for c in body {
                match c {
                    Control::Block(name) => {
                        scope.push(name);
                        cb = cb.block(BlockType(None));
                    }
                    Control::BlockEnd(name) => {
                        let top = scope.pop().unwrap();
                        assert_eq!(name, top);
                        cb = cb.end();
                    }
                    Control::Loop(name) => {
                        scope.push(name);
                        cb = cb.loop_(BlockType(None));
                    }
                    Control::LoopEnd(name) => {
                        let top = scope.pop().unwrap();
                        assert_eq!(name, top);
                        cb = cb.end();
                    }
                    Control::Body(b) => {
                        use crate::lir::Op::*;
                        for op in &b.body {
                            match op {
                                ConstI32(reg, c) => {
                                    cb = cb.constant(*c as i32).set_local(reg!(reg))
                                }
                                AddI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_add()
                                        .set_local(reg!(reg1))
                                }
                                SubI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_sub()
                                        .set_local(reg!(reg1))
                                }
                                MulI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_mul()
                                        .set_local(reg!(reg1))
                                }
                                DivI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_div_s()
                                        .set_local(reg!(reg1))
                                }
                                ModI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_rem_s()
                                        .set_local(reg!(reg1))
                                }
                                EqI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_eq()
                                        .set_local(reg!(reg1))
                                }
                                NeqI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_ne()
                                        .set_local(reg!(reg1))
                                }
                                GtI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_gt_s()
                                        .set_local(reg!(reg1))
                                }
                                GeI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_ge_s()
                                        .set_local(reg!(reg1))
                                }
                                LtI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_lt_s()
                                        .set_local(reg!(reg1))
                                }
                                LeI32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i32_le_s()
                                        .set_local(reg!(reg1))
                                }
                                MoveI32(reg1, reg2)
                                | MoveI64(reg1, reg2)
                                | MoveF32(reg1, reg2)
                                | MoveF64(reg1, reg2) => {
                                    cb = cb.get_local(reg!(reg2)).set_local(reg!(reg1))
                                }
                                StoreI32(addr, value) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .get_local(reg!(value))
                                        .i32_store(addr.1);
                                }
                                LoadI32(reg, addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .i32_load(addr.1)
                                        .set_local(reg!(reg));
                                }
                                JumpIfI32(reg, label) => {
                                    cb = cb.get_local(reg!(reg)).br_if(label!(&label));
                                }

                                JumpTableI32(reg, labels, default) => {
                                    cb = cb.get_local(reg!(reg)).br_table(
                                        labels.iter().map(|l| label!(&l)).collect(),
                                        default.as_ref().map(|l| label!(&l)).unwrap_or(
                                            // FIXME: should be `unreachable` branch
                                            0,
                                        ),
                                    );
                                }

                                ConstI64(reg, c) => {
                                    cb = cb.constant(*c as i64).set_local(reg!(reg))
                                }
                                AddI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_add()
                                        .set_local(reg!(reg1))
                                }
                                SubI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_sub()
                                        .set_local(reg!(reg1))
                                }
                                MulI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_mul()
                                        .set_local(reg!(reg1))
                                }
                                DivI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_div_s()
                                        .set_local(reg!(reg1))
                                }
                                ModI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_rem_s()
                                        .set_local(reg!(reg1))
                                }
                                EqI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_eq()
                                        .set_local(reg!(reg1))
                                }
                                NeqI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_ne()
                                        .set_local(reg!(reg1))
                                }
                                GtI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_gt_s()
                                        .set_local(reg!(reg1))
                                }
                                GeI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_ge_s()
                                        .set_local(reg!(reg1))
                                }
                                LtI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_lt_s()
                                        .set_local(reg!(reg1))
                                }
                                LeI64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .i64_le_s()
                                        .set_local(reg!(reg1))
                                }
                                LoadI64(reg, addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .i64_load(addr.1)
                                        .set_local(reg!(reg));
                                }

                                StoreI64(addr, value) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .get_local(reg!(value))
                                        .i64_store(addr.1);
                                }

                                ConstF32(reg, c) => cb = cb.constant(*c).set_local(reg!(reg)),
                                AddF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_add()
                                        .set_local(reg!(reg1))
                                }
                                SubF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_sub()
                                        .set_local(reg!(reg1))
                                }
                                MulF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_mul()
                                        .set_local(reg!(reg1))
                                }
                                DivF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_div()
                                        .set_local(reg!(reg1))
                                }
                                EqF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_eq()
                                        .set_local(reg!(reg1))
                                }
                                NeqF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_ne()
                                        .set_local(reg!(reg1))
                                }
                                GtF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_gt()
                                        .set_local(reg!(reg1))
                                }
                                GeF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_ge()
                                        .set_local(reg!(reg1))
                                }
                                LtF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_lt()
                                        .set_local(reg!(reg1))
                                }
                                LeF32(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f32_le()
                                        .set_local(reg!(reg1))
                                }
                                StoreF32(addr, value) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .get_local(reg!(value))
                                        .f32_store(addr.1);
                                }
                                LoadF32(reg, addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .f32_load(addr.1)
                                        .set_local(reg!(reg));
                                }

                                ConstF64(reg, c) => cb = cb.constant(*c).set_local(reg!(reg)),
                                AddF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_add()
                                        .set_local(reg!(reg1))
                                }
                                SubF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_sub()
                                        .set_local(reg!(reg1))
                                }
                                MulF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_mul()
                                        .set_local(reg!(reg1))
                                }
                                DivF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_div()
                                        .set_local(reg!(reg1))
                                }
                                EqF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_eq()
                                        .set_local(reg!(reg1))
                                }
                                NeqF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_ne()
                                        .set_local(reg!(reg1))
                                }
                                GtF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_gt()
                                        .set_local(reg!(reg1))
                                }
                                GeF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_ge()
                                        .set_local(reg!(reg1))
                                }
                                LtF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_lt()
                                        .set_local(reg!(reg1))
                                }
                                LeF64(reg1, reg2, reg3) => {
                                    cb = cb
                                        .get_local(reg!(reg2))
                                        .get_local(reg!(reg3))
                                        .f64_le()
                                        .set_local(reg!(reg1))
                                }
                                StoreF64(addr, value) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .get_local(reg!(value))
                                        .f64_store(addr.1);
                                }

                                LoadF64(reg, addr) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .f64_load(addr.1)
                                        .set_local(reg!(reg));
                                }

                                HeapAlloc(reg, value, _tys) => {
                                    cb = match value {
                                        I(i) => cb.constant(*i as i32),
                                        R(r) => cb.get_local(reg!(r)),
                                    };

                                    cb = cb//.constant(tys_to_ptrbits(tys) as i32)
                                        .call(self.alloc_fun)
                                        .set_local(reg!(reg))
                                }
                                StackAlloc(reg, size, _tys) => {
                                    // allocating to heap, not stack
                                    cb = cb
                                        .constant(*size as i32)
                                        //.constant(tys_to_ptrbits(tys) as i32)
                                        .call(self.alloc_fun)
                                        .set_local(reg!(reg))
                                }
                                StoreFnPtr(addr, value) => {
                                    cb = cb
                                        .get_local(reg!(addr.0))
                                        .constant(self.intern_fun(&value) as i32)
                                        .i32_store(addr.1);
                                }

                                ClosureCall(reg, fun, args) => {
                                    cb = cb
                                        .get_local(reg!(fun))
                                        // load ptr to captured env
                                        // assuming sizeof(*p) == 4
                                        .constant(4)
                                        .i32_add();

                                    // load the rest args
                                    for arg in args.iter() {
                                        cb = cb.get_local(reg!(arg))
                                    }

                                    let ret = lty_to_valuetype_opt(&reg.0);
                                    let ftype = {
                                        let mut params = vec![
                                            // pointer to closure
                                            ValueType::I32,
                                        ];
                                        params.extend(args.iter().map(|r| lty_to_valuetype(&r.0)));
                                        FuncType {
                                            params,
                                            ret: ret.clone(),
                                        }
                                    };

                                    cb = cb
                                        .get_local(reg!(fun))
                                        // load function
                                        .i32_load(0)
                                        .call_indirect(self.function_type_table[&ftype], false);

                                    if let Some(_) = ret {
                                        cb = cb.set_local(reg!(reg));
                                    }
                                }
                                FunCall(reg, fun, args) => {
                                    for arg in args.iter() {
                                        cb = cb.get_local(reg!(arg))
                                    }

                                    cb = cb.call(self.function_index(&fun));
                                    let ret = lty_to_valuetype_opt(&reg.0);
                                    if let Some(_) = ret {
                                        cb = cb.set_local(reg!(reg));
                                    }
                                }
                                ExternCall(reg, module, fun, args) => {
                                    for arg in args.iter() {
                                        cb = cb.get_local(reg!(arg))
                                    }
                                    let fun = self.extern_functions[&(module.clone(), fun.clone())];
                                    cb = cb.call(fun);
                                    let ret = lty_to_valuetype_opt(&reg.0);
                                    if let Some(_) = ret {
                                        cb = cb.set_local(reg!(reg));
                                    }
                                }
                                Jump(label) => {
                                    cb = cb.br(label!(&label));
                                }
                                Unreachable => {
                                    cb = cb.unreachable();
                                }
                                Ret(reg) => {
                                    cb = match reg {
                                        Some(r) => cb.get_local(reg!(r)),
                                        None => cb,
                                    };
                                    cb = cb.return_()
                                }
                            }
                        }
                    }
                }
            }
            cb
        });
        let (_, body) = fb.build();
        // use calculated type index,
        NewFunction::new_function(&mut self.md, self.function_type_table[&ftype], body);
    }

    /// allocate block and loop scopes for jump -> break transformation.
    /// Forward jump will be block + break,
    /// and backword jump will be loop + break in following transformation.
    fn alloc_loop_block_break<'a>(&mut self, v: &'a [lir::Block]) -> Vec<Control<'a>> {
        // 1. calculate minimum coverings of loops, blocks and insert them
        // 2. adjust interleavings (lift down loopends and lift up blocks)

        let ret = v.iter().map(Control::Body).collect();
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
        let mut loop_targets = HashSet::new();
        let mut labels = HashMap::new();
        for (i, c) in v.iter().enumerate() {
            match *c {
                Control::Body(ref block) => {
                    labels.insert(&block.name, i);

                    for label in block.branches().into_iter() {
                        if labels.contains_key(&label) {
                            loop_targets.insert(label);
                        }
                    }
                }
                _ => (),
            }
        }

        let mut ret = Vec::new();
        let mut closed_loops = HashSet::new();
        // note: iterating reverse order
        for c in v.into_iter().rev() {
            match c {
                Control::Body(block) => {
                    let name = &block.name;

                    // loopend
                    let mut targets = block
                        .branches()
                        .into_iter()
                        .map(|label| (labels[&label], label))
                        .collect::<Vec<_>>();
                    targets.sort_by_key(|(n, _)| *n);
                    for (_, label) in targets {
                        if loop_targets.contains(&label) && !closed_loops.contains(&label) {
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
                }
                c => ret.push(c),
            }
        }
        let ret = ret.into_iter().rev().collect();
        ret
    }

    fn insert_block<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let mut block_targets = HashSet::new();
        let mut labels = HashMap::new();
        for (i, c) in v.iter().enumerate() {
            match *c {
                Control::Body(ref block) => {
                    labels.insert(&block.name, i);

                    for label in block.branches().into_iter() {
                        if !labels.contains_key(&label) {
                            block_targets.insert(label);
                        }
                    }
                }
                _ => (),
            }
        }

        let mut ret = Vec::new();
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
                    let mut targets = block
                        .branches()
                        .into_iter()
                        .map(|label| (labels[&label], label))
                        .collect::<Vec<_>>();
                    targets.sort_by_key(|(n, _)| *n);
                    for (_, label) in targets {
                        if block_targets.contains(&label) && !opened_blocks.contains(&label) {
                            ret.push(Control::Block(&label));
                            opened_blocks.insert(label);
                        }
                    }

                    // ebb
                    ret.push(Control::Body(block));
                }
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
                }
                Control::LoopEnd(name) => {
                    let last_name = scope.pop().unwrap();
                    if name == last_name {
                        ret.push(c);
                        for d in defers.remove(&name).unwrap() {
                            let ds = self
                                .resolve_defers(d, &mut defers)
                                .into_iter()
                                .map(Control::LoopEnd);
                            ret.extend(ds);
                        }
                    } else {
                        defers.entry(last_name).or_insert(Vec::new()).push(name);
                        scope.push(last_name);
                    }
                }
                Control::Body(_) | Control::Block(_) | Control::BlockEnd(_) => ret.push(c),
            }
        }
        assert!(defers.is_empty());
        ret
    }

    fn adjust_block<'a>(&mut self, v: Vec<Control<'a>>) -> Vec<Control<'a>> {
        let mut tmp = Vec::new();
        let mut scope = Vec::new();
        let mut defers = HashMap::<&lir::Label, Vec<&lir::Label>>::new();
        for c in v.into_iter().rev() {
            match c {
                Control::BlockEnd(name) | Control::LoopEnd(name) => {
                    scope.push(name);
                    tmp.push(c);
                }
                Control::Block(name) | Control::Loop(name) => {
                    let last_name = scope.pop().unwrap();
                    if name == last_name {
                        tmp.extend(
                            self.resolve_defers(name, &mut defers)
                                .into_iter()
                                .map(Control::Block),
                        );
                    } else {
                        // Note: in the arm of Loop, this else clause must not occure,
                        // thus assuming safe.
                        defers.entry(last_name).or_insert(Vec::new()).push(name);
                        scope.push(last_name);
                    }
                }
                c => tmp.push(c),
            }
        }
        assert!(defers.is_empty());
        tmp.into_iter().rev().collect()
    }

    fn resolve_defers<'a>(
        &mut self,
        name: &'a lir::Label,
        defers: &mut HashMap<&'a lir::Label, Vec<&'a lir::Label>>,
    ) -> Vec<&'a lir::Label> {
        let mut ret = Vec::new();
        ret.push(name);
        let mut rest = Vec::new();
        rest.extend(defers.remove(&name).iter().flat_map(|v| v.iter()).rev());
        while !rest.is_empty() {
            let mut tmp = Vec::new();
            for d in rest {
                ret.push(d);
                tmp.extend(defers.remove(&d).iter().flat_map(|v| v.iter()).rev())
            }
            rest = tmp;
        }
        ret
    }
}

impl<E> Pass<(lir::ExternTypes, lir::LIR), E> for LIR2WASM {
    type Target = Module;

    fn trans(
        &mut self,
        (extern_types, lir): (lir::ExternTypes, lir::LIR),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        let mut pass = self.generate_pass(extern_types);
        Ok(pass.trans_lir(lir))
    }
}
