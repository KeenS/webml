extern crate web_assembler;

use std::collections::{HashSet, HashMap};

use prim::*;
use pass::Pass;
use mir;
use self::web_assembler::builder::*;
use self::web_assembler::*;

#[derive(Debug, Clone)]
enum Control<'a> {
    Ebb(&'a mir::EBB),
    Loop(&'a Symbol),
    LoopEnd(&'a Symbol),
    Block(&'a Symbol),
    BlockEnd(&'a Symbol),
}

fn print_control<'a>(c: &Control<'a>) {
    use self::Control::*;
    match c {
        &Ebb(ref e) => {
            println!("EBB{}", e.next_ebbs().iter().map(|&(name, _)| format!(" break {}", name.0)).collect::<String>())
        },
        &Loop(ref name) => {
            println!("loop: {}", name.0);
        }
        &LoopEnd(ref name) => {
            println!("loopend: {}", name.0);
        }
        &Block(ref name) => {
            println!("block: {}", name.0);
        }
        &BlockEnd(ref name) => {
            println!("blockend: {}", name.0);
        }
    }
}

fn ebbty_to_valuetype(t: &mir::EbbTy) -> ValueType {
    use mir::EbbTy::*;
    match t {
        &Unit => ValueType::I32,
        &Int => ValueType::I64,
        &Bool => ValueType::I32,
        &Cls {..} => Pointer,
        &Ebb {..} => Pointer,

    }
}

pub struct MIR2WASM {
    md: ModuleBuilder,
}

// may vary over environment
const Pointer: ValueType = ValueType::I64;

impl MIR2WASM {
    pub fn new() -> Self {
        MIR2WASM {
            md: ModuleBuilder::new()
        }
    }

    pub fn trans_mir(&mut self, m: mir::MIR) -> () {
        for f in m.0 {
            self.trans_function(f);
        }
    }

    fn trans_function(&mut self, f: mir::Function) {
        use mir::EbbTy::*;
        let mir::Function { body, body_ty, name } = f;
        let mut fb = FunctionBuilder::new(FuncType {
            params: body[0]
                .params
                .iter()
                .map(|&(ref ty, _)|
                     // fixme: unit param only fun should be non-param fun
                     ebbty_to_valuetype(ty)
                )
                .collect(),
            ret: match body_ty {
                Unit => None,
                ty => Some(ebbty_to_valuetype(&ty)),
            },
        });

        // without params
        let mut symbol_table = self.make_symbol_table(&mut fb, body.as_ref());


        // let fb = fb.code(|cb, params| {
        //     for (&(_, ref p), i) in body[0].params.iter().zip(params) {
        //         symbol_table.insert(p, i.clone());
        //     }


        //     let body = self.alloc_loop_block_break(&body);


        //     println!("fn {}", name.0);
        //     println!("{:?}", symbol_table);
        //     for c in body {
        //         print_control(&c);
        //     }
        //     println!("");

        //     let scope = Vec::new();
        //     for c in body {
        //         match c {
        //             Control::Block(name) => {
        //                 scope.push(name);
        //                 cb.block(BlockType(None));
        //             },
        //             Control::BlockEnd(name) => {
        //                 let top = scope.pop().unwrap();
        //                 assert_eq!(name, top);
        //                 cb.end();
        //             }
        //             Control::Loop(name) => {
        //                 scope.push(name);
        //                 cb.loop_(BlockType(None));
        //             },
        //             Control::LoopEnd(name) => {
        //                 let top = scope.pop().unwrap();
        //                 assert_eq!(name, top);
        //                 cb.end();
        //             },
        //             Control::Ebb(e) => {
        //                 use mir::Op::*;
        //                 for op in e.body {
        //                     match op {
        //                         Lit {var, value, ..} => {
        //                             match value {
        //                                 Literal::Bool(b) => cb.constant(b as i32),
        //                                 Literal::Int(i) => cb.constant(i),
        //                             };
        //                             cb.set_local(symbol_table[&var]);
        //                         },
        //                         Alias {var, sym, ..} => {
        //                             cb.get_local(symbol_table[&sym]);
        //                             cb.set_local(symbol_table[&var]);
        //                         },
        //                         Add {var, l, r, ..} => {
        //                             cb.get_local(symbol_table[&l]);
        //                             cb.get_local(symbol_table[&r]);
        //                             // FIXME: adjust argument type (bit size)
        //                             cb.i64_add();
        //                         },
        //                         Mul {var, l, r, ..} => {
        //                             cb.get_local(symbol_table[&l]);
        //                             cb.get_local(symbol_table[&r]);
        //                             // FIXME: adjust argument type (bit size)
        //                             cb.i64_mul();
        //                         },
        //                         Closure {
        //                             var: Symbol,
        //                             param_ty: EbbTy,
        //                             ret_ty: EbbTy,
        //                             fun: Symbol,
        //                             env: Vec<(EbbTy, Symbol)>,
        //                         },
        //                         Call {
        //                             var: Symbol,
        //                             ty: EbbTy,
        //                             fun: Symbol,
        //                             args: Vec<Symbol>,
        //                         },
        //                         Branch {cond, then, else_} => {
        //                             cb.get_local(symbol_table([&cond]));
        //                             let then_depth = scope.rindex(then);
        //                             let else_depth = scope.rindex(else_);
        //                             cb.br_if(then_depth);
        //                             cb.br(else_depth);
        //                         },
        //                         Jump {
        //                             target: Symbol,
        //                             forward: bool,
        //                             args: Vec<Symbol>,
        //                         },
        //                         Ret { value: Symbol } => {
        //                             cb.get_local(symbol_table([&value]));
        //                             cb.return_();
        //                         },

        //                     }}
        //             }
        //         }
        //     }
        //     cb
        // })
            ;


        // TODO:
        //  * [ ] symbol to local_variable
        //  * [ ] how to solve join point -> ebbs that have more than 1 incomings. branch point is dominant ebb.
        //  * [ ] how to handle loop -> strong connected components
        //  * [ ] how to handle jump -> jump other than loop or branch/join can be compacted. ignorable.
        // forward jump: block & break
        // backward jupm: loop & break
    }

    fn make_symbol_table<'a>(&self, fb: &mut FunctionBuilder, v: &'a [mir::EBB]) -> HashMap<&'a Symbol, LocalIndex> {
        let mut symbol_table = HashMap::<&'a Symbol, LocalIndex>::new();

        let params = v[0].params.iter().map(|&(_, ref var)| var).collect::<Vec<_>>();

        macro_rules! intern {
            ($ty: expr, $var: expr) => {{
                if !params.contains($var) {
                    symbol_table.entry($var).or_insert_with(|| fb.new_local($ty));
                }
            }}
        }

        for ebb in v {
            for &(ref ty, ref param) in ebb.params.iter() {
                intern!(ebbty_to_valuetype(ty), &param);
            }
            for op in ebb.body.iter() {
                match op {
                    &mir::Op::Lit {ref var, ref ty, ..} |
                    &mir::Op::Alias {ref var, ref ty, .. } |
                    &mir::Op::Add {ref var, ref ty, ..} |
                    &mir::Op::Mul {ref var, ref ty, ..} |
                    &mir::Op::Call {ref var, ref ty, ..} => {
                        intern!(ebbty_to_valuetype(ty), &var);
                    },
                    &mir::Op::Closure {ref var, ref param_ty, ref ret_ty, ..}  => {
                        intern!(Pointer, &var);
                    }
                    _ => ()
                }
            }
        }
        symbol_table
    }

    /// allocate block and loop scopes for jump -> break transformation.
    /// Forward jump will be block + break, and backword jump will be loop + break in following transformation.
    fn alloc_loop_block_break<'a>(&mut self, v: &'a [mir::EBB]) -> Vec<Control<'a>> {
        // assumes EBBs are already arranged

        // 1. calculate minimum coverings of loops, blocks and insert them
        // 2. adjust interleavings (lift down loopends and liftup blocks)

        let ret = v.iter().map(Control::Ebb).collect();
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
        for c in v.into_iter().rev() {
            match c {
                Control::Ebb(ebb) => {
                    // note: iterating reverse order
                    let name = &ebb.name;

                    // loopend
                    for (name, forward) in ebb.next_ebbs() {
                        if ! forward{
                            if ! loop_targets.contains(name) {
                                loop_targets.insert(name);
                                ret.push(Control::LoopEnd(name))
                            }
                        }
                    }
                    // ebb
                    ret.push(Control::Ebb(ebb));

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
        for c in v.into_iter() {
            match c {
                Control::Ebb(ebb) => {
                    let name = &ebb.name;

                    // blockend
                    if block_targets.contains(name) {
                        ret.push(Control::BlockEnd(name));
                    }

                    // block
                    for (name, forward) in ebb.next_ebbs() {
                        if forward && ! block_targets.contains(name) {
                            ret.push(Control::Block(name));
                            block_targets.insert(name);
                        }
                    }

                    // ebb
                    ret.push(Control::Ebb(ebb));
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
        let mut tmp = Vec::new();
        let mut scope = Vec::new();
        let mut defers = HashMap::new();
        for c in v.into_iter() {
            match c {
                Control::Loop(name) => {
                    scope.push(name);
                    tmp.push(c);
                },
                Control::LoopEnd(name) => {
                    let last_name = scope.pop().unwrap();
                    if name == last_name  {
                        tmp.push(c);
                        for d in defers.remove(&name).unwrap() {
                            let ds = self.resolve_defers(d, &mut defers).into_iter().map(Control::LoopEnd);
                            tmp.extend(ds);
                        }
                    } else {
                        defers.entry(last_name).or_insert(Vec::new()).push(name);
                        scope.push(last_name);
                    }
                },
                Control::Ebb(_) |
                Control::Block(_) |
                Control::BlockEnd(_) => tmp.push(c),
            }
        }

        tmp
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

    fn resolve_defers<'a>(&mut self, name: &'a Symbol, defers: &mut HashMap<&'a Symbol, Vec<&'a Symbol>>) -> Vec<&'a Symbol>{
        let mut ret = Vec::new();
        ret.push(name);
        for d in defers.remove(&name).iter().flat_map(|v|v.iter()) {
            ret.extend(self.resolve_defers(d, defers))
        }
        ret
    }

}

impl Pass<mir::MIR> for MIR2WASM {
    type Target = ();
    type Err = TypeError;

    fn trans(&mut self, mir: mir::MIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.trans_mir(mir))
    }
}
