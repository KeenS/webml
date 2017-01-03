extern crate web_assembler;

use mir;
use self::web_assembler::builder::*;
use self::web_assembler::*;

pub struct MIR2WASM {
    md: ModuleBuilder,
}

// may vary over environment
const Pointer: ValueType = ValueType::I64;

impl MIR2WASM {
    fn trans_function(&mut self, f: mir::Function) {
        use mir::EbbTy::*;
        let mir::Function { body, body_ty, name } = f;
        let mut fb = FunctionBuilder::new(FuncType {
            params: body[0]
                .params
                .iter()
                .map(|&(ref ty, _)| match ty {
                    &Unit | &Int | &Bool => ValueType::I32,
                    &Cls { .. } | &Ebb { .. } => Pointer,
                })
                .collect(),
            ret: match body_ty {
                Unit => None,
                Int | Bool => Some(ValueType::I32),
                Cls { .. } => Some(Pointer),
                Ebb { .. } => None,
            },
        });
        // TODO:
        //  * [ ] symbol to local_variable
        //  * [ ] how to solve join point -> ebbs that have more than 1 incomings. branch point is dominant ebb.
        //  * [ ] how to handle loop -> strong connected components
        //  * [ ] how to handle jump -> jump other than loop or branch/join can be compacted. ignorable.
    }
}
