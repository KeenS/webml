use std::path::Path;
use wasmtime::*;

fn add_ffi_module(linker: &mut Linker<()>) {
    linker
        .func_wrap("js-ffi", "print", |x: i32| {
            print!("{}", char::from_u32(x as u32).unwrap())
        })
        .expect("failed to add ffi functions");
}
fn add_rt_module(store: &mut Store<()>, linker: &mut Linker<()>) {
    let module_data =
        include_bytes!("../../webml-rt/target/wasm32-unknown-unknown/release/webml_rt.wasm");
    let module =
        Module::from_binary(linker.engine(), module_data).expect("failed to compile webml_rt");
    let instance = linker
        .instantiate(store.as_context_mut(), &module)
        .expect("failed to instanciate webml_rt");
    linker
        .instance(store.as_context_mut(), "webml-rt", instance)
        .expect("failed to import webml-rt");
}

pub fn linker(store: &mut Store<()>) -> Linker<()> {
    let mut linker = Linker::new(store.engine());
    add_ffi_module(&mut linker);
    add_rt_module(store, &mut linker);
    linker
}

pub struct WebmlInterp {
    linker: Linker<()>,
    store: Store<()>,
}

impl WebmlInterp {
    pub fn new() -> Self {
        let mut store = Store::default();
        Self {
            linker: linker(&mut store),
            store,
        }
    }

    fn is_wasm(prog: &[u8]) -> bool {
        4 <= prog.len() && &prog[0..4] == b"\0asm"
    }

    pub fn run_file(&mut self, path: impl AsRef<Path>) {
        use std::fs;
        let prog = fs::read(path).expect("failed to read file");
        self.run(prog)
    }

    pub fn run(&mut self, prog: Vec<u8>) {
        if Self::is_wasm(&prog) {
            self.run_wasm(&prog)
        } else {
            let prog = String::from_utf8(prog).expect("program must be a utf-8 string");
            let module_data = Self::compile(prog);
            self.run_wasm(&module_data)
        }
    }

    pub fn run_wasm(&mut self, module_data: &[u8]) {
        let module = Module::from_binary(self.linker.engine(), module_data)
            .expect("failed to compile module");
        self.linker
            .instantiate(&mut self.store, &module)
            .expect("failed to instanciate module");
    }

    pub fn compile(input: String) -> Vec<u8> {
        use webml::{compile_strings, Config};
        let prelude = include_str!("../../ml_src/prelude.sml").to_string();
        compile_strings(vec![prelude, input], &Config::default()).expect("failed to compile")
    }
}
