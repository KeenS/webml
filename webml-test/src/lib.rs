use std::cell::RefCell;
use std::io::prelude::*;
use wasmtime::*;

thread_local! {
    static OUTPUT: RefCell<Vec<u8>> = RefCell::new(Vec::new());
}

fn add_ffi_module(linker: &mut Linker) {
    linker
        .func("js-ffi", "print", |x: i32| {
            OUTPUT
                .with(|out| writeln!(out.borrow_mut(), "{}", x))
                .expect("failed to write");
        })
        .expect("failed to add ffi functions");
}
fn add_rt_module(linker: &mut Linker) {
    let module_data =
        include_bytes!("../../webml-rt/target/wasm32-unknown-unknown/release/webml_rt.wasm");
    let module =
        Module::from_binary(linker.store(), module_data).expect("failed to compile webml_rt");
    let instance = linker
        .instantiate(&module)
        .expect("failed to instanciate webml_rt");
    linker
        .instance("webml-rt", &instance)
        .expect("failed to import webml-rt");
}

pub fn test_runtime() -> Linker {
    let store = Store::default();
    let mut linker = Linker::new(&store);
    add_ffi_module(&mut linker);
    add_rt_module(&mut linker);
    linker
}

pub struct TestRuntime {
    linker: Linker,
}

impl TestRuntime {
    pub fn new() -> Self {
        let store = Store::default();
        let mut linker = Linker::new(&store);
        add_ffi_module(&mut linker);
        add_rt_module(&mut linker);
        TestRuntime { linker }
    }

    pub fn output(&self) -> Vec<u8> {
        OUTPUT.with(|o| Vec::clone(&*o.borrow()))
    }

    pub fn run(&mut self, module_data: &[u8]) {
        let module = Module::from_binary(self.linker.store(), module_data)
            .expect("failed to compile module");
        self.linker
            .instantiate(&module)
            .expect("failed to instanciate module");
    }

    pub fn test_output(&mut self, module_data: &[u8], output: &str) {
        self.run(module_data);
        assert_eq!(self.output(), output.as_bytes());
    }
}

#[test]
fn itworks() {
    let mut tester = TestRuntime::new();

    let module = include_bytes!("../../out.wasm");

    tester.test_output(module, "0\n1\n");
}
