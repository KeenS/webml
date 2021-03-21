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
        TestRuntime {
            linker: test_runtime(),
        }
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

pub fn compile(input: &str) -> Vec<u8> {
    use webml::{compile_string, Config};
    let mut prelude = include_str!("../../ml_src/prelude.sml").to_string();
    prelude.push_str(input);
    compile_string(prelude, &Config::default()).expect("failed to compile")
}

#[test]
fn test_add_and_print() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/add_and_print.sml"));

    tester.test_output(&module, "3\n");
}

#[test]
fn test_big_expression() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/big_expression.sml"));

    tester.test_output(&module, "24\n");
}

#[test]
fn test_binary_operators() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/binary_operators.sml"));

    tester.test_output(&module, "");
}

#[test]
fn test_boolean_case() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/boolean_case.sml"));

    tester.test_output(&module, "1\n1\n2\n3\n5\n8\n");
}

#[test]
fn test_branches() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/branches.sml"));

    tester.test_output(&module, "1\n");
}

#[test]
fn test_char() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/char.sml"));

    tester.test_output(&module, "0\n1\n");
}

#[test]
fn test_closures() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/closures.sml"));

    tester.test_output(&module, "3\n");
}

#[test]
fn test_datatype() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/datatype.sml"));

    tester.test_output(&module, "");
}

#[test]
fn test_datatype_pattern() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/datatype_pattern.sml"));

    tester.test_output(&module, "");
}

#[test]
fn test_fibonacci() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/fibonacci.sml"));

    tester.test_output(&module, "1\n1\n2\n3\n5\n8\n");
}
#[test]
fn test_fn() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/fn.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_if() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/if.sml"));

    tester.test_output(&module, "1\n");
}
#[test]
fn test_infix() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/infix.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_int_list() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/int_list.sml"));

    tester.test_output(&module, "1\n2\n3\n");
}
#[test]
fn test_integer_case() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/integer_case.sml"));

    tester.test_output(&module, "1\n1\n2\n3\n5\n8\n");
}
#[test]
fn test_multi_clause_fun() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/multi_clause_fun.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_nested_datatype_pattern() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/nested_datatype_pattern.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_nested_pattern_in_val() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/nested_pattern_in_val.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_non_alphanumeric_identifier() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!(
        "../../ml_example/non_alphanumeric_identifier.sml"
    ));

    tester.test_output(&module, "");
}
#[test]
fn test_overloaded_add() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/overloaded_add.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_pattern_in_funarg() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/pattern_in_funarg.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_prelude() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/prelude.sml"));

    tester.test_output(&module, "100000\n");
}
#[test]
fn test_random_expressions() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/random_expressions.sml"));

    tester.test_output(&module, "");
}
#[test]
fn test_tuple_pattern() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/tuple_pattern.sml"));

    tester.test_output(&module, "2\n");
}
#[test]
fn test_variable_scope() {
    let mut tester = TestRuntime::new();
    let module = compile(include_str!("../../ml_example/variable_scope.sml"));

    tester.test_output(&module, "3\n");
}
