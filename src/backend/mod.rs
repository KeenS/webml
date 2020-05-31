pub mod wasm;
pub use self::wasm::LIR2WASM;
mod pp;

#[derive(Debug, Clone)]
pub struct Output(pub ::wasm::Module);
