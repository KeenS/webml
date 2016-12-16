use prim::*;

pub enum Ops {
    Literal,
    App{ty: Ty, fun: Symbol, arg: Symbol},
    Fun{ty: Ty, param: Symbol, body: Control},
}

pub enum Control {
    Bind,
    If,
    Return,
}
