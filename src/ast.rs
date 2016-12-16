use ty::{TyDefer};

#[derive(Debug, Clone)]
pub enum AST {
    // TopFun(Fun),
    Top(Bind)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binds{ty: TyDefer, binds: Vec<Bind>, ret: Box<Expr>},
    Add{ty: TyDefer, l: Box<Expr>, r: Box<Expr>},
    Mul{ty: TyDefer, l: Box<Expr>, r: Box<Expr>},
    Fun{ty: TyDefer, param: Symbol, body: Box<Expr>},
    App{ty: TyDefer, fun: Box<Expr>, arg: Box<Expr>},
    If {ty: TyDefer, cond: Box<Expr>, then: Box<Expr>, else_: Box<Expr>},
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym(Symbol),
    LitInt(i64),
    LitBool(bool),
}

#[derive(Debug, Clone)]
pub enum Bind {
    // F(Fun),
    V(Val),
}

impl Bind {
    pub fn name(&self) -> &Symbol {
        match self {
            // &Bind::F(ref f) => &f.name,
            &Bind::V(ref v) => &v.name,
        }
    }

    pub fn ty(&self) -> &TyDefer {
        match self {
            // &Bind::F(ref f) => &f.ty,
            &Bind::V(ref v) => &v.ty,
        }
    }
}

// #[derive(Debug)]
// pub struct Fun{pub ty: TyDefer, pub name: Symbol, pub args: Vec<Symbol>, pub body: Expr}
#[derive(Debug, Clone)]
pub struct Val{pub ty: TyDefer, pub name: Symbol, pub expr: Expr}
#[derive(Debug, Clone)]
pub struct Symbol(pub String);

