use ty::{TyDefer};

#[derive(Debug)]
pub enum AST {
    // TopFun(Fun),
    Top(Bind)
}

#[derive(Debug)]
pub enum Expr {
    Binds{ty: TyDefer, binds: Vec<Bind>, ret: Box<Expr>},
    Add{ty: TyDefer, l: Box<Expr>, r: Box<Expr>},
    Mul{ty: TyDefer, l: Box<Expr>, r: Box<Expr>},
    Fun{ty: TyDefer, arg: Symbol, body: Box<Expr>},
    If{cond: Box<Expr>, ty: TyDefer, then: Box<Expr>, else_: Box<Expr>},
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym(Symbol),
    LitInt(i64),
    LitBool(bool),
}

#[derive(Debug)]
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
#[derive(Debug)]
pub struct Val{pub ty: TyDefer, pub name: Symbol, pub expr: Expr}
#[derive(Debug)]
pub struct Symbol(pub String);

