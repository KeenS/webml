use ty::Ty;

#[derive(Debug)]
pub enum HIR {
    Top(Val)
}

#[derive(Debug)]
pub enum Expr {
    Binds{ty: Ty, binds: Vec<Val>, ret: Box<Expr>},
    Fun{ty: Ty, param: Symbol, body: Box<Expr>},
    If {ty: Ty, cond: Box<Expr>, then: Box<Expr>, else_: Box<Expr>},
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym(Symbol),
    LitInt(i64),
    LitBool(bool),
}

#[derive(Debug)]
pub struct Val{pub ty: Ty, pub name: Symbol, pub expr: Expr}
#[derive(Debug)]
pub struct Symbol(pub String);

pub struct AST2HIR;
