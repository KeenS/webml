pub mod typing;

use prim::*;

#[derive(Debug, Clone)]
pub struct AST(pub Vec<Val>);

#[derive(Debug, Clone)]
pub struct Val{pub ty: TyDefer, pub rec: bool, pub name: Symbol, pub expr: Expr}

#[derive(Debug, Clone)]
pub enum Expr {
    Binds{ty: TyDefer, binds: Vec<Val>, ret: Box<Expr>},
    Add{ty: TyDefer, l: Box<Expr>, r: Box<Expr>},
    Mul{ty: TyDefer, l: Box<Expr>, r: Box<Expr>},
    Fun{param_ty: TyDefer, param: Symbol, body_ty: TyDefer, body: Box<Expr>},
    App{ty: TyDefer, fun: Box<Expr>, arg: Box<Expr>},
    If {ty: TyDefer, cond: Box<Expr>, then: Box<Expr>, else_: Box<Expr>},
    // Seq{ty: TyDefer, exprs: Vec<Expr>},
    Sym{ty: TyDefer, name: Symbol},
    Lit{ty: TyDefer, value: Literal},
}
