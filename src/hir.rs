// use ty::Ty;

// enum HIR {
//     TopVal(Val)
// }

// enum Expr {
//     Let{ty: Ty, binds: Vec<Val>, ret: Box<Expr>},
//     Add{ty: Ty, l: Box<Expr>, r: Box<Expr>},
//     Mul{ty: Ty, l: Box<Expr>, r: Box<Expr>},
//     Fun{ty: Ty, args: Vec<Symbol>, body: Box<Expr>},
//     If{cond: Box<Expr>, ty: Ty, then: Box<Expr>, else_: Box<Expr>},
//     Seq{ty: Ty, exprs: Vec<Expr>},
//     LitInt(i64),
// }

// struct Val{ty: Ty, name: Symbol, expr: Expr}
// struct Symbol{ty: Ty, name: String}
