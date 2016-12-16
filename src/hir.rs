use ast;
use ty::Ty;
use typing::TypeError;
use pass::Pass;

#[derive(Debug)]
pub struct HIR(pub Vec<Val>);

#[derive(Debug)]
pub enum Expr {
    Binds{ty: Ty, binds: Vec<Val>, ret: Box<Expr>},
    PrimFun{ty: Ty, name: Symbol},
    Fun{ty: Ty, param: Symbol, body: Box<Expr>},
    App{ty: Ty, fun: Box<Expr>, arg: Box<Expr>},
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

impl Expr {
    fn add() -> Expr {
        Expr::PrimFun {
            ty: Ty::Fun(Box::new(Ty::Int), Box::new(Ty::Fun(Box::new(Ty::Int), Box::new(Ty::Int)))),
            name: Symbol("+".to_string())
        }
    }

    fn mul() -> Expr {
        Expr::PrimFun {
            ty: Ty::Fun(Box::new(Ty::Int), Box::new(Ty::Fun(Box::new(Ty::Int), Box::new(Ty::Int)))),
            name: Symbol("*".to_string())
        }
    }

    fn app1(self, ty: Ty, e: Expr) -> Expr {
        Expr::App {
            ty: ty,
            fun: Box::new(self),
            arg: Box::new(e),
        }
    }
}

pub struct AST2HIR;

impl AST2HIR {
    fn conv_asts(&self, asts: Vec<ast::AST>) -> HIR {
        HIR(asts.into_iter().map(|ast| self.conv_ast(ast)).collect())
    }

    fn conv_ast(&self, ast: ast::AST) -> Val {
        match ast {
            ast::AST::Top(ast::Bind::V(v)) => self.conv_val(v)
        }
    }

    fn conv_val(&self, val: ast::Val) -> Val {
        Val {
            ty: val.ty.force("internal typing error"),
            name: self.conv_sym(val.name),
            expr: self.conv_expr(val.expr)
        }
    }

    fn conv_expr(&self, expr: ast::Expr) -> Expr {
        match expr {
            ast::Expr::Binds{ty, binds, ret} =>
                Expr::Binds {
                    ty: ty.force("internal typing error"),
                    binds: binds.into_iter().map(|b| self.conv_bind(b)).collect(),
                    ret: Box::new(self.conv_expr(*ret)),
                },
            ast::Expr::Add{ty, l, r} =>
                Expr::add()
                .app1(Ty::Fun(Box::new(Ty::Int), Box::new(Ty::Int)), self.conv_expr(*l))
                .app1(ty.force("internal typing error"), self.conv_expr(*r)),
            ast::Expr::Mul{ty, l, r} =>
                Expr::mul()
                .app1(Ty::Fun(Box::new(Ty::Int), Box::new(Ty::Int)),self.conv_expr(*l))
                .app1(ty.force("internal typing error"), self.conv_expr(*r)),
            ast::Expr::Fun{ty, param, body} =>
                Expr::Fun {
                    ty: ty.defined().expect("internal typing error"),
                    param: self.conv_sym(param),
                    body: Box::new(self.conv_expr(*body)),

                },
            ast::Expr::App{ty, fun, arg} =>
                self.conv_expr(*fun).app1(ty.force("internal typing error"), self.conv_expr(*arg)),
            ast::Expr::If {ty, cond, then, else_} =>
                Expr::If {
                    ty: ty.force("internal typing error"),
                    cond: Box::new(self.conv_expr(*cond)),
                    then: Box::new(self.conv_expr(*then)),
                    else_: Box::new(self.conv_expr(*else_)),
                },
            ast::Expr::Sym(s) =>
                Expr::Sym(self.conv_sym(s)),
            ast::Expr::LitInt(i) =>
                Expr::LitInt(i),
            ast::Expr::LitBool(b) =>
                Expr::LitBool(b)
        }
    }

    fn conv_bind(&self, b: ast::Bind) -> Val {
        match b {
            ast::Bind::V(val) =>  self.conv_val(val)
        }
    }

    fn conv_sym(&self, sym: ast::Symbol) -> Symbol {
        Symbol(sym.0)
    }
}

impl Pass<Vec<ast::AST>> for AST2HIR {
    type Target = HIR;
    type Err = TypeError;

    fn trans(&mut self, asts: Vec<ast::AST>) -> Result<Self::Target, Self::Err> {
        Ok(self.conv_asts(asts))
    }
}
