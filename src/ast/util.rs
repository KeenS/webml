use crate::ast::*;

pub trait Traverse {
    fn traverse_ast(&mut self, ast: &mut AST) {
        for val in ast.0.iter_mut() {
            self.traverse_val(val)
        }
    }

    fn traverse_val(&mut self, val: &mut Val) {
        self.traverse_expr(&mut val.expr)
    }

    fn traverse_expr(&mut self, expr: &mut Expr) {
        use crate::ast::Expr::*;
        match *expr {
            Binds {
                ref mut ty,
                ref mut binds,
                ref mut ret,
            } => self.traverse_binds(ty, binds, ret),
            BinOp {
                ref mut op,
                ref mut ty,
                ref mut l,
                ref mut r,
            } => self.traverse_binop(op, ty, l, r),
            Fun {
                ref mut param_ty,
                ref mut param,
                ref mut body_ty,
                ref mut body,
            } => self.traverse_fun(param_ty, param, body_ty, body),
            App {
                ref mut ty,
                ref mut fun,
                ref mut arg,
            } => self.traverse_app(ty, fun, arg),
            If {
                ref mut ty,
                ref mut cond,
                ref mut then,
                ref mut else_,
            } => self.traverse_if(ty, cond, then, else_),
            Case {
                ref mut ty,
                ref mut cond,
                ref mut clauses,
            } => self.traverse_case(ty, cond, clauses),
            Tuple {
                ref mut ty,
                ref mut tuple,
            } => self.traverse_tuple(ty, tuple),

            Sym {
                ref mut ty,
                ref mut name,
            } => self.traverse_sym(ty, name),
            Lit {
                ref mut ty,
                ref mut value,
            } => self.traverse_lit(ty, value),
        }
    }
    fn traverse_binds(&mut self, _ty: &mut TyDefer, binds: &mut Vec<Val>, ret: &mut Box<Expr>) {
        for val in binds.iter_mut() {
            self.traverse_val(val)
        }
        self.traverse_expr(ret)
    }

    fn traverse_binop(
        &mut self,
        _op: &mut Symbol,
        _ty: &mut TyDefer,
        l: &mut Box<Expr>,
        r: &mut Box<Expr>,
    ) {
        self.traverse_expr(l);
        self.traverse_expr(r)
    }

    fn traverse_fun(
        &mut self,
        _param_ty: &mut TyDefer,
        _param: &mut Symbol,
        _body_ty: &mut TyDefer,
        body: &mut Box<Expr>,
    ) {
        self.traverse_expr(body)
    }

    fn traverse_app(&mut self, _ty: &mut TyDefer, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
        self.traverse_expr(fun);
        self.traverse_expr(arg);
    }

    fn traverse_if(
        &mut self,
        _ty: &mut TyDefer,
        cond: &mut Box<Expr>,
        then: &mut Box<Expr>,
        else_: &mut Box<Expr>,
    ) {
        self.traverse_expr(cond);
        self.traverse_expr(then);
        self.traverse_expr(else_);
    }

    fn traverse_case(
        &mut self,
        _ty: &mut TyDefer,
        cond: &mut Box<Expr>,
        clauses: &mut Vec<(Pattern, Expr)>,
    ) {
        self.traverse_expr(cond);
        for &mut (_, ref mut e) in clauses.iter_mut() {
            self.traverse_expr(e);
        }
    }

    fn traverse_tuple(&mut self, _ty: &mut TyDefer, tuple: &mut Vec<Expr>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_sym(&mut self, _ty: &mut TyDefer, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _ty: &mut TyDefer, _value: &mut Literal) {}
}
