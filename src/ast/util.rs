use crate::ast::*;

pub trait Traverse<Ty> {
    fn traverse_ast(&mut self, ast: &mut AST<Ty>) {
        for stmt in ast.0.iter_mut() {
            self.traverse_statement(stmt)
        }
    }

    fn traverse_statement(&mut self, stmt: &mut Statement<Ty>) {
        use Statement::*;
        match stmt {
            Val { pattern, expr } => self.traverse_val(pattern, expr),
            Fun { name, params, expr } => self.traverse_fun(name, params, expr),
        }
    }

    fn traverse_val(&mut self, __pattern: &mut Pattern<Ty>, expr: &mut Expr<Ty>) {
        self.traverse_expr(expr)
    }

    fn traverse_fun(
        &mut self,
        _name: &mut Symbol,
        _params: &mut Vec<(Ty, Symbol)>,
        expr: &mut Expr<Ty>,
    ) {
        self.traverse_expr(expr)
    }

    fn traverse_expr(&mut self, expr: &mut Expr<Ty>) {
        use crate::ast::Expr::*;
        match *expr {
            Binds {
                ref mut ty,
                ref mut binds,
                ref mut ret,
            } => self.traverse_binds(ty, binds, ret),
            BinOp {
                ref mut ty,
                ref mut op,
                ref mut l,
                ref mut r,
            } => self.traverse_binop(ty, op, l, r),
            Fn {
                ref mut ty,
                ref mut param,
                ref mut body,
            } => self.traverse_fn(ty, param, body),
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

            Symbol {
                ref mut ty,
                ref mut name,
            } => self.traverse_sym(ty, name),
            Literal {
                ref mut ty,
                ref mut value,
            } => self.traverse_lit(ty, value),
        }
    }
    fn traverse_binds(
        &mut self,
        _ty: &mut Ty,
        binds: &mut Vec<Statement<Ty>>,
        ret: &mut Box<Expr<Ty>>,
    ) {
        for stmt in binds.iter_mut() {
            self.traverse_statement(stmt)
        }
        self.traverse_expr(ret)
    }

    fn traverse_binop(
        &mut self,
        _ty: &mut Ty,
        _op: &mut Symbol,
        l: &mut Box<Expr<Ty>>,
        r: &mut Box<Expr<Ty>>,
    ) {
        self.traverse_expr(l);
        self.traverse_expr(r)
    }

    fn traverse_fn(&mut self, _ty: &mut Ty, _param: &mut Symbol, body: &mut Box<Expr<Ty>>) {
        self.traverse_expr(body)
    }

    fn traverse_app(&mut self, _ty: &mut Ty, fun: &mut Box<Expr<Ty>>, arg: &mut Box<Expr<Ty>>) {
        self.traverse_expr(fun);
        self.traverse_expr(arg);
    }

    fn traverse_if(
        &mut self,
        _ty: &mut Ty,
        cond: &mut Box<Expr<Ty>>,
        then: &mut Box<Expr<Ty>>,
        else_: &mut Box<Expr<Ty>>,
    ) {
        self.traverse_expr(cond);
        self.traverse_expr(then);
        self.traverse_expr(else_);
    }

    fn traverse_case(
        &mut self,
        _ty: &mut Ty,
        cond: &mut Box<Expr<Ty>>,
        clauses: &mut Vec<(Pattern<Ty>, Expr<Ty>)>,
    ) {
        self.traverse_expr(cond);
        for &mut (_, ref mut e) in clauses.iter_mut() {
            self.traverse_expr(e);
        }
    }

    fn traverse_tuple(&mut self, _ty: &mut Ty, tuple: &mut Vec<Expr<Ty>>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_sym(&mut self, _ty: &mut Ty, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _ty: &mut Ty, _value: &mut Literal) {}
}
