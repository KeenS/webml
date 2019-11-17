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
            Datatype { name, constructors } => self.traverse_datatype(name, constructors),
            Val { rec, pattern, expr } => self.traverse_val(rec, pattern, expr),
            Fun { name, params, expr } => self.traverse_fun(name, params, expr),
        }
    }

    fn traverse_datatype(&mut self, _name: &mut Symbol, _constructors: &mut Vec<Symbol>) {}

    fn traverse_val(&mut self, _rec: &mut bool, pattern: &mut Pattern<Ty>, expr: &mut Expr<Ty>) {
        self.traverse_expr(expr);
        self.traverse_pattern(pattern)
    }

    fn traverse_fun(
        &mut self,
        _name: &mut Symbol,
        params: &mut Vec<Pattern<Ty>>,
        expr: &mut Expr<Ty>,
    ) {
        for param in params {
            self.traverse_pattern(param);
        }
        self.traverse_expr(expr);
    }

    fn traverse_expr(&mut self, expr: &mut Expr<Ty>) {
        use crate::ast::Expr::*;
        match expr {
            Binds { ty, binds, ret } => self.traverse_binds(ty, binds, ret),
            BinOp { ty, op, l, r } => self.traverse_binop(ty, op, l, r),
            Fn { ty, param, body } => self.traverse_fn(ty, param, body),
            App { ty, fun, arg } => self.traverse_app(ty, fun, arg),
            If {
                ty,
                cond,
                then,
                else_,
            } => self.traverse_if(ty, cond, then, else_),
            Case { ty, cond, clauses } => self.traverse_case(ty, cond, clauses),
            Tuple { ty, tuple } => self.traverse_tuple(ty, tuple),
            Constructor { ty, name } => self.traverse_constructor(ty, name),
            Symbol { ty, name } => self.traverse_sym(ty, name),
            Literal { ty, value } => self.traverse_lit(ty, value),
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
        for (p, e) in clauses.iter_mut() {
            self.traverse_pattern(p);
            self.traverse_expr(e);
        }
    }

    fn traverse_tuple(&mut self, _ty: &mut Ty, tuple: &mut Vec<Expr<Ty>>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_constructor(&mut self, _ty: &mut Ty, _name: &mut Symbol) {}
    fn traverse_sym(&mut self, _ty: &mut Ty, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _ty: &mut Ty, _value: &mut Literal) {}

    fn traverse_pattern(&mut self, _pattern: &mut Pattern<Ty>) {}
}

pub trait Transform<Ty> {
    fn transform_ast(&mut self, ast: AST<Ty>) -> AST<Ty> {
        AST(ast
            .0
            .into_iter()
            .map(|stmt| self.transform_statement(stmt))
            .collect())
    }

    fn transform_statement(&mut self, stmt: Statement<Ty>) -> Statement<Ty> {
        use Statement::*;
        match stmt {
            Datatype { name, constructors } => self.transform_datatype(name, constructors),
            Val { rec, pattern, expr } => self.transform_val(rec, pattern, expr),
            Fun { name, params, expr } => self.transform_fun(name, params, expr),
        }
    }

    fn transform_datatype(&mut self, name: Symbol, constructors: Vec<Symbol>) -> Statement<Ty> {
        Statement::Datatype { name, constructors }
    }

    fn transform_val(&mut self, rec: bool, pattern: Pattern<Ty>, expr: Expr<Ty>) -> Statement<Ty> {
        Statement::Val {
            rec,
            pattern: self.transform_pattern(pattern),
            expr: self.transform_expr(expr),
        }
    }

    fn transform_fun(
        &mut self,
        name: Symbol,
        params: Vec<Pattern<Ty>>,
        expr: Expr<Ty>,
    ) -> Statement<Ty> {
        Statement::Fun {
            name,
            params: params
                .into_iter()
                .map(|param| self.transform_pattern(param))
                .collect(),
            expr: self.transform_expr(expr),
        }
    }

    fn transform_expr(&mut self, expr: Expr<Ty>) -> Expr<Ty> {
        use crate::ast::Expr::*;
        match expr {
            Binds { ty, binds, ret } => self.transform_binds(ty, binds, ret),
            BinOp { ty, op, l, r } => self.transform_binop(ty, op, l, r),
            Fn { ty, param, body } => self.transform_fn(ty, param, body),
            App { ty, fun, arg } => self.transform_app(ty, fun, arg),
            If {
                ty,
                cond,
                then,
                else_,
            } => self.transform_if(ty, cond, then, else_),
            Case { ty, cond, clauses } => self.transform_case(ty, cond, clauses),
            Tuple { ty, tuple } => self.transform_tuple(ty, tuple),
            Constructor { ty, name } => self.transform_constructor(ty, name),
            Symbol { ty, name } => self.transform_symbol(ty, name),
            Literal { ty, value } => self.transform_literal(ty, value),
        }
    }
    fn transform_binds(
        &mut self,
        ty: Ty,
        binds: Vec<Statement<Ty>>,
        ret: Box<Expr<Ty>>,
    ) -> Expr<Ty> {
        Expr::Binds {
            ty,
            binds: binds
                .into_iter()
                .map(|stmt| self.transform_statement(stmt))
                .collect(),
            ret: self.transform_expr(*ret).boxed(),
        }
    }

    fn transform_binop(
        &mut self,
        ty: Ty,
        op: Symbol,
        l: Box<Expr<Ty>>,
        r: Box<Expr<Ty>>,
    ) -> Expr<Ty> {
        Expr::BinOp {
            ty,
            op,
            l: self.transform_expr(*l).boxed(),
            r: self.transform_expr(*r).boxed(),
        }
    }

    fn transform_fn(&mut self, ty: Ty, param: Symbol, body: Box<Expr<Ty>>) -> Expr<Ty> {
        Expr::Fn {
            ty,
            param,
            body: self.transform_expr(*body).boxed(),
        }
    }

    fn transform_app(&mut self, ty: Ty, fun: Box<Expr<Ty>>, arg: Box<Expr<Ty>>) -> Expr<Ty> {
        Expr::App {
            ty,
            fun: self.transform_expr(*fun).boxed(),
            arg: self.transform_expr(*arg).boxed(),
        }
    }

    fn transform_if(
        &mut self,
        ty: Ty,
        cond: Box<Expr<Ty>>,
        then: Box<Expr<Ty>>,
        else_: Box<Expr<Ty>>,
    ) -> Expr<Ty> {
        Expr::If {
            ty,
            cond: self.transform_expr(*cond).boxed(),
            then: self.transform_expr(*then).boxed(),
            else_: self.transform_expr(*else_).boxed(),
        }
    }

    fn transform_case(
        &mut self,
        ty: Ty,
        cond: Box<Expr<Ty>>,
        clauses: Vec<(Pattern<Ty>, Expr<Ty>)>,
    ) -> Expr<Ty> {
        Expr::Case {
            ty,
            cond: self.transform_expr(*cond).boxed(),
            clauses: clauses
                .into_iter()
                .map(|(p, e)| (self.transform_pattern(p), self.transform_expr(e)))
                .collect(),
        }
    }

    fn transform_tuple(&mut self, ty: Ty, tuple: Vec<Expr<Ty>>) -> Expr<Ty> {
        Expr::Tuple {
            ty,
            tuple: tuple.into_iter().map(|t| self.transform_expr(t)).collect(),
        }
    }

    fn transform_constructor(&mut self, ty: Ty, name: Symbol) -> Expr<Ty> {
        Expr::Constructor { ty, name }
    }
    fn transform_symbol(&mut self, ty: Ty, name: Symbol) -> Expr<Ty> {
        Expr::Symbol { ty, name }
    }

    fn transform_literal(&mut self, ty: Ty, value: Literal) -> Expr<Ty> {
        Expr::Literal { ty, value }
    }

    fn transform_pattern(&mut self, pattern: Pattern<Ty>) -> Pattern<Ty> {
        use Pattern::*;
        match pattern {
            Constant { ty, value } => self.transform_pat_constant(ty, value),
            Constructor { ty, name } => self.transform_pat_constructor(ty, name),
            Tuple { ty, tuple } => self.transform_pat_tuple(ty, tuple),
            Variable { ty, name } => self.transform_pat_variable(ty, name),
            Wildcard { ty } => self.transform_pat_wildcard(ty),
        }
    }

    fn transform_pat_constant(&mut self, ty: Ty, value: i64) -> Pattern<Ty> {
        Pattern::Constant { value, ty }
    }

    fn transform_pat_constructor(&mut self, ty: Ty, name: Symbol) -> Pattern<Ty> {
        Pattern::Constructor { name, ty }
    }

    fn transform_pat_tuple(&mut self, ty: Ty, tuple: Vec<(Ty, Symbol)>) -> Pattern<Ty> {
        Pattern::Tuple { ty, tuple }
    }

    fn transform_pat_variable(&mut self, ty: Ty, name: Symbol) -> Pattern<Ty> {
        Pattern::Variable { name, ty }
    }

    fn transform_pat_wildcard(&mut self, ty: Ty) -> Pattern<Ty> {
        Pattern::Wildcard { ty }
    }
}
