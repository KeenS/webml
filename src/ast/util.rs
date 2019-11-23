use crate::ast::*;

pub trait Traverse<Ty> {
    fn traverse_ast(&mut self, ast: &mut Core<Ty>) {
        for stmt in ast.0.iter_mut() {
            self.traverse_statement(stmt)
        }
    }

    fn traverse_statement(&mut self, stmt: &mut CoreStatement<Ty>) {
        use Statement::*;
        match stmt {
            Datatype { name, constructors } => self.traverse_datatype(name, constructors),
            Val { rec, pattern, expr } => self.traverse_val(rec, pattern, expr),
            Fun { name, params, expr } => self.traverse_fun(name, params, expr),
        }
    }

    fn traverse_datatype(
        &mut self,
        _name: &mut Symbol,
        _constructors: &mut Vec<(Symbol, Option<Type>)>,
    ) {
    }

    fn traverse_val(
        &mut self,
        _rec: &mut bool,
        pattern: &mut Pattern<Ty>,
        expr: &mut CoreExpr<Ty>,
    ) {
        self.traverse_expr(expr);
        self.traverse_pattern(pattern)
    }

    fn traverse_fun(
        &mut self,
        _name: &mut Symbol,
        params: &mut Vec<Pattern<Ty>>,
        expr: &mut CoreExpr<Ty>,
    ) {
        for param in params {
            self.traverse_pattern(param);
        }
        self.traverse_expr(expr);
    }

    fn traverse_expr(&mut self, expr: &mut CoreExpr<Ty>) {
        use crate::ast::Expr::*;
        match expr {
            Binds { ty, binds, ret } => self.traverse_binds(ty, binds, ret),
            BinOp { ty, op, l, r } => self.traverse_binop(ty, op, l, r),
            Fn { ty, param, body } => self.traverse_fn(ty, param, body),
            App { ty, fun, arg } => self.traverse_app(ty, fun, arg),
            Case { ty, cond, clauses } => self.traverse_case(ty, cond, clauses),
            Tuple { ty, tuple } => self.traverse_tuple(ty, tuple),
            Constructor { ty, arg, name } => self.traverse_constructor(ty, arg, name),
            Symbol { ty, name } => self.traverse_sym(ty, name),
            Literal { ty, value } => self.traverse_lit(ty, value),
            D(_) => (),
        }
    }
    fn traverse_binds(
        &mut self,
        _ty: &mut Ty,
        binds: &mut Vec<CoreStatement<Ty>>,
        ret: &mut Box<CoreExpr<Ty>>,
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
        l: &mut Box<CoreExpr<Ty>>,
        r: &mut Box<CoreExpr<Ty>>,
    ) {
        self.traverse_expr(l);
        self.traverse_expr(r)
    }

    fn traverse_fn(&mut self, _ty: &mut Ty, _param: &mut Symbol, body: &mut Box<CoreExpr<Ty>>) {
        self.traverse_expr(body)
    }

    fn traverse_app(
        &mut self,
        _ty: &mut Ty,
        fun: &mut Box<CoreExpr<Ty>>,
        arg: &mut Box<CoreExpr<Ty>>,
    ) {
        self.traverse_expr(fun);
        self.traverse_expr(arg);
    }

    fn traverse_if(
        &mut self,
        _ty: &mut Ty,
        cond: &mut Box<CoreExpr<Ty>>,
        then: &mut Box<CoreExpr<Ty>>,
        else_: &mut Box<CoreExpr<Ty>>,
    ) {
        self.traverse_expr(cond);
        self.traverse_expr(then);
        self.traverse_expr(else_);
    }

    fn traverse_case(
        &mut self,
        _ty: &mut Ty,
        cond: &mut Box<CoreExpr<Ty>>,
        clauses: &mut Vec<(Pattern<Ty>, CoreExpr<Ty>)>,
    ) {
        self.traverse_expr(cond);
        for (p, e) in clauses.iter_mut() {
            self.traverse_pattern(p);
            self.traverse_expr(e);
        }
    }

    fn traverse_tuple(&mut self, _ty: &mut Ty, tuple: &mut Vec<CoreExpr<Ty>>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_constructor(
        &mut self,
        _ty: &mut Ty,
        arg: &mut Option<Box<CoreExpr<Ty>>>,
        _name: &mut Symbol,
    ) {
        if let Some(arg) = arg {
            self.traverse_expr(arg)
        }
    }
    fn traverse_sym(&mut self, _ty: &mut Ty, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _ty: &mut Ty, _value: &mut Literal) {}

    fn traverse_pattern(&mut self, pattern: &mut Pattern<Ty>) {
        use Pattern::*;
        match pattern {
            Constant { ty, value } => self.traverse_pat_constant(ty, value),
            Constructor { ty, name, arg } => self.traverse_pat_constructor(ty, name, arg),
            Tuple { ty, tuple } => self.traverse_pat_tuple(ty, tuple),
            Variable { ty, name } => self.traverse_pat_variable(ty, name),
            Wildcard { ty } => self.traverse_pat_wildcard(ty),
        }
    }

    fn traverse_pat_constant(&mut self, _ty: &mut Ty, _value: &mut i64) {}
    fn traverse_pat_constructor(
        &mut self,
        _ty: &mut Ty,
        _name: &mut Symbol,
        _arg: &mut Option<Box<Pattern<Ty>>>,
    ) {
    }
    fn traverse_pat_tuple(&mut self, _ty: &mut Ty, _tuple: &mut Vec<Pattern<Ty>>) {}
    fn traverse_pat_variable(&mut self, _ty: &mut Ty, _value: &mut Symbol) {}
    fn traverse_pat_wildcard(&mut self, _ty: &mut Ty) {}
}

pub trait Transform<Ty> {
    fn transform_ast(&mut self, ast: Core<Ty>) -> Core<Ty> {
        AST(ast
            .0
            .into_iter()
            .map(|stmt| self.transform_statement(stmt))
            .collect())
    }

    fn transform_statement(&mut self, stmt: CoreStatement<Ty>) -> CoreStatement<Ty> {
        use Statement::*;
        match stmt {
            Datatype { name, constructors } => self.transform_datatype(name, constructors),
            Val { rec, pattern, expr } => self.transform_val(rec, pattern, expr),
            Fun { name, params, expr } => self.transform_fun(name, params, expr),
        }
    }

    fn transform_datatype(
        &mut self,
        name: Symbol,
        constructors: Vec<(Symbol, Option<Type>)>,
    ) -> CoreStatement<Ty> {
        Statement::Datatype { name, constructors }
    }

    fn transform_val(
        &mut self,
        rec: bool,
        pattern: Pattern<Ty>,
        expr: CoreExpr<Ty>,
    ) -> CoreStatement<Ty> {
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
        expr: CoreExpr<Ty>,
    ) -> CoreStatement<Ty> {
        Statement::Fun {
            name,
            params: params
                .into_iter()
                .map(|param| self.transform_pattern(param))
                .collect(),
            expr: self.transform_expr(expr),
        }
    }

    fn transform_expr(&mut self, expr: CoreExpr<Ty>) -> CoreExpr<Ty> {
        use crate::ast::Expr::*;
        match expr {
            Binds { ty, binds, ret } => self.transform_binds(ty, binds, ret),
            BinOp { ty, op, l, r } => self.transform_binop(ty, op, l, r),
            Fn { ty, param, body } => self.transform_fn(ty, param, body),
            App { ty, fun, arg } => self.transform_app(ty, fun, arg),
            Case { ty, cond, clauses } => self.transform_case(ty, cond, clauses),
            Tuple { ty, tuple } => self.transform_tuple(ty, tuple),
            Constructor { ty, arg, name } => self.transform_constructor(ty, arg, name),
            Symbol { ty, name } => self.transform_symbol(ty, name),
            Literal { ty, value } => self.transform_literal(ty, value),
            D(d) => match d {},
        }
    }
    fn transform_binds(
        &mut self,
        ty: Ty,
        binds: Vec<CoreStatement<Ty>>,
        ret: Box<CoreExpr<Ty>>,
    ) -> CoreExpr<Ty> {
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
        l: Box<CoreExpr<Ty>>,
        r: Box<CoreExpr<Ty>>,
    ) -> CoreExpr<Ty> {
        Expr::BinOp {
            ty,
            op,
            l: self.transform_expr(*l).boxed(),
            r: self.transform_expr(*r).boxed(),
        }
    }

    fn transform_fn(&mut self, ty: Ty, param: Symbol, body: Box<CoreExpr<Ty>>) -> CoreExpr<Ty> {
        Expr::Fn {
            ty,
            param,
            body: self.transform_expr(*body).boxed(),
        }
    }

    fn transform_app(
        &mut self,
        ty: Ty,
        fun: Box<CoreExpr<Ty>>,
        arg: Box<CoreExpr<Ty>>,
    ) -> CoreExpr<Ty> {
        Expr::App {
            ty,
            fun: self.transform_expr(*fun).boxed(),
            arg: self.transform_expr(*arg).boxed(),
        }
    }

    fn transform_case(
        &mut self,
        ty: Ty,
        cond: Box<CoreExpr<Ty>>,
        clauses: Vec<(Pattern<Ty>, CoreExpr<Ty>)>,
    ) -> CoreExpr<Ty> {
        Expr::Case {
            ty,
            cond: self.transform_expr(*cond).boxed(),
            clauses: clauses
                .into_iter()
                .map(|(p, e)| (self.transform_pattern(p), self.transform_expr(e)))
                .collect(),
        }
    }

    fn transform_tuple(&mut self, ty: Ty, tuple: Vec<CoreExpr<Ty>>) -> CoreExpr<Ty> {
        Expr::Tuple {
            ty,
            tuple: tuple.into_iter().map(|t| self.transform_expr(t)).collect(),
        }
    }

    fn transform_constructor(
        &mut self,
        ty: Ty,
        arg: Option<Box<CoreExpr<Ty>>>,
        name: Symbol,
    ) -> CoreExpr<Ty> {
        Expr::Constructor {
            ty,
            arg: arg.map(|e| self.transform_expr(*e).boxed()),
            name,
        }
    }
    fn transform_symbol(&mut self, ty: Ty, name: Symbol) -> CoreExpr<Ty> {
        Expr::Symbol { ty, name }
    }

    fn transform_literal(&mut self, ty: Ty, value: Literal) -> CoreExpr<Ty> {
        Expr::Literal { ty, value }
    }

    fn transform_pattern(&mut self, pattern: Pattern<Ty>) -> Pattern<Ty> {
        use Pattern::*;
        match pattern {
            Constant { ty, value } => self.transform_pat_constant(ty, value),
            Constructor { ty, arg, name } => self.transform_pat_constructor(ty, arg, name),
            Tuple { ty, tuple } => self.transform_pat_tuple(ty, tuple),
            Variable { ty, name } => self.transform_pat_variable(ty, name),
            Wildcard { ty } => self.transform_pat_wildcard(ty),
        }
    }

    fn transform_pat_constant(&mut self, ty: Ty, value: i64) -> Pattern<Ty> {
        Pattern::Constant { value, ty }
    }

    fn transform_pat_constructor(
        &mut self,
        ty: Ty,
        arg: Option<Box<Pattern<Ty>>>,
        name: Symbol,
    ) -> Pattern<Ty> {
        Pattern::Constructor { name, arg, ty }
    }

    fn transform_pat_tuple(&mut self, ty: Ty, tuple: Vec<Pattern<Ty>>) -> Pattern<Ty> {
        Pattern::Tuple { ty, tuple }
    }

    fn transform_pat_variable(&mut self, ty: Ty, name: Symbol) -> Pattern<Ty> {
        Pattern::Variable { name, ty }
    }

    fn transform_pat_wildcard(&mut self, ty: Ty) -> Pattern<Ty> {
        Pattern::Wildcard { ty }
    }
}
