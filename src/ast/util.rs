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
            D(_) => (),
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

    fn traverse_expr(&mut self, expr: &mut CoreExpr<Ty>) {
        use crate::ast::ExprKind::*;
        match &mut expr.inner {
            Binds { binds, ret } => self.traverse_binds(binds, ret),
            BuiltinCall { fun, args } => self.traverse_builtincall(fun, args),
            Fn { param, body } => self.traverse_fn(param, body),
            App { fun, arg } => self.traverse_app(fun, arg),
            Case { cond, clauses } => self.traverse_case(cond, clauses),
            Tuple { tuple } => self.traverse_tuple(tuple),
            Constructor { arg, name } => self.traverse_constructor(arg, name),
            Symbol { name } => self.traverse_sym(name),
            Literal { value } => self.traverse_lit(value),
            D(_) => (),
        }
    }
    fn traverse_binds(&mut self, binds: &mut Vec<CoreStatement<Ty>>, ret: &mut Box<CoreExpr<Ty>>) {
        for stmt in binds.iter_mut() {
            self.traverse_statement(stmt)
        }
        self.traverse_expr(ret)
    }

    fn traverse_builtincall(&mut self, _: &mut BIF, args: &mut Vec<CoreExpr<Ty>>) {
        for arg in args {
            self.traverse_expr(arg)
        }
    }

    fn traverse_fn(&mut self, _param: &mut Symbol, body: &mut Box<CoreExpr<Ty>>) {
        self.traverse_expr(body)
    }

    fn traverse_app(&mut self, fun: &mut Box<CoreExpr<Ty>>, arg: &mut Box<CoreExpr<Ty>>) {
        self.traverse_expr(fun);
        self.traverse_expr(arg);
    }

    fn traverse_case(
        &mut self,
        cond: &mut Box<CoreExpr<Ty>>,
        clauses: &mut Vec<(Pattern<Ty>, CoreExpr<Ty>)>,
    ) {
        self.traverse_expr(cond);
        for (p, e) in clauses.iter_mut() {
            self.traverse_pattern(p);
            self.traverse_expr(e);
        }
    }

    fn traverse_tuple(&mut self, tuple: &mut Vec<CoreExpr<Ty>>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_constructor(&mut self, arg: &mut Option<Box<CoreExpr<Ty>>>, _name: &mut Symbol) {
        if let Some(arg) = arg {
            self.traverse_expr(arg)
        }
    }
    fn traverse_sym(&mut self, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _value: &mut Literal) {}

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
            D(d) => match d {},
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

    fn transform_expr(&mut self, mut expr: CoreExpr<Ty>) -> CoreExpr<Ty> {
        use crate::ast::ExprKind::*;
        expr.inner = match expr.inner {
            Binds { binds, ret } => self.transform_binds(binds, ret),
            BuiltinCall { fun, args } => self.transform_builtincall(fun, args),
            Fn { param, body } => self.transform_fn(param, body),
            App { fun, arg } => self.transform_app(fun, arg),
            Case { cond, clauses } => self.transform_case(cond, clauses),
            Tuple { tuple } => self.transform_tuple(tuple),
            Constructor { arg, name } => self.transform_constructor(arg, name),
            Symbol { name } => self.transform_symbol(name),
            Literal { value } => self.transform_literal(value),
            D(d) => match d {},
        };
        expr
    }
    fn transform_binds(
        &mut self,
        binds: Vec<CoreStatement<Ty>>,
        ret: Box<CoreExpr<Ty>>,
    ) -> CoreExprKind<Ty> {
        ExprKind::Binds {
            binds: binds
                .into_iter()
                .map(|stmt| self.transform_statement(stmt))
                .collect(),
            ret: self.transform_expr(*ret).boxed(),
        }
    }

    fn transform_builtincall(&mut self, fun: BIF, args: Vec<CoreExpr<Ty>>) -> CoreExprKind<Ty> {
        ExprKind::BuiltinCall {
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
        }
    }

    fn transform_fn(&mut self, param: Symbol, body: Box<CoreExpr<Ty>>) -> CoreExprKind<Ty> {
        ExprKind::Fn {
            param,
            body: self.transform_expr(*body).boxed(),
        }
    }

    fn transform_app(
        &mut self,
        fun: Box<CoreExpr<Ty>>,
        arg: Box<CoreExpr<Ty>>,
    ) -> CoreExprKind<Ty> {
        ExprKind::App {
            fun: self.transform_expr(*fun).boxed(),
            arg: self.transform_expr(*arg).boxed(),
        }
    }

    fn transform_case(
        &mut self,
        cond: Box<CoreExpr<Ty>>,
        clauses: Vec<(Pattern<Ty>, CoreExpr<Ty>)>,
    ) -> CoreExprKind<Ty> {
        ExprKind::Case {
            cond: self.transform_expr(*cond).boxed(),
            clauses: clauses
                .into_iter()
                .map(|(p, e)| (self.transform_pattern(p), self.transform_expr(e)))
                .collect(),
        }
    }

    fn transform_tuple(&mut self, tuple: Vec<CoreExpr<Ty>>) -> CoreExprKind<Ty> {
        ExprKind::Tuple {
            tuple: tuple.into_iter().map(|t| self.transform_expr(t)).collect(),
        }
    }

    fn transform_constructor(
        &mut self,
        arg: Option<Box<CoreExpr<Ty>>>,
        name: Symbol,
    ) -> CoreExprKind<Ty> {
        ExprKind::Constructor {
            arg: arg.map(|e| self.transform_expr(*e).boxed()),
            name,
        }
    }
    fn transform_symbol(&mut self, name: Symbol) -> CoreExprKind<Ty> {
        ExprKind::Symbol { name }
    }

    fn transform_literal(&mut self, value: Literal) -> CoreExprKind<Ty> {
        ExprKind::Literal { value }
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
        Pattern::Constructor {
            name,
            arg: arg.map(|pat| Box::new(self.transform_pattern(*pat))),
            ty,
        }
    }

    fn transform_pat_tuple(&mut self, ty: Ty, tuple: Vec<Pattern<Ty>>) -> Pattern<Ty> {
        Pattern::Tuple {
            ty,
            tuple: tuple
                .into_iter()
                .map(|pat| self.transform_pattern(pat))
                .collect(),
        }
    }

    fn transform_pat_variable(&mut self, ty: Ty, name: Symbol) -> Pattern<Ty> {
        Pattern::Variable { name, ty }
    }

    fn transform_pat_wildcard(&mut self, ty: Ty) -> Pattern<Ty> {
        Pattern::Wildcard { ty }
    }
}
