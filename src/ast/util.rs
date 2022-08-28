use crate::ast::*;

pub trait Traverse<Ty> {
    fn traverse_ast(&mut self, ast: &mut Core<Ty>) {
        for decl in ast.0.iter_mut() {
            self.traverse_statement(decl)
        }
    }

    fn traverse_statement(&mut self, decl: &mut CoreDeclaration<Ty>) {
        use Declaration::*;
        match decl {
            Datatype { name, constructors } => self.traverse_datatype(name, constructors),
            Val { rec, pattern, expr } => self.traverse_val(rec, pattern, expr),
            LangItem { decl, name } => self.traverse_langitem(name, decl),
            Local { binds, body } => self.traverse_local(binds, body),
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
        pattern: &mut CorePattern<Ty>,
        expr: &mut CoreExpr<Ty>,
    ) {
        self.traverse_expr(expr);
        self.traverse_pattern(pattern)
    }

    fn traverse_langitem(&mut self, _name: &mut LangItem, decl: &mut CoreDeclaration<Ty>) {
        self.traverse_statement(decl);
    }

    fn traverse_local(
        &mut self,
        binds: &mut Vec<CoreDeclaration<Ty>>,
        body: &mut Vec<CoreDeclaration<Ty>>,
    ) {
        for decl in binds.iter_mut() {
            self.traverse_statement(decl)
        }
        for decl in body.iter_mut() {
            self.traverse_statement(decl)
        }
    }

    fn traverse_expr(&mut self, expr: &mut CoreExpr<Ty>) {
        use crate::ast::ExprKind::*;
        let span = expr.span.clone();
        match &mut expr.inner {
            Binds { binds, ret } => self.traverse_binds(span, binds, ret),
            BuiltinCall { fun, args } => self.traverse_builtincall(span, fun, args),
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => self.traverse_externcall(span, module, fun, args, argty, retty),
            Fn { param, body } => self.traverse_fn(span, param, body),
            App { fun, arg } => self.traverse_app(span, fun, arg),
            TyApp { fun, arg } => self.traverse_tyapp(span, fun, arg),
            Case { cond, clauses } => self.traverse_case(span, cond, clauses),
            Tuple { tuple } => self.traverse_tuple(span, tuple),
            Constructor { arg, name } => self.traverse_constructor(span, arg, name),
            Symbol { name } => self.traverse_sym(span, name),
            Literal { value } => self.traverse_lit(span, value),
            D(_) => (),
        }
    }
    fn traverse_binds(
        &mut self,
        _: Span,
        binds: &mut Vec<CoreDeclaration<Ty>>,
        ret: &mut Box<CoreExpr<Ty>>,
    ) {
        for decl in binds.iter_mut() {
            self.traverse_statement(decl)
        }
        self.traverse_expr(ret)
    }

    fn traverse_builtincall(&mut self, _: Span, _: &mut BIF, args: &mut Vec<CoreExpr<Ty>>) {
        for arg in args {
            self.traverse_expr(arg)
        }
    }

    fn traverse_externcall(
        &mut self,
        _: Span,
        _module: &mut String,
        _fun: &mut String,
        args: &mut Vec<CoreExpr<Ty>>,
        _argty: &mut Vec<Type>,
        _retty: &mut Type,
    ) {
        for arg in args {
            self.traverse_expr(arg)
        }
    }

    fn traverse_fn(&mut self, _: Span, _param: &mut Symbol, body: &mut Box<CoreExpr<Ty>>) {
        self.traverse_expr(body)
    }

    fn traverse_app(&mut self, _: Span, fun: &mut Box<CoreExpr<Ty>>, arg: &mut Box<CoreExpr<Ty>>) {
        self.traverse_expr(fun);
        self.traverse_expr(arg);
    }

    fn traverse_tyapp(&mut self, _: Span, _: &mut Symbol, _: &mut Vec<Ty>) {}

    fn traverse_case(
        &mut self,
        _: Span,
        cond: &mut Box<CoreExpr<Ty>>,
        clauses: &mut Vec<(CorePattern<Ty>, CoreExpr<Ty>)>,
    ) {
        self.traverse_expr(cond);
        for (p, e) in clauses.iter_mut() {
            self.traverse_pattern(p);
            self.traverse_expr(e);
        }
    }

    fn traverse_tuple(&mut self, _: Span, tuple: &mut Vec<CoreExpr<Ty>>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_constructor(
        &mut self,
        _: Span,
        arg: &mut Option<Box<CoreExpr<Ty>>>,
        _name: &mut Symbol,
    ) {
        if let Some(arg) = arg {
            self.traverse_expr(arg)
        }
    }
    fn traverse_sym(&mut self, _: Span, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _: Span, _value: &mut Literal) {}
    fn traverse_string(&mut self, _: Span, _value: &mut Vec<u32>) {}

    fn traverse_pattern(&mut self, pattern: &mut CorePattern<Ty>) {
        use PatternKind::*;
        let span = pattern.span.clone();
        match &mut pattern.inner {
            Constant { value } => self.traverse_pat_constant(span, value),
            Char { value } => self.traverse_pat_char(span, value),
            Constructor { name, arg } => self.traverse_pat_constructor(span, name, arg),
            Tuple { tuple } => self.traverse_pat_tuple(span, tuple),
            Variable { name } => self.traverse_pat_variable(span, name),
            Wildcard {} => self.traverse_pat_wildcard(span),
            D(d) => match *d {},
        }
    }

    fn traverse_pat_constant(&mut self, _: Span, _value: &mut i64) {}
    fn traverse_pat_char(&mut self, _: Span, _value: &mut u32) {}
    fn traverse_pat_constructor(
        &mut self,
        _: Span,
        _name: &mut Symbol,
        _arg: &mut Option<Box<CorePattern<Ty>>>,
    ) {
    }
    fn traverse_pat_tuple(&mut self, _: Span, _tuple: &mut Vec<CorePattern<Ty>>) {}
    fn traverse_pat_variable(&mut self, _: Span, _value: &mut Symbol) {}
    fn traverse_pat_wildcard(&mut self, _: Span) {}
}

pub trait Transform<Ty> {
    fn transform_ast(&mut self, ast: Core<Ty>) -> Core<Ty> {
        AST(ast
            .0
            .into_iter()
            .map(|decl| self.transform_statement(decl))
            .collect())
    }

    fn transform_statement(&mut self, decl: CoreDeclaration<Ty>) -> CoreDeclaration<Ty> {
        use Declaration::*;
        match decl {
            Datatype { name, constructors } => self.transform_datatype(name, constructors),
            Val { rec, pattern, expr } => self.transform_val(rec, pattern, expr),
            LangItem { name, decl } => self.transform_langitem(name, *decl),
            Local { binds, body } => self.transform_local(binds, body),
            D(d) => match d {},
        }
    }

    fn transform_datatype(
        &mut self,
        name: Symbol,
        constructors: Vec<(Symbol, Option<Type>)>,
    ) -> CoreDeclaration<Ty> {
        Declaration::Datatype { name, constructors }
    }

    fn transform_val(
        &mut self,
        rec: bool,
        pattern: CorePattern<Ty>,
        expr: CoreExpr<Ty>,
    ) -> CoreDeclaration<Ty> {
        Declaration::Val {
            rec,
            pattern: self.transform_pattern(pattern),
            expr: self.transform_expr(expr),
        }
    }

    fn transform_langitem(
        &mut self,
        name: LangItem,
        decl: CoreDeclaration<Ty>,
    ) -> CoreDeclaration<Ty> {
        Declaration::LangItem {
            name,
            decl: Box::new(self.transform_statement(decl)),
        }
    }

    fn transform_local(
        &mut self,
        binds: Vec<CoreDeclaration<Ty>>,
        body: Vec<CoreDeclaration<Ty>>,
    ) -> CoreDeclaration<Ty> {
        let binds = binds
            .into_iter()
            .map(|b| self.transform_statement(b))
            .collect();
        let body = body
            .into_iter()
            .map(|b| self.transform_statement(b))
            .collect();
        CoreDeclaration::Local { binds, body }
    }
    fn transform_expr(&mut self, mut expr: CoreExpr<Ty>) -> CoreExpr<Ty> {
        use crate::ast::ExprKind::*;
        let span = expr.span.clone();
        expr.inner = match expr.inner {
            Binds { binds, ret } => self.transform_binds(span, binds, ret),
            BuiltinCall { fun, args } => self.transform_builtincall(span, fun, args),
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => self.transform_externcall(span, module, fun, args, argty, retty),
            Fn { param, body } => self.transform_fn(span, param, body),
            App { fun, arg } => self.transform_app(span, fun, arg),
            TyApp { fun, arg } => self.transform_tyapp(span, fun, arg),
            Case { cond, clauses } => self.transform_case(span, cond, clauses),
            Tuple { tuple } => self.transform_tuple(span, tuple),
            Constructor { arg, name } => self.transform_constructor(span, arg, name),
            Symbol { name } => self.transform_symbol(span, name),
            Literal { value } => self.transform_literal(span, value),
            D(d) => match d {},
        };
        expr
    }
    fn transform_binds(
        &mut self,
        _: Span,
        binds: Vec<CoreDeclaration<Ty>>,
        ret: Box<CoreExpr<Ty>>,
    ) -> CoreExprKind<Ty> {
        ExprKind::Binds {
            binds: binds
                .into_iter()
                .map(|decl| self.transform_statement(decl))
                .collect(),
            ret: self.transform_expr(*ret).boxed(),
        }
    }

    fn transform_builtincall(
        &mut self,
        _: Span,
        fun: BIF,
        args: Vec<CoreExpr<Ty>>,
    ) -> CoreExprKind<Ty> {
        ExprKind::BuiltinCall {
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
        }
    }

    fn transform_externcall(
        &mut self,
        _: Span,
        module: String,
        fun: String,
        args: Vec<CoreExpr<Ty>>,
        argty: Vec<Type>,
        retty: Type,
    ) -> CoreExprKind<Ty> {
        ExprKind::ExternCall {
            module,
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
            argty,
            retty,
        }
    }

    fn transform_fn(
        &mut self,
        _: Span,
        param: Symbol,
        body: Box<CoreExpr<Ty>>,
    ) -> CoreExprKind<Ty> {
        ExprKind::Fn {
            param,
            body: self.transform_expr(*body).boxed(),
        }
    }

    fn transform_app(
        &mut self,
        _: Span,
        fun: Box<CoreExpr<Ty>>,
        arg: Box<CoreExpr<Ty>>,
    ) -> CoreExprKind<Ty> {
        ExprKind::App {
            fun: self.transform_expr(*fun).boxed(),
            arg: self.transform_expr(*arg).boxed(),
        }
    }

    fn transform_tyapp(&mut self, _: Span, fun: Symbol, arg: Vec<Ty>) -> CoreExprKind<Ty> {
        ExprKind::TyApp { fun, arg }
    }

    fn transform_case(
        &mut self,
        _: Span,
        cond: Box<CoreExpr<Ty>>,
        clauses: Vec<(CorePattern<Ty>, CoreExpr<Ty>)>,
    ) -> CoreExprKind<Ty> {
        ExprKind::Case {
            cond: self.transform_expr(*cond).boxed(),
            clauses: clauses
                .into_iter()
                .map(|(p, e)| (self.transform_pattern(p), self.transform_expr(e)))
                .collect(),
        }
    }

    fn transform_tuple(&mut self, _: Span, tuple: Vec<CoreExpr<Ty>>) -> CoreExprKind<Ty> {
        ExprKind::Tuple {
            tuple: tuple.into_iter().map(|t| self.transform_expr(t)).collect(),
        }
    }

    fn transform_constructor(
        &mut self,
        _: Span,
        arg: Option<Box<CoreExpr<Ty>>>,
        name: Symbol,
    ) -> CoreExprKind<Ty> {
        ExprKind::Constructor {
            arg: arg.map(|e| self.transform_expr(*e).boxed()),
            name,
        }
    }
    fn transform_symbol(&mut self, _: Span, name: Symbol) -> CoreExprKind<Ty> {
        ExprKind::Symbol { name }
    }

    fn transform_literal(&mut self, _: Span, value: Literal) -> CoreExprKind<Ty> {
        ExprKind::Literal { value }
    }

    fn transform_pattern(&mut self, mut pattern: CorePattern<Ty>) -> CorePattern<Ty> {
        use PatternKind::*;
        let span = pattern.span.clone();
        pattern.inner = match pattern.inner {
            Constant { value } => self.transform_pat_constant(span, value),
            Char { value } => self.transform_pat_char(span, value),
            Constructor { arg, name } => self.transform_pat_constructor(span, arg, name),
            Tuple { tuple } => self.transform_pat_tuple(span, tuple),
            Variable { name } => self.transform_pat_variable(span, name),
            Wildcard {} => self.transform_pat_wildcard(span),
            D(d) => match d {},
        };
        pattern
    }

    fn transform_pat_constant(&mut self, _: Span, value: i64) -> CorePatternKind<Ty> {
        CorePatternKind::Constant { value }
    }

    fn transform_pat_char(&mut self, _: Span, value: u32) -> CorePatternKind<Ty> {
        CorePatternKind::Char { value }
    }

    fn transform_pat_constructor(
        &mut self,
        _: Span,
        arg: Option<Box<CorePattern<Ty>>>,
        name: Symbol,
    ) -> CorePatternKind<Ty> {
        PatternKind::Constructor {
            name,
            arg: arg.map(|pat| Box::new(self.transform_pattern(*pat))),
        }
    }

    fn transform_pat_tuple(&mut self, _: Span, tuple: Vec<CorePattern<Ty>>) -> CorePatternKind<Ty> {
        PatternKind::Tuple {
            tuple: tuple
                .into_iter()
                .map(|pat| self.transform_pattern(pat))
                .collect(),
        }
    }

    fn transform_pat_variable(&mut self, _: Span, name: Symbol) -> CorePatternKind<Ty> {
        CorePatternKind::Variable { name }
    }

    fn transform_pat_wildcard(&mut self, _: Span) -> CorePatternKind<Ty> {
        CorePatternKind::Wildcard {}
    }
}
