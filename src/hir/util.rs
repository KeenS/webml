use crate::hir::*;

pub trait Traverse {
    fn traverse_hir(&mut self, hir: &mut HIR) {
        for val in hir.0.iter_mut() {
            self.traverse_val(val)
        }
    }

    fn traverse_val(&mut self, val: &mut Val) {
        self.traverse_expr(&mut val.expr)
    }

    fn traverse_expr(&mut self, expr: &mut Expr) {
        use crate::hir::Expr::*;
        match expr {
            Let { ty, bind, ret } => self.traverse_binds(ty, bind, ret),
            Fun {
                param,
                body_ty,
                body,
                captures,
            } => self.traverse_fun(param, body_ty, body, captures),
            Closure {
                envs,
                param_ty,
                body_ty,
                fname,
            } => self.traverse_closure(envs, param_ty, body_ty, fname),
            BuiltinCall { ty, fun, args } => self.traverse_builtin_call(ty, fun, args),
            ExternCall {
                ty,
                module,
                fun,
                args,
            } => self.traverse_extern_call(ty, module, fun, args),
            App { ty, fun, arg } => self.traverse_app(ty, fun, arg),
            Case { ty, expr, arms } => self.traverse_case(ty, expr, arms),
            Tuple { tys, tuple } => self.traverse_tuple(tys, tuple),
            Proj { ty, index, tuple } => self.traverse_proj(ty, index, tuple),
            Constructor {
                ty,
                arg,
                descriminant,
            } => self.traverse_constructor(ty, arg, descriminant),
            Sym { ty, name } => self.traverse_sym(ty, name),
            Lit { ty, value } => self.traverse_lit(ty, value),
        }
    }
    fn traverse_binds(&mut self, _ty: &mut HTy, bind: &mut Box<Val>, ret: &mut Box<Expr>) {
        self.traverse_val(&mut *bind);
        self.traverse_expr(ret)
    }

    fn traverse_fun(
        &mut self,
        _param: &mut (HTy, Symbol),
        _body_ty: &mut HTy,
        body: &mut Box<Expr>,
        _captures: &mut Vec<(HTy, Symbol)>,
    ) {
        self.traverse_expr(body)
    }

    fn traverse_closure(
        &mut self,
        _envs: &mut Vec<(HTy, Symbol)>,
        _param_ty: &mut HTy,
        _body_ty: &mut HTy,
        _fname: &mut Symbol,
    ) {
    }

    fn traverse_builtin_call(&mut self, _ty: &mut HTy, _fun: &mut BIF, args: &mut Vec<Expr>) {
        for arg in args {
            self.traverse_expr(arg)
        }
    }

    fn traverse_extern_call(
        &mut self,
        _ty: &mut HTy,
        _module: &mut String,
        _fun: &mut String,
        args: &mut Vec<Expr>,
    ) {
        for arg in args {
            self.traverse_expr(arg)
        }
    }

    fn traverse_app(&mut self, _ty: &mut HTy, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
        self.traverse_expr(fun);
        self.traverse_expr(arg);
    }

    fn traverse_case(
        &mut self,
        _ty: &mut HTy,
        expr: &mut Box<Expr>,
        arms: &mut Vec<(Pattern, Expr)>,
    ) {
        self.traverse_expr(expr);
        for &mut (_, ref mut e) in arms.iter_mut() {
            self.traverse_expr(e);
        }
    }

    fn traverse_tuple(&mut self, _tys: &mut Vec<HTy>, tuple: &mut Vec<Expr>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_proj(&mut self, _ty: &mut HTy, _index: &mut u32, tuple: &mut Box<Expr>) {
        self.traverse_expr(tuple)
    }

    fn traverse_constructor(
        &mut self,
        _ty: &mut HTy,
        arg: &mut Option<Box<Expr>>,
        _name: &mut u32,
    ) {
        if let Some(arg) = arg {
            self.traverse_expr(&mut *arg)
        }
    }

    fn traverse_sym(&mut self, _ty: &mut HTy, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _ty: &mut HTy, _value: &mut Literal) {}
}

pub trait Transform {
    fn transform_hir(&mut self, mut hir: HIR) -> HIR {
        hir.0 = hir
            .0
            .into_iter()
            .map(|val| self.transform_val(val))
            .collect();
        hir
    }

    fn transform_val(&mut self, mut val: Val) -> Val {
        val.expr = self.transform_expr(val.expr);
        val
    }

    fn transform_expr(&mut self, expr: Expr) -> Expr {
        use crate::hir::Expr::*;
        match expr {
            Let { ty, bind, ret } => self.transform_binds(ty, bind, ret),
            Fun {
                param,
                body_ty,
                body,
                captures,
            } => self.transform_fun(param, body_ty, body, captures),
            App { fun, arg, ty } => self.transform_app(ty, fun, arg),
            Case { ty, expr, arms } => self.transform_case(ty, expr, arms),
            Tuple { tys, tuple } => self.transform_tuple(tys, tuple),
            Proj { ty, index, tuple } => self.transform_proj(ty, index, tuple),
            BuiltinCall { ty, fun, args } => self.transform_builtin_call(ty, fun, args),
            ExternCall {
                ty,
                module,
                fun,
                args,
            } => self.transform_extern_call(ty, module, fun, args),
            Closure {
                envs,
                param_ty,
                body_ty,
                fname,
            } => self.transform_closure(envs, param_ty, body_ty, fname),
            Constructor {
                ty,
                arg,
                descriminant,
            } => self.transform_constructor(ty, arg, descriminant),
            Sym { ty, name } => self.transform_sym(ty, name),
            Lit { ty, value } => self.transform_lit(ty, value),
        }
    }

    fn transform_binds(&mut self, ty: HTy, bind: Box<Val>, ret: Box<Expr>) -> Expr {
        Expr::Let {
            ty,
            bind: Box::new(self.transform_val(*bind)),
            ret: Box::new(self.transform_expr(*ret)),
        }
    }

    fn transform_fun(
        &mut self,
        param: (HTy, Symbol),
        body_ty: HTy,
        body: Box<Expr>,
        captures: Vec<(HTy, Symbol)>,
    ) -> Expr {
        Expr::Fun {
            param,
            body_ty,
            captures,
            body: Box::new(self.transform_expr(*body)),
        }
    }

    fn transform_closure(
        &mut self,
        envs: Vec<(HTy, Symbol)>,
        param_ty: HTy,
        body_ty: HTy,
        fname: Symbol,
    ) -> Expr {
        Expr::Closure {
            envs,
            param_ty,
            body_ty,
            fname,
        }
    }

    fn transform_builtin_call(&mut self, ty: HTy, fun: BIF, args: Vec<Expr>) -> Expr {
        Expr::BuiltinCall {
            ty,
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
        }
    }

    fn transform_extern_call(
        &mut self,
        ty: HTy,
        module: String,
        fun: String,
        args: Vec<Expr>,
    ) -> Expr {
        Expr::ExternCall {
            ty,
            module,
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
        }
    }

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        Expr::App {
            ty,
            fun: Box::new(self.transform_expr(*fun)),
            arg: Box::new(self.transform_expr(*arg)),
        }
    }

    fn transform_case(&mut self, ty: HTy, cond: Box<Expr>, arms: Vec<(Pattern, Expr)>) -> Expr {
        Expr::Case {
            ty,
            expr: Box::new(self.transform_expr(*cond)),
            arms: arms
                .into_iter()
                .map(|(pat, expr)|
                                       // FIXME: pass `pat` to transformer
                                       (pat, self.transform_expr(expr)))
                .collect(),
        }
    }

    fn transform_tuple(&mut self, tys: Vec<HTy>, tuple: Vec<Expr>) -> Expr {
        Expr::Tuple {
            tys,
            tuple: tuple.into_iter().map(|e| self.transform_expr(e)).collect(),
        }
    }

    fn transform_proj(&mut self, ty: HTy, index: u32, tuple: Box<Expr>) -> Expr {
        Expr::Proj {
            ty,
            index,
            tuple: Box::new(self.transform_expr(*tuple)),
        }
    }

    fn transform_constructor(
        &mut self,
        ty: HTy,
        arg: Option<Box<Expr>>,
        descriminant: u32,
    ) -> Expr {
        Expr::Constructor {
            ty,
            arg: arg.map(|arg| Box::new(self.transform_expr(*arg))),
            descriminant,
        }
    }

    fn transform_sym(&mut self, ty: HTy, name: Symbol) -> Expr {
        Expr::Sym { ty, name }
    }

    fn transform_lit(&mut self, ty: HTy, value: Literal) -> Expr {
        Expr::Lit { ty, value }
    }
}
