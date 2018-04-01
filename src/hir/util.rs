use hir::*;

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
        use hir::Expr::*;
        match *expr {
            Binds {
                ref mut ty,
                ref mut binds,
                ref mut ret,
            } => self.traverse_binds(ty, binds, ret),
            BinOp {
                ref mut ty,
                ref mut name,
                ref mut l,
                ref mut r,
            } => self.traverse_binop(ty, name, l, r),
            Fun {
                ref mut param,
                ref mut body_ty,
                ref mut body,
                ref mut captures,
            } => self.traverse_fun(param, body_ty, body, captures),
            Closure {
                ref mut envs,
                ref mut param_ty,
                ref mut body_ty,
                ref mut fname,
            } => self.traverse_closure(envs, param_ty, body_ty, fname),
            BuiltinCall {
                ref mut ty,
                ref mut fun,
                ref mut arg,
            } => self.traverse_builtin_call(ty, fun, arg),
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
            Tuple {
                ref mut tys,
                ref mut tuple,
            } => self.traverse_tuple(tys, tuple),

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
    fn traverse_binds(&mut self, _ty: &mut HTy, binds: &mut Vec<Val>, ret: &mut Box<Expr>) {
        for val in binds.iter_mut() {
            self.traverse_val(val)
        }
        self.traverse_expr(ret)
    }

    fn traverse_binop(
        &mut self,
        _ty: &mut HTy,
        _name: &mut Symbol,
        l: &mut Box<Expr>,
        r: &mut Box<Expr>,
    ) {
        self.traverse_expr(l);
        self.traverse_expr(r)
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

    fn traverse_builtin_call(&mut self, _ty: &mut HTy, fun: &mut BIF, arg: &mut Box<Expr>) {
        self.traverse_expr(arg);
    }

    fn traverse_app(&mut self, _ty: &mut HTy, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
        self.traverse_expr(fun);
        self.traverse_expr(arg);
    }

    fn traverse_if(
        &mut self,
        _ty: &mut HTy,
        cond: &mut Box<Expr>,
        then: &mut Box<Expr>,
        else_: &mut Box<Expr>,
    ) {
        self.traverse_expr(cond);
        self.traverse_expr(then);
        self.traverse_expr(else_);
    }

    fn traverse_tuple(&mut self, _tys: &mut Vec<HTy>, tuple: &mut Vec<Expr>) {
        for t in tuple.iter_mut() {
            self.traverse_expr(t)
        }
    }

    fn traverse_sym(&mut self, _ty: &mut HTy, _name: &mut Symbol) {}

    fn traverse_lit(&mut self, _ty: &mut HTy, _value: &mut Literal) {}
}

pub trait Transform {
    fn transform_hir(&mut self, mut hir: HIR) -> HIR {
        hir.0 = hir.0
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
        use hir::Expr::*;
        match expr {
            Binds { ty, binds, ret } => self.transform_binds(ty, binds, ret),
            BinOp { ty, name, l, r } => self.transform_binop(ty, name, l, r),
            Fun {
                param,
                body_ty,
                body,
                captures,
            } => self.transform_fun(param, body_ty, body, captures),
            App { fun, arg, ty } => self.transform_app(ty, fun, arg),
            If {
                ty,
                cond,
                then,
                else_,
            } => self.transform_if(ty, cond, then, else_),
            Tuple { tys, tuple } => self.transform_tuple(tys, tuple),
            BuiltinCall { ty, fun, arg } => self.transform_builtin_call(ty, fun, arg),
            Closure {
                envs,
                param_ty,
                body_ty,
                fname,
            } => self.transform_closure(envs, param_ty, body_ty, fname),
            Sym { ty, name } => self.transform_sym(ty, name),
            Lit { ty, value } => self.transform_lit(ty, value),
        }
    }

    fn transform_binds(&mut self, ty: HTy, binds: Vec<Val>, ret: Box<Expr>) -> Expr {
        Expr::Binds {
            ty: ty,
            binds: binds
                .into_iter()
                .map(|val| self.transform_val(val))
                .collect(),
            ret: Box::new(self.transform_expr(*ret)),
        }
    }

    fn transform_binop(&mut self, ty: HTy, name: Symbol, l: Box<Expr>, r: Box<Expr>) -> Expr {
        Expr::BinOp {
            ty,
            name,
            l: Box::new(self.transform_expr(*l)),
            r: Box::new(self.transform_expr(*r)),
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

    fn transform_builtin_call(&mut self, ty: HTy, fun: BIF, arg: Box<Expr>) -> Expr {
        Expr::BuiltinCall {
            ty: ty,
            fun: fun,
            arg: Box::new(self.transform_expr(*arg)),
        }
    }

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        Expr::App {
            ty: ty,
            fun: Box::new(self.transform_expr(*fun)),
            arg: Box::new(self.transform_expr(*arg)),
        }
    }

    fn transform_if(
        &mut self,
        ty: HTy,
        cond: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    ) -> Expr {
        Expr::If {
            ty: ty,
            cond: Box::new(self.transform_expr(*cond)),
            then: Box::new(self.transform_expr(*then)),
            else_: Box::new(self.transform_expr(*else_)),
        }
    }

    fn transform_tuple(&mut self, tys: Vec<HTy>, tuple: Vec<Expr>) -> Expr {
        Expr::Tuple {
            tys: tys,
            tuple: tuple.into_iter().map(|e| self.transform_expr(e)).collect(),
        }
    }

    fn transform_sym(&mut self, ty: HTy, name: Symbol) -> Expr {
        Expr::Sym { ty, name }
    }

    fn transform_lit(&mut self, ty: HTy, value: Literal) -> Expr {
        Expr::Lit { ty, value }
    }
}
