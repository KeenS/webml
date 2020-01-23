use crate::config::Config;
use crate::hir::util::Traverse;
use crate::hir::*;
use crate::pass::Pass;
use std::collections::HashSet;

struct Trav<'a> {
    t: &'a mut ForceClosure,
    bound: bool,
}

impl<'a> Trav<'a> {
    fn new(t: &'a mut ForceClosure, bound: bool) -> Self {
        Trav { t, bound }
    }

    fn to(&mut self, bound: bool) -> &mut Self {
        self.bound = bound;
        self
    }

    fn bound(&self) -> bool {
        self.bound
    }

    fn with_bound<F: FnOnce(&mut Self)>(&mut self, bound: bool, f: F) {
        let prev = self.bound();
        self.to(bound);
        f(self);
        self.to(prev);
    }
}

impl<'a> Traverse for Trav<'a> {
    fn traverse_binds(&mut self, _ty: &mut HTy, binds: &mut Vec<Val>, ret: &mut Box<Expr>) {
        self.with_bound(true, |this| {
            for bind in binds.iter_mut() {
                this.traverse_expr(&mut bind.expr);
            }
        });
        self.traverse_expr(ret);
    }

    fn traverse_expr(&mut self, expr: &mut Expr) {
        use crate::hir::Expr::*;
        let assign;
        match expr {
            Binds { ty, binds, ret } => {
                self.traverse_binds(ty, binds, ret);
                return;
            }
            Fun {
                param,
                body_ty,
                body,
                captures,
            } => {
                self.traverse_fun(param, body_ty, body, captures);
                return;
            }
            Closure {
                envs,
                param_ty,
                body_ty,
                fname,
            } => {
                self.traverse_closure(envs, param_ty, body_ty, fname);
                return;
            }
            BuiltinCall { ty, fun, args } => {
                self.traverse_builtin_call(ty, fun, args);
                return;
            }
            ExternCall {
                ty,
                module,
                fun,
                args,
            } => {
                self.traverse_extern_call(ty, module, fun, args);
                return;
            }
            App { ty, fun, arg } => {
                self.traverse_app(ty, fun, arg);
                return;
            }
            Case { ty, expr, arms } => {
                self.traverse_case(ty, expr, arms);
                return;
            }

            Tuple { tys, tuple } => {
                self.traverse_tuple(tys, tuple);
                return;
            }

            Proj { ty, index, tuple } => {
                self.traverse_proj(ty, index, tuple);
                return;
            }

            Constructor {
                ty,
                arg,
                descriminant,
            } => {
                self.traverse_constructor(ty, arg, descriminant);
                return;
            }

            Sym { ty, name } => {
                if !self.bound() || !self.t.functions.contains(name) {
                    return;
                }
                match ty {
                    HTy::Fun(arg, ret) => {
                        assign = Closure {
                            envs: vec![],
                            param_ty: *arg.clone(),
                            body_ty: *ret.clone(),
                            fname: name.clone(),
                        }
                    }
                    _ => return,
                }
            }
            Lit { ty, value } => {
                self.traverse_lit(ty, value);
                return;
            }
        }
        *expr = assign;
    }

    fn traverse_fun(
        &mut self,
        _param: &mut (HTy, Symbol),
        _body_ty: &mut HTy,
        body: &mut Box<Expr>,
        _captures: &mut Vec<(HTy, Symbol)>,
    ) {
        self.with_bound(false, |this| this.traverse_expr(body))
    }

    fn traverse_app(&mut self, _ty: &mut HTy, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
        self.with_bound(false, |this| {
            this.traverse_expr(fun);
            this.traverse_expr(arg);
        });
    }
}

struct Reg<'a> {
    t: &'a mut ForceClosure,
    bound_name: Option<Symbol>,
}

impl<'a> Reg<'a> {
    fn new(t: &'a mut ForceClosure, bound_name: Option<Symbol>) -> Self {
        Reg { t, bound_name }
    }

    fn with_bound_name<F: FnOnce(&mut Self)>(&mut self, bound_name: Option<Symbol>, f: F) {
        let prev = self.bound_name.take();
        self.bound_name = bound_name;
        f(self);
        self.bound_name = prev;
    }
}

impl<'a> Traverse for Reg<'a> {
    fn traverse_val(&mut self, val: &mut Val) {
        self.bound_name = None;
        if val.rec {
            self.bound_name = Some(val.name.clone());
        }
        self.traverse_expr(&mut val.expr);
    }

    fn traverse_binds(&mut self, _ty: &mut HTy, binds: &mut Vec<Val>, ret: &mut Box<Expr>) {
        for bind in binds.iter_mut() {
            let mut bound_name = None;
            if bind.rec {
                bound_name = Some(bind.name.clone());
            }
            self.with_bound_name(bound_name, |this| this.traverse_expr(&mut bind.expr));
        }
        self.with_bound_name(None, |this| {
            this.traverse_expr(ret);
        });
    }

    fn traverse_fun(
        &mut self,
        _param: &mut (HTy, Symbol),
        _body_ty: &mut HTy,
        body: &mut Box<Expr>,
        _captures: &mut Vec<(HTy, Symbol)>,
    ) {
        match self.bound_name {
            Some(ref name) => {
                self.t.functions.insert(name.clone());
            }
            None => (),
        };
        self.with_bound_name(None, |this| {
            this.traverse_expr(body);
        });
    }

    fn traverse_app(&mut self, _ty: &mut HTy, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
        self.with_bound_name(None, |this| {
            this.traverse_expr(fun);
            this.traverse_expr(arg);
        })
    }

    fn traverse_case(
        &mut self,
        _ty: &mut HTy,
        expr: &mut Box<Expr>,
        arms: &mut Vec<(Pattern, Expr)>,
    ) {
        self.with_bound_name(None, |this| {
            this.traverse_expr(expr);
            for &mut (_, ref mut expr) in arms.iter_mut() {
                this.traverse_expr(expr)
            }
        });
    }
}

pub struct ForceClosure {
    functions: HashSet<Symbol>,
}

impl ForceClosure {
    pub fn new() -> Self {
        ForceClosure {
            functions: HashSet::new(),
        }
    }
}

impl<E> Pass<(SymbolTable, HIR), E> for ForceClosure {
    type Target = (SymbolTable, HIR);

    fn trans(
        &mut self,
        (symbol_table, mut hir): (SymbolTable, HIR),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        Reg::new(self, None).traverse_hir(&mut hir);
        Trav::new(self, false).traverse_hir(&mut hir);
        Ok((symbol_table, hir))
    }
}
