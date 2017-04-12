use std::collections::HashSet;

use hir::*;
use hir::util::Traverse;
use pass::Pass;

struct Trav<'a> {
    t: &'a mut ForceClosure,
    bound: bool,
}

impl<'a> Trav<'a> {
    fn new(t: &'a mut ForceClosure, bound: bool) -> Self {
        Trav { t: t, bound: bound }
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
        self.with_bound(true, |this| for bind in binds.iter_mut() {
            this.traverse_expr(&mut bind.expr);
        });
        self.traverse_expr(ret);
    }

    fn traverse_expr(&mut self, expr: &mut Expr) {
        use hir::Expr::*;
        let assign;
        match *expr {
            Binds {
                ref mut ty,
                ref mut binds,
                ref mut ret,
            } => {
                self.traverse_binds(ty, binds, ret);
                return;
            }
            Op {
                ref mut ty,
                ref mut name,
                ref mut l,
                ref mut r,
            } => {
                self.traverse_op(ty, name, l, r);
                return;
            }
            PrimFun {
                ref mut param_ty,
                ref mut ret_ty,
                ref mut name,
            } => {
                self.traverse_primfun(param_ty, ret_ty, name);
                return;
            }
            Fun {
                ref mut param,
                ref mut body_ty,
                ref mut body,
                ref mut captures,
                ref mut make_closure,
            } => {
                self.traverse_fun(param, body_ty, body, captures, make_closure);
                return;
            }
            Closure {
                ref mut envs,
                ref mut param_ty,
                ref mut body_ty,
                ref mut fname,
            } => {
                self.traverse_closure(envs, param_ty, body_ty, fname);
                return;
            }
            App {
                ref mut ty,
                ref mut fun,
                ref mut arg,
            } => {
                self.traverse_app(ty, fun, arg);
                return;
            }
            If {
                ref mut ty,
                ref mut cond,
                ref mut then,
                ref mut else_,
            } => {
                self.traverse_if(ty, cond, then, else_);
                return;
            }

            Sym {
                ref mut ty,
                ref mut name,
            } => {
                if !self.bound() || !self.t.functions.contains(name) {
                    return;
                }
                match *ty {
                    HTy::Fun(ref arg, ref ret) => {
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
            Lit {
                ref mut ty,
                ref mut value,
            } => {
                self.traverse_lit(ty, value);
                return;
            }

        }
        *expr = assign;
    }


    fn traverse_op(&mut self,
                   _ty: &mut HTy,
                   _name: &mut Symbol,
                   l: &mut Box<Expr>,
                   r: &mut Box<Expr>) {
        self.with_bound(false, |this| {
            this.traverse_expr(l);
            this.traverse_expr(r);
        });
    }

    fn traverse_fun(&mut self,
                    _param: &mut (HTy, Symbol),
                    _body_ty: &mut HTy,
                    body: &mut Box<Expr>,
                    _captures: &mut Vec<(HTy, Symbol)>,
                    _make_closure: &mut Option<bool>) {
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
        Reg {
            t: t,
            bound_name: bound_name,
        }
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
        self.with_bound_name(None, |this| { this.traverse_expr(ret); });
    }


    fn traverse_op(&mut self,
                   _ty: &mut HTy,
                   _name: &mut Symbol,
                   l: &mut Box<Expr>,
                   r: &mut Box<Expr>) {
        self.with_bound_name(None, |this| {
            this.traverse_expr(l);
            this.traverse_expr(r);

        });
    }


    fn traverse_fun(&mut self,
                    _param: &mut (HTy, Symbol),
                    _body_ty: &mut HTy,
                    body: &mut Box<Expr>,
                    _captures: &mut Vec<(HTy, Symbol)>,
                    _make_closure: &mut Option<bool>) {
        match self.bound_name {
            Some(ref name) => {
                self.t.functions.insert(name.clone());
            }
            None => (),
        };
        self.with_bound_name(None, |this| { this.traverse_expr(body); });
    }

    fn traverse_app(&mut self, _ty: &mut HTy, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
        self.with_bound_name(None, |this| {
            this.traverse_expr(fun);
            this.traverse_expr(arg);
        })
    }


    fn traverse_if(&mut self,
                   _ty: &mut HTy,
                   cond: &mut Box<Expr>,
                   then: &mut Box<Expr>,
                   else_: &mut Box<Expr>) {
        self.with_bound_name(None, |this| {
            this.traverse_expr(cond);
            this.traverse_expr(then);
            this.traverse_expr(else_);
        });
    }
}

pub struct ForceClosure {
    functions: HashSet<Symbol>,
}

impl ForceClosure {
    pub fn new() -> Self {
        ForceClosure { functions: HashSet::new() }
    }
}

impl<E> Pass<HIR, E> for ForceClosure {
    type Target = HIR;

    fn trans(&mut self, mut hir: HIR) -> ::std::result::Result<Self::Target, E> {
        Reg::new(self, None).traverse_hir(&mut hir);
        Trav::new(self, false).traverse_hir(&mut hir);
        Ok(hir)
    }
}
