use crate::config::Config;
use crate::hir::*;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashSet;
use std::ops::{Deref, DerefMut, Drop};

pub struct UnnestFunc {
    tables: Vec<HashSet<Symbol>>,
    pos: usize,
    id: Id,
}

struct Scope<'a>(&'a mut UnnestFunc);

impl<'a> Deref for Scope<'a> {
    type Target = UnnestFunc;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        self.pos -= 1;
    }
}

impl<'a> Scope<'a> {
    fn new(inner: &'a mut UnnestFunc) -> Self {
        let pos = inner.pos;
        if inner.tables.len() <= pos {
            inner.tables.push(HashSet::new())
        } else {
            inner.tables[pos].clear();
        }

        inner.pos += 1;
        Scope(inner)
    }

    fn new_fname(&mut self, name: Option<Symbol>) -> Symbol {
        let new_name = match name {
            None => "<anonfun>".to_string(),
            Some(name) => format!("<{}>", name.0),
        };
        let id = self.id.next();
        Symbol(new_name, id)
    }

    fn add_scope(&mut self, symbol: Symbol) {
        let pos = self.pos - 1;
        self.tables[pos].insert(symbol);
    }

    fn is_in_scope(&mut self, symbol: &Symbol) -> bool {
        let pos = self.pos;
        for table in self.tables[0..pos].iter_mut().rev() {
            match table.contains(symbol) {
                true => return true,
                false => (),
            }
        }
        false
    }

    fn conv_hir(&mut self, mut hir: HIR) -> HIR {
        let mut closures = Vec::new();
        let mut vals = hir
            .0
            .into_iter()
            .map(|val| {
                if val.rec {
                    self.add_scope(val.name.clone());
                    self.conv_top_val(&mut closures, val)
                } else {
                    let val = self.conv_top_val(&mut closures, val);
                    self.add_scope(val.name.clone());
                    val
                }
            })
            .collect();
        closures.append(&mut vals);
        hir.0 = closures;
        hir
    }

    fn conv_top_val(&mut self, cls: &mut Vec<Val>, mut val: Val) -> Val {
        let bind_name = if val.rec {
            Some(val.name.clone())
        } else {
            None
        };
        val.expr = self.conv_expr(cls, val.expr, bind_name);
        val
    }

    fn conv_expr(&mut self, cls: &mut Vec<Val>, expr: Expr, bind_name: Option<Symbol>) -> Expr {
        use crate::hir::Expr::*;
        match expr {
            Binds {
                ty,
                mut binds,
                mut ret,
            } => {
                binds = binds
                    .into_iter()
                    .map(|mut bind| {
                        let bind_name = if bind.rec {
                            Some(bind.name.clone())
                        } else {
                            None
                        };
                        bind.expr = self.conv_expr(cls, bind.expr, bind_name);
                        bind.rec = false;
                        bind
                    })
                    .collect();
                ret = Box::new(self.conv_expr(cls, *ret, None));
                Binds { ty, binds, ret }
            }
            BinOp {
                ty,
                name,
                mut l,
                mut r,
            } => {
                l = Box::new(self.conv_expr(cls, *l, None));
                r = Box::new(self.conv_expr(cls, *r, None));
                BinOp { ty, name, l, r }
            }

            Fun {
                param,
                body_ty,
                mut body,
                mut captures,
                ..
            } => {
                assert_eq!(captures.len(), 0);
                body = Box::new(self.conv_expr(cls, *body, None));
                let (param_ty, param) = param;
                let mut frees = Vec::new();
                self.analyze_free_expr(&mut frees, &param, &body);
                let fname = self.new_fname(bind_name.clone());
                self.rename(&mut body, &bind_name, &fname);
                captures.extend(frees.clone());
                let is_closure = !captures.is_empty();
                let anonfun = Fun {
                    param: (param_ty.clone(), param),
                    body_ty: body_ty.clone(),
                    body,
                    captures,
                };
                let fty = anonfun.ty();
                cls.push(Val {
                    ty: anonfun.ty(),
                    rec: true,
                    name: fname.clone(),
                    expr: anonfun,
                });
                if is_closure {
                    Closure {
                        envs: frees,
                        param_ty,
                        body_ty,
                        fname,
                    }
                } else {
                    Expr::Sym {
                        name: fname,
                        ty: fty,
                    }
                }
            }
            BuiltinCall { ty, fun, mut arg } => {
                arg = Box::new(self.conv_expr(cls, *arg, None));
                BuiltinCall { ty, fun, arg }
            }
            App {
                ty,
                mut fun,
                mut arg,
            } => {
                fun = Box::new(self.conv_expr(cls, *fun, None));
                arg = Box::new(self.conv_expr(cls, *arg, None));
                App { ty, fun, arg }
            }
            Case {
                ty,
                mut expr,
                mut arms,
            } => {
                expr = Box::new(self.conv_expr(cls, *expr, None));
                arms = arms
                    .into_iter()
                    .map(|(pat, arm)| (pat, self.conv_expr(cls, arm, None)))
                    .collect();
                Case { ty, expr, arms }
            }
            Tuple { tys, tuple } => {
                let tuple = tuple
                    .into_iter()
                    .map(|t| self.conv_expr(cls, t, None))
                    .collect();
                Tuple { tys, tuple }
            }
            Proj { ty, index, tuple } => {
                let tuple = self.conv_expr(cls, *tuple, None);
                Proj {
                    ty,
                    tuple: Box::new(tuple),
                    index,
                }
            }
            Constructor { name, ty } => Constructor { ty, name },
            Sym { name, ty } => Sym { ty, name },
            expr @ Closure { .. } | expr @ Lit { .. } => expr,
        }
    }

    fn analyze_free_val<'b, 'c>(
        &'b mut self,
        frees: &mut Vec<(HTy, Symbol)>,
        bound: &Symbol,
        val: &'c Val,
    ) {
        self.analyze_free_expr(frees, bound, &val.expr);
    }

    fn analyze_free_expr<'b, 'c>(
        &'b mut self,
        frees: &mut Vec<(HTy, Symbol)>,
        bound: &Symbol,
        expr: &'c Expr,
    ) {
        use crate::hir::Expr::*;
        match expr {
            &Binds {
                ref binds, ref ret, ..
            } => {
                let scope = self;
                for bind in binds.iter() {
                    if bind.rec {
                        scope.add_scope(bind.name.clone());
                        scope.analyze_free_val(frees, bound, bind);
                    } else {
                        scope.analyze_free_val(frees, bound, bind);
                        scope.add_scope(bind.name.clone());
                    }
                }
                scope.analyze_free_expr(frees, bound, ret);
            }
            &BinOp { ref l, ref r, .. } => {
                self.analyze_free_expr(frees, bound, l);
                self.analyze_free_expr(frees, bound, r);
            }
            &Fun { .. } => panic!("internal bug"),
            &BuiltinCall { ref arg, .. } => {
                self.analyze_free_expr(frees, bound, arg);
            }
            &App {
                ref fun, ref arg, ..
            } => {
                self.analyze_free_expr(frees, bound, fun);
                self.analyze_free_expr(frees, bound, arg);
            }
            &Case {
                ref expr, ref arms, ..
            } => {
                self.analyze_free_expr(frees, bound, expr);
                let scope = self;
                for &(ref pat, ref arm) in arms.iter() {
                    use self::Pattern::*;
                    match *pat {
                        Constructor { .. } | Constant { .. } => (),
                        Tuple { ref tuple, .. } => {
                            for name in tuple {
                                scope.add_scope(name.clone())
                            }
                        }
                        Var { ref name, .. } => scope.add_scope(name.clone()),
                    }
                    scope.analyze_free_expr(frees, bound, arm);
                }
            }
            &Tuple { ref tuple, .. } => {
                for t in tuple.iter() {
                    self.analyze_free_expr(frees, bound, t);
                }
            }
            &Proj { ref tuple, .. } => self.analyze_free_expr(frees, bound, tuple),
            &Sym { ref name, ref ty } => {
                if !(self.is_in_scope(name) || bound == name) {
                    frees.push((ty.clone(), name.clone()))
                }
            }
            &Closure { ref envs, .. } => {
                for &(ref ty, ref name) in envs {
                    if !(self.is_in_scope(name) || bound == name) {
                        frees.push((ty.clone(), name.clone()))
                    }
                }
            }
            &Constructor { .. } | &Lit { .. } => (),
        }
    }

    fn rename(&mut self, expr: &mut Expr, from: &Option<Symbol>, to: &Symbol) {
        use crate::hir::Expr::*;
        match *expr {
            Binds {
                ref mut binds,
                ref mut ret,
                ..
            } => {
                for bind in binds.iter_mut() {
                    self.rename(&mut bind.expr, from, to)
                }
                self.rename(ret, from, to);
            }
            BinOp {
                ref mut l,
                ref mut r,
                ..
            } => {
                self.rename(l, from, to);
                self.rename(r, from, to);
            }

            Fun { body: _, .. } => {
                Box::new(());
            }
            BuiltinCall { ref mut arg, .. } => {
                self.rename(arg, from, to);
            }
            App {
                ref mut fun,
                ref mut arg,
                ..
            } => {
                self.rename(fun, from, to);
                self.rename(arg, from, to);
            }
            Case {
                ref mut expr,
                ref mut arms,
                ..
            } => {
                self.rename(expr, from, to);
                for &mut (_, ref mut arm) in arms.iter_mut() {
                    self.rename(arm, from, to);
                }
            }
            Tuple { ref mut tuple, .. } => {
                for t in tuple.iter_mut() {
                    self.rename(t, from, to);
                }
            }
            Proj { ref mut tuple, .. } => self.rename(tuple, from, to),
            Sym { ref mut name, .. } => {
                if from.is_some() && name == from.as_ref().unwrap() {
                    *name = to.clone()
                }
            }
            Closure { .. } | Constructor { .. } | Lit { .. } => (),
        }
    }
}

impl UnnestFunc {
    pub fn new(id: Id) -> Self {
        UnnestFunc {
            tables: Vec::new(),
            pos: 0,
            id,
        }
    }

    fn scope<'a>(&'a mut self) -> Scope<'a> {
        Scope::new(self)
    }
}

impl<E> Pass<HIR, E> for UnnestFunc {
    type Target = HIR;

    fn trans(&mut self, hir: HIR, _: &Config) -> ::std::result::Result<Self::Target, E> {
        Ok(self.scope().conv_hir(hir))
    }
}
