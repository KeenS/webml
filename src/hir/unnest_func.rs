use crate::config::Config;
use crate::hir::*;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashSet;
use std::ops::{Deref, DerefMut, Drop};

pub struct UnnestFunc {
    tables: Vec<HashSet<Symbol>>,
    tops: Vec<Val>,
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

    fn new_closure(&mut self, val: Val) {
        self.add_scope(val.name.clone());
        self.0.tops.push(val);
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
        let mut vals = hir
            .0
            .into_iter()
            .map(|val| {
                if val.rec {
                    self.add_scope(val.name.clone());
                    self.conv_top_val(val)
                } else {
                    let val = self.conv_top_val(val);
                    self.add_scope(val.name.clone());
                    val
                }
            })
            .collect();
        let mut closures = self.0.tops.drain(..).collect::<Vec<_>>();
        closures.append(&mut vals);
        hir.0 = closures;
        hir
    }

    fn conv_top_val(&mut self, mut val: Val) -> Val {
        let bind_name = if val.rec {
            Some(val.name.clone())
        } else {
            None
        };
        val.expr = self.conv_expr(val.expr, bind_name, true);
        val
    }

    fn conv_expr(&mut self, expr: Expr, bind_name: Option<Symbol>, is_top: bool) -> Expr {
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
                        bind.expr = self.conv_expr(bind.expr, bind_name, false);
                        bind.rec = false;
                        bind
                    })
                    .collect();
                ret = Box::new(self.conv_expr(*ret, None, false));
                Binds { ty, binds, ret }
            }
            Fun {
                param,
                body_ty,
                mut body,
                mut captures,
                ..
            } => {
                assert_eq!(captures.len(), 0);
                body = Box::new(self.conv_expr(*body, None, false));
                let (param_ty, param) = param;
                let mut frees = Vec::new();
                self.analyze_free_expr(&mut frees, &param, &body);
                frees.dedup();
                captures.extend(frees.clone());
                let is_closure = !captures.is_empty();
                if !is_closure && is_top {
                    // toplevel function
                    return Fun {
                        param: (param_ty.clone(), param),
                        body_ty: body_ty.clone(),
                        body,
                        captures,
                    };
                }

                let fname = self.new_fname(bind_name.clone());
                self.rename(&mut body, &bind_name, &fname);
                let anonfun = Fun {
                    param: (param_ty.clone(), param),
                    body_ty: body_ty.clone(),
                    body,
                    captures,
                };
                let fty = anonfun.ty();
                self.new_closure(Val {
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
            BuiltinCall { ty, fun, args } => {
                let args = args
                    .into_iter()
                    .map(|arg| self.conv_expr(arg, None, false))
                    .collect();
                BuiltinCall { ty, fun, args }
            }
            ExternCall {
                ty,
                module,
                fun,
                args,
            } => {
                let args = args
                    .into_iter()
                    .map(|arg| self.conv_expr(arg, None, false))
                    .collect();
                ExternCall {
                    ty,
                    module,
                    fun,
                    args,
                }
            }
            App {
                ty,
                mut fun,
                mut arg,
            } => {
                fun = Box::new(self.conv_expr(*fun, None, false));
                arg = Box::new(self.conv_expr(*arg, None, false));
                App { ty, fun, arg }
            }
            Case {
                ty,
                mut expr,
                mut arms,
            } => {
                expr = Box::new(self.conv_expr(*expr, None, false));
                arms = arms
                    .into_iter()
                    .map(|(pat, arm)| (pat, self.conv_expr(arm, None, false)))
                    .collect();
                Case { ty, expr, arms }
            }
            Tuple { tys, tuple } => {
                let tuple = tuple
                    .into_iter()
                    .map(|t| self.conv_expr(t, None, false))
                    .collect();
                Tuple { tys, tuple }
            }
            Proj { ty, index, tuple } => {
                let tuple = self.conv_expr(*tuple, None, false);
                Proj {
                    ty,
                    tuple: Box::new(tuple),
                    index,
                }
            }
            Constructor {
                descriminant,
                arg,
                ty,
            } => {
                let arg = arg.map(|a| Box::new(self.conv_expr(*a, None, false)));
                Constructor {
                    descriminant,
                    arg,
                    ty,
                }
            }
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
            Binds { binds, ret, .. } => {
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
            Fun { .. } => panic!("internal bug"),
            BuiltinCall { args, .. } => {
                for arg in args {
                    self.analyze_free_expr(frees, bound, arg);
                }
            }
            ExternCall { args, .. } => {
                for arg in args {
                    self.analyze_free_expr(frees, bound, arg);
                }
            }
            App { fun, arg, .. } => {
                self.analyze_free_expr(frees, bound, fun);
                self.analyze_free_expr(frees, bound, arg);
            }
            Case { expr, arms, .. } => {
                self.analyze_free_expr(frees, bound, expr);
                let scope = self;
                for (pat, arm) in arms.iter() {
                    use self::Pattern::*;
                    match pat {
                        Constructor { arg, .. } => {
                            if let Some((_, arg)) = arg {
                                scope.add_scope(arg.clone())
                            }
                        }
                        Constant { .. } => (),
                        Char { .. } => (),
                        Tuple { tuple, .. } => {
                            for name in tuple {
                                scope.add_scope(name.clone())
                            }
                        }
                        Var { name, .. } => scope.add_scope(name.clone()),
                    }
                    scope.analyze_free_expr(frees, bound, arm);
                }
            }
            Tuple { tuple, .. } => {
                for t in tuple.iter() {
                    self.analyze_free_expr(frees, bound, t);
                }
            }
            Proj { tuple, .. } => self.analyze_free_expr(frees, bound, tuple),
            Sym { name, ty } => {
                if !(self.is_in_scope(name) || bound == name) {
                    frees.push((ty.clone(), name.clone()))
                }
            }
            Closure { envs, .. } => {
                for (ty, name) in envs {
                    if !(self.is_in_scope(name) || bound == name) {
                        frees.push((ty.clone(), name.clone()))
                    }
                }
            }
            Constructor { arg, .. } => {
                if let Some(arg) = arg {
                    self.analyze_free_expr(frees, bound, arg)
                }
            }
            Lit { .. } => (),
        }
    }

    fn rename(&mut self, expr: &mut Expr, from: &Option<Symbol>, to: &Symbol) {
        use crate::hir::Expr::*;
        match expr {
            Binds { binds, ret, .. } => {
                for bind in binds.iter_mut() {
                    self.rename(&mut bind.expr, from, to)
                }
                self.rename(ret, from, to);
            }
            Fun { body, .. } => self.rename(body, from, to),
            BuiltinCall { args, .. } => {
                for arg in args {
                    self.rename(arg, from, to);
                }
            }
            ExternCall { args, .. } => {
                for arg in args {
                    self.rename(arg, from, to);
                }
            }
            App { fun, arg, .. } => {
                self.rename(fun, from, to);
                self.rename(arg, from, to);
            }
            Case { expr, arms, .. } => {
                self.rename(expr, from, to);
                for (_, arm) in arms.iter_mut() {
                    self.rename(arm, from, to);
                }
            }
            Tuple { tuple, .. } => {
                for t in tuple.iter_mut() {
                    self.rename(t, from, to);
                }
            }
            Proj { tuple, .. } => self.rename(tuple, from, to),
            Sym { name, .. } => {
                if from.is_some() && name == from.as_ref().unwrap() {
                    *name = to.clone()
                }
            }
            Constructor { arg, .. } => {
                if let Some(arg) = arg {
                    self.rename(arg, from, to)
                }
            }
            Closure { .. } | Lit { .. } => (),
        }
    }
}

impl UnnestFunc {
    pub fn new(id: Id) -> Self {
        UnnestFunc {
            tables: Vec::new(),
            tops: Vec::new(),
            pos: 0,
            id,
        }
    }

    fn scope<'a>(&'a mut self) -> Scope<'a> {
        Scope::new(self)
    }
}

impl<E> Pass<(SymbolTable, HIR), E> for UnnestFunc {
    type Target = (SymbolTable, HIR);

    fn trans(
        &mut self,
        (symbol_table, hir): (SymbolTable, HIR),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        Ok((symbol_table, self.scope().conv_hir(hir)))
    }
}
