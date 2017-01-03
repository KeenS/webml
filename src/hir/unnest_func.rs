use std::collections::HashSet;
use std::ops::{Deref, DerefMut, Drop};

use prim::*;
use hir::*;
use pass::Pass;

pub struct UnnestFunc {
    tables: Vec<HashSet<Symbol>>,
    pos: usize,
    id: usize,
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

    fn new_scope(&mut self) -> Scope {
        Scope::new(self)
    }

    fn new_fname(&mut self) -> Symbol {
        let new_name = format!("<anonfun>@{}", self.id);
        self.id += 1;
        Symbol(new_name.to_string())
    }

    fn add_scope(&mut self, symbol: Symbol) {
        let pos = self.pos - 1;
        self.tables[pos].insert(symbol);
        self.id += 1;

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
        let mut vals = hir.0
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
        val.expr = self.conv_expr(cls, val.expr);
        val
    }

    fn conv_expr(&mut self, cls: &mut Vec<Val>, expr: Expr) -> Expr {
        use hir::Expr::*;
        match expr {
            Binds { ty, mut binds, mut ret } => {
                binds = binds.into_iter()
                    .map(|mut bind| {
                        bind.expr = self.conv_expr(cls, bind.expr);
                        bind

                    })
                    .collect();
                ret = Box::new(self.conv_expr(cls, *ret));
                Binds {
                    ty: ty,
                    binds: binds,
                    ret: ret,
                }
            }
            Op { ty, name, mut l, mut r } => {
                l = Box::new(self.conv_expr(cls, *l));
                r = Box::new(self.conv_expr(cls, *r));
                Op {
                    ty: ty,
                    name: name,
                    l: l,
                    r: r,
                }
            }

            Fun { param, body_ty, mut body, mut captures } => {
                assert_eq!(captures.len(), 0);
                body = Box::new(self.conv_expr(cls, *body));
                let (param_ty, param) = param;
                let mut frees = Vec::new();
                self.analyze_free_expr(&mut frees, &param, &body);
                let fname = self.new_fname();
                let closure = Closure {
                    envs: frees.clone(),
                    param_ty: param_ty.clone(),
                    body_ty: body_ty.clone(),
                    fname: fname.clone(),
                };
                captures.extend(frees);
                let is_closure = captures.len() != 0;
                let anonfun = Fun {
                    param: (param_ty, param),
                    body_ty: body_ty,
                    body: body,
                    captures: captures,
                };
                let fty = anonfun.ty();
                cls.push(Val {
                    ty: anonfun.ty(),
                    rec: false,
                    name: fname.clone(),
                    expr: anonfun,
                });
                if is_closure {
                    closure
                } else {
                    Expr::Sym {
                        name: fname,
                        ty: fty,
                    }
                }
            }
            App { ty, mut fun, mut arg } => {
                fun = Box::new(self.conv_expr(cls, *fun));
                arg = Box::new(self.conv_expr(cls, *arg));
                App {
                    ty: ty,
                    fun: fun,
                    arg: arg,
                }
            }
            If { ty, mut cond, mut then, mut else_ } => {
                cond = Box::new(self.conv_expr(cls, *cond));
                then = Box::new(self.conv_expr(cls, *then));
                else_ = Box::new(self.conv_expr(cls, *else_));
                If {
                    ty: ty,
                    cond: cond,
                    then: then,
                    else_: else_,
                }
            }

            Sym { name, ty } => {
                Sym {
                    ty: ty,
                    name: name,
                }
            }
            expr @ Closure { .. } |
            expr @ Lit { .. } |
            expr @ PrimFun { .. } => expr,

        }
    }


    fn analyze_free_val<'b, 'c>(&'b mut self,
                                frees: &mut Vec<(Ty, Symbol)>,
                                bound: &Symbol,
                                val: &'c Val) {
        self.analyze_free_expr(frees, bound, &val.expr);
    }

    fn analyze_free_expr<'b, 'c>(&'b mut self,
                                 frees: &mut Vec<(Ty, Symbol)>,
                                 bound: &Symbol,
                                 expr: &'c Expr) {
        use hir::Expr::*;
        match expr {
            &Binds { ref binds, ref ret, .. } => {
                let mut scope = self;
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
            &Op { ref l, ref r, .. } => {
                self.analyze_free_expr(frees, bound, l);
                self.analyze_free_expr(frees, bound, r);
            }
            &Fun { .. } => panic!("internal bug"),
            &App { ref fun, ref arg, .. } => {
                self.analyze_free_expr(frees, bound, fun);
                self.analyze_free_expr(frees, bound, arg);
            }
            &If { ref cond, ref then, ref else_, .. } => {
                self.analyze_free_expr(frees, bound, cond);
                self.analyze_free_expr(frees, bound, then);
                self.analyze_free_expr(frees, bound, else_);
            }
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
            &Lit { .. } | &PrimFun { .. } => (),
        }
    }
}


impl UnnestFunc {
    pub fn new() -> Self {
        UnnestFunc {
            tables: Vec::new(),
            pos: 0,
            id: 0,
        }
    }

    fn scope<'a>(&'a mut self) -> Scope<'a> {
        Scope::new(self)
    }
}

impl Pass<HIR> for UnnestFunc {
    type Target = HIR;
    type Err = TypeError;
    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.scope().conv_hir(hir))
    }
}
