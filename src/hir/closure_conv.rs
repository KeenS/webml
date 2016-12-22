use std::collections::HashSet;

use prim::*;
use hir::*;
use pass::Pass;

pub struct ClosureConv {
    globals: HashSet<Symbol>,
    id: usize,
}

impl ClosureConv {
    pub fn new() -> Self {
        ClosureConv {
            globals: HashSet::new(),
            id: 0,
        }
    }

    fn new_fname(&mut self) -> Symbol {
        let new_name = format!("<anonfun>@{}", self.id);
        self.id += 1;
        Symbol(new_name.to_string())
    }

    fn add_scope(&mut self, symbol: Symbol) {
        self.globals.insert(symbol.clone());
    }

    fn is_in_scope(&mut self, symbol: &Symbol) -> bool {
        self.globals.contains(symbol)
    }

    fn conv_hir(&mut self, mut hir: HIR) -> HIR {
        let mut closures = Vec::new();
        let mut vals = hir.0.into_iter().map(|val| {
            if val.rec {
                self.add_scope(val.name.clone());
                self.conv_top_val(&mut closures, val)
            } else {
                let val = self.conv_top_val(&mut closures, val);
                self.add_scope(val.name.clone());
                val
            }
        }).collect();
        closures.append(&mut vals);
        hir.0 = closures;
        hir
    }

    fn conv_top_val(&mut self, cls: &mut Vec<Val>, mut val:  Val) -> Val {
        val.expr = self.conv_expr(cls, val.expr);
        val
    }

    fn conv_expr(&mut self, cls: &mut Vec<Val>, expr: Expr) -> Expr {
        use hir::Expr::*;
        match expr {
            Binds{ty, mut binds, mut ret} => {
                binds = binds.into_iter().map(|mut bind| {
                    bind.expr = self.conv_expr(cls, bind.expr);
                    bind

                }).collect();
                ret = Box::new(self.conv_expr(cls, *ret));
                Binds {
                    ty: ty,
                    binds: binds,
                    ret: ret,
                }
            },
            Fun{param_ty, param, body_ty, mut body} => {
                body = Box::new(self.conv_expr(cls, *body));
                let mut frees = Vec::new();
                self.analyze_free_expr(&mut frees, &param, &body);
                let fname = self.new_fname();
                let closure = Closure {
                    envs: frees,
                    param_ty: param_ty.clone(),
                    body_ty: body_ty.clone(),
                    fname: fname.clone(),
                };
                let anonfun = Fun {
                    param_ty: param_ty,
                    param: param,
                    body_ty: body_ty,
                    body: body
                };
                cls.push(Val {
                    ty: anonfun.ty(),
                    rec: false,
                    name: fname,
                    expr: anonfun,
                });
                closure
            },
            App{ty, mut fun, mut arg} => {
                fun = Box::new(self.conv_expr(cls, *fun));
                arg = Box::new(self.conv_expr(cls, *arg));
                App {
                    ty: ty,
                    fun: fun,
                    arg: arg
                }
            },
            If {ty, mut cond, mut then, mut else_} => {
                cond  = Box::new(self.conv_expr(cls, *cond));
                then  = Box::new(self.conv_expr(cls, *then));
                else_ = Box::new(self.conv_expr(cls, *else_));
                If {
                    ty: ty,
                    cond: cond,
                    then: then,
                    else_: else_,
                }
            }

            Sym{name, ty} => {
                Sym {ty: ty, name: name}
            },
            expr @ Closure{..} |
            expr @ Lit{..} |
            expr @ PrimFun{..} => expr

        }
    }


    fn analyze_free_val<'b, 'c>(&'b mut self, frees: &mut Vec<(Symbol, Ty)>, bound: &Symbol, val: &'c Val) {
        self.analyze_free_expr(frees, bound, &val.expr);
    }

    fn analyze_free_expr<'b, 'c>(&'b mut self, frees: &mut Vec<(Symbol, Ty)>, bound: &Symbol, expr: &'c Expr) {
        use hir::Expr::*;
        match expr {
            &Binds{ref binds, ref ret, ..} => {
                let mut scope = self;
                for bind in binds.iter() {
                    scope.analyze_free_val(frees, bound, bind);
                }
                scope.analyze_free_expr(frees, bound, ret);
            }
            ,
            &Fun{..} => panic!("internal bug"),
            &App{ref fun, ref arg, ..} => {
                self.analyze_free_expr(frees, bound, fun);
                self.analyze_free_expr(frees, bound, arg);
            },
            &If {ref cond, ref then, ref else_, ..} => {
                self.analyze_free_expr(frees, bound, cond);
                self.analyze_free_expr(frees, bound, then);
                self.analyze_free_expr(frees, bound, else_);
            }
            &Sym{ref name, ref ty} => {
                if ! (self.is_in_scope(name) ||  bound == name) {
                    frees.push((name.clone(), ty.clone()))
                }
            }
            &Closure{ref envs, ..} => {
                for &(ref name, ref ty) in envs {
                    if ! (self.is_in_scope(name) ||  bound == name) {
                        frees.push((name.clone(), ty.clone()))
                    }
                }
            },
            &Lit{..} |
            &PrimFun{..} => ()
        }
    }
}

impl Pass<HIR> for ClosureConv {
    type Target = HIR;
    type Err = TypeError;
    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, Self::Err> {
        Ok(self.conv_hir(hir))
    }
}
