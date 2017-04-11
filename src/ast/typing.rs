use std::collections::HashMap;
use std::ops::{Drop, Deref, DerefMut};

use ast;
use prim::*;

#[derive(Debug)]
pub struct TyEnv {
    envs: Vec<HashMap<String, TyDefer>>,
    position: usize,
}


#[derive(Debug)]
struct Scope<'a>(&'a mut TyEnv);
impl<'a> Deref for Scope<'a> {
    type Target = TyEnv;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

fn unify<'a>(t1: &mut TyDefer, t2: &mut TyDefer) -> Result<'a, ()> {
    match (t1.get_mut().deref_mut(), t2.get_mut().deref_mut()) {
        (&mut Some(ref mut t1), &mut Some(ref mut t2)) => {
            if t1 == t2 {
                Ok(())
            } else {
                match (t1, t2) {
                    (&mut Ty::Fun(ref mut p1, ref mut b1),
                     &mut Ty::Fun(ref mut p2, ref mut b2)) => {
                        unify(p1, p2)?;
                        unify(b1, b2)?;
                        Ok(())
                    }
                    (t1, t2) => {
                        Err(TypeError::MisMatch {
                                expected: t1.clone(),
                                actual: t2.clone(),
                            })
                    }
                }
            }
        }
        (unknown @ &mut None, concrete @ &mut Some(_)) |
        (concrete @ &mut Some(_), unknown @ &mut None) => {
            *unknown = concrete.clone();
            Ok(())
        }
        _ => return Err(TypeError::CannotInfer),
    }

}



impl<'a> Scope<'a> {
    fn new(tyenv: &'a mut TyEnv) -> Self {
        if tyenv.envs.len() <= tyenv.position {
            tyenv.envs.push(HashMap::new())
        }
        tyenv.envs[tyenv.position].clear();
        tyenv.position += 1;

        Scope(tyenv)
    }

    fn scope(&mut self) -> Scope {
        Scope::new(self)
    }

    fn get_mut(&mut self, name: &str) -> Option<&mut TyDefer> {
        let range = 0..self.position;
        for env in self.envs[range].iter_mut().rev() {
            match env.get_mut(name) {
                found @ Some(_) => return found,
                _ => (),
            }
        }
        None

    }

    fn insert(&mut self, k: String, v: TyDefer) -> Option<TyDefer> {
        let pos = self.position - 1;
        self.envs[pos].insert(k, v)
    }

    fn infer_ast<'b, 'r>(&'b mut self, ast: &mut ast::AST) -> Result<'r, ()> {
        for mut val in ast.0.iter_mut() {
            self.infer_val(&mut val)?;
        }
        Ok(())
    }


    fn infer_val<'b, 'r>(&'b mut self, val: &mut ast::Val) -> Result<'r, ()> {
        let &mut ast::Val {
                     ref mut ty,
                     ref rec,
                     ref mut name,
                     ref mut expr,
                 } = val;
        if *rec {
            self.insert(name.0.clone(), ty.clone());
            self.infer_expr(expr, ty)?;

        } else {
            self.infer_expr(expr, ty)?;
            self.insert(name.0.clone(), ty.clone());
        }
        Ok(())
    }


    fn infer_expr<'b, 'r>(&'b mut self,
                          expr: &mut ast::Expr,
                          given: &mut TyDefer)
                          -> Result<'r, ()> {
        use ast::Expr::*;
        match expr {
            &mut Binds {
                     ref mut ty,
                     ref mut binds,
                     ref mut ret,
                 } => {
                let mut scope = self.scope();
                for mut bind in binds {
                    scope.infer_val(&mut bind)?;
                }
                scope.infer_expr(ret, ty)?;
                unify(ty, given)?;
                Ok(())
            }
            &mut Add {
                     ref mut ty,
                     ref mut l,
                     ref mut r,
                 } |
            &mut Mul {
                     ref mut ty,
                     ref mut l,
                     ref mut r,
                 } => {
                let mut lty = TyDefer::new(Some(Ty::Int));
                self.infer_expr(l, &mut lty)
                    .or_else(|_| {
                                 *lty.get_mut() = Some(Ty::Float);
                                 self.infer_expr(l, &mut lty)
                             })?;
                self.infer_expr(r, &mut lty)?;
                unify(&mut lty, given)?;
                unify(ty, given)?;
                Ok(())
            }
            &mut Fun {
                     ref mut param_ty,
                     ref mut param,
                     ref mut body_ty,
                     ref mut body,
                 } => {
                let mut scope = self.scope();
                scope.insert(param.0.clone(), param_ty.clone());

                scope.infer_expr(body, body_ty)?;
                let mut fn_ty = match (param_ty.defined(), body_ty.defined()) {
                    (Some(p), Some(b)) => TyDefer::new(Some(Ty::fun(p, b))),
                    _ => TyDefer::new(None),
                };
                unify(&mut fn_ty, given)?;

                Ok(())
            }
            &mut App {
                     ref mut ty,
                     ref mut fun,
                     ref mut arg,
                 } => {
                let mut fun_ty = TyDefer::new(Some(Ty::Fun(TyDefer::new(None), ty.clone())));
                self.infer_expr(fun, &mut fun_ty)?;
                match fun_ty.get_mut().deref_mut() {
                    &mut Some(Ty::Fun(ref mut param, ref mut ret)) => {
                        self.infer_expr(arg, param)?;
                        unify(given, ret)?;
                    }
                    _ => return Err(TypeError::NotFunction(fun.deref_mut().clone())),
                };

                unify(ty, given)?;
                Ok(())
            }
            &mut If {
                     ref mut cond,
                     ref mut ty,
                     ref mut then,
                     ref mut else_,
                 } => {
                let _cond_ty = self.infer_expr(cond, &mut TyDefer::new(Some(Ty::Bool)))?;
                self.infer_expr(then, given)?;
                self.infer_expr(else_, given)?;
                unify(ty, given)?;
                Ok(())
            }
            &mut Sym {
                     ref mut ty,
                     ref mut name,
                 } => {
                self.infer_symbol(name, given)?;
                unify(ty, given)?;
                Ok(())
            }
            &mut Lit {
                     ref mut ty,
                     ref mut value,
                 } => {
                self.infer_literal(value, given)?;
                unify(ty, given)?;
                Ok(())
            }
        }

    }

    fn infer_symbol<'b, 'r>(&'b mut self, sym: &mut Symbol, given: &mut TyDefer) -> Result<'r, ()> {
        match self.get_mut(&sym.0) {
            Some(t) => unify(t, given),
            None => {
                if &sym.0 == "print" {
                    *given.get_mut() = Some(Ty::fun(Ty::Float, Ty::Unit));
                    Ok(())
                } else {
                    Err(TypeError::FreeVar)
                }
            }
        }
    }

    fn infer_literal<'b, 'r>(&'b mut self,
                             lit: &mut Literal,
                             given: &mut TyDefer)
                             -> Result<'r, ()> {
        use prim::Literal::*;
        let ty = match lit {
            &mut Int(_) => Ty::Int,
            &mut Float(_) => Ty::Float,
            &mut Bool(_) => Ty::Bool,
        };
        match (ty, given.get_mut().deref_mut()) {
            (Ty::Int, &mut Some(Ty::Int)) => Ok(()),
            (Ty::Bool, &mut Some(Ty::Bool)) => Ok(()),
            (Ty::Float, &mut Some(Ty::Float)) => Ok(()),
            (ty, g @ &mut None) => {
                *g = Some(ty.clone());
                Ok(())
            }
            (ref ty, &mut Some(ref exp)) => {
                Err(TypeError::MisMatch {
                        expected: exp.clone(),
                        actual: ty.clone(),
                    })
            }
        }
    }
}



impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        assert!(0 < self.0.position);
        self.0.position -= 1;
    }
}

impl TyEnv {
    pub fn new() -> Self {
        TyEnv {
            envs: Vec::new(),
            position: 0,
        }
    }

    fn scope(&mut self) -> Scope {
        Scope::new(self)
    }

    pub fn infer<'a, 'b>(&'a mut self, ast: &mut ast::AST) -> Result<'b, ()> {
        let mut scope = self.scope();
        scope.infer_ast(ast)?;
        Ok(())
    }
}


use pass::Pass;
impl<'a> Pass<ast::AST, TypeError<'a>> for TyEnv {
    type Target = ast::AST;

    fn trans<'b>(&'b mut self, mut ast: ast::AST) -> Result<'a, Self::Target> {
        self.infer(&mut ast)?;
        Ok(ast)
    }
}
