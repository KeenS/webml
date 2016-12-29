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
impl <'a>Deref for Scope<'a> {
    type Target = TyEnv;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl <'a>DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

macro_rules! assert_or_set {
    ($expected: expr, $actual: expr) => {
        match ($expected as &mut Option<Ty>, $actual as &Option<Ty>) {
            (&mut Some(ref expected), &Some(ref actual)) => {
                if expected == actual {
                    ()
                } else {
                    return Err(TypeError::MisMatch{expected: expected.clone(), actual: actual.clone()})
                }
            },
            (var @ &mut None, actual @ &Some(_)) => {
                *var = actual.clone();
                ()
            },
            _ => return Err(TypeError::CannotInfer)
        }
    }
}

macro_rules! check_or_set {
    ($expected: expr, $actual: expr) => {
        match ($expected as &mut Option<Ty>, $actual as &Option<Ty>) {
            (&mut Some(ref expected), &Some(ref actual)) => {
                if expected == actual {
                    ()
                } else {
                    return Err(TypeError::MisMatch{expected: expected.clone(), actual: actual.clone()})
                }
            },
            (var @ &mut None, actual @ &Some(_)) => {
                *var = actual.clone();
                ()
            },
            (_, &None) => ()
        }
    }
}



impl <'a>Scope<'a> {
    fn new(tyenv: &'a mut TyEnv) -> Self {
        if  tyenv.envs.len() <= tyenv.position {
            tyenv.envs.push(HashMap::new())
        }
        tyenv.envs[tyenv.position].clear();
        tyenv.position += 1;

        Scope(tyenv)
    }

    fn scope(&mut self) -> Scope {
        Scope::new(self)
    }


    fn get(&self, name: &str) -> Option<&TyDefer> {
        for env in self.envs[0..self.position].iter().rev() {
            match env.get(name) {
                found @ Some(_) => return found,
                _ => ()
            }
        }
        None

    }

    fn get_mut (&mut self, name: &str) -> Option<&mut TyDefer> {
        let range = 0..self.position;
        for env in self.envs[range].iter_mut().rev() {
            match env.get_mut(name) {
                found @ Some(_) => return found,
                _ => ()
            }
        }
        None

    }

    fn insert(&mut self, k: String, v: TyDefer) -> Option<TyDefer> {
        let pos = self.position - 1;
        self.envs[pos].insert(k, v)
    }

    fn infer_ast(&mut self, ast: &mut ast::AST) -> Result<()> {
        for mut val in ast.0.iter_mut() {
            self.infer_val(&mut val)?;
        };
        Ok(())
    }


    fn infer_val(&mut self, val: &mut ast::Val) -> Result<()> {
        let &mut ast::Val{ref mut ty, ref rec, ref mut name, ref mut expr} = val;
        let body_ty;
        if *rec {
            self.insert(name.0.clone(), ty.clone());
            body_ty = self.infer_expr(expr, &None)?;

        } else {
            body_ty = self.infer_expr(expr, &None)?;
            self.insert(name.0.clone(), ty.clone());
        }
        assert_or_set!(ty, &body_ty);
        self.insert(name.0.clone(), ty.clone());
        Ok(())
    }


    fn infer_expr(&mut self, expr: &mut ast::Expr, given: &Option<Ty>) -> Result<TyDefer> {
        use ast::Expr::*;
        match expr {
            &mut Binds{ref mut ty, ref mut binds, ref mut ret} => {
                check_or_set!(ty, given);
                let mut scope = self.scope();
                for mut bind in binds {
                    scope.infer_val(&mut bind)?;
                }
                let ret_ty = scope.infer_expr(ret, ty)?;
                assert_or_set!(ty, &ret_ty);

                Ok(ret_ty)
            }
            &mut Add{ref mut ty, ref mut l, ref mut r} |
            &mut Mul{ref mut ty, ref mut l, ref mut r}=> {
                check_or_set!(ty, given);
                assert_or_set!(ty, &Some(Ty::Int));
                let _lty = self.infer_expr(l, &Some(Ty::Int))?;
                let _rty = self.infer_expr(r, &Some(Ty::Int))?;
                Ok(ty.clone())
            }
            &mut Fun{ref mut param_ty, ref mut param, ref mut body_ty, ref mut body} => {
                //check_or_set!(ty, given);
                let mut scope = self.scope();
                scope.insert(param.0.clone(), param_ty.clone());

                let body_ty_ = scope.infer_expr(body, &body_ty)?;
                let param_ty_ = scope.get(&param.0).and_then(|ty| ty.deref().clone());
                assert_or_set!(param_ty, &param_ty_);
                assert_or_set!(body_ty, &body_ty_);
                let param_ty = param_ty.deref().deref().clone();
                let body_ty = body_ty.deref().deref().clone();
                let fn_ty = param_ty
                    .and_then(|p|
                              body_ty.map(|b| Ty::fun(p, b)));
                Ok(TyDefer(fn_ty))
            },
            &mut App{ref mut ty, ref mut fun, ref mut arg} => {
                check_or_set!(ty, given);
                let fun_ty = self.infer_expr(fun, &None)?;
                let (param_ty, ret_ty) = match fun_ty.deref() {
                    &Some(Ty::Fun(ref param, ref ret)) => (param, ret),
                    _ => return Err(TypeError::NotFunction(fun.deref_mut().clone()))
                };
                let _arg_ty = self.infer_expr(arg, &Some(param_ty.deref().clone()))?;
                assert_or_set!(ty, &Some(ret_ty.deref().clone()));
                Ok(TyDefer(Some(ret_ty.deref().clone())))
            }
            &mut If{ref mut cond, ref mut ty, ref mut then, ref mut else_} => {
                check_or_set!(ty, given);
                let _cond_ty = self.infer_expr(cond, &Some(Ty::Bool))?;
                let then_ty_ = self.infer_expr(then, given)?;
                let else_ty_ = self.infer_expr(else_, &then_ty_)?;
                let then_ty_ = self.infer_expr(then, &else_ty_)?;
                let ret_ty = match (then_ty_.defined(), else_ty_.defined()) {
                    (Some(tty), Some(ety)) => if tty == ety {
                        tty
                    } else {
                        return Err(TypeError::MisMatch{expected: tty, actual: ety})
                    },
                    _ => return Err(TypeError::CannotInfer)
                };
                assert_or_set!(ty, &Some(ret_ty));

                Ok(ty.clone())
            },
            &mut Sym{ref mut ty, ref mut name} => {
                check_or_set!(ty, given);
                let ty_ = self.infer_symbol(name, given)?;
                assert_or_set!(ty, ty_.deref());
                Ok(ty_)
            },
            &mut Lit{ref mut ty, ref mut value} => {
                check_or_set!(ty, given);
                let ty_ = self.infer_literal(value, given)?;
                assert_or_set!(ty, ty_.deref());
                Ok(ty_)
            },
        }

    }

    fn infer_symbol(&mut self, sym: &mut Symbol, given: &Option<Ty>) -> Result<TyDefer> {
        match self.get_mut(&sym.0) {
            Some(t) => match (t.deref_mut(), given) {
                (&mut Some(ref t), &Some(ref g)) => if t == g {
                    return Ok(TyDefer(Some(t.clone())))
                } else {
                    return Err(TypeError::MisMatch{expected: g.clone(), actual: t.clone()})
                },
                (&mut Some(ref t), &None) => return Ok(TyDefer(Some(t.clone()))),
                (x @ &mut None, g @ &Some(_)) => {
                    *x = g.clone();
                    return Ok(TyDefer(g.clone()))
                },
                _ => return Err(TypeError::CannotInfer)
            },
            None => return Err(TypeError::FreeVar)
        };
    }

    fn infer_literal(&mut self, lit: &mut Literal, given: &Option<Ty>) -> Result<TyDefer> {
        use prim::Literal::*;
        let ty = match lit {
            &mut Int(_)  => Ty::Int,
            &mut Bool(_) => Ty::Bool,
        };
        match (ty, given) {
            (Ty::Int , &Some(Ty::Int))  => Ok(TyDefer(Some(Ty::Int))),
            (Ty::Bool, &Some(Ty::Bool)) => Ok(TyDefer(Some(Ty::Bool))),
            (ty, &None)           => Ok(TyDefer(Some(ty))),
            (ref ty, &Some(ref exp)) => Err(TypeError::MisMatch{expected: exp.clone(), actual: ty.clone()})
        }
    }

}



impl <'a>Drop for Scope<'a> {
    fn drop(&mut self) {
        assert!(0 < self.0.position);
        self.0.position -= 1;
    }
}

impl TyEnv {
    pub fn new() -> Self {
        TyEnv {
            envs: Vec::new(),
            position: 0
        }
    }

    fn scope(&mut self) -> Scope {
        Scope::new(self)
    }

    pub fn infer(&mut self, ast: &mut ast::AST) -> Result<()> {
        let mut scope = self.scope();
        scope.infer_ast(ast)?;
        Ok(())
    }
}


use pass::Pass;
impl Pass<ast::AST> for TyEnv {
    type Target = ast::AST;
    type Err = TypeError;
    fn trans(&mut self, mut ast: ast::AST) -> Result<Self::Target> {
        self.infer(&mut ast)?;
        Ok(ast)
    }
}
