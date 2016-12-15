use std::collections::HashMap;
use std::fmt;
use std::error::Error;
use std::ops::{Drop, Deref, DerefMut};
// use std::cell::RefCell;
// use std::rc::Rc;

use ast;
use ty::*;

pub struct TyEnv {
    envs: Vec<HashMap<String, TyDefer>>,
    position: usize,
}


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
        use ast::AST::*;
        match ast {
            // &mut TopFun(ref mut f) => {
            //     self.infer_fun(f)?;
            //     self.insert(f.name.0.clone(), f.ty.clone());
            //     Ok(())
            // },
            &mut TopVal(ref mut b) => {
                self.infer_val(b)?;
                Ok(())
            },
        }
    }

    fn infer_expr(&mut self, expr: &mut ast::Expr, given: &Option<Ty>) -> Result<TyDefer> {
        use ast::Expr::*;
        match expr {
            &mut Binds{ref mut ty, ref mut binds, ref mut ret} => {
                check_or_set!(ty, given);
                let mut scope = self.scope();
                for mut bind in binds {
                    scope.infer_bind(&mut bind)?;
                }
                let ret_ty = scope.infer_expr(ret, ty)?;
                assert_or_set!(ty, &ret_ty);

                Ok(ret_ty)
            }
            &mut Add{ref mut ty, ref mut l, ref mut r} |
            &mut Mul{ref mut ty, ref mut l, ref mut r}=> {
                check_or_set!(ty, given);
                assert_or_set!(ty, &Some(Ty::Int));
                let mut lty = self.infer_expr(l, &Some(Ty::Int))?;
                let mut rty = self.infer_expr(r, &Some(Ty::Int))?;
                Ok(ty.clone())
            }
            &mut Fun{ref mut ty, ref mut arg, ref mut body} => {
                check_or_set!(ty, given);
                let (arg_ty, ret_ty) = match ty.deref().deref() {
                    &Some(Ty::Fun(ref arg, ref ret)) => (Some(arg.deref().clone()), Some(ret.deref().clone())),
                    _ => (None, None),
                };
                let mut scope = self.scope();
                scope.insert(arg.0.clone(), TyDefer(arg_ty.clone()));

                let ret_ty_ = scope.infer_expr(body, &arg_ty)?;
                let arg_ty_ = scope.get(&arg.0).and_then(|ty| ty.deref().clone());
                let (arg_ty, ret_ty) = match (arg_ty_, ret_ty_.deref()) {
                    (Some(ref arg_ty), &Some(ref ret_ty)) => (arg_ty.clone(), ret_ty.clone()),
                    _ => return Err(TypeError::CannotInfer),
                };
                let fn_ty = Some(Ty::Fun(Box::new(arg_ty), Box::new(ret_ty)));
                assert_or_set!(ty, &fn_ty);
                Ok(TyDefer(fn_ty))

            },
            &mut If{ref mut cond, ref mut ty, ref mut then, ref mut else_} => {
                check_or_set!(ty, given);
                let mut cond_ty = self.infer_expr(cond, &Some(Ty::Bool))?;
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
            // &mut Seq{ref mut ty, ref mut exprs} => {
            //     // all but last is ()
            //     // the last is ty
            //     Err(TypeError::CannotInfer)
            // },
            &mut Sym(ref mut sym) => {
                let ty = self.infer_symbol(sym, given)?;
                assert!(ty.defined().is_some());
                Ok(ty)
            },
            &mut LitInt(_) => {
                check_or_set!(&mut TyDefer(Some(Ty::Int)), given);
                Ok(TyDefer(Some(Ty::Int)))
            },

            &mut LitBool(_) => {
                check_or_set!(&mut TyDefer(Some(Ty::Bool)), given);
                Ok(TyDefer(Some(Ty::Bool)))
            },

        }

    }

    fn infer_bind(&mut self, bind: &mut ast::Bind) -> Result<()> {
        use ast::Bind::*;
        match bind {
            &mut V(ref mut v) => self.infer_val(v),
        }
    }

    // fn infer_fun(&mut self, fun: &mut ast::Fun) -> Result<()> {
    //     Err(TypeError::CannotInfer)
    // }

    fn infer_val(&mut self, val: &mut ast::Val) -> Result<()> {
        let &mut ast::Val{ref mut ty, ref mut name, ref mut expr} = val;
        let body_ty = self.infer_expr(expr, &None)?;
        assert_or_set!(ty, &body_ty);
        self.insert(name.0.clone(), ty.clone());
        Ok(())
    }

    fn infer_symbol(&mut self, sym: &mut ast::Symbol, given: &Option<Ty>) -> Result<TyDefer> {
        let g = match self.get(&sym.0) {
            Some(t) => match (t.deref(), given) {
                (&Some(ref t), &Some(ref g)) => if t == g {
                    return Ok(TyDefer(Some(t.clone())))
                } else {
                    return Err(TypeError::MisMatch{expected: g.clone(), actual: t.clone()})
                },
                (&Some(ref t), &None) => return Ok(TyDefer(Some(t.clone()))),
                (&None, &Some(ref g)) => {
                    g.clone()
                },
                _ => return Err(TypeError::CannotInfer)
            },
            None => return Err(TypeError::FreeVar)
        };
        self.insert(sym.0.clone(), TyDefer(Some(g.clone())));
        Ok(TyDefer(Some(g)))

    }

}



impl <'a>Drop for Scope<'a> {
    fn drop(&mut self) {
        assert!(0 < self.0.position);
        self.0.position -= 1;
    }
}


#[derive(Debug)]
pub enum TypeError {
    MisMatch{expected: Ty, actual: Ty},
    CannotInfer,
    FreeVar
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Error for TypeError {
    fn description(&self) -> &str {
        use self::TypeError::*;
        match self {
            &MisMatch{..} => "type mismatches against expected type",
            &CannotInfer => "cannot infer the type",
            &FreeVar => "free variable is found"
        }
    }
}



type Result<T> = ::std::result::Result<T, TypeError>;

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

    pub fn infer(&mut self, asts: &mut Vec<ast::AST>) -> Result<()> {
        let mut scope = self.scope();
        for mut ast in asts {
            scope.infer_ast(ast)?
        }
        Ok(())
    }
}
