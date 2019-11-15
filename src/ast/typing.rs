use crate::ast::*;
use crate::config::Config;
use crate::prim::*;
use std::collections::HashMap;
use std::ops::DerefMut;

#[derive(Debug)]
pub struct TyEnv {
    env: HashMap<String, TyDefer>,
}

fn unify<'a>(t1: &mut TyDefer, t2: &mut TyDefer) -> Result<'a, ()> {
    match (t1.get_mut().deref_mut(), t2.get_mut().deref_mut()) {
        (&mut Some(ref mut t1), &mut Some(ref mut t2)) => {
            if t1 == t2 {
                Ok(())
            } else {
                match (t1, t2) {
                    (
                        &mut Type::Fun(ref mut p1, ref mut b1),
                        &mut Type::Fun(ref mut p2, ref mut b2),
                    ) => {
                        unify(p1, p2)?;
                        unify(b1, b2)?;
                        Ok(())
                    }
                    (&mut Type::Tuple(ref mut tu1), &mut Type::Tuple(ref mut tu2)) => {
                        if tu1.len() != tu2.len() {
                            return Err(TypeError::MisMatch {
                                expected: Type::Tuple(tu1.clone()),
                                actual: Type::Tuple(tu2.clone()),
                            });
                        } else {
                            for (t1, t2) in tu1.iter_mut().zip(tu2) {
                                unify(t1, t2)?
                            }
                            Ok(())
                        }
                    }
                    (t1, t2) => Err(TypeError::MisMatch {
                        expected: t1.clone(),
                        actual: t2.clone(),
                    }),
                }
            }
        }
        (unknown @ &mut None, concrete @ &mut Some(_))
        | (concrete @ &mut Some(_), unknown @ &mut None) => {
            *unknown = concrete.clone();
            Ok(())
        }
        _ => return Err(TypeError::CannotInfer),
    }
}

impl TyEnv {
    fn get_mut(&mut self, name: &str) -> Option<&mut TyDefer> {
        self.env.get_mut(name)
    }

    fn insert(&mut self, k: String, v: TyDefer) -> Option<TyDefer> {
        self.env.insert(k, v)
    }
}

impl TyEnv {
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
            ref mut pattern,
            ref mut expr,
        } = val;
        let names = pattern.binds();
        if *rec {
            for (name, ty) in names {
                self.insert(name.0.clone(), ty.clone());
            }
            self.infer_expr(expr, ty)?;
        } else {
            self.infer_expr(expr, ty)?;
            for (name, ty) in names {
                self.insert(name.0.clone(), ty.clone());
            }
        }
        self.infer_pat(pattern, ty)?;

        Ok(())
    }

    fn infer_expr<'b, 'r>(
        &'b mut self,
        expr: &mut ast::Expr,
        given: &mut TyDefer,
    ) -> Result<'r, ()> {
        use crate::ast::Expr::*;
        match expr {
            &mut Binds {
                ref mut ty,
                ref mut binds,
                ref mut ret,
            } => {
                for mut bind in binds {
                    self.infer_val(&mut bind)?;
                }
                self.infer_expr(ret, ty)?;
                unify(ty, given)?;
                Ok(())
            }
            &mut BinOp {
                ref mut op,
                ref mut ty,
                ref mut l,
                ref mut r,
            } => {
                if ["+", "-", "*"].contains(&op.0.as_str()) {
                    let mut lty = TyDefer::new(Some(Type::Int));
                    self.infer_expr(l, &mut lty).or_else(|_| {
                        *lty.get_mut() = Some(Type::Float);
                        self.infer_expr(l, &mut lty)
                    })?;
                    self.infer_expr(r, &mut lty)?;
                    unify(&mut lty, given)?;
                    unify(ty, given)?;
                    Ok(())
                } else if ["=", "<>", ">", ">=", "<", "<="].contains(&op.0.as_str()) {
                    let mut lty = TyDefer::new(Some(Type::Int));
                    self.infer_expr(l, &mut lty).or_else(|_| {
                        *lty.get_mut() = Some(Type::Float);
                        self.infer_expr(l, &mut lty)
                    })?;
                    self.infer_expr(r, &mut lty)?;
                    let mut ret_ty = TyDefer::new(Some(Type::Bool));
                    unify(&mut ret_ty, given)?;
                    unify(ty, given)?;
                    Ok(())
                } else if ["div", "mod"].contains(&op.0.as_str()) {
                    let mut lty = TyDefer::new(Some(Type::Int));
                    self.infer_expr(l, &mut lty)?;
                    self.infer_expr(r, &mut lty)?;
                    unify(&mut lty, given)?;
                    unify(ty, given)?;
                    Ok(())
                } else if ["/"].contains(&op.0.as_str()) {
                    let mut lty = TyDefer::new(Some(Type::Float));
                    self.infer_expr(l, &mut lty)?;
                    self.infer_expr(r, &mut lty)?;
                    unify(&mut lty, given)?;
                    unify(ty, given)?;
                    Ok(())
                } else {
                    unimplemented!()
                }
            }
            &mut Fun {
                ref mut param_ty,
                ref mut param,
                ref mut body_ty,
                ref mut body,
            } => {
                self.insert(param.0.clone(), param_ty.clone());

                self.infer_expr(body, body_ty)?;
                let mut fn_ty = match (param_ty.defined(), body_ty.defined()) {
                    (Some(p), Some(b)) => TyDefer::new(Some(Type::fun(p, b))),
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
                let mut fun_ty = TyDefer::new(Some(Type::Fun(TyDefer::new(None), ty.clone())));
                self.infer_expr(fun, &mut fun_ty)?;
                match fun_ty.get_mut().deref_mut() {
                    &mut Some(Type::Fun(ref mut param, ref mut ret)) => {
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
                let _cond_ty = self.infer_expr(cond, &mut TyDefer::new(Some(Type::Bool)))?;
                self.infer_expr(then, given)?;
                self.infer_expr(else_, given)?;
                unify(ty, given)?;
                Ok(())
            }
            &mut Case {
                ref mut cond,
                ref mut ty,
                ref mut clauses,
            } => {
                let mut cond_ty = TyDefer::empty();
                // ignore error to allow to be inferred by patterns
                let _ = self.infer_expr(cond, &mut cond_ty);
                for &mut (ref mut pat, ref mut branch) in clauses.iter_mut() {
                    self.infer_pat(pat, &mut cond_ty)?;
                    self.infer_expr(branch, given)?;
                }
                // re-infer
                self.infer_expr(cond, &mut cond_ty)?;
                unify(ty, given)?;
                Ok(())
            }
            &mut Tuple {
                ref mut ty,
                ref mut tuple,
            } => {
                self.infer_tuple(tuple, given)?;
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
                    *given.get_mut() = Some(Type::fun(Type::Int, Type::unit()));
                    Ok(())
                } else {
                    Err(TypeError::FreeVar)
                }
            }
        }
    }

    fn infer_literal<'b, 'r>(
        &'b mut self,
        lit: &mut Literal,
        given: &mut TyDefer,
    ) -> Result<'r, ()> {
        use crate::prim::Literal::*;
        let ty = match lit {
            &mut Int(_) => Type::Int,
            &mut Float(_) => Type::Float,
            &mut Bool(_) => Type::Bool,
        };
        match (ty, given.get_mut().deref_mut()) {
            (Type::Int, &mut Some(Type::Int)) => Ok(()),
            (Type::Bool, &mut Some(Type::Bool)) => Ok(()),
            (Type::Float, &mut Some(Type::Float)) => Ok(()),
            (ty, g @ &mut None) => {
                *g = Some(ty.clone());
                Ok(())
            }
            (ref ty, &mut Some(ref exp)) => Err(TypeError::MisMatch {
                expected: exp.clone(),
                actual: ty.clone(),
            }),
        }
    }

    fn infer_pat<'b, 'r>(&'b mut self, pat: &mut Pattern, given: &mut TyDefer) -> Result<'r, ()> {
        use self::Pattern::*;
        match *pat {
            Lit {
                ref mut ty,
                ref mut value,
            } => {
                self.infer_literal(value, ty)?;
            }
            Tuple { .. } | Wildcard { .. } | Var { .. } => (),
        };
        let mut ty = pat.ty_defer();
        unify(&mut ty, given)?;
        for (name, ty) in pat.binds() {
            self.insert(name.0.clone(), ty.clone());
        }
        Ok(())
    }

    fn infer_tuple<'b, 'r>(
        &'b mut self,
        tuple: &mut Vec<Expr>,
        given: &mut TyDefer,
    ) -> Result<'r, ()> {
        let mut tys = vec![TyDefer::empty(); tuple.len()];

        for (e, t) in tuple.iter_mut().zip(tys.iter_mut()) {
            // ignoring the error of infering. Right, maybe.
            let _res = self.infer_expr(e, t);
        }
        unify(&mut TyDefer::new(Some(Type::Tuple(tys))), given)?;
        Ok(())
    }
}

impl TyEnv {
    pub fn new() -> Self {
        TyEnv {
            env: HashMap::new(),
        }
    }

    pub fn infer<'a, 'b>(&'a mut self, ast: &mut ast::AST) -> Result<'b, ()> {
        self.infer_ast(ast)?;
        Ok(())
    }
}

use crate::pass::Pass;
impl<'a> Pass<ast::AST, TypeError<'a>> for TyEnv {
    type Target = ast::AST;

    fn trans<'b>(&'b mut self, mut ast: ast::AST, _: &Config) -> Result<'a, Self::Target> {
        self.infer(&mut ast)?;
        Ok(ast)
    }
}
