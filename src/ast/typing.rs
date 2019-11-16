use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use crate::prim::*;
use crate::unification_pool::{NodeId, UnificationPool};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TyEnv {
    env: HashMap<Symbol, NodeId>,
    pool: TypePool,
}

#[derive(Debug)]
struct TypePool {
    pool: UnificationPool<Typing>,
    id: Id,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Typing {
    Var(u64),
    Bool,
    Int,
    Float,
    Fun(NodeId, NodeId),
    Tuple(Vec<NodeId>),
}

fn resolve(pool: &UnificationPool<Typing>, id: NodeId) -> Type {
    conv_ty(pool, pool.value_of(id).clone())
}

fn conv_ty(pool: &UnificationPool<Typing>, ty: Typing) -> Type {
    use Typing::*;
    match ty {
        Var(id) => Type::Var(id),
        Bool => Type::Bool,
        Int => Type::Int,
        Float => Type::Float,
        Fun(param, body) => Type::Fun(
            Box::new(resolve(pool, param)),
            Box::new(resolve(pool, body)),
        ),
        Tuple(tys) => Type::Tuple(tys.into_iter().map(|ty| resolve(pool, ty)).collect()),
    }
}

fn try_unify<'b, 'r>(
    pool: &'b mut UnificationPool<Typing>,
    t1: Typing,
    t2: Typing,
) -> Result<'r, Typing> {
    use Typing::*;
    match (t1, t2) {
        (t1, t2) if t1 == t2 => Ok(t1),
        (Var(_), ty) | (ty, Var(_)) => Ok(ty.clone()),
        (Fun(p1, b1), Fun(p2, b2)) => {
            let p = pool.try_unify_with(p1, p2, try_unify)?;
            let b = pool.try_unify_with(b1, b2, try_unify)?;
            Ok(Fun(p, b))
        }
        (Tuple(tu1), Tuple(tu2)) => {
            if tu1.len() != tu2.len() {
                return Err(TypeError::MisMatch {
                    expected: conv_ty(pool, Tuple(tu1).clone()),
                    actual: conv_ty(pool, Tuple(tu2).clone()),
                });
            } else {
                let tu = tu1
                    .into_iter()
                    .zip(tu2)
                    .map(|(t1, t2)| pool.try_unify_with(t1, t2, try_unify))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Tuple(tu))
            }
        }
        (t1, t2) => Err(TypeError::MisMatch {
            expected: conv_ty(pool, t1.clone()),
            actual: conv_ty(pool, t2.clone()),
        }),
    }
}

impl TyEnv {
    fn get(&self, name: &Symbol) -> Option<NodeId> {
        self.env.get(name).cloned()
    }

    fn insert(&mut self, k: Symbol, v: NodeId) -> Option<NodeId> {
        self.env.insert(k, v)
    }
}

impl TypePool {
    fn new() -> Self {
        Self {
            pool: UnificationPool::new(),
            id: Id::new(),
        }
    }

    fn tyvar(&mut self) -> NodeId {
        self.pool.node_new(Typing::Var(self.id.next()))
    }

    fn ty(&mut self, ty: Typing) -> NodeId {
        self.pool.node_new(ty)
    }

    fn node_new(&mut self, t: Typing) -> NodeId {
        self.pool.node_new(t)
    }

    fn try_unify_with<'r>(
        &mut self,
        id1: NodeId,
        id2: NodeId,
        try_unify: impl FnOnce(&mut UnificationPool<Typing>, Typing, Typing) -> Result<'r, Typing>,
    ) -> Result<'r, NodeId> {
        self.pool.try_unify_with(id1, id2, try_unify)
    }
}

impl<Ty> AST<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> AST<Ty2> {
        AST(self.0.into_iter().map(move |val| val.map_ty(f)).collect())
    }
}

impl<Ty> Val<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Val<Ty2> {
        Val {
            ty: f(self.ty),
            rec: self.rec,
            pattern: self.pattern.map_ty(&mut *f),
            expr: self.expr.map_ty(f),
        }
    }
}

impl<Ty> Expr<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Expr<Ty2> {
        use crate::ast::Expr::*;
        match self {
            Binds { ty, binds, ret } => Binds {
                ty: f(ty),
                binds: binds.into_iter().map(|val| val.map_ty(f)).collect(),
                ret: ret.map_ty(f).boxed(),
            },
            BinOp { op, ty, l, r } => BinOp {
                op,
                ty: f(ty),
                l: l.map_ty(f).boxed(),
                r: r.map_ty(f).boxed(),
            },
            Fun { param, ty, body } => Fun {
                param: param,
                ty: f(ty),
                body: body.map_ty(f).boxed(),
            },
            App { ty, fun, arg } => App {
                ty: f(ty),
                fun: fun.map_ty(f).boxed(),
                arg: arg.map_ty(f).boxed(),
            },
            If {
                ty,
                cond,
                then,
                else_,
            } => If {
                ty: f(ty),
                cond: cond.map_ty(f).boxed(),
                then: then.map_ty(f).boxed(),
                else_: else_.map_ty(f).boxed(),
            },
            Case { ty, cond, clauses } => Case {
                ty: f(ty),
                cond: cond.map_ty(&mut *f).boxed(),
                clauses: clauses
                    .into_iter()
                    .map(move |(pat, expr)| (pat.map_ty(&mut *f), expr.map_ty(f)))
                    .collect(),
            },
            Tuple { ty, tuple } => Tuple {
                ty: f(ty),
                tuple: tuple.into_iter().map(|t| t.map_ty(f)).collect(),
            },

            Sym { ty, name } => Sym {
                ty: f(ty),
                name: name,
            },
            Lit { ty, value } => Lit {
                ty: f(ty),
                value: value,
            },
        }
    }
}

impl<Ty> Pattern<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Pattern<Ty2> {
        use Pattern::*;
        match self {
            Lit { value, ty } => Lit { value, ty: f(ty) },
            Tuple { tuple, ty } => Tuple {
                tuple: tuple.into_iter().map(|(ty, sym)| (f(ty), sym)).collect(),
                ty: f(ty),
            },
            Var { name, ty } => Var { name, ty: f(ty) },
            Wildcard { ty } => Wildcard { ty: f(ty) },
        }
    }
}

impl TypePool {
    fn typing_ast(&mut self, ast: UntypedAst) -> AST<NodeId> {
        ast.map_ty(&mut |_| self.tyvar())
    }
}

impl TypePool {
    fn typed_ast(&self, ast: AST<NodeId>) -> TypedAst {
        ast.map_ty(&mut |ty| resolve(&self.pool, ty))
    }
}

impl TyEnv {
    fn infer_ast<'b, 'r>(&'b mut self, ast: &mut AST<NodeId>) -> Result<'r, ()> {
        for mut val in ast.0.iter_mut() {
            self.infer_val(&mut val)?;
        }
        Ok(())
    }

    fn infer_val<'b, 'r>(&'b mut self, val: &mut Val<NodeId>) -> Result<'r, ()> {
        let &mut ast::Val {
            ref mut ty,
            ref rec,
            ref mut pattern,
            ref mut expr,
        } = val;
        let names = pattern.binds();
        if *rec {
            for (name, ty) in names {
                self.insert(name.clone(), ty.clone());
            }
            self.unify(expr.ty(), *ty)?;
            self.infer_expr(expr)?;
        } else {
            self.unify(expr.ty(), *ty)?;
            self.infer_expr(expr)?;
            for (name, ty) in names {
                self.insert(name.clone(), ty.clone());
            }
        }
        self.infer_pat(pattern)?;
        self.unify(*ty, pattern.ty())?;
        Ok(())
    }

    fn infer_expr<'b, 'r>(&'b mut self, expr: &mut Expr<NodeId>) -> Result<'r, ()> {
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
                self.unify(ret.ty(), *ty)?;
                self.infer_expr(ret)?;
                Ok(())
            }
            &mut BinOp {
                ref mut op,
                ref mut ty,
                ref mut l,
                ref mut r,
            } => {
                if ["+", "-", "*"].contains(&op.0.as_str()) {
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    self.unify(l.ty(), r.ty())?;
                    self.give(l.ty(), Typing::Int)
                        .or_else(|_| self.give(l.ty(), Typing::Float))?;
                    self.unify(*ty, l.ty())?;
                    Ok(())
                } else if ["=", "<>", ">", ">=", "<", "<="].contains(&op.0.as_str()) {
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    self.unify(l.ty(), r.ty())?;
                    self.give(l.ty(), Typing::Int)
                        .or_else(|_| self.give(l.ty(), Typing::Float))?;
                    self.give(*ty, Typing::Bool)?;
                    Ok(())
                } else if ["div", "mod"].contains(&op.0.as_str()) {
                    self.give(l.ty(), Typing::Int)?;
                    self.give(r.ty(), Typing::Int)?;
                    self.give(*ty, Typing::Int)?;
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    Ok(())
                } else if ["/"].contains(&op.0.as_str()) {
                    self.give(l.ty(), Typing::Float)?;
                    self.give(r.ty(), Typing::Float)?;
                    self.give(*ty, Typing::Float)?;
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    Ok(())
                } else {
                    unimplemented!()
                }
            }
            &mut Fun {
                ref mut ty,
                ref mut param,
                ref mut body,
            } => {
                let param_ty = self.pool.tyvar();
                self.insert(param.clone(), param_ty);
                self.infer_expr(body)?;
                self.give(*ty, Typing::Fun(param_ty, body.ty()))?;
                Ok(())
            }
            &mut App {
                ref mut ty,
                ref mut fun,
                ref mut arg,
            } => {
                self.infer_expr(fun)?;
                self.infer_expr(arg)?;
                self.give(fun.ty(), Typing::Fun(arg.ty(), *ty))?;
                Ok(())
            }
            &mut If {
                ref mut cond,
                ref mut ty,
                ref mut then,
                ref mut else_,
            } => {
                let bool_ty = self.pool.ty(Typing::Bool);
                self.unify(cond.ty(), bool_ty)?;
                self.infer_expr(cond)?;
                self.unify(*ty, then.ty())?;
                self.unify(then.ty(), else_.ty())?;
                self.infer_expr(then)?;
                self.infer_expr(else_)?;
                Ok(())
            }
            &mut Case {
                ref mut cond,
                ref mut ty,
                ref mut clauses,
            } => {
                self.infer_expr(cond)?;
                for &mut (ref mut pat, ref mut branch) in clauses.iter_mut() {
                    self.infer_pat(pat)?;
                    self.unify(pat.ty(), cond.ty())?;
                    self.infer_expr(branch)?;
                    self.unify(branch.ty(), *ty)?;
                }
                Ok(())
            }
            &mut Tuple {
                ref mut ty,
                ref mut tuple,
            } => {
                self.infer_tuple(tuple, *ty)?;
                Ok(())
            }
            &mut Sym {
                ref mut ty,
                ref mut name,
            } => {
                self.infer_symbol(name, *ty)?;
                Ok(())
            }
            &mut Lit {
                ref mut ty,
                ref mut value,
            } => {
                self.infer_literal(value, *ty)?;
                Ok(())
            }
        }
    }

    fn infer_symbol<'b, 'r>(&'b mut self, sym: &mut Symbol, given: NodeId) -> Result<'r, ()> {
        match self.get(&sym) {
            Some(t) => self.unify(t, given),
            None => {
                if &sym.0 == "print" {
                    let fun_ty = Typing::Fun(
                        self.pool.ty(Typing::Int),
                        self.pool.ty(Typing::Tuple(vec![])),
                    );
                    self.give(given, fun_ty)
                } else {
                    Err(TypeError::FreeVar)
                }
            }
        }
    }

    fn infer_literal<'b, 'r>(&'b mut self, lit: &mut Literal, given: NodeId) -> Result<'r, ()> {
        use crate::prim::Literal::*;
        let ty = match lit {
            &mut Int(_) => Typing::Int,
            &mut Float(_) => Typing::Float,
            &mut Bool(_) => Typing::Bool,
        };
        self.give(given, ty)?;
        Ok(())
    }

    fn infer_pat<'b, 'r>(&'b mut self, pat: &mut Pattern<NodeId>) -> Result<'r, ()> {
        use self::Pattern::*;
        match *pat {
            Lit {
                ref mut ty,
                ref mut value,
            } => {
                self.infer_literal(value, *ty)?;
            }
            Tuple {
                ref mut ty,
                ref mut tuple,
            } => {
                let tuple_ty = self.pool.ty(Typing::Tuple(
                    tuple.iter().map(|(node_id, _)| *node_id).collect(),
                ));
                self.unify(*ty, tuple_ty)?;
            }
            Wildcard { .. } | Var { .. } => (),
        };
        for (name, ty) in pat.binds() {
            self.insert(name.clone(), *ty);
        }
        Ok(())
    }

    fn infer_tuple<'b, 'r>(
        &'b mut self,
        tuple: &mut Vec<Expr<NodeId>>,
        given: NodeId,
    ) -> Result<'r, ()> {
        let mut tys = vec![self.pool.tyvar(); tuple.len()];

        for (e, t) in tuple.iter_mut().zip(tys.iter_mut()) {
            self.infer_expr(e)?;
            self.unify(e.ty(), *t)?;
        }
        let tuple_ty = self.pool.ty(Typing::Tuple(tys));
        self.unify(tuple_ty, given)?;
        Ok(())
    }

    fn unify<'b, 'r>(&'b mut self, id1: NodeId, id2: NodeId) -> Result<'r, ()> {
        self.pool.try_unify_with(id1, id2, try_unify).map(|_| ())
    }

    fn give<'b, 'r>(&'b mut self, id1: NodeId, ty: Typing) -> Result<'r, ()> {
        let id2 = self.pool.node_new(ty);
        self.unify(id1, id2)
    }
}

impl TyEnv {
    pub fn new() -> Self {
        TyEnv {
            env: HashMap::new(),
            pool: TypePool::new(),
        }
    }

    pub fn infer<'a, 'b>(&'a mut self, ast: &mut ast::AST<NodeId>) -> Result<'b, ()> {
        self.infer_ast(ast)?;
        Ok(())
    }
}

use crate::pass::Pass;
impl<'a> Pass<ast::UntypedAst, TypeError<'a>> for TyEnv {
    type Target = ast::TypedAst;

    fn trans<'b>(&'b mut self, ast: ast::UntypedAst, _: &Config) -> Result<'a, Self::Target> {
        let mut typing_ast = self.pool.typing_ast(ast);
        self.infer(&mut typing_ast)?;
        let typed_ast = self.pool.typed_ast(typing_ast);
        Ok(typed_ast)
    }
}
