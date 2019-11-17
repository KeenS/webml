use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use crate::prim::*;
use crate::unification_pool::{NodeId, UnificationPool};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TyEnv {
    env: HashMap<Symbol, NodeId>,
    symbol_table: Option<SymbolTable>,
    pool: TypePool,
}

#[derive(Debug)]
struct TypePool {
    cache: HashMap<Typing, NodeId>,
    pool: UnificationPool<Typing>,
    id: Id,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Typing {
    Variable(u64),
    Int,
    Real,
    Fun(NodeId, NodeId),
    Tuple(Vec<NodeId>),
    Datatype(Symbol),
}

fn resolve(pool: &UnificationPool<Typing>, id: NodeId) -> Type {
    conv_ty(pool, pool.value_of(id).clone())
}

fn conv_ty(pool: &UnificationPool<Typing>, ty: Typing) -> Type {
    use Typing::*;
    match ty {
        Variable(id) => Type::Variable(id),
        Int => Type::Int,
        Real => Type::Real,
        Fun(param, body) => Type::Fun(
            Box::new(resolve(pool, param)),
            Box::new(resolve(pool, body)),
        ),
        Tuple(tys) => Type::Tuple(tys.into_iter().map(|ty| resolve(pool, ty)).collect()),
        Datatype(type_id) => Type::Datatype(type_id),
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
        (Variable(_), ty) | (ty, Variable(_)) => Ok(ty),
        (Fun(p1, b1), Fun(p2, b2)) => {
            let p = pool.try_unify_with(p1, p2, try_unify)?;
            let b = pool.try_unify_with(b1, b2, try_unify)?;
            Ok(Fun(p, b))
        }
        (Tuple(tu1), Tuple(tu2)) => {
            if tu1.len() != tu2.len() {
                Err(TypeError::MisMatch {
                    expected: conv_ty(pool, Tuple(tu1)),
                    actual: conv_ty(pool, Tuple(tu2)),
                })
            } else {
                let tu = tu1
                    .into_iter()
                    .zip(tu2)
                    .map(|(t1, t2)| pool.try_unify_with(t1, t2, try_unify))
                    .collect::<Result<'_, Vec<_>>>()?;
                Ok(Tuple(tu))
            }
        }
        (t1, t2) => Err(TypeError::MisMatch {
            expected: conv_ty(pool, t1),
            actual: conv_ty(pool, t2),
        }),
    }
}

impl<Ty> AST<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> AST<Ty2> {
        AST(self.0.into_iter().map(move |val| val.map_ty(f)).collect())
    }
}

impl<Ty> Statement<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Statement<Ty2> {
        use Statement::*;
        match self {
            Datatype { name, constructors } => Datatype { name, constructors },

            Fun { name, params, expr } => Fun {
                name,
                params: params
                    .into_iter()
                    .map(|(ty, param)| (f(ty), param))
                    .collect(),
                expr: expr.map_ty(f),
            },
            Val { pattern, expr } => Val {
                pattern: pattern.map_ty(&mut *f),
                expr: expr.map_ty(f),
            },
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
            Fn { param, ty, body } => Fn {
                param,
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

            Symbol { ty, name } => Symbol { ty: f(ty), name },
            Constructor { ty, name } => Constructor { ty: f(ty), name },
            Literal { ty, value } => Literal { ty: f(ty), value },
        }
    }
}

impl<Ty> Pattern<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Pattern<Ty2> {
        use Pattern::*;
        match self {
            Literal { value, ty } => Literal { value, ty: f(ty) },
            Constructor { name, ty } => Constructor { name, ty: f(ty) },
            Tuple { tuple, ty } => Tuple {
                tuple: tuple.into_iter().map(|(ty, sym)| (f(ty), sym)).collect(),
                ty: f(ty),
            },
            Variable { name, ty } => Variable { name, ty: f(ty) },
            Wildcard { ty } => Wildcard { ty: f(ty) },
        }
    }
}

impl TypePool {
    fn new() -> Self {
        let mut ret = Self {
            cache: HashMap::new(),
            pool: UnificationPool::new(),
            id: Id::new(),
        };
        ret.init();
        ret
    }

    fn init(&mut self) {
        self.node_new(Typing::Int);
        self.node_new(Typing::Real);
    }

    fn feed_symbol_table(&mut self, symbol_table: &SymbolTable) {
        for typename in symbol_table.types.keys() {
            self.node_new(Typing::Datatype(typename.clone()));
        }
    }

    fn tyvar(&mut self) -> NodeId {
        self.pool.node_new(Typing::Variable(self.id.next()))
    }

    fn ty(&mut self, ty: Typing) -> NodeId {
        self.pool.node_new(ty)
    }

    fn ty_int(&mut self) -> NodeId {
        *self.cache.get(&Typing::Int).unwrap()
    }

    fn ty_bool(&mut self) -> NodeId {
        *self
            .cache
            .get(&Typing::Datatype(Symbol::new("bool")))
            .unwrap()
    }

    fn ty_real(&mut self) -> NodeId {
        *self.cache.get(&Typing::Real).unwrap()
    }

    fn node_new(&mut self, t: Typing) -> NodeId {
        let node_id = self.pool.node_new(t.clone());
        match t {
            t @ Typing::Int | t @ Typing::Real | t @ Typing::Datatype(_) => {
                self.cache.insert(t, node_id);
            }
            _ => (), // no cache
        }
        node_id
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
    pub fn new() -> Self {
        let mut ret = TyEnv {
            env: HashMap::new(),
            symbol_table: None,
            pool: TypePool::new(),
        };
        let fun_ty = Typing::Fun(ret.pool.ty_int(), ret.pool.ty(Typing::Tuple(vec![])));
        let node_id = ret.pool.ty(fun_ty);
        ret.insert(Symbol::new("print"), node_id);
        ret
    }

    pub fn init(&mut self, symbol_table: SymbolTable) {
        self.pool.feed_symbol_table(&symbol_table);
        for (cname, name) in &symbol_table.constructors {
            let ty = self.pool.ty(Typing::Datatype(name.clone()));
            self.insert(cname.clone(), ty);
        }
        self.symbol_table = Some(symbol_table);
    }

    pub fn infer<'a, 'b>(&'a mut self, ast: &mut ast::AST<NodeId>) -> Result<'b, ()> {
        self.infer_ast(ast)?;
        Ok(())
    }

    fn symbol_table(&self) -> &SymbolTable {
        self.symbol_table.as_ref().unwrap()
    }

    pub fn generate_symbol_table(&mut self) -> SymbolTable {
        self.symbol_table.take().unwrap()
    }

    fn get(&self, name: &Symbol) -> Option<NodeId> {
        self.env.get(name).cloned()
    }

    fn insert(&mut self, k: Symbol, v: NodeId) -> Option<NodeId> {
        self.env.insert(k, v)
    }
}

impl TyEnv {
    fn infer_ast<'b, 'r>(&'b mut self, ast: &AST<NodeId>) -> Result<'r, ()> {
        for stmt in ast.0.iter() {
            self.infer_statement(&stmt)?;
        }
        Ok(())
    }

    fn infer_statement<'b, 'r>(&'b mut self, stmt: &Statement<NodeId>) -> Result<'r, ()> {
        use Statement::*;
        match stmt {
            Datatype { .. } => Ok(()),
            Val { pattern, expr } => {
                let names = pattern.binds();
                self.infer_expr(expr)?;
                self.infer_pat(pattern)?;
                self.unify(expr.ty(), pattern.ty())?;
                for (name, ty) in names {
                    self.insert(name.clone(), ty.clone());
                }
                Ok(())
            }
            Fun { name, params, expr } => {
                for (ty, param) in params {
                    self.insert(param.clone(), ty.clone());
                }
                let params_ty = params.iter().map(|(ty, _)| *ty);
                let body_ty = expr.ty();
                let fun_ty = params_ty.rev().fold(body_ty, |body_ty, param_ty| {
                    self.pool.ty(Typing::Fun(param_ty, body_ty))
                });
                self.insert(name.clone(), fun_ty);
                // self.infer_pat(pattern)?;
                self.infer_expr(expr)?;
                Ok(())
            }
        }
    }

    fn infer_expr<'b, 'r>(&'b mut self, expr: &Expr<NodeId>) -> Result<'r, ()> {
        use crate::ast::Expr::*;
        let int = self.pool.ty_int();
        let real = self.pool.ty_real();
        let bool = self.pool.ty_bool();
        match expr {
            Binds { ty, binds, ret } => {
                for stmt in binds {
                    self.infer_statement(stmt)?;
                }
                self.unify(ret.ty(), *ty)?;
                self.infer_expr(ret)?;
                Ok(())
            }
            BinOp { op, ty, l, r } => {
                if ["+", "-", "*"].contains(&op.0.as_str()) {
                    // TODO: support these cases
                    // fun add x y = x + y + 1.0
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    self.unify(l.ty(), r.ty())?;
                    self.unify(l.ty(), int)
                        .or_else(|_| self.unify(l.ty(), real))?;
                    self.unify(*ty, l.ty())?;
                    Ok(())
                } else if ["=", "<>", ">", ">=", "<", "<="].contains(&op.0.as_str()) {
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    self.unify(l.ty(), r.ty())?;
                    self.unify(l.ty(), int)
                        .or_else(|_| self.unify(l.ty(), real))?;
                    self.unify(*ty, bool)?;
                    Ok(())
                } else if ["div", "mod"].contains(&op.0.as_str()) {
                    self.unify(l.ty(), int)?;
                    self.unify(r.ty(), int)?;
                    self.unify(*ty, int)?;
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    Ok(())
                } else if ["/"].contains(&op.0.as_str()) {
                    self.unify(l.ty(), real)?;
                    self.unify(r.ty(), real)?;
                    self.unify(*ty, real)?;
                    self.infer_expr(l)?;
                    self.infer_expr(r)?;
                    Ok(())
                } else {
                    unimplemented!()
                }
            }
            Fn { ty, param, body } => {
                let param_ty = self.pool.tyvar();
                self.insert(param.clone(), param_ty);
                self.infer_expr(body)?;
                self.give(*ty, Typing::Fun(param_ty, body.ty()))?;
                Ok(())
            }
            App { ty, fun, arg } => {
                self.infer_expr(fun)?;
                self.infer_expr(arg)?;
                self.give(fun.ty(), Typing::Fun(arg.ty(), *ty))?;
                Ok(())
            }
            If {
                cond,
                ty,
                then,
                else_,
            } => {
                self.unify(cond.ty(), bool)?;
                self.infer_expr(cond)?;
                self.unify(*ty, then.ty())?;
                self.unify(then.ty(), else_.ty())?;
                self.infer_expr(then)?;
                self.infer_expr(else_)?;
                Ok(())
            }
            Case { cond, ty, clauses } => {
                self.infer_expr(cond)?;
                for (pat, branch) in clauses {
                    self.infer_pat(pat)?;
                    self.unify(pat.ty(), cond.ty())?;
                    self.infer_expr(branch)?;
                    self.unify(branch.ty(), *ty)?;
                }
                Ok(())
            }
            Tuple { ty, tuple } => {
                self.infer_tuple(tuple, *ty)?;
                Ok(())
            }
            Constructor { ty, name } => {
                self.infer_constructor(name, *ty)?;
                Ok(())
            }
            Symbol { ty, name } => {
                self.infer_symbol(name, *ty)?;
                Ok(())
            }
            Literal { ty, value } => {
                self.infer_literal(value, *ty)?;
                Ok(())
            }
        }
    }

    fn infer_constructor<'b, 'r>(&'b mut self, sym: &Symbol, given: NodeId) -> Result<'r, ()> {
        match self.get(&sym) {
            Some(t) => self.unify(t, given),
            None => Err(TypeError::FreeVar),
        }
    }

    fn infer_symbol<'b, 'r>(&'b mut self, sym: &Symbol, given: NodeId) -> Result<'r, ()> {
        match self.get(&sym) {
            Some(t) => self.unify(t, given),
            None => Err(TypeError::FreeVar),
        }
    }

    fn infer_literal<'b, 'r>(&'b mut self, lit: &Literal, given: NodeId) -> Result<'r, ()> {
        use crate::prim::Literal::*;
        let ty = match lit {
            Int(_) => self.pool.ty_int(),
            Real(_) => self.pool.ty_real(),
        };
        self.unify(given, ty)?;
        Ok(())
    }

    fn infer_pat<'b, 'r>(&'b mut self, pat: &Pattern<NodeId>) -> Result<'r, ()> {
        use self::Pattern::*;
        match pat {
            Literal { ty, value } => {
                self.infer_literal(value, *ty)?;
            }
            Constructor { ty, name } => {
                let type_name = self
                    .symbol_table()
                    .get_datatype_of_constructor(name)
                    .expect("internal error: typing")
                    .clone();
                self.give(*ty, Typing::Datatype(type_name))?;
            }
            Tuple { ty, tuple } => {
                let tuple_ty = self.pool.ty(Typing::Tuple(
                    tuple.iter().map(|(node_id, _)| *node_id).collect(),
                ));
                self.unify(*ty, tuple_ty)?;
            }
            Wildcard { .. } | Variable { .. } => (),
        };
        for (name, ty) in pat.binds() {
            self.insert(name.clone(), *ty);
        }
        Ok(())
    }

    fn infer_tuple<'b, 'r>(
        &'b mut self,
        tuple: &Vec<Expr<NodeId>>,
        given: NodeId,
    ) -> Result<'r, ()> {
        let tys = vec![self.pool.tyvar(); tuple.len()];

        for (e, t) in tuple.iter().zip(tys.iter()) {
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

use crate::pass::Pass;
impl<'a> Pass<(SymbolTable, UntypedAst), TypeError<'a>> for TyEnv {
    type Target = (SymbolTable, TypedAst);

    fn trans<'b>(
        &'b mut self,
        (symbol_table, ast): (SymbolTable, UntypedAst),
        _: &Config,
    ) -> Result<'a, Self::Target> {
        self.init(symbol_table);
        let mut typing_ast = self.pool.typing_ast(ast);
        self.infer(&mut typing_ast)?;
        let typed_ast = self.pool.typed_ast(typing_ast);

        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, typed_ast))
    }
}
