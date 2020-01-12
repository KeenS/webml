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
    OverloadedArith,
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
        OverloadedArith => Type::Int,
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
        (Int, OverloadedArith) | (OverloadedArith, Int) => Ok(Int),
        (Real, OverloadedArith) | (OverloadedArith, Real) => Ok(Real),
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

impl<Ty> Core<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Core<Ty2> {
        AST(self.0.into_iter().map(move |val| val.map_ty(f)).collect())
    }
}

impl<Ty> CoreDeclaration<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> CoreDeclaration<Ty2> {
        use Declaration::*;
        match self {
            Datatype { name, constructors } => Datatype { name, constructors },

            Val { pattern, expr, rec } => Val {
                rec,
                pattern: pattern.map_ty(&mut *f),
                expr: expr.map_ty(f),
            },
            D(d) => match d {},
        }
    }
}

impl<Ty> CoreExpr<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> CoreExpr<Ty2> {
        use crate::ast::ExprKind::*;
        let ty = f(self.ty);
        let inner = match self.inner {
            Binds { binds, ret } => Binds {
                binds: binds.into_iter().map(|val| val.map_ty(f)).collect(),
                ret: ret.map_ty(f).boxed(),
            },
            BuiltinCall { fun, args } => BuiltinCall {
                fun,
                args: args.into_iter().map(|arg| arg.map_ty(f)).collect(),
            },
            Fn { param, body } => Fn {
                param,
                body: body.map_ty(f).boxed(),
            },
            App { fun, arg } => App {
                fun: fun.map_ty(f).boxed(),
                arg: arg.map_ty(f).boxed(),
            },
            Case { cond, clauses } => Case {
                cond: cond.map_ty(&mut *f).boxed(),
                clauses: clauses
                    .into_iter()
                    .map(move |(pat, expr)| (pat.map_ty(&mut *f), expr.map_ty(f)))
                    .collect(),
            },
            Tuple { tuple } => Tuple {
                tuple: tuple.into_iter().map(|t| t.map_ty(f)).collect(),
            },

            Symbol { name } => Symbol { name },
            Constructor { arg, name } => Constructor {
                arg: arg.map(|a| a.map_ty(f).boxed()),
                name,
            },
            Literal { value } => Literal { value },
            D(d) => match d {},
        };
        Expr { ty, inner }
    }
}

impl<Ty> Pattern<Ty> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Pattern<Ty2> {
        use PatternKind::*;
        let ty = f(self.ty);
        let inner = match self.inner {
            Constant { value } => Constant { value },
            Constructor { name, arg } => Constructor {
                name,
                arg: arg.map(|pat| Box::new(pat.map_ty(f))),
            },
            Tuple { tuple } => Tuple {
                tuple: tuple.into_iter().map(|pat| pat.map_ty(f)).collect(),
            },
            Variable { name } => Variable { name },
            Wildcard {} => Wildcard {},
        };
        Pattern { ty, inner }
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

    fn ty_overloaded_arith(&mut self) -> NodeId {
        self.node_new(Typing::OverloadedArith)
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
    fn typing_ast(&mut self, ast: UntypedCore) -> Core<NodeId> {
        ast.map_ty(&mut |_| self.tyvar())
    }
}

impl TypePool {
    fn typed_ast(&self, ast: Core<NodeId>) -> TypedCore {
        ast.map_ty(&mut |ty| resolve(&self.pool, ty))
    }
}

impl TyEnv {
    pub fn new() -> Self {
        TyEnv {
            env: HashMap::new(),
            symbol_table: None,
            pool: TypePool::new(),
        }
    }

    pub fn init(&mut self, symbol_table: SymbolTable) {
        self.pool.feed_symbol_table(&symbol_table);
        for cname in symbol_table.constructors.keys() {
            let ty = symbol_table
                .get_datatype_of_constructor(cname)
                .expect("internal error: typing");
            let ty = Type::Datatype(ty.clone());
            let typing = self.convert(ty);
            let node_id = self.pool.ty(typing);
            self.insert(cname.clone(), node_id);
        }
        self.symbol_table = Some(symbol_table);
    }

    pub fn infer<'a, 'b>(&'a mut self, ast: &mut ast::Core<NodeId>) -> Result<'b, ()> {
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

    fn convert(&mut self, ty: Type) -> Typing {
        match ty {
            Type::Variable(v) => Typing::Variable(v),
            Type::Int => Typing::Int,
            Type::Real => Typing::Real,
            Type::Fun(arg, ret) => {
                let arg_typing = self.convert(*arg);
                let ret_typing = self.convert(*ret);
                Typing::Fun(self.pool.ty(arg_typing), self.pool.ty(ret_typing))
            }
            Type::Tuple(tuple) => Typing::Tuple(
                tuple
                    .into_iter()
                    .map(|ty| {
                        let typing = self.convert(ty);
                        self.pool.ty(typing)
                    })
                    .collect(),
            ),
            Type::Datatype(name) => Typing::Datatype(name),
        }
    }
}

impl TyEnv {
    fn infer_ast<'b, 'r>(&'b mut self, ast: &Core<NodeId>) -> Result<'r, ()> {
        for stmt in ast.0.iter() {
            self.infer_statement(&stmt)?;
        }
        Ok(())
    }

    fn infer_statement<'b, 'r>(&'b mut self, stmt: &CoreDeclaration<NodeId>) -> Result<'r, ()> {
        use Declaration::*;
        match stmt {
            Datatype { .. } => Ok(()),
            Val { rec, pattern, expr } => {
                let names = pattern.binds();
                if *rec {
                    for &(name, ty) in &names {
                        self.insert(name.clone(), ty.clone());
                    }
                }
                self.infer_expr(expr)?;
                self.infer_pat(pattern)?;
                self.unify(expr.ty(), pattern.ty())?;
                if !rec {
                    for &(name, ty) in &names {
                        self.insert(name.clone(), ty.clone());
                    }
                }
                Ok(())
            }
            D(d) => match *d {},
        }
    }

    fn infer_expr<'b, 'r>(&'b mut self, expr: &CoreExpr<NodeId>) -> Result<'r, ()> {
        use crate::ast::ExprKind::*;
        let int = self.pool.ty_int();
        let real = self.pool.ty_real();
        let bool = self.pool.ty_bool();
        let overloaded_arith = self.pool.ty_overloaded_arith();
        let ty = &expr.ty;
        match &expr.inner {
            Binds { binds, ret } => {
                for stmt in binds {
                    self.infer_statement(stmt)?;
                }
                self.unify(ret.ty(), *ty)?;
                self.infer_expr(ret)?;
                Ok(())
            }
            BuiltinCall { fun, args } => {
                use BIF::*;
                match fun {
                    Print => {
                        assert!(args.len() == 1);
                        let arg = &args[0];
                        self.infer_expr(arg)?;
                        self.unify(arg.ty(), int)?;
                        self.give(*ty, Typing::Tuple(vec![]))?;
                        Ok(())
                    }
                    Add | Sub | Mul => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        self.unify(l.ty(), r.ty())?;
                        self.unify(l.ty(), overloaded_arith)?;
                        self.unify(*ty, l.ty())?;
                        Ok(())
                    }
                    Eq | Neq | Gt | Ge | Lt | Le => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        self.unify(l.ty(), r.ty())?;
                        self.unify(l.ty(), overloaded_arith)?;
                        self.unify(*ty, bool)?;
                        Ok(())
                    }
                    Div | Mod => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.unify(l.ty(), int)?;
                        self.unify(r.ty(), int)?;
                        self.unify(*ty, int)?;
                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        Ok(())
                    }
                    Divf => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.unify(l.ty(), real)?;
                        self.unify(r.ty(), real)?;
                        self.unify(*ty, real)?;
                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        Ok(())
                    }
                }
            }
            Fn { param, body } => {
                let param_ty = self.pool.tyvar();
                self.insert(param.clone(), param_ty);
                self.infer_expr(body)?;
                self.give(*ty, Typing::Fun(param_ty, body.ty()))?;
                Ok(())
            }
            App { fun, arg } => {
                self.infer_expr(fun)?;
                self.infer_expr(arg)?;
                self.give(fun.ty(), Typing::Fun(arg.ty(), *ty))?;
                Ok(())
            }
            Case { cond, clauses } => {
                self.infer_expr(cond)?;
                for (pat, branch) in clauses {
                    self.infer_pat(pat)?;
                    self.unify(pat.ty(), cond.ty())?;
                    self.infer_expr(branch)?;
                    self.unify(branch.ty(), *ty)?;
                }
                Ok(())
            }
            Tuple { tuple } => {
                self.infer_tuple(tuple, *ty)?;
                Ok(())
            }
            Constructor { arg, name } => {
                self.infer_constructor(name, arg, *ty)?;
                Ok(())
            }
            Symbol { name } => {
                self.infer_symbol(name, *ty)?;
                Ok(())
            }
            Literal { value } => {
                self.infer_literal(value, *ty)?;
                Ok(())
            }
            D(d) => match *d {},
        }
    }

    fn infer_constructor<'b, 'r>(
        &'b mut self,
        sym: &Symbol,
        arg: &Option<Box<CoreExpr<NodeId>>>,
        given: NodeId,
    ) -> Result<'r, ()> {
        match self.get(&sym) {
            Some(ty) => {
                self.unify(ty, given)?;
                let arg_ty = self.symbol_table().get_argtype_of_constructor(sym);
                if let (Some(arg), Some(arg_ty)) = (arg.clone(), arg_ty.cloned()) {
                    self.infer_expr(&arg)?;
                    let arg_typing = self.convert(arg_ty);
                    let arg_ty_id = self.pool.ty(arg_typing);
                    self.unify(arg.ty(), arg_ty_id)?;
                }
                Ok(())
            }
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

    fn infer_constant<'b, 'r>(&'b mut self, _: &i64, given: NodeId) -> Result<'r, ()> {
        let ty = self.pool.ty_int();
        self.unify(given, ty)?;
        Ok(())
    }

    fn infer_pat<'b, 'r>(&'b mut self, pat: &Pattern<NodeId>) -> Result<'r, ()> {
        use self::PatternKind::*;
        let ty = &pat.ty();
        match &pat.inner {
            Constant { value } => {
                self.infer_constant(value, *ty)?;
            }
            Constructor { arg, name } => {
                let type_name = self
                    .symbol_table()
                    .get_datatype_of_constructor(name)
                    .expect("internal error: typing")
                    .clone();
                self.give(*ty, Typing::Datatype(type_name.clone()))?;
                if let Some(arg) = arg {
                    self.infer_pat(arg)?;
                    let arg_ty = self
                        .symbol_table()
                        .get_type(&type_name)
                        .expect("internal error: typing")
                        .constructors
                        .iter()
                        .find(|(cname, _)| cname == name)
                        .map(|(_, arg)| arg.clone())
                        .expect("internal error: typing")
                        .expect("internal error: typing");
                    let arg_typing = self.convert(arg_ty);
                    let arg_ty_id = self.pool.ty(arg_typing);
                    self.unify(arg.ty(), arg_ty_id)?;
                }
            }
            Tuple { tuple } => {
                for t in tuple {
                    self.infer_pat(t)?;
                }
                let tuple_ty = self
                    .pool
                    .ty(Typing::Tuple(tuple.iter().map(|pat| pat.ty()).collect()));
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
        tuple: &Vec<CoreExpr<NodeId>>,
        given: NodeId,
    ) -> Result<'r, ()> {
        use std::iter;
        let tys = iter::repeat_with(|| self.pool.tyvar())
            .take(tuple.len())
            .collect::<Vec<_>>();

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
impl<'a> Pass<(SymbolTable, UntypedCore), TypeError<'a>> for TyEnv {
    type Target = (SymbolTable, TypedCore);

    fn trans<'b>(
        &'b mut self,
        (symbol_table, ast): (SymbolTable, UntypedCore),
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
