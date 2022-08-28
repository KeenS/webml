use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use crate::prim::*;
use crate::unification_pool::{NodeId, UnificationPool};
use std::collections::HashMap;

type Result<T> = std::result::Result<T, TypeError>;

#[derive(Debug)]
pub struct Typer;

#[derive(Debug)]
struct TyEnv {
    env: HashMap<Symbol, NodeId>,
    symbol_table: SymbolTable,
    pool: TypePool,
}

#[derive(Debug)]
struct TypePool {
    cache: HashMap<Typing, NodeId>,
    lang_items: HashMap<LangItem, Symbol>,
    pool: UnificationPool<Typing>,
    id: Id,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Typing {
    Variable(TypeId),
    Char,
    Int,
    Real,
    Fun(NodeId, NodeId),
    Tuple(Vec<NodeId>),
    Datatype(Symbol),
    OverloadedNum,
    OverloadedNumText,
}

#[derive(Debug, Clone)]
enum TypingError {
    MisMatch { expected: Type, actual: Type },
}

fn resolve(pool: &UnificationPool<Typing>, id: NodeId) -> Type {
    conv_ty(pool, pool.value_of(id).clone())
}

fn conv_ty(pool: &UnificationPool<Typing>, ty: Typing) -> Type {
    use Typing::*;
    match ty {
        Variable(id) => Type::Variable(id),
        Char => Type::Char,
        Int => Type::Int,
        Real => Type::Real,
        Fun(param, body) => Type::Fun(
            Box::new(resolve(pool, param)),
            Box::new(resolve(pool, body)),
        ),
        Tuple(tys) => Type::Tuple(tys.into_iter().map(|ty| resolve(pool, ty)).collect()),
        Datatype(type_id) => Type::Datatype(type_id),
        OverloadedNum => Type::Int,
        OverloadedNumText => Type::Int,
    }
}

fn try_unify(
    string_ty: Symbol,
) -> impl FnMut(&mut UnificationPool<Typing>, Typing, Typing) -> std::result::Result<Typing, TypingError>
{
    move |pool: &mut UnificationPool<Typing>, t1: Typing, t2: Typing| {
        use Typing::*;
        match (t1, t2) {
            (t1, t2) if t1 == t2 => Ok(t1),
            (Int, OverloadedNum) | (OverloadedNum, Int) => Ok(Int),
            (Int, OverloadedNumText) | (OverloadedNumText, Int) => Ok(Int),
            (Char, OverloadedNumText) | (OverloadedNumText, Char) => Ok(Char),
            (Real, OverloadedNum) | (OverloadedNum, Real) => Ok(Real),
            (Real, OverloadedNumText) | (OverloadedNumText, Real) => Ok(Real),
            (OverloadedNumText, OverloadedNum) | (OverloadedNum, OverloadedNumText) => {
                Ok(OverloadedNumText)
            }
            (Datatype(sym), OverloadedNumText) | (OverloadedNumText, Datatype(sym))
                if sym == string_ty =>
            {
                Ok(Datatype(sym))
            }
            (Variable(_), ty) | (ty, Variable(_)) => Ok(ty),
            (Fun(p1, b1), Fun(p2, b2)) => {
                let p = pool.try_unify_with(p1, p2, try_unify(string_ty.clone()))?;
                let b = pool.try_unify_with(b1, b2, try_unify(string_ty.clone()))?;
                Ok(Fun(p, b))
            }
            (Tuple(tu1), Tuple(tu2)) => {
                if tu1.len() != tu2.len() {
                    Err(TypingError::MisMatch {
                        expected: conv_ty(pool, Tuple(tu1)),
                        actual: conv_ty(pool, Tuple(tu2)),
                    })
                } else {
                    let tu = tu1
                        .into_iter()
                        .zip(tu2)
                        .map(|(t1, t2)| pool.try_unify_with(t1, t2, try_unify(string_ty.clone())))
                        .collect::<std::result::Result<Vec<_>, _>>()?;
                    Ok(Tuple(tu))
                }
            }
            (t1, t2) => Err(TypingError::MisMatch {
                expected: conv_ty(pool, t1),
                actual: conv_ty(pool, t2),
            }),
        }
    }
}

impl Typer {
    fn generate_pass(
        &mut self,
        symbol_table: SymbolTable,
        lang_items: HashMap<LangItem, Symbol>,
    ) -> TyEnv {
        TyEnv::new(symbol_table, lang_items)
    }
}

impl Default for Typer {
    fn default() -> Self {
        Typer
    }
}

impl TypePool {
    fn new(lang_items: HashMap<LangItem, Symbol>) -> Self {
        let mut ret = Self {
            cache: HashMap::new(),
            lang_items,
            pool: UnificationPool::new(),
            id: Id::new(),
        };
        ret.init();
        ret
    }

    fn init(&mut self) {
        self.node_new(Typing::Char);
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

    fn ty_char(&mut self) -> NodeId {
        *self.cache.get(&Typing::Char).unwrap()
    }

    fn ty_bool(&mut self) -> NodeId {
        let b = self
            .lang_items
            .get(&LangItem::Bool)
            .expect("no lang item bool found")
            .clone();
        *self.cache.get(&Typing::Datatype(b)).unwrap()
    }

    fn ty_real(&mut self) -> NodeId {
        *self.cache.get(&Typing::Real).unwrap()
    }

    fn ty_overloaded_num(&mut self) -> NodeId {
        self.node_new(Typing::OverloadedNum)
    }

    fn ty_overloaded_num_text(&mut self) -> NodeId {
        self.node_new(Typing::OverloadedNumText)
    }

    fn node_new(&mut self, t: Typing) -> NodeId {
        let node_id = self.pool.node_new(t.clone());
        match t {
            t @ Typing::Char | t @ Typing::Int | t @ Typing::Real | t @ Typing::Datatype(_) => {
                self.cache.insert(t, node_id);
            }
            _ => (), // no cache
        }
        node_id
    }

    fn try_unify_with(
        &mut self,
        id1: NodeId,
        id2: NodeId,
        try_unify: impl FnOnce(
            &mut UnificationPool<Typing>,
            Typing,
            Typing,
        ) -> std::result::Result<Typing, TypingError>,
    ) -> std::result::Result<NodeId, TypingError> {
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
    pub fn new(symbol_table: SymbolTable, lang_items: HashMap<LangItem, Symbol>) -> Self {
        let mut ret = TyEnv {
            env: HashMap::new(),
            symbol_table,
            pool: TypePool::new(lang_items),
        };
        ret.init();

        ret
    }

    fn init(&mut self) {
        self.pool.feed_symbol_table(&self.symbol_table);
        let cnames = self
            .symbol_table
            .constructors
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for cname in cnames {
            let ty = self
                .symbol_table
                .get_datatype_of_constructor(&cname)
                .expect("internal error: typing");
            let ty = Type::Datatype(ty.clone());
            let typing = self.convert(ty);
            let node_id = self.pool.ty(typing);
            self.insert(cname, node_id);
        }
    }

    pub fn infer(&mut self, ast: &mut ast::Core<NodeId>) -> Result<()> {
        self.infer_ast(ast)?;
        Ok(())
    }

    fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    pub fn into_data(self) -> (SymbolTable, HashMap<LangItem, Symbol>) {
        (self.symbol_table, self.pool.lang_items)
    }

    fn get(&self, name: &Symbol) -> Option<NodeId> {
        self.env.get(name).cloned()
    }

    fn insert(&mut self, k: Symbol, v: NodeId) -> Option<NodeId> {
        self.env.insert(k, v)
    }

    fn polymorphic_variables(&self, ty: NodeId) -> Vec<TypeId> {
        use Typing::*;
        match self.pool.pool.value_of(ty) {
            Variable(id) => vec![*id],
            Char | Int | Real | TyAbs(_, _) | OverloadedNum | OverloadedNumText => vec![],
            Fun(param, body) => {
                let mut ret = self.polymorphic_variables(param.clone());
                ret.append(&mut self.polymorphic_variables(body.clone()));
                ret.sort();
                ret.dedup();
                ret
            }
            Tuple(tys) => {
                let mut ret = tys
                    .iter()
                    .flat_map(|ty| self.polymorphic_variables(ty.clone()))
                    .collect::<Vec<_>>();
                ret.sort();
                ret.dedup();
                ret
            }
            // currently Datatype will not be polymorphic
            Datatype(_) => vec![],
        }
    }

    fn convert(&mut self, ty: Type) -> Typing {
        match ty {
            Type::Variable(v) => Typing::Variable(v),
            Type::Char => Typing::Char,
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
    fn infer_ast(&mut self, ast: &Core<NodeId>) -> Result<()> {
        for decl in ast.0.iter() {
            self.infer_statement(decl)?;
        }
        Ok(())
    }

    fn infer_statement(&mut self, decl: &CoreDeclaration<NodeId>) -> Result<()> {
        use Declaration::*;
        match decl {
            Datatype { .. } => Ok(()),
            Val { rec, pattern, expr } => {
                let names = pattern.binds();
                if *rec {
                    for &(name, ty) in &names {
                        self.insert(name.clone(), Clone::clone(ty));
                    }
                }
                self.infer_expr(expr)?;
                self.infer_pat(pattern.span(), pattern)?;
                self.unify(expr.span(), expr.ty(), pattern.ty())?;
                if !rec {
                    for &(name, ty) in &names {
                        self.insert(name.clone(), Clone::clone(ty));
                    }
                }
                let ty = expr.ty();
                match (&self.polymorphic_variables(ty)[..], expr.is_value()) {
                    ([], _) => Ok(()),
                    (_, false) => Err(TypeError::new(
                        expr.span(),
                        TypeErrorKind::PolymorphicExpression,
                    )),
                    (_vars, true) => todo!(),
                }
            }
            LangItem { decl, .. } => self.infer_statement(decl.as_ref()),
            Local { binds, body } => {
                for b in binds {
                    self.infer_statement(b)?
                }
                for b in body {
                    self.infer_statement(b)?
                }
                Ok(())
            }
            D(d) => match *d {},
        }
    }

    fn infer_expr(&mut self, expr: &CoreExpr<NodeId>) -> Result<()> {
        use crate::ast::ExprKind::*;
        let int = self.pool.ty_int();
        let real = self.pool.ty_real();
        let bool = self.pool.ty_bool();
        let overloaded_num = self.pool.ty_overloaded_num();
        let overloaded_num_text = self.pool.ty_overloaded_num_text();
        let ty = &expr.ty;
        match &expr.inner {
            Binds { binds, ret } => {
                for decl in binds {
                    self.infer_statement(decl)?;
                }
                self.unify(ret.span(), ret.ty(), *ty)?;
                self.infer_expr(ret)?;
                Ok(())
            }
            BuiltinCall { fun, args } => {
                use BIF::*;
                match fun {
                    Add | Sub | Mul => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        self.unify(r.span(), l.ty(), r.ty())?;
                        self.unify(l.span(), l.ty(), overloaded_num)?;
                        self.unify(expr.span(), *ty, l.ty())?;
                        Ok(())
                    }
                    Eq | Neq | Gt | Ge | Lt | Le => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        self.unify(r.span(), l.ty(), r.ty())?;
                        self.unify(l.span(), l.ty(), overloaded_num_text)?;
                        self.unify(expr.span(), *ty, bool)?;
                        Ok(())
                    }
                    Div | Mod => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.unify(l.span(), l.ty(), int)?;
                        self.unify(r.span(), r.ty(), int)?;
                        self.unify(expr.span(), *ty, int)?;
                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        Ok(())
                    }
                    Divf => {
                        assert!(args.len() == 2);
                        let l = &args[0];
                        let r = &args[1];

                        self.unify(l.span(), l.ty(), real)?;
                        self.unify(r.span(), r.ty(), real)?;
                        self.unify(expr.span(), *ty, real)?;
                        self.infer_expr(l)?;
                        self.infer_expr(r)?;
                        Ok(())
                    }
                    AddInt | AddReal | SubInt | SubReal | MulInt | MulReal | ModInt | EqInt
                    | EqReal | EqChar | NeqInt | NeqReal | NeqChar | GtInt | GtReal | GtChar
                    | GeInt | GeReal | GeChar | LtInt | LtReal | LtChar | LeInt | LeReal
                    | LeChar => {
                        panic!("specified BIF will not be appear before type inference")
                    }
                }
            }
            ExternCall {
                args, argty, retty, ..
            } => {
                for (arg, argty) in args.iter().zip(argty) {
                    self.infer_expr(arg)?;
                    let argty = self.convert(argty.clone());
                    self.give(arg.span(), arg.ty(), argty)?;
                }
                let retty = self.convert(retty.clone());
                self.give(expr.span(), *ty, retty)?;
                Ok(())
            }
            Fn { param, body } => {
                let param_ty = self.pool.tyvar();
                self.insert(param.clone(), param_ty);
                self.infer_expr(body)?;
                self.give(expr.span(), *ty, Typing::Fun(param_ty, body.ty()))?;
                Ok(())
            }
            App { fun, arg } => {
                self.infer_expr(fun)?;
                self.infer_expr(arg)?;
                self.give(expr.span(), fun.ty(), Typing::Fun(arg.ty(), *ty))?;
                Ok(())
            }
            Case { cond, clauses } => {
                self.infer_expr(cond)?;
                for (pat, branch) in clauses {
                    self.infer_pat(pat.span(), pat)?;
                    self.unify(pat.span(), pat.ty(), cond.ty())?;
                    self.infer_expr(branch)?;
                    self.unify(branch.span(), branch.ty(), *ty)?;
                }
                Ok(())
            }
            Tuple { tuple } => {
                self.infer_tuple(expr.span(), tuple, *ty)?;
                Ok(())
            }
            Constructor { arg, name } => {
                self.infer_constructor(expr.span(), name, arg, *ty)?;
                Ok(())
            }
            Symbol { name } => {
                self.infer_symbol(expr.span(), name, *ty)?;
                Ok(())
            }
            Literal { value } => {
                self.infer_literal(expr.span(), value, *ty)?;
                Ok(())
            }
            D(d) => match *d {},
        }
    }

    fn infer_constructor(
        &mut self,
        span: Span,
        sym: &Symbol,
        arg: &Option<Box<CoreExpr<NodeId>>>,
        given: NodeId,
    ) -> Result<()> {
        match self.get(sym) {
            Some(ty) => {
                self.unify(span, ty, given)?;
                let arg_ty = self.symbol_table().get_argtype_of_constructor(sym);
                if let (Some(arg), Some(arg_ty)) = (arg.clone(), arg_ty.cloned()) {
                    self.infer_expr(&arg)?;
                    let arg_typing = self.convert(arg_ty);
                    let arg_ty_id = self.pool.ty(arg_typing);
                    self.unify(arg.span(), arg.ty(), arg_ty_id)?;
                }
                Ok(())
            }
            None => Err(TypeError::new(span, TypeErrorKind::FreeVar)),
        }
    }

    fn infer_symbol(&mut self, span: Span, sym: &Symbol, given: NodeId) -> Result<()> {
        match self.get(sym) {
            Some(t) => self.unify(span, t, given),
            None => Err(TypeError::new(span, TypeErrorKind::FreeVar)),
        }
    }

    fn infer_literal(&mut self, span: Span, lit: &Literal, given: NodeId) -> Result<()> {
        use crate::prim::Literal::*;
        let ty = match lit {
            Int(_) => self.pool.ty_int(),
            Real(_) => self.pool.ty_real(),
            Char(_) => self.pool.ty_char(),
        };
        self.unify(span, given, ty)?;
        Ok(())
    }

    fn infer_constant(&mut self, span: Span, _: &i64, given: NodeId) -> Result<()> {
        let ty = self.pool.ty_int();
        self.unify(span, given, ty)?;
        Ok(())
    }

    fn infer_char(&mut self, span: Span, _: &u32, given: NodeId) -> Result<()> {
        let ty = self.pool.ty_char();
        self.unify(span, given, ty)?;
        Ok(())
    }

    fn infer_pat(&mut self, span: Span, pat: &CorePattern<NodeId>) -> Result<()> {
        use self::PatternKind::*;
        let ty = &pat.ty();
        match &pat.inner {
            Constant { value } => {
                self.infer_constant(span, value, *ty)?;
            }
            Char { value } => {
                self.infer_char(span, value, *ty)?;
            }
            Constructor { arg, name } => {
                let type_name = self
                    .symbol_table()
                    .get_datatype_of_constructor(name)
                    .expect("internal error: typing")
                    .clone();
                self.give(pat.span(), *ty, Typing::Datatype(type_name.clone()))?;
                if let Some(arg) = arg {
                    self.infer_pat(arg.span(), arg)?;
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
                    self.unify(arg.span(), arg.ty(), arg_ty_id)?;
                }
            }
            Tuple { tuple } => {
                for t in tuple {
                    self.infer_pat(t.span(), t)?;
                }
                let tuple_ty = self
                    .pool
                    .ty(Typing::Tuple(tuple.iter().map(|pat| pat.ty()).collect()));
                self.unify(pat.span(), *ty, tuple_ty)?;
            }
            Wildcard { .. } | Variable { .. } => (),
            D(d) => match *d {},
        };
        for (name, ty) in pat.binds() {
            self.insert(name.clone(), *ty);
        }
        Ok(())
    }

    fn infer_tuple(
        &mut self,
        span: Span,
        tuple: &Vec<CoreExpr<NodeId>>,
        given: NodeId,
    ) -> Result<()> {
        use std::iter;
        let tys = iter::repeat_with(|| self.pool.tyvar())
            .take(tuple.len())
            .collect::<Vec<_>>();

        for (e, t) in tuple.iter().zip(tys.iter()) {
            self.infer_expr(e)?;
            self.unify(e.span(), e.ty(), *t)?;
        }
        let tuple_ty = self.pool.ty(Typing::Tuple(tys));
        self.unify(span, tuple_ty, given)?;
        Ok(())
    }

    fn unify(&mut self, span: Span, id1: NodeId, id2: NodeId) -> Result<()> {
        let string_ty = self
            .pool
            .lang_items
            .get(&LangItem::String)
            .expect("no lang item string found")
            .clone();
        self.pool
            .try_unify_with(id1, id2, try_unify(string_ty))
            .map(|_| ())
            .map_err(|e| match e {
                TypingError::MisMatch { expected, actual } => {
                    TypeError::new(span, TypeErrorKind::MisMatch { expected, actual })
                }
            })
    }

    fn give(&mut self, span: Span, id1: NodeId, ty: Typing) -> Result<()> {
        let id2 = self.pool.node_new(ty);
        self.unify(span, id1, id2)
    }
}

use crate::pass::Pass;
impl Pass<UntypedCoreContext, crate::Error> for Typer {
    type Target = TypedCoreContext;

    fn trans(
        &mut self,
        context: UntypedCoreContext,
        _: &Config,
    ) -> std::result::Result<Self::Target, crate::Error> {
        let symbol_table = context.symbol_table;
        let ast = context.ast;
        let lang_items = context.lang_items;
        let mut pass = self.generate_pass(symbol_table, lang_items);
        let mut typing_ast = pass.pool.typing_ast(ast);
        pass.infer(&mut typing_ast).map_err(crate::Error::Typing)?;
        let typed_ast = pass.pool.typed_ast(typing_ast);

        let (symbol_table, lang_items) = pass.into_data();
        Ok(Context {
            symbol_table,
            ast: typed_ast,
            lang_items,
        })
    }
}
