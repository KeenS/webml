mod case_simplify;
mod collect_langitems;
mod desugar;
mod pp;
mod rename;
mod resolve_overload;
mod typing;
mod util;
mod var2constructor;

pub use self::case_simplify::CaseSimplify;
pub use self::collect_langitems::CollectLangItems;
pub use self::desugar::Desugar;
pub use self::rename::Rename;
pub use self::resolve_overload::ResolveOverload;
pub use self::typing::Typer;
pub use self::var2constructor::VarToConstructor;
use crate::ast;
use crate::prim::*;
pub use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::ops::Range;

pub type UntypedAst = AST<Empty>;
pub type Core<Ty> = AST<Ty, Nothing, Nothing, Nothing>;
pub type UntypedCore = Core<Empty>;
pub type TypedCore = Core<Type>;
pub type UntypedContext = Context<Empty>;
pub type CoreContext<Ty> = Context<Ty, Nothing, Nothing, Nothing>;
pub type UntypedCoreContext = CoreContext<Empty>;
pub type TypedCoreContext = CoreContext<Type>;

#[derive(Debug, Clone, PartialEq)]
pub struct Context<
    Ty,
    DE = DerivedExprKind<Ty>,
    DS = DerivedDeclaration<Ty>,
    DP = DerivedPatternKind,
> {
    pub symbol_table: SymbolTable,
    pub lang_items: HashMap<LangItem, Symbol>,
    pub ast: AST<Ty, DE, DS, DP>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>, DP = DerivedPatternKind>(
    pub Vec<Declaration<Ty, DE, DS, DP>>,
);

pub type UntypedDeclaration =
    Declaration<Empty, DerivedExprKind<Empty>, DerivedDeclaration<Empty>, DerivedPatternKind>;
pub type CoreDeclaration<Ty> = Declaration<Ty, Nothing, Nothing, Nothing>;
pub type UntypedCoreDeclaration = CoreDeclaration<Empty>;
pub type TypedCoreDeclaration = CoreDeclaration<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration<
    Ty,
    DE = DerivedExprKind<Ty>,
    DS = DerivedDeclaration<Ty>,
    DP = DerivedPatternKind,
> {
    Datatype {
        name: Symbol,
        constructors: Vec<(Symbol, Option<Type>)>,
    },
    Val {
        rec: bool,
        pattern: Pattern<Ty, DP>,
        expr: Expr<Ty, DE, DS, DP>,
    },
    LangItem {
        name: LangItem,
        decl: Box<Declaration<Ty, DE, DS, DP>>,
    },
    D(DS),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DerivedDeclaration<Ty> {
    Fun {
        name: Symbol,
        clauses: Vec<(Vec<Pattern<Ty>>, Expr<Ty>)>,
    },
    Infix {
        priority: Option<u8>,
        names: Vec<Symbol>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangItem {
    Bool,
    String,
    StringEq,
    StringNeq,
    StringGt,
    StringGe,
    StringLt,
    StringLe,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Nothing {}
#[derive(Debug, Clone, PartialEq)]
pub struct Empty {}

pub type UntypedExpr = Expr<Empty>;
pub type CoreExpr<Ty> = Expr<Ty, Nothing, Nothing, Nothing>;
pub type CoreExprKind<Ty> = ExprKind<Ty, Nothing, Nothing, Nothing>;
pub type UntypedCoreExpr = CoreExpr<Empty>;
pub type UntypedCoreExprKind = CoreExprKind<Empty>;
pub type TypedCoreExpr = CoreExpr<Type>;
pub type TypedCoreExprKind = CoreExprKind<Type>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

pub type Span = Range<Location>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<Ty, Inner> {
    pub ty: Ty,
    pub inner: Inner,
}

pub type Expr<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>, DP = DerivedPatternKind> =
    Annot<Ty, ExprKind<Ty, DE, DS, DP>>;

impl<T> Annot<Empty, T> {
    pub fn new(inner: T) -> Self {
        Self {
            ty: Empty {},
            inner,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<
    Ty,
    DE = DerivedExprKind<Ty>,
    DS = DerivedDeclaration<Ty>,
    DP = DerivedPatternKind,
> {
    Binds {
        binds: Vec<Declaration<Ty, DE, DS, DP>>,
        ret: Box<Expr<Ty, DE, DS, DP>>,
    },
    BuiltinCall {
        fun: BIF,
        args: Vec<Expr<Ty, DE, DS, DP>>,
    },
    ExternCall {
        module: String,
        fun: String,
        args: Vec<Expr<Ty, DE, DS, DP>>,
        argty: Vec<Type>,
        retty: Type,
    },
    Fn {
        param: Symbol,
        body: Box<Expr<Ty, DE, DS, DP>>,
    },
    App {
        fun: Box<Expr<Ty, DE, DS, DP>>,
        arg: Box<Expr<Ty, DE, DS, DP>>,
    },
    Case {
        cond: Box<Expr<Ty, DE, DS, DP>>,
        clauses: Vec<(Pattern<Ty, DP>, Expr<Ty, DE, DS, DP>)>,
    },
    Tuple {
        tuple: Vec<Expr<Ty, DE, DS, DP>>,
    },
    Symbol {
        name: Symbol,
    },
    Constructor {
        arg: Option<Box<Expr<Ty, DE, DS, DP>>>,
        name: Symbol,
    },
    Literal {
        value: Literal,
    },
    D(DE),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DerivedExprKind<Ty> {
    If {
        cond: Box<Expr<Ty>>,
        then: Box<Expr<Ty>>,
        else_: Box<Expr<Ty>>,
    },
    String {
        value: Vec<u32>,
    },
}

pub type CorePattern<Ty> = Pattern<Ty, Nothing>;
pub type CorePatternKind<Ty> = PatternKind<Ty, Nothing>;
pub type UntypedPattern = Pattern<Empty>;
pub type UntypedPatternKind = PatternKind<Empty>;
pub type UntypedCorePattern = CorePattern<Empty>;
pub type UntypedCorePatternKind = CorePatternKind<Empty>;
pub type TypedCorePattern = CorePattern<Type>;
pub type TypedCorePatternKind = CorePatternKind<Type>;

pub type Pattern<Ty, DP = DerivedPatternKind> = Annot<Ty, PatternKind<Ty, DP>>;

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind<Ty, DP = DerivedPatternKind> {
    Constant {
        // same type as Literal::Int
        value: i64,
    },
    Char {
        // same type as Literal::Char
        value: u32,
    },
    Constructor {
        name: Symbol,
        arg: Option<Box<Pattern<Ty, DP>>>,
    },
    Tuple {
        tuple: Vec<Pattern<Ty, DP>>,
    },
    Variable {
        name: Symbol,
    },
    Wildcard {},
    D(DP),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DerivedPatternKind {
    String { value: Vec<u32> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub types: HashMap<Symbol, TypeInfo>,
    pub constructors: HashMap<Symbol, Symbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Variable(u64),
    Char,
    Int,
    Real,
    Fun(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Datatype(Symbol),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    pub constructors: Vec<(Symbol, Option<Type>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BIF {
    // overloaded versions
    Add,
    Sub,
    Mul,
    Div,
    Divf,
    Mod,
    Eq,
    Neq,
    Gt,
    Ge,
    Lt,
    Le,
    // specified versions
    AddInt,
    AddReal,
    SubInt,
    SubReal,
    MulInt,
    MulReal,
    ModInt,
    EqInt,
    EqReal,
    EqChar,
    NeqInt,
    NeqReal,
    NeqChar,
    GtInt,
    GtReal,
    GtChar,
    GeInt,
    GeReal,
    GeChar,
    LtInt,
    LtReal,
    LtChar,
    LeInt,
    LeReal,
    LeChar,
}

impl<Ty, Inner> Annot<Ty, Inner> {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl<Ty: Clone, Inner> Annot<Ty, Inner> {
    fn ty(&self) -> Ty {
        self.ty.clone()
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
            LangItem { name, decl } => LangItem {
                name,
                decl: Box::new(decl.map_ty(&mut *f)),
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
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => ExternCall {
                module,
                fun,
                args: args.into_iter().map(|arg| arg.map_ty(f)).collect(),
                retty: retty,
                argty: argty,
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

impl<Ty, DP> Pattern<Ty, DP> {
    fn map_ty<Ty2>(self, f: &mut dyn FnMut(Ty) -> Ty2) -> Pattern<Ty2, DP> {
        use PatternKind::*;
        let ty = f(self.ty);
        let inner = match self.inner {
            Constant { value } => Constant { value },
            Char { value } => Char { value },
            Constructor { name, arg } => Constructor {
                name,
                arg: arg.map(|pat| Box::new(pat.map_ty(f))),
            },
            Tuple { tuple } => Tuple {
                tuple: tuple.into_iter().map(|pat| pat.map_ty(f)).collect(),
            },
            Variable { name } => Variable { name },
            Wildcard {} => Wildcard {},
            D(d) => D(d),
        };
        Pattern { ty, inner }
    }

    pub fn binds(&self) -> Vec<(&Symbol, &Ty)> {
        use self::PatternKind::*;
        match &self.inner {
            Constant { .. } | Char { .. } | Wildcard { .. } => vec![],
            Variable { name } => vec![(name, &self.ty)],
            Tuple { tuple, .. } => tuple.iter().flat_map(|pat| pat.binds()).collect(),
            Constructor { arg, .. } => arg.iter().flat_map(|pat| pat.binds()).collect(),
            D(_) => vec![],
        }
    }

    pub fn is_variable(&self) -> bool {
        use self::PatternKind::*;
        match &self.inner {
            Variable { .. } => true,
            _ => false,
        }
    }

    pub fn is_constructor(&self) -> bool {
        use self::PatternKind::*;
        match &self.inner {
            Constructor { .. } => true,
            _ => false,
        }
    }

    pub fn is_constant(&self) -> bool {
        use self::PatternKind::*;
        match &self.inner {
            Constant { .. } => true,
            _ => false,
        }
    }

    pub fn is_char(&self) -> bool {
        use self::PatternKind::*;
        match &self.inner {
            Char { .. } => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        use self::PatternKind::*;
        match &self.inner {
            Tuple { .. } => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn fun(param: Type, ret: Type) -> Type {
        Type::Fun(Box::new(param), Box::new(ret))
    }
    pub fn unit() -> Type {
        Type::Tuple(Vec::new())
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            constructors: HashMap::new(),
        }
    }

    pub fn register_type(&mut self, name: Symbol, info: TypeInfo) {
        for (cname, _) in &info.constructors {
            self.constructors.insert(cname.clone(), name.clone());
        }
        self.types.insert(name, info);
    }

    pub fn get_type(&self, name: &Symbol) -> Option<&TypeInfo> {
        self.types.get(&name)
    }

    pub fn get_datatype_of_constructor(&self, name: &Symbol) -> Option<&Symbol> {
        self.constructors.get(name)
    }

    pub fn get_argtype_of_constructor(&self, name: &Symbol) -> Option<&Type> {
        let type_name = self.constructors.get(name)?;
        let param_ty = self
            .get_type(type_name)?
            .constructors
            .iter()
            .find(|(cname, _)| cname == name)
            .map(|(_, param)| param)?;
        param_ty.as_ref()
    }

    pub fn constructor_to_id(&self, name: &Symbol) -> u32 {
        let typename = self
            .get_datatype_of_constructor(name)
            .expect("internal error: type not found for construcor");
        let type_info = self
            .get_type(typename)
            .expect("internal error: type not found");
        type_info
            .constructors
            .iter()
            .position(|(cname, _)| cname == name)
            .expect("internal error: constructor is not a memberof its ADT") as u32
    }
}

#[derive(Debug)]
pub enum TypeError {
    MisMatch { expected: Type, actual: Type },
    CannotInfer,
    FreeVar,
    NotFunction(ast::Expr<Type>),
    ParseError(String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Error for TypeError {
    fn description(&self) -> &str {
        use self::TypeError::*;
        match self {
            &MisMatch { .. } => "type mismatches against expected type",
            &CannotInfer => "cannot infer the type",
            &FreeVar => "free variable is found",
            &NotFunction(_) => "not a function",
            &ParseError(_) => "parse error",
        }
    }
}

pub type Result<T> = ::std::result::Result<T, TypeError>;
