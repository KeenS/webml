mod case_simplify;
mod desugar;
mod pp;
mod rename;
mod typing;
mod util;
mod var2constructor;

pub use self::case_simplify::CaseSimplify;
pub use self::desugar::Desugar;
pub use self::rename::Rename;
pub use self::typing::Typer;
pub use self::var2constructor::VarToConstructor;
use crate::ast;
use crate::prim::*;
use nom;
pub use std::collections::HashMap;
use std::error::Error;
use std::fmt;

pub type UntypedAst = AST<()>;
pub type Core<Ty> = AST<Ty, Nothing, Nothing>;
pub type UntypedCore = Core<()>;
pub type TypedCore = Core<Type>;

#[derive(Debug, Clone, PartialEq)]
pub struct AST<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>>(
    pub Vec<Declaration<Ty, DE, DS>>,
);

pub type UntypedDeclaration = Declaration<(), DerivedExprKind<()>, DerivedDeclaration<()>>;
pub type CoreDeclaration<Ty> = Declaration<Ty, Nothing, Nothing>;
pub type UntypedCoreDeclaration = CoreDeclaration<()>;
pub type TypedCoreDeclaration = CoreDeclaration<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>> {
    Datatype {
        name: Symbol,
        constructors: Vec<(Symbol, Option<Type>)>,
    },
    Val {
        rec: bool,
        pattern: Pattern<Ty>,
        expr: Expr<Ty, DE, DS>,
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

#[derive(Debug, Clone, PartialEq)]
pub enum Nothing {}

pub type UntypedExpr = Expr<()>;
pub type CoreExpr<Ty> = Expr<Ty, Nothing, Nothing>;
pub type CoreExprKind<Ty> = ExprKind<Ty, Nothing, Nothing>;
pub type UntypedCoreExpr = CoreExpr<()>;
pub type UntypedCoreExprKind = CoreExprKind<()>;
pub type TypedCoreExpr = CoreExpr<Type>;
pub type TypedCoreExprKind = CoreExprKind<Type>;

#[derive(Debug, Clone, PartialEq)]
pub struct Annot<Ty, Inner> {
    pub ty: Ty,
    pub inner: Inner,
}

pub type Expr<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>> =
    Annot<Ty, ExprKind<Ty, DE, DS>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<Ty, DE = DerivedExprKind<Ty>, DS = DerivedDeclaration<Ty>> {
    Binds {
        binds: Vec<Declaration<Ty, DE, DS>>,
        ret: Box<Expr<Ty, DE, DS>>,
    },
    BuiltinCall {
        fun: BIF,
        args: Vec<Expr<Ty, DE, DS>>,
    },
    ExternCall {
        module: String,
        fun: String,
        args: Vec<Expr<Ty, DE, DS>>,
        argty: Vec<Type>,
        retty: Type,
    },
    Fn {
        param: Symbol,
        body: Box<Expr<Ty, DE, DS>>,
    },
    App {
        fun: Box<Expr<Ty, DE, DS>>,
        arg: Box<Expr<Ty, DE, DS>>,
    },
    Case {
        cond: Box<Expr<Ty, DE, DS>>,
        clauses: Vec<(Pattern<Ty>, Expr<Ty, DE, DS>)>,
    },
    Tuple {
        tuple: Vec<Expr<Ty, DE, DS>>,
    },
    Symbol {
        name: Symbol,
    },
    Constructor {
        arg: Option<Box<Expr<Ty, DE, DS>>>,
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
}

pub type UntypedPattern = Pattern<()>;
pub type UntypedPatternKind = PatternKind<()>;
pub type TypedPattern = Pattern<Type>;
pub type TypedPatternKind = PatternKind<Type>;

pub type Pattern<Ty> = Annot<Ty, PatternKind<Ty>>;

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind<Ty> {
    Constant {
        // same type as Literal::Int
        value: i64,
    },
    Constructor {
        name: Symbol,
        arg: Option<Box<Pattern<Ty>>>,
    },
    Tuple {
        tuple: Vec<Pattern<Ty>>,
    },
    Variable {
        name: Symbol,
    },
    Wildcard {},
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

    pub fn binds(&self) -> Vec<(&Symbol, &Ty)> {
        use self::PatternKind::*;
        match &self.inner {
            Constant { .. } | Wildcard { .. } => vec![],
            Variable { name } => vec![(name, &self.ty)],
            Tuple { tuple, .. } => tuple.iter().flat_map(|pat| pat.binds()).collect(),
            Constructor { arg, .. } => arg.iter().flat_map(|pat| pat.binds()).collect(),
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
pub enum TypeError<'a> {
    MisMatch { expected: Type, actual: Type },
    CannotInfer,
    FreeVar,
    NotFunction(ast::Expr<Type>),
    ParseError(nom::Err<(&'a str, nom::error::ErrorKind)>),
}

impl<'a> fmt::Display for TypeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl<'a> Error for TypeError<'a> {
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

impl<'a> From<nom::Err<(&'a str, nom::error::ErrorKind)>> for TypeError<'a> {
    fn from(e: nom::Err<(&'a str, nom::error::ErrorKind)>) -> Self {
        // fn conv<'b>(e: nom::Err<&'b [u8]>) -> nom::Err<&'b str> {
        //     use std::str::from_utf8;
        //     use nom::Err::*;
        //     match e {
        //         Code(e) => Code(e),
        //         Node(kind, box_err) => Node(kind, Box::new(conv(*box_err))),
        //         Position(kind, slice) => Position(kind, from_utf8(slice).unwrap()),
        //         NodePosition(kind, slice, box_err) => {
        //             NodePosition(kind, from_utf8(slice).unwrap(), Box::new(conv(*box_err)))
        //         }
        //     }
        // }

        TypeError::ParseError(e)
    }
}

pub type Result<'a, T> = ::std::result::Result<T, TypeError<'a>>;
