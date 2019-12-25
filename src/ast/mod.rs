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
pub use self::typing::TyEnv as Typer;
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
pub struct AST<Ty, DE = DerivedExpr<Ty>, DS = DerivedStatement<Ty>>(pub Vec<Statement<Ty, DE, DS>>);

pub type UntypedStatement = Statement<(), DerivedExpr<()>, DerivedStatement<()>>;
pub type CoreStatement<Ty> = Statement<Ty, Nothing, Nothing>;
pub type UntypedCoreStatement = CoreStatement<()>;
pub type TypedCoreStatement = CoreStatement<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<Ty, DE = DerivedExpr<Ty>, DS = DerivedStatement<Ty>> {
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
pub enum DerivedStatement<Ty> {
    Fun {
        name: Symbol,
        clauses: Vec<(Vec<Pattern<Ty>>, Expr<Ty>)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Nothing {}

pub type UntypedExpr = Expr<()>;
pub type CoreExpr<Ty> = Expr<Ty, Nothing, Nothing>;
pub type UntypedCoreExpr = CoreExpr<()>;
pub type TypedCoreExpr = CoreExpr<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<Ty, DE = DerivedExpr<Ty>, DS = DerivedStatement<Ty>> {
    Binds {
        ty: Ty,
        binds: Vec<Statement<Ty, DE, DS>>,
        ret: Box<Expr<Ty, DE, DS>>,
    },
    BinOp {
        op: Symbol,
        ty: Ty,
        l: Box<Expr<Ty, DE, DS>>,
        r: Box<Expr<Ty, DE, DS>>,
    },
    Fn {
        ty: Ty,
        param: Symbol,
        body: Box<Expr<Ty, DE, DS>>,
    },
    App {
        ty: Ty,
        fun: Box<Expr<Ty, DE, DS>>,
        arg: Box<Expr<Ty, DE, DS>>,
    },
    Case {
        ty: Ty,
        cond: Box<Expr<Ty, DE, DS>>,
        clauses: Vec<(Pattern<Ty>, Expr<Ty, DE, DS>)>,
    },
    Tuple {
        ty: Ty,
        tuple: Vec<Expr<Ty, DE, DS>>,
    },
    Symbol {
        ty: Ty,
        name: Symbol,
    },
    Constructor {
        ty: Ty,
        arg: Option<Box<Expr<Ty, DE, DS>>>,
        name: Symbol,
    },
    Literal {
        ty: Ty,
        value: Literal,
    },
    D(DE),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DerivedExpr<Ty> {
    If {
        ty: Ty,
        cond: Box<Expr<Ty>>,
        then: Box<Expr<Ty>>,
        else_: Box<Expr<Ty>>,
    },
}

pub type UntypedPattern = Pattern<()>;
pub type TypedPattern = Pattern<Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<Ty> {
    Constant {
        // same type as Literal::Int
        value: i64,
        ty: Ty,
    },
    Constructor {
        name: Symbol,
        arg: Option<Box<Pattern<Ty>>>,
        ty: Ty,
    },
    Tuple {
        tuple: Vec<Pattern<Ty>>,
        ty: Ty,
    },
    Variable {
        name: Symbol,
        ty: Ty,
    },
    Wildcard {
        ty: Ty,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    types: HashMap<Symbol, TypeInfo>,
    constructors: HashMap<Symbol, Symbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Variable(u64),
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

impl<Ty, DE, DS> Expr<Ty, DE, DS> {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl<Ty: Clone> CoreExpr<Ty> {
    fn ty(&self) -> Ty {
        use self::Expr::*;
        match *self {
            Binds { ref ty, .. }
            | BinOp { ref ty, .. }
            | App { ref ty, .. }
            | Case { ref ty, .. }
            | Tuple { ref ty, .. }
            | Symbol { ref ty, .. }
            | Constructor { ref ty, .. }
            | Literal { ref ty, .. }
            | Fn { ref ty, .. } => ty.clone(),
            D(ref d) => match *d {},
        }
    }
}

impl<Ty> Pattern<Ty> {
    pub fn binds(&self) -> Vec<(&Symbol, &Ty)> {
        use self::Pattern::*;
        match self {
            Constant { .. } | Wildcard { .. } => vec![],
            Variable { name, ty } => vec![(name, ty)],
            Tuple { tuple, .. } => tuple.iter().flat_map(|pat| pat.binds()).collect(),
            Constructor { arg, .. } => arg.iter().flat_map(|pat| pat.binds()).collect(),
        }
    }

    pub fn is_variable(&self) -> bool {
        use self::Pattern::*;
        match self {
            Variable { .. } => true,
            _ => false,
        }
    }

    pub fn is_constructor(&self) -> bool {
        use self::Pattern::*;
        match self {
            Constructor { .. } => true,
            _ => false,
        }
    }

    pub fn is_constant(&self) -> bool {
        use self::Pattern::*;
        match self {
            Constant { .. } => true,
            _ => false,
        }
    }

    pub fn is_tuple(&self) -> bool {
        use self::Pattern::*;
        match self {
            Tuple { .. } => true,
            _ => false,
        }
    }
}

impl<Ty: Clone> Pattern<Ty> {
    fn ty(&self) -> Ty {
        use self::Pattern::*;
        match self {
            Constant { ty, .. }
            | Variable { ty, .. }
            | Wildcard { ty }
            | Tuple { ty, .. }
            | Constructor { ty, .. } => ty.clone(),
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
