pub mod case_check;
mod pp;
pub mod rename;
pub mod typing;
mod util;
pub mod var2constructor;

pub use self::case_check::CaseCheck;
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
pub type TypedAst = AST<Type>;

#[derive(Debug, Clone, PartialEq)]
pub struct AST<Ty>(pub Vec<Statement<Ty>>);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<Ty> {
    Datatype {
        name: Symbol,
        constructors: Vec<Symbol>,
    },
    Val {
        pattern: Pattern<Ty>,
        expr: Expr<Ty>,
    },
    Fun {
        name: Symbol,
        // TODO: let it patterns
        params: Vec<(Ty, Symbol)>,
        expr: Expr<Ty>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<Ty> {
    Binds {
        ty: Ty,
        binds: Vec<Statement<Ty>>,
        ret: Box<Expr<Ty>>,
    },
    BinOp {
        op: Symbol,
        ty: Ty,
        l: Box<Expr<Ty>>,
        r: Box<Expr<Ty>>,
    },
    Fn {
        ty: Ty,
        param: Symbol,
        body: Box<Expr<Ty>>,
    },
    App {
        ty: Ty,
        fun: Box<Expr<Ty>>,
        arg: Box<Expr<Ty>>,
    },
    If {
        ty: Ty,
        cond: Box<Expr<Ty>>,
        then: Box<Expr<Ty>>,
        else_: Box<Expr<Ty>>,
    },
    Case {
        ty: Ty,
        cond: Box<Expr<Ty>>,
        clauses: Vec<(Pattern<Ty>, Expr<Ty>)>,
    },
    Tuple {
        ty: Ty,
        tuple: Vec<Expr<Ty>>,
    },
    Symbol {
        ty: Ty,
        name: Symbol,
    },
    Constructor {
        ty: Ty,
        name: Symbol,
    },
    Literal {
        ty: Ty,
        value: Literal,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<Ty> {
    Literal { value: Literal, ty: Ty },
    Constructor { name: Symbol, ty: Ty },
    // having redundant types for now
    Tuple { tuple: Vec<(Ty, Symbol)>, ty: Ty },
    Variable { name: Symbol, ty: Ty },
    Wildcard { ty: Ty },
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
    constructors: Vec<Symbol>,
}

impl<Ty> Expr<Ty> {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl<Ty: Clone> Expr<Ty> {
    fn ty(&self) -> Ty {
        use self::Expr::*;
        match *self {
            Binds { ref ty, .. }
            | BinOp { ref ty, .. }
            | App { ref ty, .. }
            | If { ref ty, .. }
            | Case { ref ty, .. }
            | Tuple { ref ty, .. }
            | Symbol { ref ty, .. }
            | Constructor { ref ty, .. }
            | Literal { ref ty, .. }
            | Fn { ref ty, .. } => ty.clone(),
        }
    }
}

impl<Ty> Pattern<Ty> {
    pub fn binds(&self) -> Vec<(&Symbol, &Ty)> {
        use self::Pattern::*;
        match *self {
            Literal { .. } | Wildcard { .. } => vec![],
            Variable { ref name, ref ty } => vec![(name, ty)],
            Tuple { ref tuple, .. } => tuple.iter().map(|&(ref ty, ref sym)| (sym, ty)).collect(),
            Constructor { .. } => vec![],
        }
    }
}

impl<Ty: Clone> Pattern<Ty> {
    fn ty(&self) -> Ty {
        use self::Pattern::*;
        match *self {
            Literal { ref ty, .. }
            | Variable { ref ty, .. }
            | Wildcard { ref ty }
            | Tuple { ref ty, .. }
            | Constructor { ref ty, .. } => ty.clone(),
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
        for cname in &info.constructors {
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
}

#[derive(Debug)]
pub enum TypeError<'a> {
    MisMatch { expected: Type, actual: Type },
    CannotInfer,
    FreeVar,
    NotFunction(ast::Expr<Type>),
    ParseError(nom::Err<&'a str>),
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

impl<'a> From<nom::Err<&'a str>> for TypeError<'a> {
    fn from(e: nom::Err<&'a str>) -> Self {
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
