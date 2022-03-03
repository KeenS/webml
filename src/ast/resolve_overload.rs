use crate::ast::*;
use crate::prim::*;
use crate::Config;
use std::collections::HashMap;
use util::Traverse;

#[derive(Debug)]
pub struct ResolveOverload {}

impl ResolveOverload {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug)]
struct Resolver {
    lang_items: HashMap<LangItem, Symbol>,
    symbol_table: SymbolTable,
}

impl Resolver {
    fn new(symbol_table: SymbolTable, lang_items: HashMap<LangItem, Symbol>) -> Self {
        Self {
            symbol_table,
            lang_items,
        }
    }
}

impl Traverse<Type> for Resolver {
    fn traverse_builtincall(&mut self, bif: &mut BIF, args: &mut Vec<CoreExpr<Type>>) {
        use self::BIF::*;
        assert_eq!(args.len(), 2);
        let types = args.into_iter().map(|e| e.ty()).collect::<Vec<_>>();
        assert_eq!(types[0], types[1]);
        match (*bif, &types[0]) {
            (Add, &Type::Int) => *bif = AddInt,
            (Add, &Type::Real) => *bif = AddReal,
            (Add, _) => panic!("unknown overloaded tpye for add"),
            (Sub, &Type::Int) => *bif = SubInt,
            (Sub, &Type::Real) => *bif = SubReal,
            (Sub, _) => panic!("unknown overloaded tpye for sub"),
            (Mul, &Type::Int) => *bif = MulInt,
            (Mul, &Type::Real) => *bif = MulReal,
            (Mul, _) => panic!("unknown overloaded tpye for mul"),
            (Div, &Type::Int) => *bif = Div,
            (Div, _) => panic!("unknown overloaded tpye for div"),
            (Divf, &Type::Real) => *bif = Divf,
            (Divf, _) => panic!("unknown overloaded tpye for divf"),
            (Mod, &Type::Int) => *bif = ModInt,
            (Mod, _) => panic!("unknown overloaded tpye for mod"),
            (Eq, &Type::Int) => *bif = EqInt,
            (Eq, &Type::Real) => *bif = EqReal,
            (Eq, &Type::Char) => *bif = EqChar,
            (Eq, _) => panic!("unknown overloaded tpye for eq"),
            (Neq, &Type::Int) => *bif = NeqInt,
            (Neq, &Type::Real) => *bif = NeqReal,
            (Neq, &Type::Char) => *bif = NeqChar,
            (Neq, _) => panic!("unknown overloaded tpye for neq"),
            (Gt, &Type::Int) => *bif = GtInt,
            (Gt, &Type::Real) => *bif = GtReal,
            (Gt, &Type::Char) => *bif = GtChar,
            (Gt, _) => panic!("unknown overloaded tpye for gt"),
            (Ge, &Type::Int) => *bif = GeInt,
            (Ge, &Type::Real) => *bif = GeReal,
            (Ge, &Type::Char) => *bif = GeChar,
            (Ge, _) => panic!("unknown overloaded tpye for ge"),
            (Lt, &Type::Int) => *bif = LtInt,
            (Lt, &Type::Real) => *bif = LtReal,
            (Lt, &Type::Char) => *bif = LtChar,
            (Lt, _) => panic!("unknown overloaded tpye for lt"),
            (Le, &Type::Int) => *bif = LeInt,
            (Le, &Type::Real) => *bif = LeReal,
            (Le, &Type::Char) => *bif = LeChar,
            (Le, _) => panic!("unknown overloaded tpye for le"),
            (AddInt, _)
            | (AddReal, _)
            | (SubInt, _)
            | (SubReal, _)
            | (MulInt, _)
            | (MulReal, _)
            | (ModInt, _)
            | (EqInt, _)
            | (EqReal, _)
            | (EqChar, _)
            | (NeqInt, _)
            | (NeqReal, _)
            | (NeqChar, _)
            | (GtInt, _)
            | (GtReal, _)
            | (GtChar, _)
            | (GeInt, _)
            | (GeReal, _)
            | (GeChar, _)
            | (LtInt, _)
            | (LtReal, _)
            | (LtChar, _)
            | (LeInt, _)
            | (LeReal, _)
            | (LeChar, _) => panic!("specified BIF must not appear before overload resolve"),
        }

        for arg in args {
            self.traverse_expr(arg)
        }
    }
}

use crate::pass::Pass;
impl Pass<TypedCoreContext, TypeError> for ResolveOverload {
    type Target = TypedCoreContext;

    fn trans(&mut self, context: TypedCoreContext, _: &Config) -> Result<Self::Target> {
        let symbol_table = context.symbol_table;
        let mut ast = context.ast;
        let lang_items = context.lang_items;

        let mut resolver = Resolver::new(symbol_table, lang_items);
        resolver.traverse_ast(&mut ast);
        let symbol_table = resolver.symbol_table;
        let lang_items = resolver.lang_items;

        Ok(Context {
            symbol_table,
            ast,
            lang_items,
        })
    }
}
