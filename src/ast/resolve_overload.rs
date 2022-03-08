use crate::ast::*;
use crate::prim::*;
use crate::Config;
use std::collections::HashMap;
use util::Transform;

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

impl Transform<Type> for Resolver {
    fn transform_builtincall(
        &mut self,
        span: Span,
        mut fun: BIF,
        args: Vec<TypedCoreExpr>,
    ) -> TypedCoreExprKind {
        use self::BIF::*;
        assert_eq!(args.len(), 2);
        let types = args.iter().map(|e| e.ty()).collect::<Vec<_>>();
        assert_eq!(types[0], types[1]);
        let str_ty = self
            .lang_items
            .get(&LangItem::String)
            .expect("lang item string not found");
        let bool_ty = self
            .lang_items
            .get(&LangItem::Bool)
            .expect("lang item bool not found");
        let bool_ty = Type::Datatype(bool_ty.clone());
        let str_ty = Type::Datatype(str_ty.clone());
        let str_binop_ty = Type::fun(Type::Tuple(vec![str_ty.clone(), str_ty.clone()]), bool_ty);
        let arg_ty = Type::Tuple(types.clone());
        let args = args
            .clone()
            .into_iter()
            .map(|arg| self.transform_expr(arg))
            .collect::<Vec<_>>();
        let args2 = args.clone();

        let gen_str_op = move |item| {
            let str_eq = self
                .lang_items
                .get(&item)
                .expect("lang item string not found")
                .clone();
            let kind = ExprKind::App {
                fun: Expr {
                    ty: str_binop_ty,
                    span: span.clone(),
                    inner: ExprKind::Symbol { name: str_eq },
                }
                .boxed(),
                arg: Expr {
                    ty: arg_ty,
                    span: span.clone(),
                    inner: ExprKind::Tuple { tuple: args2 },
                }
                .boxed(),
            };
            return kind;
        };

        match (fun, &types[0]) {
            (Add, &Type::Int) => fun = AddInt,
            (Add, &Type::Real) => fun = AddReal,
            (Add, _) => panic!("unknown overloaded tpye for add"),
            (Sub, &Type::Int) => fun = SubInt,
            (Sub, &Type::Real) => fun = SubReal,
            (Sub, _) => panic!("unknown overloaded tpye for sub"),
            (Mul, &Type::Int) => fun = MulInt,
            (Mul, &Type::Real) => fun = MulReal,
            (Mul, _) => panic!("unknown overloaded tpye for mul"),
            (Div, &Type::Int) => fun = Div,
            (Div, _) => panic!("unknown overloaded tpye for div"),
            (Divf, &Type::Real) => fun = Divf,
            (Divf, _) => panic!("unknown overloaded tpye for divf"),
            (Mod, &Type::Int) => fun = ModInt,
            (Mod, _) => panic!("unknown overloaded tpye for mod"),
            (Eq, &Type::Int) => fun = EqInt,
            (Eq, &Type::Real) => fun = EqReal,
            (Eq, &Type::Char) => fun = EqChar,
            (Eq, ty) if ty == &str_ty => return gen_str_op(LangItem::StringEq),
            (Eq, _) => panic!("unknown overloaded tpye for eq"),
            (Neq, &Type::Int) => fun = NeqInt,
            (Neq, &Type::Real) => fun = NeqReal,
            (Neq, &Type::Char) => fun = NeqChar,
            (Neq, ty) if ty == &str_ty => return gen_str_op(LangItem::StringNeq),
            (Neq, _) => panic!("unknown overloaded tpye for neq"),
            (Gt, &Type::Int) => fun = GtInt,
            (Gt, &Type::Real) => fun = GtReal,
            (Gt, &Type::Char) => fun = GtChar,
            (Gt, ty) if ty == &str_ty => return gen_str_op(LangItem::StringGt),
            (Gt, _) => panic!("unknown overloaded tpye for gt"),
            (Ge, &Type::Int) => fun = GeInt,
            (Ge, &Type::Real) => fun = GeReal,
            (Ge, &Type::Char) => fun = GeChar,
            (Ge, ty) if ty == &str_ty => return gen_str_op(LangItem::StringGe),
            (Ge, _) => panic!("unknown overloaded tpye for ge"),
            (Lt, &Type::Int) => fun = LtInt,
            (Lt, &Type::Real) => fun = LtReal,
            (Lt, &Type::Char) => fun = LtChar,
            (Lt, ty) if ty == &str_ty => return gen_str_op(LangItem::StringLt),
            (Lt, _) => panic!("unknown overloaded tpye for lt"),
            (Le, &Type::Int) => fun = LeInt,
            (Le, &Type::Real) => fun = LeReal,
            (Le, &Type::Char) => fun = LeChar,
            (Le, ty) if ty == &str_ty => return gen_str_op(LangItem::StringLe),
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

        ExprKind::BuiltinCall { fun, args }
    }
}

use crate::pass::Pass;
impl Pass<TypedCoreContext, TypeError> for ResolveOverload {
    type Target = TypedCoreContext;

    fn trans(&mut self, context: TypedCoreContext, _: &Config) -> Result<Self::Target> {
        let symbol_table = context.symbol_table;
        let ast = context.ast;
        let lang_items = context.lang_items;

        let mut resolver = Resolver::new(symbol_table, lang_items);
        let ast = resolver.transform_ast(ast);
        let symbol_table = resolver.symbol_table;
        let lang_items = resolver.lang_items;

        Ok(Context {
            symbol_table,
            ast,
            lang_items,
        })
    }
}
