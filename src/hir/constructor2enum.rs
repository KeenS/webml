use crate::config::Config;
use crate::hir::util::Transform;
use crate::hir::*;
use crate::pass::Pass;
use std::collections::{HashMap, HashSet};

pub struct ConstructorToEnumPass {
    enum_likes: HashSet<Symbol>,
    symbol_table: SymbolTable,
}

fn rewrite_ty(enum_likes: &HashSet<Symbol>, ty: HTy) -> HTy {
    use HTy::*;
    match ty {
        Datatype(name) if enum_likes.contains(&name) => HTy::Int,
        Fun(arg, ret) => Fun(
            Box::new(rewrite_ty(enum_likes, *arg)),
            Box::new(rewrite_ty(enum_likes, *ret)),
        ),
        Tuple(tuple) => Tuple(
            tuple
                .into_iter()
                .map(|t| rewrite_ty(enum_likes, t))
                .collect(),
        ),
        ty => ty,
    }
}

impl ConstructorToEnumPass {
    fn new(symbol_table: SymbolTable) -> Self {
        let (enum_likes, types) = symbol_table
            .types
            .into_iter()
            .partition::<HashMap<Symbol, TypeInfo>, _>(|(_, type_info)| {
                type_info.constructors.iter().all(|(_, arg)| arg.is_none())
            });
        let enum_likes = enum_likes.into_iter().map(|(name, _)| name).collect();

        let types = types
            .into_iter()
            .map(|(name, mut type_info)| {
                type_info.constructors = type_info
                    .constructors
                    .into_iter()
                    .map(|(descriminant, argty)| match argty {
                        Some(ty) => (descriminant, Some(rewrite_ty(&enum_likes, ty))),
                        argty @ None => (descriminant, argty),
                    })
                    .collect();
                (name, type_info)
            })
            .collect();

        let symbol_table = SymbolTable { types };

        let mut this = Self {
            enum_likes,
            symbol_table,
        };
        this.rewrite_table();
        this
    }

    fn rewrite_table(&mut self) {}

    fn is_enum_like(&self, name: &Symbol) -> bool {
        self.enum_likes.contains(name)
    }

    fn rewrite_ty(&self, ty: HTy) -> HTy {
        rewrite_ty(&self.enum_likes, ty)
    }
}

impl Transform for ConstructorToEnumPass {
    fn transform_val(&mut self, mut val: Val) -> Val {
        val.ty = self.rewrite_ty(val.ty);
        val.expr = self.transform_expr(val.expr);
        val
    }

    fn transform_binds(&mut self, ty: HTy, bind: Box<Val>, ret: Box<Expr>) -> Expr {
        let ty = self.rewrite_ty(ty);
        Expr::Let {
            ty,
            bind: Box::new(self.transform_val(*bind)),
            ret: Box::new(self.transform_expr(*ret)),
        }
    }

    fn transform_fun(
        &mut self,
        param: (HTy, Symbol),
        body_ty: HTy,
        body: Box<Expr>,
        captures: Vec<(HTy, Symbol)>,
    ) -> Expr {
        let (param_ty, param) = param;
        let param_ty = self.rewrite_ty(param_ty);
        let param = (param_ty, param);
        let body_ty = self.rewrite_ty(body_ty);
        let captures = captures
            .into_iter()
            .map(|(ty, name)| (self.rewrite_ty(ty), name))
            .collect();
        Expr::Fun {
            param,
            body_ty,
            captures,
            body: Box::new(self.transform_expr(*body)),
        }
    }

    fn transform_closure(
        &mut self,
        envs: Vec<(HTy, Symbol)>,
        param_ty: HTy,
        body_ty: HTy,
        fname: Symbol,
    ) -> Expr {
        let envs = envs
            .into_iter()
            .map(|(ty, name)| (self.rewrite_ty(ty), name))
            .collect();
        let param_ty = self.rewrite_ty(param_ty);
        let body_ty = self.rewrite_ty(body_ty);

        Expr::Closure {
            envs,
            param_ty,
            body_ty,
            fname,
        }
    }

    fn transform_builtin_call(&mut self, ty: HTy, fun: BIF, args: Vec<Expr>) -> Expr {
        let ty = self.rewrite_ty(ty);

        Expr::BuiltinCall {
            ty,
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
        }
    }

    fn transform_extern_call(
        &mut self,
        ty: HTy,
        module: String,
        fun: String,
        args: Vec<Expr>,
    ) -> Expr {
        let ty = self.rewrite_ty(ty);
        Expr::ExternCall {
            ty,
            module,
            fun,
            args: args
                .into_iter()
                .map(|arg| self.transform_expr(arg))
                .collect(),
        }
    }

    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        let ty = self.rewrite_ty(ty);
        Expr::App {
            ty,
            fun: Box::new(self.transform_expr(*fun)),
            arg: Box::new(self.transform_expr(*arg)),
        }
    }

    fn transform_case(&mut self, ty: HTy, cond: Box<Expr>, arms: Vec<(Pattern, Expr)>) -> Expr {
        let ty = self.rewrite_ty(ty);
        let mut arms = arms;
        arms = arms
            .into_iter()
            .map(|(pat, expr)| {
                use Pattern::*;
                let pat = match pat {
                    Constant { value, ty } => {
                        let ty = self.rewrite_ty(ty);
                        Constant { value, ty }
                    }
                    Char { value, ty } => {
                        let ty = self.rewrite_ty(ty);
                        Char { value, ty }
                    }
                    Tuple { tys, tuple } => {
                        let tys = tys.into_iter().map(|ty| self.rewrite_ty(ty)).collect();
                        Tuple { tys, tuple }
                    }
                    Constructor {
                        descriminant,
                        ty,
                        arg,
                    } => match ty {
                        HTy::Datatype(name) if self.is_enum_like(&name) => Constant {
                            ty: HTy::Int,
                            value: descriminant as i64,
                        },
                        ty => Constructor {
                            descriminant,
                            ty,
                            arg,
                        },
                    },
                    Var { name, ty } => Var {
                        name,
                        ty: self.rewrite_ty(ty),
                    },
                };
                (pat, expr)
            })
            .collect();
        Expr::Case {
            ty,
            expr: Box::new(self.transform_expr(*cond)),
            arms: arms
                .into_iter()
                .map(|(pat, expr)| (pat, self.transform_expr(expr)))
                .collect(),
        }
    }

    fn transform_tuple(&mut self, tys: Vec<HTy>, tuple: Vec<Expr>) -> Expr {
        let tys = tys.into_iter().map(|ty| self.rewrite_ty(ty)).collect();

        Expr::Tuple {
            tys,
            tuple: tuple.into_iter().map(|e| self.transform_expr(e)).collect(),
        }
    }

    fn transform_proj(&mut self, ty: HTy, index: u32, tuple: Box<Expr>) -> Expr {
        let ty = self.rewrite_ty(ty);
        Expr::Proj {
            ty,
            index,
            tuple: Box::new(self.transform_expr(*tuple)),
        }
    }

    fn transform_constructor(
        &mut self,
        ty: HTy,
        arg: Option<Box<Expr>>,
        descriminant: u32,
    ) -> Expr {
        let name = match &ty {
            HTy::Datatype(name) => name,
            _ => unreachable!(),
        };
        if self.is_enum_like(name) {
            Expr::Lit {
                ty: HTy::Int,
                value: Literal::Int(descriminant as i64),
            }
        } else {
            Expr::Constructor {
                ty,
                arg,
                descriminant,
            }
        }
    }

    fn transform_sym(&mut self, ty: HTy, name: Symbol) -> Expr {
        let ty = self.rewrite_ty(ty);
        Expr::Sym { ty, name }
    }

    fn transform_lit(&mut self, ty: HTy, value: Literal) -> Expr {
        let ty = self.rewrite_ty(ty);
        Expr::Lit { ty, value }
    }
}

pub struct ConstructorToEnum {}
impl ConstructorToEnum {
    pub fn new() -> Self {
        Self {}
    }
}

impl<E> Pass<Context, E> for ConstructorToEnum {
    type Target = Context;

    fn trans(
        &mut self,
        Context(symbol_table, hir): Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        let mut pass = ConstructorToEnumPass::new(symbol_table);
        let hir = pass.transform_hir(hir);
        let symbol_table = pass.symbol_table;
        Ok(Context(symbol_table, hir))
    }
}
