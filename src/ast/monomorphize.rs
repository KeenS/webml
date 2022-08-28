use crate::ast::*;
use crate::id::Id;
use crate::prim::*;
use crate::Config;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use util::{Transform, Traverse};

#[derive(Debug)]
pub struct Monomorphize {
    id: Id,
}

impl Monomorphize {
    pub fn new(id: Id) -> Self {
        Self { id }
    }
}

#[derive(Debug, Default)]
struct InstanceCollector {
    instance_table: HashMap<Symbol, HashSet<Vec<Type>>>,
}

impl Traverse<Type> for InstanceCollector {
    fn traverse_tyapp(&mut self, _: Span, fun: &mut Symbol, arg: &mut Vec<Type>) {
        self.instance_table
            .entry(fun.clone())
            .or_default()
            .insert(arg.clone());
    }
}

#[derive(Debug)]
struct Monomorphizer {
    instance_table: HashMap<Symbol, HashSet<Vec<Type>>>,
    name_table: HashMap<(Symbol, Vec<Type>), Symbol>,
    id: Id,
}

impl Monomorphizer {
    fn new(instance_table: HashMap<Symbol, HashSet<Vec<Type>>>, id: Id) -> Self {
        Self {
            instance_table,
            name_table: HashMap::new(),
            id,
        }
    }

    fn instanciated_name(&mut self, name: Symbol, arg: Vec<Type>) -> Symbol {
        // TODO: use Entry API
        if let Some(n) = self.name_table.get(&(name.clone(), arg.clone())) {
            return n.clone();
        }
        let mut new_name = name.0.clone();
        new_name.push_str(&format!("{}", arg.iter().format("{}")));
        let id = self.id.next();
        let new_symbol = Symbol(new_name, id);
        self.name_table.insert((name, arg), new_symbol.clone());
        new_symbol
    }
}

fn rewrite_type(ty: &mut Type, params: &[TypeId], args: &[Type]) {
    use Type::*;
    match ty {
        Variable(id) => {
            if let Some(index) = params.iter().position(|id2| id2 == id) {
                *ty = args[index].clone();
            }
        }
        Char | Int | Real | Datatype(_) => (),
        Fun(f, arg) => {
            rewrite_type(&mut *f, params, args);
            rewrite_type(&mut *arg, params, args);
        }
        Tuple(tys) => {
            for ty in tys {
                rewrite_type(&mut *ty, params, args);
            }
        }
        TyAbs(_, _) => unreachable!(),
    }
}

#[derive(Debug)]
pub struct Instanciator<'a> {
    params: &'a [TypeId],
    args: &'a [Type],
    m: &'a mut Monomorphizer,
}

impl<'a> Traverse<Type> for Instanciator<'a> {
    fn traverse_expr(&mut self, expr: &mut CoreExpr<Type>) {
        rewrite_type(&mut expr.ty, self.params, self.args);

        // the same as default implementation
        use crate::ast::ExprKind::*;

        let span = expr.span.clone();
        match &mut expr.inner {
            Binds { binds, ret } => self.traverse_binds(span, binds, ret),
            BuiltinCall { fun, args } => self.traverse_builtincall(span, fun, args),
            ExternCall {
                module,
                fun,
                args,
                argty,
                retty,
            } => self.traverse_externcall(span, module, fun, args, argty, retty),
            Fn { param, body } => self.traverse_fn(span, param, body),
            App { fun, arg } => self.traverse_app(span, fun, arg),
            TyApp { fun, arg } => self.traverse_tyapp(span, fun, arg),
            Case { cond, clauses } => self.traverse_case(span, cond, clauses),
            Tuple { tuple } => self.traverse_tuple(span, tuple),
            Constructor { arg, name } => self.traverse_constructor(span, arg, name),
            Symbol { name } => self.traverse_sym(span, name),
            Literal { value } => self.traverse_lit(span, value),
            D(_) => (),
        }
    }

    fn traverse_pattern(&mut self, pattern: &mut CorePattern<Type>) {
        rewrite_type(&mut pattern.ty, self.params, self.args);

        // the same as default implementation
        use PatternKind::*;
        let span = pattern.span.clone();
        match &mut pattern.inner {
            Constant { value } => self.traverse_pat_constant(span, value),
            Char { value } => self.traverse_pat_char(span, value),
            Constructor { name, arg } => self.traverse_pat_constructor(span, name, arg),
            Tuple { tuple } => self.traverse_pat_tuple(span, tuple),
            Variable { name } => self.traverse_pat_variable(span, name),
            Wildcard {} => self.traverse_pat_wildcard(span),
            D(d) => match *d {},
        }
    }
    fn traverse_pat_variable(&mut self, _: Span, name: &mut Symbol) {
        if self.m.instance_table.contains_key(&name) {
            *name = self.m.instanciated_name(name.clone(), self.args.to_vec())
        }
    }
}

impl Transform<Type> for Monomorphizer {
    fn transform_val(
        &mut self,
        rec: bool,
        pattern: CorePattern<Type>,
        expr: CoreExpr<Type>,
    ) -> CoreDeclaration<Type> {
        let pattern = self.transform_pattern(pattern);
        let expr = self.transform_expr(expr);

        if !matches!(pattern.ty, Type::TyAbs(_, _)) {
            return Declaration::Val { rec, pattern, expr };
        }

        let binds = pattern
            .binds()
            .into_iter()
            .map(|(n, _)| n)
            .cloned()
            .collect::<Vec<_>>();

        // currently only supports `val variable = expr`
        assert_eq!(binds.len(), 1);

        let mut ret = vec![];
        // TODO: no need to clone
        for args in self.instance_table[&binds[0]].clone() {
            let mut expr = expr.clone();
            let mut pattern = pattern.clone();
            match pattern.ty {
                Type::TyAbs(params, body) => {
                    pattern.ty = *body;

                    Instanciator {
                        params: &params,
                        args: &args,
                        m: self,
                    }
                    .traverse_pattern(&mut pattern);
                }
                _ => (),
            }

            match expr.ty {
                Type::TyAbs(params, body) => {
                    expr.ty = *body;
                    Instanciator {
                        params: &params,
                        args: &args,
                        m: self,
                    }
                    .traverse_expr(&mut expr);
                }
                _ => (),
            }
            ret.push(Declaration::Val { rec, expr, pattern });
        }
        Declaration::Local {
            binds: vec![],
            body: ret,
        }
    }

    fn transform_tyapp(&mut self, _: Span, fun: Symbol, arg: Vec<Type>) -> CoreExprKind<Type> {
        let name = self.instanciated_name(fun, arg);
        ExprKind::Symbol { name }
    }
}

use crate::pass::Pass;
impl Pass<TypedCoreContext, crate::Error> for Monomorphize {
    type Target = TypedCoreContext;

    fn trans(&mut self, context: TypedCoreContext, _: &Config) -> Result<Self::Target> {
        let mut ast = context.ast;

        let mut collector = InstanceCollector::default();
        collector.traverse_ast(&mut ast);
        let mut monomorphizer = Monomorphizer::new(collector.instance_table, self.id.clone());
        let ast = monomorphizer.transform_ast(ast);
        let symbol_table = context.symbol_table;
        let lang_items = context.lang_items;

        Ok(Context {
            symbol_table,
            ast,
            lang_items,
        })
    }
}
