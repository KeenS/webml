use crate::ast::util::{Transform, Traverse};
use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};

pub struct Rename {
    symbol_table: Option<SymbolTable>,
    variable_tables: Vec<HashMap<Symbol, u64>>,
    type_tables: Vec<HashMap<Symbol, u64>>,
    constructor_tables: Vec<HashMap<Symbol, u64>>,
    pos: usize,
    id: Id,
}

struct Scope<'a>(&'a mut Rename);

impl<'a> Deref for Scope<'a> {
    type Target = Rename;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        self.pos -= 1;
    }
}

impl<'a> Scope<'a> {
    fn new(inner: &'a mut Rename) -> Self {
        let pos = inner.pos;
        if inner.variable_tables.len() <= pos {
            inner.variable_tables.push(HashMap::new());
            inner.type_tables.push(HashMap::new());
            inner.constructor_tables.push(HashMap::new());
        } else {
            inner.variable_tables[pos].clear();
            inner.type_tables[pos].clear();
            inner.constructor_tables[pos].clear();
        }

        inner.pos += 1;
        Scope(inner)
    }

    fn new_scope(&mut self) -> Scope<'_> {
        Scope::new(self)
    }

    fn new_variable(&mut self, symbol: &mut Symbol) {
        let pos = self.pos - 1;
        let new_id = self.id.next();
        self.variable_tables[pos].insert(symbol.clone(), new_id);
        symbol.1 = new_id;
    }

    fn new_type(&mut self, symbol: &mut Symbol) {
        let pos = self.pos - 1;
        let new_id = self.id.next();
        self.type_tables[pos].insert(symbol.clone(), new_id);
        symbol.1 = new_id;
    }

    fn new_constructor(&mut self, symbol: &mut Symbol) {
        let pos = self.pos - 1;
        let new_id = self.id.next();
        self.constructor_tables[pos].insert(symbol.clone(), new_id);
        symbol.1 = new_id;
    }

    fn is_constructor(&mut self, symbol: &Symbol) -> bool {
        let pos = self.pos;
        self.constructor_tables[0..pos]
            .iter_mut()
            .rev()
            .any(|t| t.contains_key(symbol))
    }

    fn rename(&mut self, symbol: &mut Symbol) {
        let pos = self.pos;
        for table in self.variable_tables[0..pos].iter_mut().rev() {
            match table.get(symbol) {
                Some(new_id) => {
                    symbol.1 = *new_id;
                    return;
                }
                None => {}
            }
        }
    }

    fn rename_constructor(&mut self, symbol: &mut Symbol) {
        let pos = self.pos;
        for table in self.constructor_tables[0..pos].iter_mut().rev() {
            match table.get(symbol) {
                Some(new_id) => {
                    symbol.1 = *new_id;
                    return;
                }
                None => {}
            }
        }
    }

    fn rename_type(&mut self, ty: &mut Type) {
        use Type::*;

        match ty {
            Variable(_) | Char | Int | Real => {
                // noop
                ()
            }
            Fun(arg, body) => {
                self.rename_type(arg);
                self.rename_type(body);
            }
            Tuple(tuple) => {
                for t in tuple {
                    self.rename_type(t)
                }
            }
            Datatype(name) => {
                let pos = self.pos;
                for table in self.type_tables[0..pos].iter_mut().rev() {
                    match table.get(name) {
                        Some(new_id) => {
                            name.1 = *new_id;
                            return;
                        }
                        None => {}
                    }
                }
            }
        }
    }
}

impl<'a, Ty: Clone> util::Traverse<Ty> for Scope<'a> {
    fn traverse_datatype<'b, 'c>(
        &'b mut self,
        name: &mut Symbol,
        constructors: &mut Vec<(Symbol, Option<Type>)>,
    ) {
        let scope = self;
        scope.new_type(name);
        for (cname, argty) in constructors.iter_mut() {
            scope.new_constructor(cname);
            if let Some(argty) = argty {
                scope.rename_type(argty);
            }
        }

        let constructor_info = TypeInfo {
            constructors: constructors.clone(),
        };
        scope
            .symbol_table()
            .register_type(name.clone(), constructor_info);
    }

    fn traverse_val<'b, 'c>(
        &'b mut self,
        rec: &mut bool,
        pattern: &mut CorePattern<Ty>,
        expr: &mut CoreExpr<Ty>,
    ) {
        let scope = self;
        if *rec {
            scope.traverse_pattern(pattern);
            scope.traverse_expr(expr);
        } else {
            scope.traverse_expr(expr);
            scope.traverse_pattern(pattern);
        }
    }

    fn traverse_binds(
        &mut self,
        binds: &mut Vec<CoreDeclaration<Ty>>,
        ret: &mut Box<CoreExpr<Ty>>,
    ) {
        let mut scope = self.new_scope();
        for bind in binds.iter_mut() {
            scope.traverse_statement(bind);
        }
        scope.traverse_expr(ret);
    }

    fn traverse_fn(&mut self, param: &mut Symbol, body: &mut Box<CoreExpr<Ty>>) {
        let mut scope = self.new_scope();
        scope.new_variable(param);
        scope.traverse_expr(body);
    }

    fn traverse_case(
        &mut self,
        expr: &mut Box<CoreExpr<Ty>>,
        arms: &mut Vec<(CorePattern<Ty>, CoreExpr<Ty>)>,
    ) {
        self.traverse_expr(expr);
        for &mut (ref mut pat, ref mut arm) in arms.iter_mut() {
            let mut scope = self.new_scope();
            scope.traverse_pattern(pat);
            scope.traverse_expr(arm);
        }
    }

    fn traverse_constructor(&mut self, arg: &mut Option<Box<CoreExpr<Ty>>>, name: &mut Symbol) {
        if self.is_constructor(name) {
            self.rename_constructor(name);
        } else {
            self.rename(name);
        }

        if let Some(arg) = arg {
            self.traverse_expr(arg)
        }
    }
    fn traverse_sym(&mut self, name: &mut Symbol) {
        if self.is_constructor(name) {
            self.rename_constructor(name);
        } else {
            self.rename(name);
        }
    }

    fn traverse_pat_constructor(
        &mut self,
        name: &mut Symbol,
        arg: &mut Option<Box<CorePattern<Ty>>>,
    ) {
        self.rename_constructor(name);
        if let Some(pat) = arg {
            self.traverse_pattern(&mut *pat);
        }
    }

    fn traverse_pat_variable(&mut self, name: &mut Symbol) {
        if self.is_constructor(&name) {
            self.rename_constructor(name)
        } else {
            self.new_variable(name)
        }
    }

    fn traverse_pat_tuple(&mut self, tuple: &mut Vec<CorePattern<Ty>>) {
        for pat in tuple {
            self.traverse_pattern(pat)
        }
    }
}

static BUILTIN_FUNCTIONS: &[(&str, BIF)] = &[
    ("+", BIF::Add),
    ("-", BIF::Sub),
    ("*", BIF::Mul),
    ("div", BIF::Div),
    ("/", BIF::Divf),
    ("mod", BIF::Mod),
    ("=", BIF::Eq),
    ("<>", BIF::Neq),
    (">", BIF::Gt),
    (">=", BIF::Ge),
    ("<", BIF::Lt),
    ("<=", BIF::Le),
];

impl Rename {
    pub fn new(id: Id) -> Self {
        // leave built in functions as non_renamed
        let functions = BUILTIN_FUNCTIONS
            .iter()
            .map(|(s, _)| (Symbol::new(*s), 0))
            .collect();

        let symbol_table = SymbolTable::new();
        Rename {
            symbol_table: Some(symbol_table),
            variable_tables: vec![functions],
            type_tables: vec![HashMap::new()],
            constructor_tables: vec![HashMap::new()],
            pos: 0,
            id,
        }
    }

    fn symbol_table(&mut self) -> &mut SymbolTable {
        self.symbol_table.as_mut().unwrap()
    }

    fn generate_symbol_table(&mut self) -> SymbolTable {
        self.symbol_table.take().unwrap()
    }

    fn scope<'a>(&'a mut self) -> Scope<'a> {
        Scope::new(self)
    }
}

// bif -> fn x => _builtincall "bif"(x)
struct WrapBIF {
    bif_table: HashMap<String, BIF>,
    id: Id,
}
impl WrapBIF {
    fn new(id: Id) -> Self {
        Self {
            bif_table: BUILTIN_FUNCTIONS
                .iter()
                .map(|(s, bif)| (s.to_string(), *bif))
                .collect(),
            id,
        }
    }

    fn gensym(&mut self, name: impl Into<String>) -> Symbol {
        let id = self.id.next();
        Symbol(name.into(), id)
    }
}

impl Transform<Empty> for WrapBIF {
    fn transform_symbol(&mut self, name: Symbol) -> UntypedCoreExprKind {
        if name.1 == 0 {
            if let Some(bif) = self.bif_table.get(&name.0).cloned() {
                use BIF::*;
                return match bif {
                    Add | Sub | Mul | Div | Divf | Mod | Eq | Neq | Gt | Ge | Lt | Le => {
                        let tuple = self.gensym("tuple");
                        let l = self.gensym("x");
                        let r = self.gensym("y");
                        // fn tuple => case tuple of (x, y) => _builtincall "op"(x, y)
                        ExprKind::Fn {
                            param: tuple.clone(),
                            body: Expr {
                                ty: Empty {},
                                inner: ExprKind::Case {
                                    cond: Expr {
                                        ty: Empty {},
                                        inner: ExprKind::Symbol { name: tuple },
                                    }
                                    .boxed(),
                                    clauses: vec![(
                                        Pattern {
                                            ty: Empty {},
                                            inner: PatternKind::Tuple {
                                                tuple: vec![
                                                    Pattern {
                                                        ty: Empty {},
                                                        inner: PatternKind::Variable {
                                                            name: l.clone(),
                                                        },
                                                    },
                                                    Pattern {
                                                        ty: Empty {},
                                                        inner: PatternKind::Variable {
                                                            name: r.clone(),
                                                        },
                                                    },
                                                ],
                                            },
                                        },
                                        Expr {
                                            ty: Empty {},
                                            inner: ExprKind::BuiltinCall {
                                                fun: bif,
                                                args: vec![
                                                    Expr {
                                                        ty: Empty {},
                                                        inner: ExprKind::Symbol { name: l },
                                                    },
                                                    Expr {
                                                        ty: Empty {},
                                                        inner: ExprKind::Symbol { name: r },
                                                    },
                                                ],
                                            },
                                        },
                                    )],
                                },
                            }
                            .boxed(),
                        }
                    }
                };
            }
        }
        ExprKind::Symbol { name }
    }
}

impl<E> Pass<UntypedCore, E> for Rename {
    type Target = UntypedCoreContext;

    fn trans(
        &mut self,
        mut ast: UntypedCore,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        self.scope().traverse_ast(&mut ast);
        let mut wrap_bif = WrapBIF::new(self.id.clone());
        let ast = wrap_bif.transform_ast(ast);
        let symbol_table = self.generate_symbol_table();
        let lang_items = HashMap::new();
        Ok(Context {
            symbol_table,
            ast,
            lang_items,
        })
    }
}
