use crate::ast::util::Traverse;
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
            Variable(_) | Int | Real => {
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
        pattern: &mut Pattern<Ty>,
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
        _ty: &mut Ty,
        binds: &mut Vec<CoreStatement<Ty>>,
        ret: &mut Box<CoreExpr<Ty>>,
    ) {
        let mut scope = self.new_scope();
        for bind in binds.iter_mut() {
            scope.traverse_statement(bind);
        }
        scope.traverse_expr(ret);
    }

    fn traverse_fn(&mut self, _ty: &mut Ty, param: &mut Symbol, body: &mut Box<CoreExpr<Ty>>) {
        let mut scope = self.new_scope();
        scope.new_variable(param);
        scope.traverse_expr(body);
    }

    fn traverse_case(
        &mut self,
        _ty: &mut Ty,
        expr: &mut Box<CoreExpr<Ty>>,
        arms: &mut Vec<(Pattern<Ty>, CoreExpr<Ty>)>,
    ) {
        self.traverse_expr(expr);
        for &mut (ref mut pat, ref mut arm) in arms.iter_mut() {
            let mut scope = self.new_scope();
            scope.traverse_pattern(pat);
            scope.traverse_expr(arm);
        }
    }

    fn traverse_sym(&mut self, _ty: &mut Ty, name: &mut Symbol) {
        if self.is_constructor(name) {
            self.rename_constructor(name);
        } else {
            self.rename(name);
        }
    }

    fn traverse_pat_constructor(
        &mut self,
        _ty: &mut Ty,
        name: &mut Symbol,
        arg: &mut Option<Box<Pattern<Ty>>>,
    ) {
        self.rename_constructor(name);
        if let Some(pat) = arg {
            self.traverse_pattern(&mut *pat);
        }
    }

    fn traverse_pat_variable(&mut self, _ty: &mut Ty, name: &mut Symbol) {
        if self.is_constructor(name) {
            self.rename_constructor(name)
        } else {
            self.new_variable(name)
        }
    }

    fn traverse_pat_tuple(&mut self, _ty: &mut Ty, tuple: &mut Vec<Pattern<Ty>>) {
        for pat in tuple {
            self.traverse_pattern(pat)
        }
    }
}

impl Rename {
    pub fn new(id: Id) -> Self {
        // leave built in functions as non_renamed
        let functions = crate::BUILTIN_FUNCTIONS
            .iter()
            .map(|s| (Symbol::new(*s), 0))
            .collect();
        let datatypes = ["bool"].iter().map(|s| (Symbol::new(*s), 0)).collect();
        let constructors = ["false", "true"]
            .iter()
            .map(|s| (Symbol::new(*s), 0))
            .collect();

        let mut symbol_table = SymbolTable::new();
        symbol_table.register_type(
            Symbol::new("bool"),
            TypeInfo {
                constructors: vec![(Symbol::new("false"), None), (Symbol::new("true"), None)],
            },
        );

        Rename {
            symbol_table: Some(symbol_table),
            variable_tables: vec![functions],
            type_tables: vec![datatypes],
            constructor_tables: vec![constructors],
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

impl<E, Ty: Clone> Pass<Core<Ty>, E> for Rename {
    type Target = (SymbolTable, Core<Ty>);

    fn trans(&mut self, mut ast: Core<Ty>, _: &Config) -> ::std::result::Result<Self::Target, E> {
        self.scope().traverse_ast(&mut ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
