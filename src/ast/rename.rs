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

    fn new_scope(&mut self) -> Scope {
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
        self.variable_tables[pos].insert(symbol.clone(), new_id);
        symbol.1 = new_id;
    }

    fn new_constructor(&mut self, symbol: &mut Symbol) {
        let pos = self.pos - 1;
        let new_id = self.id.next();
        self.variable_tables[pos].insert(symbol.clone(), new_id);
        symbol.1 = new_id;
    }

    fn new_symbol_pattern<Ty>(&mut self, pat: &mut Pattern<Ty>) {
        use Pattern::*;
        match pat {
            Constructor { .. } | Wildcard { .. } | Literal { .. } => (),
            Variable { name, .. } => self.new_variable(name),
            Tuple { tuple, .. } => {
                for (_, sym) in tuple {
                    self.new_variable(sym)
                }
            }
        }
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
}

impl<'a, Ty> util::Traverse<Ty> for Scope<'a> {
    fn traverse_datatype<'b, 'c>(&'b mut self, name: &mut Symbol, constructors: &mut Vec<Symbol>) {
        let scope = self;
        scope.new_type(name);
        for cname in constructors.iter_mut() {
            scope.new_constructor(cname);
            scope.new_variable(cname);
            // handle arg types
        }

        let constructor_info = TypeInfo {
            constructors: constructors.clone(),
        };
        scope
            .symbol_table()
            .register_type(name.clone(), constructor_info);
    }

    fn traverse_fun<'b, 'c>(
        &'b mut self,
        name: &mut Symbol,
        params: &mut Vec<(Ty, Symbol)>,
        expr: &mut Expr<Ty>,
    ) {
        self.new_variable(name);
        let mut scope = self.new_scope();
        for (_, param) in params {
            scope.new_variable(param);
        }
        scope.traverse_expr(expr);
    }

    fn traverse_val<'b, 'c>(&'b mut self, pattern: &mut Pattern<Ty>, expr: &mut Expr<Ty>) {
        let scope = self;
        scope.traverse_expr(expr);
        scope.new_symbol_pattern(pattern);
    }

    fn traverse_binds(
        &mut self,
        _ty: &mut Ty,
        binds: &mut Vec<Statement<Ty>>,
        ret: &mut Box<Expr<Ty>>,
    ) {
        let mut scope = self.new_scope();
        for bind in binds.iter_mut() {
            scope.traverse_statement(bind);
        }
        scope.traverse_expr(ret);
    }

    fn traverse_binop(
        &mut self,
        _ty: &mut Ty,
        op: &mut Symbol,
        l: &mut Box<Expr<Ty>>,
        r: &mut Box<Expr<Ty>>,
    ) {
        self.rename(op);
        self.traverse_expr(l);
        self.traverse_expr(r);
    }

    fn traverse_fn(&mut self, _ty: &mut Ty, param: &mut Symbol, body: &mut Box<Expr<Ty>>) {
        let mut scope = self.new_scope();
        scope.new_variable(param);
        scope.traverse_expr(body);
    }

    fn traverse_case(
        &mut self,
        _ty: &mut Ty,
        expr: &mut Box<Expr<Ty>>,
        arms: &mut Vec<(Pattern<Ty>, Expr<Ty>)>,
    ) {
        self.traverse_expr(expr);
        for &mut (ref mut pat, ref mut arm) in arms.iter_mut() {
            let mut scope = self.new_scope();
            scope.new_symbol_pattern(pat);
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
                constructors: vec![Symbol::new("false"), Symbol::new("true")],
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

impl<E, Ty> Pass<AST<Ty>, E> for Rename {
    type Target = (SymbolTable, AST<Ty>);

    fn trans(&mut self, mut ast: AST<Ty>, _: &Config) -> ::std::result::Result<Self::Target, E> {
        self.scope().traverse_ast(&mut ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
