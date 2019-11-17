use crate::ast::util::Transform;
use crate::ast::*;
use crate::config::Config;
use crate::pass::Pass;

pub struct VarToConstructor {
    symbol_table: Option<SymbolTable>,
}

impl VarToConstructor {
    pub fn new() -> VarToConstructor {
        Self { symbol_table: None }
    }

    fn init(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = Some(symbol_table)
    }

    fn generate_symbol_table(&mut self) -> SymbolTable {
        self.symbol_table.take().unwrap()
    }

    fn symbol_table(&self) -> &SymbolTable {
        self.symbol_table.as_ref().unwrap()
    }

    fn is_constructor(&self, name: &Symbol) -> bool {
        self.symbol_table()
            .get_datatype_of_constructor(&name)
            .is_some()
    }
}

impl<Ty: std::fmt::Debug> Transform<Ty> for VarToConstructor {
    fn transform_symbol(&mut self, ty: Ty, name: Symbol) -> Expr<Ty> {
        if self.is_constructor(&name) {
            Expr::Constructor { ty, name }
        } else {
            Expr::Symbol { ty, name }
        }
    }

    fn transform_pat_variable(&mut self, ty: Ty, name: Symbol) -> Pattern<Ty> {
        if self.is_constructor(&name) {
            Pattern::Constructor { ty, name }
        } else {
            Pattern::Variable { ty, name }
        }
    }
}

impl<E, Ty: Clone + std::fmt::Debug> Pass<(SymbolTable, AST<Ty>), E> for VarToConstructor {
    type Target = (SymbolTable, AST<Ty>);

    fn trans(
        &mut self,
        (symbol_table, ast): (SymbolTable, AST<Ty>),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        self.init(symbol_table);
        let ast = self.transform_ast(ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
