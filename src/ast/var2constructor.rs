use crate::ast::util::Traverse;
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
}

impl<Ty: Clone> Traverse<Ty> for VarToConstructor {
    fn traverse_expr(&mut self, expr: &mut Expr<Ty>) {
        use crate::ast::Expr::*;
        let new_expr = match expr {
            Symbol { ty, name }
                if self
                    .symbol_table()
                    .get_datatype_of_constructor(name)
                    .is_some() =>
            {
                Expr::Constructor {
                    ty: ty.clone(),
                    name: name.clone(),
                }
            }
            _ => return,
        };

        *expr = new_expr
    }

    fn traverse_pattern(&mut self, pattern: &mut Pattern<Ty>) {
        use crate::ast::Pattern::*;
        let new_pattern = match pattern {
            Variable { ty, name }
                if self
                    .symbol_table()
                    .get_datatype_of_constructor(name)
                    .is_some() =>
            {
                Pattern::Constructor {
                    ty: ty.clone(),
                    name: name.clone(),
                }
            }
            _ => return,
        };

        *pattern = new_pattern;
    }
}

impl<E, Ty: Clone> Pass<(SymbolTable, AST<Ty>), E> for VarToConstructor {
    type Target = (SymbolTable, AST<Ty>);

    fn trans(
        &mut self,
        (symbol_table, mut ast): (SymbolTable, AST<Ty>),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        self.init(symbol_table);
        self.traverse_ast(&mut ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
