use crate::ast::util::Transform;
use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use crate::pass::Pass;

pub struct VarToConstructor {
    symbol_table: Option<SymbolTable>,
    id: Id,
}

impl VarToConstructor {
    pub fn new(id: Id) -> Self {
        Self {
            symbol_table: None,
            id,
        }
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

    fn arg_type(&self, name: &Symbol) -> Option<&Type> {
        self.symbol_table().get_argtype_of_constructor(&name)
    }

    fn gensym(&mut self) -> Symbol {
        let id = self.id.next();
        Symbol("#arg".into(), id)
    }
}

impl Transform<()> for VarToConstructor {
    fn transform_symbol(&mut self, ty: (), name: Symbol) -> UntypedCoreExpr {
        if self.is_constructor(&name) {
            if let Some(_) = self.arg_type(&name) {
                let sym = self.gensym();
                Expr::Fn {
                    ty,
                    param: sym.clone(),
                    body: Expr::Constructor {
                        ty: (),
                        arg: Some(Expr::Symbol { name: sym, ty: () }.boxed()),
                        name,
                    }
                    .boxed(),
                }
            } else {
                Expr::Constructor {
                    ty,
                    arg: None,
                    name,
                }
            }
        } else {
            Expr::Symbol { ty, name }
        }
    }

    fn transform_pat_variable(&mut self, ty: (), name: Symbol) -> UntypedPattern {
        if self.is_constructor(&name) {
            Pattern::Constructor {
                ty,
                arg: None,
                name,
            }
        } else {
            Pattern::Variable { ty, name }
        }
    }
}

impl<E> Pass<(SymbolTable, UntypedCore), E> for VarToConstructor {
    type Target = (SymbolTable, UntypedCore);

    fn trans(
        &mut self,
        (symbol_table, ast): (SymbolTable, UntypedCore),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        self.init(symbol_table);
        let ast = self.transform_ast(ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
