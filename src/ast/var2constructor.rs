use crate::ast::util::Transform;
use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use crate::pass::Pass;

pub struct VarToConstructor {
    id: Id,
}

impl VarToConstructor {
    pub fn new(id: Id) -> Self {
        VarToConstructor { id }
    }

    fn generate_pass(&mut self, symbol_table: SymbolTable) -> VarToConstructorPass {
        VarToConstructorPass::new(symbol_table, self.id.clone())
    }
}

struct VarToConstructorPass {
    symbol_table: SymbolTable,
    id: Id,
}

impl VarToConstructorPass {
    fn new(symbol_table: SymbolTable, id: Id) -> Self {
        Self { symbol_table, id }
    }

    fn into_inner(self) -> (SymbolTable, Id) {
        (self.symbol_table, self.id)
    }

    fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
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

impl Transform<Empty> for VarToConstructorPass {
    fn transform_symbol(&mut self, name: Symbol) -> UntypedCoreExprKind {
        if self.is_constructor(&name) {
            if let Some(_) = self.arg_type(&name) {
                let sym = self.gensym();
                ExprKind::Fn {
                    param: sym.clone(),
                    body: Expr {
                        ty: Empty {},
                        inner: ExprKind::Constructor {
                            arg: Some(
                                Expr {
                                    ty: Empty {},
                                    inner: ExprKind::Symbol { name: sym },
                                }
                                .boxed(),
                            ),
                            name,
                        },
                    }
                    .boxed(),
                }
            } else {
                ExprKind::Constructor { arg: None, name }
            }
        } else {
            ExprKind::Symbol { name }
        }
    }

    fn transform_pat_variable(&mut self, name: Symbol) -> UntypedCorePatternKind {
        if self.is_constructor(&name) {
            PatternKind::Constructor { arg: None, name }
        } else {
            PatternKind::Variable { name }
        }
    }
}

impl<E> Pass<UntypedCoreContext, E> for VarToConstructor {
    type Target = UntypedCoreContext;

    fn trans(
        &mut self,
        context: UntypedCoreContext,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        let symbol_table = context.symbol_table;
        let ast = context.ast;
        let lang_items = context.lang_items;
        let mut pass = self.generate_pass(symbol_table);
        let ast = pass.transform_ast(ast);
        let (symbol_table, _) = pass.into_inner();
        Ok(Context {
            symbol_table,
            ast,
            lang_items,
        })
    }
}
