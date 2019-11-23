use super::util::Transform;
use crate::ast::*;
use crate::config::Config;
use crate::id::Id;
use std::collections::HashSet;

#[derive(Debug)]
pub struct CaseSimplify {
    id: Id,
    symbol_table: Option<SymbolTable>,
}

impl CaseSimplify {
    pub fn new(id: Id) -> Self {
        CaseSimplify {
            symbol_table: None,
            id,
        }
    }
    fn symbol_table(&self) -> &SymbolTable {
        self.symbol_table.as_ref().unwrap()
    }
    fn generate_symbol_table(&mut self) -> SymbolTable {
        self.symbol_table.take().unwrap()
    }

    fn gensym(&mut self, name: &str) -> Symbol {
        let id = self.id.next();
        Symbol(format!("#{}", name), id)
    }
}

impl Transform<Type> for CaseSimplify {
    fn transform_case(
        &mut self,
        ty: Type,
        cond: Box<TypedCoreExpr>,
        clauses: Vec<(TypedPattern, TypedCoreExpr)>,
    ) -> TypedCoreExpr {
        let condsym = self.gensym("cond");
        let condty = cond.ty();
        Expr::Binds {
            ty: ty.clone(),
            binds: vec![Statement::Val {
                pattern: Pattern::Variable {
                    name: condsym.clone(),
                    ty: condty.clone(),
                },
                rec: false,
                expr: *cond,
            }],
            ret: Expr::Case {
                ty: ty,
                cond: Expr::Symbol {
                    name: condsym,
                    ty: condty,
                }
                .boxed(),
                clauses,
            }
            .boxed(),
        }
    }
}

use crate::pass::Pass;
impl<'a> Pass<(SymbolTable, TypedCore), TypeError<'a>> for CaseSimplify {
    type Target = (SymbolTable, TypedCore);

    fn trans<'b>(
        &'b mut self,
        (symbol_table, ast): (SymbolTable, TypedCore),
        _: &Config,
    ) -> Result<'a, Self::Target> {
        self.symbol_table = Some(symbol_table);
        let ast = self.transform_ast(ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
