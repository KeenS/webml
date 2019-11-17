use super::util::Traverse;
use crate::ast::*;
use crate::config::Config;
use std::collections::HashSet;

#[derive(Debug)]
pub struct CaseCheck {
    symbol_table: Option<SymbolTable>,
}

impl CaseCheck {
    pub fn new() -> Self {
        CaseCheck { symbol_table: None }
    }
    fn symbol_table(&self) -> &SymbolTable {
        self.symbol_table.as_ref().unwrap()
    }
    fn generate_symbol_table(&mut self) -> SymbolTable {
        self.symbol_table.take().unwrap()
    }
}

impl Traverse<Type> for CaseCheck {
    // TODO: do it in typed ast
    // FIXME: return errors instead of panic
    fn traverse_case(
        &mut self,
        _ty: &mut Type,
        cond: &mut Box<Expr<Type>>,
        arms: &mut Vec<(Pattern<Type>, Expr<Type>)>,
    ) {
        let ty = cond.ty();
        match ty {
            // variants like
            Type::Datatype(type_name) => {
                let variants = self
                    .symbol_table()
                    .get_type(&type_name)
                    .expect("internal error: type not found")
                    .constructors
                    .clone()
                    .into_iter()
                    .collect::<HashSet<_>>();
                let mut matched = HashSet::new();
                let mut defaulted = false;
                for &mut (ref pat, _) in arms {
                    if defaulted {
                        panic!("pattern after default case is redundant");
                    }
                    match pat {
                        Pattern::Constructor { name, .. } => {
                            if matched.insert(name.clone()) {
                                // ok
                            } else {
                                panic!("redundant patterns")
                            }
                        }
                        Pattern::Variable { .. } | Pattern::Wildcard { .. } => defaulted = true,
                        _ => unreachable!("expression and pattern doesn't match. It'a bug"),
                    }
                }
                if matched != variants && !defaulted {
                    panic!("pattern non-exhausitive. rest: {:?}", matched)
                }
            }
            // integer like
            Type::Int => {
                let mut matched = HashSet::new();
                let mut defaulted = false;
                for &mut (ref pat, _) in arms {
                    if defaulted {
                        panic!("pattern after default case is redundant");
                    }
                    match pat {
                        Pattern::Constant { value: i, .. } => {
                            if matched.insert(i) {
                                // ok
                            } else {
                                panic!("redundant patterns")
                            }
                        }
                        Pattern::Variable { .. } | Pattern::Wildcard { .. } => defaulted = true,
                        _ => unreachable!("expression and pattern doesn't match. It'a bug"),
                    }
                }
                // FIXME: treat cases when all the integers are specified by literal pattern
                if !defaulted {
                    panic!("pattern non-exhausitive. rest: {:?}", matched)
                }
            }
            // record like
            Type::Tuple(_tuple) => {
                assert_eq!(arms.len(), 1);
                for &mut (ref pat, _) in arms {
                    match pat {
                        &Pattern::Tuple { .. }
                        | &Pattern::Variable { .. }
                        | &Pattern::Wildcard { .. } => {
                            // ok
                        }
                        _ => unreachable!("expression and pattern doesn't match. It'a bug"),
                    }
                }
            }
            _ => panic!("non variant pattern match isn't supported"),
        }
    }
}

use crate::pass::Pass;
impl<'a> Pass<(SymbolTable, ast::TypedAst), TypeError<'a>> for CaseCheck {
    type Target = (SymbolTable, ast::TypedAst);

    fn trans<'b>(
        &'b mut self,
        (symbol_table, mut ast): (SymbolTable, ast::TypedAst),
        _: &Config,
    ) -> Result<'a, Self::Target> {
        self.symbol_table = Some(symbol_table);
        self.traverse_ast(&mut ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
