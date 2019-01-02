use super::util::Traverse;
use crate::ast::*;
use crate::config::Config;
use crate::prim::*;
use std::collections::HashSet;

#[derive(Debug)]
pub struct CaseCheck;

impl CaseCheck {
    pub fn new() -> Self {
        CaseCheck
    }
}

impl Traverse for CaseCheck {
    // TODO: do it in typed ast
    // FIXME: return errors instead of panic
    fn traverse_case(
        &mut self,
        _ty: &mut TyDefer,
        cond: &mut Box<Expr>,
        arms: &mut Vec<(Pattern, Expr)>,
    ) {
        let ty = cond
            .ty_defer()
            .force("internal error: typed AST isn't typed");
        match ty {
            // variants like
            Ty::Bool => {
                let variants = {
                    let mut set = HashSet::new();
                    set.insert(true);
                    set.insert(false);
                    set
                };
                let mut matched = HashSet::new();
                let mut defaulted = false;
                for &mut (ref pat, _) in arms {
                    if defaulted {
                        panic!("pattern after default case is redundant");
                    }
                    match pat {
                        &Pattern::Lit {
                            value: Literal::Bool(ref b),
                            ..
                        } => {
                            if matched.insert(*b) {
                                // ok
                            } else {
                                panic!("redundant patterns")
                            }
                        }
                        &Pattern::Var { .. } | &Pattern::Wildcard { .. } => defaulted = true,
                        _ => unreachable!("expression and pattern doesn't match. It'a bug"),
                    }
                }
                if matched != variants && !defaulted {
                    panic!("pattern non-exhausitive. rest: {:?}", matched)
                }
            }
            // integer like
            Ty::Int => {
                let mut matched = HashSet::new();
                let mut defaulted = false;
                for &mut (ref pat, _) in arms {
                    if defaulted {
                        panic!("pattern after default case is redundant");
                    }
                    match pat {
                        &Pattern::Lit {
                            value: Literal::Int(ref i),
                            ..
                        } => {
                            if matched.insert(i) {
                                // ok
                            } else {
                                panic!("redundant patterns")
                            }
                        }
                        &Pattern::Var { .. } | &Pattern::Wildcard { .. } => defaulted = true,
                        _ => unreachable!("expression and pattern doesn't match. It'a bug"),
                    }
                }
                // FIXME: treat cases when all the integers are specified by literal pattern
                if !defaulted {
                    panic!("pattern non-exhausitive. rest: {:?}", matched)
                }
            }
            // record like
            Ty::Tuple(_tuple) => {
                assert_eq!(arms.len(), 1);
                for &mut (ref pat, _) in arms {
                    match pat {
                        &Pattern::Tuple { .. }
                        | &Pattern::Var { .. }
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
impl<'a> Pass<ast::AST, TypeError<'a>> for CaseCheck {
    type Target = ast::AST;

    fn trans<'b>(&'b mut self, mut ast: ast::AST, _: &Config) -> Result<'a, Self::Target> {
        self.traverse_ast(&mut ast);
        Ok(ast)
    }
}
