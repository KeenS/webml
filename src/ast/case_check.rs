use std::collections::HashSet;

use ast::*;
use prim::*;
use super::util::Traverse;

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
        let ty = cond.ty_defer()
            .force("internal error: typed AST isn't typed");
        if ty == Ty::Bool {
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
                    &Pattern::Lit { ref value, .. } => match value {
                        &Literal::Bool(ref b) => if matched.insert(*b) {
                            // ok
                        } else {
                            panic!("redundant patterns")
                        },
                        _ => panic!("pattern bool expected but got other"),
                    },
                    &Pattern::Var { .. } | &Pattern::Wildcard { .. } => defaulted = true,
                }
            }
            if matched != variants && !defaulted {
                panic!("pattern non-exhausitive. rest: {:?}", matched)
            }
        } else if ty == Ty::Int {
            let mut matched = HashSet::new();
            let mut defaulted = false;
            for &mut (ref pat, _) in arms {
                if defaulted {
                    panic!("pattern after default case is redundant");
                }
                match pat {
                    &Pattern::Lit { ref value, .. } => match value {
                        &Literal::Int(ref i) => if matched.insert(i) {
                            // ok
                        } else {
                            panic!("redundant patterns")
                        },
                        _ => panic!("pattern bool expected but got other"),
                    },
                    &Pattern::Var { .. } | &Pattern::Wildcard { .. } => defaulted = true,
                }
            }
            // FIXME: treat cases when all the integers are specified by literal pattern
            if !defaulted {
                panic!("pattern non-exhausitive. rest: {:?}", matched)
            }
        } else {
            panic!("non variant pattern match isn't supported");
        }
    }
}

use pass::Pass;
impl<'a> Pass<ast::AST, TypeError<'a>> for CaseCheck {
    type Target = ast::AST;

    fn trans<'b>(&'b mut self, mut ast: ast::AST) -> Result<'a, Self::Target> {
        self.traverse_ast(&mut ast);
        Ok(ast)
    }
}
