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
        ty: &mut TyDefer,
        cond: &mut Box<Expr>,
        arms: &mut Vec<(Pattern, Expr)>,
    ) {
        let ty = cond.ty_defer()
            .force("internal error: typed AST isn't typed");
        if ty == Ty::Bool {
            let mut variants = HashSet::new();
            variants.insert(true);
            variants.insert(false);
            for &mut (ref pat, _) in arms {
                match pat {
                    &Pattern::Lit { ref value } => match value {
                        &Literal::Bool(ref b) => if variants.remove(b) {
                            // ok
                        } else {
                            panic!("redundant patterns")
                        },
                        _ => panic!("pattern bool expected but got other"),
                    },
                }
            }
            if !variants.is_empty() {
                panic!("pattern non-exhausitive. rest: {:?}", variants)
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
