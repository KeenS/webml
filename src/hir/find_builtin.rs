use std::collections::HashMap;

use prim::*;
use hir::*;
use pass::Pass;
use hir::util::Traverse;

pub struct FindBuiltin {
    tables: Vec<HashMap<Symbol, u64>>,
    pos: usize,
}

impl Traverse for FindBuiltin {
    fn traverse_app(&mut self, ty: &mut HTy, fun: &mut Box<Expr>, arg: &mut Box<Expr>) {
        let prim_name;
        if let Expr::Sym{ref mut name, ..} = **fun {
            if name.1 == 0 {
                prim_name = name.clone();
                // pass through to satisfy borrow checher
            } else {
                return
            }
        } else {
            return
        }
        *fun = Box::new(Expr::PrimFun {
            param_ty: arg.ty(),
            ret_ty: ty.clone(),
            name: prim_name,

        });
        self.traverse_expr(arg);
    }

    fn traverse_sym(&mut self, _ty: &mut HTy, name: &mut Symbol) {
        if name.1 == 0 {
            panic!("primitive {} is used in non-call position, which is not supported");
        }
    }
}

impl FindBuiltin {
    pub fn new() -> Self {
        Self {
            tables: Vec::new(),
            pos: 0,
        }
    }
}

impl<E> Pass<HIR, E> for FindBuiltin {
    type Target = HIR;

    fn trans(&mut self, mut hir: HIR) -> ::std::result::Result<Self::Target, E> {
        self.traverse_hir(&mut hir);
        Ok(hir)
    }
}
