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
    fn traverse_sym(&mut self, _ty: &mut HTy, name: &mut Symbol) {
        if name.1 == 0 {
            println!("fond: {}", name.0);
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
