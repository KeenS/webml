use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};

use prim::*;
use hir::*;
use pass::Pass;
use hir::util::Traverse;
use id::Id;

pub struct Rename {
    tables: Vec<HashMap<Symbol, u64>>,
    pos: usize,
    id: Id,
}

struct Scope<'a>(&'a mut Rename);

impl<'a> Deref for Scope<'a> {
    type Target = Rename;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> Drop for Scope<'a> {
    fn drop(&mut self) {
        self.pos -= 1;
    }
}

impl<'a> Scope<'a> {
    fn new(inner: &'a mut Rename) -> Self {
        let pos = inner.pos;
        if inner.tables.len() <= pos {
            inner.tables.push(HashMap::new())
        } else {
            inner.tables[pos].clear();
        }

        inner.pos += 1;
        Scope(inner)
    }

    fn new_scope(&mut self) -> Scope {
        Scope::new(self)
    }

    fn new_symbol(&mut self, symbol: &mut Symbol) {
        let pos = self.pos - 1;
        let new_id = self.id.next();
        self.tables[pos].insert(symbol.clone(), new_id);
        symbol.1 = new_id;
    }

    fn rename(&mut self, symbol: &mut Symbol) {
        let pos = self.pos;
        for table in self.tables[0..pos].iter_mut().rev() {
            match table.get(symbol) {
                Some(new_id) => {
                    symbol.1 = *new_id;
                    return;
                }
                None => {}
            }
        }
    }
}

impl<'a> util::Traverse for Scope<'a> {
    fn traverse_hir<'b, 'c>(&'b mut self, hir: &'c mut HIR) {
        let scope = self;
        for val in hir.0.iter_mut() {
            if val.rec {
                scope.new_symbol(&mut val.name);
                scope.traverse_val(val);
            } else {
                scope.traverse_val(val);
                scope.new_symbol(&mut val.name);
            }
        }
    }

    fn traverse_val<'b, 'c>(&'b mut self, val: &'c mut Val) {
        self.traverse_expr(&mut val.expr);
    }

    fn traverse_binds(&mut self, _ty: &mut HTy, binds: &mut Vec<Val>, ret: &mut Box<Expr>) {
        let mut scope = self.new_scope();
        for bind in binds.iter_mut() {
            scope.traverse_val(bind);
            scope.new_symbol(&mut bind.name);
        }
        scope.traverse_expr(ret);
    }

    fn traverse_fun(
        &mut self,
        param: &mut (HTy, Symbol),
        _body_ty: &mut HTy,
        body: &mut Box<Expr>,
        _captures: &mut Vec<(HTy, Symbol)>,
        _make_closure: &mut Option<bool>,
    ) {
        let mut scope = self.new_scope();
        scope.new_symbol(&mut param.1);
        scope.traverse_expr(body);
    }

    fn traverse_closure(
        &mut self,
        envs: &mut Vec<(HTy, Symbol)>,
        _param_ty: &mut HTy,
        _body_ty: &mut HTy,
        _fname: &mut Symbol,
    ) {
        for &mut (_, ref mut var) in envs.iter_mut() {
            self.rename(var);
        }
    }

    fn traverse_sym(&mut self, _ty: &mut HTy, name: &mut Symbol) {
        self.rename(name);
    }
}

impl Rename {
    pub fn new(id: Id) -> Self {
        Rename {
            tables: Vec::new(),
            pos: 0,
            id,
        }
    }

    fn scope<'a>(&'a mut self) -> Scope<'a> {
        Scope::new(self)
    }
}

impl<E> Pass<HIR, E> for Rename {
    type Target = HIR;

    fn trans(&mut self, mut hir: HIR) -> ::std::result::Result<Self::Target, E> {
        self.scope().traverse_hir(&mut hir);
        Ok(hir)
    }
}
