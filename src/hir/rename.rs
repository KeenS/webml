use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};

use prim::*;
use hir::*;
use pass::Pass;
use hir::util::Traverse;

pub struct Rename {
    tables: Vec<HashMap<Symbol, String>>,
    pos: usize,
    id: usize,
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
        let new_name = format!("{}@{}", &symbol.0, self.id);
        self.tables[pos].insert(symbol.clone(), new_name.clone());
        self.id += 1;
        symbol.0 = new_name;
    }

    fn rename(&mut self, symbol: &mut Symbol) {
        let pos = self.pos;
        for table in self.tables[0..pos].iter_mut().rev() {
            match table.get(symbol) {
                Some(new_name) => {
                    symbol.0 = new_name.clone();
                    return;
                }
                None => {}
            }
        }
    }


    fn rename_hir<'b, 'c>(&'b mut self, hir: &'c mut HIR) {
        let mut scope = self;
        for val in hir.0.iter_mut() {
            if val.rec {
                scope.new_symbol(&mut val.name);
                scope.rename_val(val);
            } else {
                scope.rename_val(val);
                scope.new_symbol(&mut val.name);
            }
        }
    }

    fn rename_val<'b, 'c>(&'b mut self, val: &'c mut Val) {
        self.rename_expr(&mut val.expr);
    }

    fn rename_expr<'b, 'c>(&'b mut self, expr: &'c mut Expr) {
        use hir::Expr::*;
        match *expr {
            Binds {
                ref mut binds,
                ref mut ret,
                ..
            } => {
                let mut scope = self.new_scope();
                for bind in binds.iter_mut() {
                    scope.rename_val(bind);
                    scope.new_symbol(&mut bind.name);
                }
                scope.rename_expr(ret);
            }
            Op {
                ref mut l,
                ref mut r,
                ..
            } => {
                self.rename_expr(l);
                self.rename_expr(r);
            }
            Fun {
                ref mut param,
                ref mut body,
                ..
            } => {
                let mut scope = self.new_scope();
                scope.new_symbol(&mut param.1);
                scope.rename_expr(body);
            }
            Closure { ref mut envs, .. } => {
                for &mut (_, ref mut var) in envs.iter_mut() {
                    self.rename(var);
                }
            }
            App {
                ref mut fun,
                ref mut arg,
                ..
            } => {
                self.rename_expr(fun);
                self.rename_expr(arg);
            }
            If {
                ref mut cond,
                ref mut then,
                ref mut else_,
                ..
            } => {
                self.rename_expr(cond);
                self.rename_expr(then);
                self.rename_expr(else_);
            }
            Tuple { ref mut tuple, .. } => {
                for t in tuple.iter_mut() {
                    self.rename_expr(t)
                }
            }
            Sym { ref mut name, .. } => {
                self.rename(name);
            }
            Lit { .. } | PrimFun { .. } => (),

        }
    }
}

impl<'a> util::Traverse for Scope<'a> {
    fn traverse_hir<'b, 'c>(&'b mut self, hir: &'c mut HIR) {
        let mut scope = self;
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
    pub fn new() -> Self {
        Rename {
            tables: Vec::new(),
            pos: 0,
            id: 0,
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
