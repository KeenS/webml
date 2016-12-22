use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Drop};

use prim::*;
use hir::*;
use pass::Pass;

pub struct Rename {
    tables: Vec<HashMap<Symbol, String>>,
    pos: usize,
    id: usize,
}

struct Scope<'a>(&'a mut Rename);

impl <'a>Deref for Scope<'a> {
    type Target = Rename;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl <'a>DerefMut for Scope<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl <'a>Drop for Scope<'a> {
    fn drop(&mut self) {
        self.pos -= 1;
    }
}

impl <'a>Scope<'a> {
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
                    return
                },
                None => {},
            }
        }
    }


    fn rename_hir<'b , 'c>(&'b mut self, hir: &'c mut  HIR) {
        let mut scope = self;
        for val in  hir.0.iter_mut() {
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
        match expr {
            &mut Binds{ref mut binds, ref mut ret, ..} => {
                let mut scope = self;
                for bind in  binds.iter_mut() {
                    scope.rename_val(bind);
                    scope.new_symbol(&mut bind.name);
                }
                scope.rename_expr(ret);
            }
            ,
            &mut Fun{ref mut param, ref mut body, ..} => {
                let mut scope = self.new_scope();
                scope.new_symbol(&mut param.1);
                scope.rename_expr(body);
            },
            &mut Closure {ref mut envs, ..} => {
                for &mut (_, ref mut var) in envs.iter_mut() {
                    self.rename(var);
                }
            }
            &mut App{ref mut fun, ref mut arg, ..} => {
                self.rename_expr(fun);
                self.rename_expr(arg);
            },
            &mut If {ref mut cond, ref mut then, ref mut else_, ..} => {
                self.rename_expr(cond);
                self.rename_expr(then);
                self.rename_expr(else_);
            }

            &mut Sym{ref mut name, ..} => {
                self.rename(name);
            }
            &mut Lit{..} |
            &mut PrimFun{..} => ()

        }
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

impl Pass<HIR> for Rename {
    type Target = HIR;
    type Err = TypeError;
    fn trans(&mut self, mut hir: HIR) -> ::std::result::Result<Self::Target, Self::Err> {
        self.scope().rename_hir(&mut hir);
        Ok(hir)
    }
}
