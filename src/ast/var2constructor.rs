use crate::ast::util::Traverse;
use crate::ast::*;
use crate::config::Config;
use crate::pass::Pass;

pub struct VarToConstructor {
    symbol_table: Option<SymbolTable>,
}

impl VarToConstructor {
    pub fn new() -> VarToConstructor {
        Self { symbol_table: None }
    }

    fn init(&mut self, symbol_table: SymbolTable) {
        self.symbol_table = Some(symbol_table)
    }

    fn generate_symbol_table(&mut self) -> SymbolTable {
        self.symbol_table.take().unwrap()
    }

    fn symbol_table(&self) -> &SymbolTable {
        self.symbol_table.as_ref().unwrap()
    }
}

impl<Ty: Clone + std::fmt::Debug> Traverse<Ty> for VarToConstructor {
    fn traverse_expr(&mut self, expr: &mut Expr<Ty>) {
        use crate::ast::Expr::*;
        let new_expr = match expr {
            Binds { ty, binds, ret } => {
                self.traverse_binds(ty, binds, ret);
                return;
            }
            BinOp { ty, op, l, r } => {
                self.traverse_binop(ty, op, l, r);
                return;
            }
            Fn { ty, param, body } => {
                self.traverse_fn(ty, param, body);
                return;
            }
            App { ty, fun, arg } => {
                self.traverse_app(ty, fun, arg);
                return;
            }
            If {
                ty,
                cond,
                then,
                else_,
            } => {
                self.traverse_if(ty, cond, then, else_);
                return;
            }
            Case { ty, cond, clauses } => {
                self.traverse_case(ty, cond, clauses);
                return;
            }
            Tuple { ty, tuple } => {
                self.traverse_tuple(ty, tuple);
                return;
            }
            Constructor { ty, name } => {
                self.traverse_constructor(ty, name);
                return;
            }
            Literal { ty, value } => {
                self.traverse_lit(ty, value);
                return;
            }
            Symbol { ty, name }
                if self
                    .symbol_table()
                    .get_datatype_of_constructor(name)
                    .is_some() =>
            {
                Expr::Constructor {
                    ty: ty.clone(),
                    name: name.clone(),
                }
            }
            Symbol { ty, name } => {
                self.traverse_sym(ty, name);
                return;
            }
        };

        *expr = new_expr
    }

    fn traverse_pattern(&mut self, pattern: &mut Pattern<Ty>) {
        use crate::ast::Pattern::*;
        let new_pattern = match pattern {
            Constant { .. } | Constructor { .. } | Wildcard { .. } | Tuple { .. } => return,

            Variable { ty, name }
                if self
                    .symbol_table()
                    .get_datatype_of_constructor(name)
                    .is_some() =>
            {
                Pattern::Constructor {
                    ty: ty.clone(),
                    name: name.clone(),
                }
            }
            Variable { .. } => return,
        };

        *pattern = new_pattern;
    }
}

impl<E, Ty: Clone + std::fmt::Debug> Pass<(SymbolTable, AST<Ty>), E> for VarToConstructor {
    type Target = (SymbolTable, AST<Ty>);

    fn trans(
        &mut self,
        (symbol_table, mut ast): (SymbolTable, AST<Ty>),
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        self.init(symbol_table);
        self.traverse_ast(&mut ast);
        let symbol_table = self.generate_symbol_table();
        Ok((symbol_table, ast))
    }
}
