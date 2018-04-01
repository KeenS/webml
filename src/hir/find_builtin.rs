use hir::*;
use pass::Pass;
use hir::util::Transform;

pub struct FindBuiltin;

impl Transform for FindBuiltin {
    fn transform_app(&mut self, ty: HTy, fun: Box<Expr>, arg: Box<Expr>) -> Expr {
        use hir::Expr::*;
        match *fun {
            Expr::Sym { ref name, .. } if name.1 == 0 => {
                assert!(::BUILTIN_FUNCTIONS.contains(&name.0.as_str()));
                let fun = match name.0.as_str() {
                    "print" => BIF::Print,
                    name => unreachable!("unknown builtin function found: {}", name),
                };
                let arg = self.transform_expr(*arg);
                return BuiltinCall {
                    ty: ty.clone(),
                    fun: fun,
                    arg: Box::new(arg),
                };
            }
            _ => {
                let arg = self.transform_expr(*arg);
                let fun = self.transform_expr(*fun);

                App {
                    fun: Box::new(fun),
                    arg: Box::new(arg),
                    ty: ty,
                }
            }
        }
    }
}

impl FindBuiltin {
    pub fn new() -> Self {
        FindBuiltin
    }
}

impl<E> Pass<HIR, E> for FindBuiltin {
    type Target = HIR;

    fn trans(&mut self, hir: HIR) -> ::std::result::Result<Self::Target, E> {
        Ok(self.transform_hir(hir))
    }
}
