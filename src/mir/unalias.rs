use crate::config::Config;
use crate::mir::*;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct UnAlias {
    alias: HashMap<Symbol, Symbol>,
}

impl UnAlias {
    fn conv_mir(&mut self, mir: MIR) -> MIR {
        MIR(mir.0.into_iter().map(|f| self.conv_fun(f)).collect())
    }

    fn conv_fun(&mut self, mut fun: Function) -> Function {
        self.alias.clear();
        let mut body = Vec::new();
        for mut ebb in fun.body.into_iter() {
            ebb = self.conv_ebb(ebb);
            body.push(ebb);
        }
        fun.body = body;
        fun
    }

    fn conv_ebb(&mut self, mut ebb: EBB) -> EBB {
        use crate::mir::Op::*;
        let mut body = Vec::new();
        for mut op in ebb.body.into_iter() {
            match &mut op {
                Alias { var, sym, .. } => {
                    self.alias(var.clone(), sym.clone());
                    continue;
                }
                BinOp { l, r, .. } => {
                    self.resolv_alias(l);
                    self.resolv_alias(r);
                }
                Tuple { tuple, .. } => {
                    for v in tuple.iter_mut() {
                        self.resolv_alias(v);
                    }
                }
                Proj { tuple, .. } => {
                    self.resolv_alias(tuple);
                }
                Union { variant, .. } => {
                    self.resolv_alias(variant);
                }
                Select { union, .. } => {
                    self.resolv_alias(union);
                }

                Closure { fun, env, .. } => {
                    self.resolv_alias(fun);
                    for &mut (_, ref mut var) in env.iter_mut() {
                        self.resolv_alias(var);
                    }
                }
                ExternCall { args, .. } => {
                    for arg in args.iter_mut() {
                        self.resolv_alias(arg);
                    }
                }

                Call { fun, args, .. } => {
                    self.resolv_alias(fun);
                    for arg in args.iter_mut() {
                        self.resolv_alias(arg);
                    }
                }
                Jump { args, .. } => {
                    for arg in args.iter_mut() {
                        self.resolv_alias(arg);
                    }
                }
                Ret { value, .. } => {
                    if let Some(v) = value.as_mut() {
                        self.resolv_alias(v)
                    }
                }
                Lit { .. } => (),
                Branch { cond, .. } => self.resolv_alias(cond),
            }
            body.push(op)
        }
        ebb.body = body;
        ebb
    }

    fn alias(&mut self, al: Symbol, mut orig: Symbol) {
        while let Some(s) = self.alias.get(&orig) {
            orig = s.clone()
        }
        self.alias.insert(al, orig);
    }

    fn resolv_alias(&mut self, sym: &mut Symbol) {
        match self.alias.get(sym) {
            None => (),
            Some(orig) => {
                sym.0 = orig.0.clone();
                sym.1 = orig.1;
            }
        }
    }
}

impl<E> Pass<Context, E> for UnAlias {
    type Target = Context;

    fn trans(
        &mut self,
        Context(symbol_table, mir): Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        Ok(Context(symbol_table, self.conv_mir(mir)))
    }
}
