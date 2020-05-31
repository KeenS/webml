use crate::config::Config;
use crate::mir::*;
use crate::pass::Pass;
use crate::prim::*;
use std::collections::HashSet;

pub struct BlockArrange;

impl BlockArrange {
    pub fn new() -> Self {
        BlockArrange
    }

    fn arrange_mir(&mut self, mir: MIR) -> MIR {
        MIR(mir.0.into_iter().map(|f| self.arrange_fun(f)).collect())
    }

    fn arrange_fun(&mut self, mut fun: Function) -> Function {
        let mut ret = Vec::new();
        let mut dones = HashSet::new();
        let cur = fun.body.swap_remove(0);
        visit(&mut ret, &mut dones, cur, fun.body);
        fun.body = ret.into_iter().rev().collect();
        fun
    }
}

fn visit(
    ret: &mut Vec<EBB>,
    dones: &mut HashSet<Symbol>,
    cur: EBB,
    mut blocks: Vec<EBB>,
) -> Vec<EBB> {
    if !dones.contains(&cur.name) {
        dones.insert(cur.name.clone());
        for (next, forward) in cur.next_ebbs().into_iter().rev() {
            if forward {
                if let Some(idx) = blocks.iter().position(|ebb| &ebb.name == next) {
                    let b = blocks.swap_remove(idx);
                    blocks = visit(ret, dones, b, blocks);
                }
            }
        }
        ret.push(cur)
    }
    blocks
}

impl<E> Pass<Context, E> for BlockArrange {
    type Target = Context;

    fn trans(
        &mut self,
        Context(symbol_table, mir): Context,
        _: &Config,
    ) -> ::std::result::Result<Self::Target, E> {
        Ok(Context(symbol_table, self.arrange_mir(mir)))
    }
}
