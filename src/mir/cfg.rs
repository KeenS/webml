use std::collections::{HashSet, VecDeque};
use prim::*;

use super::{Function, EBB};
use petgraph::graph::Graph;

impl Function {
    pub fn cfg(&self) -> Graph<usize, ()> {
        let mut graph = Graph::new();
        let mut queue = VecDeque::new();
        let mut done = HashSet::new();
        queue.push_back(0);
        while !queue.is_empty() {
            let ebb_idx = queue.pop_front()
                // this is safe because queue is non_empty
                .expect("internal error");
            done.insert(ebb_idx);
            let node = graph.add_node(ebb_idx);
            for &(next, _) in self.body[ebb_idx].next_ebbs().iter() {
                let next_idx = self.find_ebb(next)
                    // this is safe because jump target must be in the function
                    .expect("internal error");
                if done.contains(&next_idx) {
                    continue;
                }
                let next_node = graph.add_node(next_idx);
                graph.add_edge(node, next_node, ());
                queue.push_back(next_idx);
            }
        }

        graph
    }

    pub fn find_ebb(&self, name: &Symbol) -> Option<usize> {
        self.body.iter().position(|ebb| &ebb.name == name)
    }
}

impl EBB {
    pub fn next_ebbs<'a>(&'a self) -> Vec<(&'a Symbol, bool)> {
        use mir::Op::*;
        let last = self.body.len() - 1;
        match &self.body[last] {
            &Branch { ref clauses, .. } => clauses
                .iter()
                .map(|&(_, ref lbl, forward)| (lbl, forward))
                .collect(),
            &Jump {
                ref target,
                forward,
                ..
            } => vec![(target, forward)],
            &Ret { .. } => vec![],
            _ => unreachable!(),
        }
    }
}
