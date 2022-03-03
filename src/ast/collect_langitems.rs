use log::{debug, warn};

use crate::ast::*;
use crate::prim::*;
use crate::Config;
use std::collections::HashMap;
use util::Traverse;

#[derive(Debug)]
pub struct CollectLangItems {}

impl CollectLangItems {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug)]
struct Collector {
    lang_items: HashMap<LangItem, Symbol>,
}

impl Collector {
    fn new(lang_items: HashMap<LangItem, Symbol>) -> Self {
        Self { lang_items }
    }
}

impl Traverse<Empty> for Collector {
    fn traverse_langitem(&mut self, name: &mut LangItem, decl: &mut UntypedCoreDeclaration) {
        use Declaration::*;
        println!("processing {name}");
        let key = *name;
        let value = match decl {
            Val {
                pattern:
                    Pattern {
                        inner: PatternKind::Variable { name, .. },
                        ..
                    },
                ..
            } => name.clone(),
            Val { .. } => {
                warn!("lang item annotated to non simple binding. Skipping it");
                return;
            }
            Datatype { name, .. } => name.clone(),
            LangItem { .. } => {
                debug!("lang item annotated to useless decl");
                return;
            }
            D(d) => match *d {},
        };
        if self.lang_items.insert(key, value).is_some() {
            warn!("lang item overrided")
        }
        self.traverse_statement(decl)
    }
}

use crate::pass::Pass;
impl Pass<UntypedCoreContext, TypeError> for CollectLangItems {
    type Target = UntypedCoreContext;

    fn trans(&mut self, context: UntypedCoreContext, _: &Config) -> Result<Self::Target> {
        let symbol_table = context.symbol_table;
        let mut ast = context.ast;
        let lang_items = context.lang_items;

        let mut collector = Collector::new(lang_items);
        collector.traverse_ast(&mut ast);
        let lang_items = collector.lang_items;
        debug!("collected lang items: {lang_items:?}");

        Ok(Context {
            symbol_table,
            ast,
            lang_items,
        })
    }
}
