use std::collections::HashSet;

#[derive(Clone, Debug, Default)]
pub struct Config {
    pub pretty_print_ir: HashSet<String>,
}
