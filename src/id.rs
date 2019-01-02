use std::cell::Cell;
use std::rc::Rc;

#[derive(Clone, Default, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Id(Rc<Cell<u64>>);

impl Id {
    pub fn new() -> Self {
        Id(Rc::new(Cell::new(1)))
    }

    pub fn next(&mut self) -> u64 {
        let ret = self.0.get();
        self.0.set(ret + 1);
        ret
    }
}
