use std::rc::Rc;
use std::cell::Cell;

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
