use std::cell::RefCell;
use std::rc::Rc;
use std::ops::{Deref, DerefMut};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Unit,
    Bool,
    Int,
    Fun(Box<Ty>, Box<Ty>),
}


#[derive(Debug, Clone, PartialEq, Eq)]
// pub enum TyDefer {
//     Defer(Rc<RefCell<Option<Ty>>>),
//     Exists(Ty)
// }

pub struct TyDefer(pub Option<Ty>);

impl Deref for TyDefer {
    type Target = Option<Ty>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TyDefer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl TyDefer {
    pub fn empty() -> Self {
        TyDefer(None)
    }

    pub fn defined(&self) -> Option<Ty> {
        self.clone().0
    }

    pub fn force(self, message: &str) -> Ty {
        self.0.expect(message)
    }
}
