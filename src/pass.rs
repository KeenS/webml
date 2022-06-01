use crate::config::Config;
use log::info;
use std::fmt::{Debug, Display};
use std::marker::PhantomData;

pub trait Pass<T, E> {
    type Target;
    fn trans(&mut self, t: T, config: &Config) -> Result<Self::Target, E>;
}

impl<In, Out, Err, F> Pass<In, Err> for F
where
    F: Fn(In) -> Result<Out, Err>,
{
    type Target = Out;
    fn trans(&mut self, t: In, _: &Config) -> Result<Self::Target, Err> {
        let out = self(t)?;
        Ok(out)
    }
}

pub struct DebugPass<T>(pub T);

impl<T, In, Out, Err> Pass<In, Err> for DebugPass<T>
where
    T: Pass<In, Err, Target = Out>,
    Out: Debug,
{
    type Target = Out;

    fn trans(&mut self, i: In, config: &Config) -> Result<Self::Target, Err> {
        let o = self.0.trans(i, config)?;
        println!("{:#?}", o);
        Ok(o)
    }
}

pub struct DisplayPass<T>(pub T);

impl<T, In, Out, Err> Pass<In, Err> for DisplayPass<T>
where
    T: Pass<In, Err, Target = Out>,
    Out: Display,
{
    type Target = Out;

    fn trans(&mut self, i: In, config: &Config) -> Result<Self::Target, Err> {
        let o = self.0.trans(i, config)?;
        println!("{}", o);
        Ok(o)
    }
}

pub struct PrintablePass<T>(pub T, pub &'static str);

impl<T, In, Out, Err> Pass<In, Err> for PrintablePass<T>
where
    T: Pass<In, Err, Target = Out>,
    Out: Display,
{
    type Target = Out;

    fn trans(&mut self, i: In, config: &Config) -> Result<Self::Target, Err> {
        let o = self.0.trans(i, config)?;
        info!("pass: {}", self.1);
        if config.pretty_print_ir.contains(self.1) {
            println!("{}", o);
        }

        Ok(o)
    }
}

pub struct Chain<In, Mid, Out, E> {
    pub fst: Box<dyn Pass<In, E, Target = Mid>>,
    pub snd: Box<dyn Pass<Mid, E, Target = Out>>,
}

// impl<F, FO, S, SO> Chain<F, FO, S, SO> {
//     pub fn new(fst: F, snd: S) -> Self {
//         Chain {
//             fst,
//             snd,
//             phantom: PhantomData,
//         }
//     }
// }

impl<In, Mid, Out, E> Pass<In, E> for Chain<In, Mid, Out, E> {
    type Target = Out;

    fn trans(&mut self, i: In, config: &Config) -> Result<Self::Target, E> {
        let &mut Chain {
            ref mut fst,
            ref mut snd,
            ..
        } = self;
        let t = fst.trans(i, config)?;
        let o = snd.trans(t, config)?;
        Ok(o)
    }
}

pub struct ConvError<P, FE, O> {
    pass: P,
    phantom: PhantomData<(FE, O)>,
}

impl<P, FE, O> ConvError<P, FE, O> {
    pub fn new(pass: P) -> Self {
        ConvError {
            pass,
            phantom: PhantomData,
        }
    }
}

impl<In, FE, SE, O, P> Pass<In, SE> for ConvError<P, FE, O>
where
    P: Pass<In, FE, Target = O>,
    SE: From<FE>,
{
    type Target = O;

    fn trans(&mut self, i: In, config: &Config) -> Result<Self::Target, SE> {
        Ok(self.pass.trans(i, config)?)
    }
}

#[macro_export]
macro_rules! compile_pass {
    ($($labels: ident : $passes: expr,)*) => {
        compile_pass!($($labels: $passes),*)
    };
    ($label: ident : $pass: expr, $($labels: ident : $passes: expr),*) => {
        Chain{
            fst:Box::new(PrintablePass($pass, stringify!($label))),
            snd: Box::new(compile_pass!($($labels: $passes),*)),
        }
    };
    ($label: ident : $pass: expr) => {
        PrintablePass($pass, stringify!($label))
    };
}
