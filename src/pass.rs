use std::fmt::Debug;
use std::marker::PhantomData;
use util::PP;

pub trait Pass<T, E> {
    type Target;
    fn trans(&mut self, t: T) -> Result<Self::Target, E>;
}

impl<In, Out, Err, F> Pass<In, Err> for F
where
    F: Fn(In) -> Result<Out, Err>,
{
    type Target = Out;
    fn trans(&mut self, t: In) -> Result<Self::Target, Err> {
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

    fn trans(&mut self, i: In) -> Result<Self::Target, Err> {
        let o = self.0.trans(i)?;
        println!("{:#?}", o);
        Ok(o)
    }
}

pub struct PPPass<T>(pub T);

impl<T, In, Out, Err> Pass<In, Err> for PPPass<T>
where
    T: Pass<In, Err, Target = Out>,
    Out: PP,
{
    type Target = Out;

    fn trans(&mut self, i: In) -> Result<Self::Target, Err> {
        let o = self.0.trans(i)?;
        o.pp(&mut ::std::io::stdout(), 0).unwrap();
        Ok(o)
    }
}


pub struct Chain<F, FO, S, SO> {
    pub fst: F,
    pub snd: S,
    phantom: PhantomData<(FO, SO)>,
}

impl<F, FO, S, SO> Chain<F, FO, S, SO> {
    pub fn new(fst: F, snd: S) -> Self {
        Chain {
            fst: fst,
            snd: snd,
            phantom: PhantomData,
        }
    }
}

impl<F, E, S, T, In, Out> Pass<In, E> for Chain<F, T, S, Out>
where
    F: Pass<In, E, Target = T>,
    S: Pass<T, E, Target = Out>,
{
    type Target = Out;

    fn trans(&mut self, i: In) -> Result<Self::Target, E> {
        let &mut Chain {
            ref mut fst,
            ref mut snd,
            ..
        } = self;
        let t = fst.trans(i)?;
        let o = snd.trans(t)?;
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
            pass: pass,
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

    fn trans(&mut self, i: In) -> Result<Self::Target, SE> {
        Ok(self.pass.trans(i)?)
    }
}

#[macro_export]
macro_rules! compile_pass {
    (? $pass: expr) => {DebugPass($pass)};
    (? $pass: expr, ) => {DebugPass($pass)};
    (? $pass: expr, $($passes: tt)*) => {
        Chain::new(DebugPass($pass), compile_pass!($($passes)*))
    };
    (! $pass: expr) => {PPPass($pass)};
    (! $pass: expr, ) => {PPPass($pass)};
    (! $pass: expr, $($passes: tt)*) => {
        Chain::new(PPPass($pass), compile_pass!($($passes)*))
    };
    ($pass: expr) => {$pass};
    ($pass: expr, ) => {$pass};
    ($pass: expr, $($passes: tt)*) => {
        Chain::new(compile_pass!($pass), compile_pass!($($passes)*))
    };
}
