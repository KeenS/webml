use std::fmt::Debug;

use util::PP;

pub trait Pass<T> {
    type Target;
    type Err;
    fn trans(&mut self, t: T) -> Result<Self::Target, Self::Err>;
}

impl<In, Out, Err, F> Pass<In> for F
    where F: Fn(In) -> Result<Out, Err>
{
    type Target = Out;
    type Err = Err;
    fn trans(&mut self, t: In) -> Result<Self::Target, Self::Err> {
        let out = self(t)?;
        Ok(out)
    }
}

pub struct DebugPass<T>(pub T);

impl<T, In, Out, Err> Pass<In> for DebugPass<T>
    where T: Pass<In, Target = Out, Err = Err>,
          Out: Debug
{
    type Target = Out;
    type Err = Err;

    fn trans(&mut self, i: In) -> Result<Self::Target, Self::Err> {
        let o = self.0.trans(i)?;
        println!("{:#?}", o);
        Ok(o)
    }
}

pub struct PPPass<T>(pub T);

impl<T, In, Out, Err> Pass<In> for PPPass<T>
    where T: Pass<In, Target = Out, Err = Err>,
          Out: PP
{
    type Target = Out;
    type Err = Err;

    fn trans(&mut self, i: In) -> Result<Self::Target, Self::Err> {
        let o = self.0.trans(i)?;
        o.pp(&mut ::std::io::stdout(), 0).unwrap();
        Ok(o)
    }
}


pub struct Chain<F, S>(pub F, pub S);

impl<F, FE, S, SE, T, In, Out> Pass<In> for Chain<F, S>
    where F: Pass<In, Target = T, Err = FE>,
          SE: From<FE>,
          S: Pass<T, Target = Out, Err = SE>
{
    type Target = Out;
    type Err = SE;

    fn trans(&mut self, i: In) -> Result<Self::Target, Self::Err> {
        let &mut Chain(ref mut fst, ref mut snd) = self;
        let t = fst.trans(i)?;
        let o = snd.trans(t)?;
        Ok(o)
    }
}


#[macro_export]
macro_rules! compile_pass {
    (? $pass: expr) => {DebugPass($pass)};
    (? $pass: expr, ) => {DebugPass($pass)};
    (? $pass: expr, $($passes: tt)*) => {
        Chain(DebugPass($pass), compile_pass!($($passes)*))
    };
    (! $pass: expr) => {PPPass($pass)};
    (! $pass: expr, ) => {PPPass($pass)};
    (! $pass: expr, $($passes: tt)*) => {
        Chain(PPPass($pass), compile_pass!($($passes)*))
    };
    ($pass: expr) => {$pass};
    ($pass: expr, ) => {$pass};
    ($pass: expr, $($passes: tt)*) => {
        Chain(compile_pass!($pass), compile_pass!($($passes)*))
    };
}
