pub trait Pass<T> {
    type Target;
    type Err;
    fn trans(&mut self, t: T) -> Result<Self::Target, Self::Err>;
}

impl <In, Out, Err, F> Pass<In> for F
    where F: Fn(In) -> Result<Out, Err> {
    type Target = Out;
    type Err = Err;
    fn trans(&mut self, t: In) -> Result<Self::Target, Self::Err> {
        let out = self(t)?;
        Ok(out)
    }
}

pub struct Chain<F, S>(pub F, pub S);

impl <F, FE, S, SE, T, In, Out> Pass<In> for Chain<F, S>
    where F: Pass<In, Target = T, Err = FE>,
          SE: From<FE>,
          S: Pass<T, Target = Out, Err = SE>,
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
    ($pass: expr) => {$pass};
    ($pass: expr, ) => {$pass};
    ($pass: expr, $($passes: expr, )*) => {
        Chain($pass, compile_pass!($($passes, )*))
    };
}
