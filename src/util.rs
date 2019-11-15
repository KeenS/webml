use std::io;

pub trait PP {
    fn pp<W: io::Write>(&self, w: &mut W, indent: usize) -> io::Result<()>;
    fn nspaces(n: usize) -> String {
        let mut s = String::new();
        for _ in 0..n {
            s.push(' ');
        }
        s
    }
}

#[macro_export]
macro_rules! inter_iter {
    ($v:expr, $inter: expr, |$e: pat| => $body: expr) => {
        loop {
            let mut itr = $v.into_iter();
            let $e = match itr.next() {
                Some(e) => e,
                None => break,
            };
            $body;
            while let Some($e) = itr.next() {
                $inter;
                $body;
            }
            break;
        }
    };
}
