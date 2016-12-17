use std::io;

pub trait PP {
    fn pp(&self, w: &mut io::Write, indent: usize) -> io::Result<()>;
    fn nspaces(n: usize) -> String {
        let mut s = String::new();
        for _ in 0..n {
            s.push(' ');
        }
        s
    }
}
