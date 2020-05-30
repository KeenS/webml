use std::env::args;
use webml_interp::WebmlInterp;

fn main() {
    let path = args().nth(1).expect("Usage: FILE");
    let mut interp = WebmlInterp::new();
    interp.run_file(path)
}
