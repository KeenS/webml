use clap::{app_from_crate, crate_authors, crate_description, crate_name, crate_version, Arg};
use std::collections::HashSet;
use std::fs;
use std::io::{self, prelude::*};
use std::path::Path;
use webml::{compile_string, Config};

fn read_and_append_to_string(path: impl AsRef<Path>, buf: &mut String) -> io::Result<usize> {
    let file = fs::File::open(path)?;
    let mut input = io::BufReader::new(file);
    input.read_to_string(buf)
}

fn main() {
    env_logger::init();
    let matches = app_from_crate!()
        .arg(
            Arg::with_name("PRINT_IR")
                .long("print-ir")
                .help("print the output of IR")
                .value_name("IR")
                .takes_value(true)
                .multiple(true),
        )
        .arg(
            Arg::with_name("INPUT")
                .help("file to compile")
                .required(true),
        )
        .get_matches();

    let filename = matches
        .value_of("INPUT")
        .unwrap_or("ml_example/example1.sml");
    let pretty_print_ir = matches
        .values_of("PRINT_IR")
        .into_iter()
        .flatten()
        .map(|s| s.to_string())
        .collect::<HashSet<String>>();

    let config = Config {
        pretty_print_ir,
        ..Default::default()
    };

    let prelude = include_str!("../ml_src/prelude.sml").to_string();
    let mut input = prelude;
    read_and_append_to_string(filename, &mut input).expect("failed to load file");
    let code = compile_string(input, &config).expect("failed to compile file");
    fs::write("out.wasm", &code).unwrap()
}
