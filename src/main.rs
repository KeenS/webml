use clap::{app_from_crate, crate_authors, crate_description, crate_name, crate_version, Arg};
use std::collections::HashSet;
use std::fs;
use std::io;
use webml::{compile_strings, Config};

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
                .multiple(true)
                .required(true),
        )
        .get_matches();

    let filenames = matches.values_of("INPUT").expect("no file provided");
    let pretty_print_ir = matches
        .values_of("PRINT_IR")
        .into_iter()
        .flatten()
        .map(|s| s.to_string())
        .collect::<HashSet<String>>();

    let config = Config { pretty_print_ir };

    let prelude = include_str!("../ml_src/prelude.sml").to_string();
    let mut inputs = match filenames
        .into_iter()
        .map(fs::read_to_string)
        .collect::<io::Result<Vec<String>>>()
    {
        Ok(i) => i,
        Err(e) => {
            eprintln!("Compile error: failed to load file {e}");
            std::process::exit(1)
        }
    };
    inputs.insert(0, prelude);
    let code = match compile_strings(inputs, &config) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("compile error: {e}");
            std::process::exit(1)
        }
    };
    fs::write("out.wasm", &code).unwrap()
}
