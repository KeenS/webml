use clap::{app_from_crate, crate_authors, crate_description, crate_name, crate_version, Arg};
use std::collections::HashSet;
use std::fs;
use webml::{compile_str, Config};

fn main() {
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
        .flat_map(|vs| vs)
        .map(|s| s.to_string())
        .collect::<HashSet<String>>();

    let config = Config {
        pretty_print_ir,
        ..Default::default()
    };

    let input = fs::read_to_string(filename).expect("failed to load file");
    let code = compile_str(&input, &config).unwrap();
    fs::write("out.wasm", &code).unwrap()
}
