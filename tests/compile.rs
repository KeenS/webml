use std::fs;
use std::io::{self, prelude::*};
use std::path::{Path, PathBuf};
use webml::TypeError;
use webml::{compile_str, Config};

fn read_and_append_to_string(path: impl AsRef<Path>, buf: &mut String) -> io::Result<usize> {
    let file = fs::File::open(path)?;
    let mut input = io::BufReader::new(file);
    input.read_to_string(buf)
}

fn compile_dir(
    name: impl AsRef<Path>,
    mut callback: impl for<'a> FnMut(PathBuf, Result<Vec<u8>, TypeError<'a>>),
) {
    use walkdir::WalkDir;

    let prelude = include_str!("../ml_src/prelude.sml").to_string();
    let config = Config::default();
    for entry in WalkDir::new(name.as_ref())
        .into_iter()
        .filter(|e| e.as_ref().map(|e| e.file_type().is_file()).unwrap_or(false))
    {
        let mut input = prelude.clone();
        let path = entry.unwrap().into_path();
        read_and_append_to_string(&path, &mut input).expect("failed to load file");
        let result = compile_str(&input, &config);
        callback(path, result)
    }
}

#[test]
fn examples_compile_pass() {
    compile_dir("ml_example", |path, result| {
        println!("{}", path.to_str().unwrap());
        result
            .map(|_| ())
            .unwrap_or_else(|e| panic!("failed to compile {}: {}", path.to_str().unwrap(), e))
    })
}

#[test]
fn test_compile_pass() {
    compile_dir("tests/compile_pass", |path, result| {
        println!("{}", path.to_str().unwrap());
        result
            .map(|_| ())
            .unwrap_or_else(|e| panic!("failed to compile {}: {}", path.to_str().unwrap(), e))
    })
}

#[test]
#[should_panic]
fn test_compile_fail() {
    compile_dir("tests/compile_fail", |path, result| {
        println!("{}", path.to_str().unwrap());
        result
            .map(|_| ())
            .unwrap_or_else(|e| panic!("failed to compile {}: {}", path.to_str().unwrap(), e))
    })
}
