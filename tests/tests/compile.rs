use std::fs;
use std::io::{self, prelude::*};
use std::path::{Path, PathBuf};
use webml::TypeError;
use webml::{compile_string, Config};

fn read_and_append_to_string(path: impl AsRef<Path>, buf: &mut String) -> io::Result<usize> {
    let file = fs::File::open(path)?;
    let mut input = io::BufReader::new(file);
    input.read_to_string(buf)
}

fn with_compile_result(path: impl AsRef<Path>, callback: impl FnOnce(Result<Vec<u8>, TypeError>)) {
    let path = path.as_ref();
    let mut input = include_str!("../../ml_src/prelude.sml").to_string();
    let config = Config::default();
    read_and_append_to_string(&path, &mut input).expect("failed to load file");
    println!("compiling {}", path.display());
    let result = compile_string(input, &config);
    println!("   done");
    callback(result)
}

fn assert_compile_pass(path: impl AsRef<Path>) {
    let path = path.as_ref();
    with_compile_result(path, |res| match res {
        Ok(_) => (),
        Err(e) => panic!("failed to compile {}: {}", path.display(), e),
    })
}

fn assert_compile_fail(path: impl AsRef<Path>) {
    let path = path.as_ref();
    with_compile_result(path, |res| match res {
        Ok(_) => panic!("succeded to compile {}, which should fail", path.display(),),
        Err(_) => (),
    })
}

fn walk_dir(name: impl AsRef<Path>, mut callback: impl for<'a> FnMut(PathBuf)) {
    use walkdir::WalkDir;

    for entry in WalkDir::new(name.as_ref())
        .into_iter()
        .filter(|e| e.as_ref().map(|e| e.file_type().is_file()).unwrap_or(false))
    {
        let path = entry.unwrap().into_path();
        callback(path)
    }
}

#[test]
fn examples_compile_pass() {
    walk_dir("ml_example", assert_compile_pass)
}

#[test]
fn test_compile_pass() {
    walk_dir("tests/compile_pass", assert_compile_pass)
}

#[test]
fn test_compile_fail() {
    walk_dir("tests/compile_fail", assert_compile_fail)
}
