use std::fs;
use std::path::{Path, PathBuf};
use webml::TypeError;
use webml::{compile_str, Config};

fn compile_dir(
    name: impl AsRef<Path>,
    mut callback: impl for<'a> FnMut(PathBuf, Result<Vec<u8>, TypeError<'a>>),
) {
    use walkdir::WalkDir;

    let config = Config::default();
    for entry in WalkDir::new(name.as_ref())
        .into_iter()
        .filter(|e| e.as_ref().map(|e| e.file_type().is_file()).unwrap_or(false))
    {
        let path = entry.unwrap().into_path();
        let input = fs::read_to_string(&path).unwrap();
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
            .unwrap_or_else(|_| panic!("failed to compile {}", path.to_str().unwrap()))
    })
}

#[test]
fn test_compile_pass() {
    compile_dir("tests/compile_pass", |path, result| {
        println!("{}", path.to_str().unwrap());
        result
            .map(|_| ())
            .unwrap_or_else(|_| panic!("failed to compile {}", path.to_str().unwrap()))
    })
}

#[test]
#[should_panic]
fn test_compile_fail() {
    compile_dir("tests/compile_fail", |path, result| {
        println!("{}", path.to_str().unwrap());
        result
            .map(|_| ())
            .unwrap_or_else(|_| panic!("failed to compile {}", path.to_str().unwrap()))
    })
}
