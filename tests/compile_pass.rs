use std::fs;
use webml::{compile_str, Config};

#[test]
fn examples_compile_pass() {
    use walkdir::WalkDir;

    let config = Config::default();
    for entry in WalkDir::new("ml_example")
        .into_iter()
        .filter(|e| e.as_ref().map(|e| e.file_type().is_file()).unwrap_or(false))
    {
        let path = entry.unwrap().into_path();
        let input = fs::read_to_string(&path).unwrap();
        compile_str(&input, &config)
            .expect(&format!("failed to compile {}", path.to_str().unwrap()));
    }
}
