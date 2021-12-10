#![cfg(test)]

use std::{cell::RefCell, collections::HashSet, path::{Path, PathBuf}, rc::Rc};

use muncher::Intrinsics;

#[derive(Clone)]
struct Intr {
    stdout: RefCell<String>,
}

impl Intrinsics for Intr {
    fn print(&self, value: &str) {
        self.stdout.borrow_mut().push_str(value);
    }
}

fn do_run(source: &str) -> (String, String) {
    let intr = Rc::new(Intr {
        stdout: Default::default(),
    });
    let stderr = match muncher::eval(source, intr.clone()) {
        Ok(()) => "".to_owned(),
        Err(e) => e.pretty(Path::new("test.mnc"), source),
    };
    let stdout: String = intr.stdout.borrow().clone();
    (stdout, stderr)
}

fn check_program_run(
    path: &Path,
    stdout_path: Option<&Path>,
    stderr_path: Option<&Path>,
    allow_recreation: bool,
) {
    println!("running {}", path.display());
    let source = std::fs::read_to_string(path)
        .expect(&format!("failed to read {:?}", path));
    let stdout = match stdout_path {
        Some(path) => std::fs::read_to_string(path)
            .expect(&format!("failed to read {:?}", path))
            .replace("\r\n", "\n"),
        None => "".to_owned(),
    };
    let stderr = match stderr_path {
        Some(path) => std::fs::read_to_string(path)
            .expect(&format!("failed to read {:?}", path))
            .replace("\r\n", "\n"),
        None => "".to_owned(),
    };
    let (actual_out, actual_err) = do_run(&source);

    if cfg!(feature = "recreate") && allow_recreation {
        if actual_err != stderr {
            println!("stderr of {} changed", path.display());
            let stderr_path = add_extension(path, "stderr");
            std::fs::write(&stderr_path, actual_err.as_bytes())
                .expect(&format!("failed to write {}", stderr_path.display()));
        }
        if actual_out != stdout {
            println!("stdout of {} changed", path.display());
            let stdout_path = add_extension(path, "stdout");
            std::fs::write(&stdout_path, actual_out.as_bytes())
                .expect(&format!("failed to write {}", stdout_path.display()));
        }
    } else {
        if actual_err != stderr {
            eprintln!("expected error:\n{}", stderr);
            eprintln!("actual error:\n{}", actual_err);
            panic!(
                "program {} gave incorrect error, expected {:?}, got {:?}",
                path.display(),
                stderr,
                actual_err,
            );
        }
        if actual_out != stdout {
            eprintln!("expected output:\n{}", stdout);
            eprintln!("actual output:\n{}", actual_out);
            panic!(
                "program {} gave incorrect output, expected {:?}, got {:?}",
                path.display(),
                stdout,
                actual_out,
            );
        }
    }
}

fn convert_literate(path: &Path) -> String {
    let source = std::fs::read_to_string(path)
        .expect(&format!("failed to read {:?}", path));

    let arena = comrak::Arena::new();
    let root = comrak::parse_document(&arena, &source, &comrak::ComrakOptions::default());

    let mut program = Vec::new();

    fn iter_nodes<'a, F>(node: &'a comrak::nodes::AstNode<'a>, f: &mut F)
    where
        F: FnMut(&'a comrak::nodes::AstNode<'a>),
    {
        f(node);
        for c in node.children() {
            iter_nodes(c, f);
        }
    }

    iter_nodes(root, &mut |node| {
        match &node.data.borrow().value {
            comrak::nodes::NodeValue::CodeBlock(
                comrak::nodes::NodeCodeBlock { literal, .. },
            ) => {
                if program.len() > 0 {
                    program.push(b'\n');
                }
                program.extend(literal.iter().copied().filter(|&b| b != b'\r'));
            }
            _ => (),
        }
    });

    String::from_utf8(program).unwrap()
}

fn get_outputs(source: &str) -> String {
    let mut output = String::new();
    for line in source.lines() {
        let line = line.trim();
        if line.starts_with("// prints:") {
            output.push_str(line[10..].trim());
            output.push('\n');
        }
    }
    output
}

fn read_optional(path: &Path) -> Option<String> {
    match std::fs::read_to_string(path) {
        Ok(s) => Some(s),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(_) => panic!("failed to read {:?}", path),
    }
}

fn literate(dir: &Path) {
    let source = convert_literate(&dir.join("readme.md"));
    let outputs = get_outputs(&source);
    let program_file = dir.join("program.mnc");
    let output_file = add_extension(&program_file, "stdout");
    let actual_source = read_optional(&program_file);
    let actual_output = read_optional(&output_file);
    if Some(source.as_str()) != actual_source.as_deref() {
        if cfg!(feature = "recreate") {
            std::fs::write(&program_file, source.as_bytes())
                .expect(&format!("failed to write {:?}", program_file))
        } else {
            panic!("program is not up to date in {}", dir.display());
        }
    }
    if Some(outputs.as_str()) != actual_output.as_deref() {
        if cfg!(feature = "recreate") {
            std::fs::write(&output_file, outputs.as_bytes())
                .expect(&format!("failed to write {:?}", output_file))
        } else {
            panic!("output is not up to date in {}", dir.display());
        }
    }
    check_program_run(
        &program_file,
        Some(&output_file),
        None,
        false,
    )
}

fn file_exists(path: &Path) -> bool {
    match std::fs::metadata(path) {
        Ok(_) => true,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => false,
        Err(e) => panic!("io error: {}", e),
    }
}

fn add_extension(path: &Path, ext: &str) -> PathBuf {
    let mut path = path.to_path_buf().into_os_string();
    path.push(".");
    path.push(ext);
    PathBuf::from(path)
}

#[test]
fn test_cases() {
    let mut seen_files = HashSet::new();
    let mut used_files = HashSet::new();
    for entry in std::fs::read_dir("../programs/test-cases").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        seen_files.insert(path.clone());
        match path.extension() {
            None => continue,
            Some(x) if x != "mnc" => continue,
            Some(_) => {}
        }
        let stdout_path = add_extension(&path, "stdout");
        let stderr_path = add_extension(&path, "stderr");
        let stdout_path = file_exists(&stdout_path).then(|| stdout_path);
        let stderr_path = file_exists(&stderr_path).then(|| stderr_path);
        check_program_run(&path, stdout_path.as_deref(), stderr_path.as_deref(), true);
        used_files.insert(path);
        used_files.extend(stdout_path);
        used_files.extend(stderr_path);
    }
    for file in &seen_files {
        if !used_files.contains(file) {
            panic!("file {} was not used", file.display());
        }
    }
}

#[test]
fn intro() {
    literate(Path::new("../programs/examples/intro"));
}

#[test]
fn array() {
    literate(Path::new("../programs/examples/array"));
}

#[test]
fn aoc() {
    literate(Path::new("../programs/examples/aoc2021-day7"));
}

#[test]
fn unparsable() {
    literate(Path::new("../programs/examples/unparsable"));
}
