use std::{fs::File, io::Read, path::PathBuf, str::FromStr};

use ariadne::Cache;
use clap::{App, Arg, SubCommand};
use parser::parser::Lexer;

fn main() {
    let matches = App::new("DESTRUCTION")
        .bin_name("DESTRUCTION")
        .author("krista-chan <qbotdev84@gmail.com>, spu7nix <main@spu7nix.net>, flow, camila314") // put emails here
        .version("v0.0.0")
        .about("Langjam0002 entry")
        .subcommand(
            SubCommand::with_name("build")
                .about("Build and run a source file")
                .arg(
                    Arg::with_name("path")
                        .help("Path to the source file to be built")
                        .short("p")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("input")
                        .help("String for the interpreter to use as input")
                        .short("i")
                        .long("input")
                        .takes_value(true)
                        .required(false),
                ),
        )
        .subcommand(
            SubCommand::with_name("eval")
                .alias("ev")
                .alias("e")
                .about("Evaluate arbitrary code from the command line")
                .arg(
                    Arg::with_name("code")
                        .help("Code to evaluate")
                        .short("c")
                        .long("code")
                        .required(true)
                        .index(1),
                )
                .arg(
                    Arg::with_name("input")
                        .help("String for the interpreter to use as input")
                        .short("i")
                        .long("input")
                        .takes_value(true)
                        .required(true),
                ),
        )
        .get_matches();

    use ariadne::FileCache;

    if let Some(m) = matches.subcommand_matches("build") {
        let path = m.value_of("path").unwrap();
        let mut file = File::open(path).unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();

        let mut cache = FileCache::default();
        match cache.fetch(&PathBuf::from(path)) {
            Ok(_) => (),
            Err(_) => unreachable!(),
        }

        let mut lexer = Lexer::new(&contents, PathBuf::from_str(path).ok());
        let parsed = match lexer.parse() {
            Ok(p) => p,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        };

        let evaled = match interpreter::interpret::interpret(
            parsed,
            interpreter::traits::Value::String(m.value_of("input").unwrap_or("").to_string()),
        ) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}", e);
                return;
            }
        };
        println!("{}", evaled);
    } else if let Some(m) = matches.subcommand_matches("eval") {
        let parsed = m.value_of("code").unwrap().parse().unwrap();

        let evaled = interpreter::interpret::interpret(
            parsed,
            interpreter::traits::Value::String(m.value_of("input").unwrap().to_string()),
        )
        .unwrap();
        println!("{}", evaled);
    }
}
