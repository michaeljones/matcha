use std::fmt::Debug;

use codespan_reporting::term::termcolor::{self, ColorChoice, StandardStream};
use structopt::StructOpt;
use walkdir::WalkDir;

mod error;
mod parser;
mod renderer;
mod scanner;

use error::{Error, Source};

fn convert(filepath: &std::path::Path) -> Result<(), ()> {
    let result = std::fs::read_to_string(filepath)
        .map_err(|err| Error::IO(err, filepath.to_path_buf()))
        .and_then(|contents| {
            let source = Source {
                filename: filepath.to_string_lossy().into_owned(),
                contents: contents.clone(),
            };
            scanner::scan(&contents)
                .map_err(|err| Error::Scan(err, source.clone()))
                .and_then(|tokens| {
                    parser::parse(&mut tokens.iter().peekable())
                        .map_err(|error| Error::Parse(error, source.clone()))
                })
                .and_then(|ast| {
                    renderer::render(&mut ast.iter().peekable())
                        .map_err(|error| Error::Render(error, source.clone()))
                })
        })
        .and_then(|output| {
            let out_file_path = filepath.with_extension("gleam");
            std::fs::write(&out_file_path, output)
                .map_err(|err| Error::IO(err, out_file_path.to_path_buf()))
        });

    match result {
        Ok(()) => Ok(()),
        Err(error) => {
            let mut writer = StandardStream::stderr(color_choice());
            error::write(&mut writer, error);
            Err(())
        }
    }
}

fn color_choice() -> ColorChoice {
    if atty::is(atty::Stream::Stderr) {
        termcolor::ColorChoice::Auto
    } else {
        termcolor::ColorChoice::Never
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "templates", about = "Compiles templates into Gleam modules")]
struct Opt {
    #[structopt(short, long)]
    verbose: bool,

    #[structopt(long)]
    version: bool,
}

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let opt = Opt::from_args();
    if opt.version {
        println!("{}", VERSION);
        return;
    }

    let result = WalkDir::new(".")
        .into_iter()
        .filter_map(|e| e.ok())
        .filter_map(|entry| {
            let path = entry.path();

            if path.extension() == Some(std::ffi::OsStr::new("gleamx")) {
                if opt.verbose {
                    println!("Converting {}", path.display());
                }
                Some(convert(&path.to_path_buf()))
            } else {
                None
            }
        })
        .collect::<Result<Vec<_>, _>>();

    match result {
        Ok(_) => {}
        Err(()) => {
            std::process::exit(1);
        }
    }
}
