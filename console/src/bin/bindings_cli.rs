use std::fs::File;
#[allow(unused_imports)]
use std::io::Read;
use std::io::Error as IoError;
use clap::{App, AppSettings, Arg, ArgMatches};
use failure::Error;
use console::bindings;

#[derive(Debug, Fail)]
#[fail(display = "Errors that occur while generating bindings")]
enum BindingError {
    /// Indicates that an io::Error occurred involving a given file.
    /// _0: The name of the file involved.
    /// _1: The io::Error with details.
    #[fail(display = "File error for '{}': {}", _0, _1)]
    FileError(String, IoError),
}

enum OutputLanguage {
    C,
    Swift
}

impl OutputLanguage {
    fn from(name: &str) -> Option<OutputLanguage> {
        match name {
            "c" => Some(OutputLanguage::C),
            "swift" => Some(OutputLanguage::Swift),
            _ => None
        }
    }
}

pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
    App::new("generate")
        .settings(&[
            AppSettings::ArgRequiredElseHelp,
            AppSettings::DeriveDisplayOrder,
            AppSettings::ColoredHelp,
        ])
        .alias("gen")
        .about("Generate Flipper language bindings")
        .before_help("Generate bindings for the specified module")
        .arg(Arg::with_name("file")
            .index(1)
            .takes_value(true)
            .required(true)
            .help("The module file to generate language bindings for"))
        .arg(Arg::with_name("name")
            .index(2)
            .takes_value(true)
            .required(true)
            .help("The name of the module"))
        .arg(Arg::with_name("language")
            .index(3)
            .takes_value(true)
            .required(false)
            .help("The language to generate. Defaults to \"c\"."))
}

pub fn execute(args: &ArgMatches) -> Result<(), Error> {
    // Guaranteed to be safe because these are required arguments.
    let filename = args.value_of("file").unwrap();
    let module_name = args.value_of("name").unwrap();
    let lang_str = args.value_of("language").unwrap_or("c");

    let language = OutputLanguage::from(lang_str).unwrap_or(OutputLanguage::C);

    let mut file = File::open(filename)
        .map_err(|e| BindingError::FileError(filename.to_owned(), e))?;

    let module_binary = {
        let mut v = Vec::new();
        let _ = file.read_to_end(&mut v);
        v
    };

    let out_file = match language {
        OutputLanguage::C => "./binding.c",
        OutputLanguage::Swift => "./binding.swift",
    };

    let mut out = File::create(out_file)
        .map_err(|e| BindingError::FileError(out_file.to_owned(), e))?;

    let module = bindings::Module::parse(String::from(module_name), "".to_owned(), &module_binary)?;

    match language {
        OutputLanguage::C => bindings::generators::c::generate_module(module, &mut out),
        OutputLanguage::Swift => bindings::generators::swift::generate_module(module, &mut out),
    }
}
